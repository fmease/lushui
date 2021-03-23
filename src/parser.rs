//! The parser.
//!
//! I *think* it can be classified as a top-down recursive-descent parser with arbitrary look-ahead.
//!
//! ## Issues
//!
//! * crude error locations
//! * ugly API
//! * all syntax errors are fatal. instead, she should have a "poisoned" mode
//!
//! ## Grammar Notation
//!
//! Most parsing functions in this module are accompanied by a grammar snippet.
//! These snippets are written in an EBNF explained below:
//!
//! | Notation  | Name                                | Definition or Remarks                                         |
//! |-----------|-------------------------------------|---------------------------------------------------------------|
//! | `; C`     | Comment                             | Stretches until the end of the line                           |
//! | `N ::= R` | Definition                          | Defines non-terminal `A` by rule `R`                          |
//! | `A B`     | Sequence                            | Rule `B` immediately followed by rule `A` modulo lexed tokens |
//! | `(A)`     | Grouping                            | To escape default precedence                                  |
//! | <code>A &vert; B</code>   | Ordered Alternative                 | Either `A` or `B` first trying `A` then `B`                   |
//! | `A?`      | Option                              | `A` or nothing (ε)                                            |
//! | `A*`      | Kleene Star (Multiplicity)          | Arbitrarily long sequence of `A`s                             |
//! | `A+`      | Kleene Plus (Positive Multiplicity) | Arbitrarily long non-empty sequence of `A`s                   |
//! | `"T"`     | Terminal                            | Lexed token by textual content                                |
//! | `#T`      | Named Terminal                      | Lexed token by name                                           |
//! | `<!A`     | Negative Look-Behind                |                                                               |

pub mod ast;
#[cfg(test)]
mod test;

use crate::{
    diagnostics::{Code, Diagnostic, Diagnostics, Results, Warn},
    format::{ordered_listing, Conjunction},
    lexer::{Token, TokenKind},
    smallvec,
    span::{SourceFile, Span, Spanned, Spanning},
    SmallVec,
};
use ast::*;
use std::{cell::RefCell, convert::TryInto, default::default, rc::Rc};

type Result<T = (), E = ()> = std::result::Result<T, E>;

const STANDARD_DECLARATION_DELIMITERS: [Delimiter; 3] = {
    use Delimiter::*;
    [
        TypeAnnotationPrefix,
        DefinitionPrefix,
        Token(TokenKind::LineBreak),
    ]
};

const BRACKET_POTENTIAL_PI_TYPE_LITERAL: &str =
    "add round brackets around the potential pi type literal to disambiguate the expression";

/// The state of the parser.
pub struct Parser<'a> {
    file: Rc<SourceFile>,
    tokens: &'a [Token],
    // @Task make it a RefCell, too
    warnings: &'a mut Diagnostics,
    errors: RefCell<Diagnostics>,
    reflection_depth: u16,
    index: usize,
}

impl<'a> Parser<'a> {
    pub fn new(file: Rc<SourceFile>, tokens: &'a [Token], warnings: &'a mut Diagnostics) -> Self {
        Self {
            file,
            tokens,
            warnings,
            errors: default(),
            reflection_depth: 0,
            index: 0,
        }
    }

    /// Parse in a sandboxed way.
    ///
    /// Used for arbitrary look-ahead. Restores the old cursor on failure.
    fn reflect<T>(&mut self, parser: impl FnOnce(&mut Self) -> Result<T>) -> Result<T> {
        let index = self.index;
        let result = self.manually_reflect(parser);

        if result.is_err() {
            self.index = index;
        }

        result
    }

    fn manually_reflect<T>(&mut self, handler: impl FnOnce(&mut Self) -> T) -> T {
        self.reflection_depth += 1;
        let result = handler(self);
        self.reflection_depth -= 1;
        result
    }

    fn reflecting(&self) -> bool {
        self.reflection_depth != 0
    }

    fn error<T, D: FnOnce() -> Diagnostic>(&self, diagnostic: D) -> Result<T> {
        fn error<D: FnOnce() -> Diagnostic>(parser: &Parser<'_>, diagnostic: D) -> Result<!> {
            if !parser.reflecting() {
                parser.errors.borrow_mut().insert(diagnostic());
            }

            Err(())
        }

        error(self, diagnostic).map(|okay| okay)
    }

    fn expect(&self, expected: TokenKind) -> Result<Token> {
        let token = self.current_token();
        if token.kind == expected {
            Ok(token.clone())
        } else {
            self.error(|| Expected::Token(expected).but_actual_is(token))
        }
    }

    fn expect_among_others(&self, expected: TokenKind, other_expected: Expected) -> Result<Token> {
        let token = self.current_token();
        if token.kind == expected {
            Ok(token.clone())
        } else {
            self.error(|| other_expected.added(expected).but_actual_is(token))
        }
    }

    fn consume(&mut self, token_kind: TokenKind) -> Result<Token> {
        let token = self.expect(token_kind)?;
        self.advance();
        Ok(token)
    }

    // @Note bad name
    fn consume_after_expecting(
        &mut self,
        token_kind: TokenKind,
        other_expected: Expected,
    ) -> Result<Token> {
        let token = self.expect_among_others(token_kind, other_expected)?;
        self.advance();
        Ok(token)
    }

    fn consume_identifier(&mut self) -> Result<Identifier> {
        self.consume(TokenKind::Identifier)
            .map(|identifier| identifier.try_into().unwrap())
    }

    /// A general identifier includes (alphanumeric) identifiers and punctuation.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// General-Identifier ::= #Identifier | #Punctuation
    /// ```
    fn consume_general_identifier(&mut self) -> Result<Identifier> {
        use TokenKind::*;
        match self.current_token().kind {
            Identifier | Punctuation => {
                let identifier = self.current_token_into_identifier();
                self.advance();
                Ok(identifier)
            }
            _ => self.error(|| {
                expected_one_of![Identifier, Punctuation].but_actual_is(self.current_token())
            }),
        }
    }

    /// Indicate whether the given token was consumed.
    ///
    /// Conceptually equivalent to `self.manually_reflect(|this| this.consume(..)).is_ok()`
    /// but more memory-efficient as it does not clone the consumed token.
    #[must_use]
    fn has_consumed(&mut self, kind: TokenKind) -> bool {
        if self.current_token().kind == kind {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Conceptually equivalent to `Self::consume(..).map(Spanning::span).ok()` but more memory-efficient as
    /// it neither constructs a [crate::diagnostics::Diagnostic] nor clones the consumed token.
    fn consume_span(&mut self, kind: TokenKind) -> Option<Span> {
        if self.current_token().kind == kind {
            let span = self.current_token().span;
            self.advance();
            Some(span)
        } else {
            None
        }
    }

    /// Try to turn the current token into an identifier.
    ///
    /// May panic if the token is neither an identifier nor punctuation.
    fn current_token_into_identifier(&self) -> ast::Identifier {
        self.current_token().clone().try_into().unwrap()
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    fn current_token(&self) -> &Token {
        &self.tokens[self.index]
    }

    fn current_token_is_delimiter(&self, delimiters: &[Delimiter]) -> bool {
        let queried_token_kind = self.current_token().kind;

        delimiters
            .iter()
            .map(|delimiter| <&TokenKind>::from(delimiter))
            .any(|&token| token == queried_token_kind)
    }

    fn succeeding_token(&self) -> &Token {
        &self.tokens[self.index + 1]
    }

    /// Parse a declaration.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Declaration ::= (Attribute Line-Break*)* Naked-Declaration
    /// Naked-Declaration ::=
    ///     | Value-Declaration
    ///     | Data-Declaration
    ///     | Module-Declaration
    ///     | Crate-Declaration
    ///     | Use-Declaration
    /// Crate-Declaration ::= "crate" #Identifier Line-Break
    /// ```
    // @Task re-add attribute groups (needs syntax proposals)
    fn parse_declaration(&mut self) -> Result<Declaration> {
        use TokenKind::*;
        let attributes = self.parse_attributes(SkipLineBreaks::Yes)?;

        let span = self.current_token().span;
        match self.current_token().kind {
            Identifier => {
                let identifier = self.current_token_into_identifier();
                self.advance();
                self.finish_parse_value_declaration(identifier, attributes)
            }
            Data => {
                self.advance();
                self.finish_parse_data_declaration(span, attributes)
            }
            Module => {
                self.advance();
                self.finish_parse_module_declaration(span, attributes)
            }
            // @Task remove
            Crate => {
                self.advance();
                let binder = self.consume_identifier()?;
                self.consume(LineBreak)?;

                Ok(decl! {
                    Crate {
                        attributes,
                        span.merge(&binder.span);
                        binder,
                    }
                })
            }
            Use => {
                self.advance();
                self.finish_parse_use_declaration(span, attributes)
            }
            _ => self.error(|| Expected::Declaration.but_actual_is(self.current_token())),
        }
    }

    /// Parse attributes.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Attribute ::= Regular-Attribute | Documentation-Comment
    /// ```
    fn parse_attributes(&mut self, skip_line_breaks: SkipLineBreaks) -> Result<Attributes> {
        use TokenKind::*;
        let mut attributes = Attributes::default();

        loop {
            let span = self.current_token().span;
            attributes.push(match self.current_token().kind {
                At => {
                    self.advance();
                    let attribute = self.finish_parse_regular_attribute(span)?;
                    if matches!(skip_line_breaks, SkipLineBreaks::Yes) {
                        while self.has_consumed(LineBreak) {}
                    }
                    attribute
                }
                DocumentationComment => {
                    self.advance();
                    let attribute = Attribute {
                        binder: ast::Identifier::new(
                            crate::Atom::from("documentation"),
                            Span::SHAM,
                        ),
                        span,
                        arguments: smallvec![AttributeArgument::new(
                            Span::SHAM,
                            AttributeArgumentKind::TextEncodedInSpan,
                        )],
                    };
                    if matches!(skip_line_breaks, SkipLineBreaks::Yes) {
                        while self.has_consumed(LineBreak) {}
                    }
                    attribute
                }
                _ => break,
            });
        }

        Ok(attributes)
    }

    /// Finish parsing a regular attribute.
    ///
    /// Regular attributes do not include documentation comments.
    ///
    /// ## Grammar
    ///
    /// Note: The grammar is not complete yet since we cannot represent the
    /// arguments of `@if` yet which are the most complex.
    ///
    /// ```ebnf
    /// Regular-Attribute ::= "@" (#Identifier | "(" #Identifier Attribute-Argument* ")")
    /// ```
    fn finish_parse_regular_attribute(&mut self, keyword_span: Span) -> Result<Attribute> {
        use TokenKind::*;
        let mut span = keyword_span;

        let binder;
        let mut arguments = SmallVec::new();

        match self.current_token().kind {
            Identifier => {
                binder = span.merging(self.current_token_into_identifier());
                self.advance();
            }
            OpeningRoundBracket => {
                self.advance();
                binder = self.consume_identifier()?;

                while self.current_token().kind != ClosingRoundBracket {
                    arguments.push(self.parse_attribute_argument()?);
                }

                // @Note constructs and wastes a diagnostic
                // @Task transform above while into a loop
                // @Beacon :while_current_not
                span.merging(self.consume(ClosingRoundBracket).unwrap());
            }
            _ => {
                return self.error(|| {
                    expected_one_of![Identifier, OpeningRoundBracket]
                        .but_actual_is(self.current_token())
                        .with_note("`@` introduces attributes")
                });
            }
        }

        Ok(Attribute {
            binder,
            arguments,
            span,
        })
    }

    /// Parse an argument of an attribute.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Attribute-Argument ::= Lower-Attribute-Argument | "(" #Identifier Lower-Attribute-Argument ")"
    /// Lower-Attribute-Argument ::= Path | #Number-Literal | #Text-Literal
    /// ```
    fn parse_attribute_argument(&mut self) -> Result<AttributeArgument> {
        use TokenKind::*;
        Ok(match self.current_token().kind {
            kind if kind.is_path_head() => {
                let path = self.parse_path()?;
                attrarg! { Path(path.span(); path) }
            }
            NumberLiteral => {
                let token = self.current_token().clone();
                self.advance();
                attrarg! { NumberLiteral(token.span; token.number_literal().unwrap()) }
            }
            TextLiteral => {
                let token = self.current_token().clone();
                let span = token.span;
                let text_literal = token
                    .text_literal()
                    .unwrap()
                    .or_else(|error| self.error(|| error))?;

                self.advance();
                attrarg! { TextLiteral(span; text_literal) }
            }
            OpeningRoundBracket => {
                let mut span = self.current_token().span;
                self.advance();
                let binder = self.consume_identifier()?;
                let value = self.parse_attribute_argument()?;
                span.merging(&self.consume(ClosingRoundBracket)?);
                attrarg! { Named(span; NamedAttributeArgument { binder, value }) }
            }
            _ => {
                return self
                    .error(|| Expected::AttributeArgument.but_actual_is(self.current_token()))
            }
        })
    }

    /// Finish parsing a value declaration.
    ///
    /// The leading identifier should have already parsed beforehand.
    /// The span does not include the trailing line break.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Value-Declaration ::=
    ///     #Identifier
    ///     Parameters Type-Annotation?
    ///     ("=" Terminated-Expression | Line-Break)
    /// ```
    fn finish_parse_value_declaration(
        &mut self,
        binder: Identifier,
        attributes: Attributes,
    ) -> Result<Declaration> {
        use TokenKind::*;
        let mut span = binder.span;

        let parameters = span.merging(self.parse_parameters(&STANDARD_DECLARATION_DELIMITERS)?);
        let type_annotation = span.merging(self.parse_optional_type_annotation()?);

        let body = if self.has_consumed(Equals) {
            Some(span.merging(self.parse_terminated_expression()?))
        } else {
            self.consume(LineBreak)?;
            None
        };

        Ok(decl! {
            Value {
                attributes,
                span;
                binder,
                parameters,
                type_annotation,
                body,
            }
        })
    }

    /// Finish parsing a data declaration.
    ///
    /// The keyword `data` should have already been parsed beforehand.
    /// The span does not include the trailing line break.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Data-Declaration ::=
    ///     "data" #Identifier
    ///     Parameters Type-Annotation?
    ///     (Line-Break | "of" "{" (Line-Break | Constructor)* "}")
    /// ```
    fn finish_parse_data_declaration(
        &mut self,
        keyword_span: Span,
        attributes: Attributes,
    ) -> Result<Declaration> {
        use TokenKind::*;
        let mut span = keyword_span;

        let binder = span.merging(self.consume_identifier()?);
        let parameters = span.merging(self.parse_parameters(&[
            Delimiter::TypeAnnotationPrefix,
            Of.into(),
            LineBreak.into(),
        ])?);
        let type_annotation = span.merging(self.parse_optional_type_annotation()?);

        let constructors = match self.current_token().kind {
            Of => {
                span.merging(self.current_token().span);
                self.advance();

                let mut constructors = Vec::new();

                self.parse_block(|this| {
                    constructors.push(this.parse_constructor()?);
                    Ok(())
                })?;

                span.merging(constructors.last());

                Some(constructors)
            }
            LineBreak => {
                self.advance();
                None
            }
            _ => {
                return self.error(|| {
                    expected_one_of![Delimiter::DefinitionPrefix, LineBreak, Expected::Expression]
                        .but_actual_is(self.current_token())
                });
            }
        };

        Ok(decl! {
            Data {
                attributes,
                keyword_span.merge(span);
                binder,
                parameters,
                type_annotation,
                constructors,
            }
        })
    }

    /// Finish parsing module declaration.
    ///
    /// This is either a module declaration or an external module declaration.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Module-Declaration ::=
    ///     | Header
    ///     | "module" #Identifier (Line-Break | "of" "{" (Line-Break | Declaration)* "}")
    /// Header ::= "module" Line-Break
    /// ```
    fn finish_parse_module_declaration(
        &mut self,
        keyword_span: Span,
        attributes: Attributes,
    ) -> Result<Declaration> {
        use TokenKind::*;
        let mut span = keyword_span;

        if self.has_consumed(LineBreak) {
            return Ok(decl! { Header { attributes, span } });
        }

        let binder = span.merging(self.consume_identifier()?);

        match self.current_token().kind {
            // external module declaration
            LineBreak => {
                self.advance();
                Ok(decl! {
                    Module {
                        attributes,
                        span;
                        binder,
                        file: self.file.clone(),
                        declarations: None,
                    }
                })
            }
            Of => {
                self.advance();
                let mut declarations = Vec::new();

                self.parse_block(|this| {
                    declarations.push(this.parse_declaration()?);
                    Ok(())
                })?;

                // @Bug span is wrong: we need to store the last token's span: dedentation/line break
                Ok(decl! {
                    Module {
                        attributes,
                        span;
                        binder,
                        file: self.file.clone(),
                        declarations: Some(declarations),
                    }
                })
            }
            _ => self.error(|| expected_one_of![LineBreak, Of].but_actual_is(self.current_token())),
        }
    }

    /// Parse a file module.
    ///
    /// It takes the identifier of the module as an argument.
    pub fn parse(&mut self, binder: Identifier) -> Results<Declaration> {
        self.parse_top_level(binder).map_err(|_| self.errors.take())
    }

    /// Parse the "top level" aka the body of a module file.
    ///
    /// It takes the identifier of the module as an argument.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Top-Level ::= (Line-Break | Declaration)* #End-Of-Input
    /// ```
    fn parse_top_level(&mut self, binder: Identifier) -> Result<Declaration> {
        use TokenKind::*;

        let mut declarations = Vec::new();

        loop {
            if self.has_consumed(LineBreak) {
                continue;
            }

            if self.has_consumed(EndOfInput) {
                break Ok(decl! {
                    Module {
                        Attributes::new(),
                        self.file.span;
                        binder,
                        file: self.file.clone(),
                        declarations: Some(declarations)
                    }
                });
            }

            declarations.push(self.parse_declaration()?);
        }
    }

    fn parse_block(&mut self, mut subparser: impl FnMut(&mut Self) -> Result<()>) -> Result {
        use TokenKind::*;

        let _ = self.consume(OpeningCurlyBracket)?;

        loop {
            // @Note necessary I guess in cases where we have Line-Break ((Comment)) Line-Break
            if self.has_consumed(LineBreak) {
                continue;
            }

            if self.has_consumed(ClosingCurlyBracket) {
                break Ok(());
            }

            subparser(self)?;
        }
    }

    /// Finish parsing a use-declaration.
    ///
    /// The keyword `use` should have already been parsed.
    /// The span does not contain the trailing line break.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Use-Declaration ::= "use" Use-Path-Tree Line-Break
    /// ```
    fn finish_parse_use_declaration(
        &mut self,
        keyword_span: Span,
        attributes: Attributes,
    ) -> Result<Declaration> {
        let bindings = self.parse_use_path_tree(&[TokenKind::LineBreak.into()])?;
        self.consume(TokenKind::LineBreak)?;

        Ok(decl! {
            Use {
                attributes,
                keyword_span.merge(&bindings);
                bindings,
            }
        })
    }

    /// Parse a use-path tree.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Use-Path-Tree ::=
    ///     | Path
    ///     | Path "." "(" (Use-Path-Tree | "(" Renaming ")")* ")"
    ///     | Renaming
    /// Renaming ::= Path "as" General-Identifier
    /// ```
    // @Task rewrite this following a simpler grammar mirroring expression applications
    fn parse_use_path_tree(&mut self, delimiters: &[Delimiter]) -> Result<UsePathTree> {
        use TokenKind::*;

        let mut path = self.parse_first_path_segment()?;

        while self.has_consumed(Dot) {
            match self.current_token().kind {
                Identifier | Punctuation => {
                    let identifier = self.current_token_into_identifier();
                    self.advance();
                    path.segments.push(identifier);
                }
                OpeningRoundBracket => {
                    let mut span = self.current_token().span;
                    self.advance();

                    let mut bindings = Vec::new();

                    while self.current_token().kind != ClosingRoundBracket {
                        if let Ok(bracket) =
                            self.manually_reflect(|this| this.consume(OpeningRoundBracket))
                        {
                            let mut span = bracket.span;

                            let target = self.parse_path()?;
                            self.consume(As)?;
                            let binder = self.consume_general_identifier()?;
                            span.merging(&self.consume(ClosingRoundBracket)?);

                            bindings.push(UsePathTree::new(
                                span,
                                UsePathTreeKind::Single {
                                    target,
                                    binder: Some(binder),
                                },
                            ));
                        } else {
                            // @Note @Bug this is really really fragile=non-extensible!
                            bindings.push(self.parse_use_path_tree(&[
                                OpeningRoundBracket.into(),
                                ClosingRoundBracket.into(),
                                Identifier.into(),
                                Punctuation.into(),
                                Self_.into(),
                                Super.into(),
                                Crate.into(),
                            ])?);
                        }
                    }

                    // @Note constructs and wastes a diagnostic
                    // @Task transform above while into a loop
                    // @Beacon :while_current_not
                    span.merging(&self.consume(ClosingRoundBracket).unwrap());

                    return Ok(UsePathTree::new(
                        path.span().merge(span),
                        UsePathTreeKind::Multiple { path, bindings },
                    ));
                }
                _ => {
                    return self.error(|| {
                        expected_one_of![Identifier, Punctuation, OpeningRoundBracket]
                            .but_actual_is(self.current_token())
                    })
                }
            }
        }

        // @Question is there a grammar transformation to a self-contained construct
        // instead of a delimited one?
        let binder = if self.current_token_is_delimiter(delimiters) {
            None
        } else if self.has_consumed(As) {
            self.current_token();
            Some(self.consume_general_identifier()?)
        } else {
            return self.error(|| {
                delimiters_with_expected(delimiters, Some(As.into()))
                    .but_actual_is(self.current_token())
            });
        };

        Ok(UsePathTree::new(
            path.span().merge(&binder),
            UsePathTreeKind::Single {
                target: path,
                binder,
            },
        ))
    }

    /// Parse a (value) constructor.
    ///
    /// The span does not include the trailing line break.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Constructor ::=
    ///     (Attribute Line-Break*)*
    ///     #Identifier Parameters Type-Annotation?
    ///     ("=" Terminated-Expression)? Line-Break
    /// ```
    fn parse_constructor(&mut self) -> Result<Declaration> {
        use TokenKind::*;

        let attributes = self.parse_attributes(SkipLineBreaks::Yes)?;

        let binder = self.consume_identifier()?;
        let mut span = binder.span;

        let parameters = span.merging(self.parse_parameters(&STANDARD_DECLARATION_DELIMITERS)?);

        let type_annotation = span.merging(self.parse_optional_type_annotation()?);

        let body = if self.has_consumed(Equals) {
            Some(span.merging(self.parse_terminated_expression()?))
        } else {
            self.consume(LineBreak)?;
            None
        };

        Ok(decl! {
            Constructor {
                attributes,
                span;
                binder,
                parameters,
                type_annotation,
                body,
            }
        })
    }

    /// Parse an expression.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Expression ::= Pi-Type-Literal-Or-Lower
    /// ```
    // @Task parse sigma type literals
    // @Task once that has been completed, inline Self::parse_pi_type_literal_or_lower
    fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_pi_type_literal_or_lower()
    }

    /// Parse a pi-type literal or a lower expression.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// ; among other things, the grammar for pretty-printers differs from the one for parsers
    /// ; in that `Pi-Type-Literal-Or-Lower` also includes several complex (in the sense that they
    /// ; contain further expressions) `Lower-Expression`s namely let/in, use/in, lambda literals,
    /// ; case analyses and do blocks (not sure about sequence literals)
    /// Pi-Type-Literal-Or-Lower ::=
    ///     (Designated-Pi-Type-Domain | Application-Or-Lower)
    ///     "->" Pi-Type-Literal-Or-Lower
    /// Designated-Pi-Type-Domain ::=
    ///     Explicitness
    ///     "(" Parameter-Aspect #Identifier Type-Annotation ")"
    /// ```
    fn parse_pi_type_literal_or_lower(&mut self) -> Result<Expression> {
        use TokenKind::*;

        let domain = self
            .reflect(|this| {
                let explicitness = this.parse_optional_implicitness();
                let mut span = this
                    .consume(OpeningRoundBracket)?
                    .span
                    .merge_into(explicitness);

                let aspect = this.parse_parameter_aspect();
                let binder = this.consume_identifier()?;
                // @Question should this be fatal in respect to reflecting?
                let domain = this.parse_type_annotation(ClosingRoundBracket.into())?;

                span.merging(&this.consume(ClosingRoundBracket)?);

                Ok(Spanned::new(
                    span,
                    Domain {
                        explicitness: explicitness.into(),
                        aspect,
                        binder: Some(binder),
                        expression: domain,
                    },
                ))
            })
            .or_else(|_| -> Result<_> {
                // @Question should we parse `lazy` here too to allow for unnamed laziness
                // which is reasonable?
                let domain = self.parse_application_or_lower()?;
                Ok(Spanned::new(
                    domain.span,
                    Domain {
                        explicitness: Explicit,
                        aspect: default(),
                        binder: None,
                        expression: domain,
                    },
                ))
            })?;

        if self.current_token().kind == ThinArrowRight {
            self.advance();

            let mut span = domain.span;
            let codomain = span.merging(self.parse_pi_type_literal_or_lower()?);

            Ok(expr! {
                PiTypeLiteral {
                    Attributes::new(),
                    span;
                    domain: domain.kind,
                    codomain,
                }
            })
        }
        // the case where we don't actually have a pi type literal but merely
        // an application or lower
        else if domain.kind.binder.is_none() {
            Ok(domain.kind.expression)
        } else {
            self.error(|| {
                Expected::Token(ThinArrowRight)
                    .but_actual_is(self.current_token())
                    .with_labeled_secondary_span(domain, "the start of a pi type literal")
            })
        }
    }

    /// Parse the aspect of a pi type parameter.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Parameter-Aspect ::= Laziness Fieldness
    /// Laziness ::= "lazy"?
    /// Fieldness ::= "::"?
    /// ```
    fn parse_parameter_aspect(&mut self) -> ParameterAspect {
        use TokenKind::*;

        let laziness = self.consume_span(Lazy);
        let fieldness = self.consume_span(DoubleColon);

        ParameterAspect {
            laziness,
            fieldness,
        }
    }

    /// Parse an application or a lower expression.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Application-Or-Lower ::= Lower-Expression Argument*
    /// Argument ::=
    ///     | Explicitness Lower-Expression
    ///     | Explicitness "(" (#Identifier "=")? Expression ")"
    ///
    /// ; ; left-recursive version unsuitable for the recursive descent parser
    /// ; ; but indeed usable for pretty-printers:
    /// ;
    /// ; Application-Or-Lower ::= Application-Or-Lower? Argument*
    /// ; Argument ::=
    /// ;     | Lower-Expression
    /// ;     | Explicitness "(" (#Identifier "=")? Expression ")"
    /// ```
    fn parse_application_or_lower(&mut self) -> Result<Expression> {
        self.parse_application_like_or_lower()
    }

    /// Parse a lower expression.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Lower-Expression ::= Attribute* Naked-Lower-Expression
    /// ; @Task rename into Field-Or-Lower
    /// Naked-Lower-Expression ::= Lowest-Expression ("::" Identifier)*
    /// Lowest-Expression ::=
    ///     | Path
    ///     | "Type"
    ///     | #Number-Literal
    ///     | #Text-Literal
    ///     | Typed-Hole
    ///     | Let-In
    ///     | Use-In
    ///     | Lambda-Literal
    ///     | Case-Analysis
    ///     | Do-Block
    ///     | Sequence-Literal
    ///     | "(" Expression ")"
    /// Typed-Hole ::= "?" #Identifier
    /// Sequence-Literal ::= "[" Lower-Expression* "]"
    /// ```
    fn parse_lower_expression(&mut self) -> Result<Expression> {
        use TokenKind::*;

        let attributes = self.parse_attributes(SkipLineBreaks::No)?;

        let mut span = self.current_token().span;
        // @Task don't pass attributes down but make them empty at first, then update the attributes
        // dependeninh on if it's a field or not
        let mut expression = match self.current_token().kind {
            kind if kind.is_path_head() => {
                let path = self.parse_path()?;
                expr! { Path(default(), path.span(); path) }
            }
            Type => {
                self.advance();
                expr! { TypeLiteral { default(), span } }
            }
            NumberLiteral => {
                let token = self.current_token().clone();
                self.advance();
                expr! {
                    NumberLiteral(default(), token.span; token.number_literal().unwrap())
                }
            }
            TextLiteral => {
                let token = self.current_token().clone();
                self.advance();

                let span = token.span;
                let text_literal = token
                    .text_literal()
                    .unwrap()
                    .or_else(|error| self.error(|| error))?;

                expr! { TextLiteral(default(), span; text_literal) }
            }
            QuestionMark => {
                self.advance();
                let tag = self.consume_identifier()?;
                expr! { TypedHole { default(), span.merge(&tag); tag } }
            }
            Let => {
                self.advance();
                self.finish_parse_let_in(span)?
            }
            Use => {
                self.advance();
                self.finish_parse_use_in(span)?
            }
            Backslash => {
                self.advance();
                self.finish_parse_lambda_literal(span)?
            }
            Case => {
                self.advance();
                self.finish_parse_case_analysis(span)?
            }
            Do => {
                self.advance();
                self.finish_parse_do_block(span)?
            }
            OpeningSquareBracket => {
                self.advance();

                let mut elements = Vec::new();

                while self.current_token().kind != ClosingSquareBracket {
                    elements.push(self.parse_lower_expression()?);
                }

                span.merging(self.current_token());
                self.advance();

                expr! { SequenceLiteral { default(), span; elements } }
            }
            OpeningRoundBracket => {
                self.advance();

                let mut expression = self.parse_expression()?;

                span.merging(self.consume(ClosingRoundBracket)?);
                expression.span = span;

                expression
            }
            _ => {
                return self.error(|| {
                    Expected::Expression
                        .but_actual_is(self.current_token())
                        // @Beacon @Note this is a prime example for a situation where we can
                        // make a parsing error non-fatal: we can just skip the `->` and keep
                        // parsing w/o introducing too many (any?) useless/confusing consequential
                        // errors!
                        .when(self.current_token().kind == ThinArrowRight, |this| {
                            this.with_help(BRACKET_POTENTIAL_PI_TYPE_LITERAL)
                        })
                });
            }
        };

        let mut attributes = Some(attributes);

        while self.has_consumed(DoubleColon) {
            let member = self.consume_identifier()?;

            expression = expr! {
                Field {
                    attributes.take().unwrap_or_default(), expression.span.merge(&member);
                    base: expression,
                    member,
                }
            }
        }

        if let Some(attributes) = attributes {
            expression.attributes.extend(attributes);
        }

        Ok(expression)
    }

    /// Parse a path.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Path ::= Path-Head ("." General-Identifier)*
    /// Path-Head ::= Path-Hanger | General-Identifier
    /// Path-Hanger ::= "crate" | "super" | "self"
    /// ```
    fn parse_path(&mut self) -> Result<Path> {
        let mut path = self.parse_first_path_segment()?;

        while self.has_consumed(TokenKind::Dot) {
            path.segments.push(self.consume_general_identifier()?);
        }

        Ok(path)
    }

    /// Parse the first segment of a path.
    fn parse_first_path_segment(&mut self) -> Result<Path> {
        use TokenKind::*;
        let path = match self.current_token().kind {
            Identifier | Punctuation => Path::try_from_token(self.current_token().clone()).unwrap(),
            Crate | Super | Self_ => Path::hanger(self.current_token().clone()),
            _ => return self.error(|| Expected::Path.but_actual_is(self.current_token())),
        };
        self.advance();
        Ok(path)
    }

    /// Finish parsing a lambda literal expression.
    ///
    /// The initial `\` should have already been parsed beforehand.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Lambda-Literal ::= "\" Parameters Type-Annotation? "=>" Expression
    /// ```
    fn finish_parse_lambda_literal(&mut self, keyword_span: Span) -> Result<Expression> {
        let mut span = keyword_span;
        let parameters =
            self.parse_parameters(&[Delimiter::TypeAnnotationPrefix, TokenKind::WideArrow.into()])?;
        let body_type_annotation = self.parse_optional_type_annotation()?;
        self.consume(TokenKind::WideArrow)?;
        let body = span.merging(self.parse_expression()?);

        Ok(expr! {
            LambdaLiteral {
                Attributes::new(),
                span;
                parameters,
                body_type_annotation,
                body,
            }
        })
    }

    /// Finish parsing an let/in expression.
    ///
    /// The initial `let` should have already been parsed beforehand.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Let-In ::=
    ///     "let" #Identifier Parameters Type_Annotation?
    ///     ("=" Expression)?
    ///     "in" Line-Break? Expression
    /// ```
    // @Task allow omitting the `in` at the end of the line (@Note we might want to restrict it to
    // scopes of type let/in and use/in; is that still context-free?)
    // @Note however (at least in the future): `let x = start\n    end in 0` should mean that the
    // "expression" of the let/in is the application `start end` and `let x = start\nend in 0`
    // (on its own) w/o indentation is a syntax error and `let x = start\nend` is a let/in followed
    // by a scope (w/o `in`) being the identifier `end`
    // @Task allow writing `in` at the start of the next line (unless the previous line
    // already had one ofc)
    // @Task skip duplicate `=`, `in`, `:`s (throw an error for each one)
    fn finish_parse_let_in(&mut self, span_of_let: Span) -> Result<Expression> {
        use TokenKind::*;

        let mut span = span_of_let;
        let binder = self.consume_identifier()?;
        let parameters = self.parse_parameters(&[
            Delimiter::TypeAnnotationPrefix,
            Delimiter::DefinitionPrefix,
            In.into(),
            // next up: line break (maybe)
        ])?;
        let type_annotation = self.parse_optional_type_annotation()?;

        let expression = if self.has_consumed(Equals) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume(In)?;
        let _ = self.has_consumed(LineBreak);
        let scope = span.merging(self.parse_expression()?);

        Ok(expr! {
            LetIn {
                Attributes::new(),
                span;
                binder,
                parameters,
                type_annotation,
                expression,
                scope,
            }
        })
    }

    /// Finish parsing a use/in expression.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Use-In ::= "use" Use-Path-Tree "in" Line-Break? Expression
    /// ```
    fn finish_parse_use_in(&mut self, span_of_use: Span) -> Result<Expression> {
        let bindings = self.parse_use_path_tree(&[TokenKind::In.into()])?;
        self.consume(TokenKind::In)?;
        let _ = self.has_consumed(TokenKind::LineBreak);
        let scope = self.parse_expression()?;

        Ok(expr! {
            UseIn {
                Attributes::new(),
                span_of_use.merge(&scope);
                bindings,
                scope,
            }
        })
    }

    /// Finish parsing a case analysis expression.
    ///
    /// The initial `case` should have already been parsed beforehand.
    ///
    /// ## Grammar
    ///
    ///
    /// ```ebnf
    /// Case-Analysis ::= "case" Expression "of" "{" Case* "}"
    /// Case ::= Pattern "=>" Expression
    /// ```
    fn finish_parse_case_analysis(&mut self, span_of_case: Span) -> Result<Expression> {
        use TokenKind::*;
        let mut span = span_of_case;

        let scrutinee = self.parse_expression()?;
        span.merging(self.consume(Of)?);

        let mut cases = Vec::new();

        let _ = self.consume(OpeningCurlyBracket)?;

        // @Task use parse_block function for this (but don't trash the span!)

        while self.current_token().kind != ClosingCurlyBracket {
            let pattern = self.parse_pattern()?;
            self.consume(WideArrow)?;
            let body = self.parse_terminated_expression()?;

            cases.push(ast::Case { pattern, body });
        }

        // @Beacon :while_current_not
        span.merging(self.current_token());
        self.advance();

        Ok(expr! {
            CaseAnalysis {
                Attributes::new(),
                span;
                scrutinee,
                cases,
            }
        })
    }

    /// Finish parsing a do block.
    ///
    /// The keyword `do` should have already been consumed.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Do-Block ::= "do" "{" Statement* "}"
    /// Statement ::= Let-Statement | Use-Declaration | Bind-Statement | Expression-Statement
    /// Let-Statement ::= "let" Value-Declaration
    /// Bind-Statement ::= #Identifier Type-Annotation? "<-" Expression Line-Break
    /// Expression-Statement ::= Expression Line-Break
    /// ```
    ///
    /// Bind statements are the worst right now. We need to look ahead for `:` (type annotation)
    /// or `<-` to differenciate them from expressions. Maybe there is prefix-oriented syntax
    /// we could switch to like `!x = …` or `set x = …`. The latter look-ahead is not much of an
    /// issue, `:` is a bad *but only in case* of adding type annotation expressions (not that likely
    /// as they clash with other syntactic elements like pi literals).
    fn finish_parse_do_block(&mut self, span_of_do: Span) -> Result<Expression> {
        use TokenKind::*;
        let mut span = span_of_do;
        let mut statements = Vec::new();

        let _ = self.consume(OpeningCurlyBracket)?;

        while self.current_token().kind != ClosingCurlyBracket {
            // @Note necessary I guess in cases where we have Line-Break ((Comment)) Line-Break
            if self.has_consumed(LineBreak) {
                continue;
            }

            statements.push(match self.current_token().kind {
                // @Task move to its own function
                Let => {
                    self.advance();
                    let binder = self.consume_identifier()?;
                    let parameters = self.parse_parameters(&[
                        Delimiter::TypeAnnotationPrefix,
                        Delimiter::DefinitionPrefix,
                    ])?;
                    let type_annotation = self.parse_optional_type_annotation()?;
                    self.consume(TokenKind::Equals)?;
                    let expression = self.parse_terminated_expression()?;
                    Statement::Let(LetStatement {
                        binder,
                        parameters,
                        type_annotation,
                        expression,
                    })
                }
                Use => {
                    self.advance();
                    let bindings = self.parse_use_path_tree(&[TokenKind::LineBreak.into()])?;
                    self.consume(LineBreak)?;
                    Statement::Use(ast::Use { bindings })
                }
                _ => {
                    if self.current_token().kind == Identifier
                        && matches!(self.succeeding_token().kind, Colon | ThinArrowLeft)
                    {
                        // @Task move to its own function
                        let binder = self.current_token_into_identifier();
                        self.advance();
                        let type_annotation = self.parse_optional_type_annotation()?;
                        self.consume(ThinArrowLeft)?;
                        let expression = self.parse_terminated_expression()?;
                        Statement::Bind(BindStatement {
                            binder,
                            type_annotation,
                            expression,
                        })
                    } else {
                        // @Task improve error diagnostics for an unexpected token to not only mention an
                        // expression was expected but also statements were
                        let expression = self.parse_expression()?;
                        self.consume(LineBreak)?;
                        Statement::Expression(expression)
                    }
                }
            });
        }

        // @Beacon :while_current_not
        span.merging(self.current_token());
        self.advance();

        Ok(expr! {
            DoBlock {
                Attributes::new(),
                span;
                statements,
            }
        })
    }

    /// Parse parameters until one of the given delimiters is encountered.
    ///
    /// One needs to specify delimiters to allow for better error diagnostics.
    /// A delimiter must not be [TokenKind::OpeningRoundBracket] or [TokenKind::Identifier].
    /// The delimiter list must be non-empty.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Parameters ::= Parameter-Group*
    /// ```
    fn parse_parameters(&mut self, delimiters: &[Delimiter]) -> Result<Parameters> {
        let mut parameters = Vec::new();

        while !self.current_token_is_delimiter(delimiters) {
            parameters.push(self.parse_parameter_group(delimiters)?);
        }

        Ok(parameters)
    }

    /// Parse a parameter group.
    ///
    /// Delimiters are taken as a parameter merely for constructing
    /// the Diagnostic.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Parameter-Group ::= Explicitness Naked-Parameter-Group
    /// Naked-Parameter-Group ::=
    ///     | #Identifier
    ///     | "(" Parameter-Aspect #Identifier+ Type-Annotation? ")"
    /// ```
    fn parse_parameter_group(&mut self, delimiters: &[Delimiter]) -> Result<ParameterGroup> {
        use TokenKind::*;

        let explicitness = self.parse_optional_implicitness();
        let mut span = self.current_token().span.merge_into(explicitness);

        match self.current_token().kind {
            Identifier => {
                let binder = self.current_token_into_identifier();
                self.advance();

                Ok(ParameterGroup {
                    explicitness: explicitness.into(),
                    aspect: default(),
                    parameters: smallvec![binder],
                    type_annotation: None,
                    span,
                })
            }
            OpeningRoundBracket => {
                self.advance();
                let aspect = self.parse_parameter_aspect();
                let mut parameters = SmallVec::new();

                parameters.push(self.consume_identifier()?);

                let delimiters = [Delimiter::TypeAnnotationPrefix, ClosingRoundBracket.into()];

                while !self.current_token_is_delimiter(&delimiters) {
                    parameters.push(self.consume_identifier()?);
                }

                let type_annotation = self.parse_optional_type_annotation()?;

                span.merging(self.consume(ClosingRoundBracket)?);

                Ok(ParameterGroup {
                    explicitness: explicitness.into(),
                    aspect,
                    parameters,
                    type_annotation,
                    span,
                })
            }
            _ => self.error(|| {
                delimiters_with_expected(delimiters, Some(Expected::Parameter))
                    .but_actual_is(self.current_token())
            }),
        }
    }

    /// Parse a pattern.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Pattern ::= Lower-Pattern Pattern-Argument*
    /// Pattern-Argument ::=
    ///     | Explicitness Lower-Pattern
    ///     | Explicitness "(" (#Identifier "=")? Pattern ")"
    /// ```
    // @Task add alternative precedence for formatting to the documentation above
    fn parse_pattern(&mut self) -> Result<Pattern> {
        self.parse_application_like_or_lower()
    }

    /// Parse a (de)application or something lower.
    // @Task rewrite this with a `delimiter: Delimiter`-parameter for better error messages!
    fn parse_application_like_or_lower<Expat: ExpressionOrPattern>(&mut self) -> Result<Expat> {
        use TokenKind::*;
        let mut callee = Expat::parse_lower(self)?;

        struct Argument<Expat> {
            explicitness: Explicitness,
            binder: Option<ast::Identifier>,
            expat: Expat,
        }

        let mut illegal_pi = None;

        while let Ok(argument) = self
            .reflect(|this| {
                // @Beacon @Question can pi_type_literal_was_used_as_lower_expression also happen here???
                // @Task use the span returned by parse_optional_implicitness
                let explicitness = this.parse_optional_implicitness();
                let expat = Expat::parse_lower(this)?;
                Ok(Spanned::new(
                    expat.span(),
                    Argument {
                        binder: None,
                        explicitness: explicitness.into(),
                        expat,
                    },
                ))
            })
            .or_else(|_| -> Result<_> {
                self.reflect(|this| {
                    let explicitness = this.parse_optional_implicitness();
                    let mut span = this.consume(OpeningRoundBracket)?.span;
                    span.merging_from(explicitness);

                    let binder = this.consume_identifier()?;

                    if Expat::IS_EXPRESSION && this.current_token().kind == Colon {
                        illegal_pi = Some(this.current_token().clone());
                        this.advance();
                    } else {
                        this.consume(Equals)?;
                    }

                    let argument = Expat::parse_lower(this)?;
                    span.merging(this.consume(ClosingRoundBracket)?);

                    Ok(Spanned::new(
                        span,
                        Argument {
                            explicitness: explicitness.into(),
                            binder: Some(binder),
                            expat: argument,
                        },
                    ))
                })
            })
        {
            if let Some(token) = illegal_pi {
                let explicitness = match argument.kind.explicitness {
                    Implicit => "an implicit",
                    Explicit => "a",
                };

                self.errors.borrow_mut().insert(
                    expected_one_of![Expected::Expression, Equals]
                        .but_actual_is(&token)
                        .with_labeled_secondary_span(
                            &argument,
                            format!("this is treated as {explicitness} function argument,\nnot as the domain of a pi type literal"),
                        )
                        .with_help(BRACKET_POTENTIAL_PI_TYPE_LITERAL),
                );

                return Err(());
            }

            let span = callee.span().merge(&argument);

            callee = Expat::application_like(
                callee,
                argument.kind.expat,
                argument.kind.explicitness,
                argument.kind.binder,
                span,
            );
        }

        Ok(callee)
    }

    /// Parse a lower pattern.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Lower-Pattern ::= Attribute* Naked-Lower-Pattern
    /// Naked-Lower-Pattern ::=
    ///     | Path
    ///     | #Number-Literal
    ///     | #Text-Literal
    ///     | Binder
    ///     | Sequence-Literal-Pattern
    ///     | "(" Pattern ")"
    /// Binder ::= "\" #Identifier
    /// Sequence-Literal-Pattern ::= "[" Lower-Pattern* "]"
    /// ```
    fn parse_lower_pattern(&mut self) -> Result<Pattern> {
        use TokenKind::*;
        let attributes = self.parse_attributes(SkipLineBreaks::No)?;

        let mut span = self.current_token().span;
        match self.current_token().kind {
            kind if kind.is_path_head() => self.parse_path().map(|path| {
                pat! { Path(attributes, path.span(); path) }
            }),
            NumberLiteral => {
                let token = self.current_token().clone();
                self.advance();
                Ok(pat! {
                    NumberLiteral(attributes, token.span; token.number_literal().unwrap())
                })
            }
            TextLiteral => {
                let token = self.current_token().clone();
                self.advance();
                let span = token.span;
                let text_literal = token
                    .text_literal()
                    .unwrap()
                    .or_else(|error| self.error(|| error))?;

                Ok(pat! { TextLiteral(attributes, span; text_literal) })
            }
            Backslash => {
                self.advance();
                self.consume_identifier()
                    .map(|binder| pat! { Binder { attributes, span.merge(&binder); binder } })
            }
            OpeningSquareBracket => {
                self.advance();

                let mut elements = Vec::new();

                while self.current_token().kind != ClosingSquareBracket {
                    elements.push(self.parse_lower_pattern()?);
                }

                span.merging(self.current_token());
                self.advance();

                Ok(pat! { SequenceLiteralPattern { attributes, span; elements } })
            }
            OpeningRoundBracket => {
                self.advance();
                let mut pattern = self.parse_pattern()?;
                span.merging(self.consume(ClosingRoundBracket)?);
                pattern.span = span;
                pattern.attributes.extend(attributes);
                Ok(pattern)
            }
            _ => self.error(|| Expected::Pattern.but_actual_is(self.current_token())),
        }
    }

    /// Parse a type annotation.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Type-Annotation ::= ":" Expression
    /// ```
    fn parse_type_annotation(&mut self, other_expected: Expected) -> Result<Expression> {
        // @Note that the error message that can be thrown by this method will actually
        // very likely to certainly not show up in the final report since it gets swallowed
        // by reflect (the branch do-not-reflect experiments with this stuff and makes the
        // below actually show up)
        self.consume_after_expecting(TokenKind::Colon, other_expected)?;
        self.reflect(Self::parse_expression)
    }

    /// Parse an optional type annotation.
    fn parse_optional_type_annotation(&mut self) -> Result<Option<Expression>> {
        self.has_consumed(TokenKind::Colon)
            .then(|| self.parse_expression())
            .transpose()
    }

    /// Parse an expression terminated by a line break or dedentation.
    ///
    /// A line break is parsed, dedentation is not.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Terminated-Expression ::= Expression (<! "{" Line-Break)?
    /// ```
    // @Question just inline/remove?
    fn parse_terminated_expression(&mut self) -> Result<Expression> {
        let expression = self.parse_expression()?;
        // @Note special-casing is also called programming hackily
        if self.current_token().kind != TokenKind::ClosingCurlyBracket {
            self.consume(TokenKind::LineBreak)?;
        }
        Ok(expression)
    }

    /// Consume the explicitness symbol.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Explicitness ::= "'"?
    /// ```
    fn parse_optional_implicitness(&mut self) -> SpannedExplicitness {
        match self.current_token().kind {
            TokenKind::SingleQuote => {
                let span = self.current_token().span;
                self.advance();
                SpannedExplicitness::Implicit { marker: span }
            }
            _ => SpannedExplicitness::Explicit,
        }
    }
}

impl Warn for Parser<'_> {
    fn diagnostics(&mut self) -> &mut Diagnostics {
        &mut self.warnings
    }
}

/// Abstraction over expressions and patterns.
// @Task consider replacing this with an enum
trait ExpressionOrPattern: Sized + Spanning {
    fn application_like(
        callee: Self,
        argument: Self,
        explicitness: Explicitness,
        binder: Option<Identifier>,
        span: Span,
    ) -> Self;
    fn parse(parser: &mut Parser<'_>) -> Result<Self>;
    fn parse_lower(parser: &mut Parser<'_>) -> Result<Self>;

    const IS_EXPRESSION: bool;
}

impl ExpressionOrPattern for Expression {
    fn application_like(
        callee: Self,
        argument: Self,
        explicitness: Explicitness,
        binder: Option<Identifier>,
        span: Span,
    ) -> Self {
        expr! { Application { Attributes::new(), span; callee, argument, explicitness, binder } }
    }
    fn parse(parser: &mut Parser<'_>) -> Result<Self> {
        parser.parse_expression()
    }
    fn parse_lower(parser: &mut Parser<'_>) -> Result<Self> {
        parser.parse_lower_expression()
    }

    const IS_EXPRESSION: bool = true;
}

impl ExpressionOrPattern for Pattern {
    fn application_like(
        callee: Self,
        argument: Self,
        explicitness: Explicitness,
        binder: Option<Identifier>,
        span: Span,
    ) -> Self {
        pat! { Deapplication { Attributes::new(), span; callee, argument, explicitness, binder } }
    }
    fn parse(parser: &mut Parser<'_>) -> Result<Self> {
        parser.parse_pattern()
    }
    fn parse_lower(parser: &mut Parser<'_>) -> Result<Self> {
        parser.parse_lower_pattern()
    }

    const IS_EXPRESSION: bool = false;
}

enum Expected {
    Token(TokenKind),
    Path,
    Declaration,
    Expression,
    Pattern,
    Parameter,
    AttributeArgument,
    Delimiter(Delimiter),
    // @Bug nested OneOf's
    OneOf(Vec<Self>),
}

impl Expected {
    fn added(mut self, extra: impl Into<Self>) -> Self {
        let extra = extra.into();

        if let Self::OneOf(expected) = &mut self {
            expected.push(extra);
        } else {
            self = Self::OneOf(vec![self, extra])
        }

        self
    }
}

// @Task improve API
macro expected_one_of($( $expected:expr ),+ $(,)?) {
    Expected::OneOf(vec![$( $expected.into() ),+])
}

// @Task impprove API
fn delimiters_with_expected(
    delimiters: &[Delimiter],
    expected: impl IntoIterator<Item = Expected>,
) -> Expected {
    let delimiters = delimiters.iter().copied().map(Expected::Delimiter);
    Expected::OneOf(expected.into_iter().chain(delimiters).collect())
}

impl Expected {
    fn but_actual_is(self, actual: &Token) -> Diagnostic {
        Diagnostic::error()
            .with_code(Code::E010)
            .with_message(format!(
                "found {}, but expected {}",
                match actual.kind {
                    kind @ TokenKind::Illegal => {
                        let character = actual.illegal();
                        format!("{} U+{:04X} `{}`", kind, character as u32, character)
                    }
                    kind => kind.to_string(),
                },
                self
            ))
            .with_labeled_primary_span(actual, "unexpected token")
    }
}

impl From<TokenKind> for Expected {
    fn from(token: TokenKind) -> Self {
        Self::Token(token)
    }
}

impl From<Delimiter> for Expected {
    fn from(delimiter: Delimiter) -> Self {
        Self::Delimiter(delimiter)
    }
}

use std::fmt;

impl fmt::Display for Expected {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expected::*;

        match self {
            Token(token) => write!(f, "{}", token),
            Path => write!(f, "path"),
            Declaration => write!(f, "declaration"),
            Expression => write!(f, "expression"),
            Pattern => write!(f, "pattern"),
            Parameter => write!(f, "parameter"),
            AttributeArgument => write!(f, "attribute argument"),
            Delimiter(delimiter) => write!(f, "{}", delimiter),
            OneOf(expected) => write!(f, "{}", ordered_listing(expected.iter(), Conjunction::Or)),
        }
    }
}

// @Question merge with Expected?
#[derive(Clone, Copy)]
enum Delimiter {
    TypeAnnotationPrefix,
    DefinitionPrefix,
    Token(TokenKind),
}

impl fmt::Display for Delimiter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TypeAnnotationPrefix => write!(f, "type annotation"),
            Self::DefinitionPrefix => write!(f, "definition with `=`"),
            Self::Token(token) => write!(f, "{}", token),
        }
    }
}

impl From<TokenKind> for Delimiter {
    fn from(token: TokenKind) -> Self {
        Self::Token(token)
    }
}

impl<'a> From<&'a Delimiter> for &'a TokenKind {
    fn from(delimiter: &'a Delimiter) -> &'a TokenKind {
        match delimiter {
            Delimiter::TypeAnnotationPrefix => &TokenKind::Colon,
            Delimiter::DefinitionPrefix => &TokenKind::Equals,
            Delimiter::Token(token) => token,
        }
    }
}

enum SkipLineBreaks {
    Yes,
    No,
}
