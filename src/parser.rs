//! The parser.
//!
//! I *think* it can be classified as a top-down recursive-descent parser with arbitrary look-ahead.
//!
//! ## Issues
//!
//! * crude error locations
//! * cannot really handle optional indentation
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
//! | `!A`      | Associativity                       | To the left of `::=`                                          |
//! | `<!A`     | Negative Look-Behind                |                                                               |

// @Task allow underscores in places where binders are allowed
// @Task parse temporary Lazy-Expression ::= "lazy" Expression

pub mod ast;

use crate::{
    diagnostic::{Code, Diagnostic, Diagnostics, Result},
    lexer::{Token, TokenKind},
    smallvec,
    span::{SourceFile, Span, Spanning},
    support::listing,
    SmallVec,
};
use ast::*;
use std::rc::Rc;

const STANDARD_DECLARATION_DELIMITERS: [Delimiter; 3] = {
    use Delimiter::*;
    [
        TypeAnnotationPrefix,
        DefinitionPrefix,
        Token(TokenKind::LineBreak),
    ]
};

/// The state of the parser.
pub struct Parser<'a> {
    file: Rc<SourceFile>,
    tokens: &'a [Token],
    warnings: &'a mut Diagnostics,
    index: usize,
}

impl<'a> Parser<'a> {
    pub fn new(file: Rc<SourceFile>, tokens: &'a [Token], warnings: &'a mut Diagnostics) -> Self {
        Self {
            file,
            tokens,
            warnings,
            index: 0,
        }
    }

    #[allow(dead_code)]
    fn warn(&mut self, diagnostic: Diagnostic) {
        self.warnings.insert(diagnostic);
    }

    /// Parse in a sandboxed way.
    ///
    /// Used for arbitrary look-ahead. Restores the old cursor on failure.
    // @Bug because of arbitrary look-ahead/this function, a magnitude of Diagnostics are constructed
    // but never emitted/wasted
    // @Task "restore" old warnings state on Err, so we don't get false positive warnings
    // when looking arbitrarily far ahead
    fn reflect<Node>(&mut self, parser: impl FnOnce(&mut Self) -> Result<Node>) -> Result<Node> {
        let saved_index = self.index;
        let result = parser(self);

        if result.is_err() {
            self.index = saved_index;
        }

        result
    }

    fn expect(&self, expected: TokenKind) -> Result<Token> {
        let token = self.current_token();
        if token.kind == expected {
            Ok(token.clone())
        } else {
            Err(Expected::Token(expected).but_actual_is(token))
        }
    }

    fn consume(&mut self, token_kind: TokenKind) -> Result<Token> {
        let token = self.expect(token_kind)?;
        self.advance();
        Ok(token)
    }

    fn consume_identifier(&mut self) -> Result<Identifier> {
        self.consume(TokenKind::Identifier)
            .map(Identifier::from_token)
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
        match self.current_token_kind() {
            Identifier | Punctuation => {
                let identifier = self.current_token_into_identifier();
                self.advance();
                Ok(identifier)
            }
            _ => Err(expected_one_of![Identifier, Punctuation].but_actual_is(self.current_token())),
        }
    }

    /// Indicate whether the given token was consumed.
    ///
    /// Conceptually equivalent to `Self::consume(..).is_ok()` but more memory-efficient.
    #[must_use]
    fn has_consumed(&mut self, kind: TokenKind) -> bool {
        if self.current_token_kind() == kind {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Conceptually equivalent to `Self::consume(..).map(Spanning::span).ok()` but more memory-efficient.
    fn consume_span(&mut self, kind: TokenKind) -> Option<Span> {
        if self.current_token_kind() == kind {
            let span = self.current_token_span();
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
        ast::Identifier::from_token(self.current_token().clone())
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    fn current_token(&self) -> &Token {
        &self.tokens[self.index]
    }

    fn current_token_kind(&self) -> TokenKind {
        self.current_token().kind
    }

    fn current_token_span(&self) -> Span {
        self.current_token().span
    }

    fn current_token_is_delimiter(&self, delimiters: &[Delimiter]) -> bool {
        let queried_token_kind = self.current_token_kind();

        delimiters
            .iter()
            .map(|delimiter| <&TokenKind>::from(delimiter))
            .find(|&&token| token == queried_token_kind)
            .is_some()
    }

    fn succeeding_token_kind(&self) -> TokenKind {
        self.tokens[self.index + 1].kind
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
    ///     | Group
    /// Crate-Declaration ::= "crate" #Identifier Line-Break
    /// Group ::= Indentation Declaration* Dedentation
    /// ```
    fn parse_declaration(&mut self) -> Result<Declaration> {
        use TokenKind::*;
        let attributes = self.parse_attributes(SkipLineBreaks::Yes)?;

        let span = self.current_token_span();
        match self.current_token_kind() {
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
            Indentation => {
                self.advance();
                Err(Diagnostic::bug()
                    .with_message("attribute groups not support yet")
                    .with_span(&span))
                // self.consume(Dedentation)?;
            }
            _ => Err(Expected::Declaration.but_actual_is(self.current_token())),
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
            let span = self.current_token_span();
            attributes.push(match self.current_token_kind() {
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
                        arguments: smallvec![AttributeArgument::Generated],
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

        match self.current_token_kind() {
            Identifier => {
                binder = span.merging(self.current_token_into_identifier());
                self.advance();
            }
            OpeningRoundBracket => {
                self.advance();
                binder = self.consume_identifier()?;

                while self.current_token_kind() != ClosingRoundBracket {
                    arguments.push(self.parse_attribute_argument()?);
                }

                span.merging(self.consume(ClosingRoundBracket)?);
            }
            _ => {
                return Err(expected_one_of![Identifier, OpeningRoundBracket]
                    .but_actual_is(self.current_token())
                    .with_note("`@` introduces attributes"));
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
        Ok(match self.current_token_kind() {
            kind if kind.is_path_head() => AttributeArgument::Path(Box::new(self.parse_path()?)),
            NumberLiteral => {
                let token = self.current_token().clone();
                self.advance();
                AttributeArgument::NumberLiteral(Box::new(token.number_literal().unwrap()))
            }
            TextLiteral => {
                let token = self.current_token().clone();

                if !token.text_literal_is_terminated() {
                    return Err(text_literal_is_not_terminated(&token));
                }

                self.advance();
                AttributeArgument::TextLiteral(Box::new(token.text_literal()))
            }
            OpeningRoundBracket => {
                self.advance();
                let binder = self.consume_identifier()?;
                let value = self.parse_attribute_argument()?;
                self.consume(ClosingRoundBracket)?;
                AttributeArgument::Named(Box::new(NamedAttributeArgument { binder, value }))
            }
            _ => return Err(Expected::AttributeArgument.but_actual_is(self.current_token())),
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
    ///     ("=" Possibly-Indented-Terminated-Expression | Line-Break)
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

        let expression = if self.has_consumed(Equals) {
            Some(span.merging(self.parse_possibly_indented_terminated_expression()?))
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
                expression,
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
    ///     (Line-Break | "=" Line-Break Indentation (Line-Break | Constructor)* Dedentation)
    /// ```
    fn finish_parse_data_declaration(
        &mut self,
        keyword_span: Span,
        attributes: Attributes,
    ) -> Result<Declaration> {
        use TokenKind::*;
        let mut span = keyword_span;

        let binder = span.merging(self.consume_identifier()?);
        let parameters = span.merging(self.parse_parameters(&STANDARD_DECLARATION_DELIMITERS)?);
        let type_annotation = span.merging(self.parse_optional_type_annotation()?);

        let constructors = match self.current_token_kind() {
            Equals => {
                span.merging(self.current_token_span());
                self.advance();
                self.consume(LineBreak)?;

                let mut constructors = Vec::new();

                self.parse_indented(|parser| {
                    constructors.push(parser.parse_constructor()?);
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
                return Err(expected_one_of![
                    Delimiter::DefinitionPrefix,
                    LineBreak,
                    Expected::Expression
                ]
                .but_actual_is(self.current_token()));
            }
        };

        Ok(decl! {
            Data {
                attributes,
                keyword_span.merge(&span);
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
    ///     | "module" #Identifier (Line-Break | "=" Line-Break Indentation (Line-Break | Declaration)* Dedentation)
    /// Header ::= "module" "=" Line-Break
    /// ```
    fn finish_parse_module_declaration(
        &mut self,
        keyword_span: Span,
        attributes: Attributes,
    ) -> Result<Declaration> {
        use TokenKind::*;
        let mut span = keyword_span;

        if self.has_consumed(Equals) {
            return Ok(decl! {
                Header {
                    attributes,
                    span
                }
            });
        }

        let binder = span.merging(self.consume_identifier()?);

        match self.current_token_kind() {
            // external module declaration
            LineBreak => {
                self.advance();
                return Ok(decl! {
                    Module {
                        attributes,
                        span;
                        binder,
                        file: self.file.clone(),
                        declarations: None,
                    }
                });
            }
            Equals => {
                self.advance();
                self.consume(LineBreak)?;
            }
            _ => {
                return Err(expected_one_of![LineBreak, Equals].but_actual_is(self.current_token()))
            }
        };

        let mut declarations = Vec::new();

        self.parse_indented(|parser| {
            declarations.push(parser.parse_declaration()?);
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

    /// Parse the "top level" aka the body of a module file.
    ///
    /// It takes the identifier of the module as an argument.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Top-Level ::= (Line-Break | Declaration)* #End-Of-Input
    /// ```
    pub fn parse_top_level(&mut self, binder: Identifier) -> Result<Declaration> {
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

    // @Note this is fragile and ugly as heck
    fn parse_indented(&mut self, mut subparser: impl FnMut(&mut Self) -> Result<()>) -> Result<()> {
        use TokenKind::*;

        while self.has_consumed(Indentation) {
            while self.current_token_kind() != Dedentation {
                if self.has_consumed(LineBreak) {
                    continue;
                }

                subparser(self)?;
            }

            self.consume(Dedentation)?;

            while self.has_consumed(LineBreak) {}
        }

        Ok(())
    }

    /// Finish parsing use declaration.
    ///
    /// The keyword `use` should have already been parsed.
    /// The span does not contain the trailing line break.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Use-Declaration ::= "use" Path-Tree Line-Break
    /// ```
    fn finish_parse_use_declaration(
        &mut self,
        keyword_span: Span,
        attributes: Attributes,
    ) -> Result<Declaration> {
        let bindings = self.parse_path_tree(&[TokenKind::LineBreak.into()])?;
        self.consume(TokenKind::LineBreak)?;

        Ok(decl! {
            Use {
                attributes,
                keyword_span.merge(&bindings);
                bindings,
            }
        })
    }

    /// Parse a path tree.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Path-Tree ::= Path | Path "." "(" (Path-Tree | "(" Renaming ")")* ")" | Renaming
    /// Renaming ::= Path "as" General-Identifier
    /// ```
    // @Task rewrite this following a simpler grammar mirroring expression applications
    fn parse_path_tree(&mut self, delimiters: &[Delimiter]) -> Result<PathTree> {
        use TokenKind::*;

        let mut path = self.parse_first_path_segment()?;

        while self.has_consumed(Dot) {
            match self.current_token_kind() {
                Identifier | Punctuation => {
                    let identifier = self.current_token_into_identifier();
                    self.advance();
                    path.segments.push(identifier);
                }
                OpeningRoundBracket => {
                    self.advance();

                    let mut bindings = Vec::new();

                    while !self.has_consumed(ClosingRoundBracket) {
                        if self.has_consumed(OpeningRoundBracket) {
                            let target = self.parse_path()?;
                            self.consume(As)?;
                            let binder = self.consume_general_identifier()?;
                            self.consume(ClosingRoundBracket)?;

                            bindings.push(PathTree::Single {
                                target,
                                binder: Some(binder),
                            });
                        } else {
                            // @Note @Bug this is really really fragile=non-extensible!
                            bindings.push(self.parse_path_tree(&[
                                OpeningRoundBracket.into(),
                                ClosingRoundBracket.into(),
                                Identifier.into(),
                                Punctuation.into(),
                                Self_.into(),
                            ])?);
                        }
                    }

                    return Ok(PathTree::Multiple { path, bindings });
                }
                _ => {
                    return Err(
                        expected_one_of![Identifier, Punctuation, OpeningRoundBracket]
                            .but_actual_is(self.current_token()),
                    )
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
            return Err(
                Expected::OneOf(&delimiters_with_expected(delimiters, Some(As.into())))
                    .but_actual_is(self.current_token()),
            );
        };

        // @Task correct span info
        Ok(PathTree::Single {
            target: path,
            binder,
        })
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
    ///     #Identifier Parameters Type-Annotation? Line-Break
    /// ```
    fn parse_constructor(&mut self) -> Result<Declaration> {
        let attributes = self.parse_attributes(SkipLineBreaks::Yes)?;

        let binder = self.consume_identifier()?;
        let mut span = binder.span;

        let parameters =
            span.merging(self.parse_parameters(&[
                Delimiter::TypeAnnotationPrefix,
                TokenKind::LineBreak.into(),
            ])?);

        let type_annotation = span.merging(self.parse_optional_type_annotation()?);

        self.consume(TokenKind::LineBreak)?;

        Ok(decl! {
            Constructor {
                attributes,
                span;
                binder,
                parameters,
                type_annotation,
            }
        })
    }

    /// Parse an expression.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Expression ::= Pi-Literal-Or-Lower
    /// ```
    // @Task parse sigma literals
    // @Task once that has been completed, inline parse_pi_literal_or_lower
    fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_pi_type_literal_or_lower()
    }

    /// Parse a pi-type literal or a lower expression.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Pi-Literal-Or-Lower !right ::=
    ///     ("(" Explicitness #Identifier Type-Annotation ")" | Application-Or-Lower)
    ///     ("->" Pi-Literal-Or-Lower)*
    /// ```
    fn parse_pi_type_literal_or_lower(&mut self) -> Result<Expression> {
        use TokenKind::*;
        let mut span = Span::SHAM;

        let (explicitness, fieldness, binder, domain) = self
            .reflect(|parser| {
                span = parser.consume(OpeningRoundBracket)?.span;

                let explicitness = parser.consume_explicitness_symbol();
                let fieldness = parser.consume_span(Field);
                let binder = parser.consume_identifier()?;
                let parameter = parser.parse_type_annotation()?;

                parser.consume(ClosingRoundBracket)?;

                Ok((explicitness, fieldness, Some(binder), parameter))
            })
            .or_else(|_| -> Result<_> {
                let parameter = self.parse_application_or_lower()?;
                span = parameter.span;
                Ok((Explicit, None, None, parameter))
            })?;

        Ok(match self.consume(ThinArrowRight) {
            Ok(_) => {
                let codomain = span.merging(self.parse_pi_type_literal_or_lower()?);

                expr! {
                    PiTypeLiteral {
                        Attributes::new(),
                        span;
                        codomain,
                        binder,
                        domain,
                        explicitness,
                        fieldness,
                    }
                }
            }
            Err(_) if binder.is_none() => domain,
            Err(error) => return Err(error),
        })
    }

    /// Parse an application or a lower expression.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Application-Or-Lower !left ::=
    ///     Lower-Expression
    ///     (Lower-Expression | "(" Explicitness (#Identifier "=")? Expression ")")*
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
    /// Naked-Lower-Expression ::=
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

        let mut span = self.current_token_span();
        match self.current_token_kind() {
            kind if kind.is_path_head() => self.parse_path().map(|path| Expression {
                span: path.span(),
                kind: ExpressionKind::Path(Box::new(path)),
                attributes,
            }),
            Type => {
                self.advance();
                Ok(expr! { TypeLiteral { attributes, span } })
            }
            NumberLiteral => {
                let token = self.current_token().clone();
                self.advance();
                Ok(expr! {
                    NumberLiteral(attributes, token.span; token.number_literal().unwrap())
                })
            }
            TextLiteral => {
                let token = self.current_token().clone();
                self.advance();

                if !token.text_literal_is_terminated() {
                    return Err(text_literal_is_not_terminated(&token));
                }

                Ok(expr! { TextLiteral(attributes, token.span; token.text_literal()) })
            }
            QuestionMark => {
                self.advance();
                self.consume_identifier()
                    .map(|tag| expr! { TypedHole { attributes, span.merge(&tag); tag } })
            }
            Let => {
                self.advance();
                self.finish_parse_let_in(span, attributes)
            }
            Use => {
                self.advance();
                self.finish_parse_use_in(span, attributes)
            }
            Backslash => {
                self.advance();
                self.finish_parse_lambda_literal(span, attributes)
            }
            Case => {
                self.advance();
                self.finish_parse_case_analysis(span, attributes)
            }
            Do => {
                self.advance();
                self.finish_parse_do_block(span, attributes)
            }
            OpeningSquareBracket => {
                self.advance();

                let mut elements = Vec::new();

                while self.current_token_kind() != ClosingSquareBracket {
                    elements.push(self.parse_lower_expression()?);
                }

                span.merging(self.current_token());
                self.advance();

                Ok(expr! { SequenceLiteral { attributes, span; elements } })
            }
            OpeningRoundBracket => {
                self.advance();

                let mut expression = self.parse_expression()?;

                span.merging(self.consume(ClosingRoundBracket)?);
                expression.span = span;
                expression.attributes.extend(attributes);

                Ok(expression)
            }
            _ => Err(Expected::Expression.but_actual_is(self.current_token())),
        }
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
        let path = match self.current_token_kind() {
            Identifier | Punctuation => Path::identifier(self.current_token().clone()),
            Crate | Super | Self_ => Path::hanger(self.current_token().clone()),
            _ => return Err(Expected::Path.but_actual_is(self.current_token())),
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
    fn finish_parse_lambda_literal(
        &mut self,
        keyword_span: Span,
        attributes: Attributes,
    ) -> Result<Expression> {
        let mut span = keyword_span;
        let parameters =
            self.parse_parameters(&[Delimiter::TypeAnnotationPrefix, TokenKind::WideArrow.into()])?;
        let body_type_annotation = self.parse_optional_type_annotation()?;
        self.consume(TokenKind::WideArrow)?;
        let body = span.merging(self.parse_expression()?);

        Ok(expr! {
            LambdaLiteral {
                attributes,
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
    ///     "let" #Identifier Parameters Type_Annotation? "="
    ///     Possibly-Breakably-Indented-Expression "in" Line-Break? Expression
    /// ```
    fn finish_parse_let_in(
        &mut self,
        span_of_let: Span,
        attributes: Attributes,
    ) -> Result<Expression> {
        let mut span = span_of_let;
        let binder = self.consume_identifier()?;
        let parameters =
            self.parse_parameters(&[Delimiter::TypeAnnotationPrefix, Delimiter::DefinitionPrefix])?;
        let type_annotation = self.parse_optional_type_annotation()?;
        self.consume(TokenKind::Equals)?;
        let expression = self.parse_possibly_breakably_indented_expression()?;
        self.consume(TokenKind::In)?;
        let _ = self.has_consumed(TokenKind::LineBreak);
        let scope = span.merging(self.parse_expression()?);

        Ok(expr! {
            LetIn {
                attributes,
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
    /// Use-In ::= "use" Path-Tree "in" Line-Break? Expression
    /// ```
    fn finish_parse_use_in(
        &mut self,
        span_of_use: Span,
        attributes: Attributes,
    ) -> Result<Expression> {
        let bindings = self.parse_path_tree(&[TokenKind::In.into()])?;
        self.consume(TokenKind::In)?;
        let _ = self.has_consumed(TokenKind::LineBreak);
        let scope = self.parse_expression()?;

        Ok(expr! {
            UseIn {
                attributes,
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
    /// Case-Analysis ::= "case" Possibly-Breakably-Indented-Expression "of"
    ///     (Line-Break (Indentation Case* Dedentation)?)?
    /// Case ::= Pattern "=>" Expression
    /// ```
    fn finish_parse_case_analysis(
        &mut self,
        span_of_case: Span,
        attributes: Attributes,
    ) -> Result<Expression> {
        use TokenKind::*;
        let mut span = span_of_case;

        let expression = self.parse_possibly_breakably_indented_expression()?;
        span.merging(self.consume(Of)?);

        let mut cases = Vec::new();

        if self.current_token_kind() == LineBreak && self.succeeding_token_kind() == Indentation {
            self.advance();
            self.advance();

            while self.current_token_kind() != Dedentation {
                let pattern = self.parse_pattern()?;
                self.consume(WideArrow)?;
                let expression = self.parse_possibly_indented_terminated_expression()?;

                cases.push(ast::Case {
                    pattern,
                    expression,
                });
            }

            span.merging(self.current_token());
            self.advance();
        }

        Ok(expr! {
            CaseAnalysis {
                attributes,
                span;
                expression,
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
    /// Do-Block ::= "do" Line-Break Indentation Statement* Dedentation
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
    fn finish_parse_do_block(
        &mut self,
        span_of_do: Span,
        attributes: Attributes,
    ) -> Result<Expression> {
        use TokenKind::*;
        let mut span = span_of_do;
        let mut statements = Vec::new();

        self.consume(LineBreak)?;
        self.consume(Indentation)?;

        while self.current_token_kind() != Dedentation {
            statements.push(match self.current_token_kind() {
                Let => {
                    self.advance();
                    let binder = self.consume_identifier()?;
                    let parameters = self.parse_parameters(&[
                        Delimiter::TypeAnnotationPrefix,
                        Delimiter::DefinitionPrefix,
                    ])?;
                    let type_annotation = self.parse_optional_type_annotation()?;
                    self.consume(TokenKind::Equals)?;
                    let expression = self.parse_possibly_indented_terminated_expression()?;
                    Statement::Let(LetStatement {
                        binder,
                        parameters,
                        type_annotation,
                        expression,
                    })
                }
                Use => {
                    self.advance();
                    let bindings = self.parse_path_tree(&[TokenKind::LineBreak.into()])?;
                    self.consume(LineBreak)?;
                    Statement::Use(ast::Use { bindings })
                }
                _ => {
                    if self.current_token_kind() == Identifier
                        && matches!(self.succeeding_token_kind(), Colon | ThinArrowLeft)
                    {
                        let binder = self.current_token_into_identifier();
                        self.advance();
                        let type_annotation = self.parse_optional_type_annotation()?;
                        self.consume(ThinArrowLeft)?;
                        let expression = self.parse_possibly_indented_terminated_expression()?;
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

        span.merging(self.current_token());
        self.advance();

        Ok(expr! {
            DoBlock {
                attributes,
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
    /// Parameter-Group ::=
    ///     | #Identifier
    ///     | "(" Explicitness "field"? #Identifier+ Type-Annotation? ")"
    /// ```
    fn parse_parameter_group(&mut self, delimiters: &[Delimiter]) -> Result<ParameterGroup> {
        use TokenKind::*;
        let mut span = self.current_token_span();
        match self.current_token_kind() {
            Identifier => {
                let binder = self.current_token_into_identifier();
                self.advance();
                Ok(ParameterGroup {
                    span,
                    parameters: smallvec![binder],
                    type_annotation: None,
                    fieldness: None,
                    explicitness: Explicit,
                })
            }
            OpeningRoundBracket => {
                self.advance();
                let explicitness = self.consume_explicitness_symbol();
                let fieldness = self.consume_span(Field);
                let mut parameters = SmallVec::new();

                parameters.push(self.consume_identifier()?);

                let delimiters = [Delimiter::TypeAnnotationPrefix, ClosingRoundBracket.into()];

                while !self.current_token_is_delimiter(&delimiters) {
                    parameters.push(self.consume_identifier()?);
                }

                let type_annotation = self.parse_optional_type_annotation()?;

                span.merging(self.consume(ClosingRoundBracket)?);

                Ok(ParameterGroup {
                    parameters,
                    type_annotation,
                    explicitness,
                    fieldness,
                    span,
                })
            }

            _ => {
                return Err(Expected::OneOf(&delimiters_with_expected(
                    delimiters,
                    Some(Expected::Parameter),
                ))
                .but_actual_is(self.current_token()));
            }
        }
    }

    /// Parse a pattern.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Pattern !left ::=
    ///     Lower-Pattern
    ///     (Lower-Pattern | "(" Explicitness (#Identifier =)? Pattern ")")*
    /// ```
    fn parse_pattern(&mut self) -> Result<Pattern> {
        self.parse_application_like_or_lower()
    }

    /// Parse a (de)application or something lower.
    fn parse_application_like_or_lower<EP: ExpressionOrPattern>(&mut self) -> Result<EP> {
        use TokenKind::*;
        let mut callee = self.reflect(EP::parse_lower)?;

        while let Ok((argument, explicitness, binder)) = self
            .reflect(|parser| Ok((EP::parse_lower(parser)?, Explicit, None)))
            .or_else(|_| -> Result<_> {
                self.consume(OpeningRoundBracket)?;
                let explicitness = self.consume_explicitness_symbol();
                let binder = (self.current_token_kind() == Identifier
                    && self.succeeding_token_kind() == Equals)
                    .then(|| {
                        let binder = self.current_token_into_identifier();
                        self.advance();
                        self.advance();
                        binder
                    });
                let argument = EP::parse_lower(self)?;
                self.consume(ClosingRoundBracket)?;
                Ok((argument, explicitness, binder))
            })
        {
            let span = callee.span().merge(&argument);

            callee = EP::application_like(callee, argument, explicitness, binder, span);
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

        let mut span = self.current_token_span();
        match self.current_token_kind() {
            kind if kind.is_path_head() => self.parse_path().map(|path| Pattern {
                span: path.span(),
                kind: PatternKind::Path(Box::new(path)),
                attributes,
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

                if !token.text_literal_is_terminated() {
                    return Err(text_literal_is_not_terminated(&token));
                }

                Ok(pat! { TextLiteral(attributes, token.span; token.text_literal()) })
            }
            Backslash => {
                self.advance();
                self.consume_identifier()
                    .map(|binder| pat! { Binder { attributes, span.merge(&binder); binder } })
            }
            OpeningSquareBracket => {
                self.advance();

                let mut elements = Vec::new();

                while self.current_token_kind() != ClosingSquareBracket {
                    elements.push(self.parse_lower_pattern()?);
                }

                span.merging(self.current_token());
                self.advance();

                Ok(pat! { SequenceLiteralPattern { attributes, span; elements } })
            }
            OpeningRoundBracket => {
                self.advance();
                let mut pattern = self.parse_pattern()?;
                span.merging(self.consume(TokenKind::ClosingRoundBracket)?);
                pattern.span = span;
                pattern.attributes.extend(attributes);
                Ok(pattern)
            }
            _ => Err(Expected::Pattern.but_actual_is(self.current_token())),
        }
    }

    /// Parse a type annotation.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Type-Annotation ::= ":" Expression
    /// ```
    fn parse_type_annotation(&mut self) -> Result<Expression> {
        self.consume(TokenKind::Colon)?;
        self.reflect(Self::parse_expression)
    }

    /// Parse an optional type annotation.
    fn parse_optional_type_annotation(&mut self) -> Result<Option<Expression>> {
        self.has_consumed(TokenKind::Colon)
            .then(|| self.parse_expression())
            .transpose()
    }

    /// Parse a possibly indented expression terminated by a line break.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Possibly-Indented-Terminated-Expression ::=
    ///     | Line-Break Indentation Expression Line-Break? Dedentation
    ///     | Terminated-Expression
    /// ```
    fn parse_possibly_indented_terminated_expression(&mut self) -> Result<Expression> {
        use TokenKind::*;

        if self.has_consumed(LineBreak) {
            self.consume(Indentation)?;
            let expression = self.parse_expression()?;
            let _ = self.has_consumed(LineBreak);
            self.consume(Dedentation)?;
            Ok(expression)
        } else {
            self.parse_terminated_expression()
        }
    }

    /// Parse a possibly indented expression whose indentation can be broken.
    ///
    /// The user of this function needs to parse the indentation breaker by themself.
    ///
    /// An **indentation breaker** is a token which functions as an alternative to
    /// a line break followed by dedentation. This feature exists for ergonomic reasons.
    /// Well-known tokens of this sort are `in` in let/in and use/in and `of` in case/of.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Possibly-Breakably-Indented-Expression ::=
    ///     | Line-Break Indentation Expression (Line-Break Dedentation)?
    ///     | Expression
    /// ```
    // @Bug even though this function works locally, it does not to with more context.
    // the indentation breaking logic does not work because our parser then inserts a
    // Dedentation token somewhere later down the line (e.g. after the In)
    fn parse_possibly_breakably_indented_expression(&mut self) -> Result<Expression> {
        use TokenKind::*;

        if self.has_consumed(LineBreak) {
            self.consume(Indentation)?;
            let expression = self.parse_expression()?;
            if self.has_consumed(LineBreak) {
                self.consume(Dedentation)?;
            }
            Ok(expression)
        } else {
            self.parse_expression()
        }
    }

    /// Parse an expression terminated by a line break or dedentation.
    ///
    /// A line break is parsed, dedentation is not.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Terminated-Expression ::= Expression (<!Dedentation Line-Break)?
    /// ```
    fn parse_terminated_expression(&mut self) -> Result<Expression> {
        let expression = self.parse_expression()?;
        // @Note special-casing is also called programming hackily
        if self.current_token_kind() != TokenKind::Dedentation {
            self.consume(TokenKind::LineBreak)?;
        }
        Ok(expression)
    }

    /// Consume the explicitness symbol.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Explicitness ::= ","?
    /// ```
    fn consume_explicitness_symbol(&mut self) -> Explicitness {
        if let Ok(token) = self.consume(TokenKind::Comma) {
            // @Note there might be false positives (through arbitrary look-ahead)
            // (current issue of Self::reflect)
            self.warn(
                Diagnostic::warning()
                    .with_code(Code::W001)
                    .with_message("implicitness markers are currently ignored")
                    .with_span(&token),
            );
            Implicit
        } else {
            Explicit
        }
    }
}

/// Abstraction over expressions and patterns.
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
}

enum Expected<'a> {
    Token(TokenKind),
    Path,
    Declaration,
    Expression,
    Pattern,
    Parameter,
    AttributeArgument,
    Delimiter(Delimiter),
    OneOf(&'a [Self]),
}

// because we cannot collect into an array yet (std library lacks lots of const generic functionality)
macro expected_one_of($( $expected:expr ),+ $(,)?) {
    Expected::OneOf(&[$( $expected.into() ),+])
}

// @Task return a Expected::OneOf straight away
fn delimiters_with_expected<'a>(
    delimiters: &[Delimiter],
    expected: impl IntoIterator<Item = Expected<'a>>,
) -> Vec<Expected<'a>> {
    let delimiters = delimiters.iter().copied().map(Expected::Delimiter);
    expected.into_iter().chain(delimiters).collect()
}

impl<'a> Expected<'a> {
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
            .with_span(actual)
    }
}

impl From<TokenKind> for Expected<'_> {
    fn from(token: TokenKind) -> Self {
        Self::Token(token)
    }
}

impl From<Delimiter> for Expected<'_> {
    fn from(delimiter: Delimiter) -> Self {
        Self::Delimiter(delimiter)
    }
}

use std::fmt;

impl fmt::Display for Expected<'_> {
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
            OneOf(expected) => write!(f, "{}", listing(expected.iter(), "or")),
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

fn text_literal_is_not_terminated(span: &impl Spanning) -> Diagnostic {
    Diagnostic::error()
        .with_code(Code::E004)
        .with_message("unterminated text literal")
        .with_span(span)
}

enum SkipLineBreaks {
    Yes,
    No,
}
