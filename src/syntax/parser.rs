//! The syntactic analyzer (parser).
//!
//! I *think* it can be classified as a top-down recursive-descent parser with arbitrary look-ahead.
//!
//! # Grammar Notation
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
//! | `(> A)`   | Positive Look-Ahead                 |                                                               |
//! | `(< A)`   | Positive Look-Behind                |                                                               |

#[cfg(test)]
mod test;

#[allow(clippy::wildcard_imports)]
use super::{
    ast::{
        self, attrarg, decl, expr, pat, Attribute, AttributeArgument, AttributeKind, Attributes,
        BindStatement, Declaration, Domain,
        Explicitness::{self, *},
        Expression, Identifier, LetStatement, NamedAttributeArgument, Parameter, ParameterKind,
        Parameters, Path, Pattern, SpannedExplicitness, Statement, UsePathTree, UsePathTreeKind,
    },
    token::{
        Token,
        TokenName::{self, *},
    },
};
use crate::{
    diagnostics::{Code, Diagnostic, Reporter},
    error::Result,
    format::{ordered_listing, Conjunction},
    span::{SharedSourceMap, SourceFileIndex, Span, Spanned, Spanning},
    utility::SmallVec,
};
use std::default::default;

const STANDARD_DECLARATION_DELIMITERS: [Delimiter; 3] = {
    use Delimiter::*;

    [Terminator, TypeAnnotationPrefix, DefinitionPrefix]
};

const BRACKET_POTENTIAL_PI_TYPE_LITERAL: &str =
    "add round brackets around the potential pi type literal to disambiguate the expression";

/// Parse a file (the crate root or an out-of-line module).
pub fn parse_file(
    tokens: &[Token],
    file: SourceFileIndex,
    module: Identifier,
    map: SharedSourceMap,
    reporter: &Reporter,
) -> Result<Declaration> {
    Parser::new(tokens, file, map, reporter).parse_top_level(module)
}

// @Task get rid of the parameters file and map!
pub(super) fn parse_path(
    tokens: &[Token],
    file: SourceFileIndex,
    map: SharedSourceMap,
    reporter: &Reporter,
) -> Result<Path> {
    Parser::new(tokens, file, map, reporter).parse_path()
}

/// The state of the parser.
// @Beacon @Beacon @Beacon @Task create stripped down version of the Parser
// that does not have `map` and `file` (naming?) and move all parsing methods that don't
// need those files to it
struct Parser<'a> {
    tokens: &'a [Token],
    file: SourceFileIndex,
    reflection_depth: u16,
    index: usize,
    map: SharedSourceMap,
    reporter: &'a Reporter,
}

impl<'a> Parser<'a> {
    fn new(
        tokens: &'a [Token],
        file: SourceFileIndex,
        map: SharedSourceMap,
        reporter: &'a Reporter,
    ) -> Self {
        Self {
            tokens,
            file,
            reflection_depth: 0,
            index: 0,
            map,
            reporter,
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

    fn manually_reflect<T>(&mut self, reporter: impl FnOnce(&mut Self) -> T) -> T {
        self.reflection_depth += 1;
        let result = reporter(self);
        self.reflection_depth -= 1;
        result
    }

    fn reflecting(&self) -> bool {
        self.reflection_depth != 0
    }

    fn error<T, D: FnOnce() -> Diagnostic>(&self, diagnostic: D) -> Result<T> {
        fn error<D: FnOnce() -> Diagnostic>(parser: &Parser<'_>, diagnostic: D) -> Result<!> {
            if !parser.reflecting() {
                diagnostic().report(parser.reporter);
            }

            Err(())
        }

        error(self, diagnostic).map(|okay| okay)
    }

    fn expect(&self, expected: TokenName) -> Result<Token> {
        let token = self.current_token();
        if token.name() == expected {
            Ok(token.clone())
        } else {
            self.error(|| Expected::Token(expected).but_actual_is(token))
        }
    }

    // @Note horrible name
    fn expect_among_others(&self, expected: TokenName, other_expected: Expected) -> Result<Token> {
        let token = self.current_token();
        if token.name() == expected {
            Ok(token.clone())
        } else {
            self.error(|| other_expected.added(expected).but_actual_is(token))
        }
    }

    fn consume(&mut self, token: TokenName) -> Result<Token> {
        let token = self.expect(token)?;
        self.advance();
        Ok(token)
    }

    // @Note bad name
    fn consume_after_expecting(
        &mut self,
        token: TokenName,
        other_expected: Expected,
    ) -> Result<Token> {
        let token = self.expect_among_others(token, other_expected)?;
        self.advance();
        Ok(token)
    }

    fn consume_word(&mut self) -> Result<Identifier> {
        self.consume(TokenName::Word)
            .map(|identifier| identifier.try_into().unwrap())
    }

    /// Consume an identifier.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Identifier ::= #Word | #Punctuation
    /// ```
    // @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Task rename to consume_identifier
    fn consume_identifier(&mut self) -> Result<Identifier> {
        match self.current_token().name() {
            Word | Punctuation => {
                let identifier = self.current_token_into_identifier();
                self.advance();
                Ok(identifier)
            }
            // @Question should we introduce Expected::Identifier for added clarity?
            _ => self
                .error(|| expected_one_of![Word, Punctuation].but_actual_is(self.current_token())),
        }
    }

    /// Indicate whether the given token was consumed.
    ///
    /// Conceptually equivalent to `self.manually_reflect(|this| this.consume(..)).is_ok()`
    /// but more memory-efficient as it does not clone the consumed token.
    #[must_use]
    fn has_consumed(&mut self, token: TokenName) -> bool {
        if self.current_token().name() == token {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Conceptually equivalent to `Self::consume(..).map(Spanning::span).ok()`.
    ///
    /// However, it is more memory-efficient as it neither constructs a [`Diagnostic`]
    /// nor clones the consumed token.
    fn consume_span(&mut self, token: TokenName) -> Option<Span> {
        if self.current_token().name() == token {
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
    fn current_token_into_identifier(&self) -> Identifier {
        self.current_token().clone().try_into().unwrap()
    }

    /// Step to the next token output by the lexer.
    fn advance(&mut self) {
        self.index += 1;
    }

    /// Inspect the current token.
    fn current_token(&self) -> &Token {
        &self.tokens[self.index]
    }

    /// Indicate whether the current token is one of the given delimiters.
    fn current_token_is_delimiter(&self, delimiters: &[Delimiter]) -> bool {
        let token = self.current_token().name();
        delimiters.iter().any(|delimiter| delimiter.matches(token))
    }

    /// Inspect the token following the current one.
    ///
    /// # Panics
    ///
    /// Panics if the current token is the [end of input](TokenName::EndOfInput).
    fn succeeding_token(&self) -> &Token {
        &self.tokens[self.index + 1]
    }

    fn preceeding_token(&self) -> Option<&Token> {
        Some(&self.tokens[self.index.checked_sub(1)?])
    }

    /// Parse a declaration.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Declaration ::= (Attribute #Line-Break*)* Bare-Declaration
    /// Bare-Declaration ::=
    ///     | Function-Declaration
    ///     | Data-Declaration
    ///     | Module-Declaration
    ///     | Use-Declaration
    /// ```
    // @Task re-add attribute groups (needs syntax proposals)
    fn parse_declaration(&mut self) -> Result<Declaration> {
        let attributes = self.parse_attributes(SkipLineBreaks::Yes)?;

        let span = self.current_token().span;
        match self.current_token().name() {
            Word => {
                let identifier = self.current_token_into_identifier();
                self.advance();
                self.finish_parse_function_declaration(identifier, attributes)
            }
            Data => {
                self.advance();
                self.finish_parse_data_declaration(span, attributes)
            }
            Module => {
                self.advance();
                self.finish_parse_module_declaration(span, attributes)
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
    /// # Grammar
    ///
    /// ```ebnf
    /// Attribute ::= Regular-Attribute | Documentation-Comment
    /// ```
    fn parse_attributes(&mut self, skip_line_breaks: SkipLineBreaks) -> Result<Attributes> {
        let mut attributes = Attributes::default();

        loop {
            let span = self.current_token().span;
            attributes.push(match self.current_token().name() {
                At => {
                    self.advance();
                    let attribute = self.finish_parse_regular_attribute(span)?;
                    if skip_line_breaks == SkipLineBreaks::Yes {
                        while self.has_consumed(Semicolon) {}
                    }
                    attribute
                }
                DocumentationComment => {
                    self.advance();
                    if skip_line_breaks == SkipLineBreaks::Yes {
                        while self.has_consumed(Semicolon) {}
                    }
                    Attribute::new(span, AttributeKind::Documentation)
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
    /// # Grammar
    ///
    /// Note: The grammar is not complete yet since we cannot represent the
    /// arguments of `@if` yet which are the most complex.
    ///
    /// ```ebnf
    /// Regular-Attribute ::= "@" (#Word | "(" #Word Attribute-Argument* ")")
    /// ```
    fn finish_parse_regular_attribute(&mut self, keyword_span: Span) -> Result<Attribute> {
        let mut span = keyword_span;

        let binder;
        let mut arguments = SmallVec::new();

        match self.current_token().name() {
            Word => {
                binder = span.merging(self.current_token_into_identifier());
                self.advance();
            }
            OpeningRoundBracket => {
                self.advance();
                binder = self.consume_word()?;

                while self.current_token().name() != ClosingRoundBracket {
                    arguments.push(self.parse_attribute_argument()?);
                }

                // @Note constructs and wastes a diagnostic
                // @Task transform above while into a loop
                // @Beacon :while_current_not
                span.merging(self.consume(ClosingRoundBracket).unwrap());
            }
            _ => {
                return self.error(|| {
                    expected_one_of![Word, OpeningRoundBracket]
                        .but_actual_is(self.current_token())
                        .note("`@` introduces attributes")
                });
            }
        }

        Ok(Attribute::new(
            span,
            AttributeKind::Regular { binder, arguments },
        ))
    }

    /// Parse an argument of an attribute.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Attribute-Argument ::= Lower-Attribute-Argument | "(" #Word Lower-Attribute-Argument ")"
    /// Lower-Attribute-Argument ::= Path | #Number-Literal | #Text-Literal
    /// ```
    fn parse_attribute_argument(&mut self) -> Result<AttributeArgument> {
        Ok(match self.current_token().name() {
            name if name.is_path_head() => {
                let path = self.parse_path()?;
                attrarg! { Path(path.span(); path) }
            }
            NumberLiteral => {
                let token = self.current_token().clone();
                self.advance();
                attrarg! { NumberLiteral(token.span; token.into_number_literal().unwrap()) }
            }
            TextLiteral => {
                let token = self.current_token().clone();
                let span = token.span;
                let text_literal = token
                    .into_text_literal()
                    .unwrap()
                    .or_else(|error| self.error(|| error))?;

                self.advance();
                attrarg! { TextLiteral(span; text_literal) }
            }
            OpeningRoundBracket => {
                let mut span = self.current_token().span;
                self.advance();
                let binder = self.consume_word()?;
                let argument = self.parse_attribute_argument()?;
                span.merging(&self.consume(ClosingRoundBracket)?);
                attrarg! { Named(span; NamedAttributeArgument { binder, value: argument }) }
            }
            _ => {
                return self
                    .error(|| Expected::AttributeArgument.but_actual_is(self.current_token()))
            }
        })
    }

    /// Finish parsing a function declaration.
    ///
    /// The leading identifier should have already parsed beforehand.
    /// The span does not include the trailing line break.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Function-Declaration ::=
    ///     #Word
    ///     Parameters Type-Annotation?
    ///     ("=" Expression)?
    ///     Terminator
    /// ```
    fn finish_parse_function_declaration(
        &mut self,
        binder: Identifier,
        attributes: Attributes,
    ) -> Result<Declaration> {
        let mut span = binder.span();

        let parameters = span.merging(self.parse_parameters(&STANDARD_DECLARATION_DELIMITERS)?);
        let type_annotation = span.merging(self.parse_optional_type_annotation()?);

        let body = if self.has_consumed(Equals) {
            Some(span.merging(self.parse_expression()?))
        } else {
            None
        };

        self.expect_terminator()?;

        Ok(decl! {
            Function {
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
    /// # Grammar
    ///
    /// ```ebnf
    /// Data-Declaration ::=
    ///     "data" #Word
    ///     Parameters Type-Annotation?
    ///     ("of" ("{" (Terminator | Constructor)* "}")?)?
    ///     Terminator
    /// ```
    fn finish_parse_data_declaration(
        &mut self,
        keyword_span: Span,
        attributes: Attributes,
    ) -> Result<Declaration> {
        let mut span = keyword_span;

        let binder = span.merging(self.consume_word()?);
        let parameters = span.merging(self.parse_parameters(&[
            Delimiter::Terminator,
            Delimiter::TypeAnnotationPrefix,
            Of.into(),
        ])?);
        let type_annotation = span.merging(self.parse_optional_type_annotation()?);

        let constructors = match self.current_token().name() {
            name if name.is_terminator() => {
                if name == Semicolon {
                    self.advance();
                }
                None
            }
            Of => {
                span.merging(self.current_token().span);
                self.advance();

                let mut constructors = Vec::new();

                self.parse_optional_block(|this| {
                    constructors.push(this.parse_constructor()?);
                    Ok(())
                })?;

                span.merging(constructors.last());
                self.expect_terminator()?;

                Some(constructors)
            }
            _ => {
                return self.error(|| {
                    expected_one_of![
                        Delimiter::Terminator,
                        Delimiter::DefinitionPrefix,
                        Expected::Expression
                    ]
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
    /// This is either an inline or an out-of-line module declaration.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Module-Declaration ::=
    ///     | Header
    ///     | "module" #Word ("of" ("{" (Terminator | Declaration)* "}")?)? Terminator
    /// Header ::= "module" Terminator
    /// ```
    fn finish_parse_module_declaration(
        &mut self,
        keyword_span: Span,
        attributes: Attributes,
    ) -> Result<Declaration> {
        let mut span = keyword_span;

        match self.current_token().name() {
            name if name.is_terminator() => {
                if name == Semicolon {
                    self.advance();
                }

                return Ok(decl! { ModuleHeader { attributes, span } });
            }
            _ => {}
        }

        let binder = span.merging(self.consume_word()?);

        match self.current_token().name() {
            // out-of-line module declaration
            name if name.is_terminator() => {
                if name == Semicolon {
                    self.advance();
                }

                Ok(decl! {
                    Module {
                        attributes,
                        span;
                        binder,
                        file: self.file,
                        declarations: None,
                    }
                })
            }
            Of => {
                self.advance();
                let mut declarations = Vec::new();

                self.parse_optional_block(|this| {
                    declarations.push(this.parse_declaration()?);
                    Ok(())
                })?;

                span.merging(self.expect_terminator()?);

                Ok(decl! {
                    Module {
                        attributes,
                        span;
                        binder,
                        file: self.file,
                        declarations: Some(declarations),
                    }
                })
            }
            _ => self.error(|| {
                expected_one_of![Delimiter::Terminator, Of].but_actual_is(self.current_token())
            }),
        }
    }

    /// Parse the "top level" aka the body of a module file given the module name.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Top-Level ::= (#Line-Break | Declaration)* #End-Of-Input
    /// ```
    fn parse_top_level(&mut self, module_binder: Identifier) -> Result<Declaration> {
        let mut declarations = Vec::new();

        loop {
            if self.has_consumed(Semicolon) {
                continue;
            }

            if self.has_consumed(EndOfInput) {
                break Ok(decl! {
                    Module {
                        Attributes::new(),
                        self.map.borrow()[self.file].span();
                        binder: module_binder,
                        file: self.file,
                        declarations: Some(declarations)
                    }
                });
            }

            declarations.push(self.parse_declaration()?);
        }
    }

    fn parse_optional_block(&mut self, mut subparser: impl FnMut(&mut Self) -> Result) -> Result {
        if self.has_consumed(OpeningCurlyBracket) {
            loop {
                while self.has_consumed(Semicolon) {}

                if self.has_consumed(ClosingCurlyBracket) {
                    break;
                }

                subparser(self)?;
            }
        }

        Ok(())
    }

    /// Finish parsing a use-declaration.
    ///
    /// The keyword `use` should have already been parsed.
    /// The span does not contain the trailing line break.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Use-Declaration ::= "use" Use-Path-Tree Terminator
    /// ```
    fn finish_parse_use_declaration(
        &mut self,
        keyword_span: Span,
        attributes: Attributes,
    ) -> Result<Declaration> {
        let bindings = self.parse_use_path_tree(&[Delimiter::Terminator])?;
        self.expect_terminator()?;

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
    /// # Grammar
    ///
    /// ```ebnf
    /// Use-Path-Tree ::=
    ///     | Path
    ///     | Path "." "(" (Use-Path-Tree | "(" Renaming ")")* ")"
    ///     | Renaming
    /// Renaming ::= Path "as" Identifier
    /// ```
    // @Task rewrite this following a simpler grammar mirroring expression applications
    fn parse_use_path_tree(&mut self, delimiters: &[Delimiter]) -> Result<UsePathTree> {
        let mut path = self.parse_first_path_segment()?;

        while self.has_consumed(Dot) {
            match self.current_token().name() {
                Word | Punctuation => {
                    let identifier = self.current_token_into_identifier();
                    self.advance();
                    path.segments.push(identifier);
                }
                OpeningRoundBracket => {
                    let mut span = self.current_token().span;
                    self.advance();

                    let mut bindings = Vec::new();

                    while self.current_token().name() != ClosingRoundBracket {
                        if let Ok(bracket) =
                            self.manually_reflect(|this| this.consume(OpeningRoundBracket))
                        {
                            let mut span = bracket.span;

                            let target = self.parse_path()?;
                            self.consume(As)?;
                            let binder = self.consume_identifier()?;
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
                                // @Question Expected::Identifier?
                                Word.into(),
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
                        // @Question Expected::Identifier?
                        expected_one_of![Word, Punctuation, OpeningRoundBracket]
                            .but_actual_is(self.current_token())
                    });
                }
            }
        }

        // @Question is there a grammar transformation to a self-contained construct
        // instead of a delimited one?
        let binder = if self.current_token_is_delimiter(delimiters) {
            None
        } else if self.has_consumed(As) {
            self.current_token();
            Some(self.consume_identifier()?)
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

    /// Parse a (non-type) constructor.
    ///
    /// The span does not include the trailing line break.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Constructor ::=
    ///     (Attribute #Line-Break*)*
    ///     #Word Parameters Type-Annotation?
    ///     ("=" Expression)?
    ///     Terminator
    /// ```
    fn parse_constructor(&mut self) -> Result<Declaration> {
        let attributes = self.parse_attributes(SkipLineBreaks::Yes)?;

        let binder = self.consume_word()?;
        let mut span = binder.span();

        let parameters = span.merging(self.parse_parameters(&STANDARD_DECLARATION_DELIMITERS)?);

        let type_annotation = span.merging(self.parse_optional_type_annotation()?);

        let body = if self.has_consumed(Equals) {
            Some(span.merging(self.parse_expression()?))
        } else {
            None
        };

        self.expect_terminator()?;

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
    /// # Grammar
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
    /// # Grammar
    ///
    /// ```ebnf
    /// ; among other things, the grammar for pretty-printers differs from the one for parsers
    /// ; in that `Pi-Type-Literal-Or-Lower` also includes several complex (in the sense that they
    /// ; contain further expressions) `Lower-Expression`s namely let/in, use/in, lambda literals,
    /// ; case analyses and do blocks (not sure about sequence literals)
    /// Pi-Type-Literal-Or-Lower ::=
    ///     (Designated-Pi-Type-Domain | Application-Or-Lower)
    ///     "->" Pi-Type-Literal-Or-Lower
    /// Designated-Pi-Type-Domain ::= Explicitness "(" "lazy"? #Word Type-Annotation ")"
    /// ```
    fn parse_pi_type_literal_or_lower(&mut self) -> Result<Expression> {
        let domain = self
            .reflect(|this| {
                let explicitness = this.parse_optional_implicitness();
                let mut span = this
                    .consume(OpeningRoundBracket)?
                    .span
                    .merge_into(explicitness);

                let laziness = this.consume_span(Lazy);
                let binder = this.consume_word()?;
                // @Question should this be fatal in respect to reflecting?
                let domain = this.parse_type_annotation(ClosingRoundBracket.into())?;

                span.merging(&this.consume(ClosingRoundBracket)?);

                Ok(Spanned::new(
                    span,
                    Domain {
                        explicitness: explicitness.into(),
                        laziness,
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
                        laziness: None,
                        binder: None,
                        expression: domain,
                    },
                ))
            })?;

        if self.current_token().name() == ThinArrowRight {
            self.advance();

            let mut span = domain.span;
            let codomain = span.merging(self.parse_pi_type_literal_or_lower()?);

            Ok(expr! {
                PiTypeLiteral {
                    Attributes::new(),
                    span;
                    domain: domain.value,
                    codomain,
                }
            })
        }
        // the case where we don't actually have a pi type literal but merely
        // an application or lower
        else if domain.value.binder.is_none() {
            Ok(domain.value.expression)
        } else {
            self.error(|| {
                Expected::Token(ThinArrowRight)
                    .but_actual_is(self.current_token())
                    .labeled_secondary_span(domain, "start of a pi type literal")
            })
        }
    }

    /// Parse an application or a lower expression.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Application-Or-Lower ::= Lower-Expression Argument*
    /// Argument ::=
    ///     | Explicitness Lower-Expression
    ///     | Explicitness "(" (#Word "=")? Expression ")"
    ///
    /// ; ; left-recursive version unsuitable for the recursive descent parser
    /// ; ; but indeed usable for pretty-printers:
    /// ;
    /// ; Application-Or-Lower ::= Application-Or-Lower? Argument*
    /// ; Argument ::=
    /// ;     | Lower-Expression
    /// ;     | Explicitness "(" (#Word "=")? Expression ")"
    /// ```
    fn parse_application_or_lower(&mut self) -> Result<Expression> {
        self.parse_application_like_or_lower()
    }

    /// Parse a lower expression.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Lower-Expression ::= Attribute* Bare-Lower-Expression
    /// ; @Task rename into Field-Or-Lower
    /// Bare-Lower-Expression ::= Lowest-Expression ("::" Identifier)*
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
    /// Typed-Hole ::= "?" #Word
    /// Sequence-Literal ::= "[" Lower-Expression* "]"
    /// ```
    fn parse_lower_expression(&mut self) -> Result<Expression> {
        let attributes = self.parse_attributes(SkipLineBreaks::No)?;

        let mut span = self.current_token().span;
        let mut expression = match self.current_token().name() {
            name if name.is_path_head() => {
                let path = self.parse_path()?;
                expr! { Path(default(), path.span(); path) }
            }
            Type => {
                self.advance();
                expr! { TypeLiteral { default(), span } }
            }
            NumberLiteral => {
                // @Beacon @Task avoid clone!
                let token = self.current_token().clone();
                self.advance();
                expr! {
                    NumberLiteral(default(), token.span; token.into_number_literal().unwrap())
                }
            }
            TextLiteral => {
                // @Beacon @Task avoid clone!
                let token = self.current_token().clone();
                self.advance();

                let span = token.span;
                let text_literal = token
                    .into_text_literal()
                    .unwrap()
                    .or_else(|error| self.error(|| error))?;

                expr! { TextLiteral(default(), span; text_literal) }
            }
            QuestionMark => {
                self.advance();
                let tag = self.consume_word()?;
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
            // @Task move to a finish_parse_sequence_literal
            OpeningSquareBracket => {
                self.advance();

                let mut elements = Vec::new();

                while self.current_token().name() != ClosingSquareBracket {
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
                        .if_(self.current_token().name() == ThinArrowRight, |this| {
                            this.help(BRACKET_POTENTIAL_PI_TYPE_LITERAL)
                        })
                });
            }
        };

        let mut attributes = Some(attributes);

        while self.has_consumed(DoubleColon) {
            let member = self.consume_word()?;

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
    /// # Grammar
    ///
    /// ```ebnf
    /// Path ::= Path-Head ("." Identifier)*
    /// Path-Head ::= Path-Hanger | Identifier
    /// Path-Hanger ::= "extern" | "crate" | "super" | "self"
    /// ```
    fn parse_path(&mut self) -> Result<Path> {
        let mut path = self.parse_first_path_segment()?;

        while self.has_consumed(TokenName::Dot) {
            path.segments.push(self.consume_identifier()?);
        }

        Ok(path)
    }

    /// Parse the first segment of a path.
    fn parse_first_path_segment(&mut self) -> Result<Path> {
        let path = match self.current_token().name() {
            Word | Punctuation => Path::try_from_token(self.current_token().clone()).unwrap(),
            name if name.is_path_hanger() => Path::hanger(self.current_token().clone()),
            _ => return self.error(|| Expected::Path.but_actual_is(self.current_token())),
        };
        self.advance();
        Ok(path)
    }

    /// Finish parsing a lambda literal expression.
    ///
    /// The initial `\` should have already been parsed beforehand.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Lambda-Literal ::= "\" Parameters Type-Annotation? "=>" Expression
    /// ```
    fn finish_parse_lambda_literal(&mut self, keyword_span: Span) -> Result<Expression> {
        let mut span = keyword_span;
        let parameters = self.parse_parameters(&[
            Delimiter::TypeAnnotationPrefix,
            TokenName::WideArrowRight.into(),
        ])?;
        let body_type_annotation = self.parse_optional_type_annotation()?;
        self.consume(TokenName::WideArrowRight)?;
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

    /// Finish parsing an let/in-expression.
    ///
    /// The initial `let` should have already been parsed beforehand.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Let-In ::=
    ///     "let" #Word Parameters Type-Annotation?
    ///     ("=" Expression)?
    ///     #Virtual-Semicolon?
    ///     "in" Expression
    /// ```
    fn finish_parse_let_in(&mut self, span_of_let: Span) -> Result<Expression> {
        // @Task recover from two consecutive `in`s

        let mut span = span_of_let;

        let binder = self.consume_word()?;

        // @Task smh add line break aka virtual semicolon
        let parameters = self.parse_parameters(&[
            Delimiter::TypeAnnotationPrefix,
            Delimiter::DefinitionPrefix,
            In.into(),
        ])?;
        let type_annotation = self.parse_optional_type_annotation()?;

        let expression = if self.has_consumed(Equals) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        if self.current_token().is_line_break() {
            self.advance();
        }

        self.consume(In)?;

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

    /// Finish parsing a use/in-expression.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Use-In ::=
    ///     "use" Use-Path-Tree
    ///     #Virtual-Semicolon?
    ///     "in" Expression
    /// ```
    fn finish_parse_use_in(&mut self, span_of_use: Span) -> Result<Expression> {
        // @Task recover from two consecutive `in`s

        // @Task smh add line break aka virtual semicolon
        let bindings = self.parse_use_path_tree(&[In.into()])?;

        if self.current_token().is_line_break() {
            self.advance();
        }

        self.consume(In)?;

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
    /// # Grammar
    ///
    ///
    /// ```ebnf
    /// Case-Analysis ::= "case" Expression "of" ("{" Case* "}")?
    /// Case ::= Pattern "=>" Expression Terminator
    /// ```
    fn finish_parse_case_analysis(&mut self, span_of_case: Span) -> Result<Expression> {
        let mut span = span_of_case;

        let scrutinee = self.parse_expression()?;
        span.merging(self.consume(Of)?);

        let mut cases = Vec::new();

        if self.has_consumed(OpeningCurlyBracket) {
            // @Task use parse_block function for this (but don't trash the span!)

            while self.current_token().name() != ClosingCurlyBracket {
                let pattern = self.parse_pattern()?;
                self.consume(WideArrowRight)?;
                let body = self.parse_expression()?;
                self.expect_terminator()?;

                cases.push(ast::Case { pattern, body });
            }

            // @Beacon :while_current_not
            span.merging(self.current_token());
            self.advance();
        }

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
    /// # Grammar
    ///
    /// ```ebnf
    /// Do-Block ::= "do" "{" Statement* "}"
    /// Statement ::= Let-Statement | Use-Declaration | Bind-Statement | Expression-Statement
    /// Let-Statement ::= "let" Function-Declaration
    /// Bind-Statement ::= #Word Type-Annotation? "<-" Expression Terminator
    /// Expression-Statement ::= Expression Terminator
    /// ```
    ///
    /// Bind statements are the worst right now. We need to look ahead for `:` (type annotation)
    /// or `<-` to differenciate them from expressions. Maybe there is prefix-oriented syntax
    /// we could switch to like `!x = …` or `set x = …`. The latter look-ahead is not much of an
    /// issue, `:` is a bad *but only in case* of adding type annotation expressions (not that likely
    /// as they clash with other syntactic elements like pi literals).
    fn finish_parse_do_block(&mut self, span_of_do: Span) -> Result<Expression> {
        let mut span = span_of_do;
        let mut statements = Vec::new();

        self.consume(OpeningCurlyBracket)?;

        while self.current_token().name() != ClosingCurlyBracket {
            // @Note necessary I guess in cases where we have #Line-Break ((Comment)) #Line-Break
            if self.has_consumed(Semicolon) {
                continue;
            }

            statements.push(match self.current_token().name() {
                // @Task move to its own function
                Let => {
                    self.advance();
                    let binder = self.consume_word()?;
                    let parameters = self.parse_parameters(&[
                        Delimiter::TypeAnnotationPrefix,
                        Delimiter::DefinitionPrefix,
                    ])?;
                    let type_annotation = self.parse_optional_type_annotation()?;
                    self.consume(TokenName::Equals)?;
                    let expression = self.parse_expression()?;
                    self.expect_terminator()?;

                    Statement::Let(LetStatement {
                        binder,
                        parameters,
                        type_annotation,
                        expression,
                    })
                }
                Use => {
                    self.advance();
                    let bindings = self.parse_use_path_tree(&[Delimiter::Terminator])?;
                    self.expect_terminator()?;

                    Statement::Use(ast::Use { bindings })
                }
                _ => {
                    if self.current_token().name() == Word
                        && matches!(self.succeeding_token().name(), Colon | ThinArrowLeft)
                    {
                        // @Task move to its own function
                        let binder = self.current_token_into_identifier();
                        self.advance();
                        let type_annotation = self.parse_optional_type_annotation()?;
                        self.consume(ThinArrowLeft)?;
                        let expression = self.parse_expression()?;
                        self.expect_terminator()?;

                        Statement::Bind(BindStatement {
                            binder,
                            type_annotation,
                            expression,
                        })
                    } else {
                        // @Task improve error diagnostics for an unexpected token to not only mention an
                        // expression was expected but also statements were
                        let expression = self.parse_expression()?;
                        self.expect_terminator()?;

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
    /// A delimiter must not be [`TokenName::OpeningRoundBracket`] or
    /// [`TokenName::Word`]. The delimiter list must be non-empty.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Parameters ::= Parameter*
    /// ```
    fn parse_parameters(&mut self, delimiters: &[Delimiter]) -> Result<Parameters> {
        let mut parameters = Vec::new();

        // @Task rewrite to check whether self.current_token().name().is_parameter_prefix()
        // with the def: matches!(_, Apostrophe, Word, OpeningCurlyBracket)
        // @Note however, keep using the parameter `delimiters` for error reporting!
        while !self.current_token_is_delimiter(delimiters) {
            parameters.push(self.parse_parameter(delimiters)?);
        }

        Ok(parameters)
    }

    /// Parse a parameter group.
    ///
    /// Delimiters are taken as a parameter merely for constructing
    /// the Diagnostic.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Parameter ::= Explicitness Bare-Parameter
    /// Bare-Parameter ::= #Word | "(" "lazy"? #Word Type-Annotation? ")"
    /// ```
    fn parse_parameter(&mut self, delimiters: &[Delimiter]) -> Result<Parameter> {
        #![allow(clippy::shadow_unrelated)] // false positive

        let explicitness = self.parse_optional_implicitness();
        let mut span = self.current_token().span.merge_into(explicitness);

        match self.current_token().name() {
            Word => {
                let binder = self.current_token_into_identifier();
                self.advance();

                Ok(Parameter::new(
                    span,
                    ParameterKind {
                        explicitness: explicitness.into(),
                        laziness: None,
                        binder,
                        type_annotation: None,
                    },
                ))
            }
            OpeningRoundBracket => {
                self.advance();

                let laziness = self.consume_span(Lazy);
                let binder = self.consume_word()?;
                let type_annotation = self.parse_optional_type_annotation()?;

                span.merging(self.consume_after_expecting(
                    ClosingRoundBracket,
                    Delimiter::TypeAnnotationPrefix.into(),
                )?);

                Ok(Parameter::new(
                    span,
                    ParameterKind {
                        explicitness: explicitness.into(),
                        laziness,
                        binder,
                        type_annotation,
                    },
                ))
            }
            _ => self.error(|| {
                delimiters_with_expected(delimiters, Some(Expected::Parameter))
                    .but_actual_is(self.current_token())
            }),
        }
    }

    /// Parse a pattern.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Pattern ::= Lower-Pattern Pattern-Argument*
    /// Pattern-Argument ::=
    ///     | Explicitness Lower-Pattern
    ///     | Explicitness "(" (#Word "=")? Pattern ")"
    /// ```
    // @Task add alternative precedence for formatting to the documentation above
    fn parse_pattern(&mut self) -> Result<Pattern> {
        self.parse_application_like_or_lower()
    }

    /// Parse a (de)application or something lower.
    // @Task rewrite this with a `delimiter: Delimiter`-parameter for better error messages!
    fn parse_application_like_or_lower<Expat: ExpressionOrPattern>(&mut self) -> Result<Expat> {
        let mut callee = Expat::parse_lower(self)?;
        struct Argument<Expat> {
            explicitness: Explicitness,
            binder: Option<Identifier>,
            expat: Expat,
        }

        let mut illegal_pi = None;

        while let Ok(argument) = self
            .reflect(|this| {
                // @Beacon @Question can pi_type_literal_was_used_as_lower_expression also happen here???
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

                    let binder = this.consume_word()?;

                    if Expat::IS_EXPRESSION && this.current_token().name() == Colon {
                        illegal_pi = Some(this.current_token().clone());
                        this.advance();
                    } else {
                        this.consume(Equals)?;
                    }

                    let argument = Expat::parse(this)?;

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
            if let Some(token) = &illegal_pi {
                let explicitness = match argument.value.explicitness {
                    Implicit => "an implicit",
                    Explicit => "a",
                };

                self.error(|| expected_one_of![Expected::Expression, Equals]
                    .but_actual_is(token)
                    .labeled_secondary_span(
                        &argument,
                        format!("treated as {explicitness} function argument,\nnot as the domain of a pi type literal"),
                    )
                    .help(BRACKET_POTENTIAL_PI_TYPE_LITERAL))?;
            }

            let span = callee.span().merge(&argument);

            callee = Expat::application_like(
                callee,
                argument.value.expat,
                argument.value.explicitness,
                argument.value.binder,
                span,
            );
        }

        Ok(callee)
    }

    /// Parse a lower pattern.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Lower-Pattern ::= Attribute* Bare-Lower-Pattern
    /// Bare-Lower-Pattern ::=
    ///     | Path
    ///     | #Number-Literal
    ///     | #Text-Literal
    ///     | Binder
    ///     | Sequence-Literal-Pattern
    ///     | "(" Pattern ")"
    /// Binder ::= "\" #Word
    /// Sequence-Literal-Pattern ::= "[" Lower-Pattern* "]"
    /// ```
    fn parse_lower_pattern(&mut self) -> Result<Pattern> {
        let attributes = self.parse_attributes(SkipLineBreaks::No)?;

        let mut span = self.current_token().span;
        match self.current_token().name() {
            name if name.is_path_head() => self.parse_path().map(|path| {
                pat! { Path(attributes, path.span(); path) }
            }),
            NumberLiteral => {
                let token = self.current_token().clone();
                self.advance();
                Ok(pat! {
                    NumberLiteral(attributes, token.span; token.into_number_literal().unwrap())
                })
            }
            TextLiteral => {
                let token = self.current_token().clone();
                self.advance();
                let span = token.span;
                let text_literal = token
                    .into_text_literal()
                    .unwrap()
                    .or_else(|error| self.error(|| error))?;

                Ok(pat! { TextLiteral(attributes, span; text_literal) })
            }
            Backslash => {
                self.advance();
                self.consume_word()
                    .map(|binder| pat! { Binder { attributes, span.merge(&binder); binder } })
            }
            OpeningSquareBracket => {
                self.advance();

                let mut elements = Vec::new();

                while self.current_token().name() != ClosingSquareBracket {
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
    /// # Grammar
    ///
    /// ```ebnf
    /// Type-Annotation ::= ":" Expression
    /// ```
    fn parse_type_annotation(&mut self, other_expected: Expected) -> Result<Expression> {
        // @Note that the error message that can be thrown by this method will actually
        // very likely to certainly not show up in the final report since it gets swallowed
        // by reflect (the branch do-not-reflect experiments with this stuff and makes the
        // below actually show up)
        self.consume_after_expecting(TokenName::Colon, other_expected)?;
        self.reflect(Self::parse_expression)
    }

    /// Parse an optional type annotation.
    fn parse_optional_type_annotation(&mut self) -> Result<Option<Expression>> {
        self.has_consumed(TokenName::Colon)
            .then(|| self.parse_expression())
            .transpose()
    }

    /// Expect a terminator.
    ///
    /// If the terminator is a semicolon, consume it.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// ; #Start-Of-Input is not actually emitted by the lexer, the parsers needs to bound-check instead.
    /// Terminator ::= ";" | (> "}" | #End-Of-Input) | (< #Start-Of-Input | ";" | "}")
    /// ```
    fn expect_terminator(&mut self) -> Result<Option<Token>> {
        let token = self.current_token();
        if token.name().is_terminator()
            || self
                .preceeding_token()
                .map_or(true, |token| token.name().is_terminator())
        {
            if token.name() == Semicolon {
                let token = token.clone();
                self.advance();

                Ok(Some(token))
            } else {
                Ok(None)
            }
        } else {
            Expected::Delimiter(Delimiter::Terminator)
                .but_actual_is(self.current_token())
                .report(self.reporter);
            Err(())
        }
    }

    /// Consume the explicitness symbol.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Explicitness ::= "'"?
    /// ```
    fn parse_optional_implicitness(&mut self) -> SpannedExplicitness {
        match self.current_token().name() {
            TokenName::Apostrophe => {
                let span = self.current_token().span;
                self.advance();
                SpannedExplicitness::Implicit { marker: span }
            }
            _ => SpannedExplicitness::Explicit,
        }
    }
}

/// Abstraction over expressions and patterns.
// @Task consider replacing this with an enum
trait ExpressionOrPattern: Sized + Spanning + std::fmt::Debug {
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
    Token(TokenName),
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
            self = expected_one_of![self, extra];
        }

        self
    }
}

// @Task improve API
macro expected_one_of($( $expected:expr ),+ $(,)?) {
    Expected::OneOf(vec![$( $expected.into() ),+])
}

// @Task improve API
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
            .code(Code::E010)
            .message(format!("found {actual}, but expected {self}"))
            .labeled_primary_span(actual, "unexpected token")
    }
}

impl From<TokenName> for Expected {
    fn from(token: TokenName) -> Self {
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

#[derive(Clone, Copy)]
enum Delimiter {
    TypeAnnotationPrefix,
    DefinitionPrefix,
    Terminator,
    Token(TokenName),
}

impl Delimiter {
    fn matches(self, token: TokenName) -> bool {
        match (self, token) {
            (Self::TypeAnnotationPrefix, Colon)
            | (Self::DefinitionPrefix, Equals)
            | (Self::Terminator, Semicolon | ClosingCurlyBracket | EndOfInput) => true,
            (Self::Token(expected), actual) => expected == actual,
            _ => false,
        }
    }
}

impl fmt::Display for Delimiter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TypeAnnotationPrefix => write!(f, "type annotation"),
            Self::DefinitionPrefix => write!(f, "definition with `=`"),
            // @Question or spell it out? `;`, line break, `}`, dedentation, end of input?
            Self::Terminator => write!(f, "terminator"),
            Self::Token(token) => write!(f, "{}", token),
        }
    }
}

impl From<TokenName> for Delimiter {
    fn from(token: TokenName) -> Self {
        Self::Token(token)
    }
}

#[derive(PartialEq, Eq)]
enum SkipLineBreaks {
    Yes,
    No,
}
