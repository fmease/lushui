//! The syntactic analyzer (parser).
//!
//! It is a handwritten top-down recursive-descent parser with arbitrary look-ahead
//! and backtracking. Backtracking is only used to parse expressions and patterns.
//!
//! # Grammar Notation
//!
//! Most parsing functions in this module are accompanied by a grammar snippet.
//! These snippets are written in an EBNF explained below:
//!
//! | Notation  | Name                                | Definition or Remark                                          |
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

#[allow(clippy::wildcard_imports)]
use super::{
    ast::{
        self, Attribute, AttributeArgument, AttributeArgumentKind, AttributeKind, Attributes,
        BindStatement, Declaration, Domain,
        Explicitness::{self, Explicit, Implicit},
        Expression, Identifier, LetStatement, Parameter, ParameterKind, Parameters, Path, Pattern,
        SpannedExplicitness, Statement, UsePathTree, UsePathTreeKind,
    },
    token::{
        Token,
        TokenName::{self, *},
    },
    Word,
};
use crate::{
    diagnostics::{reporter::ErasedReportedError, Code, Diagnostic},
    error::Result,
    session::BuildSession,
    span::{SourceFileIndex, Span, Spanned, Spanning},
    utility::{Conjunction, ListingExt, SmallVec},
};
use std::{any::TypeId, default::default};

#[cfg(test)]
mod test;

const STANDARD_DECLARATION_DELIMITERS: [Delimiter; 3] = {
    use Delimiter::*;

    [Terminator, TypeAnnotationPrefix, DefinitionPrefix]
};

const BRACKET_POTENTIAL_PI_TYPE_LITERAL: &str =
    "add round brackets around the potential pi type literal to disambiguate the expression";

/// Parse the file of a root module / component root.
pub fn parse_root_module_file(
    tokens: &[Token],
    file: SourceFileIndex,
    session: &BuildSession,
) -> Result<Declaration> {
    // @Beacon @Task don't unwrap to_str here but handle the error correctly
    // (create another parsing function on ComponentName)
    let name = session.shared_map()[file]
        .path()
        .unwrap()
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap()
        .to_owned();

    let binder = Spanned::new(
        default(),
        Word::parse(name.clone()).map_err(|_| {
            // @Beacon @Task add component+packagename(+path?) and other details/explanations
            // @Question is the common code justified?
            Diagnostic::error()
                .code(Code::E036)
                .message(format!(
                    "the name of the root module `{name}` is not a valid word"
                ))
                .report(session.reporter())
        })?,
    )
    .into();

    parse_module_file(tokens, file, binder, session)
}

/// Parse the file of a root module or an out-of-line module.
pub(crate) fn parse_module_file(
    tokens: &[Token],
    file: SourceFileIndex,
    binder: Identifier,
    session: &BuildSession,
) -> Result<Declaration> {
    Parser::new(tokens, file, session).parse_top_level(binder)
}

pub(super) fn parse_path(
    tokens: &[Token],
    file: SourceFileIndex,
    session: &BuildSession,
) -> Result<Path> {
    Parser::new(tokens, file, session).parse_path()
}

/// The state of the parser.
// @Beacon @Beacon @Beacon @Task create stripped down version of the Parser
// that does not have `map` and `file` (naming?) and move all parsing methods that don't
// need those files to it
struct Parser<'a> {
    tokens: &'a [Token],
    file: SourceFileIndex,
    look_ahead: u16,
    index: usize,
    session: &'a BuildSession,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token], file: SourceFileIndex, session: &'a BuildSession) -> Self {
        Self {
            tokens,
            file,
            look_ahead: 0,
            index: 0,
            session,
        }
    }

    /// Execute the given parser and backtrack to the original position on error without emitting a diagnostic.
    fn parse_or_backtrack<T>(&mut self, parser: impl FnOnce(&mut Self) -> Result<T>) -> Result<T> {
        let index = self.index;
        let result = self.look_ahead(parser);

        if result.is_err() {
            self.index = index;
        }

        result
    }

    fn look_ahead<T>(&mut self, parser: impl FnOnce(&mut Self) -> T) -> T {
        self.look_ahead += 1;
        let result = parser(self);
        self.look_ahead -= 1;
        result
    }

    fn is_looking_ahead(&self) -> bool {
        self.look_ahead != 0
    }

    fn error<T, D: FnOnce() -> Diagnostic>(&self, diagnostic: D) -> Result<T> {
        fn error<D: FnOnce() -> Diagnostic>(parser: &Parser<'_>, diagnostic: D) -> Result<!> {
            if !parser.is_looking_ahead() {
                diagnostic().report(parser.session.reporter());
            }

            // @Note I am not really happy with this: if we are not "looking ahead", we won't actually
            // emit an error @Task smh change the error type to reflect the semantics
            Err(ErasedReportedError::new_unchecked())
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
        self.consume(Word)
            .map(|identifier| identifier.try_into().unwrap())
    }

    /// Consume an identifier.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Identifier ::= #Word | #Punctuation
    /// ```
    fn consume_identifier(&mut self) -> Result<Identifier> {
        match self.current_token().name() {
            Word | Punctuation => {
                let identifier = self.current_token_into_identifier();
                self.advance();
                Ok(identifier)
            }
            _ => self.error(|| Expected::Identifier.but_actual_is(self.current_token())),
        }
    }

    /// Indicate whether the given token was consumed.
    ///
    /// Conceptually equivalent to `self.look_ahead(|this| this.consume(..)).is_ok()`
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
    /// Panics if the current token is the [end of input](EndOfInput).
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

    /// Finish parsing a regular attribute given the span of the already parsed leading `@`.
    ///
    /// # Grammar
    ///
    /// Note: The grammar is not complete yet since we cannot represent the
    /// arguments of `@if` yet which are the most complex.
    ///
    /// ```ebnf
    /// Regular-Attribute ::= "@" (#Word | "(" #Word Attribute-Argument* ")")
    /// ```
    fn finish_parse_regular_attribute(&mut self, mut span: Span) -> Result<Attribute> {
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

                AttributeArgument::new(path.span(), AttributeArgumentKind::Path(Box::new(path)))
            }
            NumberLiteral => {
                let token = self.current_token().clone();
                self.advance();

                AttributeArgument::new(
                    token.span,
                    AttributeArgumentKind::NumberLiteral(token.into_number_literal().unwrap()),
                )
            }
            TextLiteral => {
                let token = self.current_token().clone();
                self.advance();

                AttributeArgument::new(
                    token.span,
                    AttributeArgumentKind::TextLiteral(
                        token
                            .into_text_literal()
                            .unwrap()
                            .or_else(|error| self.error(|| error))?,
                    ),
                )
            }
            OpeningRoundBracket => {
                let mut span = self.current_token().span;
                self.advance();
                let binder = self.consume_word()?;
                let value = self.parse_attribute_argument()?;
                span.merging(&self.consume(ClosingRoundBracket)?);

                AttributeArgument::new(
                    span,
                    AttributeArgumentKind::Named(Box::new(ast::NamedAttributeArgument {
                        binder,
                        value,
                    })),
                )
            }
            _ => {
                return self
                    .error(|| Expected::AttributeArgument.but_actual_is(self.current_token()))
            }
        })
    }

    /// Finish parsing a function declaration given the already parsed leading word.
    ///
    /// The span does not include the terminator.
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

        Ok(Declaration::new(
            attributes,
            span,
            ast::Function {
                binder,
                parameters,
                type_annotation,
                body,
            }
            .into(),
        ))
    }

    /// Finish parsing a data declaration given the span of the already parsed leading `data` keyword.
    ///
    /// The span does not include the terminator.
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
        mut span: Span,
        attributes: Attributes,
    ) -> Result<Declaration> {
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

        Ok(Declaration::new(
            attributes,
            span.merge(span),
            ast::Data {
                binder,
                parameters,
                type_annotation,
                constructors,
            }
            .into(),
        ))
    }

    /// Finish parsing module declaration given the span of the already parsed leading `module` keyword.
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
        mut span: Span,
        attributes: Attributes,
    ) -> Result<Declaration> {
        match self.current_token().name() {
            name if name.is_terminator() => {
                if name == Semicolon {
                    self.advance();
                }

                return Ok(Declaration::new(
                    attributes,
                    span,
                    ast::DeclarationKind::ModuleHeader,
                ));
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

                Ok(Declaration::new(
                    attributes,
                    span,
                    ast::Module {
                        binder,
                        file: self.file,
                        declarations: None,
                    }
                    .into(),
                ))
            }
            Of => {
                self.advance();
                let mut declarations = Vec::new();

                self.parse_optional_block(|this| {
                    declarations.push(this.parse_declaration()?);
                    Ok(())
                })?;

                span.merging(self.expect_terminator()?);

                Ok(Declaration::new(
                    attributes,
                    span,
                    ast::Module {
                        binder,
                        file: self.file,
                        declarations: Some(declarations),
                    }
                    .into(),
                ))
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
                break Ok(Declaration::new(
                    Attributes::new(),
                    self.session.shared_map()[self.file].span(),
                    ast::Module {
                        binder: module_binder,
                        file: self.file,
                        declarations: Some(declarations),
                    }
                    .into(),
                ));
            }

            declarations.push(self.parse_declaration()?);
        }
    }

    fn parse_optional_block(&mut self, mut parser: impl FnMut(&mut Self) -> Result) -> Result {
        if self.has_consumed(OpeningCurlyBracket) {
            loop {
                while self.has_consumed(Semicolon) {}

                if self.has_consumed(ClosingCurlyBracket) {
                    break;
                }

                parser(self)?;
            }
        }

        Ok(())
    }

    /// Finish parsing a use-declaration given the span of the already parsed leading `use` keyword.
    ///
    /// The span does not contain the trailing line break.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Use-Declaration ::= "use" Use-Path-Tree Terminator
    /// ```
    fn finish_parse_use_declaration(
        &mut self,
        span: Span,
        attributes: Attributes,
    ) -> Result<Declaration> {
        let bindings = self.parse_use_path_tree(&[Delimiter::Terminator])?;
        self.expect_terminator()?;

        Ok(Declaration::new(
            attributes,
            span.merge(&bindings),
            ast::Use { bindings }.into(),
        ))
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
                            self.look_ahead(|this| this.consume(OpeningRoundBracket))
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
                                Topmost.into(),
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
                        expected_one_of![Expected::Identifier, OpeningRoundBracket]
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

        Ok(Declaration::new(
            attributes,
            span,
            ast::Constructor {
                binder,
                parameters,
                type_annotation,
                body,
            }
            .into(),
        ))
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
    ///     (Designated-Pi-Type-Domain | Application-Expression-Or-Lower)
    ///     "->" Pi-Type-Literal-Or-Lower
    /// Designated-Pi-Type-Domain ::= Explicitness "(" "lazy"? #Word Type-Annotation ")"
    /// ```
    fn parse_pi_type_literal_or_lower(&mut self) -> Result<Expression> {
        let domain = self
            .parse_or_backtrack(|this| {
                let explicitness = this.parse_optional_implicitness();
                let mut span = this
                    .consume(OpeningRoundBracket)?
                    .span
                    .merge_into(explicitness);

                let laziness = this.consume_span(Lazy);
                let binder = this.consume_word()?;
                // @Question should a failure be fatal here or should we keep backtracking?
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
                let domain = self.parse_application_expression_or_lower()?;
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

            Ok(Expression::new(
                Attributes::new(),
                span,
                ast::PiTypeLiteral {
                    domain: domain.bare,
                    codomain,
                }
                .into(),
            ))
        }
        // the case where we don't actually have a pi type literal but merely
        // an application or lower
        else if domain.bare.binder.is_none() {
            Ok(domain.bare.expression)
        } else {
            self.error(|| {
                Expected::Token(ThinArrowRight)
                    .but_actual_is(self.current_token())
                    .labeled_secondary_span(domain, "start of a pi type literal")
            })
        }
    }

    /// Parse an application expression or lower.
    /// # Grammar
    ///
    /// ```ebnf
    /// Application-Expression-Or-Lower ::= Lower-Expression Expression-Argument*
    /// Expression-Argument ::=
    ///     | Explicitness Lower-Expression
    ///     | Explicitness "(" (#Word "=")? Expression ")"
    ///
    /// ; ; left-recursive version unsuitable for the recursive descent parser
    /// ; ; but indeed usable for pretty-printers:
    /// ;
    /// ; Application-Expression-Or-Lower ::= Application-Expression-Or-Lower? Expression-Argument*
    /// ; Expression-Argument ::=
    /// ;     | Lower-Expression
    /// ;     | Explicitness "(" (#Word "=")? Expression ")"
    /// ```
    fn parse_application_expression_or_lower(&mut self) -> Result<Expression> {
        self.parse_application_or_lower()
    }

    /// Parse a lower expression.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Lower-Expression ::= Attribute* Bare-Lower-Expression
    /// Bare-Lower-Expression ::= Lowest-Expression ("::" Identifier)*
    /// Lowest-Expression ::=
    ///     | "Type"
    ///     | #Number-Literal
    ///     | #Text-Literal
    ///     | Typed-Hole
    ///     | Let-In
    ///     | Use-In
    ///     | Lambda-Literal
    ///     | Case-Analysis
    ///     | Do-Block
    ///     | Sequence-Literal-Expression
    ///     | Path-Or-Namespaced-Literal-Expression
    ///     | "(" Expression ")"
    /// Typed-Hole ::= "?" #Word
    /// ```
    fn parse_lower_expression(&mut self) -> Result<Expression> {
        let attributes = self.parse_attributes(SkipLineBreaks::No)?;

        let mut span = self.current_token().span;
        let mut expression = match self.current_token().name() {
            Type => {
                self.advance();

                Expression::new(default(), span, ast::ExpressionKind::TypeLiteral)
            }
            NumberLiteral => {
                let token = self.current_token().clone();
                self.advance();

                Expression::new(
                    default(),
                    token.span,
                    ast::NumberLiteral {
                        path: None,
                        literal: Spanned::new(token.span, token.into_number_literal().unwrap()),
                    }
                    .into(),
                )
            }
            TextLiteral => {
                let token = self.current_token().clone();
                self.advance();

                Expression::new(
                    default(),
                    token.span,
                    ast::TextLiteral {
                        path: None,
                        literal: Spanned::new(
                            token.span,
                            token
                                .into_text_literal()
                                .unwrap()
                                .or_else(|error| self.error(|| error))?,
                        ),
                    }
                    .into(),
                )
            }
            QuestionMark => {
                self.advance();
                let tag = self.consume_word()?;

                Expression::new(default(), span.merge(&tag), ast::TypedHole { tag }.into())
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
                self.finish_parse_sequence_literal(None, span)?
            }
            OpeningRoundBracket => {
                self.advance();

                let mut expression = self.parse_expression()?;

                span.merging(self.consume(ClosingRoundBracket)?);
                expression.span = span;

                expression
            }
            name if name.is_path_head() => self.parse_path_or_namespaced_literal()?,
            _ => {
                return self.error(|| {
                    Expected::Expression
                        .but_actual_is(self.current_token())
                        .with(|error| {
                            // @Beacon @Note this is a prime example for a situation where we can
                            // make a parsing error non-fatal: we can just skip the `->` and keep
                            // parsing w/o introducing too many (any?) useless/confusing consequential
                            // errors!
                            if self.current_token().name() == ThinArrowRight {
                                error.help(BRACKET_POTENTIAL_PI_TYPE_LITERAL)
                            } else {
                                error
                            }
                        })
                });
            }
        };

        let mut attributes = Some(attributes);

        while self.has_consumed(DoubleColon) {
            let member = self.consume_word()?;

            expression = Expression::new(
                attributes.take().unwrap_or_default(),
                expression.span.merge(&member),
                ast::Field {
                    base: expression,
                    member,
                }
                .into(),
            );
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
    /// Path-Hanger ::= "extern" | "topmost" | "super" | "self"
    /// ```
    fn parse_path(&mut self) -> Result<Path> {
        let mut path = self.parse_first_path_segment()?;

        while self.has_consumed(Dot) {
            path.segments.push(self.consume_identifier()?);
        }

        Ok(path)
    }

    /// Parse the first segment of a path.
    fn parse_first_path_segment(&mut self) -> Result<Path> {
        let path = match self.current_token().name() {
            Word | Punctuation => Identifier::try_from(self.current_token().clone())
                .unwrap()
                .into(),
            name if name.is_path_hanger() => ast::Hanger::try_from(self.current_token().clone())
                .unwrap()
                .into(),
            _ => return self.error(|| Expected::Path.but_actual_is(self.current_token())),
        };
        self.advance();
        Ok(path)
    }

    /// Finish parsing a lambda literal given the span of the already parsed leading `\`.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Lambda-Literal ::= "\" Parameters Type-Annotation? "=>" Expression
    /// ```
    fn finish_parse_lambda_literal(&mut self, mut span: Span) -> Result<Expression> {
        let parameters =
            self.parse_parameters(&[Delimiter::TypeAnnotationPrefix, WideArrowRight.into()])?;
        let body_type_annotation = self.parse_optional_type_annotation()?;
        self.consume(WideArrowRight)?;
        let body = span.merging(self.parse_expression()?);

        Ok(Expression::new(
            Attributes::new(),
            span,
            ast::LambdaLiteral {
                parameters,
                body_type_annotation,
                body,
            }
            .into(),
        ))
    }

    /// Finish parsing an let/in-expression given the span of the already parsed leading `let` keyword.
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
    fn finish_parse_let_in(&mut self, mut span: Span) -> Result<Expression> {
        let binder = self.consume_word()?;

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

        Ok(Expression::new(
            Attributes::new(),
            span,
            ast::LetIn {
                binder,
                parameters,
                type_annotation,
                expression,
                scope,
            }
            .into(),
        ))
    }

    /// Finish parsing a use/in-expression given the span of the already parsed leading `use` keyword.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Use-In ::=
    ///     "use" Use-Path-Tree
    ///     #Virtual-Semicolon?
    ///     "in" Expression
    /// ```
    fn finish_parse_use_in(&mut self, span: Span) -> Result<Expression> {
        let bindings = self.parse_use_path_tree(&[In.into()])?;

        if self.current_token().is_line_break() {
            self.advance();
        }

        self.consume(In)?;

        let scope = self.parse_expression()?;

        Ok(Expression::new(
            Attributes::new(),
            span.merge(&scope),
            ast::UseIn { bindings, scope }.into(),
        ))
    }

    /// Finish parsing a case analysis given the span of the already parsed leading `case` keyword.
    ///
    /// # Grammar
    ///
    ///
    /// ```ebnf
    /// Case-Analysis ::= "case" Expression "of" ("{" Case* "}")?
    /// Case ::= Pattern "=>" Expression Terminator
    /// ```
    fn finish_parse_case_analysis(&mut self, mut span: Span) -> Result<Expression> {
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

        Ok(Expression::new(
            Attributes::new(),
            span,
            ast::CaseAnalysis { scrutinee, cases }.into(),
        ))
    }

    /// Finish parsing a do block given the span of the already parsed leading `do` keyword.
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
    fn finish_parse_do_block(&mut self, mut span: Span) -> Result<Expression> {
        let mut statements = Vec::new();

        self.consume(OpeningCurlyBracket)?;

        while self.current_token().name() != ClosingCurlyBracket {
            // @Note necessary I guess in cases where we have #Line-Break ##Comment+ #Line-Break
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
                    self.consume(Equals)?;
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

        Ok(Expression::new(
            Attributes::new(),
            span,
            ast::DoBlock { statements }.into(),
        ))
    }

    /// Parse parameters until one of the given delimiters is encountered.
    ///
    /// One needs to specify delimiters to allow for better error diagnostics.
    /// A delimiter must not be [`OpeningRoundBracket`] or
    /// [`Word`]. The delimiter list must be non-empty.
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
    fn parse_pattern(&mut self) -> Result<Pattern> {
        self.parse_application_or_lower()
    }

    /// Parse a lower pattern.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Lower-Pattern ::= Attribute* Bare-Lower-Pattern
    /// Bare-Lower-Pattern ::=
    ///     | #Number-Literal
    ///     | #Text-Literal
    ///     | Binder
    ///     | Sequence-Literal-Pattern
    ///     | Path-Or-Namespaced-Literal-Pattern
    ///     | "(" Pattern ")"
    /// Binder ::= "\" #Word
    /// ```
    fn parse_lower_pattern(&mut self) -> Result<Pattern> {
        let attributes = self.parse_attributes(SkipLineBreaks::No)?;

        let mut span = self.current_token().span;
        let mut pattern = match self.current_token().name() {
            NumberLiteral => {
                let token = self.current_token().clone();
                self.advance();

                Pattern::new(
                    default(),
                    token.span,
                    ast::NumberLiteral {
                        path: None,
                        literal: Spanned::new(token.span, token.into_number_literal().unwrap()),
                    }
                    .into(),
                )
            }
            TextLiteral => {
                let token = self.current_token().clone();
                self.advance();

                Pattern::new(
                    default(),
                    token.span,
                    ast::TextLiteral {
                        path: None,
                        literal: Spanned::new(
                            token.span,
                            token
                                .into_text_literal()
                                .unwrap()
                                .or_else(|error| self.error(|| error))?,
                        ),
                    }
                    .into(),
                )
            }
            Backslash => {
                self.advance();
                self.consume_word()
                    .map(|binder| Pattern::new(default(), span.merge(&binder), binder.into()))?
            }
            OpeningSquareBracket => {
                self.advance();

                let mut elements = Vec::new();

                while self.current_token().name() != ClosingSquareBracket {
                    elements.push(self.parse_lower_pattern()?);
                }

                span.merging(self.current_token());
                self.advance();

                Pattern::new(
                    default(),
                    span,
                    ast::SequenceLiteral {
                        path: None,
                        elements: Spanned::new(span, elements),
                    }
                    .into(),
                )
            }
            OpeningRoundBracket => {
                self.advance();
                let mut pattern = self.parse_pattern()?;
                span.merging(self.consume(ClosingRoundBracket)?);
                pattern.span = span;

                pattern
            }
            name if name.is_path_head() => self.parse_path_or_namespaced_literal()?,
            _ => self.error(|| Expected::Pattern.but_actual_is(self.current_token()))?,
        };

        pattern.attributes.extend(attributes);

        Ok(pattern)
    }

    /// Finish parsing a sequence literal given the already parsed leading path and the span of the already parsed leading `[`.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Sequence-Literal-Expression ::= (Path ".")? "[" Lower-Expression* "]"
    /// Sequence-Literal-Pattern ::= (Path ".")? "[" Lower-Pattern* "]"
    /// ```
    fn finish_parse_sequence_literal<T>(
        &mut self,
        path: Option<Path>,
        mut span: Span,
    ) -> Result<ast::Item<T>>
    where
        T: Parse + From<ast::SequenceLiteral<ast::Item<T>>>,
    {
        let mut elements = Vec::new();

        while self.current_token().name() != ClosingSquareBracket {
            elements.push(T::parse_lower(self)?);
        }

        span.merging(self.current_token());
        self.advance();

        Ok(ast::Item::new(
            default(),
            span.merge_into(&path),
            ast::SequenceLiteral {
                path,
                elements: Spanned::new(span, elements),
            }
            .into(),
        ))
    }

    /// Parse an application or lower.
    // @Task rewrite this with a `delimiter: Delimiter`-parameter for better error messages!
    fn parse_application_or_lower<T>(&mut self) -> Result<ast::Item<T>>
    where
        T: Parse + From<ast::Application<ast::Item<T>>> + 'static,
    {
        let mut callee = T::parse_lower(self)?;
        struct Argument<T> {
            explicitness: Explicitness,
            binder: Option<Identifier>,
            value: T,
        }

        let mut illegal_pi = None;

        while let Ok(argument) = self
            .parse_or_backtrack(|this| {
                // @Beacon @Question can pi_type_literal_was_used_as_lower_expression also happen here???
                let explicitness = this.parse_optional_implicitness();
                let value = T::parse_lower(this)?;

                Ok(Spanned::new(
                    value.span(),
                    Argument {
                        binder: None,
                        explicitness: explicitness.into(),
                        value,
                    },
                ))
            })
            .or_else(|_| -> Result<_> {
                self.parse_or_backtrack(|this| {
                    let explicitness = this.parse_optional_implicitness();
                    let mut span = this.consume(OpeningRoundBracket)?.span;
                    span.merging_from(explicitness);

                    let binder = this.consume_word()?;

                    if TypeId::of::<T>() == TypeId::of::<ast::ExpressionKind>()
                        && this.current_token().name() == Colon
                    {
                        illegal_pi = Some(this.current_token().clone());
                        this.advance();
                    } else {
                        this.consume(Equals)?;
                    }

                    let argument = T::parse(this)?;

                    span.merging(this.consume(ClosingRoundBracket)?);

                    Ok(Spanned::new(
                        span,
                        Argument {
                            explicitness: explicitness.into(),
                            binder: Some(binder),
                            value: argument,
                        },
                    ))
                })
            })
        {
            if let Some(token) = &illegal_pi {
                let explicitness = match argument.bare.explicitness {
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

            callee = ast::Item::new(
                default(),
                span,
                ast::Application {
                    callee,
                    explicitness: argument.bare.explicitness,
                    binder: argument.bare.binder,
                    argument: argument.bare.value,
                }
                .into(),
            );
        }

        Ok(callee)
    }

    /// Parse a path or a number, text or sequence literal prefixed with a path.
    ///
    /// ## Grammar
    ///
    /// ```ebnf
    /// Path-Or-Namespaced-Literal-Expression ::=
    ///     Path
    ///     ("." (#Number-Literal | #Text-Literal | Sequence-Literal-Expression))?
    /// Path-Or-Namespaced-Literal-Pattern ::=
    ///     Path
    ///     ("." (#Number-Literal | #Text-Literal | Sequence-Literal-Pattern))?
    /// ```
    fn parse_path_or_namespaced_literal<T>(&mut self) -> Result<ast::Item<T>>
    where
        T: Parse
            + From<ast::NumberLiteral>
            + From<ast::TextLiteral>
            + From<ast::SequenceLiteral<ast::Item<T>>>
            + From<ast::Path>,
    {
        let mut path = self.parse_first_path_segment()?;

        while self.has_consumed(Dot) {
            match self.current_token().name() {
                Word | Punctuation => {
                    let identifier = self.current_token_into_identifier();
                    self.advance();
                    path.segments.push(identifier);
                }
                NumberLiteral => {
                    let token = self.current_token().clone();
                    self.advance();

                    return Ok(ast::Item::new(
                        default(),
                        path.span().merge(&token),
                        ast::NumberLiteral {
                            path: Some(path),
                            literal: Spanned::new(token.span, token.into_number_literal().unwrap()),
                        }
                        .into(),
                    ));
                }
                TextLiteral => {
                    let token = self.current_token().clone();
                    self.advance();

                    return Ok(ast::Item::new(
                        default(),
                        path.span().merge(&token),
                        ast::TextLiteral {
                            path: Some(path),
                            literal: Spanned::new(
                                token.span,
                                token
                                    .into_text_literal()
                                    .unwrap()
                                    .or_else(|error| self.error(|| error))?,
                            ),
                        }
                        .into(),
                    ));
                }
                OpeningSquareBracket => {
                    let span = self.current_token().span();
                    self.advance();
                    return self.finish_parse_sequence_literal(Some(path), span);
                }
                _ => {
                    return self.error(|| {
                        expected_one_of![
                            Expected::Identifier,
                            NumberLiteral,
                            TextLiteral,
                            OpeningSquareBracket
                        ]
                        .but_actual_is(self.current_token())
                    });
                }
            }
        }

        Ok(ast::Item::new(default(), path.span(), path.into()))
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
        // by the backtracking logic (see the branch do-not-reflect)
        self.consume_after_expecting(Colon, other_expected)?;
        self.parse_or_backtrack(Self::parse_expression)
    }

    /// Parse an optional type annotation.
    fn parse_optional_type_annotation(&mut self) -> Result<Option<Expression>> {
        self.has_consumed(Colon)
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
    /// ; #Start-Of-Input is not actually emitted by the lexer, the parser needs to bound-check instead.
    /// Terminator ::= ";" | (> "}" | #End-Of-Input) | (< #Start-Of-Input | ";" | "}")
    /// ```
    fn expect_terminator(&mut self) -> Result<Option<Token>> {
        let token = self.current_token();
        if token.name().is_terminator()
            || self
                .preceeding_token()
                .map_or(true, |token| token.name().is_terminator())
        {
            Ok(if token.name() == Semicolon {
                let token = token.clone();
                self.advance();

                Some(token)
            } else {
                None
            })
        } else {
            Err(Expected::Delimiter(Delimiter::Terminator)
                .but_actual_is(self.current_token())
                .report(self.session.reporter()))
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
            Apostrophe => {
                let span = self.current_token().span;
                self.advance();
                SpannedExplicitness::Implicit { marker: span }
            }
            _ => SpannedExplicitness::Explicit,
        }
    }
}

trait Parse: Sized {
    fn parse(parser: &mut Parser<'_>) -> Result<ast::Item<Self>>;
    fn parse_lower(parser: &mut Parser<'_>) -> Result<ast::Item<Self>>;
}

impl Parse for ast::ExpressionKind {
    fn parse(parser: &mut Parser<'_>) -> Result<Expression> {
        parser.parse_expression()
    }
    fn parse_lower(parser: &mut Parser<'_>) -> Result<Expression> {
        parser.parse_lower_expression()
    }
}

impl Parse for ast::PatternKind {
    fn parse(parser: &mut Parser<'_>) -> Result<Pattern> {
        parser.parse_pattern()
    }
    fn parse_lower(parser: &mut Parser<'_>) -> Result<Pattern> {
        parser.parse_lower_pattern()
    }
}

enum Expected {
    Token(TokenName),
    Identifier,
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
            .message(format!("found {actual} but expected {self}"))
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
            Identifier => write!(f, "identifier"),
            Path => write!(f, "path"),
            Declaration => write!(f, "declaration"),
            Expression => write!(f, "expression"),
            Pattern => write!(f, "pattern"),
            Parameter => write!(f, "parameter"),
            AttributeArgument => write!(f, "attribute argument"),
            Delimiter(delimiter) => write!(f, "{}", delimiter),
            OneOf(expected) => write!(f, "{}", expected.iter().list(Conjunction::Or)),
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
