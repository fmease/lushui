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

// @Task parse sequence literals Sequence-Literal ::= "[" Expression* "]" ("_" #Identifier)?
// (but no space between "]" and "_"!!)
// @Task allow underscores in places where binders are allowed
// @Task parse temporary
// Lazy-Expression ::= "lazy" Expression

pub mod ast;

use crate::{
    diagnostic::{Code, Diagnostic, Diagnostics, Result},
    lexer::{self, Token, TokenKind},
    smallvec,
    span::{SourceFile, Span, Spanned, Spanning},
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
        let token = self.current();
        if token.kind == expected {
            Ok(token)
        } else {
            Err(Expected::Token(expected).but_actual_is(token))
        }
    }

    fn consume(&mut self, token_kind: lexer::TokenKind) -> Result<lexer::Token> {
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
    /// ```text
    /// General-Identifier ::= #Identifier | #Punctuation
    /// ```
    fn consume_general_identifier(&mut self) -> Result<Identifier> {
        use TokenKind::*;
        let token = self.current();
        match token.kind {
            Identifier | Punctuation => {
                self.advance();
                Ok(ast::Identifier::from_token(token))
            }
            _ => {
                Err(Expected::OneOf(&[Identifier.into(), Punctuation.into()]).but_actual_is(token))
            }
        }
    }

    /// Indicate whether the given token was consumed.
    ///
    /// This method is conceptually equivalent to [Self::consume] followed by
    /// [Result::is_ok] except it (probably) is more memory-efficient.
    #[must_use]
    fn consumed(&mut self, kind: lexer::TokenKind) -> bool {
        if self.current_is(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    // @Note cloning is not that good, esp. text literals
    // but otherwise, we run into nasty borrowing errors because of
    // compound borrowing (limitation of rustc)
    // @Question create a function that moves out of (using remove)?
    // @Update this is **really** bad: everytime we for example fail with
    // Self::consume, we might have cloned a whole string on the way!!!
    // @Task add pseudo-token like TokenKind::Empty (sham span) to be able
    // to move the token out of tokens using std::mem::replace (but be aware
    // that this is not compatible with Parser::reflect any more!!!!)
    fn current(&self) -> lexer::Token {
        self.tokens[self.index].clone()
    }

    fn current_is_delimiter(&self, delimiters: &[Delimiter]) -> bool {
        delimiters
            .iter()
            .map(|delimiter| <&TokenKind>::from(delimiter))
            .find(|&token| token == &self.tokens[self.index].kind)
            .is_some()
    }

    fn current_is(&self, kind: lexer::TokenKind) -> bool {
        self.tokens[self.index].kind == kind
    }

    fn succeeding_is(&self, kind: lexer::TokenKind) -> bool {
        self.tokens[self.index + 1].kind == kind
    }

    /// Parse a declaration.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Declaration ::= (Attribute | Documentation-Comment)*
    ///     (Value-Declaration | Data-Declaration | Module-Declaration | Crate-Declaration | Use-Declaration)
    /// Crate-Declaration ::= "crate" #Identifier Line-Break
    /// ```
    pub fn parse_declaration(&mut self) -> Result<Declaration> {
        use TokenKind::*;
        let attributes = self.parse_attributes()?;
        let token = self.current();

        let mut declaration = match token.kind {
            Identifier => {
                self.advance();
                self.finish_parse_value_declaration(ast::Identifier::from_token(token))
            }
            Data => {
                self.advance();
                self.finish_parse_data_declaration(token.span)
            }
            Module => {
                self.advance();
                self.finish_parse_module_declaration(token.span)
            }
            Crate => {
                self.advance();
                let binder = self.consume_identifier()?;
                self.consume(LineBreak)?;

                Ok(decl! {
                    Crate[token.span.merge(&binder.span)] {
                        binder,
                    }
                })
            }
            Use => {
                self.advance();
                self.finish_parse_use_declaration(token.span)
            }
            // @Beacon @Task attribute group
            Indentation => {
                self.advance();
                Err(Diagnostic::bug()
                    .with_message("attribute groups not support yet")
                    .with_span(&token))
                // self.consume(Dedentation)?;
            }
            _ => Err(Expected::Declaration.but_actual_is(token)),
        }?;

        declaration.attributes = attributes;

        Ok(declaration)
    }

    /// Parse attributes.
    fn parse_attributes(&mut self) -> Result<Attributes> {
        let mut attributes = Attributes::default();

        loop {
            let token = self.current();
            attributes.push(match token.kind {
                TokenKind::At => {
                    self.advance();
                    self.finish_parse_attribute(token.span)?
                }
                // currently, documentation comments don't need line breaks in between them
                // in the eyes of the parser as the lexer already handles this
                // in the future I am certain that other attributes are not going to need
                // line breaks either once we switch to the proposed new attribute syntax
                // (the ones with the mandatory parenthesis for attributes with arguments)
                TokenKind::DocumentationComment => {
                    self.advance();
                    let _ = self.consumed(TokenKind::LineBreak);
                    Attribute {
                        kind: AttributeKind::Documentation,
                        span: token.span,
                    }
                }
                _ => break,
            });
        }

        Ok(attributes)
    }

    /// Finish parsing attribute.
    ///
    /// The span does not include the trailing line break.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Attribute ::= "@" @Task
    /// ```
    fn finish_parse_attribute(&mut self, keyword_span: Span) -> Result<Attribute> {
        use AttributeKind::*;

        let mut span = keyword_span;

        let identifier = self.consume_identifier()?;
        span.merging(&identifier);
        // @Note most of these are just stubs. some of them should take arguments
        let kind = match identifier.as_str() {
            "foreign" => Foreign,
            "inherent" => Inherent,
            "moving" => Moving,
            "if" => If,
            "deprecated" => Deprecated,
            "unstable" => Unstable,
            "allow" => Allow,
            "warn" => Warn,
            "deny" => Deny,
            "forbid" => Forbid,
            "unsafe" => Unsafe,
            "shallow" => Shallow,
            _ => {
                return Err(Diagnostic::error()
                    .with_code(Code::E011)
                    .with_message(format!("`{}` is not an existing attribute", identifier))
                    .with_span(&identifier))
            }
        };
        self.consume(TokenKind::LineBreak)?;

        Ok(Attribute { span, kind })
    }

    /// Finish parsing a value declaration.
    ///
    /// The leading identifier should have already parsed beforehand.
    /// The span does not include the trailing line break.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Value-Declaration ::= #Identifier Parameters Type-Annotation?
    ///     ("=" Possibly-Indented-Terminated-Expression | Line-Break)
    /// ```
    fn finish_parse_value_declaration(&mut self, binder: Identifier) -> Result<Declaration> {
        use TokenKind::*;
        let mut span = binder.span;

        let parameters = self.parse_parameters(&STANDARD_DECLARATION_DELIMITERS)?;
        span.merging(&parameters);
        let type_annotation = self.parse_optional_type_annotation()?;
        span.merging(&type_annotation);

        let expression = if self.consumed(Equals) {
            let expression = self.parse_possibly_indented_terminated_expression()?;
            span.merging(&expression);
            Some(expression)
        } else {
            self.consume(LineBreak)?;
            None
        };

        Ok(decl! {
            Value[span] {
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
    /// ```text
    /// Data-Declaration ::= "data" #Identifier Parameters Type-Annotation?
    ///     (Line-Break |
    ///     "=" Line-Break Indentation
    ///         (Line-Break | Constructor)*
    ///     Dedentation)
    /// ```
    fn finish_parse_data_declaration(&mut self, keyword_span: Span) -> Result<Declaration> {
        let mut span = keyword_span;

        let binder = self.consume_identifier()?;
        let parameters = self.parse_parameters(&STANDARD_DECLARATION_DELIMITERS)?;
        let type_annotation = self.parse_optional_type_annotation()?;
        span.merging(&type_annotation);

        use TokenKind::*;

        let constructors = match self.consume(TokenKind::Equals).ok() {
            Some(equals) => {
                span.merging(&equals);
                self.consume(LineBreak)?;

                let mut constructors = Vec::new();

                self.parse_indented(|parser| {
                    constructors.push(parser.parse_constructor()?);
                    Ok(())
                })?;

                span.merging(&constructors.last());

                Some(constructors)
            }
            None => {
                self.consume(LineBreak)?;
                None
            }
        };

        Ok(decl! {
            Data[keyword_span.merge(&span)] {
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
    /// ```text
    /// Module-Declaration ::= Header | "module" #Identifier
    ///     (Line-Break |
    ///     ":" Exposure-List Line-Break Indentation
    ///         (Line-Break | Declaration)*
    ///     Dedentation)
    /// Header ::= "module" ":" Exposure-List Line-Break
    /// ```
    fn finish_parse_module_declaration(&mut self, keyword_span: Span) -> Result<Declaration> {
        use TokenKind::*;
        let mut span = keyword_span;

        if self.consumed(Colon) {
            let exposures = self.parse_exposure_list()?;
            self.consume(LineBreak)?;

            return Ok(decl! {
                Header[span] {
                    exposures,
                }
            });
        }

        let binder = self.consume_identifier()?;
        span.merging(&binder);

        if self.consumed(LineBreak) {
            // it is an external module declaration
            return Ok(decl! {
                Module[span] {
                    binder,
                    file: self.file.clone(),
                    exposures: Vec::new(),
                    declarations: None,
                }
            });
        }

        self.consume(Colon)?;
        let exposures = self.parse_exposure_list()?;
        self.consume(LineBreak)?;

        let mut declarations = Vec::new();

        self.parse_indented(|parser| {
            declarations.push(parser.parse_declaration()?);
            Ok(())
        })?;

        // @Bug span is wrong: we need to store the last token's span: dedentation/line break
        Ok(decl! {
            Module[span] {
                binder,
                file: self.file.clone(),
                exposures,
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
    /// ```text
    /// Top-Level ::= (Line-Break | Declaration)* #End-Of-Input
    /// ```
    pub fn parse_top_level(&mut self, binder: Identifier) -> Result<Declaration> {
        use TokenKind::*;

        let mut declarations = Vec::new();

        loop {
            if self.consumed(LineBreak) {
                continue;
            }

            if self.consumed(EndOfInput) {
                break Ok(decl! {
                    Module[self.file.span] {
                        binder,
                        file: self.file.clone(),
                        exposures: Vec::new(),
                        declarations: Some(declarations)
                    }
                });
            }

            declarations.push(self.parse_declaration()?);
        }
    }

    /// Parse an exposure list consuming the delimiter `=`.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Exposure-List ::= General-Identifier* "="
    /// ```
    // @Task parse complex and multipaths for constructor and field exposures
    fn parse_exposure_list(&mut self) -> Result<Vec<Identifier>> {
        let mut exposures = Vec::new();

        while !self.consumed(TokenKind::Equals) {
            exposures.push(self.consume_general_identifier()?);
        }

        Ok(exposures)
    }

    // @Note this is fragile and ugly as heck
    fn parse_indented(&mut self, mut subparser: impl FnMut(&mut Self) -> Result<()>) -> Result<()> {
        use TokenKind::*;

        while self.consumed(Indentation) {
            while !self.current_is(Dedentation) {
                if self.consumed(LineBreak) {
                    continue;
                }

                subparser(self)?;
            }

            self.consume(Dedentation)?;

            while self.consumed(LineBreak) {}
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
    /// ```text
    /// Use-Declaration ::= "use" Use-Bindings Line-Break
    /// ```
    fn finish_parse_use_declaration(&mut self, keyword_span: Span) -> Result<Declaration> {
        let bindings = self.parse_use_bindings(&[TokenKind::LineBreak.into()])?;
        self.consume(TokenKind::LineBreak)?;

        // @Task span info
        Ok(decl! {
            Use[keyword_span.merge(&bindings)] {
                bindings,
            }
        })
    }

    /// Parse use bindings.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Use-Bindings ::= @Task
    /// ```
    fn parse_use_bindings(&mut self, delimiters: &[Delimiter]) -> Result<UseBindings> {
        use TokenKind::*;
        let token = self.current();

        match token.kind {
            Identifier | Punctuation => {
                self.advance();
                self.finish_parse_use_bindings(Path::new_identifier(token), delimiters)
            }
            Crate => {
                self.advance();
                self.finish_parse_use_bindings(Path::new_crate(token), delimiters)
            }
            Super => {
                self.advance();
                self.finish_parse_use_bindings(Path::new_super(token), delimiters)
            }
            Self_ => {
                self.advance();
                self.finish_parse_use_bindings(Path::new_self(token), delimiters)
            }
            _ => return Err(Expected::Path.but_actual_is(token)),
        }
    }

    fn finish_parse_use_bindings(
        &mut self,
        mut path: Path,
        delimiters: &[Delimiter],
    ) -> Result<UseBindings> {
        use TokenKind::*;

        while self.consumed(Dot) {
            let token = self.current();
            match token.kind {
                Identifier | Punctuation => {
                    self.advance();
                    path.segments
                        .push(ast::Identifier::from_token(token).into());
                }
                OpeningRoundBracket => {
                    self.advance();

                    let mut bindings = Vec::new();

                    while !self.consumed(ClosingRoundBracket) {
                        if self.consumed(OpeningRoundBracket) {
                            let target = self.parse_simple_path()?;
                            self.consume(As)?;
                            let binder = self.consume_general_identifier()?;
                            self.consume(ClosingRoundBracket)?;

                            bindings.push(UseBindings::Single {
                                target,
                                binder: Some(binder),
                            });
                        } else {
                            // @Note @Bug this is really really fragile=non-extensible!
                            bindings.push(self.parse_use_bindings(&[
                                OpeningRoundBracket.into(),
                                ClosingRoundBracket.into(),
                                Identifier.into(),
                                Punctuation.into(),
                                Self_.into(),
                            ])?);
                        }
                    }

                    return Ok(UseBindings::Multiple { path, bindings });
                }
                _ => {
                    return Err(
                        Expected::OneOf(&[Identifier.into(), OpeningRoundBracket.into()])
                            .but_actual_is(token),
                    )
                }
            }
        }

        // @Question is there a grammar transformation to a self-contained construct
        // instead of a delimited one?
        let binder = if self.current_is_delimiter(delimiters) {
            None
        } else if self.consumed(As) {
            Some(self.consume_general_identifier()?)
        } else {
            return Err(
                Expected::OneOf(&delimiters_with_expected(delimiters, Some(As.into())))
                    .but_actual_is(self.current()),
            );
        };

        // @Task correct span info
        Ok(UseBindings::Single {
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
    /// ```text
    /// Constructor ::= (Attribute | Documentation-Comment)*
    ///     #Identifier Parameters Type-Annotation? Line-Break
    /// ```
    fn parse_constructor(&mut self) -> Result<Declaration> {
        let attributes = self.parse_attributes()?;

        let span = self
            .consume(TokenKind::Record)
            .ok()
            .map(|record| record.span);
        let record = span.is_some();

        let binder = self.consume_identifier()?;
        let mut span = binder.span.merge_into(&span);

        let parameters =
            self.parse_parameters(&[Delimiter::TypeAnnotationPrefix, TokenKind::LineBreak.into()])?;
        span.merging(&parameters);

        let type_annotation = self.parse_optional_type_annotation()?;
        span.merging(&type_annotation);

        self.consume(TokenKind::LineBreak)?;

        let mut constructor = decl! {
            Constructor[span] {
                binder,
                parameters,
                type_annotation,
                record,
            }
        };
        constructor.attributes = attributes;
        Ok(constructor)
    }

    /// Parse an expression.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Expression ::= Let-In | Use-In | Lambda-Literal | Case-Analysis | Pi-Literal-Or-Lower
    /// ```
    // @Task parse sigma literals
    fn parse_expression(&mut self) -> Result<Expression> {
        use TokenKind::*;
        let token = self.current();

        match token.kind {
            Let => {
                self.advance();
                self.finish_parse_let_in(token.span)
            }
            Use => {
                self.advance();
                self.finish_parse_use_in(token.span)
            }
            Backslash => {
                self.advance();
                self.finish_parse_lambda_literal(token.span)
            }
            Case => {
                self.advance();
                self.finish_parse_case_analysis(token.span)
            }
            Do => {
                self.advance();
                self.finish_parse_do_block(token.span)
            }
            _ => self.parse_pi_type_literal_or_lower(),
        }
    }

    /// Parse a pi-type literal or a lower expression.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Pi-Literal-Or-Lower !right ::= (
    ///     "(" Explicitness #Identifier Type-Annotation ")" |
    ///     Application-Or-Lower)
    ///         ("->" Pi-Literal-Or-Lower)*
    /// ```
    fn parse_pi_type_literal_or_lower(&mut self) -> Result<Expression> {
        use TokenKind::*;
        let mut span = Span::SHAM;

        let (explicitness, binder, parameter) = self
            .reflect(|parser| {
                span = parser.consume(TokenKind::OpeningRoundBracket)?.span;

                let explicitness = parser.consume_explicitness_symbol();
                let binder = parser.consume_identifier()?;
                let parameter = parser.parse_type_annotation()?;

                parser.consume(ClosingRoundBracket)?;

                Ok((explicitness, Some(binder), parameter))
            })
            .or_else(|_| -> Result<_> {
                let parameter = self.parse_application_or_lower()?;
                span = parameter.span;
                Ok((Explicit, None, parameter))
            })?;

        Ok(match self.consume(ThinArrowRight) {
            Ok(_) => {
                let expression = self.parse_pi_type_literal_or_lower()?;
                span.merging(&expression);

                expr! {
                    PiTypeLiteral[span] {
                        expression,
                        binder,
                        parameter,
                        explicitness,
                    }
                }
            }
            Err(_) if binder.is_none() => parameter,
            Err(error) => return Err(error),
        })
    }

    /// Parse an application or a lower expression.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Application-Or-Lower !left ::=
    ///     Lower-Expression
    ///         (Lower-Expression | "(" Explicitness (#Identifier "=")? Expression ")")*
    /// ```
    fn parse_application_or_lower(&mut self) -> Result<Expression> {
        use TokenKind::*;
        let mut expression = self.reflect(Self::parse_lower_expression)?;
        while let Ok((argument, explicitness, binder)) = self
            .reflect(|parser| Ok((parser.parse_lower_expression()?, Explicit, None)))
            .or_else(|_| -> Result<_> {
                self.consume(OpeningRoundBracket)?;
                let explicitness = self.consume_explicitness_symbol();
                let binder = (self.current_is(TokenKind::Identifier)
                    && self.succeeding_is(TokenKind::Equals))
                .then(|| {
                    let token = self.current();
                    self.advance();
                    self.advance();
                    ast::Identifier::from_token(token)
                });
                let expression = self.parse_expression()?;
                self.consume(ClosingRoundBracket)?;
                Ok((expression, explicitness, binder))
            })
        {
            expression = expr! {
                Application[expression.span.merge(&argument)] {
                    callee: expression,
                    argument,
                    explicitness,
                    binder,
                }
            };
        }
        Ok(expression)
    }

    /// Parse a lower expression.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Lower-Expression ::= Simple-Path | "Type" | #Number-Literal | #Text-Literal | "(" Expression ")"
    /// Simple-Path ::= ("crate" | "super" | General-Identifier)? ("." General-Identifier)*
    /// ```
    fn parse_lower_expression(&mut self) -> Result<Expression> {
        use TokenKind::*;

        let token = self.current();
        Ok(match token.kind {
            Identifier | Punctuation => {
                self.advance();
                self.finish_parse_simple_path(Path::new_identifier(token))?
                    .into()
            }
            Crate => {
                self.advance();
                self.finish_parse_simple_path(Path::new_crate(token))?
                    .into()
            }
            Super => {
                self.advance();
                self.finish_parse_simple_path(Path::new_super(token))?
                    .into()
            }
            Self_ => {
                self.advance();
                self.finish_parse_simple_path(Path::new_self(token))?.into()
            }
            Type => {
                self.advance();
                expr! { TypeLiteral[token.span] }
            }
            NumberLiteral => {
                self.advance();
                expr! { NumberLiteral[token.span](token.number_literal()) }
            }
            TextLiteral => {
                self.advance();

                if !token.text_literal_is_terminated() {
                    return Err(text_literal_is_not_terminated(&token));
                }

                expr! { TextLiteral[token.span](token.text_literal()) }
            }
            QuestionMark => {
                self.advance();
                let tag = self.consume_identifier()?;
                expr! { TypedHole[token.span.merge(&tag)] { tag } }
            }
            // @Task use advance_with//finish
            OpeningRoundBracket => return self.parse_bracketed(Self::parse_expression),
            _ => return Err(Expected::Expression.but_actual_is(token)),
        })
    }

    fn parse_simple_path(&mut self) -> Result<Path> {
        use TokenKind::*;
        let token = self.current();
        match token.kind {
            Identifier | Punctuation => {
                self.advance();
                self.finish_parse_simple_path(Path::new_identifier(token))
            }
            Crate => {
                self.advance();
                self.finish_parse_simple_path(Path::new_crate(token))
            }
            Super => {
                self.advance();
                self.finish_parse_simple_path(Path::new_super(token))
            }
            Self_ => {
                self.advance();
                self.finish_parse_simple_path(Path::new_self(token))
            }
            _ => return Err(Expected::Path.but_actual_is(token)),
        }
    }

    fn finish_parse_simple_path(&mut self, mut path: Path) -> Result<Path> {
        while self.consumed(TokenKind::Dot) {
            path.segments
                .push(self.consume_general_identifier()?.into());
        }

        Ok(path)
    }

    /// Finish parsing a lambda literal expression.
    ///
    /// The initial `\` should have already been parsed beforehand.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Lambda-Literal ::= "\" Parameters Type-Annotation? "=>" Expression
    /// ```
    fn finish_parse_lambda_literal(&mut self, keyword_span: Span) -> Result<Expression> {
        let mut span = keyword_span;
        let parameters =
            self.parse_parameters(&[Delimiter::TypeAnnotationPrefix, TokenKind::WideArrow.into()])?;
        let body_type_annotation = self.parse_optional_type_annotation()?;
        self.consume(TokenKind::WideArrow)?;
        let body = self.parse_expression()?;
        span.merging(&body);

        Ok(expr! {
            LambdaLiteral[span] {
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
    /// ```text
    /// Let-In ::=
    ///     "let" #Identifier Parameters Type_Annotation? "="
    ///     Possibly-Breakably-Indented-Expression "in" Line-Break? Expression
    /// ```
    fn finish_parse_let_in(&mut self, span_of_let: Span) -> Result<Expression> {
        let mut span = span_of_let;
        let binder = self.consume_identifier()?;
        let parameters =
            self.parse_parameters(&[Delimiter::TypeAnnotationPrefix, Delimiter::DefinitionPrefix])?;
        let type_annotation = self.parse_optional_type_annotation()?;
        self.consume(TokenKind::Equals)?;
        let expression = self.parse_possibly_breakably_indented_expression()?;
        self.consume(TokenKind::In)?;
        let _ = self.consumed(TokenKind::LineBreak);
        let scope = self.parse_expression()?;
        span.merging(&scope);

        Ok(expr! {
            LetIn[span] {
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
    /// ```text
    /// Use-In ::= "use" Use-Bindings "in" Line-Break? Expression
    /// ```
    fn finish_parse_use_in(&mut self, span_of_use: Span) -> Result<Expression> {
        let bindings = self.parse_use_bindings(&[TokenKind::In.into()])?;
        self.consume(TokenKind::In)?;
        let _ = self.consumed(TokenKind::LineBreak);
        let scope = self.parse_expression()?;

        Ok(expr! {
            UseIn[span_of_use.merge(&scope)] {
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
    /// ```text
    /// Case-Analysis ::= "case" Possibly-Breakably-Indented-Expression "of"
    ///     (Line-Break (Indentation Case* Dedentation)?)?
    /// Case ::= Pattern "=>" Expression
    /// ```
    fn finish_parse_case_analysis(&mut self, span_of_case: Span) -> Result<Expression> {
        use TokenKind::*;
        let mut span = span_of_case;

        let expression = self.parse_possibly_breakably_indented_expression()?;
        span.merging(&self.consume(Of)?);

        let mut cases = Vec::new();

        if self.current_is(LineBreak) && self.succeeding_is(Indentation) {
            self.advance();
            self.advance();

            while !self.current_is(Dedentation) {
                let pattern = self.parse_pattern()?;
                self.consume(WideArrow)?;
                let expression = self.parse_possibly_indented_terminated_expression()?;

                cases.push(ast::Case {
                    pattern,
                    expression,
                });
            }

            span.merging(&self.current());
            self.advance();
        }

        Ok(expr! {
            CaseAnalysis[span] {
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
    /// ```text
    /// Do-Block ::= "do" Line-Break Indentation Statement+ Dedentation
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

        self.consume(LineBreak)?;
        self.consume(Indentation)?;

        while !self.current_is(Dedentation) {
            let token = self.current();

            statements.push(match token.kind {
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
                    let bindings = self.parse_use_bindings(&[TokenKind::LineBreak.into()])?;
                    self.consume(LineBreak)?;
                    Statement::Use(ast::Use { bindings })
                }
                _ => {
                    if self.current_is(Identifier)
                        && (self.succeeding_is(Colon) || self.succeeding_is(ThinArrowLeft))
                    {
                        self.advance();
                        let binder = ast::Identifier::from_token(token);
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

        span.merging(&self.current());
        self.advance();

        Ok(expr! {
            DoBlock[span] {
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
    /// ```text
    /// Parameters ::= Parameter-Group*
    /// ```
    fn parse_parameters(&mut self, delimiters: &[Delimiter]) -> Result<Parameters> {
        let mut parameters = Vec::new();

        while !self.current_is_delimiter(delimiters) {
            parameters.push(self.parse_parameter_group(delimiters)?);
        }

        let span = parameters.get(0).map(|parameter| {
            let mut span = parameter.span;
            span.merging(&parameters.last());
            span
        });

        Ok(Parameters { span, parameters })
    }

    /// Parse a parameter group.
    ///
    /// Delimiters are taken as a parameter merely for constructing
    /// the Diagnostic.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Parameter-Group ::= #Identifier | "(" Explicitness #Identifier+ Type-Annotation? ")"
    /// ```
    fn parse_parameter_group(&mut self, delimiters: &[Delimiter]) -> Result<ParameterGroup> {
        use TokenKind::*;
        let token = self.current();
        match token.kind {
            Identifier => {
                self.advance();
                Ok(ParameterGroup {
                    span: token.span,
                    parameters: smallvec![ast::Identifier::from_token(token)],
                    type_annotation: None,
                    explicitness: Explicit,
                })
            }
            OpeningRoundBracket => {
                self.advance();
                let mut span = token.span;
                let explicitness = self.consume_explicitness_symbol();
                let mut parameters = SmallVec::new();

                parameters.push(self.consume_identifier()?);

                let delimiters = [Delimiter::TypeAnnotationPrefix, ClosingRoundBracket.into()];

                while !self.current_is_delimiter(&delimiters) {
                    parameters.push(self.consume_identifier()?);
                }

                let type_annotation = self.parse_optional_type_annotation()?;

                span.merging(&self.consume(ClosingRoundBracket)?);

                Ok(ParameterGroup {
                    parameters,
                    type_annotation,
                    explicitness,
                    span,
                })
            }

            _ => {
                return Err(Expected::OneOf(&delimiters_with_expected(
                    delimiters,
                    Some(Expected::Parameter),
                ))
                .but_actual_is(token));
            }
        }
    }

    /// Parse a pattern.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Pattern !left ::= Lower-Pattern | Deapplication
    /// Deapplication ::= Lower-Pattern+
    /// ```
    // @Task implicit application e.g. `foo (,bar)`
    fn parse_pattern(&mut self) -> Result<Pattern> {
        let mut callee = self.reflect(Self::parse_lower_pattern)?;

        // @Note probably produces bad error messages, replace with delimiter-logic
        while let Ok(argument) = self.reflect(Self::parse_lower_pattern) {
            callee = pat! {
                Deapplication[callee.span.merge(&argument)] {
                    callee,
                    argument,
                }
            };
        }
        Ok(callee)
    }

    /// Parse a lower pattern.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Lower-Pattern ::= #Number-Literal | #Text-Literal | Binder | Simple-Path | "(" Pattern ")"
    /// Binder ::= "\" #Identifier
    /// Simple-Path ::= ("crate" | "super" | #Identifier)? ("." #Identifier)*
    /// ```
    fn parse_lower_pattern(&mut self) -> Result<Pattern> {
        let token = self.current();

        use TokenKind::*;

        Ok(match token.kind {
            NumberLiteral => {
                self.advance();
                pat! { NumberLiteral[token.span](token.number_literal()) }
            }
            TextLiteral => {
                self.advance();

                if !token.text_literal_is_terminated() {
                    return Err(text_literal_is_not_terminated(&token));
                }

                pat! { TextLiteral[token.span](token.text_literal()) }
            }
            Backslash => {
                self.advance();
                let binder = self.consume_identifier()?;
                pat! { Binder[token.span.merge(&binder)] { binder } }
            }
            Identifier | Punctuation => {
                self.advance();
                self.finish_parse_simple_path(Path::new_identifier(token))?
                    .into()
            }
            Crate => {
                self.advance();
                self.finish_parse_simple_path(Path::new_crate(token))?
                    .into()
            }
            Super => {
                self.advance();
                self.finish_parse_simple_path(Path::new_super(token))?
                    .into()
            }
            Self_ => {
                self.advance();
                self.finish_parse_simple_path(Path::new_self(token))?.into()
            }
            // @Task @Beacon use a "finish"-parser
            OpeningRoundBracket => return self.parse_bracketed(Self::parse_pattern),
            _ => return Err(Expected::Pattern.but_actual_is(token)),
        })
    }

    /// Parse a type annotation.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Type-Annotation ::= ":" Expression
    /// ```
    fn parse_type_annotation(&mut self) -> Result<Expression> {
        self.consume(TokenKind::Colon)?;
        self.reflect(Self::parse_expression)
    }

    /// Parse an optional type annotation.
    fn parse_optional_type_annotation(&mut self) -> Result<Option<Expression>> {
        self.consumed(TokenKind::Colon)
            .then(|| self.parse_expression())
            .transpose()
    }

    /// Parse a possibly indented expression terminated by a line break.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Possibly-Indented-Terminated-Expression ::=
    ///     | Line-Break Indentation Expression Line-Break? Dedentation
    ///     | Terminated-Expression
    /// ```
    fn parse_possibly_indented_terminated_expression(&mut self) -> Result<Expression> {
        use TokenKind::*;

        if self.consumed(LineBreak) {
            self.consume(Indentation)?;
            let expression = self.parse_expression()?;
            let _ = self.consumed(LineBreak);
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
    /// ```text
    /// Possibly-Breakably-Indented-Expression ::=
    ///     | Line-Break Indentation Expression (Line-Break Dedentation)?
    ///     | Expression
    /// ```
    // @Bug even though this function works locally, it does not to with more context.
    // the indentation breaking logic does not work because our parser then inserts a
    // Dedentation token somewhere later down the line (e.g. after the In)
    fn parse_possibly_breakably_indented_expression(&mut self) -> Result<Expression> {
        use TokenKind::*;

        if self.consumed(LineBreak) {
            self.consume(Indentation)?;
            let expression = self.parse_expression()?;
            if self.consumed(LineBreak) {
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
    /// ```text
    /// Terminated-Expression ::= Expression (<!Dedentation Line-Break)?
    /// ```
    fn parse_terminated_expression(&mut self) -> Result<Expression> {
        let expression = self.parse_expression()?;
        // @Note special-casing is also called programming hackily
        if !self.current_is(TokenKind::Dedentation) {
            self.consume(TokenKind::LineBreak)?;
        }
        Ok(expression)
    }

    fn parse_bracketed<K>(
        &mut self,
        subparser: fn(&mut Self) -> Result<Spanned<K>>,
    ) -> Result<Spanned<K>> {
        let mut span = self.consume(TokenKind::OpeningRoundBracket)?.span;
        // @Question reflect necessary?
        let mut inner = self.reflect(subparser)?;
        span.merging(&self.consume(TokenKind::ClosingRoundBracket)?);
        inner.span = span;
        Ok(inner)
    }

    /// Consume the explicitness symbol.
    ///
    /// ## Grammar
    ///
    /// ```text
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

enum Expected<'a> {
    Token(TokenKind),
    Path,
    Declaration,
    Expression,
    Pattern,
    Parameter,
    Delimiter(Delimiter),
    OneOf(&'a [Self]),
}

fn delimiters_with_expected<'a>(
    delimiters: &[Delimiter],
    expected: impl IntoIterator<Item = Expected<'a>>,
) -> Vec<Expected<'a>> {
    let delimiters = delimiters.iter().copied().map(Expected::Delimiter);
    expected.into_iter().chain(delimiters).collect()
}

impl<'a> Expected<'a> {
    fn but_actual_is(self, actual: Token) -> Diagnostic {
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
            .with_span(&actual)
    }
}

impl From<TokenKind> for Expected<'_> {
    fn from(token: TokenKind) -> Self {
        Self::Token(token)
    }
}

use std::fmt;

impl fmt::Display for Expected<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use joinery::JoinableIterator;
        use Expected::*;

        match self {
            Token(token) => write!(f, "{}", token),
            Path => write!(f, "path"),
            Declaration => write!(f, "declaration"),
            Expression => write!(f, "expression"),
            Pattern => write!(f, "pattern"),
            Parameter => write!(f, "parameter"),
            Delimiter(delimiter) => write!(f, "{}", delimiter),
            OneOf(expected) => {
                debug_assert!(!expected.is_empty());

                let (first, last) = expected.split_at(expected.len() - 1);

                if first.is_empty() {
                    write!(f, "{}", last[0])
                } else {
                    write!(f, "{} or {}", first.iter().join_with(", "), last[0])
                }
            }
        }
    }
}

// @Question merge with ExpectedOccurence?
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
            Self::DefinitionPrefix => write!(f, "definition"),
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
