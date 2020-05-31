//! The parser.
//!
//! I *think* it can be classified as a top-down recursive-descent parser with arbitrary look-ahead.
//!
//! ## Issues
//!
//! * crude error locations
//! * cannot really handle optional indentation

// @Beacon @Task make some errors non-fatal (e.g. unknown attributes)

mod ast;

use crate::{
    diagnostic::{Code, Diagnostic, Level, Result},
    lexer::{self, Token, TokenKind},
    smallvec,
    span::{SourceFile, Span, Spanned},
    SmallVec,
};
pub use ast::*;
use std::rc::Rc;

const DECLARATION_DELIMITERS: [Delimiter; 3] = [
    Delimiter::TypeAnnotationPrefix,
    Delimiter::DefinitionPrefix,
    Delimiter::Token(TokenKind::LineBreak),
];

pub struct Parser<'a> {
    file: Rc<SourceFile>,
    tokens: &'a [Token],
    index: usize,
}

impl<'a> Parser<'a> {
    pub fn new(file: Rc<SourceFile>, tokens: &'a [Token]) -> Self {
        Self {
            file,
            tokens,
            index: 0,
        }
    }
}

impl Parser<'_> {
    /// Parse in a sandboxed way.
    ///
    /// Used for arbitrary look-ahead. Restores the old cursor on failure.
    // @Bug because of arbitrary look-ahead/this function, a magnitude of Diagnostics are constructed
    // but never emitted/wasted
    fn reflect<Node>(&mut self, parser: impl FnOnce(&mut Self) -> Result<Node>) -> Result<Node> {
        let saved_index = self.index;
        let result = parser(self);

        if result.is_err() {
            self.index = saved_index;
        }

        result
    }

    fn expect(&self, expected: TokenKind) -> Result<Token> {
        let actual = self.token();
        if actual.kind == expected {
            Ok(actual)
        } else {
            Err(unexpected_token(actual, expected))
        }
    }

    fn expect_identifier(&self) -> Result<Identifier> {
        let actual = self.token();
        match actual.kind {
            TokenKind::Identifier(atom) => Ok(Identifier::new(atom, actual.span)),
            _ => Err(unexpected_token(actual, "identifier")),
        }
    }

    fn consume(&mut self, token_kind: lexer::TokenKind) -> Result<lexer::Token> {
        let token = self.expect(token_kind)?;
        self.advance();
        Ok(token)
    }

    fn consume_identifier(&mut self) -> Result<Identifier> {
        let identifier = self.expect_identifier()?;
        self.advance();
        Ok(identifier)
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    // @Note cloning is not that good, esp. text literals
    // but otherwise, we run into nasty borrowing errors because of
    // compound borrowing (limitation of rustc)
    // @Question create a function that moves out of (using remove)?
    fn token(&self) -> lexer::Token {
        self.tokens[self.index].clone()
    }

    fn token_not_delimiter(&self, delimiters: &[Delimiter]) -> bool {
        delimiters
            .iter()
            .map(|delimiter| <&TokenKind>::from(delimiter))
            .find(|&token| token == &self.tokens[self.index].kind)
            .is_none()
    }

    // @Note bad name (inconsistency with Self::token)
    fn current(&self, kind: lexer::TokenKind) -> bool {
        self.tokens[self.index].kind == kind
    }

    fn succeeding(&self, kind: lexer::TokenKind) -> bool {
        self.tokens[self.index + 1].kind == kind
    }

    fn consumed(&mut self, kind: lexer::TokenKind) -> bool {
        if self.current(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Parse a declaration.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Declaration ::= (Attribute | Documentation-Comment)*
    ///     (Value-Declaration | Data-Declaration | Module-Declaration | Use-Declaration)
    /// ```
    pub fn parse_declaration(&mut self) -> Result<Declaration> {
        use TokenKind::*;

        let token = self.token();

        // @Task transform attribute logic into iterative alogorithm just like we do in
        // `parse_constructor`
        match token.kind {
            Identifier(identifier) => {
                self.advance();
                self.finish_parse_value_declaration(self::Identifier::new(identifier, token.span))
            }
            Data => {
                self.advance();
                self.finish_parse_data_declaration(token.span)
            }
            Module => {
                self.advance();
                self.finish_parse_module_declaration(token.span)
            }
            Use => {
                self.advance();
                self.finish_parse_use_declaration(token.span)
            }
            At => {
                self.advance();
                let attribute = self.finish_parse_attribute(token.span)?;
                let mut declaration = self.parse_declaration()?;
                declaration.attributes.push(attribute);
                Ok(declaration)
            }
            DocumentationComment => {
                self.advance();
                let attribute = self.finish_parse_documentation_comment(token.span)?;
                let mut declaration = self.parse_declaration()?;
                declaration.attributes.push(attribute);
                Ok(declaration)
            }
            _ => Err(unexpected_token(token, "declaration")),
        }
    }

    /// Finish parsing attribute.
    ///
    /// The first underscore should have already beeen parsed beforehand.
    /// The span does not include the trailing line break.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Attribute ::= "_" ("foreign" | "inherent") "_"
    /// ```
    // @Task abstract over attributes with 0, 1, … arguments
    fn finish_parse_attribute(&mut self, keyword_span: Span) -> Result<Attribute> {
        let mut span = keyword_span;

        let identifier = self.consume_identifier()?;
        span.merging(&identifier);
        let kind = match identifier.as_str() {
            "foreign" => AttributeKind::Foreign,
            "inherent" => AttributeKind::Inherent,
            _ => {
                return Err(Diagnostic::new(
                    Level::Fatal,
                    Code::E011,
                    format!("attribute `{}` does not exist", identifier),
                )
                .with_span(&identifier))
            }
        };
        self.consume(TokenKind::LineBreak)?;

        Ok(Attribute { span, kind })
    }

    /// Finish parsing documentation comment.
    ///
    /// The comment should have already been parsed beforehand.
    /// The span does not include the trailing line break.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Documentation-Comment ::= %Documentation-Comment% Line-Break
    /// ```
    fn finish_parse_documentation_comment(&mut self, span: Span) -> Result<Attribute> {
        self.consume(TokenKind::LineBreak)?;

        Ok(Attribute {
            kind: AttributeKind::Documentation,
            span,
        })
    }

    /// Finish parsing a value declaration.
    ///
    /// The leading identifier should have already parsed beforehand.
    /// The span does not include the trailing line break.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Value-Declaration ::= %Identifier% Annotated-Parameters Type-Annotation
    ///     ("=" Possibly-Indented-Expression-Followed-By-Line-Break | Line-Break)
    /// ```
    fn finish_parse_value_declaration(&mut self, binder: Identifier) -> Result<Declaration> {
        use TokenKind::*;
        let mut span = binder.span;

        let parameters = self.parse_parameters(&DECLARATION_DELIMITERS)?;
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
    /// @Note It's not entirely clear to me how to translate the indentation/dedentation rules
    /// to my custom EBNF. The rule below might not be 100% correct.
    ///
    /// ```text
    /// Data-Declaration ::= "data" %Identifier% Annotated-Parameters Type-Annotation
    ///     (Line-Break |
    ///     "=" Line-Break Indentation
    ///         (Line-Break | Constructor)*
    ///     Dedentation)
    /// ```
    fn finish_parse_data_declaration(&mut self, keyword_span: Span) -> Result<Declaration> {
        let mut span = keyword_span;

        let binder = self.consume_identifier()?;
        let parameters = self.parse_parameters(&DECLARATION_DELIMITERS)?;
        let type_annotation = self.parse_optional_type_annotation()?;
        span.merging(&type_annotation);

        use TokenKind::*;

        let constructors = match self.consume(TokenKind::Equals).ok() {
            Some(equals) => {
                span.merging(&equals);
                self.consume(LineBreak)?;

                let mut constructors = Vec::new();

                // @Task @Beacon abstract over it: it's also used for modules
                while self.consumed(Indentation) {
                    while !self.current(Dedentation) {
                        if self.consumed(LineBreak) {
                            continue;
                        }

                        constructors.push(self.parse_constructor()?);
                    }

                    self.consume(Dedentation)?;

                    while self.consumed(LineBreak) {}
                }

                span.merging(&constructors.last());

                Some(constructors)
            }
            None => {
                self.consume(LineBreak)?;
                None
            }
        };

        Ok(decl! {
            Data[keyword_span.merge(span)] {
                binder,
                parameters,
                type_annotation,
                constructors,
            }
        })
    }

    /// Finish parsing module declaration.
    ///
    /// This is either a module declaration or a file system module declaration.
    ///
    /// ## Grammar
    ///
    /// @Note It's not entirely clear to me how to translate the indentation/dedentation rules
    /// to my custom EBNF. The rule below might not be 100% correct.
    ///
    /// ```text
    /// Module-Declaration ::= "module" %Identifier%
    ///     (Line-Break |
    ///     ":" "=" Line-Break Indentation
    ///         (Line-Break | Declaration)*
    ///     Dedentation)
    /// ```
    fn finish_parse_module_declaration(&mut self, keyword_span: Span) -> Result<Declaration> {
        use TokenKind::*;
        let mut span = keyword_span;

        let binder = self.consume_identifier()?;
        span.merging(&binder);

        if self.consumed(LineBreak) {
            return Ok(decl! {
                Module[span] {
                    binder,
                    file: self.file.clone(),
                    declarations: None,
                }
            });
        }

        self.consume(Colon)?;
        self.consume(Equals)?;
        self.consume(LineBreak)?;

        let mut declarations = Vec::new();

        // @Task abstract over this, @Note we could maybe use an indentation counter
        // to make this very easy
        while self.consumed(Indentation) {
            while !self.current(Dedentation) {
                if self.consumed(LineBreak) {
                    continue;
                }

                declarations.push(self.parse_declaration()?);
            }

            self.consume(Dedentation)?;

            while self.consumed(LineBreak) {}
        }

        // @Bug span is wrong: we need to store the last token's span: dedentation/line break
        Ok(decl! {
            Module[span] {
                binder,
                file: self.file.clone(),
                declarations: Some(declarations),
            }
        })
    }

    /// Parse the "top level" aka the body of a module file.
    ///
    /// It takes the identifier of the module as an argument.
    /// This does not parse the module file header yet.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Top-Level ::= (Line-Break | Declaration)* %End-Of-Input%
    /// ```
    pub fn parse_top_level(&mut self, binder: Identifier) -> Result<Declaration> {
        let mut declarations = Vec::<Declaration>::new();

        loop {
            if self.consumed(TokenKind::LineBreak) {
                continue;
            }

            if self.consumed(TokenKind::EndOfInput) {
                break Ok(decl! {
                    Module[self.file.span] {
                        binder,
                        file: self.file.clone(),
                        declarations: Some(declarations)
                    }
                });
            }

            declarations.push(self.parse_declaration()?);
        }
    }

    /// Finish parsing use declaration.
    ///
    /// The keyword `use` should have already been parsed.
    /// The span does not contain the trailing line break.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Use-Declaration ::= "use" Path Line-Break
    /// ```
    fn finish_parse_use_declaration(&mut self, keyword_span: Span) -> Result<Declaration> {
        let path = self.parse_path()?;
        self.consume(TokenKind::LineBreak)?;

        Ok(decl! {
            Use[keyword_span.merge(path.span)] {
                path: path.kind,
            }
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
    ///     %Identifier% Annotated-Parameters Type-Annotation Line-Break
    /// ```
    fn parse_constructor(&mut self) -> Result<Declaration> {
        let mut attributes = Attributes::default();

        loop {
            let token = self.token();
            attributes.push(match token.kind {
                TokenKind::Underscore => {
                    self.advance();
                    self.finish_parse_attribute(token.span)?
                }
                TokenKind::DocumentationComment => {
                    self.advance();
                    self.finish_parse_documentation_comment(token.span)?
                }
                _ => break,
            });
        }

        let binder = self.consume_identifier()?;
        let mut span = binder.span;

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
    /// Expression ::= Let-In | Lambda-Literal | Case-Analysis | Pi-Literal-Or-Lower
    /// ```
    fn parse_expression(&mut self) -> Result<Expression> {
        use TokenKind::*;
        let token = self.token();

        match token.kind {
            Let => {
                self.advance();
                self.finish_parse_let_in(token.span)
            }
            Backslash => {
                self.advance();
                self.finish_parse_lambda_literal(token.span)
            }
            Case => {
                self.advance();
                self.finish_parse_case_analysis(token.span)
            }
            _ => self.parse_pi_type_literal_or_lower(),
        }
    }

    /// Parse a pi-type literal or a lower expression.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Pi-Literal-Or-Lower @right@ ::= (
    ///     "(" "|" %Identifier% Type-Annotation ")" |
    ///     Application-Or-Lower)
    ///         ("->" Pi-Literal-Or-Lower)*
    /// ```
    // @Task don't use reflect/or_else
    fn parse_pi_type_literal_or_lower(&mut self) -> Result<Expression> {
        let mut span = Span::SHAM;

        let (explicitness, binder, parameter) = self
            .reflect(|parser| {
                span = parser.consume(TokenKind::OpeningRoundBracket)?.span;

                let explicitness = parser.consume_explicitness_symbol();
                let binder = parser.consume_identifier()?;
                let parameter = parser.parse_type_annotation()?;

                parser.consume(TokenKind::ClosingRoundBracket)?;

                Ok((explicitness, Some(binder), parameter))
            })
            .or_else(|_| {
                // @Question do we need reflect here?
                self.reflect(Self::parse_application_or_lower)
                    .map(|parameter| {
                        span = parameter.span;
                        (Explicit, None, parameter)
                    })
            })?;

        Ok(match self.consume(TokenKind::ThinArrow) {
            Ok(_) => {
                let expression = self.reflect(Self::parse_pi_type_literal_or_lower)?;
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
    /// Application-Or-Lower @left@ ::= Lower-Expression (Lower-Expression | "(" "|" Expression ")")*
    /// ```
    // @Task heavily improve without using or_else/reflect
    fn parse_application_or_lower(&mut self) -> Result<Expression> {
        let mut expression = self.reflect(Self::parse_lower_expression)?;
        while let Ok((argument, explicitness)) = self
            .reflect(|parser| Ok((parser.parse_lower_expression()?, Explicit)))
            .or_else(|_| -> Result<_> {
                self.consume(TokenKind::OpeningRoundBracket)?;
                self.consume(TokenKind::Comma)?;
                let expression = self.parse_expression()?;
                self.consume(TokenKind::ClosingRoundBracket)?;
                Ok((expression, Implicit))
            })
        {
            expression = expr! {
                Application[expression.span.merge(argument.span)] {
                    callee: expression,
                    argument,
                    explicitness,
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
    /// Lower-Expression ::= Path | "Type" | Nat-Literal | Text-Literal | Bracketed-Expression
    /// ```
    fn parse_lower_expression(&mut self) -> Result<Expression> {
        use TokenKind::*;

        let token = self.token();
        Ok(match token.kind {
            Identifier(identifier) => {
                self.advance();
                self.finish_parse_path_with_identifier(self::Identifier::new(
                    identifier, token.span,
                ))?
                .into()
            }
            Crate => {
                self.advance();
                self.finish_parse_path_with_head(PathHead::new(PathHeadKind::Crate, token.span))?
                    .into()
            }
            Super => {
                self.advance();
                self.finish_parse_path_with_head(PathHead::new(PathHeadKind::Super, token.span))?
                    .into()
            }
            Type => {
                self.advance();
                expr! { TypeLiteral[token.span] }
            }
            NatLiteral(value) => {
                self.advance();
                expr! { NatLiteral[token.span] { value } }
            }
            TextLiteral(value) => {
                self.advance();
                expr! { TextLiteral[token.span] { value } }
            }
            // @Task use advance_with//finish
            OpeningRoundBracket => return self.parse_bracketed(Self::parse_expression),
            _ => return Err(unexpected_token(token, "expression")),
        })
    }

    /// Parse a path but return an unboxed expression.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Path ::= ("crate" | "super" | %Identifier%)? ("." %Identifier%)*
    /// ```
    fn parse_path(&mut self) -> Result<Spanned<Path>> {
        use TokenKind::*;
        let token = self.token();

        match token.kind {
            Identifier(identifier) => {
                self.advance();
                self.finish_parse_path_with_identifier(self::Identifier::new(
                    identifier, token.span,
                ))
            }
            Crate => {
                self.advance();
                self.finish_parse_path_with_head(PathHead::new(PathHeadKind::Crate, token.span))
            }
            Super => {
                self.advance();
                self.finish_parse_path_with_head(PathHead::new(PathHeadKind::Super, token.span))
            }
            _ => return Err(unexpected_token(token, "path")),
        }
    }

    fn finish_parse_path_with_head(&mut self, head: PathHead) -> Result<Spanned<Path>> {
        self.parse_path_tail(Spanned {
            span: head.span,
            kind: Path {
                head: Some(head),
                segments: SmallVec::new(),
            },
        })
    }

    fn finish_parse_path_with_identifier(
        &mut self,
        identifier: Identifier,
    ) -> Result<Spanned<Path>> {
        self.parse_path_tail(Spanned {
            span: identifier.span,
            kind: Path {
                head: None,
                segments: smallvec![identifier],
            },
        })
    }

    fn parse_path_tail(&mut self, mut path: Spanned<Path>) -> Result<Spanned<Path>> {
        while self.consumed(TokenKind::Dot) {
            path.kind.segments.push(self.consume_identifier()?);
        }

        path.span.merging(&path.kind.segments.last());

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

    /// Finish parsing an let-in expression.
    ///
    /// The initial `let` should have already been parsed beforehand.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Let-In ::= "'let" %Identifier% Parameters Type_Annotation? "=" Expression "in" Expression
    /// ```
    fn finish_parse_let_in(&mut self, span_of_let: Span) -> Result<Expression> {
        let mut span = span_of_let;
        let binder = self.consume_identifier()?;
        let parameters =
            self.parse_parameters(&[Delimiter::TypeAnnotationPrefix, Delimiter::DefinitionPrefix])?;
        let type_annotation = self.parse_optional_type_annotation()?;
        self.consume(TokenKind::Equals)?;
        let expression = self.parse_expression()?;
        self.consume(TokenKind::In)?;
        // @Bug commented code does not work as expected (says: unexpected dedentation)
        // let scope = self.parse_possibly_indented_expression_followed_by_line_break()?;
        let scope = self.parse_expression()?;
        span.merging(&scope);

        Ok(expr! {
            LetIn[span_of_let.merge(scope.span)] {
                binder,
                parameters,
                type_annotation,
                expression,
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
    /// Case-Analysis ::= "case" Expression "Of" (Line-Break (Indentation Case* Dedentation)?)?
    /// Case ::= Pattern "=>" Expression
    /// ```
    fn finish_parse_case_analysis(&mut self, span_of_case: Span) -> Result<Expression> {
        use TokenKind::*;
        let mut span = span_of_case;

        let expression = self.parse_possibly_indented_expression()?;
        span.merging(&self.consume(TokenKind::Of)?);

        let mut cases = Vec::new();

        if self.current(LineBreak) && self.succeeding(Indentation) {
            self.advance();
            self.advance();

            while !self.current(Dedentation) {
                let pattern = self.parse_pattern()?;
                self.consume(TokenKind::WideArrow)?;
                let expression = self.parse_possibly_indented_terminated_expression()?;

                cases.push(self::Case {
                    pattern,
                    expression,
                });
            }

            span.merging(&self.token());
            self.advance();
        }

        Ok(expr! {
            CaseAnalysis[span] {
                expression,
                cases,
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

        // @Task debug_assert delimiter invariants

        while self.token_not_delimiter(delimiters) {
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
    /// Parameter-Group ::= %Identifier% | "(" ","? %Identifier%+ Type-Annotation? ")"
    /// ```
    fn parse_parameter_group(&mut self, delimiters: &[Delimiter]) -> Result<ParameterGroup> {
        let token = self.token();
        match token.kind {
            TokenKind::Identifier(atom) => Ok(ParameterGroup {
                span: token.span,
                parameters: smallvec![Identifier::new(atom, token.span)],
                type_annotation: None,
                explicitness: Explicit,
            }),
            TokenKind::OpeningRoundBracket => {
                let mut span = token.span;
                let explicitness = self.consume_explicitness_symbol();
                let mut parameters = SmallVec::new();

                parameters.push(self.consume_identifier()?);

                let delimiters = [
                    Delimiter::TypeAnnotationPrefix,
                    TokenKind::ClosingRoundBracket.into(),
                ];

                while self.token_not_delimiter(&delimiters) {
                    parameters.push(self.consume_identifier()?);
                }

                let type_annotation = self.parse_optional_type_annotation()?;

                span.merging(&self.consume(TokenKind::ClosingRoundBracket)?);

                Ok(ParameterGroup {
                    parameters,
                    type_annotation,
                    explicitness,
                    span,
                })
            }

            _ => {
                // @Beacon @Beacon @Task create nicer API!!! dude!
                return Err(unexpected_token(
                    token,
                    &*Some(&"parameter" as &dyn Expected)
                        .into_iter()
                        .chain(delimiters.iter().map(|delimiter| delimiter as _))
                        .collect::<Vec<_>>(),
                ));
            }
        }
    }

    // Pattern ::= Lower_Pattern+
    // @Task @Beacon @Beacon implicit application e.g. `foo (,bar)`
    fn parse_pattern(&mut self) -> Result<Pattern> {
        let mut callee = self.reflect(Self::parse_lower_pattern)?;

        // @Note probably produces bad erorr messages, replace with delimiter-logic
        while let Ok(argument) = self.reflect(Self::parse_lower_pattern) {
            callee = pat! {
                Deapplication[callee.span.merge(argument.span)] {
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
    /// Lower_Pattern ::= Nat_Literal | "?" Identifier | Path | "(" Pattern ")"
    /// ```
    fn parse_lower_pattern(&mut self) -> Result<Pattern> {
        let token = self.token();

        use TokenKind::*;

        Ok(match token.kind {
            NatLiteral(nat) => {
                self.advance();
                pat! {
                    NatLiteral[token.span] {
                        value: nat,
                    }
                }
            }
            TextLiteral(text) => {
                self.advance();
                pat! {
                    TextLiteral[token.span] {
                        value: text,
                    }
                }
            }
            QuestionMark => {
                self.advance();
                let binder = self.consume_identifier()?;
                pat! { Binder[token.span.merge(binder.span)] { binder } }
            }
            Identifier(identifier) => {
                self.advance();
                self.finish_parse_path_with_identifier(self::Identifier::new(
                    identifier, token.span,
                ))?
                .into()
            }
            Crate => {
                self.advance();
                self.finish_parse_path_with_head(PathHead::new(PathHeadKind::Crate, token.span))?
                    .into()
            }
            Super => {
                self.advance();
                self.finish_parse_path_with_head(PathHead::new(PathHeadKind::Super, token.span))?
                    .into()
            }
            // @Task @Beacon use a "finish"-parser
            OpeningRoundBracket => return self.parse_bracketed(Self::parse_pattern),
            _ => return Err(unexpected_token(token, "pattern")),
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

    /// Parse a type annotation.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Optional-Type-Annotation ::= (":" Expression)?
    /// ```
    fn parse_optional_type_annotation(&mut self) -> Result<Option<Expression>> {
        if self.consumed(TokenKind::Colon) {
            self.parse_expression().map(Some)
        } else {
            Ok(None)
        }
    }

    /// Parse a possibly indented expression terminated by a line break.
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

    /// Parse a possibly indented expression terminated by a line break.
    fn parse_possibly_indented_expression(&mut self) -> Result<Expression> {
        use TokenKind::*;

        if self.consumed(LineBreak) {
            self.consume(Indentation)?;
            let expression = self.parse_expression()?;
            let _ = self.consumed(LineBreak);
            self.consume(Dedentation)?;
            Ok(expression)
        } else {
            self.parse_expression()
        }
    }

    /// Parse an expression terminated by a line break.
    fn parse_terminated_expression(&mut self) -> Result<Expression> {
        let expression = self.parse_expression()?;
        self.consume(TokenKind::LineBreak)?;
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

    fn consume_explicitness_symbol(&mut self) -> Explicitness {
        if self.consumed(TokenKind::Comma) {
            // @Note there might be false positives (through arbitrary look-ahead)
            // @Task let this function have access to the source map #ParserRefactor
            Diagnostic::new(
                Level::Warning,
                Code::W001,
                "implicitness markers are currently ignored",
            )
            // .with_span(token.span)
            .emit(None);
            Implicit
        } else {
            Explicit
        }
    }
}

use std::borrow::Cow;

trait Expected {
    fn display(&self) -> Cow<'_, str>;
}

impl Expected for TokenKind {
    fn display(&self) -> Cow<'_, str> {
        format!("{}", self).into()
    }
}
impl Expected for &'static str {
    fn display(&self) -> Cow<'_, str> {
        (*self).into()
    }
}

impl Expected for &'_ [&'_ dyn Expected] {
    fn display(&self) -> Cow<'_, str> {
        debug_assert!(!self.is_empty());

        let (first, last) = self.split_at(self.len() - 1);
        if first.is_empty() {
            last[0].display()
        } else {
            use joinery::JoinableIterator;
            format!(
                "{} or {}",
                first.iter().map(|item| item.display()).join_with(", "),
                last[0].display()
            )
            .into()
        }
    }
}

fn unexpected_token(actual: Token, expected: impl Expected) -> Diagnostic {
    Diagnostic::new(
        Level::Fatal,
        Code::E010,
        format!("found {}, but expected {}", actual.kind, expected.display()),
    )
    .with_span(&actual)
}

enum Delimiter {
    TypeAnnotationPrefix,
    DefinitionPrefix,
    Token(TokenKind),
}

impl Expected for Delimiter {
    fn display(&self) -> Cow<'_, str> {
        match self {
            Self::TypeAnnotationPrefix => "type annotation".into(),
            Self::DefinitionPrefix => "definition".into(),
            Self::Token(token) => token.display(),
        }
    }
}

impl PartialEq for Delimiter {
    fn eq(&self, other: &Self) -> bool {
        <&TokenKind>::from(self) == <&TokenKind>::from(other)
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
