//! The parser.
//!
//! I *think* it can be classified as a top-down recursive-descent parser with arbitrary look-ahead.
//!
//! ## Issues
//!
//! * crude error locations
//! * cannot really handle optional indentation

// @Beacon @Task make some errors non-fatal (e.g. unknown attributes)
// @Task merge parse_parameters and parse_annotated_parameters using enum AnnotationPolicy { Mandatory, Optional }
// to create better error messages: "missing type annotation on declaration",
// subdiagnostic: "note: type annotations are mandatory on the top level",
// subdiagnostic: "help: instead of `x`, write `(x: ?Type)`"

mod ast;

use crate::{
    diagnostic::{Code, Diagnostic, Level, Result},
    lexer::{self, Token, TokenKind},
    smallvec,
    span::{SourceFile, Span, Spanned},
    Nat, SmallVec,
};
pub use ast::*;
use std::rc::Rc;

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
}

fn unexpected_token<T: std::fmt::Display>(actual: Token, expected: T) -> Diagnostic {
    Diagnostic::new(
        Level::Fatal,
        Code::E010,
        format!("expected {}, found {}", expected, actual.kind),
    )
    .with_span(&actual)
}

trait Expect {
    type Output;

    fn expect(parser: &Parser<'_>) -> Result<Self::Output>;

    fn consume(parser: &mut Parser<'_>) -> Result<Self::Output> {
        let output = Self::expect(parser)?;
        parser.advance();
        Ok(output)
    }
}

impl Expect for Identifier {
    type Output = Self;

    fn expect(parser: &Parser<'_>) -> Result<Self> {
        let actual = parser.token();
        match actual.kind {
            TokenKind::Identifier(identifier) => Ok(Identifier::new(identifier, actual.span)),
            _ => Err(unexpected_token(actual, "identifier")),
        }
    }
}

impl Expect for Nat {
    type Output = (Self, Span);

    fn expect(parser: &Parser<'_>) -> Result<Self::Output> {
        let actual = parser.token();
        match actual.kind {
            TokenKind::NatLiteral(nat) => Ok((nat, actual.span)),
            _ => Err(unexpected_token(actual, "natural number literal")),
        }
    }
}

impl Parser<'_> {
    fn consume(&mut self, token_kind: lexer::TokenKind) -> Result<lexer::Token> {
        let token = self.expect(token_kind)?;
        self.advance();
        Ok(token)
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    fn advance_with<Node, Knowledge>(
        &mut self,
        knowledge: Knowledge,
        subparser: fn(&mut Self, Knowledge) -> Result<Node>,
    ) -> Result<Node> {
        self.advance();
        subparser(self, knowledge)
    }

    // @Note cloning is not that good, esp. text literals
    // but otherwise, we run into nasty borrowing errors because of
    // compound borrowing (limitation of rustc)
    fn token(&self) -> lexer::Token {
        self.tokens[self.index].clone()
    }

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
            Identifier(identifier) => self.advance_with(
                self::Identifier::new(identifier, token.span),
                Self::finish_parse_value_declaration,
            ),
            Data => self.advance_with(token.span, Self::finish_parse_data_declaration),
            Module => self.advance_with(token.span, Self::finish_parse_module_declaration),
            Use => self.advance_with(token.span, Self::finish_parse_use_declaration),
            Underscore => {
                let attribute = self.advance_with(token.span, Self::finish_parse_attribute)?;
                let mut declaration = self.parse_declaration()?;
                declaration.attributes.push(attribute);
                Ok(declaration)
            }
            DocumentationComment => {
                let attribute =
                    self.advance_with(token.span, Self::finish_parse_documentation_comment)?;
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

        let identifier = Identifier::consume(self)?;
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
        span.merging(&self.consume(TokenKind::Underscore)?);
        self.consume(TokenKind::LineBreak)?;

        Ok(Attribute { span, kind })
    }

    /// Finish parsing documentation comment.
    ///
    /// The comment should have already been parsed beforehand.
    /// The span does not include the trailing line break.
    ///
    /// ## Grammer
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
        let mut span = binder.span;

        let parameters = self.parse_parameters()?;
        span.merging(&parameters);
        let type_annotation = self.parse_optional_type_annotation()?;
        span.merging(&type_annotation);

        let expression = if self.consumed(TokenKind::Equals) {
            let expression = self.parse_possibly_indented_expression_followed_by_line_break()?;
            span.merging(&expression);
            Some(expression)
        } else {
            self.consume(TokenKind::LineBreak)?;
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

        let binder = self::Identifier::consume(self)?;
        let parameters = self.parse_parameters()?;
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
        let mut span = keyword_span;

        let binder = Identifier::consume(self)?;
        span.merging(&binder);

        if self.consumed(TokenKind::LineBreak) {
            return Ok(decl! {
                Module[span] {
                    binder,
                    file: self.file.clone(),
                    declarations: None,
                }
            });
        }

        self.consume(TokenKind::Colon)?;
        self.consume(TokenKind::Equals)?;
        self.consume(TokenKind::LineBreak)?;

        let mut declarations = Vec::new();

        // @Task abstract over this, @Note we could maybe use an indentation counter
        // to make this very easy
        while self.consumed(TokenKind::Indentation) {
            while !self.current(TokenKind::Dedentation) {
                if self.consumed(TokenKind::LineBreak) {
                    continue;
                }

                declarations.push(self.parse_declaration()?);
            }

            self.consume(TokenKind::Dedentation)?;

            while self.consumed(TokenKind::LineBreak) {}
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
        let path = self.parse_path_raw()?;
        self.consume(TokenKind::LineBreak)?;

        Ok(decl! {
            Use[keyword_span.merge(path.span)] {
                path: path.kind,
                bindings: (),
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
                    self.advance_with(token.span, Self::finish_parse_attribute)?
                }
                TokenKind::DocumentationComment => {
                    self.advance_with(token.span, Self::finish_parse_documentation_comment)?
                }
                _ => break,
            });
        }

        let binder = Identifier::consume(self)?;
        let mut span = binder.span;

        let parameters = self.parse_parameters()?;
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
            Let => self.advance_with(token.span, Self::finish_parse_let_in),
            Backslash => self.advance_with(token.span, Self::finish_parse_lambda_literal),
            Case => self.advance_with(token.span, Self::finish_parse_case_analysis),
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
    fn parse_pi_type_literal_or_lower(&mut self) -> Result<Expression> {
        let mut span = Span::SHAM;

        let (explicitness, binder, parameter) = self
            .reflect(|parser| {
                span = parser.consume(TokenKind::OpeningRoundBracket)?.span;

                let explicitness = parser.consume_explicitness_symbol();
                let binder = Identifier::consume(parser)?;
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
    fn parse_application_or_lower(&mut self) -> Result<Expression> {
        let mut expression = self.reflect(Self::parse_lower_expression)?;
        while let Ok((argument, explicitness)) = self
            .reflect(|parser| Ok((parser.parse_lower_expression()?, Explicit)))
            .or_else(|_| -> Result<_> {
                self.consume(TokenKind::OpeningRoundBracket)?;
                self.consume(TokenKind::VerticalBar)?;
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
        match token.kind {
            // @Task use a "finish"-parser
            Identifier(_) => self.parse_path(),
            Crate => self.parse_path(),
            Super => self.parse_path(),
            Type => {
                self.advance();
                Ok(expr! { TypeLiteral[token.span] })
            }
            NatLiteral(value) => {
                self.advance();
                Ok(expr! { NatLiteral[token.span] { value } })
            }
            TextLiteral(value) => {
                self.advance();
                Ok(expr! { TextLiteral[token.span] { value } })
            }
            // @Task use advance_with//finish
            OpeningRoundBracket => self.parse_bracketed(Self::parse_expression),
            _ => return Err(unexpected_token(token, "expression")),
        }
    }

    /// Parse a path.
    fn parse_path(&mut self) -> Result<Expression> {
        let path = self.parse_path_raw()?;
        Ok(expr! {
            Path[path.span] {
                head: path.kind.head,
                segments: path.kind.segments
            }
        })
    }

    /// Parse a path but return an unboxed expression.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Path ::= ("crate" | "super" | %Identifier%)? ("." %Identifier%)*
    /// ```
    // @Note ugly
    fn parse_path_raw(&mut self) -> Result<RawExpression<Path>> {
        let mut segments = Vec::new();

        let token = self.token();
        // @Note super ugly handling + data structures
        let head = match token.kind {
            TokenKind::Identifier(identifier) => {
                segments.push(Identifier::new(identifier, token.span));
                None
            }
            TokenKind::Crate => Some(PathHead::new(PathHeadKind::Crate, token.span)),
            TokenKind::Super => Some(PathHead::new(PathHeadKind::Super, token.span)),
            _ => return Err(unexpected_token(token, "path")),
        };
        let mut span = token.span;
        self.advance();

        while self.consumed(TokenKind::Dot) {
            segments.push(Identifier::consume(self)?);
        }

        span.merging(&segments.last());

        Ok(RawExpression {
            span,
            kind: Path { head, segments },
        })
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
        let parameters = self.parse_parameters()?;
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
        let binder = Identifier::consume(self)?;
        let parameters = self.parse_parameters()?;
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
    /// @Task update to new syntax
    ///
    /// ```text
    /// Case-Analysis ::= "case" Expression Line-Break Case-Analysis-Case-Group*
    /// ```
    // @Note grammar is out-of-sync in respect to line breaks
    fn finish_parse_case_analysis(&mut self, span_of_case: Span) -> Result<Expression> {
        let expression = self.parse_expression_followed_by_line_break()?;

        let mut cases = Vec::new();

        while self.current(TokenKind::Of) {
            cases.push(self.parse_case_analysis_case_group()?);

            // only consume a line break if the token after the line break is the keyword "of" to
            // achieve parsing line breaks as joins/separators between each case group
            if self.current(TokenKind::LineBreak) && self.succeeding(TokenKind::Of) {
                self.advance();
            }
        }

        Ok(expr! {
            CaseAnalysis[span_of_case.merge(
                cases
                    .last()
                    .map(|case_group| case_group.expression.span)
                    .unwrap_or_else(|| expression.span),
            )] {
                expression,
                cases,
            }
        })
    }

    /// Parse a case group (part of a case analysis).
    ///
    /// ## Grammar
    ///
    /// @Task update to new syntax
    ///
    /// ```text
    /// Case-Analyis-Case-Group ::= ("of" Pattern)+ "=>" Expression
    /// ```
    fn parse_case_analysis_case_group(&mut self) -> Result<CaseAnalysisCaseGroup> {
        let mut patterns = Vec::new();

        self.consume(TokenKind::Of)?;
        patterns.push(self.parse_pattern()?);

        while !self.consumed(TokenKind::WideArrow) {
            self.consume(TokenKind::Of)?;
            patterns.push(self.parse_pattern()?);
        }

        Ok(CaseAnalysisCaseGroup {
            patterns,
            expression: self.parse_expression()?,
        })
    }

    /// Parse parameters.
    ///
    /// Optional type annotations. Thus, this is used for lambdas and let/in,
    /// not declarations. Use [Parser::parse_annotated_parameters] for the latter case.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Parameters ::= Parameter-Group*
    /// ```
    // @Task @Beacon @Beacon vastly improve parsing techniques (match over or_else) for better
    // error messages!!
    // @Note maybe take a delimiter: TokenKind
    fn parse_parameters(&mut self) -> Result<Parameters> {
        let mut parameters = Vec::new();

        // @Bug produces bad error messages
        while let Ok(parameter_group) = self.reflect(Self::parse_parameter_group) {
            parameters.push(parameter_group)
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
    /// ## Grammar
    ///
    /// ```text
    /// Parameter-Group ::= %Identifier% | Optionally-Annotated-Parameter-Group
    /// ```
    fn parse_parameter_group(&mut self) -> Result<ParameterGroup> {
        // @Bug bad error message: use match instead of or_else
        Identifier::consume(self)
            .map(|identifier| ParameterGroup {
                span: identifier.span,
                parameters: smallvec![identifier],
                type_annotation: None,
                explicitness: Explicit,
            })
            .or_else(|_| self.parse_optionally_annotated_parameter_group())
    }

    /// Parse an optionally type-annotated parameter group.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Optionally-Annotated-Parameter-Group ::= "(" "|"? %Identifier%+ Type-Annotation? ")"
    /// ```
    // @Note bad naming because `parse_parameter_group` actually also parses an
    // optionally annotated parameter group
    fn parse_optionally_annotated_parameter_group(&mut self) -> Result<ParameterGroup> {
        let mut span = self.consume(TokenKind::OpeningRoundBracket)?.span;
        let explicitness = self.consume_explicitness_symbol();
        let mut parameters = SmallVec::new();

        parameters.push(Identifier::consume(self)?);

        // @Bug produces bad error messages
        while let Ok(parameter) = Identifier::consume(self) {
            parameters.push(parameter);
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

    // Pattern ::= Lower_Pattern+
    fn parse_pattern(&mut self) -> Result<Pattern> {
        let mut callee = self.reflect(Self::parse_lower_pattern)?;

        while let Ok(argument) = self.reflect(Self::parse_lower_pattern) {
            callee = pat! {
                ApplicationPattern[callee.span.merge(argument.span)] {
                    callee,
                    argument,
                }
            };
        }
        Ok(callee)
    }

    // @Task Lower_Pattern ::= Nat_Literal | "(" Path Type_Annotation? ")" | Path | "(" Pattern ")"
    // @Bug or_else(_) produces bad error messages
    // @Task @Beacon @Beacon use match + custom error message ("expected pattern")
    // @Note all this ugliness will (hopefully) go away once we improve the
    // syntax of patterns
    // @Beacon @Task update to new syntax
    fn parse_lower_pattern(&mut self) -> Result<Pattern> {
        self.parse_nat_literal_pattern()
            .or_else(|_| {
                let (span, path, type_annotation) =
                    if let Ok(opening_bracket) = self.consume(TokenKind::OpeningRoundBracket) {
                        let path = self.parse_path_raw()?;
                        let type_annotation = self.reflect(Self::parse_type_annotation)?;
                        let span_of_closing_bracket =
                            self.consume(TokenKind::ClosingRoundBracket)?.span;

                        (
                            opening_bracket.span.merge(span_of_closing_bracket),
                            path.kind,
                            Some(type_annotation),
                        )
                    } else {
                        let path = self.parse_path_raw()?;
                        (path.span, path.kind, None)
                    };

                Ok(pat! {
                    PathPattern[span] {
                        head: path.head,
                        segments: path.segments,
                        type_annotation,
                    }
                })
            })
            .or_else(|_: Diagnostic| self.parse_bracketed(Self::parse_pattern))
    }

    fn parse_nat_literal_pattern(&mut self) -> Result<Pattern> {
        Nat::consume(self).map(|(nat, span)| {
            pat! {
                NatLiteralPattern[span] {
                    value: nat,
                }
            }
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

    // @Task generalize, move somewhere else
    // @Note this is currently only used for let-declarations and I don't know if
    // it works in other parsers
    fn parse_possibly_indented_expression_followed_by_line_break(&mut self) -> Result<Expression> {
        if self.consumed(TokenKind::LineBreak) {
            self.consume(TokenKind::Indentation)?;
            let expression = self.parse_expression_followed_by_line_break()?;
            self.consume(TokenKind::Dedentation)?;
            Ok(expression)
        } else {
            self.parse_expression_followed_by_line_break()
        }
    }

    // @Note temporary, generalize
    fn parse_expression_followed_by_line_break(&mut self) -> Result<Expression> {
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
        match self.consume(TokenKind::VerticalBar) {
            Ok(_token) => {
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
            }
            Err(_) => Explicit,
        }
    }
}
