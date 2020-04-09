//! The parser.
//!
//! I *think* it can be classified as a top-down recursive-descent parser with arbitrary look-ahead.
//!
//! ## Issues
//!
//! * crude error locations
//! * cannot really handle optional indentation and some legal EOIs

// @Beacon @Task make some errors non-fatal (e.g. unknown attributes)

mod ast;

use freestanding::freestanding;
use std::fmt;

use crate::{
    diagnostic::{Code, Diagnostic, Level},
    lexer::{self, Token, TokenKind},
    smallvec,
    span::{Span, Spanned},
    Nat, SmallVec,
};
pub use ast::*;

pub struct Parser<'input> {
    tokens: &'input [Token],
    index: usize,
}

impl<'input> Parser<'input> {
    /// Construct a new context with the pointer at the beginning.
    pub fn new(tokens: &'input [Token]) -> Self {
        Self { tokens, index: 0 }
    }
}

// @Task document that they may panic if EndOfInput not correctly handled (bug)
impl Parser<'_> {
    /// Parse the source in a sandboxed context.
    ///
    /// Used for arbitrary look-ahead. Restores the old cursor on failure.
    // @Bug because of arbitrary look-ahead/this function, a magnitude of Diagnostics are constructed
    // but never emitted/wasted
    fn reflect<Node>(&mut self, parser: fn(&mut Self) -> Result<Node>) -> Result<Node> {
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
            Err(Diagnostic::new(
                Level::Fatal,
                Code::E010,
                format!("expected {}, found {}", expected, actual.kind),
            )
            .with_span(actual.span))
        }
    }
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
            _ => Err(Diagnostic::new(
                Level::Fatal,
                Code::E010,
                format!("expected identifier, found {}", actual.kind),
            )
            .with_span(actual.span)),
        }
    }
}

impl Expect for Nat {
    type Output = (Self, Span);

    fn expect(parser: &Parser<'_>) -> Result<Self::Output> {
        let actual = parser.token();
        match actual.kind {
            TokenKind::NatLiteral(nat) => Ok((nat, actual.span)),
            _ => Err(Diagnostic::new(
                Level::Fatal,
                Code::E010,
                format!("expected natural number literal, found {}", actual.kind),
            )
            .with_span(actual.span)),
        }
    }
}

impl Expect for String {
    type Output = (Self, Span);

    fn expect(parser: &Parser<'_>) -> Result<Self::Output> {
        let actual = parser.token();
        match actual.kind {
            TokenKind::TextLiteral(text) => Ok((text, actual.span)),
            _ => Err(Diagnostic::new(
                Level::Fatal,
                Code::E010,
                format!("expected text literal, found {}", actual.kind),
            )
            .with_span(actual.span)),
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
    /// Declaration ::= Value_Declaration | Data_Declaration | Foreign_Declaration
    /// ```
    pub fn parse_declaration(&mut self) -> Result<Declaration> {
        let token = self.token();

        // @Task transform attribute logic into iterative alogorithm just like we do in
        // `parse_constructor`
        match token.kind {
            TokenKind::Identifier(identifier) => self.advance_with(
                Identifier::new(identifier, token.span),
                Self::finish_parse_value_declaration,
            ),
            TokenKind::Data => self.advance_with(token.span, Self::finish_parse_data_declaration),
            TokenKind::Underscore => {
                let attribute = self.advance_with(token.span, Self::finish_parse_attribute)?;
                let mut declaration = self.parse_declaration()?;
                declaration.attributes.push(attribute);
                Ok(declaration)
            }
            TokenKind::DocumentationComment => {
                let attribute =
                    self.advance_with(token.span, Self::finish_parse_documentation_comment)?;
                let mut declaration = self.parse_declaration()?;
                declaration.attributes.push(attribute);
                Ok(declaration)
            }
            _ => Err(Diagnostic::new(
                Level::Fatal,
                Code::E010,
                format!("expected start of declaration, found {}", token.kind),
            )
            .with_span(token.span)),
        }
    }

    /// Finish parsing attribute.
    ///
    /// First underscore is already parsed.
    /// The span does not include the trailing line break.
    // @Task abstract over attributes with 0, 1, … arguments
    fn finish_parse_attribute(&mut self, span_of_underscore: Span) -> Result<Attribute> {
        let identifier = Identifier::consume(self)?;
        let kind = match &*identifier.atom {
            "foreign" => AttributeKind::Foreign,
            "inherent" => AttributeKind::Inherent,
            _ => {
                return Err(Diagnostic::new(
                    Level::Fatal,
                    Code::E011,
                    format!("attribute `{}` does not exist", identifier),
                )
                .with_span(identifier.span))
            }
        };
        let ending_underscore = self.consume(TokenKind::Underscore)?;
        self.consume(TokenKind::LineBreak)?;

        Ok(Attribute {
            span: span_of_underscore.merge(ending_underscore.span),
            kind,
        })
    }

    /// Finish parsing documentation comment.
    ///
    /// The comment is already parsed.
    /// The span does not include the trailing line break.
    fn finish_parse_documentation_comment(&mut self, span: Span) -> Result<Attribute> {
        self.consume(TokenKind::LineBreak)?;

        Ok(Attribute {
            kind: AttributeKind::Documentation,
            span,
        })
    }

    /// Finish parsing a value declaration.
    ///
    /// Leading identifier already parsed.
    /// The span does not include the trailing line break.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Value_Declaration ::= Identifier Annotated_Parameters Type_Annotation "="
    ///     Possibly_Indented_Expression_Followed_By_Line_Break
    /// ```
    fn finish_parse_value_declaration(&mut self, binder: Identifier) -> Result<Declaration> {
        let parameters = self.parse_annotated_parameters()?;
        let type_annotation = self.parse_type_annotation()?;

        let (expression, span) = if self.consumed(TokenKind::Equals) {
            let expression = self.parse_possibly_indented_expression_followed_by_line_break()?;
            let span = expression.span;
            (Some(expression), span)
        } else {
            self.consume(TokenKind::LineBreak)?;
            (None, type_annotation.span)
        };

        Ok(Declaration {
            span: binder.span.merge(span),
            kind: DeclarationKind::Value(Box::new(Value {
                binder,
                parameters,
                type_annotation,
                expression,
            })),
            attributes: Attributes::default(),
        })
    }

    // @Task grammar rule
    /// Finish parsing a data declaration.
    ///
    /// Keyword `data` is already parsed.
    /// The span does not include the trailing line break.
    // @Task allow empty lines (just line breaks) in constructor list (allows comments)
    pub fn finish_parse_data_declaration(&mut self, span_of_data: Span) -> Result<Declaration> {
        let binder = Identifier::consume(self)?;
        let parameters = self.parse_annotated_parameters()?;
        let type_annotation = self.parse_type_annotation()?;

        let (constructors, span) = match self.consume(TokenKind::Equals).ok() {
            Some(equals) => {
                self.consume(TokenKind::LineBreak)?;

                let mut constructors = Vec::new();

                if self.consumed(TokenKind::Indentation) {
                    // @Bug produces bad error messages
                    while let Ok(constructor) = self.reflect(Self::parse_constructor) {
                        constructors.push(constructor);
                    }
                    // @Question or EOI?
                    self.consume(TokenKind::Dedentation)?;
                }

                let span = constructors
                    .last()
                    .map(|constructor| constructor.span)
                    .unwrap_or(equals.span);

                (Some(constructors), span)
            }
            None => {
                self.consume(TokenKind::LineBreak)?;
                (None, type_annotation.span)
            }
        };

        Ok(Declaration {
            span: span_of_data.merge(span),
            kind: DeclarationKind::Data(Box::new(Data {
                binder,
                parameters,
                type_annotation,
                constructors,
            })),
            attributes: Attributes::default(),
        })
    }

    pub fn parse_file_module_no_header(&mut self) -> Result<Declaration> {
        let mut declarations = Vec::<Declaration>::new();

        loop {
            // skip empty lines
            if self.consumed(TokenKind::LineBreak) {
                continue;
            }

            if self.consumed(TokenKind::EndOfInput) {
                break Ok(Declaration {
                    span: (|| Some(declarations.first()?.span.merge(declarations.last()?.span)))()
                        .unwrap_or(Span::DUMMY),
                    kind: DeclarationKind::Module(Box::new(Module { declarations })),
                    attributes: Attributes::default(),
                });
            }

            declarations.push(self.parse_declaration()?);
        }
    }

    /// Parse constructor.
    ///
    /// The span does not include the trailing line break.
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
        let parameters = self.parse_annotated_parameters()?;
        let type_annotation = self.parse_type_annotation()?;
        // @Task allow EOI as an alternative
        self.consume(TokenKind::LineBreak)?;

        Ok(Declaration {
            span: binder.span.merge(type_annotation.span),
            kind: DeclarationKind::Constructor(Box::new(Constructor {
                binder,
                parameters,
                type_annotation,
            })),
            attributes,
        })
    }

    /// Parse type-annotated parameters.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Annotated_Parameters ::= Annotated_Parameter_Group*
    /// ```
    fn parse_annotated_parameters(&mut self) -> Result<AnnotatedParameters> {
        let mut parameters = Vec::new();

        while self.current(TokenKind::OpeningRoundBracket) {
            parameters.push(self.reflect(Self::parse_annotated_parameter_group)?);
        }

        Ok(parameters)
    }

    /// Parse a type-annotated parameter group.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Annotated_Parameter_Group ::= "(" "|"? Unfenced_Annotated_Parameter_Group ")"
    /// Unfenced_Annotated_Parameter_Group ::= Identifier+ Type_Annotation
    /// ```
    fn parse_annotated_parameter_group(&mut self) -> Result<AnnotatedParameterGroup> {
        self.consume(TokenKind::OpeningRoundBracket)?;
        let explicitness = self.consume_explicitness_symbol();
        let mut parameters = SmallVec::new();

        parameters.push(Identifier::consume(self)?);

        // @Note @Bug probably drops errors as well @Note shouldn't: consume
        // reflects by itself @Task verify
        // @Bug produces bad error messages
        while let Ok(parameter) = Identifier::consume(self) {
            parameters.push(parameter);
        }

        let type_annotation = self.reflect(Self::parse_type_annotation)?;
        self.consume(TokenKind::ClosingRoundBracket)?;

        Ok(AnnotatedParameterGroup {
            parameters,
            type_annotation,
            explicitness,
        })
    }

    /// Parse an expression.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Expression ::= Let_In | Lambda_Literal | Case_Analysis | Pi_Literal_Or_Lower
    /// ```
    pub fn parse_expression(&mut self) -> Result<Expression> {
        let token = self.token();

        match token.kind {
            TokenKind::Let => self.advance_with(token.span, Self::finish_parse_let_in),
            TokenKind::Backslash => {
                self.advance_with(token.span, Self::finish_parse_lambda_literal)
            }
            TokenKind::Case => self.advance_with(token.span, Self::finish_parse_case_analysis),
            _ => self.parse_pi_type_literal_or_lower(),
        }
    }

    /// Parse a pi-type literal or a lower expression.
    // @Task update grammar
    // Pi_Literal_Or_Lower %right% ::= Application_Or_Lower (-> Pi_Literal_Or_Lower)*
    fn parse_pi_type_literal_or_lower(&mut self) -> Result<Expression> {
        let (span_at_the_beginning, explicitness, binder, parameter) = self
            .reflect(|parser| {
                let span_of_opening_round_bracket =
                    parser.consume(TokenKind::OpeningRoundBracket)?.span;

                let explicitness = parser.consume_explicitness_symbol();
                let binder = Identifier::consume(parser)?;
                let parameter = parser.parse_type_annotation()?;

                parser.consume(TokenKind::ClosingRoundBracket)?;

                Ok((
                    span_of_opening_round_bracket,
                    explicitness,
                    Some(binder),
                    parameter,
                ))
            })
            .or_else(|_| {
                // @Question do we need reflect here?
                self.reflect(Self::parse_application_or_lower)
                    .map(|parameter| (parameter.span, Explicitness::Explicit, None, parameter))
            })?;

        Ok(match self.consume(TokenKind::ThinArrow) {
            Ok(_) => {
                let expression = self.reflect(Self::parse_pi_type_literal_or_lower)?;

                expr! {
                    PiTypeLiteral[span_at_the_beginning.merge(expression.span)] {
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
    /// Application_Or_Lower %left% ::= Lower_Expression (Lower_Expression | "(" "|" Expression ")")*
    /// ```
    fn parse_application_or_lower(&mut self) -> Result<Expression> {
        let mut expression = self.reflect(Self::parse_lower_expression)?;
        while let Ok((argument, explicitness)) = self
            .reflect(|parser| Ok((parser.parse_lower_expression()?, Explicitness::Explicit)))
            .or_else(|_| -> Result<_> {
                self.consume(TokenKind::OpeningRoundBracket)?;
                self.consume(TokenKind::VerticalBar)?;
                let expression = self.parse_expression()?;
                self.consume(TokenKind::ClosingRoundBracket)?;
                Ok((expression, Explicitness::Implicit))
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
    /// Lower_Expression ::=
    ///     Type_Literal | Nat_Literal | Path | Hole | Bracketed_Expression
    /// ```
    // @Task massively improve structually: match in here "without" extra functions.
    // this will increase performance as well
    fn parse_lower_expression(&mut self) -> Result<Expression> {
        self.reflect(Self::parse_path)
            .or_else(|_| self.parse_type_literal())
            .or_else(|_| self.parse_nat_literal())
            .or_else(|_| self.parse_text_literal())
            .or_else(|_| self.parse_bracketed(Self::parse_expression))
    }

    // Type_Literal ::= %type literal%
    fn parse_type_literal(&mut self) -> Result<Expression> {
        self.consume(TokenKind::Type)
            .map(|token| expr! { TypeLiteral[token.span] })
    }

    pub fn parse_nat_literal(&mut self) -> Result<Expression> {
        Nat::consume(self).map(|(nat, span)| {
            expr! {
                NatLiteral[span] {
                    value: nat
                }
            }
        })
    }

    fn parse_text_literal(&mut self) -> Result<Expression> {
        String::consume(self).map(|(text, span)| {
            expr! {
                TextLiteral[span] {
                    value: text,
                }
            }
        })
    }

    // Path ::= %identifier%
    fn parse_path(&mut self) -> Result<Expression> {
        Identifier::consume(self).map(|identifier| {
            expr! {
                Path[identifier.span] {
                    segments: identifier,
                }
            }
        })
    }

    /// Finish parsing a lambda literal expression.a
    ///
    /// The initial `\` ought to be consumed beforehand.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Lambda_Literal ::= "\" Parameters Type_Annotation? "=>" Expression
    /// ```
    fn finish_parse_lambda_literal(&mut self, span_of_backslash: Span) -> Result<Expression> {
        let parameters = self.reflect(Self::parse_parameters)?;
        let body_type_annotation = self.reflect(Self::parse_type_annotation).ok();
        self.consume(TokenKind::WideArrow)?;
        let body = self.reflect(Self::parse_expression)?;

        Ok(expr! {
            LambdaLiteral[span_of_backslash.merge(body.span)] {
                parameters,
                body_type_annotation,
                body,
            }
        })
    }

    /// Finish parsing an let-in expression.
    ///
    /// The initial `let` ought to be consumed beforehand.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Let_In ::= "'let" Identifier Parameters Type_Annotation? "=" Expression "'in" Expression
    /// ```
    fn finish_parse_let_in(&mut self, span_of_let: Span) -> Result<Expression> {
        let binder = Identifier::consume(self)?;
        let parameters = self.reflect(Self::parse_parameters)?;
        let type_annotation = self.reflect(Self::parse_type_annotation).ok();
        self.consume(TokenKind::Equals)?;
        let expression = self.reflect(Self::parse_expression)?;
        self.consume(TokenKind::In)?;
        // @Bug commented code does not work as expected (says: unexpected dedentation)
        // let scope = self.reflect(parse_possibly_indented_expression_followed_by_line_break)?;
        let scope = self.reflect(Self::parse_expression)?;

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
    /// The initial `case` ought to be consumed beforehand.
    // @Note grammar is out-of-sync in respect to line breaks
    // Case_Analysis ::= "'case" Expression Line_Break Case_Analysis_Case_Group*
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

    // Case_Analyis_Case_Group ::= ("of" Pattern)+ "=>" Expression
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

    // Parameters ::= Parameter_Group*
    fn parse_parameters(&mut self) -> Result<Parameters> {
        let mut parameters = Vec::new();

        // @Bug produces bad error messages
        while let Ok(parameter_group) = self.reflect(Self::parse_parameter_group) {
            parameters.push(parameter_group)
        }

        Ok(parameters)
    }

    // @Task grammar snippet above function decl
    fn parse_parameter_group(&mut self) -> Result<ParameterGroup> {
        Identifier::consume(self)
            .map(|identifier| ParameterGroup {
                parameters: smallvec![identifier],
                type_annotation: None,
                explicitness: Explicitness::Explicit,
            })
            .or_else(|_| self.parse_optionally_annotated_parameter_group())
    }

    fn parse_optionally_annotated_parameter_group(&mut self) -> Result<ParameterGroup> {
        self.consume(TokenKind::OpeningRoundBracket)?;
        let explicitness = self.consume_explicitness_symbol();
        let mut parameters = SmallVec::new();

        parameters.push(Identifier::consume(self)?);

        // @Bug produces bad error messages
        while let Ok(parameter) = Identifier::consume(self) {
            parameters.push(parameter);
        }

        let type_annotation = self.parse_type_annotation().ok();

        self.consume(TokenKind::ClosingRoundBracket)?;

        Ok(ParameterGroup {
            parameters,
            type_annotation,
            explicitness,
        })
    }

    // Pattern ::= Lower_Pattern+
    pub(super) fn parse_pattern(&mut self) -> Result<Pattern> {
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
    fn parse_lower_pattern(&mut self) -> Result<Pattern> {
        self.parse_nat_literal_pattern()
            .or_else(|_| {
                let (span, identifier, type_annotation) =
                    if let Ok(opening_bracket) = self.consume(TokenKind::OpeningRoundBracket) {
                        let path = Identifier::consume(self)?;
                        let type_annotation = self.reflect(Self::parse_type_annotation)?;
                        let span_of_closing_bracket =
                            self.consume(TokenKind::ClosingRoundBracket)?.span;

                        (
                            opening_bracket.span.merge(span_of_closing_bracket),
                            path,
                            Some(type_annotation),
                        )
                    } else {
                        let identifier = Identifier::consume(self)?;
                        (identifier.span, identifier, None)
                    };

                Ok(pat! {
                    PathPattern[span] {
                        segments: identifier,
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
    /// Type_Annotation ::= ":" Expression
    /// ```
    fn parse_type_annotation(&mut self) -> Result<Expression> {
        self.consume(TokenKind::Colon)?;
        self.reflect(Self::parse_expression)
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
        let span_of_opening_bracket = self.consume(TokenKind::OpeningRoundBracket)?.span;
        // @Question reflect necessary?
        let mut inner = self.reflect(subparser)?;
        let span_of_closing_bracket = self.consume(TokenKind::ClosingRoundBracket)?.span;
        inner.span = span_of_opening_bracket.merge(span_of_closing_bracket);
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
                Explicitness::Implicit
            }
            Err(_) => Explicitness::Explicit,
        }
    }
}

macro expr {
    ($kind:ident[$span:expr] { $( $body:tt )+ }) => {
        Expression {
            span: $span,
            kind: ExpressionKind::$kind(Box::new($kind { $( $body )+ })),
        }
    },
    ($kind:ident[$span:expr]) => {
        Expression {
            span: $span,
            kind: ExpressionKind::$kind,
        }
    }
}

macro pat {
    ($kind:ident[$span:expr] { $( $body:tt )+ }) => {
        Pattern {
            span: $span,
            kind: PatternKind::$kind(Box::new($kind { $( $body )+ })),
        }
    }
}

pub type Result<T, E = Diagnostic> = std::result::Result<T, E>;
