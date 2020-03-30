//! The parser.
//!
//! I *think* it can be classified as a top-down recursive-descent parser with arbitrary look-ahead.
//!
//! ## Issues
//!
//! * crude error locations
//! * cannot really handle optional indentation and some legal EOIs

// @Task @Beacon make parse_* methods of Parser

use freestanding::freestanding;
use std::fmt;

use crate::{
    diagnostic::{Diagnostic, Level},
    lexer::{self, Token, TokenKind},
    span::{Span, Spanned},
};

// @Task use SourceFiles
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
                format!("expected {}, found {}", expected, actual.kind),
            )
            .with_span(actual.span))
        }
    }

    fn expect_identifier(&self) -> Result<Identifier> {
        let actual = self.token();
        match actual.kind {
            TokenKind::Identifier(identifier) => Ok(Identifier::new(identifier, actual.span)),
            _ => Err(Diagnostic::new(
                Level::Fatal,
                format!("expected identifier, found {}", actual.kind),
            )
            .with_span(actual.span)),
        }
    }

    fn expect_nat_literal(&self) -> Result<(crate::Nat, Span)> {
        let actual = self.token();
        match actual.kind {
            TokenKind::NatLiteral(nat) => Ok((nat, actual.span)),
            _ => Err(Diagnostic::new(
                Level::Fatal,
                format!("expected natural number literal, found {}", actual.kind),
            )
            .with_span(actual.span)),
        }
    }

    fn expect_text_literal(&self) -> Result<(String, Span)> {
        let actual = self.token();
        match actual.kind {
            TokenKind::TextLiteral(text) => Ok((text, actual.span)),
            _ => Err(Diagnostic::new(
                Level::Fatal,
                format!("expected text literal, found {}", actual.kind),
            )
            .with_span(actual.span)),
        }
    }

    fn consume(&mut self, token_kind: lexer::TokenKind) -> Result<lexer::Token> {
        let token = self.expect(token_kind)?;
        self.advance();
        Ok(token)
    }

    // @Note boilerplate, use a trait smh
    fn consume_identifier(&mut self) -> Result<Identifier> {
        let identifier = self.expect_identifier()?;
        self.advance();
        Ok(identifier)
    }

    // @Note boilerplate, use a trait smh
    fn consume_nat_literal(&mut self) -> Result<(crate::Nat, Span)> {
        let nat = self.expect_nat_literal()?;
        self.advance();
        Ok(nat)
    }

    // @Note boilerplate, use a trait smh
    fn consume_text_literal(&mut self) -> Result<(String, Span)> {
        let text = self.expect_text_literal()?;
        self.advance();
        Ok(text)
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

    // @Question why clone?
    // @Bug note this probably clones text literals booo!
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
}

pub use declaration::{parse_declaration, Constructor, Declaration, DeclarationKind};

/// The part of the parser concerned with declarations.
pub mod declaration {
    use super::*;

    pub type Declaration = Spanned<DeclarationKind>;

    /// The syntax node of a declaration.
    #[freestanding]
    #[streamline(Box)]
    pub enum DeclarationKind {
        /// The syntax node of a value declaration.
        Value {
            binder: Identifier,
            parameters: AnnotatedParameters,
            type_annotation: Expression,
            expression: Expression,
        },
        /// The syntax node of a data declaration.
        Data {
            binder: Identifier,
            parameters: AnnotatedParameters,
            type_annotation: Expression,
            constructors: Vec<Constructor>,
        },
        /// The syntax node of a module declaration.
        Module { declarations: Vec<Declaration> },
        /// The syntax node of a use declaration.
        Use,
        /// The syntax node of a foreign declaration.
        Foreign {
            binder: Identifier,
            parameters: AnnotatedParameters,
            type_annotation: Expression,
        },
    }

    /// Parse a declaration.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Declaration ::= Value_Declaration | Data_Declaration | Foreign_Declaration
    /// ```
    pub fn parse_declaration(parser: &mut Parser<'_>) -> Result<Declaration> {
        let token = parser.token();

        match token.kind {
            TokenKind::Identifier(identifier) => parser.advance_with(
                Identifier::new(identifier, token.span),
                finish_parse_value_declaration,
            ),
            TokenKind::Data => parser.advance_with(token.span, finish_parse_data_declaration),
            TokenKind::Foreign => parser.advance_with(token.span, finish_parse_foreign_declaration),
            _ => Err(Diagnostic::new(
                Level::Fatal,
                format!("expected start of declaration, found {}", token.kind),
            )
            .with_span(token.span)),
        }
    }

    /// Finish parsing a value declaration.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Value_Declaration ::= Identifier Annotated_Parameters Type_Annotation "="
    ///     Possibly_Indented_Expression_Followed_By_Line_Break
    /// ```
    fn finish_parse_value_declaration(
        parser: &mut Parser<'_>,
        binder: Identifier,
    ) -> Result<Declaration> {
        let parameters = parse_annotated_parameters(parser)?;
        let type_annotation = parse_type_annotation(parser)?;
        parser.consume(TokenKind::Equals)?;
        let expression = parse_possibly_indented_expression_followed_by_line_break(parser)?;

        Ok(Declaration {
            span: binder.span.merge(expression.span),
            kind: DeclarationKind::Value(Box::new(Value {
                binder,
                parameters,
                type_annotation,
                expression,
            })),
        })
    }

    // @Task grammar rule
    /// Finish parsing a data declaration.
    pub fn finish_parse_data_declaration(
        parser: &mut Parser<'_>,
        span_of_data: Span,
    ) -> Result<Declaration> {
        let binder = parser.consume_identifier()?;
        let parameters = parse_annotated_parameters(parser)?;
        let type_annotation = parse_type_annotation(parser)?;
        let span_of_equals = parser.consume(TokenKind::Equals)?.span;
        parser.consume(TokenKind::LineBreak)?;

        let mut constructors = Vec::new();

        if parser.consumed(TokenKind::Indentation) {
            // @Bug produces bad error messages
            while let Ok(constructor) = parser.reflect(parse_constructor) {
                constructors.push(constructor);
            }
            // @Question or EOI?
            parser.consume(TokenKind::Dedentation)?;
        }

        Ok(Declaration {
            span: span_of_data.merge(
                constructors
                    .last()
                    .map(|constructor| constructor.span)
                    .unwrap_or(span_of_equals),
            ),
            kind: DeclarationKind::Data(Box::new(Data {
                binder,
                parameters,
                type_annotation,
                constructors,
            })),
        })
    }

    // @Temporary
    pub fn parse_file_module_no_header(parser: &mut Parser<'_>) -> Result<Declaration> {
        let mut declarations = Vec::<Declaration>::new();

        loop {
            // skip empty lines
            if parser.consumed(TokenKind::LineBreak) {
                continue;
            }

            if parser.consumed(TokenKind::EndOfInput) {
                break Ok(Declaration {
                    // @Temporary span handling because this function itself is temporary @Bug Span::dummy()
                    span: (|| Some(declarations.first()?.span.merge(declarations.last()?.span)))()
                        .unwrap_or(Span::dummy()),
                    kind: DeclarationKind::Module(Box::new(Module { declarations })),
                });
            }

            declarations.push(parse_declaration(parser)?);
        }
    }

    /// Finish parsing a foreign declaration.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Foreign_Declaration ::= "foreign" Identifier Annotated_Parameters Type_Annotation Line_Break
    /// ```
    fn finish_parse_foreign_declaration(
        parser: &mut Parser<'_>,
        span_of_foreign: Span,
    ) -> Result<Declaration> {
        let binder = parser.consume_identifier()?;
        let parameters = parse_annotated_parameters(parser)?;
        let type_annotation = parse_type_annotation(parser)?;
        // @Task allow EOI as an alternative
        parser.consume(TokenKind::LineBreak)?;

        Ok(Declaration {
            span: span_of_foreign.merge(type_annotation.span),
            kind: DeclarationKind::Foreign(Box::new(Foreign {
                binder,
                parameters,
                type_annotation,
            })),
        })
    }

    /// The syntax node of a constructor.
    #[derive(Debug)]
    pub struct Constructor {
        pub binder: Identifier,
        pub parameters: AnnotatedParameters,
        pub type_annotation: Expression,
        pub span: Span,
    }

    fn parse_constructor(parser: &mut Parser<'_>) -> Result<Constructor> {
        let binder = parser.consume_identifier()?;
        let parameters = parse_annotated_parameters(parser)?;
        let type_annotation = parse_type_annotation(parser)?;
        // @Task allow EOI as an alternative
        parser.consume(TokenKind::LineBreak)?;

        Ok(Constructor {
            span: binder.span.merge(type_annotation.span),
            binder,
            parameters,
            type_annotation,
        })
    }

    pub type AnnotatedParameters = Vec<AnnotatedParameterGroup>;

    /// Parse type-annotated parameters.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Annotated_Parameters ::= Annotated_Parameter_Group*
    /// ```
    fn parse_annotated_parameters(parser: &mut Parser<'_>) -> Result<AnnotatedParameters> {
        let mut parameters = Vec::new();

        while parser.current(TokenKind::OpeningRoundBracket) {
            parameters.push(parser.reflect(parse_annotated_parameter_group)?);
        }

        Ok(parameters)
    }

    #[derive(Debug)]
    pub struct AnnotatedParameterGroup {
        pub parameters: Vec<Identifier>, // @Task use SmallVec<[Identifier; 1]> instead of Vec
        pub type_annotation: Expression,
        pub explicitness: Explicitness,
    }

    /// Parse a type-annotated parameter group.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Annotated_Parameter_Group ::= "(" "|"? Unfenced_Annotated_Parameter_Group ")"
    /// Unfenced_Annotated_Parameter_Group ::= Identifier+ Type_Annotation
    /// ```
    fn parse_annotated_parameter_group(parser: &mut Parser<'_>) -> Result<AnnotatedParameterGroup> {
        parser.consume(TokenKind::OpeningRoundBracket)?;
        let explicitness = consume_explicitness_symbol(parser);
        let mut parameters = Vec::new();

        parameters.push(parser.consume_identifier()?);

        // @Note @Bug probably drops errors as well @Note shouldn't: consume
        // reflects by itself @Task verify
        // @Bug produces bad error messages
        while let Ok(parameter) = parser.consume_identifier() {
            parameters.push(parameter);
        }

        let type_annotation = parser.reflect(parse_type_annotation)?;
        parser.consume(TokenKind::ClosingRoundBracket)?;

        Ok(AnnotatedParameterGroup {
            parameters,
            type_annotation,
            explicitness,
        })
    }
}

pub use expression::{parse_expression, Expression, ExpressionKind, Path};

/// The part of the parser concerned with expressions.
pub mod expression {
    use super::*;

    pub type Expression = Spanned<ExpressionKind>;

    /// The syntax node of an expression.
    #[freestanding]
    #[streamline(Box)]
    #[derive(Debug, Clone)]
    pub enum ExpressionKind {
        /// The syntax node of pi-type literals.
        PiTypeLiteral {
            binder: Option<Identifier>,
            parameter: Expression,
            expression: Expression,
            explicitness: Explicitness,
        },
        /// The syntax node of function application.
        Application {
            callee: Expression,
            argument: Expression,
            explicitness: Explicitness,
        },
        TypeLiteral,
        NatTypeLiteral,
        NatLiteral {
            value: crate::Nat,
        },
        TextTypeLiteral,
        TextLiteral {
            value: String,
        },
        // @Task make it able to parse complex paths
        Path {
            // @Task in the future: segments: Vec<Identifier>,
            segments: Identifier,
        },
        /// The syntax node of a lambda literal expression.
        LambdaLiteral {
            parameters: Parameters,
            body_type_annotation: Option<Expression>,
            body: Expression,
        },
        /// The syntax-node of a let-in expression.
        LetIn {
            binder: Identifier,
            parameters: Parameters,
            type_annotation: Option<Expression>,
            // @Task improve upon naming
            expression: Expression,
            scope: Expression,
        },
        UseIn,
        CaseAnalysis {
            expression: Expression,
            cases: Vec<CaseAnalysisCaseGroup>,
        },
    }

    /// Parse an expression.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Expression ::= Let_In | Lambda_Literal | Case_Analysis | Pi_Literal_Or_Lower
    /// ```
    pub fn parse_expression(parser: &mut Parser<'_>) -> Result<Expression> {
        let token = parser.token();

        match token.kind {
            TokenKind::Let => parser.advance_with(token.span, finish_parse_let_in),
            TokenKind::Backslash => parser.advance_with(token.span, finish_parse_lambda_literal),
            TokenKind::Case => parser.advance_with(token.span, finish_parse_case_analysis),
            _ => parse_pi_type_literal_or_lower(parser),
        }
    }

    /// Parse a pi-type literal or a lower expression.
    // @Task update grammar
    // Pi_Literal_Or_Lower %right% ::= Application_Or_Lower (-> Pi_Literal_Or_Lower)*
    fn parse_pi_type_literal_or_lower(parser: &mut Parser<'_>) -> Result<Expression> {
        let (span_at_the_beginning, explicitness, binder, parameter) = parser
            .reflect(|parser| {
                let span_of_opening_round_bracket =
                    parser.consume(TokenKind::OpeningRoundBracket)?.span;

                let explicitness = consume_explicitness_symbol(parser);
                let binder = parser.consume_identifier()?;
                let parameter = parse_type_annotation(parser)?;

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
                parser
                    .reflect(parse_application_or_lower)
                    .map(|parameter| (parameter.span, Explicitness::Explicit, None, parameter))
            })?;

        Ok(match parser.consume(TokenKind::ThinArrow) {
            Ok(_) => {
                let expression = parser.reflect(parse_pi_type_literal_or_lower)?;

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
    fn parse_application_or_lower(parser: &mut Parser<'_>) -> Result<Expression> {
        let mut expression = parser.reflect(parse_lower_expression)?;
        while let Ok((argument, explicitness)) = parser
            .reflect(|parser| Ok((parse_lower_expression(parser)?, Explicitness::Explicit)))
            .or_else(|_| -> Result<_> {
                parser.consume(TokenKind::OpeningRoundBracket)?;
                parser.consume(TokenKind::VerticalBar)?;
                let expression = parse_expression(parser)?;
                parser.consume(TokenKind::ClosingRoundBracket)?;
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
    fn parse_lower_expression(parser: &mut Parser<'_>) -> Result<Expression> {
        parser
            .reflect(parse_path)
            .or_else(|_| parse_type_literal(parser))
            .or_else(|_| parse_nat_type_literal(parser))
            .or_else(|_| parse_nat_literal(parser))
            .or_else(|_| parse_text_type_literal(parser))
            .or_else(|_| parse_text_literal(parser))
            .or_else(|_| parse_bracketed(parser, parse_expression))
    }

    // Type_Literal ::= %type literal%
    fn parse_type_literal(parser: &mut Parser<'_>) -> Result<Expression> {
        parser
            .consume(TokenKind::Type)
            .map(|token| expr! { TypeLiteral[token.span] })
    }

    fn parse_nat_type_literal(parser: &mut Parser<'_>) -> Result<Expression> {
        parser
            .consume(TokenKind::Nat)
            .map(|token| expr! { NatTypeLiteral[token.span] })
    }

    pub fn parse_nat_literal(parser: &mut Parser<'_>) -> Result<Expression> {
        parser.consume_nat_literal().map(|(nat, span)| {
            expr! {
                NatLiteral[span] {
                    value: nat
                }
            }
        })
    }

    fn parse_text_type_literal(parser: &mut Parser<'_>) -> Result<Expression> {
        parser
            .consume(TokenKind::Text)
            .map(|token| expr! { TextTypeLiteral[token.span] })
    }

    fn parse_text_literal(parser: &mut Parser<'_>) -> Result<Expression> {
        parser.consume_text_literal().map(|(text, span)| {
            expr! {
                TextLiteral[span] {
                    value: text,
                }
            }
        })
    }

    // Path ::= %identifier%
    fn parse_path(parser: &mut Parser<'_>) -> Result<Expression> {
        parser.consume_identifier().map(|identifier| {
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
    fn finish_parse_lambda_literal(
        parser: &mut Parser<'_>,
        span_of_backslash: Span,
    ) -> Result<Expression> {
        let parameters = parser.reflect(parse_parameters)?;
        let body_type_annotation = parser.reflect(parse_type_annotation).ok();
        parser.consume(TokenKind::WideArrow)?;
        let body = parser.reflect(parse_expression)?;

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
    fn finish_parse_let_in(parser: &mut Parser<'_>, span_of_let: Span) -> Result<Expression> {
        let binder = parser.consume_identifier()?;
        let parameters = parser.reflect(parse_parameters)?;
        let type_annotation = parser.reflect(parse_type_annotation).ok();
        parser.consume(TokenKind::Equals)?;
        let expression = parser.reflect(parse_expression)?;
        parser.consume(TokenKind::In)?;
        // @Bug commented code does not work as expected (says: unexpected dedentation)
        // let scope = parser.reflect(parse_possibly_indented_expression_followed_by_line_break)?;
        let scope = parser.reflect(parse_expression)?;

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
    fn finish_parse_case_analysis(
        parser: &mut Parser<'_>,
        span_of_case: Span,
    ) -> Result<Expression> {
        let expression = parse_expression_followed_by_line_break(parser)?;

        let mut cases = Vec::new();

        while parser.current(TokenKind::Of) {
            cases.push(parse_case_analysis_case_group(parser)?);

            // only consume a line break if the token after the line break is the keyword "of" to
            // achieve parsing line breaks as joins/separators between each case group
            if parser.current(TokenKind::LineBreak) && parser.succeeding(TokenKind::Of) {
                parser.advance();
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

    #[derive(Debug, Clone)]
    pub struct CaseAnalysisCaseGroup {
        pub patterns: Vec<pattern::Pattern>,
        pub expression: Expression,
    }

    // Case_Analyis_Case_Group ::= ("of" Pattern)+ "=>" Expression
    fn parse_case_analysis_case_group(parser: &mut Parser<'_>) -> Result<CaseAnalysisCaseGroup> {
        let mut patterns = Vec::new();

        parser.consume(TokenKind::Of)?;
        patterns.push(pattern::parse_pattern(parser)?);

        while !parser.consumed(TokenKind::WideArrow) {
            parser.consume(TokenKind::Of)?;
            patterns.push(pattern::parse_pattern(parser)?);
        }

        Ok(CaseAnalysisCaseGroup {
            patterns,
            expression: parse_expression(parser)?,
        })
    }

    pub type Parameters = Vec<ParameterGroup>;

    // Parameters ::= Parameter_Group*
    fn parse_parameters(parser: &mut Parser<'_>) -> Result<Parameters> {
        let mut parameters = Vec::new();

        // @Bug produces bad error messages
        while let Ok(parameter_group) = parser.reflect(parse_parameter_group) {
            parameters.push(parameter_group)
        }

        Ok(parameters)
    }

    #[derive(Debug, Clone)] // @Temporary clone
    pub struct ParameterGroup {
        pub parameters: Vec<Identifier>,
        pub type_annotation: Option<Expression>,
        pub explicitness: Explicitness,
    }

    // @Task grammar snippet above function decl
    fn parse_parameter_group(parser: &mut Parser<'_>) -> Result<ParameterGroup> {
        fn parse_optionally_annotated_parameter_group(
            parser: &mut Parser<'_>,
        ) -> Result<ParameterGroup> {
            parser.consume(TokenKind::OpeningRoundBracket)?;
            let explicitness = consume_explicitness_symbol(parser);
            let mut parameters = Vec::new();

            parameters.push(parser.consume_identifier()?);

            // @Bug produces bad error messages
            while let Ok(parameter) = parser.consume_identifier() {
                parameters.push(parameter);
            }

            let type_annotation = parse_type_annotation(parser).ok();

            parser.consume(TokenKind::ClosingRoundBracket)?;

            Ok(ParameterGroup {
                parameters,
                type_annotation,
                explicitness,
            })
        }

        parser
            .consume_identifier()
            .map(|identifier| ParameterGroup {
                parameters: vec![identifier],
                type_annotation: None,
                explicitness: Explicitness::Explicit,
            })
            .or_else(|_| parse_optionally_annotated_parameter_group(parser))
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
}

pub use pattern::{Pattern, PatternKind};

pub mod pattern {
    use super::*;

    pub type Pattern = Spanned<PatternKind>;

    #[freestanding]
    #[streamline(Box)]
    #[derive(Debug, Clone)]
    pub enum PatternKind {
        NatLiteral {
            value: crate::Nat,
        },
        Path {
            segments: Identifier,
            type_annotation: Option<Expression>,
        },
        Application {
            callee: Pattern,
            argument: Pattern,
        },
    }

    // Pattern ::= Lower_Pattern+
    pub(super) fn parse_pattern(parser: &mut Parser<'_>) -> Result<Pattern> {
        let mut callee = parser.reflect(parse_lower_pattern)?;

        while let Ok(argument) = parser.reflect(parse_lower_pattern) {
            callee = pat! {
                Application[callee.span.merge(argument.span)] {
                    callee,
                    argument,
                }
            };
        }
        Ok(callee)
    }

    // @Task Lower_Pattern ::= Nat_Literal | "(" Path Type_Annotation? ")" | Path | "(" Pattern ")"
    // @Bug or_else(_) produces bad error messages
    fn parse_lower_pattern(parser: &mut Parser<'_>) -> Result<Pattern> {
        parse_nat_literal(parser)
            .or_else(|_| {
                let (span, identifier, type_annotation) =
                    if let Ok(opening_bracket) = parser.consume(TokenKind::OpeningRoundBracket) {
                        let path = parser.consume_identifier()?;
                        let type_annotation = parser.reflect(parse_type_annotation)?;
                        let span_of_closing_bracket =
                            parser.consume(TokenKind::ClosingRoundBracket)?.span;

                        (
                            opening_bracket.span.merge(span_of_closing_bracket),
                            path,
                            Some(type_annotation),
                        )
                    } else {
                        let identifier = parser.consume_identifier()?;
                        (identifier.span, identifier, None)
                    };

                Ok(pat! {
                    Path[span] {
                        segments: identifier,
                        type_annotation,
                    }
                })
            })
            .or_else(|_: Diagnostic| parse_bracketed(parser, parse_pattern))
    }

    fn parse_nat_literal(parser: &mut Parser<'_>) -> Result<Pattern> {
        parser.consume_nat_literal().map(|(nat, span)| {
            pat! {
                NatLiteral[span] {
                    value: nat,
                }
            }
        })
    }

    macro pat {
        ($kind:ident[$span:expr] { $( $body:tt )+ }) => {
            Pattern {
                span: $span,
                kind: PatternKind::$kind(Box::new($kind { $( $body )+ })),
            }
        }
    }
}

/// Parse a type annotation.
///
/// ## Grammar
///
/// ```text
/// Type_Annotation ::= ":" Expression
/// ```
fn parse_type_annotation(parser: &mut Parser<'_>) -> Result<Expression> {
    parser.consume(TokenKind::Colon)?;
    parser.reflect(parse_expression)
}

// @Task generalize, move somewhere else
// @Note this is currently only used for let-declarations and I don't know if
// it works in other parsers
fn parse_possibly_indented_expression_followed_by_line_break(
    parser: &mut Parser<'_>,
) -> Result<Expression> {
    if parser.consumed(TokenKind::LineBreak) {
        parser.consume(TokenKind::Indentation)?;
        let expression = parse_expression_followed_by_line_break(parser)?;
        parser.consume(TokenKind::Dedentation)?;
        Ok(expression)
    } else {
        parse_expression_followed_by_line_break(parser)
    }
}

// @Note temporary, generalize
fn parse_expression_followed_by_line_break(parser: &mut Parser<'_>) -> Result<Expression> {
    let expression = parse_expression(parser)?;
    parser.consume(TokenKind::LineBreak)?;
    Ok(expression)
}

fn parse_bracketed<K>(
    parser: &mut Parser<'_>,
    subparser: fn(&mut Parser<'_>) -> Result<Spanned<K>>,
) -> Result<Spanned<K>> {
    let span_of_opening_bracket = parser.consume(TokenKind::OpeningRoundBracket)?.span;
    // @Question reflect necessary?
    let mut inner = parser.reflect(subparser)?;
    let span_of_closing_bracket = parser.consume(TokenKind::ClosingRoundBracket)?.span;
    inner.span = span_of_opening_bracket.merge(span_of_closing_bracket);
    Ok(inner)
}

fn consume_explicitness_symbol(parser: &mut Parser<'_>) -> Explicitness {
    match parser.consume(TokenKind::VerticalBar) {
        Ok(_token) => {
            // @Note there might be false positives (through arbitrary look-ahead)
            // @Task let this function have access to the source map #ParserRefactor
            Diagnostic::new(Level::Warning, "implicitness markers are currently ignored")
                // .with_span(token.span)
                .emit(None);
            Explicitness::Implicit
        }
        Err(_) => Explicitness::Explicit,
    }
}

#[derive(Debug, Clone, Eq)]
pub struct Identifier {
    pub atom: crate::Atom,
    pub span: Span,
}

impl Identifier {
    pub fn new(atom: crate::Atom, span: Span) -> Self {
        Self { atom, span }
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.atom == other.atom
    }
}

impl std::hash::Hash for Identifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.atom.hash(state);
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.atom)
    }
}

/// The explicitness of a parameter or argument.
///
/// In the context of parameters, this specifies whether in an application, the corresponding argument has
/// to be passed explicitly or should be infered, i.e. the parameter is [Explicitness::Implicit].
///
/// In the context of applications, [Explicitness::Implicit] means that the argument is passed explicitly
/// even though the parameter is marked implicit.
#[derive(Clone, Copy, Debug)]
pub enum Explicitness {
    Implicit,
    Explicit,
}

impl fmt::Display for Explicitness {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Implicit => f.write_str("|"),
            Self::Explicit => f.write_str(""),
        }
    }
}

pub type Result<T, E = Diagnostic> = std::result::Result<T, E>;
