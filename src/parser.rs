//! The parser.
//!
//! I *think* it can be classified as a top-down recursive-descent parser with arbitrary look-ahead.
//!
//! ## Issues
//!
//! * crude error locations
//! * cannot really handle optional indentation and some legal EOIs
//! * when prefix-parsing, we check twice e.g. in:
//!   * [parse_declaration]
//!   * [parse_expression]
//!   * [expression::parse_case_analysis]

mod context;
mod error;
mod identifier;

use freestanding::freestanding;
use std::fmt;

use crate::lexer::{self, TokenKind};
use crate::span::Span;

pub use context::Parser;
pub use error::{Error, ErrorKind};
pub use identifier::Identifier;

use error::Result;

pub use declaration::{parse_declaration, Declaration};

/// The part of the parser concerned with declarations.
pub mod declaration {
    use super::*;

    /// The syntax node of a declaration.
    // @Question will the spans on them even get used (for error messages)
    // @Note in most cases, only the span of the binder or the type annotations contained within
    // will be used, I guess
    // @Task use #[common { span: Span }] once defined
    #[freestanding]
    #[streamline(Box)]
    #[derive(Debug)] // @Temporary
    pub enum Declaration {
        /// The syntax node of a value declaration.
        Value {
            binder: Identifier,
            parameters: AnnotatedParameters,
            type_annotation: Expression,
            expression: Expression,
            span: Span,
        },
        /// The syntax node of a data declaration.
        Data {
            binder: Identifier,
            parameters: AnnotatedParameters,
            type_annotation: Expression,
            constructors: Vec<Constructor>,
            span: Span,
        },
        /// The syntax node of a module declaration.
        Module {
            declarations: Vec<Declaration>,
            span: Span,
        },
        /// The syntax node of a use declaration.
        // @Task
        Use { span: Span },
        /// The syntax node of a foreign declaration.
        Foreign {
            binder: Identifier,
            parameters: AnnotatedParameters,
            type_annotation: Expression,
            span: Span,
        },
    }

    const _: () = assert!(std::mem::size_of::<Declaration>() == 16);

    impl Declaration {
        pub fn span(&self) -> Span {
            match self {
                Self::Value(r#let) => r#let.span,
                Self::Data(data) => data.span,
                Self::Module(module) => module.span,
                Self::Use(r#use) => r#use.span,
                Self::Foreign(foreign) => foreign.span,
            }
        }
    }

    /// Parse a declaration.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Declaration ::= Value_Declaration | Data_Declaration | Foreign_Declaration
    /// ```
    pub fn parse_declaration(parser: &mut Parser<'_>) -> Result<Declaration> {
        let token = parser.current_token()?;

        Ok(match token.kind() {
            TokenKind::Identifier => Declaration::Value(Box::new(parse_value_declaration(parser)?)),
            TokenKind::Data => Declaration::Data(Box::new(parse_data_declaration(parser)?)),
            TokenKind::Foreign => {
                Declaration::Foreign(Box::new(parse_foreign_declaration(parser)?))
            }
            kind => {
                return Err(Error {
                    span: token.span,
                    kind: ErrorKind::ExpectedDeclaration { actual: kind },
                })
            }
        })
    }

    /// Parse a value declaration.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Value_Declaration ::= Identifier Annotated_Parameters Type_Annotation "="
    ///     Possibly_Indented_Expression_Followed_By_Line_Break
    /// ```
    fn parse_value_declaration(parser: &mut Parser<'_>) -> Result<Value> {
        let binder = parser.consume_identifier()?;
        let parameters = parse_annotated_parameters(parser)?;

        let type_annotation = parse_type_annotation(parser)?;
        parser.consume(TokenKind::Equals)?;
        let expression = parse_possibly_indented_expression_followed_by_line_break(parser)?;

        Ok(Value {
            span: binder.span.merge(expression.span()),
            binder,
            parameters,
            type_annotation,
            expression,
        })
    }

    // @Task grammar rule
    pub fn parse_data_declaration(parser: &mut Parser<'_>) -> Result<Data> {
        let span_of_keyword = parser.consume(TokenKind::Data)?.span;
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

        Ok(Data {
            span: span_of_keyword.merge(
                constructors
                    .last()
                    .map(|constructor| constructor.span)
                    .unwrap_or(span_of_equals),
            ),
            binder,
            parameters,
            type_annotation,
            constructors,
        })
    }

    // @Task
    fn _parse_module_declaration() -> Result<Module> {
        unimplemented!()
    }

    // @Temporary
    pub fn parse_file_module_no_header(parser: &mut Parser<'_>) -> Result<Module> {
        let mut declarations = Vec::<Declaration>::new();

        loop {
            // skip empty lines
            if parser.consumed(TokenKind::LineBreak) {
                continue;
            }

            if parser.at_the_end_of_input() {
                break Ok(Module {
                    // @Temporary span handling because this function itself is temporary @Bug Span::dummy()
                    span: (|| {
                        Some(
                            declarations
                                .first()?
                                .span()
                                .merge(declarations.last()?.span()),
                        )
                    })()
                    .unwrap_or(Span::dummy()),
                    declarations,
                });
            }

            declarations.push(parse_declaration(parser)?);
        }
    }

    // @Task
    fn _parse_use_declaration() -> Result<Use> {
        unimplemented!()
    }

    /// Parse a foreign declaration.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Foreign_Declaration ::= "foreign" Identifier Annotated_Parameters Type_Annotation Line_Break
    /// ```
    fn parse_foreign_declaration(parser: &mut Parser<'_>) -> Result<Foreign> {
        let span_of_keyword = parser.consume(TokenKind::Foreign)?.span;
        let binder = parser.consume_identifier()?;
        let parameters = parse_annotated_parameters(parser)?;
        let type_annotation = parse_type_annotation(parser)?;
        // @Task allow EOI as an alternative
        parser.consume(TokenKind::LineBreak)?;

        Ok(Foreign {
            span: span_of_keyword.merge(type_annotation.span()),
            binder,
            parameters,
            type_annotation,
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

    // @Task very future: unnameable constructor `_`
    fn parse_constructor(parser: &mut Parser<'_>) -> Result<Constructor> {
        let binder = parser.consume_identifier()?;
        let parameters = parse_annotated_parameters(parser)?;
        let type_annotation = parse_type_annotation(parser)?;
        // @Task allow EOI as an alternative
        parser.consume(TokenKind::LineBreak)?;

        Ok(Constructor {
            span: binder.span.merge(type_annotation.span()),
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

    // @Task add span information @Question or shouldn't we?
    // @Task inline pattern-match
    #[derive(Debug)]
    pub struct AnnotatedParameterGroup {
        pub parameters: Vec<Identifier>, // @Task replace with OneOrMore
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

pub use expression::{parse_expression, Expression, Path};

/// The part of the parser concerned with expressions.
pub mod expression {
    use super::*;

    /// The syntax node of an expression.
    #[freestanding]
    #[streamline(Box)]
    // #[common { span: Span }]
    #[derive(Debug, Clone)]
    pub enum Expression {
        /// The syntax node of pi-type literals.
        PiTypeLiteral {
            binder: Option<Identifier>,
            parameter: Expression,
            expression: Expression,
            explicitness: Explicitness,
            span: Span,
        },
        /// The syntax node of function application.
        Application {
            callee: Expression,
            argument: Expression,
            explicitness: Explicitness,
            span: Span,
        },
        TypeLiteral {
            span: Span,
        },
        NatTypeLiteral {
            span: Span,
        },
        NatLiteral {
            value: lexer::Nat,
            span: Span,
        },
        TextTypeLiteral {
            span: Span,
        },
        TextLiteral {
            value: String,
            span: Span,
        },
        // @Task make it able to parse complex paths
        // @Update @Task replace Identifier, have the span at top-level, use atom instead
        Path {
            // @Task in the future: segments: Vec<Identifier>,
            segments: Identifier,
            span: Span,
        },
        /// The syntax node of a lambda literal expression.
        LambdaLiteral {
            parameters: Parameters,
            body_type_annotation: Option<Expression>,
            body: Expression,
            span: Span,
        },
        /// The syntax-node of a let-in expression.
        // @Task rename the field `expression` to something more descriptive
        LetIn {
            binder: Identifier,
            parameters: Parameters,
            type_annotation: Option<Expression>,
            // @Task improve upon naming
            expression: Expression,
            scope: Expression,
            span: Span,
        },
        // @Task
        UseIn {
            span: Span,
        },
        CaseAnalysis {
            expression: Expression,
            cases: Vec<CaseAnalysisCaseGroup>,
            span: Span,
        },
    }

    /// Assertion that the size of [Expression] does not exceed 16 bytes.
    ///
    /// This type is used *very* often and the size of the different variants varies *greatly*.
    /// Thus, everyone of them is boxed.
    const _: () = assert!(std::mem::size_of::<Expression>() == 16);

    // @Note should be generated automatically (by freestanding in the future)
    impl Spanning for Expression {
        /// Returns the span of an expression.
        fn span(&self) -> Span {
            match self {
                Self::PiTypeLiteral(literal) => literal.span,
                Self::Application(application) => application.span,
                Self::TypeLiteral(literal) => literal.span,
                Self::NatTypeLiteral(literal) => literal.span,
                Self::NatLiteral(literal) => literal.span,
                Self::TextTypeLiteral(literal) => literal.span,
                Self::TextLiteral(literal) => literal.span,
                Self::Path(path) => path.span,
                Self::LambdaLiteral(literal) => literal.span,
                Self::LetIn(let_in) => let_in.span,
                Self::UseIn(use_in) => use_in.span,
                Self::CaseAnalysis(case_analysis) => case_analysis.span,
            }
        }

        /// Returns a unique reference to the span of a node.
        ///
        /// Allows adjusting the span of a node after the fact which is quite handy in certain situations.
        fn span_mut(&mut self) -> &mut Span {
            match self {
                Self::PiTypeLiteral(literal) => &mut literal.span,
                Self::Application(application) => &mut application.span,
                Self::TypeLiteral(literal) => &mut literal.span,
                Self::NatTypeLiteral(literal) => &mut literal.span,
                Self::NatLiteral(literal) => &mut literal.span,
                Self::TextTypeLiteral(literal) => &mut literal.span,
                Self::TextLiteral(literal) => &mut literal.span,
                Self::Path(path) => &mut path.span,
                Self::LambdaLiteral(literal) => &mut literal.span,
                Self::LetIn(let_in) => &mut let_in.span,
                Self::UseIn(use_in) => &mut use_in.span,
                Self::CaseAnalysis(case_analysis) => &mut case_analysis.span,
            }
        }
    }

    /// Parse an expression.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Expression ::= Let_In | Lambda_Literal | Case_Analysis | Pi_Literal_Or_Lower
    /// ```
    pub fn parse_expression(parser: &mut Parser<'_>) -> Result<Expression> {
        let token = parser.current_token()?;

        Ok(match token.kind() {
            TokenKind::Let => Expression::LetIn(Box::new(parse_let_in(parser)?)),
            TokenKind::Backslash => {
                Expression::LambdaLiteral(Box::new(parse_lambda_literal(parser)?))
            }
            TokenKind::Case => Expression::CaseAnalysis(Box::new(parse_case_analysis(parser)?)),
            _ => return parse_pi_type_literal_or_lower(parser),
        })
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
                    .map(|parameter| (parameter.span(), Explicitness::Explicit, None, parameter))
            })?;

        Ok(match parser.consume(TokenKind::ThinArrow) {
            Ok(_) => {
                let expression = parser.reflect(parse_pi_type_literal_or_lower)?;

                Expression::PiTypeLiteral(Box::new(expression::PiTypeLiteral {
                    span: span_at_the_beginning.merge(expression.span()),
                    expression,
                    binder,
                    parameter,
                    explicitness,
                }))
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
            expression = Expression::Application(Box::new(expression::Application {
                span: expression.span().merge(argument.span()),
                callee: expression,
                argument,
                explicitness,
            }));
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
            .map(|path| Expression::Path(Box::new(path)))
            .or_else(|_: Error| {
                Ok(Expression::TypeLiteral(Box::new(parse_type_literal(
                    parser,
                )?)))
            })
            .or_else(|_: Error| {
                Ok(Expression::NatTypeLiteral(Box::new(
                    parse_nat_type_literal(parser)?,
                )))
            })
            .or_else(|_: Error| Ok(Expression::NatLiteral(Box::new(parse_nat_literal(parser)?))))
            .or_else(|_: Error| {
                Ok(Expression::TextTypeLiteral(Box::new(
                    parse_text_type_literal(parser)?,
                )))
            })
            .or_else(|_: Error| {
                Ok(Expression::TextLiteral(Box::new(parse_text_literal(
                    parser,
                )?)))
            })
            .or_else(|_: Error| parse_bracketed(parser, parse_expression))
    }

    // Type_Literal ::= %type literal%
    fn parse_type_literal(parser: &mut Parser<'_>) -> Result<TypeLiteral> {
        parser
            .consume(TokenKind::Type)
            .map(|token| TypeLiteral { span: token.span })
    }

    fn parse_nat_type_literal(parser: &mut Parser<'_>) -> Result<NatTypeLiteral> {
        parser
            .consume(TokenKind::Nat)
            .map(|token| NatTypeLiteral { span: token.span })
    }

    fn parse_nat_literal(parser: &mut Parser<'_>) -> Result<NatLiteral> {
        // @Note ugly match+unreachable, directly relates to consume/consume_identifier-issue mentioned above
        parser
            .consume(TokenKind::NatLiteral)
            .map(|token| NatLiteral {
                value: match token.inner {
                    lexer::Token::NatLiteral(value) => value,
                    _ => unreachable!(),
                },
                span: token.span,
            })
    }

    fn parse_text_type_literal(parser: &mut Parser<'_>) -> Result<TextTypeLiteral> {
        parser
            .consume(TokenKind::Text)
            .map(|token| TextTypeLiteral { span: token.span })
    }

    fn parse_text_literal(parser: &mut Parser<'_>) -> Result<TextLiteral> {
        // @Note ugly match+unreachable, directly relates to consume/consume_identifier-issue mentioned above
        parser
            .consume(TokenKind::TextLiteral)
            .map(|token| TextLiteral {
                value: match token.inner {
                    lexer::Token::TextLiteral(value) => value,
                    _ => unreachable!(),
                },
                span: token.span,
            })
    }

    // Path ::= %identifier%
    fn parse_path(parser: &mut Parser<'_>) -> Result<Path> {
        parser.consume_identifier().map(|identifier| Path {
            span: identifier.span,
            segments: identifier,
        })
    }

    /// Parse a lambda literal expression.a
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Lambda_Literal ::= "\" Parameters Type_Annotation? "=>" Expression
    /// ```
    fn parse_lambda_literal(parser: &mut Parser<'_>) -> Result<LambdaLiteral> {
        let span_of_backslash = parser.consume(TokenKind::Backslash)?.span;
        let parameters = parser.reflect(parse_parameters)?;
        let body_type_annotation = parser.reflect(parse_type_annotation).ok();
        parser.consume(TokenKind::WideArrow)?;
        let body = parser.reflect(parse_expression)?;

        Ok(LambdaLiteral {
            parameters,
            body_type_annotation,
            span: span_of_backslash.merge(body.span()),
            body,
        })
    }

    /// Parse an let-in expression.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Let_In ::= "'let" Identifier Parameters Type_Annotation? "=" Expression "'in" Expression
    /// ```
    fn parse_let_in(parser: &mut Parser<'_>) -> Result<LetIn> {
        let let_keyword = parser.consume(TokenKind::Let)?;
        let binder = parser.consume_identifier()?;
        let parameters = parser.reflect(parse_parameters)?;
        let type_annotation = parser.reflect(parse_type_annotation).ok();
        parser.consume(TokenKind::Equals)?;
        let expression = parser.reflect(parse_expression)?;
        parser.consume(TokenKind::In)?;
        let scope = parser.reflect(parse_expression)?;
        Ok(LetIn {
            binder,
            parameters,
            type_annotation,
            expression,
            span: let_keyword.span.merge(scope.span()),
            scope,
        })
    }

    // @Note grammar is out-of-sync in respect to line breaks
    // Case_Analysis ::= "'case" Expression Line_Break Case_Analysis_Case_Group*
    fn parse_case_analysis(parser: &mut Parser<'_>) -> Result<CaseAnalysis> {
        let span_of_keyword = parser.consume(TokenKind::Case)?.span;
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

        Ok(CaseAnalysis {
            span: span_of_keyword.merge(
                cases
                    .last()
                    .map(|case_group| case_group.expression.span())
                    .unwrap_or_else(|| expression.span()),
            ),
            expression,
            cases,
        })
    }

    #[derive(Debug, Clone)]
    pub struct CaseAnalysisCaseGroup {
        pub patterns: Vec<Pattern>,
        pub expression: Expression,
    }

    // Case_Analyis_Case_Group ::= ("of" Pattern)+ "=>" Expression
    fn parse_case_analysis_case_group(parser: &mut Parser<'_>) -> Result<CaseAnalysisCaseGroup> {
        let mut patterns = Vec::new();

        parser.consume(TokenKind::Of)?;
        patterns.push(parse_pattern(parser)?);

        loop {
            if parser.consumed(TokenKind::WideArrow) {
                break;
            }

            parser.consume(TokenKind::Of)?;
            patterns.push(parse_pattern(parser)?);
        }

        Ok(CaseAnalysisCaseGroup {
            patterns,
            expression: parse_expression(parser)?,
        })
    }

    // @Task box variants to reduce size of nat patterns
    #[derive(Debug, Clone)]
    pub enum Pattern {
        NatLiteral(NatLiteral),
        Path {
            path: Path,
            type_annotation: Option<Expression>,
            span: Span,
        },
        Application {
            callee: Box<Pattern>,
            argument: Box<Pattern>,
            span: Span,
        },
    }

    impl Spanning for Pattern {
        fn span(&self) -> Span {
            match self {
                Self::NatLiteral(literal) => literal.span,
                Self::Path { path, .. } => path.span,
                Self::Application { span, .. } => *span,
            }
        }

        fn span_mut(&mut self) -> &mut Span {
            match self {
                Self::NatLiteral(literal) => &mut literal.span,
                Self::Path { path, .. } => &mut path.span,
                Self::Application { ref mut span, .. } => span,
            }
        }
    }

    // Pattern ::= Lower_Pattern+
    // @Task verfiy
    fn parse_pattern(parser: &mut Parser<'_>) -> Result<Pattern> {
        let mut callee = parser.reflect(parse_lower_pattern)?;

        while let Ok(argument) = parser.reflect(parse_lower_pattern) {
            callee = Pattern::Application {
                span: callee.span().merge(argument.span()),
                callee: Box::new(callee),
                argument: Box::new(argument),
            };
        }
        Ok(callee)
    }

    // @Task Lower_Pattern ::= Nat_Literal | "(" Path Type_Annotation? ")" | Path | "(" Pattern ")"
    // @Task verify
    // @Bug or_else(_) produces bad error messages
    fn parse_lower_pattern(parser: &mut Parser<'_>) -> Result<Pattern> {
        parse_nat_literal(parser)
            .map(Pattern::NatLiteral)
            .or_else(|_| {
                let (span, path, type_annotation) =
                    if let Ok(opening_bracket) = parser.consume(TokenKind::OpeningRoundBracket) {
                        let path = parser.reflect(parse_path)?;
                        let type_annotation = parser.reflect(parse_type_annotation)?;
                        let span_of_closing_bracket =
                            parser.consume(TokenKind::ClosingRoundBracket)?.span;

                        (
                            opening_bracket.span.merge(span_of_closing_bracket),
                            path,
                            Some(type_annotation),
                        )
                    } else {
                        let path = parser.reflect(parse_path)?;
                        (path.span, path, None)
                    };

                Ok(Pattern::Path {
                    path,
                    span,
                    type_annotation,
                })
            })
            .or_else(|_: Error| parse_bracketed(parser, parse_pattern))
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

    // @Question add span information?
    // @Task inline pattern-match
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

// @Note temporary generalize
fn parse_expression_followed_by_line_break(parser: &mut Parser<'_>) -> Result<Expression> {
    let expression = parse_expression(parser)?;
    parser.consume(TokenKind::LineBreak)?;
    Ok(expression)
}

fn parse_bracketed<T: Spanning>(
    parser: &mut Parser<'_>,
    subparser: fn(&mut Parser<'_>) -> Result<T>,
) -> Result<T> {
    let span_of_opening_bracket = parser.consume(TokenKind::OpeningRoundBracket)?.span;
    // @Question reflect necessary?
    let mut inner = parser.reflect(subparser)?;
    let span_of_closing_bracket = parser.consume(TokenKind::ClosingRoundBracket)?.span;
    *inner.span_mut() = span_of_opening_bracket.merge(span_of_closing_bracket);
    Ok(inner)
}

fn consume_explicitness_symbol(parser: &mut Parser<'_>) -> Explicitness {
    match parser.consume(TokenKind::VerticalBar) {
        Ok(_) => Explicitness::Implicit,
        Err(_) => Explicitness::Explicit,
    }
}

trait Spanning {
    fn span(&self) -> Span;
    fn span_mut(&mut self) -> &mut Span;
}

/// The explicitness of a parameter or argument.
///
/// In the parser of parameters, this specifies whether in an application, the corresponding argument has
/// to be passed explicitly or should be infered, i.e. the parameter is [Explicitness::Implicit].
///
/// In the parser of applications, [Explicitness::Implicit] means that the argument is passed explicitly
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
