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

use crate::error::Span;
use crate::lexer::{self, TokenKind};

pub use context::Context;
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
    pub fn parse_declaration(context: &mut Context<'_>) -> Result<Declaration> {
        let token = context.current_token()?;

        Ok(match token.kind() {
            TokenKind::Identifier => {
                Declaration::Value(Box::new(parse_value_declaration(context)?))
            }
            TokenKind::Data => Declaration::Data(Box::new(parse_data_declaration(context)?)),
            TokenKind::Foreign => Declaration::Foreign(Box::new(parse_foreign_declaration(context)?)),
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
    fn parse_value_declaration(context: &mut Context<'_>) -> Result<Value> {
        let binder = context.consume_identifier()?;
        let parameters = parse_annotated_parameters(context)?;

        let type_annotation = parse_type_annotation(context)?;
        context.consume(TokenKind::Equals)?;
        let expression = parse_possibly_indented_expression_followed_by_line_break(context)?;

        Ok(Value {
            span: binder.span.merge(expression.span()),
            binder,
            parameters,
            type_annotation,
            expression,
        })
    }

    // @Task grammar rule
    pub fn parse_data_declaration(context: &mut Context<'_>) -> Result<Data> {
        let span_of_keyword = context.consume(TokenKind::Data)?.span;
        let binder = context.consume_identifier()?;
        let parameters = parse_annotated_parameters(context)?;
        let type_annotation = parse_type_annotation(context)?;
        let span_of_equals = context.consume(TokenKind::Equals)?.span;
        context.consume(TokenKind::LineBreak)?;

        let mut constructors = Vec::new();

        if context.consumed(TokenKind::Indentation) {
            // @Bug produces bad error messages
            while let Ok(constructor) = context.reflect(parse_constructor) {
                constructors.push(constructor);
            }
            // @Question or EOI?
            context.consume(TokenKind::Dedentation)?;
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
    pub fn parse_file_module_no_header(context: &mut Context<'_>) -> Result<Module> {
        let mut declarations = Vec::<Declaration>::new();

        loop {
            // skip empty lines
            if context.consumed(TokenKind::LineBreak) {
                continue;
            }

            if context.at_the_end_of_input() {
                break Ok(Module {
                    // @Temporary span handling because this function itself is temporary
                    // @Bug Span::new(0, 0)
                    span: (|| {
                        Some(
                            declarations
                                .first()?
                                .span()
                                .merge(declarations.last()?.span()),
                        )
                    })()
                    .unwrap_or(Span::new(0, 0)),
                    declarations,
                });
            }

            declarations.push(parse_declaration(context)?);
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
    fn parse_foreign_declaration(context: &mut Context<'_>) -> Result<Foreign> {
        let span_of_keyword = context.consume(TokenKind::Foreign)?.span;
        let binder = context.consume_identifier()?;
        let parameters = parse_annotated_parameters(context)?;
        let type_annotation = parse_type_annotation(context)?;
        // @Task allow EOI as an alternative
        context.consume(TokenKind::LineBreak)?;

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
    fn parse_constructor(context: &mut Context<'_>) -> Result<Constructor> {
        let binder = context.consume_identifier()?;
        let parameters = parse_annotated_parameters(context)?;
        let type_annotation = parse_type_annotation(context)?;
        // @Task allow EOI as an alternative
        context.consume(TokenKind::LineBreak)?;

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
    fn parse_annotated_parameters(context: &mut Context<'_>) -> Result<AnnotatedParameters> {
        let mut parameters = Vec::new();

        while context.current(TokenKind::OpeningRoundBracket) {
            parameters.push(context.reflect(parse_annotated_parameter_group)?);
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
    fn parse_annotated_parameter_group(
        context: &mut Context<'_>,
    ) -> Result<AnnotatedParameterGroup> {
        context.consume(TokenKind::OpeningRoundBracket)?;
        let explicitness = consume_explicitness_symbol(context);
        let mut parameters = Vec::new();

        parameters.push(context.consume_identifier()?);

        // @Note @Bug probably drops errors as well @Note shouldn't: consume
        // reflects by itself @Task verify
        // @Bug produces bad error messages
        while let Ok(parameter) = context.consume_identifier() {
            parameters.push(parameter);
        }

        let type_annotation = context.reflect(parse_type_annotation)?;
        context.consume(TokenKind::ClosingRoundBracket)?;

        Ok(AnnotatedParameterGroup {
            parameters,
            type_annotation,
            explicitness,
        })
    }
}

pub use expression::{parse_expression, Expression};

/// The part of the parser concerned with expressions.
pub mod expression {
    use super::*;

    /// The syntax node of an expression.
    #[freestanding]
    #[streamline(Box)]
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
        // @Task make it able to parse complex paths
        Path {
            inner: Identifier,
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

    impl Expression {
        /// Returns the span of an expression.
        pub fn span(&self) -> Span {
            match self {
                Self::PiTypeLiteral(literal) => literal.span,
                Self::Application(application) => application.span,
                Self::TypeLiteral(literal) => literal.span,
                Self::NatTypeLiteral(literal) => literal.span,
                Self::NatLiteral(literal) => literal.span,
                Self::Path(path) => path.inner.span,
                Self::LambdaLiteral(literal) => literal.span,
                Self::LetIn(let_in) => let_in.span,
                Self::UseIn(use_in) => use_in.span,
                Self::CaseAnalysis(case_analysis) => case_analysis.span,
            }
        }

        /// Returns a unique reference to the span of a node.
        ///
        /// Allows adjusting the span of a node after the fact which is quite handy in certain situations.
        pub fn span_mut(&mut self) -> &mut Span {
            match self {
                Self::PiTypeLiteral(literal) => &mut literal.span,
                Self::Application(application) => &mut application.span,
                Self::TypeLiteral(literal) => &mut literal.span,
                Self::NatTypeLiteral(literal) => &mut literal.span,
                Self::NatLiteral(literal) => &mut literal.span,
                Self::Path(path) => &mut path.inner.span,
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
    pub fn parse_expression(context: &mut Context<'_>) -> Result<Expression> {
        let token = context.current_token()?;

        Ok(match token.kind() {
            TokenKind::Let => Expression::LetIn(Box::new(parse_let_in(context)?)),
            TokenKind::Backslash => {
                Expression::LambdaLiteral(Box::new(parse_lambda_literal(context)?))
            }
            TokenKind::Case => Expression::CaseAnalysis(Box::new(parse_case_analysis(context)?)),
            _ => return parse_pi_type_literal_or_lower(context),
        })
    }

    /// Parse a pi-type literal or a lower expression.
    // @Task update grammar
    // Pi_Literal_Or_Lower %right% ::= Application_Or_Lower (-> Pi_Literal_Or_Lower)*
    fn parse_pi_type_literal_or_lower(context: &mut Context<'_>) -> Result<Expression> {
        let (span_at_the_beginning, explicitness, binder, parameter) = context
            .reflect(|context| {
                let span_of_opening_round_bracket =
                    context.consume(TokenKind::OpeningRoundBracket)?.span;

                let explicitness = consume_explicitness_symbol(context);
                let binder = context.consume_identifier()?;
                let parameter = parse_type_annotation(context)?;

                context.consume(TokenKind::ClosingRoundBracket)?;

                Ok((
                    span_of_opening_round_bracket,
                    explicitness,
                    Some(binder),
                    parameter,
                ))
            })
            .or_else(|_| {
                // @Question do we need reflect here?
                context
                    .reflect(parse_application_or_lower)
                    .map(|parameter| (parameter.span(), Explicitness::Explicit, None, parameter))
            })?;

        Ok(match context.consume(TokenKind::ThinArrow) {
            Ok(_) => {
                let expression = context.reflect(parse_pi_type_literal_or_lower)?;

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
    fn parse_application_or_lower(context: &mut Context<'_>) -> Result<Expression> {
        let mut expression = context.reflect(parse_lower_expression)?;
        while let Ok((argument, explicitness)) = context
            .reflect(|context| Ok((parse_lower_expression(context)?, Explicitness::Explicit)))
            .or_else(|_| -> Result<_> {
                context.consume(TokenKind::OpeningRoundBracket)?;
                context.consume(TokenKind::VerticalBar)?;
                let expression = parse_expression(context)?;
                context.consume(TokenKind::ClosingRoundBracket)?;
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
    ///     Type_Literal | Nat_Literal | Identifier | Hole | Bracketed_Expression
    /// ```
    fn parse_lower_expression(context: &mut Context<'_>) -> Result<Expression> {
        parse_type_literal(context)
            .map(|type_literal| Expression::TypeLiteral(Box::new(type_literal)))
            .or_else(|_| {
                Ok(Expression::NatTypeLiteral(Box::new(
                    parse_nat_type_literal(context)?,
                )))
            })
            .or_else(|_: Error| {
                Ok(Expression::NatLiteral(Box::new(parse_nat_literal(
                    context,
                )?)))
            })
            .or_else(|_: Error| Ok(Expression::Path(Box::new(parse_path(context)?))))
            .or_else(|_: Error| parse_bracketed_expression(context))
    }

    // Bracketed_Expression ::= "(" Expression ")"
    fn parse_bracketed_expression(context: &mut Context<'_>) -> Result<Expression> {
        let span_of_opening_bracket = context.consume(TokenKind::OpeningRoundBracket)?.span;
        let mut expression = context.reflect(parse_expression)?;
        let span_of_closing_bracket = context.consume(TokenKind::ClosingRoundBracket)?.span;
        *expression.span_mut() = span_of_opening_bracket.merge(span_of_closing_bracket);
        Ok(expression)
    }

    // Type_Literal ::= %type literal%
    fn parse_type_literal(context: &mut Context<'_>) -> Result<TypeLiteral> {
        context
            .consume(TokenKind::Type)
            .map(|token| TypeLiteral { span: token.span })
    }

    fn parse_nat_type_literal(context: &mut Context<'_>) -> Result<NatTypeLiteral> {
        context
            .consume(TokenKind::Nat)
            .map(|token| NatTypeLiteral { span: token.span })
    }

    fn parse_nat_literal(context: &mut Context<'_>) -> Result<NatLiteral> {
        // @Note ugly match+unreachable, directly relates to consume/consume_identifier-issue mentioned above
        context
            .consume(TokenKind::NatLiteral)
            .map(|token| NatLiteral {
                value: match token.inner {
                    lexer::Token::NatLiteral(value) => value,
                    _ => unreachable!(),
                },
                span: token.span,
            })
    }

    // Identifier ::= %identifier%
    fn parse_path(context: &mut Context<'_>) -> Result<Path> {
        context.consume_identifier().map(|inner| Path { inner })
    }

    /// Parse a lambda literal expression.
    ///
    /// ## Grammar
    ///
    /// ```text
    /// Lambda_Literal ::= "\" Parameters Type_Annotation? "=>" Expression
    /// ```
    fn parse_lambda_literal(context: &mut Context<'_>) -> Result<LambdaLiteral> {
        let span_of_backslash = context.consume(TokenKind::Backslash)?.span;
        let parameters = context.reflect(parse_parameters)?;
        let body_type_annotation = context.reflect(parse_type_annotation).ok();
        context.consume(TokenKind::WideArrow)?;
        let body = context.reflect(parse_expression)?;

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
    fn parse_let_in(context: &mut Context<'_>) -> Result<LetIn> {
        let let_keyword = context.consume(TokenKind::Let)?;
        let binder = context.consume_identifier()?;
        let parameters = context.reflect(parse_parameters)?;
        let type_annotation = context.reflect(parse_type_annotation).ok();
        context.consume(TokenKind::Equals)?;
        let expression = context.reflect(parse_expression)?;
        context.consume(TokenKind::In)?;
        let scope = context.reflect(parse_expression)?;
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
    fn parse_case_analysis(context: &mut Context<'_>) -> Result<CaseAnalysis> {
        let span_of_keyword = context.consume(TokenKind::Case)?.span;
        let expression = parse_expression_followed_by_line_break(context)?;

        let mut cases = Vec::new();

        while context.current(TokenKind::Of) {
            cases.push(parse_case_analysis_case_group(context)?);

            // only consume a line break if the token after the line break is the keyword "of" to
            // achieve parsing line breaks as joins/separators between each case group
            if context.current(TokenKind::LineBreak) && context.succeeding(TokenKind::Of) {
                context.advance();
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
    fn parse_case_analysis_case_group(context: &mut Context<'_>) -> Result<CaseAnalysisCaseGroup> {
        let mut patterns = Vec::new();

        context.consume(TokenKind::Of)?;
        patterns.push(parse_pattern(context)?);

        loop {
            if context.consumed(TokenKind::Equals) {
                break;
            }

            context.consume(TokenKind::Of)?;
            patterns.push(parse_pattern(context)?);
        }

        Ok(CaseAnalysisCaseGroup {
            patterns,
            expression: parse_expression(context)?,
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

    impl Pattern {
        fn span(&self) -> Span {
            match self {
                Self::NatLiteral(literal) => literal.span,
                Self::Path { path, .. } => path.inner.span,
                Self::Application { span, .. } => *span,
            }
        }

        fn span_mut(&mut self) -> &mut Span {
            match self {
                Self::NatLiteral(literal) => &mut literal.span,
                Self::Path { path, .. } => &mut path.inner.span,
                Self::Application { ref mut span, .. } => span,
            }
        }
    }

    // Pattern ::= Lower_Pattern+
    // @Task verfiy
    fn parse_pattern(context: &mut Context<'_>) -> Result<Pattern> {
        let mut callee = context.reflect(parse_lower_pattern)?;

        while let Ok(argument) = context.reflect(parse_lower_pattern) {
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
    fn parse_lower_pattern(context: &mut Context<'_>) -> Result<Pattern> {
        parse_nat_literal(context)
            .map(|literal| Pattern::NatLiteral(literal))
            .or_else(|_| {
                let (span, path, type_annotation) =
                    if let Ok(opening_bracket) = context.consume(TokenKind::OpeningRoundBracket) {
                        let path = context.reflect(parse_path)?;
                        let type_annotation = context.reflect(parse_type_annotation)?;
                        let span_of_closing_bracket =
                            context.consume(TokenKind::ClosingRoundBracket)?.span;

                        (
                            opening_bracket.span.merge(span_of_closing_bracket),
                            path,
                            Some(type_annotation),
                        )
                    } else {
                        let path = context.reflect(parse_path)?;
                        (path.inner.span, path, None)
                    };

                Ok(Pattern::Path {
                    path,
                    span,
                    type_annotation,
                })
            })
            // @Bug or_else(_) produces bad error messages
            .or_else(|_: Error| {
                let span_of_opening_bracket = context.consume(TokenKind::OpeningRoundBracket)?.span;
                let mut pattern = parse_pattern(context)?;

                let span_of_closing_bracket = context.consume(TokenKind::ClosingRoundBracket)?.span;

                *pattern.span_mut() = span_of_closing_bracket.merge(span_of_opening_bracket);

                Ok(pattern)
            })
    }

    pub type Parameters = Vec<ParameterGroup>;

    // Parameters ::= Parameter_Group*
    fn parse_parameters(context: &mut Context<'_>) -> Result<Parameters> {
        let mut parameters = Vec::new();

        // @Bug produces bad error messages
        while let Ok(parameter_group) = context.reflect(parse_parameter_group) {
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
    fn parse_parameter_group(context: &mut Context<'_>) -> Result<ParameterGroup> {
        fn parse_optionally_annotated_parameter_group(
            context: &mut Context<'_>,
        ) -> Result<ParameterGroup> {
            context.consume(TokenKind::OpeningRoundBracket)?;
            let explicitness = consume_explicitness_symbol(context);
            let mut parameters = Vec::new();

            parameters.push(context.consume_identifier()?);

            // @Bug produces bad error messages
            while let Ok(parameter) = context.consume_identifier() {
                parameters.push(parameter);
            }

            let type_annotation = parse_type_annotation(context).ok();

            context.consume(TokenKind::ClosingRoundBracket)?;

            Ok(ParameterGroup {
                parameters,
                type_annotation,
                explicitness,
            })
        }

        context
            .consume_identifier()
            .map(|identifier| ParameterGroup {
                parameters: vec![identifier],
                type_annotation: None,
                explicitness: Explicitness::Explicit,
            })
            .or_else(|_| parse_optionally_annotated_parameter_group(context))
    }
}

/// Parse a type annotation.
///
/// ## Grammar
///
/// ```text
/// Type_Annotation ::= ":" Expression
/// ```
fn parse_type_annotation(context: &mut Context<'_>) -> Result<Expression> {
    context.consume(TokenKind::Colon)?;
    context.reflect(parse_expression)
}

// @Task generalize, move somewhere else
// @Note this is currently only used for let-declarations and I don't know if
// it works in other contexts
fn parse_possibly_indented_expression_followed_by_line_break(
    context: &mut Context<'_>,
) -> Result<Expression> {
    if context.consumed(TokenKind::LineBreak) {
        context.consume(TokenKind::Indentation)?;
        let expression = parse_expression_followed_by_line_break(context)?;
        context.consume(TokenKind::Dedentation)?;
        Ok(expression)
    } else {
        parse_expression_followed_by_line_break(context)
    }
}

// @Note temporary generalize
fn parse_expression_followed_by_line_break(context: &mut Context<'_>) -> Result<Expression> {
    let expression = parse_expression(context)?;
    context.consume(TokenKind::LineBreak)?;
    Ok(expression)
}

fn consume_explicitness_symbol(context: &mut Context<'_>) -> Explicitness {
    match context.consume(TokenKind::VerticalBar) {
        Ok(_) => Explicitness::Implicit,
        Err(_) => Explicitness::Explicit,
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
