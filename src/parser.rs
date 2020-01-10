//! The parser.
//!
//! I *think* it can be classified as a top-down recursive-descent parser with arbitrary look-ahead.

mod context;
mod error;
mod identifier;

use std::fmt;

use crate::error::Span;
use crate::lexer::{self, TokenKind};

pub use context::Context;
pub use error::Error;
pub use identifier::Identifier;

use error::Result;

pub use declaration::{parse_declaration, Declaration};

/// The part of the parser concerned with declarations.
pub mod declaration {
    use super::*;

    // @Question will the spans on them even get used (for error messages)
    // @Note in most cases, only the span of the binder or the type annotations contained within
    // will be used, I guess
    /// The syntax node of a declaration.
    #[derive(Debug)] // @Temporary
    pub enum Declaration {
        Value(Box<Value>),
        Data(Box<Data>),
        Module(Box<Module>),
        Use(Box<Use>),
        Foreign(Box<Foreign>),
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

    // @Task grammar rule
    pub fn parse_declaration(context: &mut Context<'_>) -> Result<Declaration> {
        // @Temporary missing use, module and foreign declarations
        context
            .reflect(|context| {
                Ok(Declaration::Data(Box::new(parse_data_declaration(
                    context,
                )?)))
            })
            .or_else(|_| -> Result<_> {
                Ok(Declaration::Value(Box::new(
                    context.reflect(parse_value_declaration)?,
                )))
            })
            .or_else(|_| {
                Ok(Declaration::Foreign(Box::new(parse_foreign_declaration(
                    context,
                )?)))
            })
    }

    /// The syntax node of a value declaration.
    #[derive(Debug)]
    pub struct Value {
        pub binder: Identifier,
        pub parameters: AnnotatedParameters,
        pub type_annotation: Expression,
        pub expression: Expression,
        pub span: Span,
    }

    /// Parse a value declaration.
    ///
    /// Grammar rule:
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

    /// The syntax node of a data declaration.
    #[derive(Debug)]
    pub struct Data {
        pub binder: Identifier,
        pub parameters: AnnotatedParameters,
        pub type_annotation: Expression,
        pub constructors: Vec<Constructor>,
        pub span: Span,
    }

    // @Task grammar rule
    pub fn parse_data_declaration(context: &mut Context<'_>) -> Result<Data> {
        let span_of_keyword = context
            .consume(TokenKind::Keyword(lexer::Keyword::Data))?
            .span;
        let binder = context.consume_identifier()?;
        let parameters = parse_annotated_parameters(context)?;
        let type_annotation = parse_type_annotation(context)?;
        let span_of_equals = context.consume(TokenKind::Equals)?.span;
        context.consume(TokenKind::LineBreak)?;

        let mut constructors = Vec::new();

        if context.consume(TokenKind::Indentation).is_ok() {
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

    // @Temporary missing a lot of information
    #[derive(Debug)]
    pub struct Module {
        pub declarations: Vec<Declaration>,
        pub span: Span,
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
            if context.consume(TokenKind::LineBreak).is_ok() {
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
    #[derive(Debug)]
    pub struct Use {
        pub span: Span,
    }

    // @Task
    fn _parse_use_declaration() -> Result<Use> {
        unimplemented!()
    }

    /// The syntax node of a foreign declaration.
    #[derive(Debug)]
    pub struct Foreign {
        pub binder: Identifier,
        pub parameters: AnnotatedParameters,
        pub type_annotation: Expression,
        pub span: Span,
    }

    /// Parse a foreign declaration.
    ///
    /// Grammar rule:
    /// ```text
    /// Foreign_Declaration ::= "foreign" Identifier Annotated_Parameters Type_Annotation Line_Break
    /// ```
    fn parse_foreign_declaration(context: &mut Context<'_>) -> Result<Foreign> {
        let span_of_keyword = context
            .consume(TokenKind::Keyword(lexer::Keyword::Foreign))?
            .span;
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
    /// Grammar rule:
    /// ```text
    /// Annotated_Parameters ::= Annotated_Parameter_Group*
    /// ```
    fn parse_annotated_parameters(context: &mut Context<'_>) -> Result<AnnotatedParameters> {
        let mut parameters = Vec::new();

        // @Bug drops errors @Note updated code, check for bug @Task
        // @Bug produces bad error messages
        while let Ok(parameter_group) = context.reflect(parse_annotated_parameter_group) {
            parameters.push(parameter_group)
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
    /// Grammar rule:
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
    #[derive(Debug, Clone)]
    pub enum Expression {
        PiTypeLiteral(Box<PiTypeLiteral>),
        Application(Box<Application>),
        TypeLiteral(Box<TypeLiteral>),
        NatTypeLiteral(Box<NatTypeLiteral>),
        NatLiteral(Box<NatLiteral>),
        Path(Box<Path>),
        Hole(Box<Hole>),
        LambdaLiteral(Box<LambdaLiteral>),
        LetIn(Box<LetIn>),
        UseIn(Box<UseIn>),
        CaseAnalysis(Box<CaseAnalysis>),
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
                Self::Hole(hole) => hole.span,
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
                Self::Hole(hole) => &mut hole.span,
                Self::LambdaLiteral(literal) => &mut literal.span,
                Self::LetIn(let_in) => &mut let_in.span,
                Self::UseIn(use_in) => &mut use_in.span,
                Self::CaseAnalysis(case_analysis) => &mut case_analysis.span,
            }
        }
    }

    /// Parse an expression.
    ///
    /// Grammar rule:
    /// ```text
    /// Expression ::= Let_In | Lambda_Literal | Case_Analysis | Pi_Literal_Or_Lower
    /// ```
    // @Bug @Beacon error messages are really bad, @Task you need to do prefix-parsing instead of or_else's
    // @Note @Bug indentation logic not implemented (e.g. indent+let-in)
    pub fn parse_expression(context: &mut Context<'_>) -> Result<Expression> {
        context
            .reflect(|context| Ok(Expression::LetIn(Box::new(parse_let_in(context)?))))
            .or_else(|_| {
                Ok(Expression::LambdaLiteral(Box::new(
                    context.reflect(parse_lambda_literal)?,
                )))
            })
            .or_else(|_: Error| {
                Ok(Expression::CaseAnalysis(Box::new(
                    context.reflect(parse_case_analysis)?,
                )))
            })
            .or_else(|_: Error| parse_pi_type_literal_or_lower(context))
    }

    /// The syntax node of pi-type literals.
    #[derive(Debug, Clone)]
    pub struct PiTypeLiteral {
        pub binder: Option<Identifier>,
        pub parameter: Expression,
        pub expression: Expression,
        pub explicitness: Explicitness,
        pub span: Span,
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
                    // .reflect(parse_semicolon_application_or_lower)
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

    /// The syntax node of function application.
    #[derive(Debug, Clone)]
    pub struct Application {
        pub callee: Expression,
        pub argument: Expression,
        pub explicitness: Explicitness,
        pub span: Span,
    }

    // @Task implement semicolon application
    // @Note the logic should probably take place inside parse_application_or_lower
    // @Note: The precedence of semicolon application and pi literals IS THE SAME
    // we don't need the "or_lower"-logic here but simply an `or_else`
    // @Question where exactly to impl??

    // @Beacon @@Note semicolon application
    // Is right-associative and apparently (Haskell) has the same precedence
    // as `->` which is also right-assoc
    // Interaction between application, `;` and `->` looks like:
    // Alpha Beta -> Gamma Delta ;;; (Alpha Beta) -> (Gamma Delta)
    // Alpha; Beta -> Gamma; Delta ;;; (Alpha) (Beta -> ((Gamma) (Delta)))
    // @Task grammar
    // fn parse_semicolon_application_or_lower(context: &mut Context<'_>) -> Result<Expression> {
    //     let expression = parse_application_or_lower(context)?;

    //     Ok(if context.consume(TokenKind::Semicolon).is_ok() {
    //         Expression::Application(Box::new(expression::Application {
    //             expression: Box::new(expression),
    //             argument: Box::new(context.reflect(parse_semicolon_application_or_lower)?),
    //             explicitness: Explicitness::Explicit,
    //         }))
    //     } else {
    //         expression
    //     })
    // }

    /// Parse an application or a lower expression.
    ///
    /// Grammar rule:
    /// ```text
    /// Application_Or_Lower %left% ::= Lower_Expression (Lower_Expression | "(" "|" Expression ")")*
    /// ```
    // @Task application with semicolon
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
    /// Grammar rule:
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
            .or_else(|_: Error| Ok(Expression::Hole(Box::new(context.reflect(parse_hole)?))))
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

    #[derive(Debug, Clone)]
    pub struct TypeLiteral {
        pub span: Span,
    }

    // Type_Literal ::= %type literal%
    fn parse_type_literal(context: &mut Context<'_>) -> Result<TypeLiteral> {
        context
            .consume(TokenKind::Keyword(lexer::Keyword::Type))
            .map(|token| TypeLiteral { span: token.span })
    }

    #[derive(Debug, Clone)]
    pub struct NatTypeLiteral {
        pub span: Span,
    }

    fn parse_nat_type_literal(context: &mut Context<'_>) -> Result<NatTypeLiteral> {
        context
            .consume(TokenKind::Keyword(lexer::Keyword::Nat))
            .map(|token| NatTypeLiteral { span: token.span })
    }

    #[derive(Debug, Clone)]
    pub struct NatLiteral {
        pub value: lexer::Nat,
        pub span: Span,
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

    // @Task make it able to parse complex paths
    #[derive(Debug, Clone)]
    pub struct Path {
        pub inner: Identifier,
    }

    // Identifier ::= %identifier%
    fn parse_path(context: &mut Context<'_>) -> Result<Path> {
        context.consume_identifier().map(|inner| Path { inner })
    }

    #[derive(Debug, Clone)]
    pub struct Hole {
        pub tag: Identifier,
        pub span: Span,
    }

    // @Task change syntax to `"?" Identifier`
    // Hole ::= "'hole" %identifier%
    fn parse_hole(context: &mut Context<'_>) -> Result<Hole> {
        let keyword_hole = context.consume(TokenKind::Keyword(lexer::Keyword::Hole))?;
        let tag = context.consume_identifier()?;
        Ok(Hole {
            span: keyword_hole.span.merge(tag.span),
            tag,
        })
    }

    /// The syntax node of a lambda literal expression.
    #[derive(Debug, Clone)]
    pub struct LambdaLiteral {
        pub parameters: Parameters,
        pub body_type_annotation: Option<Expression>,
        pub body: Expression,
        pub span: Span,
    }

    /// Parse a lambda literal expression.
    ///
    /// Grammar rule:
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

    /// The syntax-node of a let-in expression.
    // @Task rename the field `expression` to something more descriptive
    #[derive(Debug, Clone)]
    pub struct LetIn {
        pub binder: Identifier,
        pub parameters: Parameters,
        pub type_annotation: Option<Expression>,
        // @Task improve upon naming
        pub expression: Expression,
        pub scope: Expression,
        pub span: Span,
    }

    /// Parse an let-in expression.
    ///
    /// Grammar rule:
    /// ```text
    /// Let_In ::= "'let" Identifier Parameters Type_Annotation? "=" Expression "'in" Expression
    /// ```
    fn parse_let_in(context: &mut Context<'_>) -> Result<LetIn> {
        let let_keyword = context.consume(TokenKind::Keyword(lexer::Keyword::Let))?;
        let binder = context.consume_identifier()?;
        let parameters = context.reflect(parse_parameters)?;
        let type_annotation = context.reflect(parse_type_annotation).ok();
        context.consume(TokenKind::Equals)?;
        let expression = context.reflect(parse_expression)?;
        context.consume(TokenKind::Keyword(lexer::Keyword::In))?;
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

    // @Task
    #[derive(Debug, Clone)]
    pub struct UseIn {
        pub span: Span,
    }

    #[derive(Debug, Clone)]
    pub struct CaseAnalysis {
        pub expression: Expression,
        pub cases: Vec<CaseAnalysisCaseGroup>,
        pub span: Span,
    }

    // Case_Analysis ::= "'case" Expression Line_Break Case_Analysis_Case_Group*
    fn parse_case_analysis(context: &mut Context<'_>) -> Result<CaseAnalysis> {
        let span_of_keyword = context
            .consume(TokenKind::Keyword(lexer::Keyword::Case))?
            .span;
        let expression = parse_expression_followed_by_line_break(context)?;

        let mut cases = Vec::new();

        // @Bug this produces bad error messages!
        while let Ok(case_group) = context.reflect(parse_case_analysis_case_group) {
            cases.push(case_group);
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

    // Case_Analyis_Case_Group ::= ("'of" Pattern)+ "=>" Expression Line_Break
    fn parse_case_analysis_case_group(context: &mut Context<'_>) -> Result<CaseAnalysisCaseGroup> {
        let mut patterns = Vec::new();

        context.consume(TokenKind::Keyword(lexer::Keyword::Of))?;
        patterns.push(parse_pattern(context)?);

        loop {
            // @Beacon @Note
            // loop until you find "=>" @Note this is a better break condition than `while let Ok`
            // fyi for all those in this module!!
            if context.consume(TokenKind::WideArrow).is_ok() {
                break;
            }

            context.consume(TokenKind::Keyword(lexer::Keyword::Of))?;
            patterns.push(parse_pattern(context)?);
        }

        Ok(CaseAnalysisCaseGroup {
            patterns,
            expression: parse_expression_followed_by_line_break(context)?,
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
/// Grammar rule:
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
    if context.consume(TokenKind::LineBreak).is_ok() {
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
