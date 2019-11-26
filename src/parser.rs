// @Beacon @Beacon @Beacon @Beacon @Beacon
// @Task rewrite this whole parser with great(!) error messages in mind!

mod context;
mod error;
mod identifier;

use std::fmt;

use crate::error::Span;
use crate::lexer;

pub use context::Context;
pub use declaration::{parse_declaration, Declaration};
pub use error::Error;
pub use expression::{parse_expression, Expression};
pub use identifier::Identifier;

use error::Result;

pub mod declaration {
    use super::*;

    // @Question will the spans on them even get used (for error messages)
    // @Note in most cases, only the span of the binder or the type annotations contained within
    // will be used, I guess
    #[derive(Debug)] // @Temporary
    pub enum Declaration {
        Let(Box<Let>),
        Data(Box<Data>),
        Module(Box<Module>),
        Use(Box<Use>),
        Foreign(Box<Foreign>),
    }

    impl Declaration {
        pub fn span(&self) -> Span {
            match self {
                Self::Let(box Let { span, .. }) => *span,
                Self::Data(box Data { span, .. }) => *span,
                Self::Module(box Module { span, .. }) => *span,
                Self::Use(box Use { span, .. }) => *span,
                Self::Foreign(box Foreign { span, .. }) => *span,
            }
        }
    }

    pub fn parse_declaration(context: &mut Context<'_>) -> Result<Declaration> {
        // @Temporary missing use, module and foreign declarations
        context
            .reflect(|context| {
                Ok(Declaration::Data(Box::new(parse_data_declaration(
                    context,
                )?)))
            })
            .or_else(|_| Ok(Declaration::Let(Box::new(parse_let_declaration(context)?))))
    }

    #[derive(Debug)]
    pub struct Let {
        pub binder: Identifier,
        pub parameters: AnnotatedParameters,
        pub type_annotation: Expression,
        pub expression: Expression,
        pub span: Span,
    }

    fn parse_let_declaration(context: &mut Context<'_>) -> Result<Let> {
        let span_of_let_keyword = context
            .consume(lexer::TokenKind::Keyword(lexer::Keyword::Let))?
            .span;
        let binder = context.consume_identifier()?;
        let parameters = parse_annotated_parameters(context)?;

        let type_annotation = parse_type_annotation(context)?;
        context.consume(lexer::TokenKind::Equals)?;
        let expression = parse_possibly_indented_expression_followed_by_line_break(context)?;

        Ok(Let {
            span: span_of_let_keyword.merge(expression.span()),
            binder,
            parameters,
            type_annotation,
            expression,
        })
    }

    #[derive(Debug)]
    pub struct Data {
        pub binder: Identifier,
        pub parameters: AnnotatedParameters,
        pub type_annotation: Expression,
        pub constructors: Vec<Constructor>,
        pub span: Span,
    }

    pub fn parse_data_declaration(context: &mut Context<'_>) -> Result<Data> {
        let span_of_data_keyword = context
            .consume(lexer::TokenKind::Keyword(lexer::Keyword::Data))?
            .span;
        let binder = context.consume_identifier()?;
        let parameters = parse_annotated_parameters(context)?;
        let type_annotation = parse_type_annotation(context)?;
        let span_of_equals = context.consume(lexer::TokenKind::Equals)?.span;
        context.consume(lexer::TokenKind::LineBreak)?;

        let mut constructors = Vec::new();

        if context.consume(lexer::TokenKind::Indentation).is_ok() {
            while let Ok(constructor) = context.reflect(parse_constructor) {
                constructors.push(constructor);
            }
            // @Question or EOI?
            context.consume(lexer::TokenKind::Dedentation)?;
        }

        Ok(Data {
            span: span_of_data_keyword.merge(
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
            // empty line
            if context.consume(lexer::TokenKind::LineBreak).is_ok() {
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

    // @Task
    #[derive(Debug)]
    pub struct Foreign {
        pub span: Span,
    }

    // @Task
    fn _parse_foreign_declaration() -> Result<Foreign> {
        unimplemented!()
    }

    #[derive(Debug)]
    pub struct Constructor {
        pub binder: Identifier,
        pub parameters: AnnotatedParameters,
        pub type_annotation: Expression,
        pub span: Span,
    }

    // @Task very future: unnameable constructor `'_`
    fn parse_constructor(context: &mut Context<'_>) -> Result<Constructor> {
        let binder = context.consume_identifier()?;
        let parameters = parse_annotated_parameters(context)?;
        let type_annotation = parse_type_annotation(context)?;
        // @Question what about EOI?
        context.consume(lexer::TokenKind::LineBreak)?;

        Ok(Constructor {
            span: binder.span.merge(type_annotation.span()),
            binder,
            parameters,
            type_annotation,
        })
    }

    pub type AnnotatedParameters = Vec<AnnotatedParameterGroup>;

    // Annotated_Parameters ::= Annotated_Parameter_Group*
    fn parse_annotated_parameters(context: &mut Context<'_>) -> Result<AnnotatedParameters> {
        let mut parameters = Vec::new();

        // @Bug drops errors @Note updated code, check for bug @Task
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

    // Annotated_Parameter_Group ::= "(" ","? Unfenced_Annotated_Parameter_Group ")"
    // Unfenced_Annotated_Parameter_Group ::= Identifier+ Type_Annotation
    fn parse_annotated_parameter_group(
        context: &mut Context<'_>,
    ) -> Result<AnnotatedParameterGroup> {
        context.consume(lexer::TokenKind::OpeningRoundBracket)?;
        let explicitness = consume_explicitness_comma(context);
        let mut parameters = Vec::new();

        parameters.push(context.consume_identifier()?);

        // @Note @Bug probably drops errors as well @Note shouldn't: consume
        // reflects by itself @Task verify
        while let Ok(parameter) = context.consume_identifier() {
            parameters.push(parameter);
        }

        let type_annotation = context.reflect(parse_type_annotation)?;
        context.consume(lexer::TokenKind::ClosingRoundBracket)?;

        Ok(AnnotatedParameterGroup {
            parameters,
            type_annotation,
            explicitness,
        })
    }
}

pub mod expression {
    use super::*;

    #[derive(Debug, Clone)]
    pub enum Expression {
        PiTypeLiteral(Box<PiTypeLiteral>),
        Application(Box<Application>),
        TypeLiteral(Box<TypeLiteral>),
        NatTypeLiteral(Box<NatTypeLiteral>),
        NatLiteral(Box<NatLiteral>),
        Identifier(Box<Path>),
        Hole(Box<Hole>),
        LambdaLiteral(Box<LambdaLiteral>),
        LetIn(Box<LetIn>),
        UseIn(Box<UseIn>),
        CaseAnalysis(Box<CaseAnalysis>),
    }

    impl Expression {
        pub fn span(&self) -> Span {
            match self {
                Self::PiTypeLiteral(box PiTypeLiteral { span, .. }) => *span,
                Self::Application(box Application { span, .. }) => *span,
                Self::TypeLiteral(box TypeLiteral { span }) => *span,
                Self::NatTypeLiteral(box NatTypeLiteral { span }) => *span,
                Self::NatLiteral(box NatLiteral { span, .. }) => *span,
                Self::Identifier(box Path { inner }) => inner.span,
                Self::Hole(box Hole { span, .. }) => *span,
                Self::LambdaLiteral(box LambdaLiteral { span, .. }) => *span,
                Self::LetIn(box LetIn { span, .. }) => *span,
                Self::UseIn(box UseIn { span, .. }) => *span,
                Self::CaseAnalysis(box CaseAnalysis { span, .. }) => *span,
            }
        }
    }

    // @Bug @Beacon error messages are really bad, @Task you need to do prefix-parsing instead of or_else's
    // @Note @Bug indentation logic not implemented (e.g. indent+let-in)
    // Expression ::= Let_In | Lambda_Literal | Pi_Literal_Or_Lower
    pub fn parse_expression(context: &mut Context<'_>) -> Result<Expression> {
        context
            .reflect(|context| Ok(Expression::LetIn(Box::new(parse_let_in(context)?))))
            .or_else(|_| Ok(Expression::LambdaLiteral(Box::new(context.reflect(parse_lambda_literal)?))))
            .or_else(|_: Error| parse_pi_literal_or_lower(context))
    }

    // @Task rename to PiTypeLiteral
    #[derive(Debug, Clone)]
    pub struct PiTypeLiteral {
        pub binder: Option<Identifier>,
        pub parameter: Expression,
        pub expression: Expression,
        pub explicitness: Explicitness,
        pub span: Span,
    }

    // @Task update grammar
    // Pi_Literal_Or_Lower %right% ::= Application_Or_Lower (-> Pi_Literal_Or_Lower)*
    fn parse_pi_literal_or_lower(context: &mut Context<'_>) -> Result<Expression> {
        let (span_at_the_beginning, explicitness, binder, parameter) = context
            .reflect(|context| {
                let span_of_opening_round_bracket =
                    context.consume(lexer::TokenKind::OpeningRoundBracket)?.span;

                let explicitness = consume_explicitness_comma(context);
                let binder = context.consume_identifier()?;
                let parameter = parse_type_annotation(context)?;

                context.consume(lexer::TokenKind::ClosingRoundBracket)?;

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

        Ok(match context.consume(lexer::TokenKind::ThinArrow) {
            Ok(_) => {
                let expression = context.reflect(parse_pi_literal_or_lower)?;

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

    #[derive(Debug, Clone)]
    pub struct Application {
        pub expression: Expression,
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

    //     Ok(if context.consume(lexer::TokenKind::Semicolon).is_ok() {
    //         Expression::Application(Box::new(expression::Application {
    //             expression: Box::new(expression),
    //             argument: Box::new(context.reflect(parse_semicolon_application_or_lower)?),
    //             explicitness: Explicitness::Explicit,
    //         }))
    //     } else {
    //         expression
    //     })
    // }

    // @Task application with semicolon
    // Application_Or_Lower %left% ::= Lower_Expression (Lower_Expression | "(" "," Expression ")")*
    fn parse_application_or_lower(context: &mut Context<'_>) -> Result<Expression> {
        let mut expression = context.reflect(parse_lower_expression)?;
        while let Ok((argument, explicitness)) = context
            .reflect(|context| Ok((parse_lower_expression(context)?, Explicitness::Explicit)))
            .or_else(|_| -> Result<_> {
                context.consume(lexer::TokenKind::OpeningRoundBracket)?;
                context.consume(lexer::TokenKind::Comma)?;
                let expression = parse_expression(context)?;
                context.consume(lexer::TokenKind::ClosingRoundBracket)?;
                Ok((expression, Explicitness::Implicit))
            })
        {
            expression = Expression::Application(Box::new(expression::Application {
                span: expression.span().merge(argument.span()),
                expression: expression,
                argument: argument,
                explicitness,
            }));
        }
        Ok(expression)
    }

    // Lower_Expression ::=
    //     Type_Literal | Nat_Literal | Identifier | Hole | Bracketed_Expression
    fn parse_lower_expression(context: &mut Context<'_>) -> Result<Expression> {
        parse_type_literal(context)
            .map(|type_literal| Expression::TypeLiteral(Box::new(type_literal)))
            .or_else(|_| Ok(Expression::NatTypeLiteral(Box::new(parse_nat_type_literal(context)?))))
            .or_else(|_: Error| Ok(Expression::NatLiteral(Box::new(parse_nat_literal(context)?))))
            .or_else(|_: Error| Ok(Expression::Identifier(Box::new(parse_path(context)?))))
            .or_else(|_: Error| Ok(Expression::Hole(Box::new(context.reflect(parse_hole)?))))
            .or_else(|_: Error| parse_bracketed_expression(context))
    }

    // Bracketed_Expression ::= "(" Expression ")"
    fn parse_bracketed_expression(context: &mut Context<'_>) -> Result<Expression> {
        context.consume(lexer::TokenKind::OpeningRoundBracket)?;
        let expression = context.reflect(parse_expression)?;
        context.consume(lexer::TokenKind::ClosingRoundBracket)?;
        Ok(expression)
    }

    #[derive(Debug, Clone)]
    pub struct TypeLiteral {
        pub span: Span,
    }

    // Type_Literal ::= %type literal%
    fn parse_type_literal(context: &mut Context<'_>) -> Result<TypeLiteral> {
        context
            .consume(lexer::TokenKind::Keyword(lexer::Keyword::Type))
            .map(|token| TypeLiteral { span: token.span })
    }

    #[derive(Debug, Clone)]
    pub struct NatTypeLiteral {
        pub span: Span,
    }

    fn parse_nat_type_literal(context: &mut Context<'_>) -> Result<NatTypeLiteral> {
        context
            .consume(lexer::TokenKind::Keyword(lexer::Keyword::Nat))
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
            .consume(lexer::TokenKind::NatLiteral)
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

    // Hole ::= "'hole" %identifier%
    fn parse_hole(context: &mut Context<'_>) -> Result<Hole> {
        let keyword_hole = context.consume(lexer::TokenKind::Keyword(lexer::Keyword::Hole))?;
        let tag = context.consume_identifier()?;
        Ok(Hole {
            span: keyword_hole.span.merge(tag.span),
            tag,
        })
    }

    // @Task rename to LambdaLiteral { parameters, body_type_annotation, body }
    #[derive(Debug, Clone)]
    pub struct LambdaLiteral {
        pub parameters: Parameters,
        pub type_annotation: Option<Expression>,
        pub expression: Expression,
        pub span: Span,
    }

    // Lambda_Literal ::= "\" Parameters Type_Annotation? "=>" Expression
    fn parse_lambda_literal(context: &mut Context<'_>) -> Result<LambdaLiteral> {
        let backslash = context.consume(lexer::TokenKind::Backslash)?;
        let parameters = context.reflect(parse_parameters)?;
        let type_annotation = context.reflect(parse_type_annotation).ok();
        context.consume(lexer::TokenKind::WideArrow)?;
        let expression = context.reflect(parse_expression)?;

        Ok(LambdaLiteral {
            parameters,
            type_annotation,
            span: backslash.span.merge(expression.span()),
            expression,
        })
    }

    // @Task rename the field expression to something more descriptive
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

    // Let_In ::= "'let" Identifier Parameters Type_Annotation? "=" Expression "'in" Expression
    fn parse_let_in(context: &mut Context<'_>) -> Result<LetIn> {
        let let_keyword = context.consume(lexer::TokenKind::Keyword(lexer::Keyword::Let))?;
        let binder = context.consume_identifier()?;
        let parameters = context.reflect(parse_parameters)?;
        let type_annotation = context.reflect(parse_type_annotation).ok();
        context.consume(lexer::TokenKind::Equals)?;
        let expression = context.reflect(parse_expression)?;
        context.consume(lexer::TokenKind::Keyword(lexer::Keyword::In))?;
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

    // @Task
    #[derive(Debug, Clone)]
    pub struct CaseAnalysis {
        pub expression: Expression,
        pub cases: Vec<CaseAnalysisCaseGroup>,
        pub span: Span,
    }

    fn parse_case_analysis(context: &mut Context<'_>) -> Result<CaseAnalysis> {
        unimplemented!() // @Task @Beacon @Beacon @Beacon @Beacon
    }

    #[derive(Debug, Clone)]
    pub struct CaseAnalysisCaseGroup {
        patterns: Vec<Pattern>,
        expression: Expression,
    }

    fn parse_case_analysis_case_group(context: &mut Context<'_>) -> Result<CaseAnalysisCaseGroup> {
        unimplemented!()
    }

    #[derive(Debug, Clone)]
    pub enum Pattern {
        NatLiteral {
            value: lexer::Nat,
            span: Span,
        },
        Deconstruction {
            constructor: Identifier,
            patterns: Vec<Pattern>,
            span: Span,
        },
    }

    fn parse_pattern(context: &mut Context<'_>) -> Result<Pattern> {
        unimplemented!()
    }

    pub type Parameters = Vec<ParameterGroup>;

    // Parameters ::= Parameter_Group*
    fn parse_parameters(context: &mut Context<'_>) -> Result<Parameters> {
        let mut parameters = Vec::new();

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
            context.consume(lexer::TokenKind::OpeningRoundBracket)?;
            let explicitness = consume_explicitness_comma(context);
            let mut parameters = Vec::new();

            parameters.push(context.consume_identifier()?);

            while let Ok(parameter) = context.consume_identifier() {
                parameters.push(parameter);
            }

            let type_annotation = parse_type_annotation(context).ok();

            context.consume(lexer::TokenKind::ClosingRoundBracket)?;

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

// Type_Annotation ::= ":" Expression
fn parse_type_annotation(context: &mut Context<'_>) -> Result<Expression> {
    context.consume(lexer::TokenKind::Colon)?;
    context.reflect(parse_expression)
}

// @Task generalize, move somewhere else
// @Note this is currently only used for let-declarations and I don't know if
// it works in other contexts
fn parse_possibly_indented_expression_followed_by_line_break(
    context: &mut Context<'_>,
) -> Result<Expression> {
    if context.consume(lexer::TokenKind::LineBreak).is_ok() {
        context.consume(lexer::TokenKind::Indentation)?;
        let expression = parse_expression_followed_by_line_break(context)?;
        context.consume(lexer::TokenKind::Dedentation)?;
        Ok(expression)
    } else {
        parse_expression_followed_by_line_break(context)
    }
}

// @Note temporary generalize
fn parse_expression_followed_by_line_break(context: &mut Context<'_>) -> Result<Expression> {
    let expression = parse_expression(context)?;
    context.consume(lexer::TokenKind::LineBreak)?;
    Ok(expression)
}

fn consume_explicitness_comma(context: &mut Context<'_>) -> Explicitness {
    match context.consume(lexer::TokenKind::Comma) {
        Ok(_) => Explicitness::Implicit,
        Err(_) => Explicitness::Explicit,
    }
}

/// expect either a line break or the end of input
// fn expect_delimiter(context: &Context<'_>) -> Result<()> {
//     if let Ok(token) = context.token() {
//         if token.kind() != lexer::TokenKind::LineBreak {
//             return Err(Error {
//                 span: token.span,
//                 kind: ErrorKind::UnexpectedToken(token),
//             });
//         }
//     }
//     Ok(())
// }

#[derive(Clone, Copy, Debug)]
pub enum Explicitness {
    Implicit,
    Explicit,
}

impl fmt::Display for Explicitness {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Implicit => f.write_str(","),
            Self::Explicit => f.write_str(""),
        }
    }
}
