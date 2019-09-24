//! HIR — hig-level immediate representation

use crate::error::DisplayWithSource;
use crate::parser::{self, Explicitness, Identifier};

// @Task pub lower_module

// @Task lower span information

pub enum Declaration {
    Let {
        binder: Identifier,
        type_annotation: Expression,
        expression: Expression,
    },
    Data {
        binder: Identifier,
        type_annotation: Expression,
        constructors: Vec<Constructor>,
    },
    Module,  // @Task,
    Use,     // @Task
    Foreign, // @Task
}

impl DisplayWithSource for Declaration {
    fn display_with_source(&self, source: &str) -> String {
        match self {
            Self::Let {
                binder,
                type_annotation,
                expression,
            } => format!(
                "'let {}: {} = {}\n",
                binder.display_with_source(source),
                type_annotation.display_with_source(source),
                expression.display_with_source(source)
            ),
            _ => unimplemented!() // @Task @Beacon
        }
    }
}

pub fn lower_declaration(declaration: &parser::Declaration) -> Declaration {
    match declaration {
        parser::Declaration::Let {
            binder,
            parameters,
            type_annotation,
            expression,
        } => {
            // @Note type_annotation is currently lowered twice
            // @Task remove duplicate work

            let mut expression = lower_expression(&expression); // @Temp

            {
                let mut type_annotation =
                    std::iter::once(Box::new(lower_expression(&type_annotation)));

                for parameter_group in parameters.into_iter().rev() {
                    let parameter =
                        Some(Box::new(lower_expression(&parameter_group.type_annotation)));

                    for binder in parameter_group.parameters.iter().rev() {
                        expression = Expression::LambdaLiteral {
                            binder: binder.clone(),
                            // @Note expensive cloning
                            parameter: parameter.clone(),
                            type_annotation: type_annotation.next(),
                            expression: Box::new(expression),
                        };
                    }
                }
            }

            Declaration::Let {
                binder: binder.clone(),
                type_annotation: lower_annotated_parameters(&parameters, &type_annotation),
                expression,
            }
        }
        parser::Declaration::Data {
            binder,
            parameters,
            type_annotation,
            constructors,
        } => Declaration::Data {
            binder: binder.clone(),
            type_annotation: lower_annotated_parameters(&parameters, &type_annotation),
            constructors: constructors.iter().map(lower_constructor).collect(),
        },
        _ => unimplemented!(),
    }
}

pub struct Constructor {
    pub binder: Identifier,
    pub type_annotation: Expression,
}

fn lower_constructor(constructor: &parser::Constructor) -> Constructor {
    Constructor {
        binder: constructor.binder.clone(),
        type_annotation: lower_annotated_parameters(
            &constructor.parameters,
            &constructor.type_annotation,
        ),
    }
}

// @Task improve naming (binder, parameter, etcetera)
#[derive(Clone)]
pub enum Expression {
    PiLiteral {
        binder: Option<Identifier>,
        parameter: Box<Expression>,
        expression: Box<Expression>,
        explicitness: Explicitness,
    },
    Application {
        expression: Box<Expression>,
        argument: Box<Expression>,
        explicitness: Explicitness,
    },
    TypeLiteral,
    Identifier(Identifier),
    Hole(Identifier),
    LambdaLiteral {
        binder: Identifier,
        parameter: Option<Box<Expression>>,
        type_annotation: Option<Box<Expression>>,
        expression: Box<Expression>,
    },
    UseIn, // @Task
    Case,  // @Task
}

// @Task display fewer round brackets by making use of precedence
// @Note many wasted allocations (intermediate Strings)
impl DisplayWithSource for Expression {
    fn display_with_source(&self, source: &str) -> String {
        match self {
            Self::PiLiteral {
                binder,
                parameter,
                expression,
                explicitness,
            } => {
                let (left, right) = explicitness.brackets();
                format!(
                    "{}{}{}{} -> ({})",
                    left,
                    binder
                        .as_ref()
                        .map(|binder| format!("{}: ", binder.display_with_source(source)))
                        .unwrap_or_default(),
                    right,
                    parameter.display_with_source(source),
                    expression.display_with_source(source),
                )
            },
            _ => unimplemented!() // @Beacon @Task
        }
    }
}

pub fn lower_expression(expression: &parser::Expression) -> Expression {
    match expression {
        parser::Expression::PiLiteral {
            binder,
            parameter,
            expression,
            explicitness,
        } => Expression::PiLiteral {
            binder: binder.clone(),
            parameter: Box::new(lower_expression(parameter)),
            expression: Box::new(lower_expression(expression)),
            explicitness: *explicitness,
        },
        parser::Expression::Application {
            expression,
            argument,
            explicitness,
        } => Expression::Application {
            expression: Box::new(lower_expression(expression)),
            argument: Box::new(lower_expression(argument)),
            explicitness: *explicitness,
        },
        parser::Expression::TypeLiteral => Expression::TypeLiteral,
        parser::Expression::Identifier(identifier) => Expression::Identifier(identifier.clone()),
        parser::Expression::Hole(identifier) => Expression::Hole(identifier.clone()),
        parser::Expression::LambdaLiteral {
            parameters,
            type_annotation,
            expression,
        } => {
            let mut expression = lower_expression(expression);

            let mut type_annotation = type_annotation
                .as_ref()
                .map(|expression| Box::new(lower_expression(expression)))
                .into_iter();

            for parameter_group in parameters.into_iter().rev() {
                let parameter = parameter_group
                    .type_annotation
                    .as_ref()
                    .map(lower_expression)
                    .map(Box::new);

                for binder in parameter_group.parameters.iter().rev() {
                    expression = Expression::LambdaLiteral {
                        binder: binder.clone(),
                        // @Note expensive clone
                        parameter: parameter.clone(),
                        type_annotation: type_annotation.next(),
                        expression: Box::new(expression),
                    };
                }
            }
            expression
        }
        // @Task @Beacon verify
        parser::Expression::LetIn {
            binder,
            parameters,
            type_annotation,
            expression,
            scope,
        } => {
            let mut expression = lower_expression(expression);

            let mut type_annotation = type_annotation
                .as_ref()
                .map(|expression| Box::new(lower_expression(expression)))
                .into_iter();

            for parameter_group in parameters.into_iter().rev() {
                let parameter = parameter_group
                    .type_annotation
                    .as_ref()
                    .map(lower_expression)
                    .map(Box::new);

                for binder in parameter_group.parameters.iter().rev() {
                    expression = Expression::LambdaLiteral {
                        binder: binder.clone(),
                        // @Note expensive clone
                        parameter: parameter.clone(),
                        type_annotation: type_annotation.next(),
                        expression: Box::new(expression),
                    };
                }
            }

            Expression::Application {
                expression: Box::new(Expression::LambdaLiteral {
                    binder: binder.clone(),
                    // @Note we cannot simply lower parameters and a type annotation because
                    // in the chain (`->`) of parameters, there might always be one missing and
                    // we don't support partial type annotations yet (using `'_`)
                    parameter: None,
                    type_annotation: None,
                    expression: Box::new(lower_expression(scope)),
                }),
                argument: Box::new(expression),
                explicitness: Explicitness::Explicit,
            }
        }
        _ => unimplemented!(),
    }
}

fn lower_annotated_parameters(
    parameters: &parser::AnnotatedParameters,
    type_annotation: &parser::Expression,
) -> Expression {
    let mut expression = lower_expression(type_annotation);

    for parameter_group in parameters.into_iter().rev() {
        let parameter = Box::new(lower_expression(&parameter_group.type_annotation));

        for binder in parameter_group.parameters.iter().rev() {
            expression = Expression::PiLiteral {
                binder: Some(binder.clone()),
                // @Note expensive clone
                parameter: parameter.clone(),
                expression: Box::new(expression),
                explicitness: parameter_group.explicitness,
            }
        }
    }

    expression
}
