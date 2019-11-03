//! HIR — high-level intermediate representation

use crate::parser::{self, Explicitness, Identifier};

use std::fmt;

// @Task pub lower_module

// @Task lower span information

// @Beacon @Beacon @Task add span information!!!
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

// @Question should the line break be part of the result? I don't think so:
// The parent context should decide (here: a module) (e.g. no line break on the end of output)
// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled (e.g. an indented data declaration doesn't have its constructors indented)
// @Task implement indentation logic (@Note for now, it's not that relevant because we don*t have modules yet, so a data
// declaration is never actually indented, also expressions which face the same issue when pretty-printing, are printed out in one single line!)
impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Let {
                binder,
                type_annotation,
                expression,
            } => write!(f,
                "'let {}: {} = {}",
                binder,
                type_annotation,
                expression
            ),
            Self::Data {
                binder,
                type_annotation,
                constructors,
            } => write!(f,
                "'data {}: {}\n{}",
                binder,
                type_annotation,
                constructors
                    .into_iter()
                    .map(|constructor| format!("    {}", constructor))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            _ => unimplemented!(),
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

            let mut expression = lower_expression(&expression); // @Temporary

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
                            explicitness: parameter_group.explicitness,
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

// @Question should the line break/indentation be part of the result? I don't think so:
// The parent context should decide (e.g. no line break on the end of output, further indentation)
impl fmt::Display for Constructor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,
            "{}: {}",
            self.binder,
            self.type_annotation
        )
    }
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

// @Beacon @Beacon @Task add span information!!!
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
        explicitness: Explicitness,
        type_annotation: Option<Box<Expression>>,
        expression: Box<Expression>,
    },
    UseIn, // @Task
    Case,  // @Task
}

// @Task display fewer round brackets by making use of precedence
// @Note many wasted allocations (intermediate Strings)
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::PiLiteral {
                binder,
                parameter,
                expression,
                explicitness,
            } => write!(f,
                "({}{}{}) -> ({})",
                explicitness,
                binder
                    .as_ref()
                    .map(|binder| format!("{}: ", binder))
                    .unwrap_or_default(),
                parameter,
                expression,
            ),
            Self::Application {
                expression,
                argument,
                explicitness,
            } => write!(f,
                "({}) ({}{})",
                expression,
                explicitness,
                argument,
            ),
            Self::TypeLiteral => f.write_str("'Type"),
            Self::Identifier(identifier) => write!(f, "{}", identifier),
            Self::Hole(identifier) => write!(f, "'hole {}", identifier),
            Self::LambdaLiteral {
                binder,
                parameter,
                explicitness,
                type_annotation,
                expression,
            } => write!(f,
                "\\({}{}{}){} => ({})",
                explicitness,
                binder,
                parameter
                    .as_ref()
                    .map(|parameter| format!(": {}", parameter))
                    .unwrap_or_default(),
                type_annotation
                    .as_ref()
                    .map(|type_annotation| format!(": {}", type_annotation))
                    .unwrap_or_default(),
                expression
            ),
            _ => unimplemented!(),
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
                        explicitness: parameter_group.explicitness,
                        type_annotation: type_annotation.next(),
                        expression: Box::new(expression),
                    };
                }
            }
            expression
        }
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
                        explicitness: parameter_group.explicitness,
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
                    explicitness: Explicitness::Explicit,
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
