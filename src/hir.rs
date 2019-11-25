//! HIR — high-level intermediate representation

mod identifier;

use std::fmt;

use crate::parser::{self, Explicitness};
pub use identifier::Identifier;

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
    Module {
        declarations: Vec<Declaration>,
    },
    Use,     // @Task
    Foreign, // @Task
}

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
            } => write!(f, "'let {}: {} = {}", binder, type_annotation, expression),
            Self::Data {
                binder,
                type_annotation,
                constructors,
            } => write!(
                f,
                "'data {}: {}\n{}",
                binder,
                type_annotation,
                constructors
                    .into_iter()
                    .map(|constructor| format!("    {}", constructor))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            Self::Module { declarations } => {
                f.write_str("'module =\n")?;
                for declaration in declarations {
                    write!(f, "{}\n", declaration)?;
                }
                Ok(())
            }
            Self::Use => unimplemented!(),
            Self::Foreign => unimplemented!(),
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
            span: _,
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
                            binder: Identifier::Plain(binder.clone()),
                            // @Note expensive cloning
                            parameter_type_annotation: parameter.clone(),
                            explicitness: parameter_group.explicitness,
                            body_type_annotation: type_annotation.next(),
                            body: Box::new(expression),
                            data: (),
                        };
                    }
                }
            }

            Declaration::Let {
                binder: Identifier::Plain(binder.clone()),
                type_annotation: lower_annotated_parameters(&parameters, &type_annotation),
                expression,
            }
        }
        parser::Declaration::Data {
            binder,
            parameters,
            type_annotation,
            constructors,
            span: _,
        } => Declaration::Data {
            binder: Identifier::Plain(binder.clone()),
            type_annotation: lower_annotated_parameters(&parameters, &type_annotation),
            constructors: constructors.iter().map(lower_constructor).collect(),
        },
        parser::Declaration::Module {
            declarations,
            span: _,
        } => Declaration::Module {
            declarations: declarations.into_iter().map(lower_declaration).collect(),
        },
        parser::Declaration::Use { span: _ } => unimplemented!(),
        parser::Declaration::Foreign { span: _ } => unimplemented!(),
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
        write!(f, "{}: {}", self.binder, self.type_annotation)
    }
}

fn lower_constructor(constructor: &parser::Constructor) -> Constructor {
    Constructor {
        binder: Identifier::Plain(constructor.binder.clone()),
        type_annotation: lower_annotated_parameters(
            &constructor.parameters,
            &constructor.type_annotation,
        ),
    }
}

// @Beacon @Beacon @Task add span information!!! @Note @Beacon now, we are in the HIR,
// there might not be any span information if synthesize HIR nodes (common thing probably)
// @Task improve naming (binder, parameter, etcetera)
#[derive(Clone, Debug)]
// @Beacon @Beacon @Note rename P: Phase to D: Discrimant and have Raw, Normalized, Type (more to come)
pub enum Expression<P: Phase = InitialPhase> {
    PiTypeLiteral {
        // @Task rename binder to parameter in the future
        binder: Option<Identifier>,
        domain: Box<Expression>,
        codomain: Box<Expression>,
        explicitness: Explicitness,
        data: P::PiTypeLiteral,
    },
    Application {
        expression: Box<Expression>,
        argument: Box<Expression>,
        explicitness: Explicitness,
        data: P::Application,
    },
    // Substitution(Substitution, Box<Expression>),
    TypeLiteral {
        data: P::TypeLiteral,
    },
    NatTypeLiteral {
        data: P::NatTypeLiteral,
    },
    NatLiteral {
        value: crate::lexer::Nat,
        data: P::NatLiteral,
    },
    Identifier {
        identifier: Identifier,
        data: P::Identifier,
    },
    // Identifier {
    //     identifier: Identifier,
    //     index: DebruijnIndex,
    // },
    Hole {
        tag: Identifier,
        data: P::Hole,
    },
    LambdaLiteral {
        // @Task rename binder to parameter in the future
        binder: Identifier,
        parameter_type_annotation: Option<Box<Expression>>,
        explicitness: Explicitness,
        body_type_annotation: Option<Box<Expression>>,
        body: Box<Expression>,
        data: P::LambdaLiteral,
    },
    // @Task
    UseIn {
        data: P::UseIn,
    },
    // @Task
    Case {
        data: P::Case,
    },
}

// @Task display fewer round brackets by making use of precedence
// @Note many wasted allocations (intermediate Strings)
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::PiTypeLiteral {
                binder,
                domain,
                codomain,
                explicitness,
                data: (),
            } => write!(
                f,
                "({}{}{}) -> ({})",
                explicitness,
                binder
                    .as_ref()
                    .map(|binder| format!("{}: ", binder))
                    .unwrap_or_default(),
                domain,
                codomain,
            ),
            Self::Application {
                expression,
                argument,
                explicitness,
                data: (),
            } => write!(f, "({}) ({}{})", expression, explicitness, argument,),
            Self::TypeLiteral { data: () } => f.write_str("'Type"),
            Self::NatTypeLiteral { data: () } => f.write_str("'Nat"),
            Self::NatLiteral { value, data: () } => write!(f, "{}", value),
            Self::Identifier {
                identifier,
                data: (),
            } => write!(f, "{}", identifier),
            Self::Hole { tag, data: () } => write!(f, "'hole {}", tag),
            Self::LambdaLiteral {
                binder,
                parameter_type_annotation,
                explicitness,
                body_type_annotation,
                body,
                data: (),
            } => write!(
                f,
                "\\({}{}{}){} => ({})",
                explicitness,
                binder,
                parameter_type_annotation
                    .as_ref()
                    .map(|parameter| format!(": {}", parameter))
                    .unwrap_or_default(),
                body_type_annotation
                    .as_ref()
                    .map(|type_annotation| format!(": {}", type_annotation))
                    .unwrap_or_default(),
                body
            ),
            Self::UseIn { data: () } => unimplemented!(),
            Self::Case { data: () } => unimplemented!(),
        }
    }
}

pub fn lower_expression(expression: &parser::Expression) -> Expression {
    match &expression {
        parser::Expression::PiLiteral {
            binder,
            parameter,
            expression,
            explicitness,
            span: _,
        } => Expression::PiTypeLiteral {
            binder: binder.clone().map(Identifier::Plain),
            domain: Box::new(lower_expression(parameter)),
            codomain: Box::new(lower_expression(expression)),
            explicitness: *explicitness,
            data: (),
        },
        parser::Expression::Application {
            expression,
            argument,
            explicitness,
            span: _,
        } => Expression::Application {
            expression: Box::new(lower_expression(expression)),
            argument: Box::new(lower_expression(argument)),
            explicitness: *explicitness,
            data: (),
        },
        parser::Expression::TypeLiteral { span: _ } => Expression::TypeLiteral { data: () },
        parser::Expression::NatTypeLiteral { span: _ } => Expression::NatTypeLiteral { data: () },
        parser::Expression::NatLiteral { value, span: _ } => Expression::NatLiteral {
            value: value.clone(),
            data: (),
        },
        parser::Expression::Identifier { inner: identifier } => Expression::Identifier {
            identifier: Identifier::Plain(identifier.clone()),
            data: (),
        },
        parser::Expression::Hole {
            tag: identifier,
            span: _,
        } => Expression::Hole {
            tag: Identifier::Plain(identifier.clone()),
            data: (),
        },
        parser::Expression::LambdaLiteral {
            parameters,
            type_annotation,
            expression,
            span: _,
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
                        binder: Identifier::Plain(binder.clone()),
                        // @Note expensive clone
                        parameter_type_annotation: parameter.clone(),
                        explicitness: parameter_group.explicitness,
                        body_type_annotation: type_annotation.next(),
                        body: Box::new(expression),
                        data: (),
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
            span: _,
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
                        binder: Identifier::Plain(binder.clone()),
                        // @Note expensive clone
                        parameter_type_annotation: parameter.clone(),
                        explicitness: parameter_group.explicitness,
                        body_type_annotation: type_annotation.next(),
                        body: Box::new(expression),
                        data: (),
                    };
                }
            }

            Expression::Application {
                expression: Box::new(Expression::LambdaLiteral {
                    binder: Identifier::Plain(binder.clone()),
                    // @Note we cannot simply lower parameters and a type annotation because
                    // in the chain (`->`) of parameters, there might always be one missing and
                    // we don't support partial type annotations yet (using `'_`)
                    parameter_type_annotation: None,
                    explicitness: Explicitness::Explicit,
                    body_type_annotation: None,
                    body: Box::new(lower_expression(scope)),
                    data: (),
                }),
                argument: Box::new(expression),
                explicitness: Explicitness::Explicit,
                data: (),
            }
        }
        parser::Expression::UseIn { span: _ } => unimplemented!(),
        parser::Expression::Case { span: _ } => unimplemented!(),
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
            expression = Expression::PiTypeLiteral {
                binder: Some(Identifier::Plain(binder.clone())),
                // @Note expensive clone
                domain: parameter.clone(),
                codomain: Box::new(expression),
                explicitness: parameter_group.explicitness,
                data: (),
            }
        }
    }

    expression
}

// pub type DebruijnIndex = u32;

// // Boo, like a linked list!!! bad cache behavior!
// #[derive(Clone, Debug)]
// pub enum Substitution {
//     Shift(u32),
//     Composition(Box<Expression>, Box<Substitution>),
// }

// impl Substitution {
//     pub fn compose(&self, other: &Self) -> Self {
//         match (self, other) {
//             // @Note bad clone
//             (substitution, Self::Shift(0)) => substitution.clone(),
//             // @Task alternative to recursion??
//             (Self::Composition(expression, substitution), Self::Shift(amount)) => {
//                 substitution.compose(&Self::Shift(amount - 1))
//             }
//             (Self::Shift(first_amount), Self::Shift(second_amount)) => {
//                 Self::Shift(first_amount + second_amount)
//             }
//             // @Note very expensive clones!!
//             (first_substitution, Self::Composition(expression, second_substitution)) => {
//                 Self::Composition(
//                     Box::new(Expression::Substitution(
//                         first_substitution.clone(),
//                         expression.clone(),
//                     )),
//                     Box::new(first_substitution.compose(second_substitution)),
//                 )
//             }
//         }
//     }
// }

// @Temporary
#[derive(Clone, Debug)]
pub enum InitialPhase {}

pub trait Phase {
    type PiTypeLiteral;
    type Application;
    type TypeLiteral;
    type NatTypeLiteral;
    type NatLiteral;
    type Identifier;
    type Hole;
    type LambdaLiteral;
    type UseIn;
    type Case;
}

impl Phase for InitialPhase {
    type PiTypeLiteral = ();
    type Application = ();
    type TypeLiteral = ();
    type NatTypeLiteral = ();
    type NatLiteral = ();
    type Identifier = ();
    type Hole = ();
    type LambdaLiteral = ();
    type UseIn = ();
    type Case = ();
}
