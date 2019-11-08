//! First type-checker plus backend (tree-walk interpreter).
//!
//! The code in here is all temporary and needs to be reworked quite a bit!

mod context;
mod error;

use crate::hir::{self, Declaration, Expression, Identifier};
use crate::parser::Explicitness;
use context::Environment;
pub use context::{initial, ModuleContext};
use error::{Error, Result};

use std::collections::HashMap;
use std::rc::Rc;

// @Task make pure returning new Context
// @Question this is eager evaluation, right?
// @Task handle out of order declarations and recursions (you need to go through declarations twice) and
// only register the names plus types first (there is gonna be context.insert_unresolved_binding)
pub fn evaluate_declaration(declaration: &Declaration, context: ModuleContext) -> Result<()> {
    Ok(match declaration {
        Declaration::Let {
            binder,
            type_annotation,
            expression,
        } => {
            if context.clone().contains(&binder) {
                return Err(Error::AlreadyDefined);
            }
            let infered_type = infer_type(expression, context.clone())?;
            assert_expressions_are_equal(&infered_type, type_annotation, context.clone())?;
            let value = normalize(expression, context.clone())?;
            context.insert_binding(binder.clone(), infered_type, value);
        }
        Declaration::Data {
            binder: adt_binder,
            type_annotation,
            constructors,
        } => {
            if context.clone().contains(&adt_binder) {
                return Err(Error::AlreadyDefined);
            }
            assert_expression_is_a_type(type_annotation, context.clone())?;
            context
                .clone()
                .insert_adt(adt_binder.clone(), type_annotation.clone());

            for hir::Constructor {
                binder,
                type_annotation,
            } in constructors
            {
                assert_expression_is_a_type(type_annotation, context.clone())?;
                // @Task instance check etc
                // @Task check if constructor already exists
                context.clone().insert_constructor(
                    binder.clone(),
                    type_annotation.clone(),
                    adt_binder,
                );
            }
        }
        Declaration::Module { declarations } => {
            for declaration in declarations {
                evaluate_declaration(declaration, context.clone())?;
            }
        }
        Declaration::Use => unimplemented!(),
        Declaration::Foreign => unimplemented!(),
    })
}

fn substitute(
    expression: &Expression,
    environment: Environment,
    context: ModuleContext,
) -> Expression {
    match expression {
        Expression::Identifier(identifier) => environment
            .get(identifier)
            .cloned()
            .unwrap_or_else(|| expression.clone()),
        Expression::TypeLiteral | Expression::NatTypeLiteral | Expression::NatLiteral(_) => {
            expression.clone()
        }
        Expression::PiTypeLiteral {
            binder,
            domain,
            codomain,
            explicitness: _,
        } => {
            let (binder, domain, codomain) =
                abstraction_substitute(binder.as_ref(), domain, codomain, environment, context);
            Expression::PiTypeLiteral {
                binder,
                domain: Box::new(domain),
                codomain: Box::new(codomain),
                explicitness: Explicitness::Explicit,
            }
        }
        Expression::LambdaLiteral {
            binder,
            parameter_type_annotation,
            body,
            body_type_annotation: _,
            explicitness: _,
        } => {
            // @Bug unchecked @Temporary unwrap
            let (binder, parameter_type, body) = abstraction_substitute(
                Some(binder),
                parameter_type_annotation.as_ref().unwrap(),
                body,
                environment,
                context,
            );
            // @Note this unwrap is safe, but the API of abstraction_substitute sucks
            Expression::LambdaLiteral {
                binder: binder.unwrap(),
                parameter_type_annotation: Some(Box::new(parameter_type)),
                body: Box::new(body),
                body_type_annotation: None,
                // @Temporary
                explicitness: Explicitness::Explicit,
            }
        }
        Expression::Application {
            expression,
            argument,
            explicitness: _,
        } => Expression::Application {
            expression: Box::new(substitute(expression, environment.clone(), context.clone())),
            argument: Box::new(substitute(argument, environment, context)),
            // @Temporary
            explicitness: Explicitness::Explicit,
        },
        Expression::Hole(_) => unimplemented!(),
        Expression::UseIn => unimplemented!(),
        Expression::Case => unimplemented!(),
    }
}

pub fn infer_type(expression: &Expression, context: ModuleContext) -> Result<hir::Expression> {
    Ok(match expression {
        Expression::Identifier(identifier) => context
            .lookup_type(identifier)
            .ok_or_else(|| Error::UndefinedBinding(identifier.clone()))?,
        Expression::TypeLiteral | Expression::NatTypeLiteral => Expression::TypeLiteral,
        Expression::NatLiteral(_) => Expression::NatTypeLiteral,
        Expression::PiTypeLiteral { .. } => Expression::TypeLiteral,
        Expression::LambdaLiteral {
            binder,
            parameter_type_annotation,
            body,
            body_type_annotation,
            // @Task
            explicitness: _,
        } => {
            // @Bug unhandled @Temporary unwrap
            let parameter_type: &Expression = parameter_type_annotation.as_ref().unwrap();
            assert_expression_is_a_type(&parameter_type, context.clone())?;
            let infered_body_type = infer_type(
                body,
                context
                    .clone()
                    .extend_with_neutral_binding(binder.clone(), parameter_type.clone()),
            )?;
            if let Some(body_type_annotation) = body_type_annotation {
                assert_expressions_are_equal(&infered_body_type, body_type_annotation, context)?;
            }
            Expression::PiTypeLiteral {
                binder: Some(binder.clone()),
                domain: Box::new(parameter_type.clone()),
                codomain: Box::new(infered_body_type),
                // @Task
                explicitness: Explicitness::Explicit,
            }
        }
        Expression::Application {
            expression,
            argument,
            // @Task
            explicitness: _,
        } => {
            // @Task verify type_of_expression is already normalized
            // @Note maybe use dbg-macro?
            let type_of_expression = infer_type(expression, context.clone())?;
            match normalize(&type_of_expression, context.clone())? {
                Expression::PiTypeLiteral {
                    binder,
                    domain,
                    codomain,
                    explicitness: _,
                } => {
                    let argument_type = infer_type(argument, context.clone())?;
                    assert_expressions_are_equal(&domain, &argument_type, context.clone())?;
                    match binder {
                        Some(binder) => substitute(
                            &codomain,
                            {
                                let mut environment = HashMap::new();
                                environment.insert(binder, argument.as_ref().clone());
                                Rc::new(environment)
                            },
                            context,
                        ),
                        None => *codomain,
                    }
                }
                expression => return Err(Error::FunctionExpected(expression)),
            }
        }
        Expression::Hole(_) => unimplemented!(),
        Expression::UseIn => unimplemented!(),
        Expression::Case => unimplemented!(),
    })
}

// enum Normalized {}
// type Value = Expression<Normalized>;

// @Beacon @Task use type-tagging: tag the hir::Expression with a P: Phase type parameter which for now
// differenciates between Initial and Normalized
// @Question take expression by value?
pub fn normalize(expression: &Expression, context: ModuleContext) -> Result<Expression> {
    Ok(match expression {
        Expression::Identifier(identifier) => context
            .clone()
            .lookup_value(identifier)
            .ok_or_else(|| Error::UndefinedBinding(identifier.clone()))?
            .map(|expression| normalize(&expression, context))
            .unwrap_or_else(|| Ok(expression.clone()))?,
        Expression::Application {
            expression,
            argument,
            // @Task
            explicitness: _,
        } => {
            let argument = normalize(argument, context.clone())?;
            match normalize(expression, context.clone())? {
                Expression::LambdaLiteral {
                    binder,
                    parameter_type_annotation: _,
                    body,
                    body_type_annotation: _,
                    explicitness: _,
                } => normalize(
                    &substitute(
                        &body,
                        {
                            let mut environment = HashMap::new();
                            environment.insert(binder, argument);
                            Rc::new(environment)
                        },
                        context.clone(),
                    ),
                    context,
                )?,
                expression => Expression::Application {
                    expression: Box::new(expression),
                    argument: Box::new(argument),
                    // @Temporary
                    explicitness: Explicitness::Explicit,
                },
            }
        }
        Expression::TypeLiteral | Expression::NatTypeLiteral | Expression::NatLiteral(_) => {
            expression.clone()
        }
        Expression::PiTypeLiteral {
            binder,
            domain,
            codomain,
            explicitness: _,
        } => {
            let (domain, codomain) =
                normalize_abstraction(binder.as_ref(), domain, codomain, context)?;
            Expression::PiTypeLiteral {
                binder: binder.clone(),
                domain: Box::new(domain),
                codomain: Box::new(codomain),
                // @Temporary
                explicitness: Explicitness::Explicit,
            }
        }
        Expression::LambdaLiteral {
            binder,
            parameter_type_annotation,
            body,
            body_type_annotation: _,
            explicitness: _,
        } => {
            // @Bug unchecked @Temporary unwrap
            let (parameter_type, body) = normalize_abstraction(
                Some(binder),
                parameter_type_annotation.as_ref().unwrap(),
                body,
                context,
            )?;
            Expression::LambdaLiteral {
                binder: binder.clone(),
                parameter_type_annotation: Some(Box::new(parameter_type)),
                body: Box::new(body),
                body_type_annotation: None,
                // @Temporary
                explicitness: Explicitness::Explicit,
            }
        }
        Expression::Hole(_) => unimplemented!(),
        Expression::UseIn => unimplemented!(),
        Expression::Case => unimplemented!(),
    })
}

pub fn assert_expression_is_a_type(expression: &Expression, context: ModuleContext) -> Result<()> {
    let type_of_expression = infer_type(expression, context.clone())?;
    assert_expressions_are_equal(&type_of_expression, &Expression::TypeLiteral, context)
}

fn assert_expressions_are_equal(
    left: &Expression,
    right: &Expression,
    context: ModuleContext,
) -> Result<()> {
    if !normalizing_equal(left, right, context)? {
        Err(Error::ExpressionsNotEqual(left.clone(), right.clone()))
    } else {
        Ok(())
    }
}

fn normalizing_equal(
    left: &Expression,
    right: &Expression,
    context: ModuleContext,
) -> Result<bool> {
    Ok(equal(
        &normalize(left, context.clone())?,
        &normalize(right, context.clone())?,
        context,
    ))
}

fn equal(left: &Expression, right: &Expression, context: ModuleContext) -> bool {
    match (left, right) {
        (Expression::Identifier(left), Expression::Identifier(right)) => left == right,
        (
            Expression::Application {
                expression: left_expression,
                argument: left_argument,
                explicitness: _,
            },
            Expression::Application {
                expression: right_expression,
                argument: right_argument,
                explicitness: _,
            },
        ) => {
            equal(left_expression, right_expression, context.clone())
                && equal(left_argument, right_argument, context)
        }
        (Expression::TypeLiteral, Expression::TypeLiteral) => true,
        (Expression::NatTypeLiteral, Expression::NatTypeLiteral) => true,
        (Expression::NatLiteral(left), Expression::NatLiteral(right)) => left == right,
        (
            Expression::PiTypeLiteral {
                binder: binder1,
                domain: domain1,
                codomain: codomain1,
                explicitness: _,
            },
            Expression::PiTypeLiteral {
                binder: binder2,
                domain: domain2,
                codomain: codomain2,
                explicitness: _,
            },
        ) => abstraction_equal(
            binder1.as_ref(),
            domain1,
            codomain1,
            binder2.as_ref(),
            domain2,
            codomain2,
            context,
        ),
        (
            Expression::LambdaLiteral {
                binder: binder1,
                parameter_type_annotation: parameter_type_annotation1,
                body: body1,
                body_type_annotation: _,
                explicitness: _,
            },
            Expression::LambdaLiteral {
                binder: binder2,
                parameter_type_annotation: parameter_type_annotation2,
                body: body2,
                body_type_annotation: _,
                explicitness: _,
            },
        ) => {
            // @Bug unchecked @Temporary unwrap
            abstraction_equal(
                Some(binder1),
                parameter_type_annotation1.as_ref().unwrap(),
                body1,
                Some(binder2),
                parameter_type_annotation2.as_ref().unwrap(),
                body2,
                context,
            )
        }
        // @Note this is really bad when we decide to add new stuff! but there is no
        // viable alternative really :/
        _ => false,
    }
}

// @Task impl; move it to future Value
// @Note @Temporary panics if self not a type
// @Note panics if a binder does not exist
// fn inhabited(&self, context: MutCtx) -> Result<bool> {
//     Ok(match self {
//         Expr::Type => true,
//         Expr::Pi(Abstraction { input, output, .. }) => {
//             // @Bug crashes on e.g. `(-> A # A)` because we need to extend
//             // context
//             output.inhabited(context.clone())? || !input.inhabited(context)?
//         }
//         // @Note because we treat Self=Expr as Self=Value, this is a neutral Var
//         // so its value (repr) is None. It cannot be a parameter, so ist must be
//         // an ADT i think
//         Expr::Var(binding) => {
//             if context.clone().adt(binding) {
//                 let constructors = context.clone().constructors(binding);
//                 // @Note sad: bc of error handling, we cannot use `all` but
//                 // only `try_fold`
//                 !constructors.is_empty()
//                     && constructors
//                         .into_iter()
//                         .try_fold(true, |acc, constructor| {
//                             Ok(acc
//                                 && context
//                                     .clone()
//                                     .lookup_type(&constructor)
//                                     .unwrap()
//                                     .inhabited(context.clone())?)
//                         })?
//             } else {
//                 // constructor @Bug not always a constructor but also neutral variable
//                 // @Note not extensible, hacky
//                 return Err(Error::ExpressionsNotEqual(
//                     Expr::Type,
//                     context.lookup_type(binding).unwrap(),
//                 ));
//             }
//         }
//         // @Bug hit: neutral application, e.g. `Maybe` (after lookup)
//         x => {
//             // @Temporary
//             eprintln!("{:?}", x);
//             unreachable!()
//         }
//     })
// }

// @Task impl; move to Value
// fn instance() {}

// @Temporary signature
// @Note bad signature (deviation between pi and lambda)
fn abstraction_substitute(
    binder: Option<&Identifier>,
    parameter: &Expression,
    expression: &Expression,
    environment: Environment,
    context: ModuleContext,
) -> (Option<Identifier>, Expression, Expression) {
    let parameter = substitute(parameter, environment.clone(), context.clone());

    let (refreshed_binder, environment) = match binder {
        Some(binder) => {
            let refreshed_binder = binder.refresh(context.clone());
            let mut environment = environment.as_ref().clone();
            environment.insert(
                binder.clone(),
                Expression::Identifier(refreshed_binder.clone()),
            );
            (Some(refreshed_binder), Rc::new(environment))
        }
        None => (None, environment),
    };
    (
        refreshed_binder,
        parameter,
        substitute(expression, environment, context),
    )
}

// @Temporary signature
fn normalize_abstraction(
    binder: Option<&Identifier>,
    parameter: &Expression,
    expression: &Expression,
    context: ModuleContext,
) -> Result<(Expression, Expression)> {
    let parameter = normalize(parameter, context.clone())?;
    Ok((
        parameter.clone(),
        match binder {
            Some(binder) => normalize(
                expression,
                context.extend_with_neutral_binding(binder.clone(), parameter),
            )?,
            // @Temporary expensive
            None => expression.clone(),
        },
    ))
}

// @Temporary signature
fn abstraction_equal(
    binder1: Option<&Identifier>,
    parameter1: &Expression,
    expression1: &Expression,
    binder2: Option<&Identifier>,
    parameter2: &Expression,
    expression2: &Expression,
    context: ModuleContext,
) -> bool {
    return equal(parameter1, parameter2, context.clone())
        && match (binder1, binder2) {
            (Some(binder1), Some(binder2)) => equal(
                expression1,
                &substitute(
                    expression2,
                    {
                        let mut environment = HashMap::new();
                        environment
                            .insert(binder2.clone(), Expression::Identifier(binder1.clone()));
                        Rc::new(environment)
                    },
                    context.clone(),
                ),
                context,
            ),
            (Some(binder1), None) => equal(
                expression1,
                expression2,
                context.extend_with_neutral_binding(binder1.clone(), parameter1.clone()),
            ),
            (None, Some(binder2)) => equal(
                expression1,
                expression2,
                context.extend_with_neutral_binding(binder2.clone(), parameter2.clone()),
            ),
            (None, None) => equal(expression1, expression2, context),
        };
}
