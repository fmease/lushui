//! First type-checker plus backend (tree-walk interpreter).
//!
//! The code in here is all temporary and needs to be reworked quite a bit!

mod context;
mod error;

use std::{collections::HashMap, rc::Rc};

use crate::hir::{self, Declaration, Expression, Identifier, RefreshState};
use crate::parser::Explicitness;
pub use context::initial;
use context::{Environment, MutCtx};
use error::{Error, Result};

// @Note module code (hir::Declaration::Module)
// used for recursion
// @Question move into constructor?
// fn register_binders(&mut self) -> Result<()> {
//     #[cfg(debug_assertions)]
//     assert!(!self.registered_binders);

//     for declaration in &self.declarations {
//         match declaration {
//             Declaration::Data {
//                 binder,
//                 constructors,
//                 ..
//             } => {
//                 let binder = binder.view_str().unwrap().to_string();
//                 Self::register_binder(&mut self.binders, binder)?;
//                 for constructor in constructors {
//                     Self::register_binder(
//                         &mut self.binders,
//                         constructor.binder.view_str().unwrap().to_string(),
//                     )?;
//                 }
//             }
//             Declaration::Let { binder, .. } => {
//                 Self::register_binder(
//                     &mut self.binders,
//                     binder.view_str().unwrap().to_string(),
//                 )?;
//             }
//             // is a temp on Declaration
//             Declaration::ExprStmt(..) => unreachable!(),
//         }
//     }

//     Ok(())
// }

//     fn register_binder(binders: &mut Vec<String>, binder: String) -> Result<()> {
//         if binders.contains(&binder) {
//             // @Task more information
//             Err(Error::AlreadyDefined)
//         } else {
//             binders.push(binder);
//             Ok(())
//         }
//     }

// @Question good naming scheme?
// @Note evaluate needs to take a reference to self.binders @Task to allow for recursion
// @Note currently `evaluate` means registering bindings in the context and also if
// we encounter the "declaration" expr_stmt, we evaluate it (a sort-of main function)
// @Note in future, the repl might evaluate expressions as `'let '_: 'hole 'infer = expr`
// or we just expose a better evaluate function
// @Note once we implement D/E-modes into repl, the signature will change to
// fn(&self, context: Context, state: State<'_>) -> Result<()> or
// if we switch to immutable contexts (which includes State):
// fn(&self, context: ImmContext) -> Result<ImmContext>
//     pub fn evaluate(&self, context: MutCtx, state: State<'_>) -> Result<Option<(Expr, Expr)>> {
//         Ok(self
//             .declarations
//             .iter()
//             .map(|declaration| declaration.evaluate(context.clone(), state))
//             .last()
//             .transpose()?
//             .flatten())
//     }
// }

// used for recursion
// @Note currently disallows declarations where the binding is recursive in its type signature
// the only example that currently makes sense to me is `'let A: A = 'Type` but it's not useful
// at all and only exist because of the special `the Type' Type'`.
// Are there any expressions that a well-typed if they recurse in their type??
// name: "recursion in type annotation"
// @Bug unusable
// pub fn register_declaration(&self, context: MutCtx, state: State<'_>) -> Result<()> {
// Ok(match self {
//     Declaration::Let { binder, type_, .. } => {
//         // @Task verify type_
//         context.insert_unresolved_binding(binder, type_)?;
//     }
//     Declaration::Data { binder, type_, constructors } => {
//         // @Task verify type_
//         context.insert_unresolved_binding(binder, type_)?;
//         // @Question should we register those????
//         // I think, we don't want to allow "recursion in type annotation"
//         //for constructor in constructors {
//         //    context.insert_unresolved_binding(constructor.binder, constructor);
//         //}
//     }
//     Declaration::ExprStmt(..) => unreachable!(),
// })

// @Task make pure returning new Context
// @Task change name to register2 b.c. we are a lazy lang, we should only do type_checking here
pub fn evaluate_declaration(
    declaration: &Declaration,
    context: MutCtx,
    state: RefreshState<'_>,
) -> Result<()> {
    Ok(match declaration {
        Declaration::Let {
            binder,
            type_annotation: _,
            expression,
        } => {
            if context.clone().contains(&binder) {
                return Err(Error::AlreadyDefined);
            }
            // @Task check against type_annotation
            let infered_type = infer_type(expression, context.clone(), state)?;
            let value = normalize(expression, context.clone(), state)?;
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
            check_is_type(type_annotation, context.clone(), state)?;
            context
                .clone()
                .insert_adt(adt_binder.clone(), type_annotation.clone());

            for hir::Constructor {
                binder,
                type_annotation,
            } in constructors
            {
                check_is_type(type_annotation, context.clone(), state)?;
                // @Task instance check etc
                // @Task check if constructor already exists
                context.clone().insert_constructor(
                    binder.clone(),
                    type_annotation.clone(),
                    adt_binder,
                );
            }
        }
        _ => unimplemented!(),
    })
}

// @Task create Value type
// pub enum _Value {
//     // NeutralVariable(Variable)
// // Type,
// // Pi,
// // Lambda, // NeutralTerm
// // NeutralApplication(Abstraction)
// //
// }

// @Task replace Environment with a single binding
// @Task optimize body (abstraction_*-calls)
fn substitute(expression: &Expression, env: Environment, state: RefreshState<'_>) -> Expression {
    match expression {
        Expression::Identifier(identifier) => env
            .get(identifier)
            .cloned()
            .unwrap_or_else(|| expression.clone()),
        Expression::TypeLiteral => Expression::TypeLiteral,
        Expression::PiLiteral {
            binder,
            parameter,
            expression,
            explicitness: _,
        } => {
            // @Bug unchecked @Temporary unwrap
            let (binder, parameter, expression) =
                abstraction_substitute(binder.as_ref().unwrap(), parameter, expression, env, state);
            Expression::PiLiteral {
                binder: Some(binder),
                parameter: Box::new(parameter),
                expression: Box::new(expression),
                explicitness: Explicitness::Explicit,
            }
        }
        Expression::LambdaLiteral {
            binder,
            parameter,
            expression,
            type_annotation: _,
            explicitness: _,
        } => {
            // @Bug unchecked @Temporary unwrap
            let (binder, parameter, expression) =
                abstraction_substitute(binder, parameter.as_ref().unwrap(), expression, env, state);
            Expression::LambdaLiteral {
                binder,
                parameter: Some(Box::new(parameter)),
                expression: Box::new(expression),
                type_annotation: None,
                // @Temporary
                explicitness: Explicitness::Explicit,
            }
        }
        Expression::Application {
            expression,
            argument,
            explicitness: _,
        } => Expression::Application {
            expression: Box::new(substitute(expression, env.clone(), state)),
            argument: Box::new(substitute(argument, env, state)),
            // @Temporary
            explicitness: Explicitness::Explicit,
        },
        Expression::Hole(_) => unimplemented!(),
        Expression::UseIn => unimplemented!(),
        Expression::Case => unimplemented!(),
    }
}

pub fn infer_type(
    expression: &Expression,
    context: MutCtx,
    state: RefreshState<'_>,
) -> Result<hir::Expression> {
    Ok(match expression {
        Expression::Identifier(identifier) => context
            .lookup_type(identifier)
            .ok_or_else(|| Error::UndefinedBinding(identifier.clone()))?,
        Expression::TypeLiteral => Expression::TypeLiteral,
        Expression::PiLiteral { .. } => Expression::TypeLiteral,
        Expression::LambdaLiteral {
            binder,
            parameter,
            expression,
            // @Task
            explicitness: _,
            type_annotation: _,
        } => {
            // @Bug unhandled @Temporary unwrap
            let parameter: &Expression = parameter.as_ref().unwrap();
            check_is_type(&parameter, context.clone(), state)?;
            let expression = infer_type(
                expression,
                context.extend_with_neutral_binding(binder.clone(), parameter.clone()),
                state,
            )?;
            Expression::PiLiteral {
                binder: Some(binder.clone()),
                parameter: Box::new(parameter.clone()),
                expression: Box::new(expression),
                // @Task
                explicitness: Explicitness::Explicit,
            }
        }
        Expression::Application {
            expression: function,
            argument,
            // @Task
            explicitness: _,
        } => {
            let (pi_binder, pi_parameter, pi_expression) =
                infer_pi(function, context.clone(), state)?;
            let argument_type = infer_type(argument, context.clone(), state)?;
            check_equal(&pi_parameter, &argument_type, context, state)?;
            substitute(
                &pi_expression,
                {
                    let mut env = HashMap::new();
                    env.insert(pi_binder, argument.as_ref().clone());
                    Rc::new(env)
                },
                state,
            )
        }
        Expression::Hole(_) => unimplemented!(),
        Expression::UseIn => unimplemented!(),
        Expression::Case => unimplemented!(),
    })
}

// @Question better name available?
// @Note @Bug we don't have a direct equivalent of Abstraction in hir
// @Task translate Abstraction into hir (maybe we don't need this function anymore?)
// @Temporary signature
fn infer_pi(
    expression: &Expression,
    ctx: MutCtx,
    state: RefreshState<'_>,
) -> Result<(Identifier, Expression, Expression)> {
    let ty = infer_type(expression, ctx.clone(), state)?;
    match normalize(&ty, ctx, state)? {
        // @Bug unchecked @Temporary unwrap
        Expression::PiLiteral {
            binder,
            parameter,
            expression,
            explicitness: _,
        } => Ok((binder.unwrap(), *parameter, *expression)),
        expression => Err(Error::FunctionExpected(expression)),
    }
}

// @Question take expression by value?
pub fn normalize(
    expression: &Expression,
    context: MutCtx,
    state: RefreshState<'_>,
) -> Result<Expression> {
    Ok(match expression {
        Expression::Identifier(identifier) => context
            .clone()
            .lookup_value(identifier)
            .ok_or_else(|| Error::UndefinedBinding(identifier.clone()))?
            .map(|expression| normalize(&expression, context, state))
            .unwrap_or_else(|| Ok(expression.clone()))?,
        Expression::Application {
            expression: expr1,
            argument: expr2,
            // @Task
            explicitness: _,
        } => {
            let expr2 = normalize(expr2, context.clone(), state)?;
            match normalize(expr1, context, state)? {
                Expression::LambdaLiteral {
                    binder,
                    parameter: _,
                    expression,
                    type_annotation: _,
                    explicitness: _,
                } => substitute(
                    &expression,
                    {
                        let mut env = HashMap::new();
                        env.insert(binder, expr2);
                        Rc::new(env)
                    },
                    state,
                ),
                expr1 => Expression::Application {
                    expression: Box::new(expr1),
                    argument: Box::new(expr2),
                    // @Temporary
                    explicitness: Explicitness::Explicit,
                },
            }
        }
        Expression::TypeLiteral => Expression::TypeLiteral,
        Expression::PiLiteral {
            binder,
            parameter,
            expression,
            explicitness: _,
        } => {
            // @Bug unchecked @Temporary unwrap
            let (binder, parameter, expression) = abstraction_normalize(
                binder.as_ref().unwrap(),
                parameter,
                expression,
                context,
                state,
            )?;
            Expression::PiLiteral {
                binder: Some(binder),
                parameter: Box::new(parameter),
                expression: Box::new(expression),
                // @Temporary
                explicitness: Explicitness::Explicit,
            }
        }
        Expression::LambdaLiteral {
            binder,
            parameter,
            expression,
            type_annotation: _,
            explicitness: _,
        } => {
            // @Bug unchecked @Temporary unwrap
            let (binder, parameter, expression) = abstraction_normalize(
                binder,
                parameter.as_ref().unwrap(),
                expression,
                context,
                state,
            )?;
            Expression::LambdaLiteral {
                binder,
                parameter: Some(Box::new(parameter)),
                expression: Box::new(expression),
                type_annotation: None,
                // @Temporary
                explicitness: Explicitness::Explicit,
            }
        }
        Expression::Hole(_) => unimplemented!(),
        Expression::UseIn => unimplemented!(),
        Expression::Case => unimplemented!(),
    })
}

pub fn check_is_type(
    expression: &Expression,
    context: MutCtx,
    state: RefreshState<'_>,
) -> Result<()> {
    let type_ = infer_type(expression, context.clone(), state)?;
    check_equal(&type_, &Expression::TypeLiteral, context, state)?;
    Ok(())
}

fn check_equal(
    left: &Expression,
    right: &Expression,
    context: MutCtx,
    state: RefreshState<'_>,
) -> Result<()> {
    if !normalizing_equal(left, right, context, state)? {
        Err(Error::ExpressionsNotEqual(left.clone(), right.clone()))
    } else {
        Ok(())
    }
}

fn normalizing_equal(
    left: &Expression,
    right: &Expression,
    context: MutCtx,
    state: RefreshState<'_>,
) -> Result<bool> {
    Ok(equal(
        &normalize(left, context.clone(), state)?,
        &normalize(right, context.clone(), state)?,
        context,
        state,
    ))
}

fn equal(expr1: &Expression, expr2: &Expression, context: MutCtx, state: RefreshState<'_>) -> bool {
    match (expr1, expr2) {
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
            equal(left_expression, right_expression, context.clone(), state)
                && equal(left_argument, right_argument, context, state)
        }
        (Expression::TypeLiteral, Expression::TypeLiteral) => true,
        (
            Expression::PiLiteral {
                binder: binder1,
                parameter: parameter1,
                expression: expression1,
                explicitness: _,
            },
            Expression::PiLiteral {
                binder: binder2,
                parameter: parameter2,
                expression: expression2,
                explicitness: _,
            },
        ) => {
            // @Bug unchecked @Temporary unwrap
            abstraction_equal(
                binder1.as_ref().unwrap(),
                parameter1,
                expression1,
                binder2.as_ref().unwrap(),
                parameter2,
                expression2,
                context,
                state,
            )
        }
        (
            Expression::LambdaLiteral {
                binder: binder1,
                parameter: parameter1,
                expression: expression1,
                type_annotation: _,
                explicitness: _,
            },
            Expression::LambdaLiteral {
                binder: binder2,
                parameter: parameter2,
                expression: expression2,
                type_annotation: _,
                explicitness: _,
            },
        ) => {
            // @Bug unchecked @Temporary unwrap
            abstraction_equal(
                binder1,
                parameter1.as_ref().unwrap(),
                expression1,
                binder2,
                parameter2.as_ref().unwrap(),
                expression2,
                context,
                state,
            )
        }
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
fn abstraction_substitute(
    binder: &Identifier,
    parameter: &Expression,
    expression: &Expression,
    env: Environment,
    state: RefreshState<'_>,
) -> (Identifier, Expression, Expression) {
    let binder = binder.refresh(state);
    (
        binder.clone(), // @Note redundant
        substitute(parameter, env.clone(), state),
        substitute(
            expression,
            {
                let mut env = env.as_ref().clone();
                env.insert(binder.clone(), Expression::Identifier(binder));
                Rc::new(env)
            },
            state,
        ),
    )
}

// @Temporary signature
fn abstraction_normalize(
    binder: &Identifier,
    parameter: &Expression,
    expression: &Expression,
    context: MutCtx,
    state: RefreshState<'_>,
) -> Result<(Identifier, Expression, Expression)> {
    let parameter = normalize(parameter, context.clone(), state)?;
    Ok((
        binder.clone(),    // @Note redundant
        parameter.clone(), // @Note redundant
        normalize(
            expression,
            context.extend_with_neutral_binding(binder.clone(), parameter),
            state,
        )?,
    ))
}

// @Temporary signature
fn abstraction_equal(
    binder1: &Identifier,
    parameter1: &Expression,
    expression1: &Expression,
    binder2: &Identifier,
    parameter2: &Expression,
    expression2: &Expression,
    context: MutCtx,
    state: RefreshState<'_>,
) -> bool {
    equal(parameter1, parameter2, context.clone(), state)
        && equal(
            &expression1,
            &substitute(
                expression2,
                {
                    let mut env = HashMap::new();
                    env.insert(binder2.clone(), Expression::Identifier(binder1.clone()));
                    Rc::new(env)
                },
                state,
            ),
            context,
            state,
        )
}
