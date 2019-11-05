//! First type-checker plus backend (tree-walk interpreter).
//!
//! The code in here is all temporary and needs to be reworked quite a bit!

mod context;
mod error;

use crate::hir::{self, Declaration, Expression, Identifier, RefreshState};
use crate::parser::Explicitness;
pub use context::initial;
use context::MutCtx;
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
            assert_expression_is_a_type(type_annotation, context.clone(), state)?;
            context
                .clone()
                .insert_adt(adt_binder.clone(), type_annotation.clone());

            for hir::Constructor {
                binder,
                type_annotation,
            } in constructors
            {
                assert_expression_is_a_type(type_annotation, context.clone(), state)?;
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

// @Task optimize body (abstraction_*-calls)
// @Note bad naming
fn substitute(
    expression: &Expression,
    substitution_binder: Identifier,
    substitution: Expression,
    state: RefreshState<'_>,
) -> Expression {
    match expression {
        Expression::Identifier(identifier) => {
            if identifier == &substitution_binder {
                substitution
            } else {
                expression.clone()
            }
        }
        Expression::TypeLiteral => Expression::TypeLiteral,
        Expression::PiTypeLiteral {
            binder,
            domain,
            codomain,
            explicitness: _,
        } => {
            // @Bug unchecked @Temporary unwrap
            let (binder, domain, codomain) = abstraction_substitute(
                binder.as_ref().unwrap(),
                domain,
                codomain,
                substitution_binder,
                substitution,
                state,
            );
            Expression::PiTypeLiteral {
                binder: Some(binder),
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
                binder,
                parameter_type_annotation.as_ref().unwrap(),
                body,
                substitution_binder,
                substitution,
                state,
            );
            Expression::LambdaLiteral {
                binder,
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
            expression: Box::new(substitute(
                expression,
                substitution_binder.clone(),
                substitution.clone(),
                state,
            )),
            argument: Box::new(substitute(
                argument,
                substitution_binder,
                substitution,
                state,
            )),
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
        Expression::PiTypeLiteral { .. } => Expression::TypeLiteral,
        Expression::LambdaLiteral {
            binder,
            parameter_type_annotation,
            body,
            // @Task
            explicitness: _,
            body_type_annotation: _,
        } => {
            // @Bug unhandled @Temporary unwrap
            let parameter_type: &Expression = parameter_type_annotation.as_ref().unwrap();
            assert_expression_is_a_type(&parameter_type, context.clone(), state)?;
            let body_type = infer_type(
                body,
                context.extend_with_neutral_binding(binder.clone(), parameter_type.clone()),
                state,
            )?;
            Expression::PiTypeLiteral {
                binder: Some(binder.clone()),
                domain: Box::new(parameter_type.clone()),
                codomain: Box::new(body_type),
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
            let type_of_expression = infer_type(expression, context.clone(), state)?;
            match normalize(&type_of_expression, context.clone(), state)? {
                Expression::PiTypeLiteral {
                    binder,
                    domain,
                    codomain,
                    explicitness: _,
                } => {
                    let argument_type = infer_type(argument, context.clone(), state)?;
                    assert_expressions_are_equal(&domain, &argument_type, context, state)?;
                    substitute(
                        &codomain,
                        // @Bug unchecked @Temporary unwrap
                        binder.unwrap(),
                        argument.as_ref().clone(),
                        state,
                    )
                }
                expression => return Err(Error::FunctionExpected(expression)),
            }
        }
        Expression::Hole(_) => unimplemented!(),
        Expression::UseIn => unimplemented!(),
        Expression::Case => unimplemented!(),
    })
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
                    parameter_type_annotation: _,
                    body,
                    body_type_annotation: _,
                    explicitness: _,
                } => substitute(&body, binder, expr2, state),
                expr1 => Expression::Application {
                    expression: Box::new(expr1),
                    argument: Box::new(expr2),
                    // @Temporary
                    explicitness: Explicitness::Explicit,
                },
            }
        }
        Expression::TypeLiteral => Expression::TypeLiteral,
        Expression::PiTypeLiteral {
            binder,
            domain,
            codomain,
            explicitness: _,
        } => {
            // @Bug unchecked @Temporary unwrap
            let (domain, codomain) =
                normalize_abstraction(binder.as_ref().unwrap(), domain, codomain, context, state)?;
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
                binder,
                parameter_type_annotation.as_ref().unwrap(),
                body,
                context,
                state,
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

pub fn assert_expression_is_a_type(
    expression: &Expression,
    context: MutCtx,
    state: RefreshState<'_>,
) -> Result<()> {
    let type_of_expression = infer_type(expression, context.clone(), state)?;
    assert_expressions_are_equal(
        &type_of_expression,
        &Expression::TypeLiteral,
        context,
        state,
    )
}

fn assert_expressions_are_equal(
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

fn equal(left: &Expression, right: &Expression, context: MutCtx, state: RefreshState<'_>) -> bool {
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
            equal(left_expression, right_expression, context.clone(), state)
                && equal(left_argument, right_argument, context, state)
        }
        (Expression::TypeLiteral, Expression::TypeLiteral) => true,
        (
            Expression::PiTypeLiteral {
                binder: binder1,
                domain: parameter1,
                codomain: expression1,
                explicitness: _,
            },
            Expression::PiTypeLiteral {
                binder: binder2,
                domain: parameter2,
                codomain: expression2,
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
                parameter_type_annotation: parameter1,
                body: expression1,
                body_type_annotation: _,
                explicitness: _,
            },
            Expression::LambdaLiteral {
                binder: binder2,
                parameter_type_annotation: parameter2,
                body: expression2,
                body_type_annotation: _,
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
    substitution_binder: Identifier,
    substitution: Expression,
    state: RefreshState<'_>,
) -> (Identifier, Expression, Expression) {
    let binder = binder.refresh(state);
    (
        binder.clone(),
        substitute(parameter, substitution_binder, substitution, state),
        substitute(
            expression,
            binder.clone(),
            Expression::Identifier(binder),
            state,
        ),
    )
}

// @Temporary signature
fn normalize_abstraction(
    binder: &Identifier,
    parameter: &Expression,
    expression: &Expression,
    context: MutCtx,
    state: RefreshState<'_>,
) -> Result<(Expression, Expression)> {
    let parameter = normalize(parameter, context.clone(), state)?;
    Ok((
        parameter.clone(),
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
                binder2.clone(),
                Expression::Identifier(binder1.clone()),
                state,
            ),
            context,
            state,
        )
}
