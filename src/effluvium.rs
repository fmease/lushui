//! First type-checker plus backend (tree-walk interpreter).
//!
//! The code in here is all temporary and needs to be reworked quite a bit!

// @Task rename to scope
mod context;
mod error;

use crate::hir::{self, Declaration, Expression, Identifier};
use crate::parser::Explicitness;
use context::Environment;
pub use context::ModuleScope;
use error::{Error, Result};

use std::collections::HashMap;
use std::rc::Rc;

// @Temporary @Bug we don't do non-trivial type inference yet and thus, type parameters of lambda literals
// must be annotated with a type. And since we don't have a dedicated Error::InternalCompilerError yet,
// we use this constant for an error message:
const MISSING_ANNOTATION: &str = "(compiler bug) currently lambda literal parameters must be type-annotated";

// @Task make pure returning new Context
// @Question this is eager evaluation, right?
// @Task handle out of order declarations and recursions (you need to go through declarations twice) and
// only register the names plus types first (there is gonna be scope.insert_unresolved_binding)
pub fn evaluate_declaration(declaration: &Declaration, scope: ModuleScope) -> Result<()> {
    Ok(match declaration {
        Declaration::Let {
            binder,
            type_annotation,
            expression,
        } => {
            if scope.clone().contains(&binder) {
                return Err(Error::AlreadyDefined(binder.clone()));
            }
            let infered_type = infer_type(expression, scope.clone())?;
            assert_expressions_are_equal(type_annotation, &infered_type, scope.clone())?;
            let value = normalize(expression, scope.clone())?;
            scope.insert_binding(binder.clone(), infered_type, value);
        }
        Declaration::Data {
            binder: data_type_binder,
            type_annotation,
            constructors,
        } => {
            // @Task @Beacon move logic to context.rs?
            if scope.clone().contains(&data_type_binder) {
                return Err(Error::AlreadyDefined(data_type_binder.clone()));
            }
            let r#type = normalize(type_annotation, scope.clone())?;
            assert_expression_is_a_type(&r#type, scope.clone())?;

            scope
                .clone()
                .insert_data_type(data_type_binder.clone(), r#type.clone());

            for hir::Constructor {
                binder,
                type_annotation,
            } in constructors
            {
                let r#type = normalize(type_annotation, scope.clone())?;
                assert_expression_is_a_type(&r#type, scope.clone())?;

                assert_constructor_is_instance_of_type(
                    binder.clone(),
                    type_annotation,
                    data_type_binder.clone(),
                    scope.clone(),
                )?;

                // @Task check if constructor already exists @Beacon @Note do it inside of insert_constructor
                // @Task @Beacon move logic to context.rs?
                if scope.clone().contains(&binder) {
                    return Err(Error::AlreadyDefined(binder.clone()));
                }
                scope
                    .clone()
                    .insert_constructor(binder.clone(), r#type.clone(), data_type_binder);
            }
        }
        Declaration::Module { declarations } => {
            for declaration in declarations {
                evaluate_declaration(declaration, scope.clone())?;
            }
        }
        Declaration::Use => unimplemented!(),
        Declaration::Foreign => unimplemented!(),
    })
}

fn substitute(expression: &Expression, environment: Environment, scope: ModuleScope) -> Expression {
    match expression {
        Expression::Identifier {
            identifier,
            data: (),
        } => environment
            .get(identifier)
            .cloned()
            .unwrap_or_else(|| expression.clone()),
        Expression::TypeLiteral { data: () }
        | Expression::NatTypeLiteral { data: () }
        | Expression::NatLiteral { value: _, data: () } => expression.clone(),
        Expression::PiTypeLiteral {
            binder,
            domain,
            codomain,
            explicitness: _,
            data: (),
        } => {
            let (binder, domain, codomain) =
                abstraction_substitute(binder.as_ref(), domain, codomain, environment, scope);
            Expression::PiTypeLiteral {
                binder,
                domain: Box::new(domain),
                codomain: Box::new(codomain),
                explicitness: Explicitness::Explicit,
                data: (),
            }
        }
        Expression::LambdaLiteral {
            binder,
            parameter_type_annotation,
            body,
            body_type_annotation: _,
            explicitness: _,
            data: (),
        } => {
            // @Bug unchecked @Temporary unwrap
            let (binder, parameter_type, body) = abstraction_substitute(
                Some(binder),
                parameter_type_annotation.as_ref().expect(MISSING_ANNOTATION),
                body,
                environment,
                scope,
            );
            // @Note this unwrap is safe, but the API of abstraction_substitute sucks
            Expression::LambdaLiteral {
                binder: binder.unwrap(),
                parameter_type_annotation: Some(Box::new(parameter_type)),
                body: Box::new(body),
                body_type_annotation: None,
                // @Temporary
                explicitness: Explicitness::Explicit,
                data: (),
            }
        }
        Expression::Application {
            expression,
            argument,
            explicitness: _,
            data: (),
        } => Expression::Application {
            expression: Box::new(substitute(expression, environment.clone(), scope.clone())),
            argument: Box::new(substitute(argument, environment, scope)),
            // @Temporary
            explicitness: Explicitness::Explicit,
            data: (),
        },
        Expression::Hole { tag: _, data: () } => unimplemented!(),
        Expression::UseIn { data: () } => unimplemented!(),
        Expression::Case { data: () } => unimplemented!(),
    }
}

pub fn infer_type(expression: &Expression, scope: ModuleScope) -> Result<hir::Expression> {
    Ok(match expression {
        Expression::Identifier {
            identifier,
            data: (),
        } => scope
            .lookup_type(identifier)
            .ok_or_else(|| Error::UndefinedBinding(identifier.clone()))?,
        Expression::TypeLiteral { data: () } | Expression::NatTypeLiteral { data: () } => {
            Expression::TypeLiteral { data: () }
        }
        Expression::NatLiteral { value: _, data: () } => Expression::NatTypeLiteral { data: () },
        Expression::PiTypeLiteral {
            binder,
            domain,
            codomain,
            ..
        } => {
            // ensure domain and codomain are are well-typed
            // @Question why do we need to this? shouldn't this be already handled if
            // `expression` (parameter of `infer_type`) has been normalized?
            assert_expression_is_a_type(domain, scope.clone())?;
            // @Note expensive clones
            assert_expression_is_a_type(
                codomain,
                if let Some(binder) = binder {
                    scope.extend_with_parameter(binder.clone(), domain.as_ref().clone())
                } else {
                    scope
                },
            )?;

            Expression::TypeLiteral { data: () }
        }
        Expression::LambdaLiteral {
            binder,
            parameter_type_annotation,
            body,
            body_type_annotation,
            // @Task
            explicitness: _,
            data: (),
        } => {
            // @Bug unhandled @Temporary unwrap
            let parameter_type: &Expression = parameter_type_annotation.as_ref().expect(MISSING_ANNOTATION);
            assert_expression_is_a_type(&parameter_type, scope.clone())?;
            let scope = scope
                .clone()
                .extend_with_parameter(binder.clone(), parameter_type.clone());
            let infered_body_type = infer_type(body, scope.clone())?;
            if let Some(body_type_annotation) = body_type_annotation {
                // @Question should we assert_expression_is_type(body_type_annotation) before this?
                assert_expressions_are_equal(body_type_annotation, &infered_body_type, scope)?;
            }
            Expression::PiTypeLiteral {
                binder: Some(binder.clone()),
                domain: Box::new(parameter_type.clone()),
                codomain: Box::new(infered_body_type),
                // @Task
                explicitness: Explicitness::Explicit,
                data: (),
            }
        }
        Expression::Application {
            expression,
            argument,
            // @Task
            explicitness: _,
            data: (),
        } => {
            // @Task verify type_of_expression is already normalized
            // @Note maybe use dbg-macro?
            let type_of_expression = infer_type(expression, scope.clone())?;
            match normalize(&type_of_expression, scope.clone())? {
                Expression::PiTypeLiteral {
                    binder,
                    domain,
                    codomain,
                    explicitness: _,
                    data: (),
                } => {
                    let argument_type = infer_type(argument, scope.clone())?;
                    assert_expressions_are_equal(&domain, &argument_type, scope.clone())?;
                    match binder {
                        Some(binder) => substitute(
                            &codomain,
                            {
                                let mut environment = HashMap::new();
                                environment.insert(binder, argument.as_ref().clone());
                                Rc::new(environment)
                            },
                            scope,
                        ),
                        None => *codomain,
                    }
                }
                expression => {
                    return Err(Error::FunctionExpected {
                        actual: expression,
                        // @Note expensive clone
                        argument: argument.as_ref().clone(),
                    });
                }
            }
        }
        Expression::Hole { tag: _, data: () } => unimplemented!(),
        Expression::UseIn { data: () } => unimplemented!(),
        Expression::Case { data: () } => unimplemented!(),
    })
}

// enum Normalized {}
// type Value = Expression<Normalized>;

// @Beacon @Task use type-tagging: tag the hir::Expression with a P: Phase type parameter which for now
// differenciates between Initial and Normalized
// @Question take expression by value?
pub fn normalize(expression: &Expression, scope: ModuleScope) -> Result<Expression> {
    Ok(match expression {
        Expression::Identifier {
            identifier,
            data: (),
        } => scope
            .clone()
            .lookup_value(identifier)
            .ok_or_else(|| Error::UndefinedBinding(identifier.clone()))?
            .map(|expression| normalize(&expression, scope))
            .unwrap_or_else(|| Ok(expression.clone()))?,
        Expression::Application {
            expression,
            argument,
            // @Task
            explicitness: _,
            data: (),
        } => {
            let argument = normalize(argument, scope.clone())?;
            match normalize(expression, scope.clone())? {
                Expression::LambdaLiteral {
                    binder,
                    parameter_type_annotation: _,
                    body,
                    body_type_annotation: _,
                    explicitness: _,
                    data: (),
                } => normalize(
                    &substitute(
                        &body,
                        {
                            let mut environment = HashMap::new();
                            environment.insert(binder, argument);
                            Rc::new(environment)
                        },
                        scope.clone(),
                    ),
                    scope,
                )?,
                expression => Expression::Application {
                    expression: Box::new(expression),
                    argument: Box::new(argument),
                    // @Temporary
                    explicitness: Explicitness::Explicit,
                    data: (),
                },
            }
        }
        Expression::TypeLiteral { data: () }
        | Expression::NatTypeLiteral { data: () }
        | Expression::NatLiteral { value: _, data: () } => expression.clone(),
        Expression::PiTypeLiteral {
            binder,
            domain,
            codomain,
            explicitness: _,
            data: (),
        } => {
            let (domain, codomain) =
                normalize_abstraction(binder.as_ref(), domain, codomain, scope)?;
            Expression::PiTypeLiteral {
                binder: binder.clone(),
                domain: Box::new(domain),
                codomain: Box::new(codomain),
                // @Temporary
                explicitness: Explicitness::Explicit,
                data: (),
            }
        }
        Expression::LambdaLiteral {
            binder,
            parameter_type_annotation,
            body,
            body_type_annotation: _,
            explicitness: _,
            data: (),
        } => {
            // @Bug unchecked @Temporary unwrap
            let (parameter_type, body) = normalize_abstraction(
                Some(binder),
                parameter_type_annotation.as_ref().expect(MISSING_ANNOTATION),
                body,
                scope,
            )?;
            Expression::LambdaLiteral {
                binder: binder.clone(),
                parameter_type_annotation: Some(Box::new(parameter_type)),
                body: Box::new(body),
                body_type_annotation: None,
                // @Temporary
                explicitness: Explicitness::Explicit,
                data: (),
            }
        }
        Expression::Hole { tag: _, data: () } => unimplemented!(),
        Expression::UseIn { data: () } => unimplemented!(),
        Expression::Case { data: () } => unimplemented!(),
    })
}

fn assert_expression_is_a_type(expression: &Expression, scope: ModuleScope) -> Result<()> {
    let type_of_expression = infer_type(expression, scope.clone())?;
    assert_expressions_are_equal(
        &Expression::TypeLiteral { data: () },
        &type_of_expression,
        scope,
    )
}

/// left is expected, right is actual
fn assert_expressions_are_equal(
    left: &Expression,
    right: &Expression,
    scope: ModuleScope,
) -> Result<()> {
    if !normalizing_equal(left, right, scope)? {
        Err(Error::ExpressionsNotEqual {
            expected: left.clone(),
            actual: right.clone(),
        })
    } else {
        Ok(())
    }
}

// @Update
// @Task @Beacon
// @Note maybe we want to return an enum with more information:
// * does the constructor have existential type variable (in respect to the type),
//   which means it has type params that are not "part of" the type
// * is it specialized in respect to the type i.e. it does not use some of the
//   type params of the respective type
fn assert_constructor_is_instance_of_type(
    constructor_name: Identifier,
    constructor: &Expression,
    type_name: Identifier,
    scope: ModuleScope,
) -> Result<()> {
    if !constructor_is_instance_of_type(constructor, type_name, scope) {
        Err(Error::InvalidConstructor {
            name: constructor_name,
        })
    } else {
        Ok(())
    }
}

// @Note currently allows existential quantification and specialized instances
// we might not want to allow them at first (implementation difficulty of case analysis)
// @Question do we need access to the module scope?
fn constructor_is_instance_of_type(
    constructor: &Expression,
    type_name: Identifier,
    scope: ModuleScope,
) -> bool {
    let result_type = result_type(constructor, scope.clone());
    let callee = callee(result_type);

    equal(
        &Expression::Identifier {
            identifier: type_name,
            data: (),
        },
        callee,
        scope,
    )
}

// // is it an instance, is it exist, is it specialized?
// fn constructor_information_in_respect_to_type(constructor: &Expression, r#type: &Expression) -> ! {
//     unimplemented!()
// }

// for a default value, we should return Option<...>
// gets R in A -> B -> C -> R plus an environment b.c. R could depend on outer stuff
// @Note this function assumes that the expression has already been normalized!
fn result_type(mut expression: &Expression, scope: ModuleScope) -> &Expression {
    loop {
        expression = match expression {
            Expression::PiTypeLiteral {
                binder,
                domain,
                codomain,
                explicitness: _,
                data: (),
            } => {
                if let Some(binder) = binder {
                    scope
                        .clone()
                        .insert_parameter(binder.clone(), domain.as_ref().clone());
                }
                codomain
            }
            // @Question Identifier is reachable, right??
            // @Note application is neutral
            Expression::Application { .. }
            | Expression::TypeLiteral { data: () }
            | Expression::NatTypeLiteral { data: () }
            | Expression::Identifier {
                identifier: _,
                data: (),
            } => {
                return expression;
            }
            Expression::Hole { tag: _, data: () } => unimplemented!(),
            Expression::LambdaLiteral { .. }
            | Expression::NatLiteral { value: _, data: () }
            | Expression::UseIn { data: () }
            | Expression::Case { data: () } => unreachable!(),
        }
    }
}

// returns the `f` in `f a b c`
fn callee(mut expression: &Expression) -> &Expression {
    loop {
        expression = match expression {
            Expression::Application {
                expression: callee, ..
            } => callee,
            expression => return expression,
        }
    }
}

fn normalizing_equal(left: &Expression, right: &Expression, scope: ModuleScope) -> Result<bool> {
    Ok(equal(
        &normalize(left, scope.clone())?,
        &normalize(right, scope.clone())?,
        scope,
    ))
}

fn equal(left: &Expression, right: &Expression, scope: ModuleScope) -> bool {
    match (left, right) {
        (
            Expression::Identifier {
                identifier: left,
                data: (),
            },
            Expression::Identifier {
                identifier: right,
                data: (),
            },
        ) => left == right,
        (
            Expression::Application {
                expression: left_expression,
                argument: left_argument,
                explicitness: _,
                data: (),
            },
            Expression::Application {
                expression: right_expression,
                argument: right_argument,
                explicitness: _,
                data: (),
            },
        ) => {
            equal(left_expression, right_expression, scope.clone())
                && equal(left_argument, right_argument, scope)
        }
        (Expression::TypeLiteral { data: () }, Expression::TypeLiteral { data: () }) => true,
        (Expression::NatTypeLiteral { data: () }, Expression::NatTypeLiteral { data: () }) => true,
        (
            Expression::NatLiteral {
                value: left,
                data: (),
            },
            Expression::NatLiteral {
                value: right,
                data: (),
            },
        ) => left == right,
        (
            Expression::PiTypeLiteral {
                binder: binder1,
                domain: domain1,
                codomain: codomain1,
                explicitness: _,
                data: (),
            },
            Expression::PiTypeLiteral {
                binder: binder2,
                domain: domain2,
                codomain: codomain2,
                explicitness: _,
                data: (),
            },
        ) => abstraction_equal(
            binder1.as_ref(),
            domain1,
            codomain1,
            binder2.as_ref(),
            domain2,
            codomain2,
            scope,
        ),
        (
            Expression::LambdaLiteral {
                binder: binder1,
                parameter_type_annotation: parameter_type_annotation1,
                body: body1,
                body_type_annotation: _,
                explicitness: _,
                data: (),
            },
            Expression::LambdaLiteral {
                binder: binder2,
                parameter_type_annotation: parameter_type_annotation2,
                body: body2,
                body_type_annotation: _,
                explicitness: _,
                data: (),
            },
        ) => {
            // @Bug unchecked @Temporary unwrap
            abstraction_equal(
                Some(binder1),
                parameter_type_annotation1.as_ref().expect(MISSING_ANNOTATION),
                body1,
                Some(binder2),
                parameter_type_annotation2.as_ref().expect(MISSING_ANNOTATION),
                body2,
                scope,
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
// fn inhabited(&self, scope: MutCtx) -> Result<bool> {
//     Ok(match self {
//         Expr::Type => true,
//         Expr::Pi(Abstraction { input, output, .. }) => {
//             // @Bug crashes on e.g. `(-> A # A)` because we need to extend
//             // scope
//             output.inhabited(scope.clone())? || !input.inhabited(scope)?
//         }
//         // @Note because we treat Self=Expr as Self=Value, this is a neutral Var
//         // so its value (repr) is None. It cannot be a parameter, so ist must be
//         // an ADT i think
//         Expr::Var(binding) => {
//             if scope.clone().adt(binding) {
//                 let constructors = scope.clone().constructors(binding);
//                 // @Note sad: bc of error handling, we cannot use `all` but
//                 // only `try_fold`
//                 !constructors.is_empty()
//                     && constructors
//                         .into_iter()
//                         .try_fold(true, |acc, constructor| {
//                             Ok(acc
//                                 && scope
//                                     .clone()
//                                     .lookup_type(&constructor)
//                                     .unwrap()
//                                     .inhabited(scope.clone())?)
//                         })?
//             } else {
//                 // constructor @Bug not always a constructor but also neutral variable
//                 // @Note not extensible, hacky
//                 return Err(Error::ExpressionsNotEqual(
//                     Expr::Type,
//                     scope.lookup_type(binding).unwrap(),
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

// @Temporary signature
// @Note bad signature (deviation between pi and lambda)
fn abstraction_substitute(
    binder: Option<&Identifier>,
    parameter: &Expression,
    expression: &Expression,
    environment: Environment,
    scope: ModuleScope,
) -> (Option<Identifier>, Expression, Expression) {
    let parameter = substitute(parameter, environment.clone(), scope.clone());

    let (refreshed_binder, environment) = match binder {
        Some(binder) => {
            let refreshed_binder = binder.refresh(scope.clone());
            let mut environment = environment.as_ref().clone();
            environment.insert(
                binder.clone(),
                Expression::Identifier {
                    identifier: refreshed_binder.clone(),
                    data: (),
                },
            );
            (Some(refreshed_binder), Rc::new(environment))
        }
        None => (None, environment),
    };
    (
        refreshed_binder,
        parameter,
        substitute(expression, environment, scope),
    )
}

// @Temporary signature
fn normalize_abstraction(
    binder: Option<&Identifier>,
    parameter: &Expression,
    expression: &Expression,
    scope: ModuleScope,
) -> Result<(Expression, Expression)> {
    let parameter = normalize(parameter, scope.clone())?;
    Ok((
        parameter.clone(),
        match binder {
            Some(binder) => normalize(
                expression,
                scope.extend_with_parameter(binder.clone(), parameter),
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
    scope: ModuleScope,
) -> bool {
    return equal(parameter1, parameter2, scope.clone())
        && match (binder1, binder2) {
            (Some(binder1), Some(binder2)) => equal(
                expression1,
                &substitute(
                    expression2,
                    {
                        let mut environment = HashMap::new();
                        environment.insert(
                            binder2.clone(),
                            Expression::Identifier {
                                identifier: binder1.clone(),
                                data: (),
                            },
                        );
                        Rc::new(environment)
                    },
                    scope.clone(),
                ),
                scope,
            ),
            (Some(binder1), None) => equal(
                expression1,
                expression2,
                scope.extend_with_parameter(binder1.clone(), parameter1.clone()),
            ),
            (None, Some(binder2)) => equal(
                expression1,
                expression2,
                scope.extend_with_parameter(binder2.clone(), parameter2.clone()),
            ),
            (None, None) => equal(expression1, expression2, scope),
        };
}
