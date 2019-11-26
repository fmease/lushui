//! First type-checker plus backend (tree-walk interpreter).
//!
//! The code in here is all temporary and needs to be reworked quite a bit!

// @Task rename to scope
mod context;
mod error;

use crate::hir::{self, expression, Declaration, Expression, Identifier};
use crate::parser::Explicitness;
use context::Environment;
pub use context::ModuleScope;
use error::{Error, Result};

use std::collections::HashMap;
use std::rc::Rc;

// @Temporary @Bug we don't do non-trivial type inference yet and thus, type parameters of lambda literals
// must be annotated with a type. And since we don't have a dedicated Error::InternalCompilerError yet,
// we use this constant for an error message:
const MISSING_ANNOTATION: &str =
    "(compiler bug) currently lambda literal parameters must be type-annotated";

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
            let infered_type = infer_type(expression.clone(), scope.clone())?;
            assert_expressions_are_equal(
                type_annotation.clone(),
                infered_type.clone(),
                scope.clone(),
            )?;
            let value = normalize(expression.clone(), scope.clone())?;
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
            let r#type = normalize(type_annotation.clone(), scope.clone())?;
            assert_expression_is_a_type(r#type.clone(), scope.clone())?;

            scope
                .clone()
                .insert_data_type(data_type_binder.clone(), r#type);

            for hir::Constructor {
                binder,
                type_annotation,
            } in constructors
            {
                let r#type = normalize(type_annotation.clone(), scope.clone())?;
                assert_expression_is_a_type(r#type.clone(), scope.clone())?;

                assert_constructor_is_instance_of_type(
                    binder.clone(),
                    type_annotation.clone(),
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

fn substitute(expression: Expression, environment: Environment, scope: ModuleScope) -> Expression {
    match expression {
        Expression::Path(ref path, _) => environment
            .get(&path.identifier)
            .cloned()
            .unwrap_or(expression),
        Expression::TypeLiteral(_, _)
        | Expression::NatTypeLiteral(_, _)
        | Expression::NatLiteral(_, _) => expression,
        Expression::PiTypeLiteral(literal, _) => {
            let (parameter, domain, codomain) = abstraction_substitute(
                literal.parameter.clone(),
                literal.domain.clone(),
                literal.codomain.clone(),
                environment,
                scope,
            );
            Expression::PiTypeLiteral(
                Rc::new(expression::PiTypeLiteral {
                    parameter,
                    domain,
                    codomain,
                    explicitness: Explicitness::Explicit,
                }),
                (),
            )
        }
        Expression::LambdaLiteral(literal, _) => {
            let (binder, parameter_type, body) = abstraction_substitute(
                Some(literal.parameter.clone()),
                literal
                    .parameter_type_annotation
                    .clone()
                    .expect(MISSING_ANNOTATION),
                literal.body.clone(),
                environment,
                scope,
            );
            // @Note this unwrap is safe, but the API of abstraction_substitute sucks
            Expression::LambdaLiteral(
                Rc::new(expression::LambdaLiteral {
                    parameter: binder.unwrap(),
                    parameter_type_annotation: Some(parameter_type),
                    body,
                    body_type_annotation: None,
                    explicitness: Explicitness::Explicit,
                }),
                (),
            )
        }
        Expression::Application(application, _) => Expression::Application(
            Rc::new(expression::Application {
                expression: substitute(
                    application.expression.clone(),
                    environment.clone(),
                    scope.clone(),
                ),
                argument: substitute(application.argument.clone(), environment, scope),
                explicitness: Explicitness::Explicit,
            }),
            (),
        ),
        Expression::Hole(_, _) => unimplemented!(),
        Expression::UseIn(_, _) => unimplemented!(),
        Expression::CaseAnalysis(_, _) => unimplemented!(),
    }
}

pub fn infer_type(expression: Expression, scope: ModuleScope) -> Result<hir::Expression> {
    Ok(match expression {
        Expression::Path(path, _) => scope
            .lookup_type(&path.identifier)
            .ok_or_else(|| Error::UndefinedBinding(path.identifier))?,
        Expression::TypeLiteral(_, _) | Expression::NatTypeLiteral(_, _) => {
            Expression::TypeLiteral(expression::TypeLiteral {}, ())
        }
        Expression::NatLiteral(_, _) => {
            Expression::NatTypeLiteral(expression::NatTypeLiteral {}, ())
        }
        Expression::PiTypeLiteral(literal, _) => {
            // ensure domain and codomain are are well-typed
            // @Question why do we need to this? shouldn't this be already handled if
            // `expression` (parameter of `infer_type`) has been normalized?
            assert_expression_is_a_type(literal.domain.clone(), scope.clone())?;
            assert_expression_is_a_type(
                literal.codomain.clone(),
                if let Some(parameter) = literal.parameter.clone() {
                    scope.extend_with_parameter(parameter, literal.domain.clone())
                } else {
                    scope
                },
            )?;

            Expression::TypeLiteral(expression::TypeLiteral {}, ())
        }
        Expression::LambdaLiteral(literal, _) => {
            let parameter_type: Expression = literal
                .parameter_type_annotation
                .clone()
                .expect(MISSING_ANNOTATION);
            assert_expression_is_a_type(parameter_type.clone(), scope.clone())?;
            let scope = scope
                .clone()
                .extend_with_parameter(literal.parameter.clone(), parameter_type.clone());
            let infered_body_type = infer_type(literal.body.clone(), scope.clone())?;
            if let Some(body_type_annotation) = literal.body_type_annotation.clone() {
                // @Question should we assert_expression_is_type(body_type_annotation) before this?
                assert_expressions_are_equal(
                    body_type_annotation,
                    infered_body_type.clone(),
                    scope,
                )?;
            }
            Expression::PiTypeLiteral(
                Rc::new(expression::PiTypeLiteral {
                    parameter: Some(literal.parameter.clone()),
                    domain: parameter_type,
                    codomain: infered_body_type,
                    explicitness: Explicitness::Explicit,
                }),
                (),
            )
        }
        Expression::Application(application, _) => {
            // @Task verify type_of_expression is already normalized
            // @Note maybe use dbg-macro?
            let type_of_expression = infer_type(application.expression.clone(), scope.clone())?;
            match normalize(type_of_expression, scope.clone())? {
                Expression::PiTypeLiteral(literal, _) => {
                    let argument_type = infer_type(application.argument.clone(), scope.clone())?;
                    assert_expressions_are_equal(
                        literal.domain.clone(),
                        argument_type,
                        scope.clone(),
                    )?;
                    match literal.parameter.clone() {
                        Some(parameter) => substitute(
                            literal.codomain.clone(),
                            {
                                let mut environment = HashMap::new();
                                environment.insert(parameter, application.argument.clone());
                                Rc::new(environment)
                            },
                            scope,
                        ),
                        None => literal.codomain.clone(),
                    }
                }
                expression => {
                    return Err(Error::FunctionExpected {
                        actual: expression,
                        argument: application.argument.clone(),
                    });
                }
            }
        }
        Expression::Hole(_, _) => unimplemented!(),
        Expression::UseIn(_, _) => unimplemented!(),
        Expression::CaseAnalysis(_, _) => unimplemented!(),
    })
}

// @Beacon @Task use type-tagging: tag the hir::Expression with a P: Phase type parameter which for now
// differenciates between Initial and Normalized
pub fn normalize(expression: Expression, scope: ModuleScope) -> Result<Expression> {
    Ok(match expression {
        Expression::Path(ref path, _) => scope
            .clone()
            .lookup_value(&path.identifier)
            .ok_or_else(|| Error::UndefinedBinding(path.identifier.clone()))?
            .map(|expression| normalize(expression, scope))
            .unwrap_or(Ok(expression))?,
        Expression::Application(application, _) => {
            let argument = normalize(application.argument.clone(), scope.clone())?;
            match normalize(application.expression.clone(), scope.clone())? {
                Expression::LambdaLiteral(literal, _) => normalize(
                    substitute(
                        literal.body.clone(),
                        {
                            let mut environment = HashMap::new();
                            environment.insert(literal.parameter.clone(), argument);
                            Rc::new(environment)
                        },
                        scope.clone(),
                    ),
                    scope,
                )?,
                expression => Expression::Application(
                    Rc::new(expression::Application {
                        expression,
                        argument,
                        explicitness: Explicitness::Explicit,
                    }),
                    (),
                ),
            }
        }
        Expression::TypeLiteral(_, _)
        | Expression::NatTypeLiteral(_, _)
        | Expression::NatLiteral(_, _) => expression,
        Expression::PiTypeLiteral(literal, _) => {
            let (domain, codomain) = normalize_abstraction(
                literal.parameter.clone(),
                literal.domain.clone(),
                literal.codomain.clone(),
                scope,
            )?;
            Expression::PiTypeLiteral(
                Rc::new(expression::PiTypeLiteral {
                    parameter: literal.parameter.clone(),
                    domain,
                    codomain,
                    explicitness: Explicitness::Explicit,
                }),
                (),
            )
        }
        Expression::LambdaLiteral(literal, _) => {
            let (parameter_type, body) = normalize_abstraction(
                Some(literal.parameter.clone()),
                literal
                    .parameter_type_annotation
                    .clone()
                    .expect(MISSING_ANNOTATION),
                literal.body.clone(),
                scope,
            )?;
            Expression::LambdaLiteral(
                Rc::new(expression::LambdaLiteral {
                    parameter: literal.parameter.clone(),
                    parameter_type_annotation: Some(parameter_type),
                    body,
                    body_type_annotation: None,
                    explicitness: Explicitness::Explicit,
                }),
                (),
            )
        }
        Expression::Hole(_, _) => unimplemented!(),
        Expression::UseIn(_, _) => unimplemented!(),
        Expression::CaseAnalysis(_, _) => unimplemented!(),
    })
}

fn assert_expression_is_a_type(expression: Expression, scope: ModuleScope) -> Result<()> {
    let type_of_expression = infer_type(expression, scope.clone())?;
    assert_expressions_are_equal(
        Expression::TypeLiteral(expression::TypeLiteral {}, ()),
        type_of_expression,
        scope,
    )
}

/// left is expected, right is actual
fn assert_expressions_are_equal(
    left: Expression,
    right: Expression,
    scope: ModuleScope,
) -> Result<()> {
    if !normalizing_equal(left.clone(), right.clone(), scope)? {
        Err(Error::ExpressionsNotEqual {
            expected: left,
            actual: right,
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
    constructor: Expression,
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
    constructor: Expression,
    type_name: Identifier,
    scope: ModuleScope,
) -> bool {
    let result_type = result_type(constructor, scope.clone());
    let callee = callee(result_type);

    equal(
        Expression::Path(
            expression::Path {
                identifier: type_name,
            },
            (),
        ),
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
fn result_type(mut expression: Expression, scope: ModuleScope) -> Expression {
    loop {
        expression = match expression {
            Expression::PiTypeLiteral(literal, _) => {
                if let Some(parameter) = literal.parameter.clone() {
                    scope
                        .clone()
                        .insert_parameter(parameter, literal.domain.clone());
                }
                literal.codomain.clone()
            }
            Expression::Application(_, _)
            | Expression::TypeLiteral(_, _)
            | Expression::NatTypeLiteral(_, _)
            | Expression::Path(_, _) => {
                return expression;
            }
            Expression::Hole(_, _) => unimplemented!(),
            Expression::LambdaLiteral(_, _)
            | Expression::NatLiteral(_, _)
            | Expression::UseIn(_, _)
            | Expression::CaseAnalysis(_, _) => unreachable!(),
        }
    }
}

// returns the `f` in `f a b c`
fn callee(mut expression: Expression) -> Expression {
    loop {
        expression = match expression {
            Expression::Application(application, _) => application.expression.clone(),
            expression => return expression,
        }
    }
}

fn normalizing_equal(left: Expression, right: Expression, scope: ModuleScope) -> Result<bool> {
    Ok(equal(
        normalize(left, scope.clone())?,
        normalize(right, scope.clone())?,
        scope,
    ))
}

fn equal(left: Expression, right: Expression, scope: ModuleScope) -> bool {
    match (left, right) {
        (Expression::Path(path0, _), Expression::Path(path1, _)) => {
            path0.identifier == path1.identifier
        }
        (Expression::Application(application0, _), Expression::Application(application1, _)) => {
            equal(
                application0.expression.clone(),
                application1.expression.clone(),
                scope.clone(),
            ) && equal(
                application0.argument.clone(),
                application1.argument.clone(),
                scope,
            )
        }
        (Expression::TypeLiteral(_, _), Expression::TypeLiteral(_, _))
        | (Expression::NatTypeLiteral(_, _), Expression::NatTypeLiteral(_, _)) => true,
        (Expression::NatLiteral(literal0, _), Expression::NatLiteral(literal1, _)) => {
            literal0.value == literal1.value
        }
        (Expression::PiTypeLiteral(literal0, _), Expression::PiTypeLiteral(literal1, _)) => {
            abstraction_equal(
                literal0.parameter.clone(),
                literal0.domain.clone(),
                literal0.codomain.clone(),
                literal1.parameter.clone(),
                literal1.domain.clone(),
                literal1.codomain.clone(),
                scope,
            )
        }
        (Expression::LambdaLiteral(literal0, _), Expression::LambdaLiteral(literal1, _)) => {
            abstraction_equal(
                Some(literal0.parameter.clone()),
                literal0
                    .parameter_type_annotation
                    .clone()
                    .expect(MISSING_ANNOTATION),
                literal0.body.clone(),
                Some(literal1.parameter.clone()),
                literal1
                    .parameter_type_annotation
                    .clone()
                    .expect(MISSING_ANNOTATION),
                literal1.body.clone(),
                scope,
            )
        }
        // @Note this is really bad when we decide to add new stuff! but there is no
        // viable alternative really :/
        _ => false,
    }
}

// @Temporary signature
// @Note bad signature (deviation between pi and lambda)
fn abstraction_substitute(
    binder: Option<Identifier>,
    parameter: Expression,
    expression: Expression,
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
                Expression::Path(
                    expression::Path {
                        identifier: refreshed_binder.clone(),
                    },
                    (),
                ),
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
    binder: Option<Identifier>,
    parameter: Expression,
    expression: Expression,
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
    binder1: Option<Identifier>,
    parameter1: Expression,
    expression1: Expression,
    binder2: Option<Identifier>,
    parameter2: Expression,
    expression2: Expression,
    scope: ModuleScope,
) -> bool {
    return equal(parameter1.clone(), parameter2.clone(), scope.clone())
        && match (binder1, binder2) {
            (Some(binder1), Some(binder2)) => equal(
                expression1,
                substitute(
                    expression2,
                    {
                        let mut environment = HashMap::new();
                        environment.insert(
                            binder2.clone(),
                            Expression::Path(
                                expression::Path {
                                    identifier: binder1.clone(),
                                },
                                (),
                            ),
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
                scope.extend_with_parameter(binder1.clone(), parameter1),
            ),
            (None, Some(binder2)) => equal(
                expression1,
                expression2,
                scope.extend_with_parameter(binder2.clone(), parameter2),
            ),
            (None, None) => equal(expression1, expression2, scope),
        };
}
