//! First type-checker plus backend (tree-walk interpreter).
//!
//! The code in here is all temporary and needs to be reworked quite a bit!
//! Especially substitution! We should use Debruijn-indices, for real!

// @Task rename to scope
mod adts;
mod scope;
mod error;

use crate::hir::{self, expression, Declaration, Expression, Identifier};
use crate::parser::Explicitness;
use scope::Environment;
pub use scope::ModuleScope;
use error::{Error, Result};

use std::collections::HashMap;
use std::rc::Rc;

// @Temporary @Bug we don't do non-trivial type inference yet and thus, type parameters of lambda literals
// must be annotated with a type. And since we don't have a dedicated Error::InternalCompilerError yet,
// we use this constant for an error message:
const MISSING_ANNOTATION: &str =
    "(compiler bug) currently lambda literal parameters and patterns must be type-annotated";

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

                adts::instance::assert_constructor_is_instance_of_type(
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
        Declaration::Use => todo!(),
        Declaration::Foreign => todo!(),
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
            // potential @Bug we totally ignore literal.body_type_annotation here. There should be substitutions
            // as well, right? Also, we set the body_type_annotation to None which means we drop the information
            // @Task incorporate literal.body_type_annotation in the substitution process!
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
        Expression::Hole(_, _) => todo!(),
        Expression::UseIn(_, _) => todo!(),
        Expression::CaseAnalysis(case_analysis, _) => Expression::CaseAnalysis(
            Rc::new(expression::CaseAnalysis {
                expression: substitute(
                    case_analysis.expression.clone(),
                    environment.clone(),
                    scope.clone(),
                ),
                cases: case_analysis
                    .cases
                    .iter()
                    .map(|case| {
                        case_analysis_case_substitute(
                            case.clone(),
                            environment.clone(),
                            scope.clone(),
                        )
                    })
                    .collect(),
            }),
            (),
        ),
    }
}

fn case_analysis_case_substitute(
    case: expression::CaseAnalysisCase,
    environment: Environment,
    scope: ModuleScope,
) -> expression::CaseAnalysisCase {
    let (pattern, environment) = pattern_substitute(case.pattern, environment, scope.clone());

    expression::CaseAnalysisCase {
        pattern,
        expression: substitute(case.expression.clone(), environment, scope),
    }
}

fn pattern_substitute(
    pattern: expression::Pattern,
    environment: Environment,
    scope: ModuleScope,
) -> (expression::Pattern, Environment) {
    match pattern {
        expression::Pattern::NatLiteral(_) => (pattern, environment),
        expression::Pattern::Path {
            path,
            type_annotation,
        } => {
            let type_annotation = type_annotation
                .map(|annotation| substitute(annotation, environment.clone(), scope.clone()));

            if is_matchable(&path, scope.clone()) {
                return (
                    expression::Pattern::Path {
                        path,
                        type_annotation,
                    },
                    environment,
                );
            }

            let refreshed_binder = path.identifier.refresh(scope.clone());
            // @Bug geez deep copy, fix this, use a better data structure
            let mut environment = environment.as_ref().clone();
            environment.insert(
                path.identifier.clone(),
                Expression::Path(
                    Rc::new(expression::Path {
                        identifier: refreshed_binder.clone(),
                    }),
                    (),
                ),
            );
            (
                expression::Pattern::Path {
                    path: expression::Path {
                        identifier: refreshed_binder,
                    },
                    type_annotation,
                },
                Rc::new(environment),
            )
        }
        expression::Pattern::Application { callee, argument } => {
            // @Bug disallow `n m` where n is a path which is not matchable
            // @Note `(N m) o` is still legal obviously, N matchable, m not
            // @Question is there a better place than here?

            // @Note bad (memory): we deep-copy an Rc!
            let (callee, environment) =
                pattern_substitute(callee.as_ref().clone(), environment, scope.clone());
            let (argument, environment) =
                pattern_substitute(argument.as_ref().clone(), environment, scope);

            (
                expression::Pattern::Application {
                    callee: Rc::new(callee),
                    argument: Rc::new(argument),
                },
                environment,
            )
        }
    }
}

// @Beacon @Note this is used in normalize, *not* substitute
fn insert_matchables_from_pattern(pattern: &expression::Pattern, scope: ModuleScope) {
    match &pattern {
        expression::Pattern::NatLiteral(_) => {}
        expression::Pattern::Path {
            path,
            type_annotation,
        } => {
            if !is_matchable(path, scope.clone()) {
                scope.insert_parameter(
                    path.identifier.clone(),
                    type_annotation.as_ref().expect(MISSING_ANNOTATION).clone(),
                );
            }
        }
        expression::Pattern::Application { callee, argument } => {
            insert_matchables_from_pattern(callee, scope.clone());
            insert_matchables_from_pattern(argument, scope.clone());
        }
    }
}

// @Task move somewhere more appropriate
// @Task verify (esp. b/c shadowing)
fn is_matchable(path: &expression::Path, scope: ModuleScope) -> bool {
    path.is_simple() && scope.is_constructor(&path.identifier)
}

pub fn infer_type(expression: Expression, scope: ModuleScope) -> Result<hir::Expression> {
    Ok(match expression {
        Expression::Path(path, _) => scope
            .lookup_type(&path.identifier)
            .ok_or_else(|| Error::UndefinedBinding(path.identifier.clone()))?,
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
            // @Note this is an example where we normalize after an infer_type which means infer_type
            // returns possibly non-normalized expressions, can we do better?
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
        Expression::Hole(_, _) => todo!(),
        Expression::UseIn(_, _) => todo!(),
        Expression::CaseAnalysis(case_analysis, _) => {
            let r#type = infer_type(case_analysis.expression.clone(), scope.clone())?;
            
            // @Task generalize this (because then, we don't need to check whether we supplied too
            // many constructors (would be a type error on its own, but...)/binders separately, or whatever)
            // if case_analysis.cases.is_empty() {
            //     // @Note let's do the important stuff first before going down the rabbit hole of
            //     // checking whether a type is inhabited or not
            //     todo!()
            //     // return if is_uninhabited(r#type, scope) {
            //     //     // (A: 'Type) -> A
            //     //     Ok(Expression::PiTypeLiteral(Rc::new(expression::PiTypeLiteral {
            //     //         // @Question is Identifier::Stub error-prone (in respect to substitution/name clashes)?
            //     //         parameter: Some(Identifier::Stub),
            //     //         domain: Expression::TypeLiteral(expression::TypeLiteral {}, ()),
            //     //         codomain: Expression::Path(Rc::new(expression::Path { identifier: Identifier::Stub }), ()),
            //     //         // @Temporary explicitness
            //     //         explicitness: Explicitness::Explicit,
            //     //     }), ()))
            //     // } else {
            //     //     // @Task supply more information
            //     //     Err(Error::NotAllConstructorsCovered)
            //     // }
            // }

            // @Task verify that
            // * patterns are of correct type (i.e. r#type is an ADT and the constructors are the valid ones)
            // * all constructors are covered
            // * all case_analysis.cases>>.expressions are of the same type

            let type_path = match r#type {
                Expression::Path(path, _) => path.identifier.clone(),
                // @Note support Expression::Application to allow analysing polymorphic types
                // @Task
                _ => panic!("encountered unsupported type to be case-analysed"),
            };

            let constructors = scope.constructors(&type_path);

            dbg!(&constructors);

            todo!()
        }
    })
}

// @Note assumes expression_is_type(r#type, ..) holds
// @Task we need to consider polymorphic types like `Identity` (with `Identity'`) where
// `Identity Uninhabited` is uninhabited but `Identity Inhabited` is inhabited
// @Note becomes important when we handle Expression::Application
fn is_uninhabited(r#type: Expression, scope: ModuleScope) -> bool {
    match r#type {
        Expression::PiTypeLiteral(literal, _) => {
            // return |codomain|^|domain| (consider dependent types|parameters)
            todo!()
        }
        // @Note we need to be able pass type arguments to is_uninhabited!
        Expression::Application(application, _) => todo!(),
        Expression::TypeLiteral(_, _) | Expression::NatTypeLiteral(_, _) => true,
        Expression::Path(_path, _) => {
            // @Task look up path in context and decide upon returned information
            // if is an ADT, go through every constructor and for each one check
            // @Beacon `is_applicable` which checks whether the domain (only!) of the type
            // is uninhabited
            todo!()
        }
        Expression::Hole(_hole, _) => todo!(),
        // @Question unreachable??
        Expression::LambdaLiteral(literal, _) => unreachable!(),
        Expression::UseIn(_, _) => todo!(),
        Expression::CaseAnalysis(case_analysis, _) => todo!(),
        Expression::NatLiteral(_, _) => unreachable!(),
    }
}

// @Task differenciate between Expression<InitialPhase> and Expression<Normalized>
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
        Expression::Hole(_, _) => todo!(),
        Expression::UseIn(_, _) => todo!(),
        Expression::CaseAnalysis(_, _) => todo!(),
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
    parameter: Option<Identifier>,
    parameter_type: Expression,
    expression: Expression,
    environment: Environment,
    scope: ModuleScope,
) -> (Option<Identifier>, Expression, Expression) {
    let parameter_type = substitute(parameter_type, environment.clone(), scope.clone());

    let (refreshed_parameter, environment) = match parameter {
        Some(parameter) => {
            let refreshed_parameter = parameter.refresh(scope.clone());
            // @Bug geez deep copy, fix this, use a better data structure
            let mut environment = environment.as_ref().clone();
            environment.insert(
                parameter.clone(),
                Expression::Path(
                    Rc::new(expression::Path {
                        identifier: refreshed_parameter.clone(),
                    }),
                    (),
                ),
            );
            (Some(refreshed_parameter), Rc::new(environment))
        }
        None => (None, environment),
    };
    (
        refreshed_parameter,
        parameter_type,
        substitute(expression, environment, scope),
    )
}

// @Temporary signature
fn normalize_abstraction(
    parameter: Option<Identifier>,
    parameter_type: Expression,
    expression: Expression,
    scope: ModuleScope,
) -> Result<(Expression, Expression)> {
    let parameter_type = normalize(parameter_type, scope.clone())?;
    Ok((
        parameter_type.clone(),
        match parameter {
            Some(parameter) => normalize(
                expression,
                scope.extend_with_parameter(parameter.clone(), parameter_type),
            )?,
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
                                Rc::new(expression::Path {
                                    identifier: binder1.clone(),
                                }),
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
