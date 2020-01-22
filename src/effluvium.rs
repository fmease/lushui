//! First type-checker plus backend (tree-walk interpreter).
//!
//! The code in here is all temporary and needs to be reworked quite a bit!
//! Especially substitution! We should use Debruijn-indices, for real!

mod adts;
mod error;
mod scope;

use crate::hir::{self, expr, expression, Declaration, Expression, Identifier};
use crate::parser::Explicitness;
use error::{Error, Result};
use scope::Environment;
pub use scope::{FunctionScope, ModuleScope};

use std::collections::HashMap;
use std::rc::Rc;

// @Temporary @Bug we don't do non-trivial type inference yet and thus, type parameters of lambda literals
// must be annotated with a type. And since we don't have a dedicated Error::InternalCompilerError yet,
// we use this constant for an error message:
const MISSING_ANNOTATION: &str =
    "(compiler bug) currently lambda literal parameters and patterns must be type-annotated";

// @Task handle out of order declarations and recursions (you need to go through declarations twice) and
// only register the names plus types first (there is gonna be scope.insert_unresolved_binding)
pub fn evaluate_declaration(declaration: &Declaration, module_scope: ModuleScope) -> Result<()> {
    Ok(match declaration {
        Declaration::Value {
            binder,
            type_annotation,
            expression,
        } => {
            module_scope
                .clone()
                .assert_is_not_yet_defined(binder.clone())?;
            let function_scope = FunctionScope::new(module_scope.clone());
            let infered_type = match_with_type_annotation(
                expression.clone(),
                type_annotation.clone(),
                &function_scope,
            )?;
            let value = normalize(expression.clone(), &function_scope)?;
            module_scope.insert_value_binding(binder.clone(), infered_type, value);
        }
        Declaration::Data {
            binder: data_type_binder,
            type_annotation,
            constructors,
        } => {
            module_scope
                .clone()
                .assert_is_not_yet_defined(data_type_binder.clone())?;

            let function_scope = FunctionScope::new(module_scope.clone());
            let r#type = normalize(type_annotation.clone(), &function_scope)?;
            assert_expression_is_a_type(r#type.clone(), &function_scope)?;

            module_scope
                .clone()
                .insert_data_binding(data_type_binder.clone(), r#type);

            for hir::Constructor {
                binder,
                type_annotation,
            } in constructors
            {
                let r#type = normalize(type_annotation.clone(), &function_scope)?;
                assert_expression_is_a_type(r#type.clone(), &function_scope)?;

                adts::instance::assert_constructor_is_instance_of_type(
                    binder.clone(),
                    type_annotation.clone(),
                    data_type_binder.clone(),
                    module_scope.clone(),
                )?;

                module_scope
                    .clone()
                    .assert_is_not_yet_defined(binder.clone())?;

                module_scope.clone().insert_constructor_binding(
                    binder.clone(),
                    r#type.clone(),
                    data_type_binder,
                );
            }
        }
        Declaration::Module { declarations } => {
            for declaration in declarations {
                evaluate_declaration(declaration, module_scope.clone())?;
            }
        }
        Declaration::Use => todo!(),
        Declaration::Foreign {
            binder,
            type_annotation,
        } => {
            module_scope
                .clone()
                .assert_is_not_yet_defined(binder.clone())?;

            let function_scope = FunctionScope::new(module_scope.clone());
            let r#type = normalize(type_annotation.clone(), &function_scope)?;
            assert_expression_is_a_type(r#type.clone(), &function_scope)?;

            module_scope.insert_type_for_foreign_binding(binder.clone(), r#type);
        }
    })
}

fn substitute(
    expression: Expression,
    environment: Environment,
    scope: &FunctionScope<'_>,
) -> Expression {
    match expression {
        Expression::Path(ref path) => environment
            .get(&path.identifier)
            .cloned()
            .unwrap_or(expression),
        Expression::PiTypeLiteral(literal) => {
            let (parameter, domain, codomain) = abstraction_substitute(
                literal.parameter.clone(),
                literal.domain.clone(),
                literal.codomain.clone(),
                environment,
                scope,
            );
            expr! {
                PiTypeLiteral {
                    parameter,
                    domain,
                    codomain,
                    explicitness: Explicitness::Explicit,
                }
            }
        }
        Expression::LambdaLiteral(literal) => {
            // potential @Bug we totally ignore literal.body_type_annotation here. There should be substitutions
            // as well, right? Also, we set the body_type_annotation to None which means we drop the information
            // @Task @Beacon @Beacon @Beacon incorporate literal.body_type_annotation in the substitution process!
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
            expr! {
                LambdaLiteral {
                    // @Note this unwrap is safe, but the API of abstraction_substitute sucks
                    parameter: binder.unwrap(),
                    parameter_type_annotation: Some(parameter_type),
                    body,
                    body_type_annotation: None,
                    explicitness: Explicitness::Explicit,
                }
            }
        }
        Expression::Application(application) => expr! {
            Application {
                callee: substitute(
                    application.callee.clone(),
                    environment.clone(),
                    scope,
                ),
                argument: substitute(application.argument.clone(), environment, scope),
                explicitness: Explicitness::Explicit,
            }
        },
        Expression::Hole(_) => todo!(),
        Expression::UseIn(_) => todo!(),
        Expression::CaseAnalysis(case_analysis) => expr! {
            CaseAnalysis {
                expression: substitute(
                    case_analysis.expression.clone(),
                    environment.clone(),
                    scope,
                ),
                cases: case_analysis
                    .cases
                    .iter()
                    .map(|case| {
                        case_analysis_case_substitute(
                            case.clone(),
                            environment.clone(),
                            scope,
                        )
                    })
                    .collect(),
            }
        },
        Expression::TypeLiteral | Expression::NatTypeLiteral | Expression::NatLiteral(_) => {
            expression
        }
        // @Note I think
        Expression::UnsaturatedForeignApplication(_) => unreachable!(),
    }
}

fn case_analysis_case_substitute(
    case: expression::CaseAnalysisCase,
    environment: Environment,
    scope: &FunctionScope<'_>,
) -> expression::CaseAnalysisCase {
    let (pattern, environment) = pattern_substitute(case.pattern, environment, scope);

    expression::CaseAnalysisCase {
        pattern,
        expression: substitute(case.expression.clone(), environment, scope),
    }
}

fn pattern_substitute(
    pattern: expression::Pattern,
    environment: Environment,
    scope: &FunctionScope<'_>,
) -> (expression::Pattern, Environment) {
    match pattern {
        expression::Pattern::NatLiteral(_) => (pattern, environment),
        expression::Pattern::Path {
            path,
            type_annotation,
        } => {
            let type_annotation = type_annotation
                .map(|annotation| substitute(annotation, environment.clone(), scope));

            if is_matchable(&path, scope) {
                return (
                    expression::Pattern::Path {
                        path,
                        type_annotation,
                    },
                    environment,
                );
            }

            let refreshed_binder = path.identifier.refresh();
            // @Bug geez deep copy, fix this, use a better data structure
            let mut environment = environment.as_ref().clone();
            environment.insert(
                path.identifier.clone(),
                expr! {
                    Path {
                        identifier: refreshed_binder.clone(),
                    }
                },
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
                pattern_substitute(callee.as_ref().clone(), environment, scope);
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
fn _insert_matchables_from_pattern(pattern: &expression::Pattern, scope: &FunctionScope<'_>) {
    match &pattern {
        expression::Pattern::NatLiteral(_) => {}
        expression::Pattern::Path {
            path,
            type_annotation: _type_annotation,
        } => {
            if !is_matchable(path, scope) {
                // @Temporary comment, first replace with pure version
                // scope.insert_parameter_binding(
                //     path.identifier.clone(),
                //     type_annotation.as_ref().expect(MISSING_ANNOTATION).clone(),
                // );
            }
        }
        expression::Pattern::Application { callee, argument } => {
            _insert_matchables_from_pattern(callee, scope);
            _insert_matchables_from_pattern(argument, scope);
        }
    }
}

// @Task move somewhere more appropriate
// @Task verify (esp. b/c shadowing)
fn is_matchable(path: &expression::Path, scope: &FunctionScope<'_>) -> bool {
    path.is_simple() && scope.is_constructor(&path.identifier)
}

pub fn infer_type(expression: Expression, scope: &FunctionScope<'_>) -> Result<hir::Expression> {
    Ok(match expression {
        Expression::Path(path) => scope
            .lookup_type(&path.identifier)
            .ok_or_else(|| Error::UndefinedBinding(path.identifier.clone()))?,
        Expression::TypeLiteral | Expression::NatTypeLiteral => Expression::TypeLiteral,
        Expression::NatLiteral(_) => Expression::NatTypeLiteral,
        Expression::PiTypeLiteral(literal) => {
            // ensure domain and codomain are are well-typed
            // @Question why do we need to this? shouldn't this be already handled if
            // `expression` (parameter of `infer_type`) has been normalized?
            assert_expression_is_a_type(literal.domain.clone(), scope)?;
            if let Some(parameter) = literal.parameter.clone() {
                assert_expression_is_a_type(
                    literal.codomain.clone(),
                    &scope.extend_with_parameter(parameter, literal.domain.clone()),
                )?;
            } else {
                assert_expression_is_a_type(literal.codomain.clone(), scope)?;
            }

            Expression::TypeLiteral
        }
        Expression::LambdaLiteral(literal) => {
            let parameter_type: Expression = literal
                .parameter_type_annotation
                .clone()
                .expect(MISSING_ANNOTATION);
            assert_expression_is_a_type(parameter_type.clone(), scope)?;
            let scope =
                scope.extend_with_parameter(literal.parameter.clone(), parameter_type.clone());
            let infered_body_type = infer_type(literal.body.clone(), &scope)?;
            if let Some(body_type_annotation) = literal.body_type_annotation.clone() {
                // @Question should we assert_expression_is_type(body_type_annotation) before this?
                assert_expressions_are_equal(
                    body_type_annotation,
                    infered_body_type.clone(),
                    &scope,
                )?;
            }
            expr! {
                PiTypeLiteral {
                    parameter: Some(literal.parameter.clone()),
                    domain: parameter_type,
                    codomain: infered_body_type,
                    explicitness: Explicitness::Explicit,
                }
            }
        }
        Expression::Application(application) => {
            // @Task verify type_of_expression is already normalized
            // @Note maybe use dbg-macro?
            let type_of_callee = infer_type(application.callee.clone(), scope)?;
            // @Note this is an example where we normalize after an infer_type which means infer_type
            // returns possibly non-normalized expressions, can we do better?
            match normalize(type_of_callee, scope)? {
                Expression::PiTypeLiteral(literal) => {
                    let argument_type = infer_type(application.argument.clone(), scope)?;
                    assert_expressions_are_equal(literal.domain.clone(), argument_type, scope)?;
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
        Expression::Hole(_) => todo!(),
        Expression::UseIn(_) => todo!(),
        Expression::CaseAnalysis(case_analysis) => {
            let r#type = infer_type(case_analysis.expression.clone(), scope)?;
            // @Task generalize this (because then, we don't need to check whether we supplied too
            // many constructors (would be a type error on its own, but...)/binders separately, or whatever)
            // if case_analysis.cases.is_empty() {
            // // @Note let's do the important stuff first before going down the rabbit hole of
            // // checking whether a type is inhabited or not
            // todo!()
            // return if is_uninhabited(r#type, scope) {
            //     // (A: 'Type) -> A
            // Ok(expr! {
            //     PiTypeLiteral {
            //         // @Question is Identifier::Stub error-prone (in respect to substitution/name clashes)?
            //         parameter: Some(Identifier::Stub),
            //         domain: Expression::TypeLiteral,
            //         codomain: expr! { Path { identifier: Identifier::Stub } },
            //         // @Temporary explicitness
            //         explicitness: Explicitness::Explicit,
            //     }
            // })
            // } else {
            //     // @Task supply more information
            //     Err(Error::NotAllConstructorsCovered)
            // }
            // }

            // @Task verify that
            // * patterns are of correct type (i.e. r#type is an ADT and the constructors are the valid ones)
            // * all constructors are covered
            // * all case_analysis.cases>>.expressions are of the same type

            let type_path = match r#type {
                Expression::Path(path) => path.identifier.clone(),
                // @Note support Expression::Application to allow analysing polymorphic types
                // @Task
                _ => panic!("encountered unsupported type to be case-analysed"),
            };

            let constructors = scope.constructors(&type_path);

            dbg!(&constructors);

            todo!()
        }
        Expression::UnsaturatedForeignApplication(_) => todo!(),
    })
}

// @Note assumes expression_is_type(r#type, ..) holds
// @Task we need to consider polymorphic types like `Identity` (with `Identity'`) where
// `Identity Uninhabited` is uninhabited but `Identity Inhabited` is inhabited
// @Note becomes important when we handle Expression::Application
fn _is_uninhabited(r#type: Expression, _scope: ModuleScope) -> bool {
    match r#type {
        Expression::PiTypeLiteral(_literal) => {
            // return |codomain|^|domain| (consider dependent types|parameters)
            todo!()
        }
        // @Note we need to be able pass type arguments to is_uninhabited!
        Expression::Application(_application) => todo!(),
        Expression::TypeLiteral | Expression::NatTypeLiteral => true,
        Expression::Path(_path) => {
            // @Task look up path in context and decide upon returned information
            // if is an ADT, go through every constructor and for each one check
            // @Beacon `is_applicable` which checks whether the domain (only!) of the type
            // is uninhabited
            todo!()
        }
        Expression::Hole(_hole) => todo!(),
        // @Question unreachable??
        Expression::LambdaLiteral(_literal) => unreachable!(),
        Expression::UseIn(_) => todo!(),
        Expression::CaseAnalysis(_case_analysis) => todo!(),
        Expression::NatLiteral(_) => unreachable!(),
        Expression::UnsaturatedForeignApplication(_) => todo!(),
    }
}

// @Task differenciate between Expression<InitialPhase> and Expression<Normalized>
pub fn normalize(expression: Expression, scope: &FunctionScope<'_>) -> Result<Expression> {
    Ok(match expression {
        // @Task if path refers to a foreign binding AND arity == 0, then resolve the foreign
        // binding
        Expression::Path(ref path) => scope
            .lookup_value(&path.identifier)
            .ok_or_else(|| Error::UndefinedBinding(path.identifier.clone()))?
            // @Question is this normalization necessary? I mean, yes, we got a new scope,
            // but the thing in the previous was already normalized (well, it should have been
            // at least). I guess it is necessary because it can contain parameters which could not
            // be resolved yet but potentially can be now.
            .map(|expression| normalize(expression, scope))
            .unwrap_or(Ok(expression))?,
        Expression::Application(application) => {
            let argument = normalize(application.argument.clone(), scope)?;
            match normalize(application.callee.clone(), scope)? {
                Expression::LambdaLiteral(literal) => normalize(
                    substitute(
                        literal.body.clone(),
                        {
                            let mut environment = HashMap::new();
                            environment.insert(literal.parameter.clone(), argument);
                            Rc::new(environment)
                        },
                        scope,
                    ),
                    scope,
                )?,
                Expression::Path(path) if scope.is_foreign(&path.identifier) => {
                    scope.try_applying_foreign_binding(&path.identifier, vec![argument])?
                }
                Expression::UnsaturatedForeignApplication(application) => scope
                    .try_applying_foreign_binding(&application.callee, {
                        let mut arguments = application.arguments.clone();
                        arguments.push(argument);
                        arguments
                    })?,
                expression => expr! {
                    Application {
                        callee: expression,
                        argument,
                        explicitness: Explicitness::Explicit,
                    }
                },
            }
        }
        Expression::TypeLiteral | Expression::NatTypeLiteral | Expression::NatLiteral(_) => {
            expression
        }
        Expression::PiTypeLiteral(literal) => {
            let domain = normalize(literal.domain.clone(), scope)?;
            let codomain = match literal.parameter.clone() {
                Some(parameter) => normalize(
                    literal.codomain.clone(),
                    &scope.extend_with_parameter(parameter.clone(), domain.clone()),
                )?,
                None => literal.codomain.clone(),
            };
            expr! {
                PiTypeLiteral {
                    parameter: literal.parameter.clone(),
                    domain,
                    codomain,
                    explicitness: Explicitness::Explicit,
                }
            }
        }
        Expression::LambdaLiteral(literal) => {
            let parameter_type = normalize(
                literal
                    .parameter_type_annotation
                    .clone()
                    .expect(MISSING_ANNOTATION),
                scope,
            )?;
            let body = normalize(
                literal.body.clone(),
                &scope.extend_with_parameter(literal.parameter.clone(), parameter_type.clone()),
            )?;
            expr! {
                LambdaLiteral {
                    parameter: literal.parameter.clone(),
                    parameter_type_annotation: Some(parameter_type),
                    body,
                    body_type_annotation: None,
                    explicitness: Explicitness::Explicit,
                }
            }
        }
        Expression::Hole(_) => todo!(),
        Expression::UseIn(_) => todo!(),
        Expression::CaseAnalysis(_) => todo!(),
        // @Question or should the argument be normalized *again*? (under a possibly new scope?)
        Expression::UnsaturatedForeignApplication(_) => expression,
    })
}

fn assert_expression_is_a_type(expression: Expression, scope: &FunctionScope<'_>) -> Result<()> {
    let type_of_expression = infer_type(expression, scope)?;
    assert_expressions_are_equal(Expression::TypeLiteral, type_of_expression, scope)
}

/// Infer type of expression and match it with the type annotation.
///
/// Returns infered type and also checks if supplied type annotation is actually a type for convenience.
fn match_with_type_annotation(
    expression: Expression,
    type_annotation: Expression,
    scope: &FunctionScope<'_>,
) -> Result<Expression> {
    assert_expression_is_a_type(type_annotation.clone(), scope)?;
    let infered_type = infer_type(expression.clone(), scope)?;
    assert_expressions_are_equal(type_annotation, infered_type.clone(), scope)?;

    Ok(infered_type)
}

/// left is expected, right is actual
// @Task improve API!
fn assert_expressions_are_equal(
    left: Expression,
    right: Expression,
    scope: &FunctionScope<'_>,
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

fn normalizing_equal(
    left: Expression,
    right: Expression,
    scope: &FunctionScope<'_>,
) -> Result<bool> {
    Ok(equal(
        normalize(left, scope)?,
        normalize(right, scope)?,
        scope,
    ))
}

fn equal(left: Expression, right: Expression, scope: &FunctionScope<'_>) -> bool {
    match (left, right) {
        (Expression::Path(path0), Expression::Path(path1)) => path0.identifier == path1.identifier,
        (Expression::Application(application0), Expression::Application(application1)) => {
            equal(
                application0.callee.clone(),
                application1.callee.clone(),
                scope,
            ) && equal(
                application0.argument.clone(),
                application1.argument.clone(),
                scope,
            )
        }
        (Expression::TypeLiteral, Expression::TypeLiteral)
        | (Expression::NatTypeLiteral, Expression::NatTypeLiteral) => true,
        (Expression::NatLiteral(literal0), Expression::NatLiteral(literal1)) => {
            literal0.value == literal1.value
        }
        (Expression::PiTypeLiteral(literal0), Expression::PiTypeLiteral(literal1)) => {
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
        (Expression::LambdaLiteral(literal0), Expression::LambdaLiteral(literal1)) => {
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
        // @Task case analysis
        // @Task foreign bindings
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
    scope: &FunctionScope<'_>,
) -> (Option<Identifier>, Expression, Expression) {
    let parameter_type = substitute(parameter_type, environment.clone(), scope);

    let (refreshed_parameter, environment) = match parameter {
        Some(parameter) => {
            let refreshed_parameter = parameter.refresh();
            // @Bug geez deep copy, fix this, use a better data structure
            let mut environment = environment.as_ref().clone();
            environment.insert(
                parameter.clone(),
                expr! {
                    Path {
                        identifier: refreshed_parameter.clone(),
                    }
                },
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
fn abstraction_equal(
    binder1: Option<Identifier>,
    parameter1: Expression,
    expression1: Expression,
    binder2: Option<Identifier>,
    parameter2: Expression,
    expression2: Expression,
    scope: &FunctionScope<'_>,
) -> bool {
    return equal(parameter1.clone(), parameter2.clone(), scope)
        && match (binder1, binder2) {
            (Some(binder1), Some(binder2)) => equal(
                expression1,
                substitute(
                    expression2,
                    {
                        let mut environment = HashMap::new();
                        environment.insert(
                            binder2.clone(),
                            expr! {
                                Path {
                                    identifier: binder1.clone(),

                                }
                            },
                        );
                        Rc::new(environment)
                    },
                    scope,
                ),
                scope,
            ),
            (Some(binder1), None) => equal(
                expression1,
                expression2,
                &scope.extend_with_parameter(binder1.clone(), parameter1),
            ),
            (None, Some(binder2)) => equal(
                expression1,
                expression2,
                &scope.extend_with_parameter(binder2.clone(), parameter2),
            ),
            (None, None) => equal(expression1, expression2, scope),
        };
}
