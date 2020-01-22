//! The type checker and tree-walk interpreter.
//!
//! The first backend of lushuic. Later, we are going to add a bytecode interpreter for
//! evaluation. Still, this interpreter will stay because it is necessary for type-checking.
//!
//! The plan is to somehow split this file into `typing.rs` and `backend/tree_walk_interpreter.rs`
//! with the module `typing` referencing the moved interpreter.
//!
//! This module is **heavily** under construction!
//!
//! ## Issues
//!
//! * too many bugs
//! * using substitution environments instead of locally nameless/Debruijn-indeces
//! * case analysis not implemented
//! * order-independent declarations and recursion not implemented
//! * modules not implemented
//! * non-trivial type inference not done
//! * untyped/unkinded AST-transformations
//! * bad unstructured error reporting without span information
//! * bad API/project structure could be better
//! * integration and regression tests missing

mod algebraic_data_types;
mod error;
mod scope;

use crate::hir::{self, expr, expression, Declaration, Expression};
use crate::parser::Explicitness;
use error::{Error, Result};
pub use scope::ModuleScope;
use scope::{FunctionScope, Substitutions};

use std::rc::Rc;

// @Temporary @Bug we don't do non-trivial type inference yet and thus, type parameters of lambda literals
// must be annotated with a type. And since we don't have a dedicated Error::InternalCompilerError yet,
// we use this constant for an error message:
const MISSING_ANNOTATION: &str =
    "(compiler bug) currently lambda literal parameters and patterns must be type-annotated";

/// Try to evaluate a declaration modifying the given scope.
// @Task support order-independence, recursion and proper modules
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
            let infered_type = match_expression_with_type_annotation(
                expression.clone(),
                type_annotation.clone(),
                &function_scope,
            )?;
            let value = evaluate(expression.clone(), &function_scope)?;
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
            let r#type = evaluate(type_annotation.clone(), &function_scope)?;
            assert_expression_is_a_type(r#type.clone(), &function_scope)?;

            module_scope
                .clone()
                .insert_data_binding(data_type_binder.clone(), r#type);

            for hir::Constructor {
                binder,
                type_annotation,
            } in constructors
            {
                let r#type = evaluate(type_annotation.clone(), &function_scope)?;
                assert_expression_is_a_type(r#type.clone(), &function_scope)?;

                algebraic_data_types::instance::assert_constructor_is_instance_of_type(
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
            let r#type = evaluate(type_annotation.clone(), &function_scope)?;
            assert_expression_is_a_type(r#type.clone(), &function_scope)?;

            module_scope.insert_type_for_foreign_binding(binder.clone(), r#type);
        }
    })
}

/// Substitute inside an expression using an substitutions.
///
/// Replace occurences of those identifiers inside the expression which are
/// member of the subsitution list known as the substitutions with the expression
/// which it maps to.
///
/// This procedure should be replaced with locally nameless.
fn substitute(
    expression: Expression,
    substitutions: &Substitutions<'_>,
    scope: &FunctionScope<'_>,
) -> Expression {
    match expression {
        Expression::Path(ref path) => substitutions
            .retrieve(&path.identifier)
            .unwrap_or(expression),
        Expression::PiTypeLiteral(literal) => {
            let domain = substitute(literal.domain.clone(), &substitutions, scope);

            let (parameter, codomain) = match literal.parameter.clone() {
                Some(parameter) => {
                    let refreshed_parameter = parameter.refresh();
                    let substitutions = substitutions.extend_with(
                        parameter,
                        expr! {
                            Path {
                                identifier: refreshed_parameter.clone(),
                            }
                        },
                    );
                    (
                        Some(refreshed_parameter),
                        substitute(literal.codomain.clone(), &substitutions, scope),
                    )
                }
                None => (
                    None,
                    substitute(literal.codomain.clone(), substitutions, scope),
                ),
            };
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
            let parameter_type_annotation = substitute(
                literal
                    .parameter_type_annotation
                    .clone()
                    .expect(MISSING_ANNOTATION),
                substitutions,
                scope,
            );
            let parameter = literal.parameter.refresh();
            let substitutions = substitutions.extend_with(
                literal.parameter.clone(),
                expr! {
                    Path {
                        identifier: parameter.clone(),
                    }
                },
            );
            let body_type_annotation = literal
                .body_type_annotation
                .as_ref()
                .map(|type_annotation| substitute(type_annotation.clone(), &substitutions, scope));
            let body = substitute(literal.body.clone(), &substitutions, scope);

            expr! {
                LambdaLiteral {
                    parameter,
                    parameter_type_annotation: Some(parameter_type_annotation),
                    body,
                    body_type_annotation,
                    explicitness: Explicitness::Explicit,
                }
            }
        }
        Expression::Application(application) => expr! {
            Application {
                callee: substitute(
                    application.callee.clone(),
                    substitutions.clone(),
                    scope,
                ),
                argument: substitute(application.argument.clone(), substitutions, scope),
                explicitness: Explicitness::Explicit,
            }
        },
        Expression::Hole(_) => todo!(),
        Expression::UseIn(_) => todo!(),
        Expression::CaseAnalysis(case_analysis) => expr! {
            CaseAnalysis {
                expression: substitute(
                    case_analysis.expression.clone(),
                    substitutions.clone(),
                    scope,
                ),
                cases: case_analysis
                    .cases
                    .iter()
                    .map(|case| {
                        case_analysis_case_substitute(
                            case.clone(),
                            substitutions,
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

/// Substitute inside the case of a case analysis.
fn case_analysis_case_substitute(
    case: expression::CaseAnalysisCase,
    substitutions: &Substitutions<'_>,
    scope: &FunctionScope<'_>,
) -> expression::CaseAnalysisCase {
    // @Note does not return a new substitutions anymore
    let pattern = pattern_substitute(case.pattern, substitutions, scope);

    expression::CaseAnalysisCase {
        pattern,
        expression: substitute(case.expression.clone(), substitutions, scope),
    }
}

/// Substitute inside of a pattern.
// @Bug does not work at all anymore!!!
fn pattern_substitute<'p>(
    pattern: expression::Pattern,
    substitutions: &'p Substitutions<'p>,
    scope: &FunctionScope<'_>,
) -> expression::Pattern {
    match pattern {
        expression::Pattern::NatLiteral(_) => pattern,
        expression::Pattern::Path {
            path,
            type_annotation,
        } => {
            let type_annotation = type_annotation
                .map(|annotation| substitute(annotation, substitutions.clone(), scope));

            if is_matchable(&path, scope) {
                return expression::Pattern::Path {
                    path,
                    type_annotation,
                };
            }

            let refreshed_binder = path.identifier.refresh();

            expression::Pattern::Path {
                path: expression::Path {
                    identifier: refreshed_binder.clone(),
                },
                type_annotation,
            }

            // substitutions.extend_with_substitution(
            //     path.identifier.clone(),
            //     expr! {
            //         Path {
            //             identifier: refreshed_binder,
            //         }
            //     },
            // )
        }
        expression::Pattern::Application { callee, argument } => {
            // @Bug disallow `n m` where n is a path which is not matchable
            // @Note `(N m) o` is still legal obviously, N matchable, m not
            // @Question is there a better place than here?

            // @Note does not return a new substitutions anymore
            let callee = pattern_substitute(callee.as_ref().clone(), substitutions, scope);
            // @Note does not return a new substitutions anymore
            let argument = pattern_substitute(argument.as_ref().clone(), substitutions, scope);

            expression::Pattern::Application {
                callee: Rc::new(callee),
                argument: Rc::new(argument),
            }
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

/// Indicate if a path refers to a constructor or not.
// @Task move somewhere more appropriate
// @Task verify (esp. b/c shadowing)
fn is_matchable(path: &expression::Path, scope: &FunctionScope<'_>) -> bool {
    path.is_simple() && scope.is_constructor(&path.identifier)
}

/// Try to infer the type of an expression.
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
                assert_expression_is_a_type(body_type_annotation.clone(), &scope)?;
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
            match evaluate(type_of_callee, scope)? {
                Expression::PiTypeLiteral(literal) => {
                    let argument_type = infer_type(application.argument.clone(), scope)?;
                    assert_expressions_are_equal(literal.domain.clone(), argument_type, scope)?;
                    match literal.parameter.clone() {
                        Some(parameter) => substitute(
                            literal.codomain.clone(),
                            &Substitutions::new()
                                .extend_with(parameter, application.argument.clone()),
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

/// Indicate whether a type is uninhabited.
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

/// Try to evaluate an expression.
///
/// This is beta-reduction I think.
// @Task differenciate between Expression<InitialPhase> and Expression<Normalized>
pub fn evaluate(expression: Expression, scope: &FunctionScope<'_>) -> Result<Expression> {
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
            .map(|expression| evaluate(expression, scope))
            .unwrap_or(Ok(expression))?,
        Expression::Application(application) => {
            let argument = evaluate(application.argument.clone(), scope)?;
            match evaluate(application.callee.clone(), scope)? {
                Expression::LambdaLiteral(literal) => evaluate(
                    substitute(
                        literal.body.clone(),
                        &Substitutions::new().extend_with(literal.parameter.clone(), argument),
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
            let domain = evaluate(literal.domain.clone(), scope)?;
            let codomain = match literal.parameter.clone() {
                Some(parameter) => evaluate(
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
            let parameter_type = evaluate(
                literal
                    .parameter_type_annotation
                    .clone()
                    .expect(MISSING_ANNOTATION),
                scope,
            )?;
            let body = evaluate(
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

/// Assert that an expression is of type `Type`.
fn assert_expression_is_a_type(expression: Expression, scope: &FunctionScope<'_>) -> Result<()> {
    let type_of_expression = infer_type(expression, scope)?;
    assert_expressions_are_equal(Expression::TypeLiteral, type_of_expression, scope)
}

/// Infer type of expression and match it with the type annotation.
///
/// Returns infered type and for convenience also checks if the supplied type annotation is actually a type.
fn match_expression_with_type_annotation(
    expression: Expression,
    type_annotation: Expression,
    scope: &FunctionScope<'_>,
) -> Result<Expression> {
    assert_expression_is_a_type(type_annotation.clone(), scope)?;
    let infered_type = infer_type(expression.clone(), scope)?;
    assert_expressions_are_equal(type_annotation, infered_type.clone(), scope)?;

    Ok(infered_type)
}

/// Assert that two expression are equal under evaluation/normalization.
///
/// `expected` is the "expected" expression, `actual` is the "actual" expression.
// @Task improve API!
fn assert_expressions_are_equal(
    expected: Expression,
    actual: Expression,
    scope: &FunctionScope<'_>,
) -> Result<()> {
    if !evaluating_equal(expected.clone(), actual.clone(), scope)? {
        Err(Error::ExpressionsNotEqual { expected, actual })
    } else {
        Ok(())
    }
}

/// Shows if two expressions are equal under evaluation/normalization.
fn evaluating_equal(
    left: Expression,
    right: Expression,
    scope: &FunctionScope<'_>,
) -> Result<bool> {
    Ok(equal(
        evaluate(left, scope)?,
        evaluate(right, scope)?,
        scope,
    ))
}

/// Shows if two expressions are syntactically equal.
fn equal(left: Expression, right: Expression, scope: &FunctionScope<'_>) -> bool {
    match (left, right) {
        (Expression::Path(left), Expression::Path(right)) => left.identifier == right.identifier,
        (Expression::Application(left), Expression::Application(right)) => {
            equal(left.callee.clone(), right.callee.clone(), scope)
                && equal(left.argument.clone(), right.argument.clone(), scope)
        }
        (Expression::TypeLiteral, Expression::TypeLiteral)
        | (Expression::NatTypeLiteral, Expression::NatTypeLiteral) => true,
        (Expression::NatLiteral(left), Expression::NatLiteral(right)) => left.value == right.value,
        (Expression::PiTypeLiteral(left), Expression::PiTypeLiteral(right)) => {
            equal(
                left.domain.clone().clone(),
                right.domain.clone().clone(),
                scope,
            ) && match (left.parameter.clone(), right.parameter.clone()) {
                (Some(left_parameter), Some(right_parameter)) => equal(
                    left.codomain.clone(),
                    substitute(
                        right.codomain.clone(),
                        &Substitutions::new().extend_with(
                            right_parameter.clone(),
                            expr! {
                                Path {
                                    identifier: left_parameter.clone(),

                                }
                            },
                        ),
                        scope,
                    ),
                    scope,
                ),
                (Some(left_parameter), None) => equal(
                    left.codomain.clone(),
                    right.codomain.clone(),
                    &scope.extend_with_parameter(left_parameter.clone(), left.domain.clone()),
                ),
                (None, Some(right_parameter)) => equal(
                    left.codomain.clone(),
                    right.codomain.clone(),
                    &scope.extend_with_parameter(right_parameter.clone(), right.domain.clone()),
                ),
                (None, None) => equal(left.codomain.clone(), right.codomain.clone(), scope),
            }
        }
        (Expression::LambdaLiteral(left), Expression::LambdaLiteral(right)) => {
            equal(
                left.parameter_type_annotation
                    .clone()
                    .expect(MISSING_ANNOTATION)
                    .clone(),
                right
                    .parameter_type_annotation
                    .clone()
                    .expect(MISSING_ANNOTATION)
                    .clone(),
                scope,
            ) && equal(
                left.body.clone(),
                substitute(
                    right.body.clone(),
                    &Substitutions::new().extend_with(
                        right.parameter.clone().clone(),
                        expr! {
                            Path {
                                identifier: left.parameter.clone().clone(),

                            }
                        },
                    ),
                    scope,
                ),
                scope,
            )
        }
        // @Temporary
        (Expression::CaseAnalysis(_), Expression::CaseAnalysis(_)) => unreachable!(),
        // @Note this is really bad when we decide to add new stuff! but there is no
        // viable alternative really :/
        _ => false,
    }
}
