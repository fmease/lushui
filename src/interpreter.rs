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

mod data_types;
mod error;
mod scope;

use crate::desugar::{self, expr, Declaration, Expression};
use crate::parser::Explicitness;
use error::{Error, Result};
pub use scope::ModuleScope;
use scope::{FunctionScope, Substitutions};

/// An internal compiler bug message.
///
/// We don't do non-trivial type inference yet and thus, type parameters of lambda literals
/// must be annotated with a type. And since we don't have a dedicated `Error::InternalCompilerError` yet,
/// we use this constant for an error message.
// @Bug
const MISSING_ANNOTATION: &str =
    "(compiler bug) currently lambda literal parameters and patterns must be type-annotated";

/// Try to type check and evaluate a declaration modifying the given scope.
// @Task support order-independence, recursion and proper modules
pub fn evaluate_declaration(declaration: &Declaration, module_scope: ModuleScope) -> Result<()> {
    match declaration {
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

            // @Task diagnostic note: only `Type` can be extended
            // @Note currently is: invalid constructor X
            data_types::instance::assert_constructor_is_instance_of_type(
                data_type_binder.clone(),
                r#type.clone(),
                Expression::TypeLiteral,
                module_scope.clone(),
            )?;

            module_scope
                .clone()
                .insert_data_binding(data_type_binder.clone(), r#type);

            for desugar::Constructor {
                binder,
                type_annotation,
            } in constructors
            {
                let r#type = evaluate(type_annotation.clone(), &function_scope)?;
                assert_expression_is_a_type(r#type.clone(), &function_scope)?;

                data_types::instance::assert_constructor_is_instance_of_type(
                    binder.clone(),
                    r#type.clone(),
                    expr! { Path { identifier: data_type_binder.clone() } },
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
    }

    Ok(())
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
                    substitutions,
                    scope,
                ),
                argument: substitute(application.argument.clone(), substitutions, scope),
                explicitness: Explicitness::Explicit,
            }
        },
        Expression::UseIn(_) => todo!(),
        Expression::CaseAnalysis(case_analysis) => expr! {
            CaseAnalysis {
                subject: substitute(
                    case_analysis.subject.clone(),
                    substitutions,
                    scope,
                ),
                cases: case_analysis
                    .cases
                    .iter()
                    .map(|case| {
                        data_types::case_analysis::case_analysis_case_substitute(
                            case.clone(),
                            substitutions,
                            scope,
                        )
                    })
                    .collect(),
            }
        },
        Expression::TypeLiteral
        | Expression::NatTypeLiteral
        | Expression::NatLiteral(_)
        | Expression::TextTypeLiteral
        | Expression::TextLiteral(_) => expression,
        // @Note I think
        Expression::UnsaturatedForeignApplication(_) => unreachable!(),
    }
}

/// Try to infer the type of an expression.
pub fn infer_type(
    expression: Expression,
    scope: &FunctionScope<'_>,
) -> Result<desugar::Expression> {
    Ok(match expression {
        Expression::Path(path) => scope
            .lookup_type(&path.identifier)
            .ok_or_else(|| Error::UndefinedBinding(path.identifier.clone()))?,
        Expression::TypeLiteral | Expression::NatTypeLiteral | Expression::TextTypeLiteral => {
            Expression::TypeLiteral
        }
        Expression::NatLiteral(_) => Expression::NatTypeLiteral,
        Expression::TextLiteral(_) => Expression::TextTypeLiteral,
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
                assert_expected_expression_equals_actual(
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
                    assert_expected_expression_equals_actual(
                        literal.domain.clone(),
                        argument_type,
                        scope,
                    )?;
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
        Expression::UseIn(_) => todo!(),
        // @Beacon @Beacon @Beacon @Temporary @Task
        // first: fiddeling, then: building abstractions
        // @Bug this is *not* principled design
        Expression::CaseAnalysis(case_analysis) => {
            let r#type = infer_type(case_analysis.subject.clone(), scope)?;
            // @Task verify that
            // * patterns are of correct type (i.e. r#type is an ADT and the constructors are the valid ones)
            // * all constructors are covered
            // * all case_analysis.cases>>.expressions are of the same type

            match &r#type {
                Expression::Path(_) => {}
                Expression::Application(_application) => todo!("polymorphic types in patterns"),
                _ => todo!("encountered unsupported type to be case-analysed"),
            };

            use desugar::expression::Pattern;

            let mut type_of_previous_body = None::<Expression>;

            for case in case_analysis.cases.iter() {
                match &case.pattern {
                    Pattern::NatLiteral(_) => todo!("nat literal patterns"),
                    Pattern::Path {
                        path,
                        type_annotation,
                    } => {
                        if scope.is_constructor(&path.identifier) {
                            let type_of_constructor = scope.lookup_type(&path.identifier).unwrap();
                            if let Some(annotation) = type_annotation {
                                assert_expected_expression_equals_actual(
                                    annotation.clone(),
                                    type_of_constructor.clone(),
                                    scope,
                                )?;
                            }
                            // @Note error message very general, could be specialized to constructors
                            // once our diagnostic system is in place
                            assert_expected_expression_equals_actual(
                                r#type.clone(),
                                type_of_constructor.clone(),
                                scope,
                            )?;
                        } else {
                            todo!("bindings inside of patterns");
                        }
                        // todo!() // @Beacon @Beacon @Beacon @Task
                    }
                    Pattern::Application {
                        callee: _,
                        argument: _,
                    } => todo!("application patterns"),
                }
                // @Task @Beacon insert bindings from pattern when type checking body
                let r#type = infer_type(case.body.clone(), scope)?;

                match type_of_previous_body {
                    Some(ref previous_type) => {
                        assert_expected_expression_equals_actual(
                            previous_type.clone(),
                            r#type,
                            scope,
                        )?;
                    }
                    None => {
                        type_of_previous_body = Some(r#type);
                    }
                }
            }

            type_of_previous_body.unwrap_or_else(|| {
                let parameter = desugar::Identifier::sourced("A").refresh();

                expr! {
                    PiTypeLiteral {
                        parameter: Some(parameter.clone()),
                        domain: Expression::TypeLiteral,
                        codomain: expr! { Path { identifier: parameter } },
                        explicitness: Explicitness::Implicit,
                    }
                }
            })
        }
        Expression::UnsaturatedForeignApplication(_) => todo!(),
    })
}

use std::collections::VecDeque;

/// Try to evaluate an expression.
///
/// This is beta-reduction I think.
// @Task differenciate between Expression<InitialPhase> and Expression<Normalized>
pub fn evaluate(expression: Expression, scope: &FunctionScope<'_>) -> Result<Expression> {
    Ok(match expression.clone() {
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
                // @Bug currentky not checked that it does not contain any parameters/is a non-value/is not
                // FFI-compatible
                Expression::Path(path) if scope.is_foreign(&path.identifier) => scope
                    .try_apply_foreign_binding(dbg!(&path.identifier), {
                        let mut arguments = VecDeque::with_capacity(1);
                        arguments.push_back(argument);
                        arguments
                    })?,
                Expression::UnsaturatedForeignApplication(application) => scope
                    .try_apply_foreign_binding(&application.callee, {
                        let mut arguments = application.arguments.clone();
                        arguments.push_back(argument);
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
        Expression::TypeLiteral
        | Expression::NatTypeLiteral
        | Expression::NatLiteral(_)
        | Expression::TextTypeLiteral
        | Expression::TextLiteral(_) => expression,
        Expression::PiTypeLiteral(literal) => {
            let domain = evaluate(literal.domain.clone(), scope)?;
            let codomain = match literal.parameter.clone() {
                Some(parameter) => evaluate(
                    literal.codomain.clone(),
                    &scope.extend_with_parameter(parameter, domain.clone()),
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
        Expression::UseIn(_) => todo!(),
        // @Note @Beacon, now, meta information would be nice, so we don't need to do
        // double work (the work of `infer_type` again)
        // @Beacon @Beacon @Beacon @Note this code is @Temporary as hell.
        // I just need to implement enough of it till I start building good
        // abstractions
        // @Note partially applied constructors differ from normal values
        // I guess it's very likely that the first code we write will handle them incorrectly
        // because the code will not check for the arity of the neutral application
        // @Note how to handle them: just like functions: case analysis only wotks with a binder-case
        // (default case)
        Expression::CaseAnalysis(case_analysis) => {
            use desugar::expression::Pattern;

            let subject = evaluate(case_analysis.subject.clone(), scope)?;

            // @Note we assume, subject is composed of only applications, paths
            // and natural number literals corresponding to the pattern types we
            // want to support right now
            // everything else should be impossible because of type checking
            // but I might be wrong. possible counter examples: unevaluated case
            // analysis expressions etc
            match subject {
                Expression::Path(subject_path) => {
                    // @Beacon @Beacon @Task
                    if scope.is_constructor(&subject_path.identifier) {
                        for case in case_analysis.cases.iter() {
                            match &case.pattern {
                                Pattern::NatLiteral(_) => todo!(),
                                Pattern::Path { path, .. } => {
                                    if path.identifier == subject_path.identifier {
                                        // @Task @Beacon extend with parameters when evaluating
                                        return evaluate(case.body.clone(), scope);
                                    }
                                }
                                Pattern::Application { .. } => todo!(),
                            }
                        }
                        // we should not be here
                        // @Note this is currently reachable because we don't do a check for
                        // exhaustiveness in `infer_type`, just fyi
                        unreachable!()
                    } else {
                        expression
                    }
                }
                Expression::Application(_application) => todo!(),
                Expression::NatLiteral(_literal) => todo!(),
                // @Note reachable if they contain neutrals, right??
                _ => unreachable!(),
            }
        }
        // @Question or should the argument be normalized *again*? (under a possibly new scope?)
        Expression::UnsaturatedForeignApplication(_) => expression,
    })
}

/// Assert that an expression is of type `Type`.
fn assert_expression_is_a_type(expression: Expression, scope: &FunctionScope<'_>) -> Result<()> {
    let type_of_expression = infer_type(expression, scope)?;
    assert_expected_expression_equals_actual(Expression::TypeLiteral, type_of_expression, scope)
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
    let infered_type = infer_type(expression, scope)?;
    assert_expected_expression_equals_actual(type_annotation, infered_type.clone(), scope)?;

    Ok(infered_type)
}

/// Assert that two expression are equal under evaluation/normalization.
fn assert_expected_expression_equals_actual(
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

/// Dictates if two expressions are syntactically alpha-equivalent.
fn equal(left: Expression, right: Expression, scope: &FunctionScope<'_>) -> bool {
    match (left, right) {
        (Expression::Path(left), Expression::Path(right)) => left.identifier == right.identifier,
        (Expression::Application(left), Expression::Application(right)) => {
            equal(left.callee.clone(), right.callee.clone(), scope)
                && equal(left.argument.clone(), right.argument.clone(), scope)
        }
        (Expression::TypeLiteral, Expression::TypeLiteral)
        | (Expression::NatTypeLiteral, Expression::NatTypeLiteral)
        | (Expression::TextTypeLiteral, Expression::TextTypeLiteral) => true,
        (Expression::NatLiteral(left), Expression::NatLiteral(right)) => left.value == right.value,
        (Expression::TextLiteral(left), Expression::TextLiteral(right)) => {
            left.value == right.value
        }
        (Expression::PiTypeLiteral(left), Expression::PiTypeLiteral(right)) => {
            equal(left.domain.clone(), right.domain.clone(), scope)
                && match (left.parameter.clone(), right.parameter.clone()) {
                    (Some(left_parameter), Some(right_parameter)) => equal(
                        left.codomain.clone(),
                        substitute(
                            right.codomain.clone(),
                            &Substitutions::new().extend_with(
                                right_parameter,
                                expr! {
                                    Path {
                                        identifier: left_parameter,

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
                        &scope.extend_with_parameter(left_parameter, left.domain.clone()),
                    ),
                    (None, Some(right_parameter)) => equal(
                        left.codomain.clone(),
                        right.codomain.clone(),
                        &scope.extend_with_parameter(right_parameter, right.domain.clone()),
                    ),
                    (None, None) => equal(left.codomain.clone(), right.codomain.clone(), scope),
                }
        }
        (Expression::LambdaLiteral(left), Expression::LambdaLiteral(right)) => {
            equal(
                left.parameter_type_annotation
                    .clone()
                    .expect(MISSING_ANNOTATION),
                right
                    .parameter_type_annotation
                    .clone()
                    .expect(MISSING_ANNOTATION),
                scope,
            ) && equal(
                left.body.clone(),
                substitute(
                    right.body.clone(),
                    &Substitutions::new().extend_with(
                        right.parameter.clone(),
                        expr! {
                            Path {
                                identifier: left.parameter.clone(),

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
