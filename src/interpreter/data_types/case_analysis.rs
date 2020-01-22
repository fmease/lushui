//! Case analysis.

use std::rc::Rc;

use crate::hir::expression;
use crate::interpreter::{substitute, FunctionScope, Substitutions};

/// Substitute inside the case of a case analysis.
pub fn case_analysis_case_substitute(
    case: expression::Case,
    substitutions: &Substitutions<'_>,
    scope: &FunctionScope<'_>,
) -> expression::Case {
    // @Note does not return a new substitutions anymore
    let pattern = pattern_substitute(case.pattern, substitutions, scope);

    expression::Case {
        pattern,
        body: substitute(case.body.clone(), substitutions, scope),
    }
}

/// Substitute inside of a pattern.
// @Bug does not work at all anymore!!!
pub fn pattern_substitute<'p>(
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
pub fn _insert_matchables_from_pattern(pattern: &expression::Pattern, scope: &FunctionScope<'_>) {
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
pub fn is_matchable(path: &expression::Path, scope: &FunctionScope<'_>) -> bool {
    path.is_simple() && scope.is_constructor(&path.identifier)
}
