//! Case analysis.

// use std::rc::Rc;

use crate::{desugar::expression, interpreter::Substitution, resolver::Identifier};

/// Substitute inside the case of a case analysis.
pub fn _case_analysis_case_substitute(
    case: expression::Case<Identifier>,
    substitution: Substitution,
) -> expression::Case<Identifier> {
    // @Note does not return a new substitutions anymore
    let pattern = _pattern_substitute(case.pattern, substitution.clone());

    expression::Case {
        pattern,
        body: case.body.clone().substitute(substitution),
    }
}

/// Substitute inside of a pattern.
// @Bug does not work at all anymore!!!
pub fn _pattern_substitute<'p>(
    _pattern: expression::Pattern<Identifier>,
    _substitution: Substitution,
) -> expression::Pattern<Identifier> {
    todo!()
    // match pattern {
    //     expression::Pattern::NatLiteral(_) => pattern,
    //     expression::Pattern::Binding {
    //         binder,
    //         type_annotation,
    //     } => {
    //         let type_annotation = type_annotation
    //             .map(|annotation| annotation.substitute(substitutions.clone(), scope));

    //         if is_matchable(&binder, scope) {
    //             return expression::Pattern::Binding {
    //                 binder,
    //                 type_annotation,
    //             };
    //         }

    //         let refreshed_binder = binder.identifier.refresh();

    //         expression::Pattern::Binder {
    //             binder: expression::Binder {
    //                 identifier: refreshed_binder.clone(),
    //             },
    //             type_annotation,
    //         }

    //         // substitutions.extend_with_substitution(
    //         //     path.identifier.clone(),
    //         //     expr! {
    //         //         Path {
    //         //             identifier: refreshed_binder,
    //         //         }
    //         //     },
    //         // )
    //     }
    //     expression::Pattern::Application { callee, argument } => {
    //         // @Bug disallow `n m` where n is a path which is not matchable
    //         // @Note `(N m) o` is still legal obviously, N matchable, m not
    //         // @Question is there a better place than here?

    //         // @Note does not return a new substitutions anymore
    //         let callee = pattern_substitute(callee.as_ref().clone(), substitutions, scope);
    //         // @Note does not return a new substitutions anymore
    //         let argument = pattern_substitute(argument.as_ref().clone(), substitutions, scope);

    //         expression::Pattern::Application {
    //             callee: Rc::new(callee),
    //             argument: Rc::new(argument),
    //         }
    //     }
    // }
}

// @Beacon @Note this is used in normalize, *not* substitute
// pub fn _insert_matchables_from_pattern(
//     pattern: &expression::Pattern<Identifier>,
//     scope: &FunctionScope<'_>,
// ) {
//     match &pattern {
//         expression::Pattern::Nat(_) => {}
//         expression::Pattern::Binding {
//             binder,
//             type_annotation: _type_annotation,
//         } => {
//             if !is_matchable(binder, scope) {
//                 // @Temporary comment, first replace with pure version
//                 // scope.insert_parameter_binding(
//                 //     path.identifier.clone(),
//                 //     type_annotation.as_ref().expect(MISSING_ANNOTATION).clone(),
//                 // );
//             }
//         }
//         expression::Pattern::Application { callee, argument } => {
//             _insert_matchables_from_pattern(callee, scope);
//             _insert_matchables_from_pattern(argument, scope);
//         }
//     }
// }
