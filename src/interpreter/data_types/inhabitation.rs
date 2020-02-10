//! Inhabitation analysis.

use crate::hir::Expression;
use crate::interpreter::ModuleScope;

/// Indicate whether a type is uninhabited.
// @Note assumes expression_is_type(r#type, ..) holds
// @Task we need to consider polymorphic types like `Identity` (with `Identity'`) where
// `Identity Uninhabited` is uninhabited but `Identity Inhabited` is inhabited
// @Note becomes important when we handle Expression::Application
pub fn _is_uninhabited(r#type: Expression, _scope: ModuleScope) -> bool {
    match r#type {
        Expression::PiTypeLiteral(_literal) => {
            // return |codomain|^|domain| (consider dependent types|parameters)
            todo!()
        }
        // @Note we need to be able pass type arguments to is_uninhabited!
        Expression::Application(_application) => todo!(),
        Expression::TypeLiteral | Expression::NatTypeLiteral | Expression::TextTypeLiteral => false,
        Expression::Path(_path) => {
            // @Task look up path in context and decide upon returned information
            // if is an ADT, go through every constructor and for each one check
            // @Beacon `is_applicable` which checks whether the domain (only!) of the type
            // is uninhabited
            todo!()
        }
        // @Question unreachable??
        Expression::LambdaLiteral(_literal) => unreachable!(),
        Expression::UseIn(_) => todo!(),
        Expression::CaseAnalysis(_case_analysis) => todo!(),
        Expression::NatLiteral(_) | Expression::TextLiteral(_) => unreachable!(),
        Expression::UnsaturatedForeignApplication(_) => todo!(),
    }
}
