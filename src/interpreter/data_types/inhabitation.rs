//! Inhabitation analysis.

use crate::{
    hir::{Expression, ExpressionKind},
    interpreter::ModuleScope,
    resolver::Identifier,
};

impl Expression<Identifier> {
    /// Indicate whether a type is uninhabited.
    // @Note assumes expression_is_type(r#type, ..) holds
    // @Task we need to consider polymorphic types like `Identity` (with `Identity'`) where
    // `Identity Uninhabited` is uninhabited but `Identity Inhabited` is inhabited
    // @Note becomes important when we handle Expression::Application
    pub fn _is_uninhabited(self, _scope: ModuleScope) -> bool {
        use ExpressionKind::*;

        match self.kind {
            PiType(_literal) => {
                // return |codomain|^|domain| (consider dependent types|parameters)
                todo!()
            }
            // @Note we need to be able pass type arguments to is_uninhabited!
            Application(_application) => todo!(),
            Type => false,
            Binding(_path) => {
                // @Task look up path in context and decide upon returned information
                // if is an ADT, go through every constructor and for each one check
                // @Beacon `is_applicable` which checks whether the domain (only!) of the type
                // is uninhabited
                todo!()
            }
            // @Question unreachable??
            Lambda(_literal) => unreachable!(),
            UseIn => todo!(),
            CaseAnalysis(_case_analysis) => todo!(),
            Nat(_) | Text(_) => unreachable!(),
            Substitution(_) => todo!(),
            UnsaturatedForeignApplication(_) => todo!(),
        }
    }
}
