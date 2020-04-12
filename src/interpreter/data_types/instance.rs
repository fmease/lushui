//! Instance checking.
//!
//! I.e. does a constructor of an algebraïc data type return a valid
//! instance of the respective type?
//!
//! Note: Currently, the checker does allow existential type parameters
//! and specialized instances. This will complicate the implementation
//! of case analysis. Of course, feature-complete Lushui shall support
//! existentials and specialized instances but we first might want to
//! feature-gate them.

use crate::{
    diagnostic::{Code, Diagnostic, Level, Result},
    hir::{Expression, ExpressionKind},
    interpreter::{FunctionScope, ModuleScope},
    resolver::Identifier,
};

pub(in crate::interpreter) fn assert_constructor_is_instance_of_type(
    constructor_name: Identifier,
    constructor: Expression<Identifier>,
    r#type: Expression<Identifier>,
    scope: &ModuleScope,
) -> Result<()> {
    let result_type = constructor.result_type(&FunctionScope::Module(scope));
    let callee = result_type.callee();

    if !r#type.equals(callee, &FunctionScope::Module(scope))? {
        // @Task improve error diagnostic
        // @Task add span information
        Err(Diagnostic::new(
            Level::Fatal,
            Code::E033,
            format!("invalid constructor `{}`", constructor_name),
        ))
    } else {
        Ok(())
    }
}

impl Expression<Identifier> {
    // @Question @Bug returns are type that might depend on parameters which we don't supply!!
    // gets R in A -> B -> C -> R plus an environment b.c. R could depend on outer stuff
    // @Note this function assumes that the expression has already been normalized!
    fn result_type(self, scope: &FunctionScope<'_>) -> Self {
        use ExpressionKind::*;

        match self.kind {
            PiType(literal) => {
                if literal.parameter.is_some() {
                    let scope = scope.extend_with_parameter(literal.domain.clone());
                    literal.codomain.clone().result_type(&scope)
                } else {
                    literal.codomain.clone().result_type(scope)
                }
            }
            Application(_)
            | Type
            | Binding(_) => self,
            Lambda(_)
            | Nat(_)
            | Text(_)
            | UseIn
            | CaseAnalysis(_)
            // @Note not sure
            | Substitution(_)
            | ForeignApplication(_) => unreachable!(),
        }
    }

    /// Returns the callee of an expression.
    ///
    /// Example: Returns the `f` in `f a b c`.
    fn callee(mut self) -> Self {
        loop {
            self = match self.kind {
                ExpressionKind::Application(application) => application.callee.clone(),
                _ => return self,
            }
        }
    }
}
