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
    desugar::{Expression, ExpressionKind},
    interpreter::{Error, FunctionScope, ModuleScope, Result, Scope},
    resolver::Identifier,
};

pub(in crate::interpreter) fn assert_constructor_is_instance_of_type(
    constructor_name: Identifier,
    constructor: Expression<Identifier>,
    r#type: Expression<Identifier>,
    scope: &ModuleScope,
) -> Result<()> {
    let result_type = constructor.result_type(&Scope::new(scope, &FunctionScope::Empty));
    let callee = result_type.callee();

    if !r#type.equals(callee, &Scope::new(scope, &FunctionScope::Empty))? {
        Err(Error::InvalidConstructor {
            name: constructor_name,
        })
    } else {
        Ok(())
    }
}

impl Expression<Identifier> {
    // @Question @Bug returns are type that might depend on parameters which we don't supply!!
    // gets R in A -> B -> C -> R plus an environment b.c. R could depend on outer stuff
    // @Note this function assumes that the expression has already been normalized!
    fn result_type(self, scope: &Scope<'_>) -> Self {
        use ExpressionKind::*;

        match self.kind {
            ExpressionKind::PiType(literal) => {
                if let Some(parameter) = literal.parameter.clone() {
                    let scope = Scope {
                        function: &scope.function.extend_with_parameter(parameter, literal.domain.clone()),
                        module:scope.module,
                    };
                    literal.codomain.clone().result_type(&scope)
                } else {
                    literal.codomain.clone().result_type(scope)
                }
            }
            ExpressionKind::Application(_)
            | Type
            | NatType
            | TextType
            | Binding(_) => self,
            Lambda(_)
            | Nat(_)
            | Text(_)
            | UseIn
            | CaseAnalysis(_)
            // @Note not sure
            | Substitution(_)
            | UnsaturatedForeignApplication(_) => unreachable!(),
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
