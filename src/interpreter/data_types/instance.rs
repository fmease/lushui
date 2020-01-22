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

use crate::hir::{expr, Expression, Identifier};
use crate::interpreter::{equal, Error, FunctionScope, ModuleScope, Result};

pub(in crate::interpreter) fn assert_constructor_is_instance_of_type(
    constructor_name: Identifier,
    constructor: Expression,
    type_name: Identifier,
    scope: ModuleScope,
) -> Result<()> {
    if !constructor_is_instance_of_type(constructor, type_name, scope) {
        Err(Error::InvalidConstructor {
            name: constructor_name,
        })
    } else {
        Ok(())
    }
}

// @Note currently allows existential quantification and specialized instances
pub(in crate::interpreter) fn constructor_is_instance_of_type(
    constructor: Expression,
    type_name: Identifier,
    module_scope: ModuleScope,
) -> bool {
    let function_scope = FunctionScope::new(module_scope);
    let result_type = result_type(constructor, &function_scope);
    let callee = callee(result_type);

    equal(
        expr! {
            Path {
                identifier: type_name
            }
        },
        callee,
        &function_scope,
    )
}

// @Question @Bug returns are type that might depend on parameters which we don't supply!!
// gets R in A -> B -> C -> R plus an environment b.c. R could depend on outer stuff
// @Note this function assumes that the expression has already been normalized!
fn result_type(expression: Expression, scope: &FunctionScope<'_>) -> Expression {
    match expression {
        Expression::PiTypeLiteral(literal) => {
            if let Some(parameter) = literal.parameter.clone() {
                let scope = scope.extend_with_parameter(parameter, literal.domain.clone());
                result_type(literal.codomain.clone(), &scope)
            } else {
                result_type(literal.codomain.clone(), scope)
            }
        }
        Expression::Application(_)
        | Expression::TypeLiteral
        | Expression::NatTypeLiteral
        | Expression::Path(_) => expression,
        Expression::LambdaLiteral(_)
        | Expression::NatLiteral(_)
        | Expression::UseIn(_)
        | Expression::CaseAnalysis(_)
        | Expression::UnsaturatedForeignApplication(_) => unreachable!(),
    }
}

/// Returns the callee of an expression.
///
/// Example: Returns the `f` in `f a b c`.
fn callee(mut expression: Expression) -> Expression {
    loop {
        expression = match expression {
            Expression::Application(application) => application.callee.clone(),
            expression => return expression,
        }
    }
}
