//! This module concerns itself with instance checking.
//!
//! I.e. does a constructor of an algebraïc data type return a valid
//! instance of the respective type?
//!
//! Note: Currently, the checker does allow existential type parameters
//! and specialized instances. This will complicate the implementation
//! of case analysis. Of course, feature-complete Lushui shall support
//! existentials and specialized instances but we first might want to
//! feature-gate them.

use std::rc::Rc;

use crate::effluvium::{equal, Error, ModuleScope, Result};
use crate::hir::{expression, Expression, Identifier};

// @Update
// @Task @Beacon
// @Note maybe we want to return an enum with more information:
// * does the constructor have existential type variable (in respect to the type),
//   which means it has type params that are not "part of" the type
// * is it specialized in respect to the type i.e. it does not use some of the
//   type params of the respective type
pub(in crate::effluvium) fn assert_constructor_is_instance_of_type(
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
// we might not want to allow them at first (implementation difficulty of case analysis)
// @Question do we need access to the module scope?
pub(in crate::effluvium) fn constructor_is_instance_of_type(
    constructor: Expression,
    type_name: Identifier,
    scope: ModuleScope,
) -> bool {
    let result_type = result_type(constructor, scope.clone());
    let callee = callee(result_type);

    equal(
        Expression::Path(Rc::new(expression::Path {
            identifier: type_name,
        })),
        callee,
        scope,
    )
}

// // is it an instance, is it exist, is it specialized?
// fn constructor_information_in_respect_to_type(constructor: &Expression, r#type: &Expression) -> ! {
//     unimplemented!()
// }

// for a default value, we should return Option<...>
// gets R in A -> B -> C -> R plus an environment b.c. R could depend on outer stuff
// @Note this function assumes that the expression has already been normalized!
fn result_type(mut expression: Expression, mut scope: ModuleScope) -> Expression {
    loop {
        expression = match expression {
            Expression::PiTypeLiteral(literal) => {
                if let Some(parameter) = literal.parameter.clone() {
                    // @Note very expensive right now :/
                    scope = scope.extend_with_parameter(parameter, literal.domain.clone());
                }
                literal.codomain.clone()
            }
            Expression::Application(_)
            | Expression::TypeLiteral
            | Expression::NatTypeLiteral
            | Expression::Path(_) => {
                return expression;
            }
            Expression::Hole(_) => todo!(),
            Expression::LambdaLiteral(_)
            | Expression::NatLiteral(_)
            | Expression::UseIn(_)
            | Expression::CaseAnalysis(_)
            | Expression::UnsaturatedForeignApplication(_) => unreachable!(),
        }
    }
}

/// Returns the `f` in `f a b c`
fn callee(mut expression: Expression) -> Expression {
    loop {
        expression = match expression {
            Expression::Application(application) => application.callee.clone(),
            expression => return expression,
        }
    }
}
