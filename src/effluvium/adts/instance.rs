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

use crate::hir::{expression, Expression, Identifier};
use crate::effluvium::{Result, ModuleScope, Error, equal};

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
        Expression::Path(
            expression::Path {
                identifier: type_name,
            },
            (),
        ),
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
fn result_type(mut expression: Expression, scope: ModuleScope) -> Expression {
    loop {
        expression = match expression {
            Expression::PiTypeLiteral(literal, _) => {
                if let Some(parameter) = literal.parameter.clone() {
                    scope
                        .clone()
                        .insert_parameter(parameter, literal.domain.clone());
                }
                literal.codomain.clone()
            }
            Expression::Application(_, _)
            | Expression::TypeLiteral(_, _)
            | Expression::NatTypeLiteral(_, _)
            | Expression::Path(_, _) => {
                return expression;
            }
            Expression::Hole(_, _) => unimplemented!(),
            Expression::LambdaLiteral(_, _)
            | Expression::NatLiteral(_, _)
            | Expression::UseIn(_, _)
            | Expression::CaseAnalysis(_, _) => unreachable!(),
        }
    }
}

// returns the `f` in `f a b c`
fn callee(mut expression: Expression) -> Expression {
    loop {
        expression = match expression {
            Expression::Application(application, _) => application.expression.clone(),
            expression => return expression,
        }
    }
}
