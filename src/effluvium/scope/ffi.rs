// @Task make this somewhat type-safe or at least more convenient

use std::convert::TryInto;
use std::rc::Rc;

use super::{ModuleScope, Identifier};
use crate::hir::{expr, Expression};

pub type ForeignFunction = fn(arguments: &[Expression]) -> Expression;

pub fn register_foreign_bindings(scope: ModuleScope) {
    scope.clone().insert_untyped_foreign_binding("add", 2, |arguments| {
        let [x, y]: &[_; 2] = arguments.try_into().unwrap();
        let x = assume!(NatLiteral(x));
        let y = assume!(NatLiteral(y));

        expr! {
            NatLiteral {
                value: Rc::new(&*x.value + &*y.value),
            }
        }
    });
    // @Question how do we *robustly* represent foreign function which use user-defined types like `Bool`?
    // we need to correctly resolve them! We need to declare depencencies or don't we? I mean `False` needs to be resolved
    // to `::lushui::bool::False`
    scope.insert_untyped_foreign_binding("equal", 2, |arguments| {
        let [x, y]: &[_; 2] = arguments.try_into().unwrap();
        let x = assume!(NatLiteral(x));
        let y = assume!(NatLiteral(y));

        if x.value == y.value {
            expr! {
                Path {
                    identifier: Identifier::from("True"),
                }
            }
        } else {
            expr! {
                Path {
                    identifier: Identifier::from("False"),
                }
            }
        }
    });
}

macro assume($variant:ident($ident:ident)) {
    match $ident {
        Expression::$variant(expression) => expression,
        _ => unreachable!(),
    }
}
