// @Task make this somewhat type-safe or at least more convenient

use std::convert::TryInto;
use std::rc::Rc;

use super::ModuleScope;
use crate::hir::{expr, Expression};

pub type ForeignFunction = fn(arguments: &[Expression]) -> Expression;

pub fn register_foreign_bindings(scope: ModuleScope) {
    scope.insert_untyped_foreign_binding("add", 2, |arguments| {
        let [x, y]: &[_; 2] = arguments.try_into().unwrap();
        let x = assume!(NatLiteral(x));
        let y = assume!(NatLiteral(y));

        expr! {
            NatLiteral {
                value: Rc::new(&*x.value + &*y.value),
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
