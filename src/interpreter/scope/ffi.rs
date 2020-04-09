// @Task make this somewhat type-safe or at least more convenient

use std::collections::VecDeque;

use super::ModuleScope;
use crate::interpreter::{self, Expression};

pub type ForeignFunction = fn(arguments: VecDeque<Expression>) -> Expression;

// pub enum Value {
//     Unit,
//     Bool(bool),
//     // @Question which inner type?
//     Text(String),
//     Nat(crate::Nat),
//     Option(Option<Box<Value>>),
// }

pub fn register_foreign_bindings(scope: &mut ModuleScope) {
    // @Note very ad-hoc
    scope.register_foreign_data(interpreter::NAT_TYPE_NAME);
    scope.register_foreign_data(interpreter::TEXT_TYPE_NAME);

    // scope
    //     .clone()
    //     .insert_untyped_foreign_binding("add", 2, |arguments| {
    //         todo!()
    //         let x = assume!(Nat(&arguments[0]));
    //         let y = assume!(Nat(&arguments[1]));

    //         expr! {
    //             Nat[] {
    //                 value: &x.value + &y.value,
    //             }
    //         }
    //     });
    // // @Question how do we *robustly* represent foreign function which use user-defined types like `Bool`?
    // // we need to correctly resolve them! We need to declare depencencies or don't we? I mean `False` needs to be resolved
    // // to `::lushui::bool::False`
    // scope
    //     .clone()
    //     .insert_untyped_foreign_binding("equal", 2, |arguments| {
    //         let x = assume!(Nat(&arguments[0]));
    //         let y = assume!(Nat(&arguments[1]));

    //         if x.value == y.value {
    //             expr! {
    //                 Binding[] {
    //                     binder: Identifier::from("True"),
    //                 }
    //             }
    //         } else {
    //             expr! {
    //                 Binding[] {
    //                     binder: Identifier::from("False"),
    //                 }
    //             }
    //         }
    //     });

    // // just testing whether this works as easy as I think it will
    // // @Temporary (we need `Text` for a proper one!)
    // scope.insert_untyped_foreign_binding("panic", 2, |arguments| {
    //     let message = assume!(Text(&arguments[1]));

    //     panic!("lushui panicked with argument {:?}", message.value);
    // });
}

// macro assume($variant:ident($value:expr)) {
//     match $value.kind {
//         ExpressionKind::$variant(expression) => expression,
//         _ => unreachable!(),
//     }
// }
