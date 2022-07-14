// @Task document meaning behind the word “interfaceable”

use crate::{BareExpression, Expression};
use lushui_ast::Explicitness;
use lushui_error::Result;
use lushui_utilities::{condition, Int, Nat};
use std::default::default;

// @Beacon @Beacon @Beacon @Temporary
type BuildSession = ();

/// An “interfaceable” type (i.e. an intrinsic or known type).
pub enum Type {
    Unit,
    Bool,
    Nat,
    Nat32,
    Nat64,
    Int,
    Int32,
    Int64,
    Text,
    Option(Box<Type>),
}

impl Type {
    // @Task improve this code with the new enum logic
    fn from_expression(expression: &Expression, session: &BuildSession) -> Option<Self> {
        use crate::intrinsic::{NumericType::*, Type::*};
        use crate::known::Binding::*;

        let known = |binding: &crate::Binding, known: crate::known::Binding| {
            // session
            //     .known_binding(known)
            //     .map_or(false, |known| &binding.0 == known)
            todo!() // @Beacon @Beacon @Beacon @Task
        };
        let intrinsic = |binding: &crate::Binding, intrinsic: crate::intrinsic::Type| {
            // session
            //     .intrinsic_type(intrinsic)
            //     .map_or(false, |intrinsic| &binding.0 == intrinsic)
            todo!() // @Beacon @Beacon @Beacon @Task
        };

        Some(match &expression.bare {
            // @Note this lookup looks incredibly inefficient
            BareExpression::Binding(binding) => condition! {
                known(binding, Unit) => Self::Unit,
                known(binding, Bool) => Self::Bool,
                intrinsic(binding, Nat.into()) => Self::Nat,
                intrinsic(binding, Nat32.into()) => Self::Nat32,
                intrinsic(binding, Nat64.into()) => Self::Nat64,
                intrinsic(binding, Int.into()) => Self::Int,
                intrinsic(binding, Int32.into()) => Self::Int32,
                intrinsic(binding, Int64.into()) => Self::Int64,
                intrinsic(binding, Text) => Self::Text,
                else => return None,
            },
            BareExpression::Application(application) => match &application.callee.bare {
                BareExpression::Binding(binding) if known(binding, Option) => Self::Option(
                    Box::new(Self::from_expression(&application.argument, session)?),
                ),
                _ => return None,
            },
            _ => return None,
        })
    }

    // @Task move somewhere else!
    // fn into_expression(self, component: &Component, session: &BuildSession) -> Result<Expression> {
    //     // use crate::intrinsic::{NumericType::*, Type::*};
    //     // use crate::known::Definition::*;

    //     // let intrinsic = |binding| session.look_up_intrinsic_type(binding, None);
    //     // let known = |binding| session.look_up_known_binding(binding);

    //     // match self {
    //     //     Self::Unit => known(Unit),
    //     //     Self::Bool => known(Bool),
    //     //     Self::Nat => intrinsic(Nat.into()),
    //     //     Self::Nat32 => intrinsic(Nat32.into()),
    //     //     Self::Nat64 => intrinsic(Nat64.into()),
    //     //     Self::Int => intrinsic(Int.into()),
    //     //     Self::Int32 => intrinsic(Int32.into()),
    //     //     Self::Int64 => intrinsic(Int64.into()),
    //     //     Self::Text => intrinsic(Text),
    //     //     Self::Option(type_) => Ok(application(
    //     //         known(Option)?,
    //     //         type_.into_expression(component, session)?,
    //     //     )),
    //     // }

    //     todo!() // @Beacon @Beacon @Beacon @Task
    // }
}

/// An “interfaceable” value (i.e. an intrinsic or known value).
pub enum Value {
    Unit,
    Bool(bool),
    Text(String),
    Nat(Nat),
    Nat32(u32),
    Nat64(u64),
    Int(Int),
    Int32(i32),
    Int64(i64),
    Option {
        type_: Type,
        value: Option<Box<Value>>,
    },
    IO {
        index: usize,
        arguments: Vec<Value>,
    },
}

impl Value {
    pub fn from_expression(expression: &Expression, session: &BuildSession) -> Option<Self> {
        use crate::known::Binding::*;
        use BareExpression::*;

        let known = |binding: &crate::Binding, known: crate::known::Binding| {
            // session
            //     .known_binding(known)
            //     .map_or(false, |known| &binding.0 == known)
            todo!() // @Beacon @Beacon @Beacon @Task
        };

        Some(match &expression.bare {
            Text(text) => {
                use crate::Text::*;

                match &**text {
                    // @Note not great
                    Text(text) => Self::Text(text.clone()),
                }
            }
            Number(number) => {
                use crate::Number::*;

                match &**number {
                    Nat(nat) => Self::Nat(nat.clone()),
                    Nat32(nat) => Self::Nat32(*nat),
                    Nat64(nat) => Self::Nat64(*nat),
                    Int(int) => Self::Int(int.clone()),
                    Int32(int) => Self::Int32(*int),
                    Int64(int) => Self::Int64(*int),
                }
            }
            Binding(binding) => condition! {
                known(binding, UnitUnit) => Value::Unit,
                known(binding, BoolFalse) => Value::Bool(false),
                known(binding, BoolTrue) => Value::Bool(true),
                else => return None,
            },

            Application(application0) => match &application0.callee.bare {
                Binding(binding) if known(binding, OptionNone) => Value::Option {
                    value: None,
                    type_: self::Type::from_expression(&application0.argument, session)?,
                },
                Application(application1) => match &application1.callee.bare {
                    Binding(binding) if known(binding, OptionSome) => Value::Option {
                        value: Some(Box::new(Self::from_expression(
                            &application0.argument,
                            session,
                        )?)),
                        type_: self::Type::from_expression(&application1.argument, session)?,
                    },
                    _ => return None,
                },
                _ => return None,
            },
            _ => return None,
        })
    }

    // @Task move somewhere else!
    // pub fn into_expression(
    //     self,
    //     component: &Component,
    //     session: &BuildSession,
    // ) -> Result<Expression> {
    //     // use crate::known::Definition::*;
    //     // use crate::{Number::*, Text::*};

    //     // Ok(match self {
    //     //     Self::Unit => session.look_up_known_binding(Unit)?,
    //     //     Self::Bool(value) => {
    //     //         session.look_up_known_binding(if value { BoolTrue } else { BoolFalse })?
    //     //     }
    //     //     Self::Text(value) => Expression::new(default(), default(), Text(value).into()),
    //     //     Self::Nat(value) => Expression::new(default(), default(), Nat(value).into()),
    //     //     Self::Nat32(value) => Expression::new(default(), default(), Nat32(value).into()),
    //     //     Self::Nat64(value) => Expression::new(default(), default(), Nat64(value).into()),
    //     //     Self::Int(value) => Expression::new(default(), default(), Int(value).into()),
    //     //     Self::Int32(value) => Expression::new(default(), default(), Int32(value).into()),
    //     //     Self::Int64(value) => Expression::new(default(), default(), Int64(value).into()),
    //     //     Self::Option { type_, value } => match value {
    //     //         Some(value) => application(
    //     //             application(
    //     //                 session.look_up_known_binding(OptionSome)?,
    //     //                 type_.into_expression(component, session)?,
    //     //             ),
    //     //             value.into_expression(component, session)?,
    //     //         ),
    //     //         None => application(
    //     //             session.look_up_known_binding(OptionNone)?,
    //     //             type_.into_expression(component, session)?,
    //     //         ),
    //     //     },
    //     //     Self::IO { index, arguments } => Expression::new(
    //     //         default(),
    //     //         default(),
    //     //         crate::IO {
    //     //             index,
    //     //             arguments: arguments
    //     //                 .into_iter()
    //     //                 .map(|argument| argument.into_expression(component, session))
    //     //                 .collect::<Result<Vec<_>>>()?,
    //     //         }
    //     //         .into(),
    //     //     ),
    //     // })

    //     todo!() // @Beacon @Beacon @Beacon @Task
    // }
}

/// Rust types that can be mapped to FFI-compatible lushui types.
///
/// This trait is not strictly necessary but it makes defining intrinsic functions on
/// the Rust side much more ergonomic!
pub(crate) trait IntoValue {
    fn into_type() -> Type;
    fn into_value(self) -> Value;
}

impl<V: IntoValue> From<V> for Value {
    fn from(value: V) -> Self {
        value.into_value()
    }
}

macro simple_value_correspondence($( $rust_type:ty => $lushui_type:ident ),+ $(,)?) {
    $(
        impl IntoValue for $rust_type {
            fn into_type() -> Type {
                Type::$lushui_type
            }

            fn into_value(self) -> Value {
                Value::$lushui_type(self)
            }
        }
    )+
}

simple_value_correspondence! {
    bool => Bool,
    String => Text,
    Nat => Nat,
    u32 => Nat32,
    u64 => Nat64,
    Int => Int,
    i32 => Int32,
    i64 => Int64,
}

impl IntoValue for () {
    fn into_type() -> Type {
        Type::Unit
    }

    fn into_value(self) -> Value {
        Value::Unit
    }
}

impl<V: IntoValue> IntoValue for Option<V> {
    fn into_type() -> Type {
        Type::Option(Box::new(V::into_type()))
    }

    fn into_value(self) -> Value {
        Value::Option {
            type_: V::into_type(),
            value: self.map(|value| Box::new(value.into())),
        }
    }
}

fn application(callee: Expression, argument: Expression) -> Expression {
    Expression::new(
        default(),
        default(),
        crate::Application {
            callee,
            argument,
            explicitness: Explicitness::Explicit,
        }
        .into(),
    )
}
