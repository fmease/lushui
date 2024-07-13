// @Task get rid of interfaceable::{Type, Value} if possible, it's needless indirection

use utility::{Int, Nat};

// @Beacon @Task make this an enum { Intrinsic(_), Known(_) } to DRY
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
    Option { ty: Type, value: Option<Box<Value>> },
    IO { index: usize, args: Vec<Value> },
}

/// Rust types that can be mapped to interfaceable lushui types.
///
/// This trait is not strictly necessary but it makes defining intrinsic functions on
/// the Rust side much more ergonomic!
pub(crate) trait IntoValue {
    fn into_ty() -> Type;
    fn into_value(self) -> Value;
}

impl<V: IntoValue> From<V> for Value {
    fn from(value: V) -> Self {
        value.into_value()
    }
}

macro simple_value_correspondence($( $rust_ty:ty => $lushui_ty:ident ),+ $(,)?) {
    $(
        impl IntoValue for $rust_ty {
            fn into_ty() -> Type {
                Type::$lushui_ty
            }

            fn into_value(self) -> Value {
                Value::$lushui_ty(self)
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
    fn into_ty() -> Type {
        Type::Unit
    }

    fn into_value(self) -> Value {
        Value::Unit
    }
}

impl<V: IntoValue> IntoValue for Option<V> {
    fn into_ty() -> Type {
        Type::Option(Box::new(V::into_ty()))
    }

    fn into_value(self) -> Value {
        Value::Option {
            ty: V::into_ty(),
            value: self.map(|value| Box::new(value.into())),
        }
    }
}
