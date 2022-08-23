use crate::interfaceable::Value;
use derivation::{FromStr, Str};
use num_traits::{CheckedDiv, CheckedSub};
use std::{fmt, str::FromStr};
use utilities::HashMap;
/// An intrinsic type.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Type {
    Numeric(NumericType),
    Text,
    IO,
}

impl FromStr for Type {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        use NumericType::*;

        Ok(match input {
            "Nat" => Nat.into(),
            "Nat32" => Nat32.into(),
            "Nat64" => Nat64.into(),
            "Int" => Int.into(),
            "Int32" => Int32.into(),
            "Int64" => Int64.into(),
            "Text" => Self::Text,
            "IO" => Self::IO,
            _ => return Err(()),
        })
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Numeric(type_) => write!(f, "{type_}"),
            Self::Text => write!(f, "Text"),
            Self::IO => write!(f, "IO"),
        }
    }
}

impl From<NumericType> for Type {
    fn from(type_: NumericType) -> Self {
        Self::Numeric(type_)
    }
}

/// An intrinsic numeric type.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum NumericType {
    Nat,
    Nat32,
    Nat64,
    Int,
    Int32,
    Int64,
}

impl NumericType {
    pub const fn interval(self) -> &'static str {
        // @Question use `∞`?
        match self {
            Self::Nat => "[0, infinity)",
            Self::Nat32 => "[0, 2^32-1]",
            Self::Nat64 => "[0, 2^64-1]",
            Self::Int => "(-infinity, infinity)",
            Self::Int32 => "[-2^31, 2^31-1]",
            Self::Int64 => "[-2^63, 2^63-1]",
        }
    }
}

// @Task derive this with `#[format(upper_dash_case)]`
impl fmt::Display for NumericType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Nat => "Nat",
            Self::Nat32 => "Nat32",
            Self::Nat64 => "Nat64",
            Self::Int => "Int",
            Self::Int32 => "Int32",
            Self::Int64 => "Int64",
        })
    }
}

/// An intrinsic function.
#[derive(PartialEq, Eq, Hash, Clone, Copy, FromStr, Str)]
#[format(dash_case)]
pub enum Function {
    Add,
    Subtract,
    // @Temporary
    PanickingSubtract,
    Multiply,
    Divide,
    Equal,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Display,
    Concat,
    AddNat32,
    Print,
    Panic,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

pub type BareFunctionValue = fn(arguments: Vec<super::interfaceable::Value>) -> Value;

pub struct FunctionValue {
    pub arity: usize,
    pub function: BareFunctionValue,
}

pub enum Kind {
    Type,
    Function,
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Type => "type",
            Self::Function => "function",
        })
    }
}

// @Task replace this HashMap business with a match and interpret at call-site!
pub fn functions() -> HashMap<Function, FunctionValue> {
    use Function::*;

    let mut intrinsics = HashMap::default();

    intrinsics.insert(Add, pure!(|x: Nat, y: Nat| x + y));
    intrinsics.insert(Subtract, pure!(|x: Nat, y: Nat| x.checked_sub(&y)));
    intrinsics.insert(PanickingSubtract, pure!(|x: Nat, y: Nat| x - y));
    intrinsics.insert(Multiply, pure!(|x: Nat, y: Nat| x * y));
    intrinsics.insert(Divide, pure!(|x: Nat, y: Nat| x.checked_div(&y)));
    intrinsics.insert(Equal, pure!(|x: Nat, y: Nat| x == y));
    intrinsics.insert(Less, pure!(|x: Nat, y: Nat| x < y));
    intrinsics.insert(LessEqual, pure!(|x: Nat, y: Nat| x <= y));
    intrinsics.insert(Greater, pure!(|x: Nat, y: Nat| x > y));
    intrinsics.insert(GreaterEqual, pure!(|x: Nat, y: Nat| x >= y));
    intrinsics.insert(Display, pure!(|x: Nat| x.to_string()));
    intrinsics.insert(Concat, pure!(|a: Text, b: Text| a + &b));
    // @Temporary until we can target specific modules
    intrinsics.insert(AddNat32, pure!(|a: Nat32, b: Nat32| a + b));
    // @Temporary
    intrinsics.insert(
        Print,
        pure!(|message: Text| Value::IO {
            index: 0,
            arguments: vec![Value::Text(message)],
        }),
    );
    intrinsics.insert(
        Panic,
        pure!(|_type: Opaque, message: Text| Value::Panic { message }),
    );

    intrinsics
}

macro pure(|$( $var:ident: $variant:ident ),*| $body:expr ) {
    #[allow(unreachable_code)]
    FunctionValue {
        arity: count!($( $var )*),
        function: |arguments| {
            let mut arguments = arguments.into_iter();

            $(
                let $var = match arguments.next() {
                    Some(Value::$variant(value)) => value,
                    _ => unreachable!(),
                };
            )+

            $body.into()
        }
    }
}

macro count {
    () => { 0 },
    ($var:ident $( $rest:tt )*) => { 1 + count!($( $rest )*) },
}
