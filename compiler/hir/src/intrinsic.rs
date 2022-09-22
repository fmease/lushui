use crate::interfaceable::{self, Value};
use num_traits::{CheckedDiv, CheckedSub};
use std::{fmt, str::FromStr};

/// An intrinsic type.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Type {
    Type,
    Numeric(NumericType),
    Text,
    IO,
}

impl FromStr for Type {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        use NumericType::*;

        Ok(match input {
            "Type" => Self::Type,
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
            Self::Type => write!(f, "Type"),
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
// @Task derive Str, FromStr it again
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Function {
    NatAdd,
    NatSubtract,
    NatUncheckedSubtract,
    NatMultiply,
    NatDivide,
    NatEqual,
    NatLess,
    NatLessEqual,
    NatGreater,
    NatGreaterEqual,
    NatDisplay,
    TextConcat,
    Nat32Add,
    // @Temporary
    Nat32Successor,
    IoPrint,
}

// @Task find a better system than this arity/eval-split
impl Function {
    pub const fn name(self) -> &'static str {
        match self {
            Self::NatAdd => "nat.add",
            Self::NatSubtract => "nat.subtract",
            Self::NatUncheckedSubtract => "nat.unchecked-subtract",
            Self::NatMultiply => "nat.multiply",
            Self::NatDivide => "nat.divide",
            Self::NatEqual => "nat.equal",
            Self::NatLess => "nat.less",
            Self::NatLessEqual => "nat.less-equal",
            Self::NatGreater => "nat.greater",
            Self::NatGreaterEqual => "nat.greater-equal",
            Self::NatDisplay => "nat.display",
            Self::TextConcat => "text.concat",
            Self::Nat32Add => "nat32.add",
            Self::Nat32Successor => "nat32.successor",
            Self::IoPrint => "io.print",
        }
    }

    pub const fn arity(self) -> usize {
        match self {
            Self::Nat32Successor | Self::NatDisplay | Self::IoPrint => 1,
            Self::NatAdd
            | Self::NatSubtract
            | Self::NatUncheckedSubtract
            | Self::NatMultiply
            | Self::NatDivide
            | Self::NatEqual
            | Self::NatLess
            | Self::NatLessEqual
            | Self::NatGreater
            | Self::NatGreaterEqual
            | Self::TextConcat
            | Self::Nat32Add => 2,
        }
    }

    pub fn evaluate(self, arguments: Vec<interfaceable::Value>) -> interfaceable::Value {
        use interfaceable::Value::*;

        let mut arguments = arguments.into_iter();
        match self {
            Self::NatAdd => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                (x + y).into()
            }
            Self::NatSubtract => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                x.checked_sub(&y).into()
            }
            Self::NatUncheckedSubtract => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                (x - y).into()
            }
            Self::NatMultiply => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                (x * y).into()
            }
            Self::NatDivide => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                x.checked_div(&y).into()
            }
            Self::NatEqual => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                (x == y).into()
            }
            Self::NatLess => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                (x < y).into()
            }
            Self::NatLessEqual => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                (x <= y).into()
            }
            Self::NatGreater => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                (x > y).into()
            }
            Self::NatGreaterEqual => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                (x >= y).into()
            }
            Self::NatDisplay => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                x.to_string().into()
            }
            Self::TextConcat => {
                let Some(Text(x)) = arguments.next() else { unreachable!() };
                let Some(Text(y)) = arguments.next() else { unreachable!() };
                (x + &y).into()
            }
            Self::Nat32Add => {
                let Some(Nat32(x)) = arguments.next() else { unreachable!() };
                let Some(Nat32(y)) = arguments.next() else { unreachable!() };
                (x + y).into()
            }
            Self::Nat32Successor => {
                let Some(Nat32(x)) = arguments.next() else { unreachable!() };
                (x + 1).into()
            }
            Self::IoPrint => {
                let Some(Text(message)) = arguments.next() else { unreachable!() };
                Value::IO {
                    index: 0,
                    arguments: vec![message.into()],
                }
            }
        }
    }
}

impl FromStr for Function {
    type Err = ();

    // @Task get rid of legacy names (e.g. via derivation) once
    //       you implement properly namespaced intrinsics (see #138)
    fn from_str(source: &str) -> Result<Self, Self::Err> {
        Ok(match source {
            "add" => Self::NatAdd,
            "subtract" => Self::NatSubtract,
            "unchecked-subtract" => Self::NatUncheckedSubtract,
            "multiply" => Self::NatMultiply,
            "divide" => Self::NatDivide,
            "equal" => Self::NatEqual,
            "less" => Self::NatLess,
            "less-equal" => Self::NatLessEqual,
            "greater" => Self::NatGreater,
            "greater-equal" => Self::NatGreaterEqual,
            "display" => Self::NatDisplay,
            "concat" => Self::TextConcat,
            "add-nat32" => Self::Nat32Add,
            "successor-nat32" => Self::Nat32Successor,
            "print" => Self::IoPrint,
            _ => return Err(()),
        })
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::NatAdd => "add",
            Self::NatSubtract => "subtract",
            Self::NatUncheckedSubtract => "unchecked-subtract",
            Self::NatMultiply => "multiply",
            Self::NatDivide => "divide",
            Self::NatEqual => "equal",
            Self::NatLess => "less",
            Self::NatLessEqual => "less-equal",
            Self::NatGreater => "greater",
            Self::NatGreaterEqual => "greater-equal",
            Self::NatDisplay => "display",
            Self::TextConcat => "concat",
            Self::Nat32Add => "add-nat32",
            Self::Nat32Successor => "successor-nat32",
            Self::IoPrint => "print",
        })
    }
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
