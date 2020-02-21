//! Errors happening during type-checking and interpreting.
//!
//! ## Issues:
//!
//! * still missing span information
//! * enum best solution? rustc's structured diagnostics system with text messages
//!   seems way better

use std::fmt;

use crate::hir::{Expression, Identifier};

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub enum Error {
    UndefinedBinding(Identifier),
    FunctionExpected {
        actual: Expression,
        argument: Expression,
    },
    ExpressionsNotEqual {
        expected: Expression,
        actual: Expression,
    },
    // @Task replace with InvalidInstance and also reference said instance
    // @Update use multispan/subdiagnostics (once we hook everything up correctly)
    InvalidConstructor {
        name: Identifier,
    },
    AlreadyDefined(Identifier),
    NotAllConstructorsCovered,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UndefinedBinding(binding) => write!(f, "undefined binding `{}`", binding),
            Error::FunctionExpected { actual, argument } => {
                write!(f, "cannot apply `{}` to a `{}`", argument, actual)
            }
            Error::ExpressionsNotEqual { expected, actual } => {
                write!(f, "expected `{}` got `{}`", expected, actual)
            }
            Error::InvalidConstructor { name } => write!(f, "invalid constructor `{}`", name),
            Error::AlreadyDefined(binder) => write!(f, "`{}` is already defined", binder),
            Error::NotAllConstructorsCovered => write!(f, "not all constructors covered"),
        }
    }
}
