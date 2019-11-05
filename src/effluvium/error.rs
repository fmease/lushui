use std::fmt;

use crate::hir::{Identifier, Expression};

pub type Result<T> = std::result::Result<T, Error>;

pub enum Error {
    UndefinedBinding(Identifier),
    FunctionExpected(Expression),
    ExpressionsNotEqual(Expression, Expression),
    AlreadyDefined,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UndefinedBinding(binding) => write!(f, "undefined binding `{}`", binding),
            Error::FunctionExpected(actual) => write!(f, "expected function, got `{}`", actual),
            Error::ExpressionsNotEqual(expected, actual) => {
                write!(f, "expected `{}` got `{}`", expected, actual)
            }
            Error::AlreadyDefined => write!(f, "already defined"),
        }
    }
}
