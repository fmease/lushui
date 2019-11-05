use crate::error::Span;

use std::fmt;

#[derive(Debug)] // @Temporary
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug)] // @Temporary
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum ErrorKind {
    IllegalCharacter(char),
    UnknownKeyword(String),
    InvalidIndentation(usize),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::IllegalCharacter(character) => write!(
                f,
                "illegal character U+{:04X} `{}`",
                *character as u32, character,
            ),
            Self::UnknownKeyword(source) => write!(f, "unknown keyword `{}`", source),
            Self::InvalidIndentation(indentation) => write!(
                f,
                "invalid indentation consisting of {} spaces",
                indentation
            ),
        }
    }
}
