//! Errors occurring during lexing.

use std::fmt;

use crate::error::Span;

pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

pub enum ErrorKind {
    IllegalCharacter(char),
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
            Self::InvalidIndentation(indentation) => write!(
                f,
                "invalid indentation consisting of {} spaces",
                indentation
            ),
        }
    }
}
