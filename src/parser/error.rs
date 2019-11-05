use crate::error::Span;
use crate::lexer::SourceToken;

use std::fmt;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)] // @Temporary
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug)] // @Temporary
pub enum ErrorKind {
    UnexpectedEndOfInput,
    UnexpectedToken(SourceToken),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedEndOfInput => write!(f, "unexpected end of input"),
            // @Temporary
            Self::UnexpectedToken(token) => write!(f, "unexpected token {:?}", token.kind()),
        }
    }
}
