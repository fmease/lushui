// @Task restructure this, can't we just use Strings as errors and maybe
// nullary variants for ErrorKind if at all??

use crate::error::Span;
use crate::lexer::TokenKind;

use std::fmt;

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

// @Task improve, missing information
pub enum ErrorKind {
    UnexpectedEndOfInput,
    UnexpectedToken { expected: TokenKind, actual: TokenKind },
    ExpectedDeclaration { actual: TokenKind },
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedEndOfInput => f.write_str("unexpected end of input"),
            Self::UnexpectedToken { expected, actual } => write!(f, "expected {}, found {}", expected, actual),
            Self::ExpectedDeclaration { actual } => {
                write!(f, "expected start of declaration, found {}", actual)
            }
        }
    }
}
