// @Task restructure this, can't we just use Strings as errors and maybe
// nullary variants for ErrorKind if at all??

use crate::error::Span;
use crate::lexer::TokenKind;

use std::fmt;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)] // @Temporary
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

// @Task improve, missing information
#[derive(Debug)] // @Temporary
pub enum ErrorKind {
    UnexpectedEndOfInput,
    UnexpectedToken(TokenKind),
    ExpectedDeclaration(TokenKind),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedEndOfInput => f.write_str("unexpected end of input"),
            // @Temporary
            Self::UnexpectedToken(token_kind) => write!(f, "unexpected {}", token_kind),
            Self::ExpectedDeclaration(token_kind) => {
                write!(f, "expected start of declaration, found {}", token_kind)
            }
        }
    }
}
