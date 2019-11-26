use std::convert::TryFrom;
use std::fmt;

use crate::error::Span;
use crate::lexer::{self, Atom};

// @Note is going to become more complex in the future (or is going to be replaced)
// when we implement to dotted identifiers, blanks (`'_`) and symbols
// @Note and possibly also generated identifiers (see effluvium::Variable) except if
// we use a variation on debruijn-indeces
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub atom: Atom,
    pub span: Span,
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.atom)
    }
}

// @Note only ever used in consume_identifier @Question over-engineered?
impl TryFrom<lexer::SourceToken> for Identifier {
    type Error = ();

    fn try_from(token: lexer::SourceToken) -> Result<Self, Self::Error> {
        match token.inner {
            lexer::Token::Identifier(atom) => Ok(Self {
                atom,
                span: token.span,
            }),
            _ => Err(()),
        }
    }
}
