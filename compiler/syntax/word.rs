use super::{ast::Identifier, lexer, token::TokenKind};
use crate::{
    error::Outcome,
    span::{Spanned, Spanning},
    utility::{obtain, Atom},
};
use std::fmt;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Word(Atom);

impl Word {
    pub(crate) fn new_unchecked(name: Atom) -> Self {
        Self(name)
    }

    #[allow(clippy::result_unit_err)]
    pub fn parse(name: String) -> Result<Self, ()> {
        let Outcome!(tokens, health) = lexer::lex_string(name)?;

        if health.is_tainted() {
            return Err(());
        }

        let mut tokens = tokens.into_iter().map(|token| token.value);

        obtain!(
            (tokens.next().ok_or(())?, tokens.next().ok_or(())?),
            (TokenKind::Word(atom), TokenKind::EndOfInput) => atom
        )
        .map(Self)
        .ok_or(())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Debug for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}w", self.as_str())
    }
}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl From<Spanned<Word>> for Identifier {
    fn from(name: Spanned<Word>) -> Self {
        Self::new_unchecked(name.value.0, name.span)
    }
}

impl TryFrom<Identifier> for Spanned<Word> {
    type Error = ();

    fn try_from(identifier: Identifier) -> Result<Self, Self::Error> {
        identifier
            .is_word()
            .then(|| Self::new(identifier.span(), Word(identifier.into_atom())))
            .ok_or(())
    }
}
