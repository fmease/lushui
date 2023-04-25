use crate::{lex_string, token::BareToken, Outcome};
use std::fmt;
use utility::{obtain, Atom};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Word(Atom);

impl Word {
    pub fn new_unchecked(name: Atom) -> Self {
        Self(name)
    }

    // @Bug this allows words like `   foo-bar ` (leading & trailing ws)
    // @Note and not long before it used to allow `hey;;;wkwkwkwwkw`!
    // @Task just write a custom lexer for this!
    pub fn parse(name: String) -> Result<Self, ()> {
        let Outcome { tokens, errors } = lex_string(name);

        if !errors.is_empty() {
            return Err(());
        }

        let mut tokens = tokens.into_iter().map(|token| token.bare);

        obtain!(
            (tokens.next().ok_or(())?, tokens.next().ok_or(())?),
            (BareToken::Word(word), BareToken::EndOfInput) => word
        )
        .map(Self)
        .ok_or(())
    }

    pub fn into_inner(self) -> Atom {
        self.0
    }

    pub fn to_str(self) -> &'static str {
        self.0.to_str()
    }
}

impl fmt::Debug for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}w", self.to_str())
    }
}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}
