use super::{lex_string, Outcome};
use token::{BareToken, Word};
use utilities::obtain;

pub trait WordExt: Sized {
    fn parse(name: String) -> Result<Self, ()>;
}

impl WordExt for Word {
    // @Bug this allows words like `   foo-bar ` (leading & trailing ws)
    // @Note and not long before it used to allow `hey;;;wkwkwkwwkw`!
    // @Task just write a custom lexer for this!
    fn parse(name: String) -> Result<Self, ()> {
        let Outcome { tokens, errors } = lex_string(name);

        if !errors.is_empty() {
            return Err(());
        }

        let mut tokens = tokens.into_iter().map(|token| token.bare);

        obtain!(
            (tokens.next().ok_or(())?, tokens.next().ok_or(())?),
            (BareToken::Word(word), BareToken::EndOfInput) => word
        )
        .map(Self::new_unchecked)
        .ok_or(())
    }
}
