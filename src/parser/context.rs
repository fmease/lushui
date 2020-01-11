use std::convert::TryInto;

use crate::lexer;
use super::error::{Error, ErrorKind, Result};
use super::Identifier;

#[derive(Clone)]
pub struct Context<'i> {
    tokens: &'i [lexer::SourceToken],
    index: usize,
}

impl<'i> Context<'i> {
    pub fn new(tokens: &'i [lexer::SourceToken]) -> Self {
        Self { tokens, index: 0 }
    }

    pub(super) fn reflect<T>(&mut self, parser: fn(&mut Context<'i>) -> Result<T>) -> Result<T> {
        let mut context = self.clone();
        parser(&mut context).map(|value| {
            *self = context;
            value
        })
    }

    // @Note unused exept in consume
    pub(super) fn expect(&self, expected_token_kind: lexer::TokenKind) -> Result<lexer::SourceToken> {
        let token = self.current_token()?;
        let actual_token_kind = token.kind();
        if actual_token_kind == expected_token_kind {
            Ok(token)
        } else {
            Err(Error {
                span: token.span,
                kind: ErrorKind::UnexpectedToken(actual_token_kind),
            })
        }
    }

    pub(super) fn at_the_end_of_input(&self) -> bool {
        self.index >= self.tokens.len()
    }

    // @Question don't just accept tokens but anything of a certain trait which tokens implement,
    // optional tokens, either this or that token (with special token EOI), parser::Identifier,...
    pub(super) fn consume(&mut self, token_kind: lexer::TokenKind) -> Result<lexer::SourceToken> {
        let token = self.expect(token_kind)?;
        self.accept();
        Ok(token)
    }

    // @Note ugly: special-casing identifiers... need to generalize? via trait?
    pub(super) fn consume_identifier(&mut self) -> Result<Identifier> {
        self.consume(lexer::TokenKind::Identifier)
            .map(|token| token.try_into().unwrap())
    }

    pub(super) fn accept(&mut self) {
        self.index += 1;
    }

    pub(super) fn current_token(&self) -> Result<lexer::SourceToken> {
        self.tokens.get(self.index).cloned().ok_or_else(|| Error {
            kind: ErrorKind::UnexpectedEndOfInput,
            // @Bug should point to the "token after" (in the error.rs display)
            // @Bug this might panic bc of underflow
            span: self.tokens.get(self.index - 1).unwrap().span,
        })
    }
}
