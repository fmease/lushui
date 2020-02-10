//! The context of the parser.
//!
//! Stores a reference to the source code of one file and a cursor.

use std::convert::TryInto;

use super::error::{Error, ErrorKind, Result};
use super::Identifier;
use crate::lexer;

// @Task use SourceFiles
pub struct Parser<'i> {
    tokens: &'i [lexer::SourceToken],
    index: usize,
}

// @Note this API is **horrible**!!
// @Task move back into main module and make all the parse_*-functions methods
// @Task make use of the new TokenKind::EndOfLine to simplify the logic tremendously
impl<'input> Parser<'input> {
    /// Construct a new context with the pointer at the beginning.
    pub fn new(tokens: &'input [lexer::SourceToken]) -> Self {
        Self { tokens, index: 0 }
    }

    /// Parse the source in a sandboxed context.
    ///
    /// Used for arbitrary look-ahead. Restores the old cursor on failure.
    pub fn reflect<T>(&mut self, parser: fn(&mut Self) -> Result<T>) -> Result<T> {
        let saved_index = self.index;
        let result = parser(self);

        if result.is_err() {
            self.index = saved_index;
        }

        result
    }

    pub fn expect(&self, expected_token_kind: lexer::TokenKind) -> Result<lexer::SourceToken> {
        let token = self.current_token()?;
        let actual_token_kind = token.kind();
        if actual_token_kind == expected_token_kind {
            Ok(token)
        } else {
            Err(Error {
                span: token.span,
                kind: ErrorKind::UnexpectedToken {
                    expected: expected_token_kind,
                    actual: actual_token_kind,
                },
            })
        }
    }

    pub fn at_the_end_of_input(&self) -> bool {
        self.index >= self.tokens.len()
    }

    // @Question don't just accept tokens but anything of a certain trait which tokens implement,
    // optional tokens, either this or that token (with special token EOI), parser::Identifier,...
    pub fn consume(&mut self, token_kind: lexer::TokenKind) -> Result<lexer::SourceToken> {
        let token = self.expect(token_kind)?;
        self.advance();
        Ok(token)
    }

    // @Note ugly: special-casing identifiers... need to generalize? via trait?
    pub fn consume_identifier(&mut self) -> Result<Identifier> {
        self.consume(lexer::TokenKind::Identifier)
            .map(|token| token.try_into().unwrap())
    }

    pub fn advance(&mut self) {
        self.index += 1;
    }

    pub fn current_token(&self) -> Result<lexer::SourceToken> {
        self.tokens.get(self.index).cloned().ok_or_else(|| Error {
            kind: ErrorKind::UnexpectedEndOfInput,
            // @Bug should point to the "token after" (in the error.rs display)
            // @Bug this might panic bc of underflow
            span: self.tokens.get(self.index - 1).unwrap().span,
        })
    }
}

/// The modern API.
impl Parser<'_> {
    pub fn current(&self, kind: lexer::TokenKind) -> bool {
        self.tokens
            .get(self.index)
            .map(|token| token.kind() == kind)
            .unwrap_or(false)
    }

    pub fn succeeding(&self, kind: lexer::TokenKind) -> bool {
        self.tokens
            .get(self.index + 1)
            .map(|token| token.kind() == kind)
            .unwrap_or(false)
    }

    pub fn consumed(&mut self, kind: lexer::TokenKind) -> bool {
        if self.current(kind) {
            self.advance();
            true
        } else {
            false
        }
    }
}
