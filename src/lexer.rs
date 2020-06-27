//! The lexer.
//!
//! It parses indentation and dedentation intwo two pseudo tokens:
//! [TokenKind::Indentation] and [TokenKind::Dedentation] respectively.
//!
//! Natural number literals are directly converted into [num_bigint::BigUint]
//! and identifiers are interned.

#[cfg(test)]
mod test;
mod token;

use crate::{
    diagnostic::*,
    span::{LocalByteIndex, LocalSpan, SourceFile, Span},
    support::ManyErrExt,
    Atom, Nat, INDENTATION_IN_SPACES,
};
use std::{
    iter::Peekable,
    str::{CharIndices, FromStr},
};
pub use token::{
    is_punctuation, Token,
    TokenKind::{self, *},
};

/// Utility to parse identifiers from a string slice.
///
/// Used for non-lushui code like crate names.
// @Note this is ugly
pub fn parse_identifier(source: String) -> Option<Atom> {
    let mut tokens = lex(source).ok()?;
    let mut tokens = tokens.drain(..);
    match [tokens.next(), tokens.next()] {
        [Some(Token {
            kind: Identifier,
            data: token::TokenData::Identifier(atom),
            ..
        }), Some(Token {
            kind: EndOfInput, ..
        })] => Some(atom),
        _ => None,
    }
}

pub fn lex(source: String) -> Result<Vec<Token>, Diagnostics> {
    let path = String::new();
    let file = SourceFile::new(path, source, crate::span::START_OF_FIRST_SOURCE_FILE)
        .ok()
        .unwrap();
    Lexer::new(&file).lex()
}

pub struct Lexer<'a> {
    source: &'a SourceFile,
    characters: Peekable<CharIndices<'a>>,
    tokens: Vec<Token>,
    span: LocalSpan,
    indentation_in_spaces: usize,
    round_brackets: Vec<Span>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a SourceFile) -> Self {
        Self {
            source,
            characters: source.content().char_indices().peekable(),
            tokens: Vec::new(),
            span: LocalSpan::zero(),
            indentation_in_spaces: 0,
            round_brackets: Vec::new(),
        }
    }

    /// Lex source code into an array of tokens
    pub fn lex(mut self) -> Result<Vec<Token>, Diagnostics> {
        while let Some(character) = self.peek() {
            self.span = LocalSpan::from(self.index().unwrap());
            match character {
                // @Bug if it is SOI, don't lex_whitespace but lex_indentation
                // (SOI should act as a line break)
                ' ' => self.lex_whitespace(),
                ';' => self.lex_comment(),
                character if character.is_ascii_alphabetic() => self.lex_identifier().many_err()?,
                '\n' => self.lex_indentation().many_err()?,
                character if token::is_punctuation(character) => self.lex_punctuation(),
                character if character.is_ascii_digit() => self.lex_nat_literal(),
                '"' => self.lex_text_literal().many_err()?,
                '(' => self.lex_opening_round_bracket(),
                ')' => self.lex_closing_round_bracket().many_err()?,
                ',' => self.lex_comma(),
                '_' => self.lex_underscore(),
                character => {
                    self.take();
                    return Err(Diagnostic::new(
                        Level::Fatal,
                        Code::E000,
                        format!(
                            "illegal character U+{:04X} `{}`",
                            character as u32, character
                        ),
                    )
                    .with_span(&self.span()))
                    .many_err();
                }
            }
        }

        if !self.round_brackets.is_empty() {
            return Err(self
                .round_brackets
                .into_iter()
                .map(|bracket| {
                    Diagnostic::new(Level::Fatal, Code::E001, "unbalanced brackets")
                        .with_labeled_span(&bracket, "has no matching closing bracket")
                })
                .collect());
        }

        // @Bug panics if last byte is not a valid codepoint, right?
        let last = LocalByteIndex::from_usize(self.source.content().len() - 1);

        self.extend_with_dedentations(last, self.indentation_in_spaces);
        self.span = LocalSpan::from(last);
        self.add(EndOfInput);

        Ok(self.tokens)
    }

    fn lex_whitespace(&mut self) {
        self.advance();
        while let Some(character) = self.peek() {
            if character != ' ' {
                break;
            }
            self.advance();
        }
    }

    fn lex_comment(&mut self) {
        self.advance();

        let mut documentation = true;

        if let Some(character) = self.peek() {
            if character == ';' {
                documentation = false;
            } else {
                self.take();
            }
            self.advance();
        }

        while let Some(character) = self.peek() {
            if character == '\n' {
                break;
            }

            if documentation {
                self.take();
            }

            self.advance();
        }

        if documentation {
            self.add(DocumentationComment)
        }
    }

    fn lex_identifier(&mut self) -> Result<()> {
        self.lex_identifier_segment();
        while self.peek() == Some('-') {
            let dash = self.index().unwrap();
            self.take();
            self.advance();
            let previous = self.span;
            self.lex_identifier_segment();
            if self.span == previous {
                return Err(Diagnostic::new(
                    Level::Fatal,
                    Code::E002,
                    "trailing dash on identifier",
                )
                .with_span(&Span::from_local(self.source, dash.into())));
            }
        }

        match token::parse_keyword(&self.source[self.span]) {
            Some(keyword) => self.add(keyword),
            None => {
                let identifier = self.source[self.span].into();
                self.add_with(|span| Token::new_identifier(identifier, span))
            }
        };

        Ok(())
    }

    fn lex_identifier_segment(&mut self) {
        if let Some(character) = self.peek() {
            if character.is_ascii_alphabetic() {
                self.take();
                self.advance();
                self.take_while(|character| character.is_ascii_alphanumeric());
                self.take_while(|character| character == token::PRIME);
            }
        }
    }

    fn lex_indentation(&mut self) -> Result<()> {
        use std::cmp::Ordering::*;

        self.advance();
        self.add(LineBreak);

        self.span = match self.index() {
            Some(index) => LocalSpan::from(index),
            None => return Ok(()),
        };
        let mut spaces = 0;
        self.take_while_with(|character| character == ' ', || spaces += 1);

        let change = spaces.cmp(&self.indentation_in_spaces);

        let absolute_difference = match change {
            Greater => spaces - self.indentation_in_spaces,
            Less => self.indentation_in_spaces - spaces,
            Equal => return Ok(()),
        };

        self.span = LocalSpan::new(self.span.end + 1 - absolute_difference, self.span.end);

        if absolute_difference % INDENTATION_IN_SPACES != 0
            || change == Greater && absolute_difference > INDENTATION_IN_SPACES
        {
            return Err(Diagnostic::new(
                Level::Fatal,
                Code::E003,
                format!(
                    "invalid indentation consisting of {} spaces",
                    absolute_difference
                ),
            )
            .with_span(&self.span()));
        }

        match change {
            Greater => self.add(Indentation),
            // @Note hacky
            Less => self.extend_with_dedentations(self.span.end - 1, absolute_difference),
            Equal => unreachable!(),
        }

        self.indentation_in_spaces = spaces;

        Ok(())
    }

    fn lex_punctuation(&mut self) {
        self.advance();
        self.take_while(token::is_punctuation);

        // @Beacon @Note strange API, return a bool or Result<(), ()>
        match token::parse_reserved_punctuation(&self.source[self.span]) {
            Some(punctuation) => self.add(punctuation),
            None => {
                let identifier = self.source[self.span].into();
                self.add_with(|span| Token::new_punctuation(identifier, span))
            }
        }
    }

    // @Task numeric separator `'`
    fn lex_nat_literal(&mut self) {
        self.advance();
        self.take_while(|character| character.is_ascii_digit());

        let nat = Nat::from_str(&self.source[self.span]).unwrap();
        self.add_with(|span| Token::new_nat_literal(nat, span));
    }

    // @Task escape sequences
    fn lex_text_literal(&mut self) -> Result<()> {
        let mut terminated = false;
        self.advance();

        while let Some(character) = self.peek() {
            self.take();
            self.advance();

            if character == '"' {
                terminated = true;
                break;
            }
        }

        if !terminated {
            return Err(
                Diagnostic::new(Level::Fatal, Code::E004, "unterminated text literal")
                    .with_span(&self.span()),
            );
        }

        // @Note once we implement escaping, this won't cut it and we need to build our own string
        let text = self.source[LocalSpan::new(self.span.start + 1, self.span.end - 1)].to_owned();
        self.add_with(|span| Token::new_text_literal(text, span));

        Ok(())
    }

    fn lex_opening_round_bracket(&mut self) {
        self.add(OpeningRoundBracket);
        self.round_brackets.push(self.span());
        self.advance();
    }

    fn lex_closing_round_bracket(&mut self) -> Result<()> {
        self.add(ClosingRoundBracket);
        if self.round_brackets.is_empty() {
            return Err(
                Diagnostic::new(Level::Fatal, Code::E001, "unbalanced brackets")
                    .with_labeled_span(&self.span(), "has no matching opening bracket"),
            );
        }
        self.round_brackets.pop();
        self.advance();

        Ok(())
    }

    fn lex_comma(&mut self) {
        self.add(Comma);
        self.advance();
    }

    fn lex_underscore(&mut self) {
        self.add(Underscore);
        self.advance();
    }

    fn span(&self) -> Span {
        Span::from_local(self.source, self.span)
    }

    fn advance(&mut self) {
        self.characters.next();
    }

    fn take(&mut self) {
        let &(index, character) = self.characters.peek().unwrap();
        self.span.end = LocalByteIndex::from_usize(index) + character;
    }

    fn peek(&mut self) -> Option<char> {
        self.characters.peek().map(|&(_, character)| character)
    }

    fn index(&mut self) -> Option<LocalByteIndex> {
        self.characters
            .peek()
            .map(|&(index, _)| LocalByteIndex::from_usize(index))
    }

    fn take_while(&mut self, predicate: fn(char) -> bool) {
        self.take_while_with(predicate, || ())
    }

    fn take_while_with(&mut self, predicate: fn(char) -> bool, mut action: impl FnMut()) {
        while let Some(character) = self.peek() {
            if !predicate(character) {
                break;
            }
            self.take();
            self.advance();
            action();
        }
    }

    fn add(&mut self, token: TokenKind) {
        self.tokens.push(Token::new(token, self.span()));
    }

    fn add_with(&mut self, constructor: impl FnOnce(Span) -> Token) {
        self.tokens.push(constructor(self.span()))
    }

    fn extend_with_dedentations(&mut self, start: LocalByteIndex, amount_of_spaces: usize) {
        if amount_of_spaces == 0 {
            return;
        }

        // @Task use better span (it should span 4 spaces if possible) @Note you need to go backwards
        self.span = LocalSpan::from(start);
        let span = self.span();

        let extension = std::iter::repeat(Token::new(TokenKind::Dedentation, span))
            .take(amount_of_spaces / INDENTATION_IN_SPACES);

        self.tokens.extend(extension);
    }
}
