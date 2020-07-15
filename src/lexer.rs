//! The lexer.
//!
//! It parses indentation and dedentation intwo two pseudo tokens:
//! [TokenKind::Indentation] and [TokenKind::Dedentation] respectively.
//!
//! Natural number literals are directly converted into [num_bigint::BigUint]
//! and identifiers are interned.

// #[cfg(test)]
#[cfg(FALSE)] // @Temporary
mod test;
mod token;

use crate::{
    diagnostic::{Code, Diagnostic, Result, Results},
    span::{LocalByteIndex, LocalSpan, SourceFile, Span},
    support::ManyErrExt,
    Atom, Int, INDENTATION_IN_SPACES,
};
use std::{
    iter::Peekable,
    str::{CharIndices, FromStr},
};
pub use token::{
    is_punctuation, Number, Token,
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

pub fn lex(source: String) -> Results<Vec<Token>> {
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
    pub fn lex(mut self) -> Results<Vec<Token>> {
        while let Some(character) = self.peek() {
            self.span = LocalSpan::from(self.index().unwrap());
            match character {
                // @Bug if it is SOI, don't lex_whitespace but lex_indentation
                // (SOI should act as a line break)
                ' ' => self.lex_whitespace(),
                ';' => self.lex_comment(),
                character if character.is_ascii_alphabetic() => self.lex_identifier().many_err()?,
                '\n' => self.lex_indentation().many_err()?,
                '-' => {
                    self.advance();
                    if self
                        .peek()
                        .map(|character| character.is_ascii_digit())
                        .unwrap_or(false)
                    {
                        self.lex_number_literal(-1).many_err()?;
                    } else {
                        self.lex_punctuation();
                    }
                }
                character if character.is_ascii_digit() => {
                    self.advance();
                    self.lex_number_literal(1).many_err()?
                }
                character if token::is_punctuation(character) => self.lex_punctuation(),
                '"' => self.lex_text_literal().many_err()?,
                '(' => self.lex_opening_round_bracket(),
                ')' => self.lex_closing_round_bracket().many_err()?,
                ',' => self.lex_comma(),
                '_' => self.lex_underscore(),
                character => {
                    self.take();
                    return Err(Diagnostic::error()
                        .with_code(Code::E000)
                        .with_message(format!(
                            "illegal character U+{:04X} `{}`",
                            character as u32, character
                        ))
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
                    Diagnostic::error()
                        .with_code(Code::E001)
                        .with_message("unbalanced brackets")
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
                return Err(Diagnostic::error()
                    .with_code(Code::E002)
                    .with_message("trailing dash on identifier")
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
            return Err(Diagnostic::error()
                .with_code(Code::E003)
                .with_message(format!(
                    "invalid indentation consisting of {} spaces",
                    absolute_difference
                ))
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
        self.take_while(token::is_punctuation);

        match token::parse_reserved_punctuation(&self.source[self.span]) {
            Some(punctuation) => self.add(punctuation),
            None => {
                let identifier = self.source[self.span].into();
                self.add_with(|span| Token::new_punctuation(identifier, span))
            }
        }
    }

    // @Note the hash suffx syntax is only temporary until we decide to
    // implement generic numbers
    // @Task numeric separator `'`
    // @Task negative numbers, handle out range error
    fn lex_number_literal(&mut self, sign: i32) -> Result<()> {
        self.take_while(|character| character.is_ascii_digit());

        let suffix = if let Some(character) = self.peek() {
            let mut suffix = String::new();

            if character == '#' {
                self.advance();

                while let Some(character) = self.peek() {
                    if !character.is_ascii_alphanumeric() {
                        break;
                    }

                    suffix.push(character);
                    self.advance();
                }
                Some(suffix)
            } else {
                None
            }
        } else {
            None
        };

        use token::{NumberLiteralSuffix::*, TokenData};

        let suffix = match suffix {
            Some(suffix) => suffix.parse().map_err(|_| {
                // @Task code
                Diagnostic::error()
                    .with_message(format!("invalid number literal suffix `{}`", suffix))
                    .with_span(&self.span())
            })?,
            None => N,
        };

        if sign < 0 && !suffix.signed() {
            // @Task code, @Beacon @Task message
            return Err(Diagnostic::error()
                .with_message("sign does not match type")
                .with_span(&self.span()));
        }

        let data = &self.source[self.span];

        // @Task move into token module
        // @Task error messages
        let data = match suffix {
            N => TokenData::NatLiteral(data.parse().unwrap()),
            N32 => TokenData::Nat32Literal(data.parse().map_err(|_| {
                Diagnostic::error()
                    .with_message("value does not fit type")
                    .with_span(&self.span())
            })?),
            N64 => TokenData::Nat64Literal(data.parse().map_err(|_| {
                Diagnostic::error()
                    .with_message("value does not fit type")
                    .with_span(&self.span())
            })?),
            I => TokenData::IntLiteral(sign * Int::from_str(data).unwrap()),
            I32 => TokenData::Int32Literal(
                sign * i32::from_str(data).map_err(|_| {
                    Diagnostic::error()
                        .with_message("value does not fit type")
                        .with_span(&self.span())
                })?,
            ),
            I64 => TokenData::Int64Literal(
                sign as i64
                    * i64::from_str(data).map_err(|_| {
                        Diagnostic::error()
                            .with_message("value does not fit type")
                            .with_span(&self.span())
                    })?,
            ),
        };

        self.add_with(|span| Token::with_data(TokenKind::NumberLiteral, data, span));

        Ok(())
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
            return Err(Diagnostic::error()
                .with_code(Code::E004)
                .with_message("unterminated text literal")
                .with_span(&self.span()));
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
            return Err(Diagnostic::error()
                .with_code(Code::E001)
                .with_message("unbalanced brackets")
                .with_labeled_span(&self.span(), "has no matching opening bracket"));
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
