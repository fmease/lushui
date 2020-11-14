//! The lexer.
//!
//! It parses indentation and dedentation intwo two pseudo tokens:
//! [TokenKind::Indentation] and [TokenKind::Dedentation] respectively.
//!
//! ## Issues
//!
//! * most lexical errors are fatal right now. we shouldn't make the lexer fail
//!   on mismatching brackets, that should be the task of the parser (indeed also
//!   keeping the bracket stack!), as well as invalid tokens: we should just
//!   keep going having a new TokenKind, TokenKind::Invalid(..) (which is a cluster)
//!   as a result, the parser can skip them and what not
//!   (both things should not poison the lexer!)

#[cfg(test)]
// @Beacon @Task update errors
mod test;
pub mod token;

use crate::{
    diagnostic::{Code, Diagnostic, Diagnostics, Result, Results},
    span::{LocalByteIndex, LocalSpan, SourceFile, Span},
    support::ManyErrExt,
    Atom, INDENTATION_IN_SPACES,
};
use std::{iter::Peekable, str::CharIndices};
pub use token::{
    is_punctuation, Token,
    TokenKind::{self, *},
};

fn lex(source: String) -> Results<Vec<Token>> {
    Lexer::new(
        &SourceFile::fake(source.to_owned()),
        &mut Default::default(),
    )
    .lex()
}

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

/// The state of the lexer.
pub struct Lexer<'a> {
    source: &'a SourceFile,
    characters: Peekable<CharIndices<'a>>,
    tokens: Vec<Token>,
    span: LocalSpan,
    indentation_in_spaces: usize,
    round_brackets: Vec<Span>,
    warnings: &'a mut Diagnostics,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a SourceFile, warnings: &'a mut Diagnostics) -> Self {
        Self {
            source,
            warnings,
            characters: source.content().char_indices().peekable(),
            tokens: Vec::new(),
            span: LocalSpan::zero(),
            indentation_in_spaces: 0,
            round_brackets: Vec::new(),
        }
    }

    #[allow(dead_code)]
    fn warn(&mut self, warning: Diagnostic) {
        self.warnings.insert(warning);
    }

    // @Task move balanced bracket validation out of lexer to the parser and
    // also start supporting square and curly brachets for future use
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
                '-' => self.lex_punctuation_or_number_literal()?,
                character if character.is_ascii_digit() => {
                    self.advance();
                    self.lex_number_literal().many_err()?
                }
                character if token::is_punctuation(character) => self.lex_punctuation(),
                '"' => self.lex_text_literal().many_err()?,
                '(' => self.lex_opening_round_bracket(),
                ')' => self.lex_closing_round_bracket().many_err()?,
                '[' => {
                    self.add(OpeningSquareBracket);
                    self.advance();
                }
                ']' => {
                    self.add(ClosingSquareBracket);
                    self.advance();
                }
                '{' => {
                    self.add(OpeningCurlyBracket);
                    self.advance();
                }
                '}' => {
                    self.add(ClosingCurlyBracket);
                    self.advance();
                }
                ',' => self.lex_comma(),
                '_' => self.lex_underscore(),
                character => {
                    self.take();
                    self.advance();
                    self.add_with(|span| Token::new_illegal(character, span))
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

    // @Task merge consecutive documentation comments
    // @Task emit TokenKind::Comment if asked
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
                self.take();
                self.advance();
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

    // @Task make trailing dashes part of punctuation (this removes the error condition)
    // @Task make `_` a valid token for identifiers
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
                    .with_span(&Span::local(self.source, dash.into())));
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

        // squash consecutive line breaks into a single one
        self.advance();
        self.take_while(|character| character == '\n');
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
                .with_span(&self.span())
                .with_note(format!(
                    "indentation needs to be a multiple of {}",
                    INDENTATION_IN_SPACES
                )));
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

    fn lex_punctuation_or_number_literal(&mut self) -> Results<()> {
        self.advance();
        if self
            .peek()
            .map(|character| character.is_ascii_digit())
            .unwrap_or(false)
        {
            self.lex_number_literal().many_err()?;
        } else {
            self.lex_punctuation();
        }
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

    // @Task move validation logic out of the lexer
    fn lex_number_literal(&mut self) -> Result<()> {
        const NUMERIC_SEPERATOR: char = '\'';

        let mut number = self.source().to_owned();

        let mut trailing_prime = false;
        let mut consecutive_primes = false;

        while let Some(character) = self.peek() {
            if !(character.is_ascii_digit() || character == NUMERIC_SEPERATOR) {
                break;
            }
            self.take();
            self.advance();

            if character != NUMERIC_SEPERATOR {
                number.push(character);
            } else {
                if let Some(NUMERIC_SEPERATOR) = self.peek() {
                    consecutive_primes = true;
                }
                if self
                    .peek()
                    .filter(|&character| {
                        character.is_ascii_digit() || character == NUMERIC_SEPERATOR
                    })
                    .is_none()
                {
                    trailing_prime = true;
                }
            }
        }

        // @Task emit an invalid identifier token if an identifier immedially follows
        // a number literal (maybe)

        // @Task return an invalid token instead
        if consecutive_primes {
            return Err(Diagnostic::error()
                .with_code(Code::E005)
                .with_message("consecutive primes in number literal")
                .with_span(&self.span()));
        }

        // @Task return an invalid token instead

        if trailing_prime {
            return Err(Diagnostic::error()
                .with_code(Code::E005)
                .with_message("trailing prime in number literal")
                .with_span(&self.span()));
        }

        self.add_with(|span| Token::new_number_literal(number, span));

        Ok(())
    }

    // @Task escape sequences @Update do them in the parser (or at least mark them as invalid
    // and do the error reporting in the parser)
    // @Task parse suffixes
    fn lex_text_literal(&mut self) -> Result<()> {
        let mut is_terminated = false;
        self.advance();

        while let Some(character) = self.peek() {
            self.take();
            self.advance();

            if character == '"' {
                is_terminated = true;
                break;
            }
        }

        // @Note once we implement escaping, this won't cut it and we need to build our own string
        let text = self.source[LocalSpan::new(
            self.span.start + 1,
            if is_terminated {
                self.span.end - 1
            } else {
                self.span.end
            },
        )]
        .to_owned();
        self.add_with(|span| Token::new_text_literal(text, span, is_terminated));

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
        Span::local(self.source, self.span)
    }

    fn source(&self) -> &str {
        &self.source[self.span]
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
