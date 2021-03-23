//! The lexer.
//!
//! ## Issues
//!
//! * most lexical errors are fatal right now. we shouldn't make the lexer fail
//!   on mismatching brackets, that should be the task of the parser (indeed also
//!   keeping the bracket stack!)

#[cfg(test)]
mod test;
pub mod token;

use crate::{
    diagnostics::{Code, Diagnostic, Diagnostics, Result, Results, Warn},
    error::ManyErrExt,
    span::{LocalByteIndex, LocalSpan, SourceFile, Span},
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

// @Task add documentation, better name
#[derive(Clone, Copy, PartialEq, Eq)]
// @Temporary
#[derive(Debug)]
enum LineContinuation {
    TopLevel,
    DelimitedBlock,
    IndentedBlock,
    Indentation,
}

impl LineContinuation {
    const fn line_break_is_terminator(self) -> bool {
        matches!(self, Self::TopLevel | Self::IndentedBlock)
    }
}

impl Default for LineContinuation {
    fn default() -> Self {
        Self::TopLevel
    }
}

// #[derive(Default)]
// struct LineContinuationStack(Vec<LineContinuation>);

// impl LineContinuationStack {
//     fn push(&mut self, mode: LineContinuation) {
//         eprintln!("push({:?}) to {:?}", mode, self.0);
//         self.0.push(mode);
//     }

//     // @Question good API?
//     fn pop(&mut self) -> LineContinuation {
//         eprintln!("pop({:?})", self.current());
//         self.0.pop().unwrap_or_default()
//     }

//     fn current(&self) -> LineContinuation {
//         self.0.last().copied().unwrap_or_default()
//     }
// }

/// The state of the lexer.
pub struct Lexer<'a> {
    source: &'a SourceFile,
    characters: Peekable<CharIndices<'a>>,
    tokens: Vec<Token>,
    span: LocalSpan,
    indentation_in_spaces: usize,
    round_brackets: Vec<Span>,
    warnings: &'a mut Diagnostics,
    line_continuation: Vec<LineContinuation>,
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
            line_continuation: Vec::new(),
        }
    }

    // @Task move balanced bracket validation out of lexer to the parser and
    // also start supporting square and curly brachets for future use
    /// Lex source code into an array of tokens
    pub fn lex(mut self) -> Results<Vec<Token>> {
        while let Some(character) = self.peek() {
            self.span = LocalSpan::from(self.index().unwrap());
            match character {
                // @Bug @Beacon if it is SOI, don't lex_whitespace but lex_indentation
                // (SOI should act as a line break)
                ' ' => self.lex_whitespace(),
                ';' => self.lex_comment(),
                character if token::is_identifier_segment_start(character) => {
                    self.lex_identifier().many_err()?
                }
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
                    self.line_continuation
                        .push(LineContinuation::DelimitedBlock);
                    self.add(OpeningCurlyBracket);
                    self.advance();
                }
                '}' => {
                    self.add(ClosingCurlyBracket);
                    self.advance();
                }
                '\'' => {
                    self.add(SingleQuote);
                    self.advance();
                }
                character => {
                    self.take();
                    self.advance();
                    self.add_with(|span| Token::new_illegal(character, span))
                }
            }
        }

        // @Task move this out of the lexer to the parser
        if !self.round_brackets.is_empty() {
            return Err(self
                .round_brackets
                .into_iter()
                .map(|bracket| {
                    Diagnostic::error()
                        .with_code(Code::E001)
                        .with_message("unbalanced brackets")
                        .with_labeled_primary_span(bracket, "has no matching closing bracket")
                })
                .collect());
        }

        // @Bug panics if last byte is not a valid codepoint, right?
        let last = LocalByteIndex::from_usize(self.source.content().len() - 1);

        // ideally, we'd like to set its span to the index after the last token,
        // but they way `SourceMap` is currently defined, that would not work,
        // it would reach into the next `SourceFile` if there even was one
        // I'd love to put "the red caret" in diagnostics visually after the last
        // token. However, even with fake source files as separators between real ones,
        // we would need to add a lot of complexity to be able to handle that in
        // `Diagnostic::format_for_terminal`.
        // @Task just take the span of the last token (this will break tests)
        // @Update don't! consider what would happen with the diagnostic of
        // unterminated text literals!
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
    fn lex_identifier(&mut self) -> Result {
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
                    .with_primary_span(Span::local(self.source, dash.into())));
            }
        }

        match token::parse_keyword(&self.source[self.span]) {
            Some(keyword) => self.add(keyword),
            None => {
                let identifier = self.source[self.span].into();
                self.add_with(|span| Token::new_identifier(identifier, span))
            }
        };

        if self.peek() == Some('.') {
            self.span = LocalSpan::from(self.index().unwrap());
            self.add(TokenKind::Dot);
            self.advance();
        }

        Ok(())
    }

    fn lex_identifier_segment(&mut self) {
        if let Some(character) = self.peek() {
            if token::is_identifier_segment_start(character) {
                self.take();
                self.advance();
                self.take_while(token::is_identifier_segment_middle);
            }
        }
    }

    fn lex_indentation(&mut self) -> Result {
        use std::cmp::Ordering::*;

        // @Note not extensible to DelimitedBlocks
        let found_block_marker = self
            .tokens
            .last()
            .map_or(false, |token| matches!(token.kind, Of | Do));

        // squash consecutive line breaks into a single one
        self.advance();
        self.take_while(|character| character == '\n');
        self.add(LineBreak);

        // self.span = match self.index() {
        //     Some(index) => LocalSpan::from(index),
        //     None => return Ok(()),
        // };
        let mut spaces = 0;
        self.take_while_with(|character| character == ' ', || spaces += 1);

        let change = spaces.cmp(&self.indentation_in_spaces);
        let absolute_difference = match change {
            Greater => spaces - self.indentation_in_spaces,
            Less => self.indentation_in_spaces - spaces,
            Equal => 0,
        };

        // self.span = LocalSpan::new(self.span.end + 1 - absolute_difference, self.span.end);

        // @Task don't fail fatally, accept it but push error into an error buffer
        if absolute_difference % INDENTATION_IN_SPACES != 0
            || change == Greater && absolute_difference > INDENTATION_IN_SPACES
        {
            return Err(Diagnostic::error()
                .with_code(Code::E003)
                .with_message(format!(
                    "invalid indentation consisting of {} spaces",
                    absolute_difference
                ))
                .with_primary_span(self.span())
                .with_note(format!(
                    "indentation needs to be a multiple of {}",
                    INDENTATION_IN_SPACES
                )));
        }

        // @Note
        // let x = do
        // let y = do
        // means do{}, do{}

        // @Beacon @Beacon @Task don't necessarily emit a LineBreak before a ClosingCurlyBracket
        // ... the  parser can handle it (in parse_terminated_expression)

        let line_continuation = self.line_continuation.last().copied().unwrap_or_default();

        // @Task simplify
        if found_block_marker {
            self.tokens.pop(); // remove the line break
            self.add(OpeningCurlyBracket); // @Bug wrong span
            self.line_continuation.push(LineContinuation::IndentedBlock);
        } else {
            if change == Greater || change == Equal && !line_continuation.line_break_is_terminator()
            {
                self.tokens.pop(); // remove the line break
            }
            if change == Greater {
                self.line_continuation.push(LineContinuation::Indentation);
            }
        };

        if change == Less || change == Equal && found_block_marker {
            let dedentation = absolute_difference / INDENTATION_IN_SPACES;

            for _ in 0..=dedentation {
                // @Note _or_default prob never reaches, @Task use unwrap()
                let line_continuation = self.line_continuation.pop().unwrap_or_default();

                if line_continuation == LineContinuation::IndentedBlock {
                    // @Bug wrong span
                    self.add(ClosingCurlyBracket);
                    // @Temporary @Note a temp fix so that mod/data decls are terminated by
                    // a line break which they have to be. issue: not compatible with
                    // case analysis. @Task we need to update the parser such that it
                    // accepts declarations with a trailing block but no trailing line break
                    self.add(LineBreak);
                }
            }
        }

        self.indentation_in_spaces = spaces;

        Ok(())
    }

    fn lex_punctuation_or_number_literal(&mut self) -> Results {
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
    fn lex_number_literal(&mut self) -> Result {
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
                .with_primary_span(self.span()));
        }

        // @Task return an invalid token instead

        if trailing_prime {
            return Err(Diagnostic::error()
                .with_code(Code::E005)
                .with_message("trailing prime in number literal")
                .with_primary_span(self.span()));
        }

        self.add_with(|span| Token::new_number_literal(number, span));

        Ok(())
    }

    // @Task escape sequences @Update do them in the parser (or at least mark them as invalid
    // and do the error reporting in the parser)
    fn lex_text_literal(&mut self) -> Result {
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

    fn lex_closing_round_bracket(&mut self) -> Result {
        self.add(ClosingRoundBracket);
        if self.round_brackets.is_empty() {
            return Err(Diagnostic::error()
                .with_code(Code::E001)
                .with_message("unbalanced brackets")
                .with_labeled_primary_span(self.span(), "has no matching opening bracket"));
        }
        self.round_brackets.pop();
        self.advance();

        Ok(())
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
}

impl Warn for Lexer<'_> {
    fn diagnostics(&mut self) -> &mut Diagnostics {
        &mut self.warnings
    }
}
