//! The lexer (lexical analyzer).
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
    span::{LocalByteIndex, LocalSpan, SourceFile, SourceMap, Span},
    Atom,
};
use std::{
    cmp::Ordering::{self, *},
    convert::{TryFrom, TryInto},
    iter::Peekable,
    str::CharIndices,
};
pub use token::{
    is_punctuation, Token,
    TokenKind::{self, *},
};

/// The unit indentation in spaces.
pub const INDENTATION: Spaces = Indentation::UNIT.to_spaces();

fn lex(source: String) -> Results<Vec<Token>> {
    let mut map = SourceMap::default();
    let file = map.add(None, source).unwrap_or_else(|_| unreachable!());
    Lexer::new(&map[file], &mut Default::default()).lex()
}

/// Utility to parse identifiers from a string.
///
/// Used for non-lushui code like crate names.
pub fn parse_identifier(source: String) -> Option<Atom> {
    let mut tokens = lex(source).ok()?;
    let mut tokens = tokens.drain(..);

    match (tokens.next()?, tokens.next()?.kind) {
        (
            Token {
                kind: Identifier,
                data: token::TokenData::Identifier(atom),
                ..
            },
            EndOfInput,
        ) => Some(atom),
        _ => None,
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Section {
    TopLevel,
    /// A section of code within curly brackets.
    Delimited,
    /// An indented section of code following th keyword `of` or `do`.
    Indented,
    /// An indented section of code than seamlessly continues the previous line.
    Continued,
}

impl Section {
    /// Indicate whether line breaks terminate syntactic constructs in this section.
    const fn line_breaks_are_terminators(self) -> bool {
        matches!(self, Self::TopLevel | Self::Indented)
    }
}

impl Default for Section {
    fn default() -> Self {
        Self::TopLevel
    }
}

type Stack<T> = Vec<T>;

/// The state of the lexer.
pub struct Lexer<'a> {
    source: &'a SourceFile,
    characters: Peekable<CharIndices<'a>>,
    tokens: Vec<Token>,
    span: LocalSpan,
    indentation: Spaces,
    round_brackets: Stack<Span>,
    warnings: &'a mut Diagnostics,
    section: Stack<Section>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a SourceFile, warnings: &'a mut Diagnostics) -> Self {
        Self {
            source,
            warnings,
            characters: source.content().char_indices().peekable(),
            tokens: Vec::new(),
            span: LocalSpan::zero(),
            indentation: Spaces(0),
            round_brackets: Stack::new(),
            section: Stack::new(),
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
                    self.section.push(Section::Delimited);
                    self.add(OpeningCurlyBracket);
                    self.advance();
                }
                '}' => {
                    // @Temporary unverified
                    if self
                        .section
                        .last()
                        .map_or(false, |&section| section == Section::Delimited)
                    {
                        self.section.pop();
                    }
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

        // Ideally, we'd like to set its span to the index after the last token,
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
        // @Note not extensible to Section::Delimited
        let is_start_of_indented_section = self
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
        let mut spaces = Spaces(0);
        self.take_while_with(|character| character == ' ', || spaces.0 += 1);

        let (change, difference) = spaces.difference(self.indentation);

        // self.span = LocalSpan::new(self.span.end + 1 - absolute_difference, self.span.end);

        let difference: Indentation = match (change, difference).try_into() {
            Ok(difference) => difference,
            Err(_error) => {
                // @Task don't fail fatally, accept it but push error into an error buffer
                return Err(Diagnostic::error()
                    .with_code(Code::E003)
                    .with_message(format!(
                        "invalid indentation consisting of {} spaces",
                        difference.0
                    ))
                    .with_primary_span(self.span())
                    // @Bug we display this even in case of ind==8|12|…
                    .with_note(format!(
                        "indentation needs to be a multiple of {}",
                        INDENTATION.0
                    )));
            }
        };

        let section = self.section.last().copied().unwrap_or_default();

        if is_start_of_indented_section {
            if change == Greater {
                // remove the line break again
                self.tokens.pop();
                // @Bug wrong span
                self.add(OpeningCurlyBracket);
                self.section.push(Section::Indented);
            }
        } else {
            // Remove the line break again if the next line is indented or if we are in a Section
            // where line breaks are not terminators or if we dedent by some amount and the
            // “target Section” treats line breaks as terminators.
            if change == Greater
                || change == Equal && !section.line_breaks_are_terminators()
                || change == Less
                    && !self
                        .section
                        .len()
                        .checked_sub(1 + difference.0)
                        .map(|index| self.section[index])
                        .unwrap_or_default()
                        .line_breaks_are_terminators()
            {
                // remove the line break again
                self.tokens.pop();
            }

            if change == Greater {
                self.section.push(Section::Continued);
            }
        };

        if change == Less {
            for _ in 0..difference.0 {
                // unwrap: we currently handle dedentation which means priorly code
                // was indented i.e. is not the top level
                let section = self.section.pop().unwrap();

                if section == Section::Indented {
                    // // @Temporary
                    // if self
                    //     .tokens
                    //     .last()
                    //     .map_or(false, |token| token.kind == LineBreak)
                    // {
                    //     self.tokens.pop();
                    // }

                    // @Bug wrong span
                    self.add(ClosingCurlyBracket);

                    self.add(LineBreak); // @Temporary

                    // if self
                    //     .section
                    //     .last()
                    //     .map_or(true, |section| section.line_breaks_are_terminators())
                    // {
                    //     // @Temporary @Note a temp fix so that mod/data decls are terminated by
                    //     // a line break which they have to be. issue: not compatible with
                    //     // case analysis. @Task we need to update the parser such that it
                    //     // accepts declarations with a trailing block but no trailing line break
                    //     self.add(LineBreak);
                    // }
                }
            }
        }

        self.indentation = spaces;

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

#[derive(Clone, Copy)]
pub struct Spaces(pub usize);

impl Spaces {
    /// Returns the ordering and the absolute difference.
    pub fn difference(self, other: Self) -> (Ordering, Self) {
        let change = self.0.cmp(&other.0);
        let difference = match change {
            Greater => self.0 - other.0,
            Less => other.0 - self.0,
            Equal => 0,
        };
        (change, Self(difference))
    }
}

#[derive(Clone, Copy)]
pub struct Indentation(pub usize);

impl Indentation {
    pub const UNIT: Self = Self(1);

    pub const fn to_spaces(self) -> Spaces {
        const INDENTATION_IN_SPACES: usize = 4;

        Spaces(self.0 * INDENTATION_IN_SPACES)
    }
}

impl From<Indentation> for Spaces {
    fn from(indentation: Indentation) -> Self {
        indentation.to_spaces()
    }
}

impl TryFrom<(Ordering, Spaces)> for Indentation {
    type Error = IndentationError;

    fn try_from((change, spaces): (Ordering, Spaces)) -> Result<Self, Self::Error> {
        if spaces.0 % INDENTATION.0 != 0 {
            return Err(IndentationError::Misaligned);
        }

        if change == Greater && spaces.0 > INDENTATION.0 {
            return Err(IndentationError::TooDeep);
        }

        Ok(Indentation(spaces.0 / INDENTATION.0))
    }
}

pub enum IndentationError {
    Misaligned,
    TooDeep,
}
