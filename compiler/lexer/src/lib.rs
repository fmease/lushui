//! The lexical analyzer (lexer).
#![feature(default_free_fn, decl_macro)]

use span::{FileName, LocalByteIndex, LocalSpan, SourceFile, SourceMap, Span, Spanned};
use std::{cmp::Ordering, default::default, iter::Peekable, mem, str::CharIndices, sync::Arc};
use token::{
    BareToken, Bracket, BracketKind, BracketOrientation, Indentation, IndentationError, Provenance,
    Spaces, Token, TokenExt,
};
use utilities::{self, GetFromEndExt};
use BareToken::*;

#[cfg(test)]
mod test;
// @Task move this out of this crate
pub mod word;

pub fn lex(file: &SourceFile, options: &Options) -> Outcome {
    Lexer::new(file, options).lex()
}

#[derive(PartialEq, Eq, Debug)]
pub struct Outcome {
    pub tokens: Vec<Token>,
    pub errors: Vec<Error>,
}

#[derive(Default)]
pub struct Options {
    /// Reify comments and shebangs as tokens.
    pub keep_comments: bool,
}

/// The state of the lexer.
struct Lexer<'a> {
    characters: Peekable<CharIndices<'a>>,
    file: &'a SourceFile,
    options: &'a Options,
    tokens: Vec<Token>,
    errors: Vec<Error>,
    local_span: LocalSpan,
    indentation: Spaces,
    sections: Sections,
    brackets: Brackets,
}

impl<'a> Lexer<'a> {
    fn new(file: &'a SourceFile, options: &'a Options) -> Self {
        Self {
            characters: file.content().char_indices().peekable(),
            file,
            options,
            tokens: Vec::new(),
            errors: Vec::new(),
            local_span: LocalSpan::default(),
            indentation: Spaces(0),
            sections: Sections::default(),
            brackets: Brackets::default(),
        }
    }

    fn lex(mut self) -> Outcome {
        while let Some((index, character)) = self.peek_with_index() {
            self.local_span = LocalSpan::empty(index);

            match character {
                '#' if index == default() => self.lex_shebang_candidate(),
                ' ' if index == default() => self.lex_indentation(),
                ' ' => self.lex_whitespace(),
                ';' => {
                    self.take();
                    self.advance();

                    if let Some(';') = self.peek() {
                        self.lex_comment();
                    } else {
                        self.add(Semicolon(Provenance::Source));
                    }
                }
                character if is_identifier_segment_start(character) => self.lex_identifier(),
                '\n' if self.sections.current() != Section::Delimited => {
                    self.take();
                    self.advance();
                    self.lex_indentation();
                }
                '\n' => self.advance(),
                '-' => self.lex_symbol_or_number_literal(),
                character if character.is_ascii_digit() => {
                    self.take();
                    self.advance();
                    self.lex_number_literal();
                }
                character if token::is_symbol(character) => {
                    self.take();
                    self.advance();
                    self.lex_symbol();
                }
                '"' => self.lex_text_literal(),
                '(' => self.add_opening_bracket(BracketKind::Round),
                '[' => self.add_opening_bracket(BracketKind::Square),
                '{' => {
                    self.sections.enter(Section::Delimited);
                    self.add_opening_bracket(BracketKind::Curly);
                }
                ')' => {
                    self.add_closing_bracket(BracketKind::Round);
                }
                ']' => {
                    self.add_closing_bracket(BracketKind::Square);
                }
                '}' => {
                    if self.sections.current() == Section::Delimited {
                        self.sections.exit();
                    }
                    self.add_closing_bracket(BracketKind::Curly);
                }
                '\'' => self.consume(Apostrophe),
                character => {
                    self.take();
                    self.advance();
                    self.error(BareError::InvalidToken(character));
                }
            }
        }

        if !self.brackets.stack.is_empty() {
            // @Task don't report all remaining brackets in the stack, only unclosed ones! See #113.
            for bracket in &self.brackets.stack {
                self.errors.push(bracket.map(|bracket| {
                    BareError::UnbalancedBracket(Bracket::new(bracket, BracketOrientation::Opening))
                }));
            }
        }

        self.local_span = self.file.local_span().end();
        for section in self.sections.exit_all() {
            if section.is_indented() {
                self.add(ClosingCurlyBracket(Provenance::Lexer));
                self.add(Semicolon(Provenance::Lexer));
            }
        }
        self.add(EndOfInput);

        Outcome {
            tokens: self.tokens,
            errors: self.errors,
        }
    }

    fn add_opening_bracket(&mut self, bracket: BracketKind) {
        self.take();
        self.add(bracket.opening());

        self.brackets.open(Spanned::new(self.span(), bracket));
        self.advance();
    }

    fn add_closing_bracket(&mut self, bracket: BracketKind) {
        self.take();

        if let (Section::Indented { brackets }, size) = self.sections.current_continued() {
            if self.brackets.stack.len() <= brackets {
                self.add(ClosingCurlyBracket(Provenance::Lexer));
                // @Question can this panic??
                let dedentation = self.sections.0.len() - size;
                self.sections.0.truncate(size);
                self.indentation -= Indentation(dedentation);
            }
        }

        self.add(bracket.closing());

        if let Err(error) = self.brackets.close(Spanned::new(self.span(), bracket)) {
            self.errors.push(error);
        };
        self.advance();
    }

    fn lex_shebang_candidate(&mut self) {
        self.take();
        self.advance();

        if let Some('!') = self.peek() {
            while let Some(character) = self.peek() {
                if self.options.keep_comments {
                    self.take();
                }
                self.advance();

                if character == '\n' {
                    break;
                }
            }

            if self.options.keep_comments {
                self.add(Shebang);
            }
        } else {
            self.lex_symbol();
        }
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
        self.take();
        self.advance();

        let mut is_documentation = true;

        if let Some(character) = self.peek() {
            self.advance();

            if character == ';' {
                is_documentation = false;
            }

            if character != '\n' {
                self.take();

                while let Some(character) = self.peek() {
                    if character == '\n' {
                        self.advance();
                        break;
                    }

                    if is_documentation || self.options.keep_comments {
                        self.take();
                    }

                    self.advance();
                }
            }
        }

        if is_documentation {
            self.add(DocumentationComment);
        } else if self.options.keep_comments {
            self.add(Comment);
        }
    }

    fn lex_identifier(&mut self) {
        self.lex_first_identifier_segment();
        while self.peek() == Some('-') {
            self.take();
            self.advance();
            self.lex_identifier_segment();
        }

        match lex_keyword(self.source()) {
            Some(keyword) => self.add(keyword),
            None => {
                self.add(Word(self.source().into()));
            }
        };

        const DOT: char = '.';
        if let Some((index, DOT)) = self.peek_with_index() {
            #[allow(clippy::cast_possible_truncation)] // always within 1..=4
            const LENGTH: u32 = DOT.len_utf8() as _;
            self.local_span = LocalSpan::with_length(index, LENGTH);
            self.add(Dot);
            self.advance();
        }
    }

    fn lex_first_identifier_segment(&mut self) {
        if let Some(character) = self.peek() {
            if is_identifier_segment_start(character) {
                self.take();
                self.advance();
                self.take_while(is_identifier_segment_middle);
            }
        }
    }

    fn lex_identifier_segment(&mut self) {
        self.take_while(is_identifier_segment_middle);
    }

    // @Task recover from tabs (treat them as 4 spaces) and emit a custom error
    fn lex_indentation(&mut self) {
        let is_start_of_indented_section = self
            .tokens
            .last()
            .map_or(false, |token| token.name().introduces_indented_section());

        // squash consecutive line breaks into a single one
        self.take_while(|character| character == '\n');
        // might be removed again under certain conditions later on
        self.add(Semicolon(Provenance::Lexer));

        self.local_span = self
            .index()
            .map_or_else(|| self.file.local_span().end(), LocalSpan::empty);

        let mut spaces = Spaces(0);
        self.take_while_with(|character| character == ' ', || spaces.0 += 1);

        // if the line is empty, ignore it (important for indented comments)
        if self.line_is_empty() {
            spaces = self.indentation;
        }

        let (change, difference) = spaces.difference(self.indentation);

        let indentation: Indentation = match (change, difference).try_into() {
            Ok(difference) => difference,
            Err((indentation, error)) => {
                self.error(BareError::InvalidIndentation(difference, error));
                // @Task add some tests for this "recovered"/reconstructed indentation!
                spaces = indentation.to_spaces();
                indentation
            }
        };

        let section = self.sections.current();

        if is_start_of_indented_section {
            if change == Ordering::Greater {
                self.tokens.pop(); // remove the line break again
                self.add(OpeningCurlyBracket(Provenance::Lexer));
                self.sections.enter(Section::Indented {
                    brackets: self.brackets.stack.len(),
                });
            }
        } else {
            // Remove the line break again if the next line is indented or if we are in a section
            // where line breaks ((virtual) semicolons) are not terminators (which include semicolons)
            // or if we dedent by some amount and the section reached treats line breaks as terminators.
            if change == Ordering::Greater
                || change == Ordering::Equal && !section.line_breaks_are_terminators()
                || change == Ordering::Less
                    && !self
                        .sections
                        .get(indentation.0)
                        .line_breaks_are_terminators()
            {
                self.tokens.pop(); // remove the line break again
            }

            if change == Ordering::Greater {
                self.sections.enter(Section::Continued);
            }
        };

        if change == Ordering::Less {
            // Remove syntactically legal but superfluous virtual semicolons before virtual
            // closing curly brackets (which also act as terminators).
            if self.sections.current_continued().0.is_indented()
                && self.tokens.last().map_or(false, Token::is_line_break)
            {
                self.tokens.pop();
            }

            for _ in 0..indentation.0 {
                let section = self.sections.exit();

                // @Task handle the case where `!section.brackets.is_empty()`
                if section.is_indented() {
                    self.add(ClosingCurlyBracket(Provenance::Lexer));
                    self.add(Semicolon(Provenance::Lexer));
                }
            }
        }

        self.indentation = spaces;
    }

    fn line_is_empty(&mut self) -> bool {
        self.peek() == Some('\n')
        // @Bug has unforseen consequences (investigation needed)
        // || self.peek() == Some(';') && {
        //     let mut characters = self.characters.clone();
        //     characters.next();
        //     matches!(characters.next(), Some((_, ';')))
        //         && matches!(characters.next(), Some((_, ';')))
        // }
    }

    fn lex_symbol_or_number_literal(&mut self) {
        self.take();
        self.advance();

        match self.peek() {
            Some(character) if character.is_ascii_digit() => self.lex_number_literal(),
            _ => self.lex_symbol(),
        }
    }

    fn lex_symbol(&mut self) {
        self.take_while(token::is_symbol);

        match lex_reserved_symbol(&self.file[self.local_span]) {
            Some(symbol) => self.add(symbol),
            None => {
                self.add(Symbol(self.source().into()));
            }
        }
    }

    fn lex_number_literal(&mut self) {
        let mut number = self.source().to_owned();

        while let Some(character) = self.peek() {
            if !is_number_literal_middle(character) {
                break;
            }
            self.take();
            self.advance();

            if character != NUMERIC_SEPARATOR {
                number.push(character);
            }
        }

        self.add(NumberLiteral(number.into()));
    }

    // @Task escape sequences
    fn lex_text_literal(&mut self) {
        self.take();
        self.advance();

        let mut is_terminated = false;

        const QUOTE: char = '"';
        while let Some(character) = self.peek() {
            self.take();
            self.advance();

            if character == QUOTE {
                is_terminated = true;
                break;
            }
        }

        // @Note this naive trimming and indexing into the source file does not scale to
        //       escape sequences (once we implement them)
        #[allow(clippy::cast_possible_truncation)] // always within 1..=4
        const TRIM_LENGTH: u32 = QUOTE.len_utf8() as _;
        let content_span = if is_terminated {
            self.local_span.trim(TRIM_LENGTH)
        } else {
            self.error(BareError::UnterminatedTextLiteral);
            self.local_span.trim_start(TRIM_LENGTH)
        };

        self.add(TextLiteral(self.file[content_span].into()));
    }

    fn span(&self) -> Span {
        self.local_span.global(self.file)
    }

    fn source(&self) -> &'a str {
        &self.file[self.local_span]
    }

    /// Step to the next token in the input stream.
    fn advance(&mut self) {
        self.characters.next();
    }

    /// Include the span of the current token in the span of the token-to-be-added.
    ///
    /// Preparation for [`Self::add`] and variants.
    fn take(&mut self) {
        let (index, character) = self.peek_with_index().unwrap();
        self.local_span.set_end(index + character);
    }

    fn peek(&mut self) -> Option<char> {
        self.peek_with_index().map(|(_, character)| character)
    }

    fn peek_with_index(&mut self) -> Option<(LocalByteIndex, char)> {
        self.characters
            .peek()
            .map(|&(index, character)| (index.try_into().unwrap(), character))
    }

    fn index(&mut self) -> Option<LocalByteIndex> {
        self.peek_with_index().map(|(index, _)| index)
    }

    /// [Take](Self::take) the span of all succeeding tokens where the predicate holds and step.
    fn take_while(&mut self, predicate: fn(char) -> bool) {
        self.take_while_with(predicate, || ());
    }

    /// [Take](Self::take) the span of all succeeding tokens where the predicate holds, step and perform the given action.
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

    /// Add a token with the given kind to the output of the lexer.
    ///
    /// The other component of a token – the span – is stored in the lexer and is most commonly
    /// updated using [`Self::take`].
    fn add(&mut self, token: BareToken) {
        self.add_with(|span| Spanned::new(span, token));
    }

    /// [Add](Self::add) a token given a constructor.
    fn add_with(&mut self, constructor: impl FnOnce(Span) -> Spanned<BareToken>) {
        let span = self.span();
        self.tokens.push(constructor(span));
    }

    fn error(&mut self, error: BareError) {
        self.errors.push(Error::new(self.span(), error));
    }

    fn consume(&mut self, token: BareToken) {
        self.take();
        self.advance();
        self.add(token);
    }
}

fn lex_string(source: String) -> Outcome {
    let mut map = SourceMap::default();
    let file = map.add(FileName::Anonymous, Arc::new(source), None);
    Lexer::new(
        &map[file],
        &Options {
            keep_comments: true,
        },
    )
    .lex()
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
enum Section {
    /// The top-level section.
    ///
    /// Basically the same as [Section::Indented] except that it does not call
    /// for adding any virtual closing curly brackets since the indentation is
    /// zero.
    #[default]
    Top,
    /// A section of code within curly brackets.
    ///
    /// Line breaks are not terminators and it is not indentation-sensitive.
    Delimited,
    /// An indented section of code following the keyword `of` or `do`.
    ///
    /// Stores the amount of open brackets coming before it.
    Indented { brackets: usize },
    /// An indented section of code that seamlessly continues the previous line.
    Continued,
}

impl Section {
    /// Indicate whether line breaks terminate syntactic constructs in this section.
    const fn line_breaks_are_terminators(self) -> bool {
        matches!(self, Self::Top | Self::Indented { .. })
    }

    const fn is_indented(self) -> bool {
        matches!(self, Self::Indented { .. })
    }
}

#[derive(Default, Debug)]
struct Sections(Stack<Section>);

impl Sections {
    fn current(&self) -> Section {
        self.get(0)
    }

    /// Current section stripped from any [`Section::Continued`]s.
    fn current_continued(&self) -> (Section, usize) {
        // @Task replace implementation with Iterator::position

        let mut index = self.0.len();

        let section = (|| {
            let mut section;

            loop {
                index = index.checked_sub(1)?;
                section = *unsafe { self.0.get_unchecked(index) };

                if section != Section::Continued {
                    break;
                }
            }

            Some(section)
        })()
        .unwrap_or_default();

        (section, index)
    }

    fn get(&self, index: usize) -> Section {
        self.0.get_from_end(index).copied().unwrap_or_default()
    }

    fn enter(&mut self, section: Section) {
        self.0.push(section);
    }

    fn exit(&mut self) -> Section {
        self.0.pop().unwrap_or_default()
    }

    fn exit_all(&mut self) -> impl Iterator<Item = Section> {
        mem::take(&mut self.0).into_iter().rev()
    }
}

#[derive(Default)]
struct Brackets {
    stack: Stack<Spanned<BracketKind>>,
}

impl Brackets {
    fn open(&mut self, bracket: Spanned<BracketKind>) {
        self.stack.push(bracket);
    }

    fn close(&mut self, bracket: Spanned<BracketKind>) -> Result<(), Error> {
        match self.stack.pop() {
            Some(opening_bracket) if opening_bracket.bare == bracket.bare => Ok(()),
            _ => Err(bracket.map(|bracket| {
                BareError::UnbalancedBracket(Bracket::new(bracket, BracketOrientation::Closing))
            })),
        }
    }
}

type Stack<T> = Vec<T>;

const fn is_identifier_segment_start(character: char) -> bool {
    character.is_ascii_alphabetic() || character == '_'
}

const fn is_identifier_segment_middle(character: char) -> bool {
    character.is_ascii_alphanumeric() || character == '_'
}

const NUMERIC_SEPARATOR: char = '\'';

const fn is_number_literal_middle(character: char) -> bool {
    character.is_ascii_digit() || character == NUMERIC_SEPARATOR
}

fn lex_keyword(source: &str) -> Option<BareToken> {
    Some(match source {
        "_" => Underscore,
        "as" => As,
        "case" => Case,
        "data" => Data,
        "do" => Do,
        "extern" => Extern,
        "for" => ForLower,
        "For" => ForUpper,
        "in" => In,
        "let" => Let,
        "module" => Module,
        "of" => Of,
        "self" => Self_,
        "super" => Super,
        "topmost" => Topmost,
        "use" => Use,
        _ => return None,
    })
}

fn lex_reserved_symbol(source: &str) -> Option<BareToken> {
    Some(match source {
        "." => Dot,
        ":" => Colon,
        "=" => Equals,
        "\\" => Backslash,
        "?" => QuestionMark,
        "@" => At,
        "->" => ThinArrowRight,
        "<-" => ThinArrowLeft,
        "=>" => WideArrowRight,
        "::" => DoubleColon,
        "**" => DoubleAsterisk,
        _ => return None,
    })
}

pub type Error = Spanned<BareError>;

#[derive(PartialEq, Eq, Debug)]
pub enum BareError {
    InvalidIndentation(Spaces, IndentationError),
    InvalidToken(char),
    UnbalancedBracket(Bracket),
    UnterminatedTextLiteral,
}
