//! The lexer (lexical analyzer).

#[cfg(test)]
mod test;
pub mod token;

use crate::{
    diagnostics::{reporter::SilentReporter, Code, Diagnostic, Reporter},
    error::{outcome, Health, Outcome, Result},
    span::{LocalByteIndex, LocalSpan, SourceFile, SourceMap, Span, Spanned},
    util::{self, lexer::Lexer as _, obtain, Atom, GetFromEndExt},
};
use std::{
    cmp::Ordering::{self, *},
    convert::{TryFrom, TryInto},
    fmt,
    iter::Peekable,
    ops::{Sub, SubAssign},
    path::Path,
    str::CharIndices,
};
pub use token::{Token, TokenKind, TokenName, NUMERIC_SEPARATOR};
use TokenKind::*;

use self::token::{Provenance, UnterminatedTextLiteral};

/// The unit indentation in spaces.
pub const INDENTATION: Spaces = Indentation::UNIT.to_spaces();

fn lex(source: String) -> Result<Outcome<Vec<Token>>> {
    let mut map = SourceMap::default();
    let file = map.add(None, source).unwrap_or_else(|_| unreachable!());
    Lexer::new(&map[file], &SilentReporter.into()).lex()
}

/// Utility to parse identifiers from a string.
///
/// Used for non-lushui code like crate names.
pub fn parse_identifier(source: String) -> Option<Atom> {
    let outcome!(mut tokens, health) = lex(source).ok()?;

    if health == Health::Tainted {
        return None;
    }

    let mut tokens = tokens.drain(..).map(|token| token.data);

    obtain!(
        (tokens.next()?, tokens.next()?),
        (Identifier(atom), EndOfInput) => atom
    )
}

pub fn parse_crate_name(
    file: impl AsRef<Path>,
    reporter: &Reporter,
) -> Result<crate::ast::Identifier> {
    let file = file.as_ref();

    if !crate::util::has_file_extension(file, crate::FILE_EXTENSION) {
        Diagnostic::warning()
            .message("missing or non-standard file extension")
            .report(reporter);
    }

    // @Question does unwrap ever fail in a real-world example?
    let stem = file.file_stem().unwrap();

    let atom = (|| parse_identifier(stem.to_str()?.to_owned()))().ok_or_else(|| {
        Diagnostic::error()
            .message(format!(
                "`{}` is not a valid crate name",
                stem.to_string_lossy()
            ))
            .report(reporter);
    })?;

    Ok(crate::ast::Identifier::new(atom, Span::SHAM))
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

    /// Current section stripped from any [Section::Continued]s.
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
        std::mem::take(&mut self.0).into_iter().rev()
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Bracket {
    Round,
    Square,
    Curly,
}

impl Bracket {
    const fn opening(self) -> TokenKind {
        match self {
            Self::Round => OpeningRoundBracket,
            Self::Square => OpeningSquareBracket,
            Self::Curly => OpeningCurlyBracket(Provenance::Source),
        }
    }

    const fn closing(self) -> TokenKind {
        match self {
            Self::Round => ClosingRoundBracket,
            Self::Square => ClosingSquareBracket,
            Self::Curly => ClosingCurlyBracket(Provenance::Source),
        }
    }
}

impl fmt::Display for Bracket {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Round => "round",
            Self::Square => "square",
            Self::Curly => "curly",
        })
    }
}

#[derive(Default)]
struct Brackets(Stack<Spanned<Bracket>>);

impl Brackets {
    fn open(&mut self, opening_bracket: Spanned<Bracket>) {
        self.0.push(opening_bracket);
    }

    fn close(&mut self, closing_bracket: Spanned<Bracket>) -> Result<(), Diagnostic> {
        match self.0.pop() {
            Some(opening_bracket) => {
                if opening_bracket.data == closing_bracket.data {
                    Ok(())
                } else {
                    // @Beacon @Bug we are not smart enough here yet, the error messages are too confusing
                    // or even incorrect!
                    Err(Diagnostic::error()
                        .code(Code::E001)
                        .message(format!("unbalanced {} bracket", closing_bracket.data))
                        .labeled_primary_span(
                            closing_bracket,
                            format!("has no matching opening {} bracket", closing_bracket.data),
                        ))
                }
            }
            // @Beacon @Bug we are not smart enough here yet, the error messages are too confusing
            // or even incorrect!
            None => Err(Diagnostic::error()
                .code(Code::E001)
                .message(format!("unbalanced {} bracket", closing_bracket.data))
                .labeled_primary_span(
                    closing_bracket,
                    format!("has no matching opening {} bracket", closing_bracket.data),
                )),
        }
    }
}

type Stack<T> = Vec<T>;

/// The state of the lexer.
pub struct Lexer<'a> {
    source_file: &'a SourceFile,
    characters: Peekable<CharIndices<'a>>,
    tokens: Vec<Token>,
    local_span: LocalSpan,
    indentation: Spaces,
    sections: Sections,
    brackets: Brackets,
    health: Health,
    reporter: &'a Reporter,
}

impl<'a> Lexer<'a> {
    pub fn new(source_file: &'a SourceFile, reporter: &'a Reporter) -> Self {
        Self {
            characters: source_file.content().char_indices().peekable(),
            source_file,
            tokens: Vec::new(),
            local_span: LocalSpan::zero(),
            indentation: Spaces(0),
            sections: Sections::default(),
            brackets: Brackets::default(),
            health: Health::Untainted,
            reporter,
        }
    }

    /// Lex source code into an array of tokens
    // @Task return (Vec<Token>, Diagnostics)
    // @Beacon @Task disallow Indented sections inside delimited ones
    // @Beacon @Task disallow semicolons in non-delimited sections
    pub fn lex(mut self) -> Result<Outcome<Vec<Token>>> {
        while let Some(character) = self.peek() {
            let index = self.index().unwrap();
            self.local_span = LocalSpan::from(index);

            match character {
                '#' if index == LocalByteIndex::new(0) => self.lex_shebang_candidate(),
                // @Bug @Beacon if it is SOI, don't lex_whitespace but lex_indentation
                // (SOI should act as a line break)
                ' ' => self.lex_whitespace(),
                ';' => {
                    self.advance();

                    if let Some(';') = self.peek() {
                        self.lex_comment();
                    } else {
                        self.add(Semicolon(Provenance::Source));
                    }
                }
                character if token::is_identifier_segment_start(character) => {
                    self.lex_identifier()?
                }
                '\n' if self.sections.current() != Section::Delimited => self.lex_indentation()?,
                '\n' => self.advance(),
                '-' => self.lex_punctuation_or_number_literal()?,
                character if character.is_ascii_digit() => {
                    self.advance();
                    self.lex_number_literal()?
                }
                character if token::is_punctuation(character) => self.lex_punctuation(),
                '"' => self.lex_text_literal(),
                '(' => self.add_opening_bracket(Bracket::Round),
                '[' => self.add_opening_bracket(Bracket::Square),
                '{' => {
                    self.sections.enter(Section::Delimited);
                    self.add_opening_bracket(Bracket::Curly);
                }
                ')' => {
                    self.add_closing_bracket(Bracket::Round);
                }
                ']' => {
                    self.add_closing_bracket(Bracket::Square);
                }
                '}' => {
                    // @Temporary unverified
                    if self.sections.current() == Section::Delimited {
                        self.sections.exit();
                    }
                    self.add_closing_bracket(Bracket::Curly);
                }
                '\'' => {
                    self.add(SingleQuote);
                    self.advance();
                }
                character => {
                    self.take();
                    self.add(Illegal(character));
                    self.advance();
                }
            }
        }

        if !self.brackets.0.is_empty() {
            // @Task don't report all remaining brackets in the stack, only unclosed ones!
            // @Beacon @Bug we are not smart enough here yet, the error messages are too confusing
            // or even incorrect!

            for bracket in &self.brackets.0 {
                self.health.taint();
                Diagnostic::error()
                    .code(Code::E001)
                    .message(format!("unbalanced {} bracket", bracket.data))
                    .labeled_primary_span(
                        bracket,
                        format!("has no matching closing {} bracket", bracket.data),
                    )
                    .report(&self.reporter);
            }
        }

        // @Bug panics if last byte is not a valid codepoint, right?
        let last = LocalByteIndex::from_usize(self.source_file.content().len().saturating_sub(1));

        // Ideally, we'd like to set its span to the index after the last token,
        // but they way `SourceMap` is currently defined, that would not work,
        // it would reach into the next `SourceFile` if there even was one
        // I'd love to put "the red caret" in diagnostics visually after the last
        // token. However, even with fake source files as separators between real ones,
        // we would need to add a lot of complexity to be able to handle that in
        // `Diagnostic::format_for_terminal`.
        self.local_span = LocalSpan::from(last);

        for section in self.sections.exit_all() {
            if section.is_indented() {
                self.add(ClosingCurlyBracket(Provenance::Lexer));
                self.add(Semicolon(Provenance::Lexer));
            }
        }

        self.add(EndOfInput);
        Ok(self.health.of(self.tokens))
    }

    fn add_opening_bracket(&mut self, bracket: Bracket) {
        self.add(bracket.opening());
        let span = self.span();
        self.brackets.open(Spanned::new(span, bracket));
        self.advance();
    }

    fn add_closing_bracket(&mut self, bracket: Bracket) {
        if let (Section::Indented { brackets }, size) = self.sections.current_continued() {
            if self.brackets.0.len() <= brackets {
                self.add(ClosingCurlyBracket(Provenance::Lexer));
                // @Question can this panic??
                let dedentation = self.sections.0.len() - size;
                self.sections.0.truncate(size);
                self.indentation -= Indentation(dedentation);
            }
        }

        self.add(bracket.closing());
        if let Err(error) = self.brackets.close(Spanned::new(self.span(), bracket)) {
            self.health.taint();
            error.report(&self.reporter);
        };
        self.advance();
    }

    fn lex_shebang_candidate(&mut self) {
        self.advance();

        if let Some('!') = self.peek() {
            while let Some(character) = self.peek() {
                self.advance();

                if character == '\n' {
                    break;
                }
            }
        } else {
            self.lex_punctuation();
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

    // @Task merge consecutive documentation comments
    // @Task emit TokenKind::Comment if asked
    fn lex_comment(&mut self) {
        self.advance();

        let mut is_documentation = true;

        if let Some(character) = self.peek() {
            if character == ';' {
                is_documentation = false;
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

            if is_documentation {
                self.take();
            }

            self.advance();
        }

        if is_documentation {
            self.add(DocumentationComment);
        }
    }

    // @Task make trailing dashes part of punctuation (this removes the error condition)
    fn lex_identifier(&mut self) -> Result {
        self.lex_identifier_segment();
        while self.peek() == Some('-') {
            let dash = self.index().unwrap();
            self.take();
            self.advance();
            let previous = self.local_span;
            self.lex_identifier_segment();
            if self.local_span == previous {
                self.local_span = dash.into();
                Diagnostic::error()
                    .code(Code::E002)
                    .message("trailing dash on identifier")
                    .primary_span(self.span())
                    .report(&self.reporter);
                return Err(());
            }
        }

        match token::parse_keyword(self.source()) {
            Some(keyword) => self.add(keyword),
            None => {
                self.add(Identifier(self.source().into()));
            }
        };

        if self.peek() == Some('.') {
            self.local_span = LocalSpan::from(self.index().unwrap());
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
            .map_or(false, |token| token.name().introduces_indented_section());

        // squash consecutive line breaks into a single one
        self.advance();
        self.take_while(|character| character == '\n');
        self.add(Semicolon(Provenance::Lexer));

        // self.span = match self.index() {
        //     Some(index) => LocalSpan::from(index),
        //     None => return Ok(()),
        // };
        let mut spaces = Spaces(0);
        self.take_while_with(|character| character == ' ', || spaces.0 += 1);

        // if the line is empty, ignore it (important for indented comments)
        if self.line_is_empty() {
            spaces = self.indentation;
        }

        let (change, difference) = spaces.difference(self.indentation);

        // self.span = LocalSpan::new(self.span.end + 1 - absolute_difference, self.span.end);

        let difference: Indentation = match (change, difference).try_into() {
            Ok(difference) => difference,
            Err(error) => {
                // @Task push that to the error buffer instead (making this non-fatal)
                // and treat Spaces(1) as Spaces(4), Spaces(6) as Spaces(8) etc.
                Diagnostic::error()
                    .code(Code::E003)
                    .message(format!(
                        "invalid indentation consisting of {} spaces",
                        difference.0
                    ))
                    .primary_span(self.span())
                    .note(match error {
                        IndentationError::Misaligned => {
                            format!("indentation needs to be a multiple of {}", INDENTATION.0)
                        }
                        IndentationError::TooDeep => format!(
                            "indentation is greater than {} and therefore too deep",
                            INDENTATION.0
                        ),
                    })
                    .report(&self.reporter);
                return Err(());
            }
        };

        let section = self.sections.current();

        if is_start_of_indented_section {
            if change == Greater {
                // remove the line break again
                self.tokens.pop();
                // @Bug wrong span
                self.add(OpeningCurlyBracket(Provenance::Lexer));
                self.sections.enter(Section::Indented {
                    brackets: self.brackets.0.len(),
                });
            }
        } else {
            // Remove the line break again if the next line is indented or if we are in a section
            // where line breaks are not terminators or if we dedent by some amount and the
            // section reached treats line breaks as terminators.
            if change == Greater
                || change == Equal && !section.line_breaks_are_terminators()
                || change == Less
                    && !self
                        .sections
                        .get(difference.0)
                        .line_breaks_are_terminators()
            {
                // remove the line break again
                self.tokens.pop();
            }

            if change == Greater {
                self.sections.enter(Section::Continued);
            }
        };

        if change == Less {
            // Remove legal but superfluous virtual semicolons before virtual
            // closing curly brackets (which also act as terminators).
            if self.sections.current_continued().0.is_indented() {
                if self.tokens.last().map_or(false, Token::is_line_break) {
                    self.tokens.pop();
                }
            }

            for _ in 0..difference.0 {
                let section = self.sections.exit();

                if section.is_indented() {
                    // @Beacon @Task handle the case where `!section.brackets.is_empty()`

                    // @Bug wrong span
                    self.add(ClosingCurlyBracket(Provenance::Lexer));
                    self.add(Semicolon(Provenance::Lexer));
                }
            }
        }

        self.indentation = spaces;

        Ok(())
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

    fn lex_punctuation_or_number_literal(&mut self) -> Result {
        self.advance();
        if self
            .peek()
            .map(|character| character.is_ascii_digit())
            .unwrap_or(false)
        {
            self.lex_number_literal()?;
        } else {
            self.lex_punctuation();
        }
        Ok(())
    }

    fn lex_punctuation(&mut self) {
        self.take_while(token::is_punctuation);

        match token::parse_reserved_punctuation(&self.source_file[self.local_span]) {
            Some(punctuation) => self.add(punctuation),
            None => {
                self.add(Punctuation(self.source().into()));
            }
        }
    }

    // @Task move validation logic out of the lexer
    fn lex_number_literal(&mut self) -> Result {
        let mut number = self.source().to_owned();

        let mut trailing_separator = false;
        let mut consecutive_separators = false;

        while let Some(character) = self.peek() {
            if !token::is_number_literal_middle(character) {
                break;
            }
            self.take();
            self.advance();

            if character != NUMERIC_SEPARATOR {
                number.push(character);
            } else {
                if let Some(NUMERIC_SEPARATOR) = self.peek() {
                    consecutive_separators = true;
                }
                if self.peek().map_or(true, |character| {
                    !token::is_number_literal_middle(character)
                }) {
                    trailing_separator = true;
                }
            }
        }

        // @Task emit an invalid identifier token if an identifier immedially follows
        // a number literal (maybe)

        if consecutive_separators || trailing_separator {
            if consecutive_separators {
                Diagnostic::error()
                    .code(Code::E005)
                    .message("consecutive primes in number literal")
                    .primary_span(self.span())
                    .report(&self.reporter);
            }

            if trailing_separator {
                Diagnostic::error()
                    .code(Code::E005)
                    .message("trailing prime in number literal")
                    .primary_span(self.span())
                    .report(&self.reporter);
            }

            // @Task don't return early here, just taint the health and
            // return an InvalidNumberLiteral token
            return Err(());
        }

        self.add(NumberLiteral(number));

        Ok(())
    }

    // @Task escape sequences @Update do them in the parser (or at least mark them as invalid
    // and do the error reporting in the parser)
    fn lex_text_literal(&mut self) {
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
        // @Beacon @Beacon @Beacon @Note @Bug currently we cannot represent empty spans, so this trimming
        // below on empty text literals is incorrect but still if we leave out the assertions in
        // mod span, everything works out fine since interally we just use std::ops::RangeInclusive
        // which is empty by definition if start > end
        // @Note this is fixed once we change the definition of AbstractSpans to be exclusive ranges
        let content_span = self.local_span.trim_start(1).trim_end(1);
        let content = self.source_file[content_span].to_owned();
        self.add(TextLiteral(match is_terminated {
            true => Ok(content),
            false => Err(UnterminatedTextLiteral),
        }));
    }
}

impl<'a> util::lexer::Lexer<'a, TokenKind> for Lexer<'a> {
    fn source_file(&self) -> &'a SourceFile {
        &self.source_file
    }

    fn characters(&mut self) -> &mut Peekable<CharIndices<'a>> {
        &mut self.characters
    }

    fn tokens(&mut self) -> &mut Vec<Token> {
        &mut self.tokens
    }

    fn local_span(&self) -> LocalSpan {
        self.local_span
    }

    fn local_span_mut(&mut self) -> &mut LocalSpan {
        &mut self.local_span
    }
}

#[derive(Clone, Copy)]
pub struct Spaces(pub usize);

impl Spaces {
    /// Return the ordering / direction / sign and the absolute difference.
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

impl<S: Into<Spaces>> Sub<S> for Spaces {
    type Output = Self;

    fn sub(self, other: S) -> Self::Output {
        Self(self.0.saturating_sub(other.into().0))
    }
}

impl<S: Into<Spaces>> SubAssign<S> for Spaces {
    fn sub_assign(&mut self, other: S) {
        *self = *self - other
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
