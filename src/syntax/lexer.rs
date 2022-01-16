//! The lexical analyzer (lexer).

#[cfg(test)]
mod test;

use super::token::{Provenance, Token, TokenKind, UnterminatedTextLiteral};
use crate::{
    diagnostics::{reporter::SilentReporter, Code, Diagnostic, Reporter},
    error::{Health, Outcome, Result},
    span::{LocalSpan, SourceFile, SourceMap, Spanned},
    utility::{self, lexer::Lexer as _, GetFromEndExt},
};
use std::{
    cmp::Ordering::{self, *},
    default::default,
    fmt,
    iter::Peekable,
    ops::{Sub, SubAssign},
    str::CharIndices,
};
use TokenKind::*;

pub(crate) fn lex_string(source: String) -> Result<Outcome<Vec<Token>>> {
    let mut map = SourceMap::default();
    let file = map.add(None, source);
    Lexer::new(&map[file], &SilentReporter.into()).lex()
}

/// The unit indentation in spaces.
pub(crate) const INDENTATION: Spaces = Indentation::UNIT.to_spaces();

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
                if opening_bracket.value == closing_bracket.value {
                    Ok(())
                } else {
                    // @Beacon @Bug we are not smart enough here yet, the error messages are too confusing
                    // or even incorrect!
                    Err(Diagnostic::error()
                        .code(Code::E001)
                        .message(format!("unbalanced {} bracket", closing_bracket.value))
                        .labeled_primary_span(
                            closing_bracket,
                            format!("has no matching opening {} bracket", closing_bracket.value),
                        ))
                }
            }
            // @Beacon @Bug we are not smart enough here yet, the error messages are too confusing
            // or even incorrect!
            None => Err(Diagnostic::error()
                .code(Code::E001)
                .message(format!("unbalanced {} bracket", closing_bracket.value))
                .labeled_primary_span(
                    closing_bracket,
                    format!("has no matching opening {} bracket", closing_bracket.value),
                )),
        }
    }
}

type Stack<T> = Vec<T>;

/// Lex source code into an array of tokens
///
/// The health of the tokens can be ignored if the tokens are fed into the parser
/// immediately after lexing since the parser will handle invalid tokens.
pub fn lex(source_file: &SourceFile, reporter: &Reporter) -> Result<Outcome<Vec<Token>>> {
    Lexer::new(source_file, reporter).lex()
}

/// The state of the lexer.
struct Lexer<'a> {
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
    fn new(source_file: &'a SourceFile, reporter: &'a Reporter) -> Self {
        Self {
            characters: source_file.content().char_indices().peekable(),
            source_file,
            tokens: Vec::new(),
            local_span: LocalSpan::default(),
            indentation: Spaces(0),
            sections: Sections::default(),
            brackets: Brackets::default(),
            health: Health::Untainted,
            reporter,
        }
    }

    // @Task return (Vec<Token>, Diagnostics)
    // @Task improve diagnostics when encountering "indented sections" inside delimited ones
    fn lex(mut self) -> Result<Outcome<Vec<Token>>> {
        while let Some((index, character)) = self.peek_with_index() {
            self.local_span = LocalSpan::empty(index);

            match character {
                '#' if index == default() => self.lex_shebang_candidate(),
                // @Bug @Beacon if it is SOI, don't lex_whitespace but lex_indentation
                // (SOI should act as a line break)
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
                character if is_identifier_segment_start(character) => self.lex_identifier()?,
                '\n' if self.sections.current() != Section::Delimited => self.lex_indentation()?,
                '\n' => self.advance(),
                '-' => self.lex_punctuation_or_number_literal()?,
                character if character.is_ascii_digit() => {
                    self.take();
                    self.advance();
                    self.lex_number_literal()?;
                }
                character if is_punctuation(character) => {
                    self.take();
                    self.advance();

                    self.lex_punctuation();
                }
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
                '\'' => self.consume(Apostrophe),
                character => {
                    self.consume(Illegal(character));
                    self.health.taint();
                }
            }
        }

        if !self.brackets.0.is_empty() {
            // @Task don't report all remaining brackets in the stack, only unclosed ones! See #113.
            // @Bug we are not smart enough here yet, the error messages are too confusing / even incorrect!

            for bracket in &self.brackets.0 {
                self.health.taint();
                Diagnostic::error()
                    .code(Code::E001)
                    .message(format!("unbalanced {} bracket", bracket.value))
                    .labeled_primary_span(
                        bracket,
                        format!("has no matching closing {} bracket", bracket.value),
                    )
                    .report(self.reporter);
            }
        }

        self.local_span = self.source_file.local_span().end();
        for section in self.sections.exit_all() {
            if section.is_indented() {
                self.add(ClosingCurlyBracket(Provenance::Lexer));
                self.add(Semicolon(Provenance::Lexer));
            }
        }
        self.add(EndOfInput);

        Ok(Outcome::new(self.tokens, self.health))
    }

    fn add_opening_bracket(&mut self, bracket: Bracket) {
        self.take();
        self.add(bracket.opening());

        self.brackets.open(Spanned::new(self.span(), bracket));
        self.advance();
    }

    fn add_closing_bracket(&mut self, bracket: Bracket) {
        self.take();

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
            error.report(self.reporter);
        };
        self.advance();
    }

    fn lex_shebang_candidate(&mut self) {
        self.take();
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

    // @Task merge consecutive documentation comments, smh
    // @Task emit TokenKind::Comment if asked
    fn lex_comment(&mut self) {
        self.take();
        self.advance();

        let mut is_documentation = true;
        let mut found_immediate_line_break = false;

        if let Some(character) = self.peek() {
            self.advance();

            match character {
                ';' => is_documentation = false,
                '\n' => found_immediate_line_break = true,
                _ => self.take(),
            }
        }

        if !found_immediate_line_break {
            while let Some(character) = self.peek() {
                if character == '\n' {
                    self.advance();
                    break;
                }

                if is_documentation {
                    self.take();
                }

                self.advance();
            }
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
                self.local_span = LocalSpan::with_length(dash, 1);
                Diagnostic::error()
                    .code(Code::E002)
                    .message("trailing dash on identifier")
                    .primary_span(self.span())
                    .report(self.reporter);
                return Err(());
            }
        }

        match parse_keyword(self.source()) {
            Some(keyword) => self.add(keyword),
            None => {
                self.add(Word(self.source().into()));
            }
        };

        if let Some((index, '.')) = self.peek_with_index() {
            self.local_span = LocalSpan::with_length(index, 1);
            self.add(Dot);
            self.advance();
        }

        Ok(())
    }

    fn lex_identifier_segment(&mut self) {
        if let Some(character) = self.peek() {
            if is_identifier_segment_start(character) {
                self.take();
                self.advance();
                self.take_while(is_identifier_segment_middle);
            }
        }
    }

    fn lex_indentation(&mut self) -> Result {
        let is_start_of_indented_section = self
            .tokens
            .last()
            .map_or(false, |token| token.name().introduces_indented_section());

        // squash consecutive line breaks into a single one
        self.take();
        self.advance();
        self.take_while(|character| character == '\n');
        // might be removed again in certain conditions later on
        self.add(Semicolon(Provenance::Lexer));

        self.local_span = self
            .index()
            .map_or_else(|| self.source_file.local_span().end(), LocalSpan::empty);

        let mut spaces = Spaces(0);
        self.take_while_with(|character| character == ' ', || spaces.0 += 1);

        // if the line is empty, ignore it (important for indented comments)
        if self.line_is_empty() {
            spaces = self.indentation;
        }

        let (change, difference) = spaces.difference(self.indentation);

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
                    .report(self.reporter);
                return Err(());
            }
        };

        let section = self.sections.current();

        if is_start_of_indented_section {
            if change == Greater {
                // remove the line break again
                self.tokens.pop();
                self.add(OpeningCurlyBracket(Provenance::Lexer));
                self.sections.enter(Section::Indented {
                    brackets: self.brackets.0.len(),
                });
            }
        } else {
            // Remove the line break again if the next line is indented or if we are in a section
            // where line breaks ((virtual) semicolons) are not terminators (which include semicolons)
            // or if we dedent by some amount and the section reached treats line breaks as terminators.
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
            // Remove syntactically legal but superfluous virtual semicolons before virtual
            // closing curly brackets (which also act as terminators).
            if self.sections.current_continued().0.is_indented()
                && self.tokens.last().map_or(false, Token::is_line_break)
            {
                self.tokens.pop();
            }

            for _ in 0..difference.0 {
                let section = self.sections.exit();

                // @Beacon @Task handle the case where `!section.brackets.is_empty()`
                if section.is_indented() {
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
        self.take();
        self.advance();

        match self.peek() {
            Some(character) if character.is_ascii_digit() => self.lex_number_literal()?,
            _ => self.lex_punctuation(),
        }

        Ok(())
    }

    fn lex_punctuation(&mut self) {
        self.take_while(is_punctuation);

        match parse_reserved_punctuation(&self.source_file[self.local_span]) {
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
            if !is_number_literal_middle(character) {
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
                if self
                    .peek()
                    .map_or(true, |character| !is_number_literal_middle(character))
                {
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
                    .report(self.reporter);
            }

            if trailing_separator {
                Diagnostic::error()
                    .code(Code::E005)
                    .message("trailing prime in number literal")
                    .primary_span(self.span())
                    .report(self.reporter);
            }

            // @Task don't return early here, just taint the health and
            // return an InvalidNumberLiteral token
            return Err(());
        }

        self.add(NumberLiteral(number.into()));

        Ok(())
    }

    // @Task escape sequences @Update do them in the parser (or at least mark them as invalid
    // and do the error reporting in the parser)
    fn lex_text_literal(&mut self) {
        self.take();
        self.advance();

        let mut is_terminated = false;

        while let Some(character) = self.peek() {
            self.take();
            self.advance();

            if character == '"' {
                is_terminated = true;
                break;
            }
        }

        // @Note once we implement escaping, this won't cut it and we need to build our own string
        let content_span = self.local_span.trim(1);
        self.add(TextLiteral(match is_terminated {
            true => Ok(self.source_file[content_span].into()),
            false => Err(UnterminatedTextLiteral),
        }));
    }
}

impl<'a> utility::lexer::Lexer<'a, TokenKind> for Lexer<'a> {
    fn source_file(&self) -> &'a SourceFile {
        self.source_file
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

pub(crate) const fn is_punctuation(character: char) -> bool {
    #[rustfmt::skip]
    matches!(
        character,
        '.' | ':' | '+' | '-' | '~' | '=' | '<' | '>' | '*' | '^' |
        '!' | '?' | '|' | '/' | '\\' | '&' | '#' | '%' | '$' | '@'
    )
}

pub(crate) const fn is_identifier_segment_start(character: char) -> bool {
    character.is_ascii_alphabetic() || character == '_'
}

pub(crate) const fn is_identifier_segment_middle(character: char) -> bool {
    character.is_ascii_alphanumeric() || character == '_'
}

pub(crate) const NUMERIC_SEPARATOR: char = '\'';

pub(crate) const fn is_number_literal_middle(character: char) -> bool {
    character.is_ascii_digit() || character == NUMERIC_SEPARATOR
}

fn parse_keyword(source: &str) -> Option<TokenKind> {
    Some(match source {
        "_" => Underscore,
        "as" => As,
        "capsule" => Capsule,
        "case" => Case,
        "extern" => Extern,
        "data" => Data,
        "do" => Do,
        "in" => In,
        "lazy" => Lazy,
        "let" => Let,
        "module" => Module,
        "of" => Of,
        "self" => Self_,
        "super" => Super,
        "Type" => Type,
        "use" => Use,
        _ => return None,
    })
}

fn parse_reserved_punctuation(source: &str) -> Option<TokenKind> {
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
        _ => return None,
    })
}

#[derive(Clone, Copy)]
pub(crate) struct Spaces(pub(crate) usize);

impl Spaces {
    /// Return the ordering / direction / sign and the absolute difference.
    pub(crate) fn difference(self, other: Self) -> (Ordering, Self) {
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
        *self = *self - other;
    }
}

#[derive(Clone, Copy)]
pub(crate) struct Indentation(pub(crate) usize);

impl Indentation {
    pub(crate) const UNIT: Self = Self(1);

    pub(crate) const fn to_spaces(self) -> Spaces {
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

pub(crate) enum IndentationError {
    Misaligned,
    TooDeep,
}
