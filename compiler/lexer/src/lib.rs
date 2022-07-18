//! The lexical analyzer (lexer).
#![feature(default_free_fn)]

use diagnostics::{reporter::ErasedReportedError, Diagnostic, ErrorCode, Reporter};
use error::{Health, Outcome, Result};
use span::{LocalByteIndex, LocalSpan, SourceFile, SourceMap, Span, Spanned};
use std::{cmp::Ordering, default::default, fmt, iter::Peekable, str::CharIndices, sync::Arc};
use token::{
    BareToken, Indentation, IndentationError, Provenance, Spaces, Token, TokenExt,
    UnterminatedTextLiteral, Word, INDENTATION,
};
use utilities::{self, obtain, GetFromEndExt};
use BareToken::*;

#[cfg(test)]
mod test;

pub fn lex(file: &SourceFile, reporter: &Reporter) -> Result<Outcome<Vec<Token>>> {
    Lexer::new(file, reporter).lex()
}

fn lex_string(source: String) -> Result<Outcome<Vec<Token>>, ()> {
    let mut map = SourceMap::default();
    let file = map.add(None, Arc::new(source), None);
    Lexer::new(&map[file], &Reporter::silent())
        .lex()
        .map_err(drop)
}

pub trait WordExt: Sized {
    fn parse(name: String) -> Result<Self, ()>;
}

impl WordExt for Word {
    fn parse(name: String) -> Result<Self, ()> {
        let Outcome!(tokens, health) = lex_string(name)?;

        if health.is_tainted() {
            return Err(());
        }

        let mut tokens = tokens.into_iter().map(|token| token.bare);

        obtain!(
            (tokens.next().ok_or(())?, tokens.next().ok_or(())?),
            (BareToken::Word(atom), BareToken::EndOfInput) => atom
        )
        .map(Self::new_unchecked)
        .ok_or(())
    }
}

/// The state of the lexer.
struct Lexer<'a> {
    file: &'a SourceFile,
    characters: Peekable<CharIndices<'a>>,
    tokens: Vec<Token>,
    local_span: LocalSpan,
    indentation: Spaces,
    sections: Sections,
    brackets: Brackets,
    reporter: &'a Reporter,
    health: Health,
}

impl<'a> Lexer<'a> {
    fn new(file: &'a SourceFile, reporter: &'a Reporter) -> Self {
        Self {
            characters: file.content().char_indices().peekable(),
            file,
            tokens: Vec::new(),
            local_span: LocalSpan::default(),
            indentation: Spaces(0),
            sections: Sections::default(),
            brackets: Brackets::default(),
            reporter,
            health: Health::Untainted,
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
                character if token::is_punctuation(character) => {
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
                    .code(ErrorCode::E044)
                    .message(format!("unbalanced {} bracket", bracket.bare))
                    .labeled_primary_span(
                        bracket,
                        format!("has no matching closing {} bracket", bracket.bare),
                    )
                    .report(self.reporter);
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
                return Err(Diagnostic::error()
                    .code(ErrorCode::E045)
                    .message("trailing dash on identifier")
                    .primary_span(self.span())
                    .report(self.reporter));
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

    // @Beacon @Bug *very* confusing indentation errors if the file contains tabs \t
    // which do *not* result in an error in the lexer but "later" in the parser (which is never reached)
    // @Task treat tabs differently when calculating indentation
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
            .map_or_else(|| self.file.local_span().end(), LocalSpan::empty);

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
                return Err(Diagnostic::error()
                    .code(ErrorCode::E046)
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
                    .report(self.reporter));
            }
        };

        let section = self.sections.current();

        if is_start_of_indented_section {
            if change == Ordering::Greater {
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
            if change == Ordering::Greater
                || change == Ordering::Equal && !section.line_breaks_are_terminators()
                || change == Ordering::Less
                    && !self
                        .sections
                        .get(difference.0)
                        .line_breaks_are_terminators()
            {
                // remove the line break again
                self.tokens.pop();
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
        self.take_while(token::is_punctuation);

        match parse_reserved_punctuation(&self.file[self.local_span]) {
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
                    .code(ErrorCode::E005)
                    .message("consecutive primes in number literal")
                    .primary_span(self.span())
                    .report(self.reporter);
            }

            if trailing_separator {
                Diagnostic::error()
                    .code(ErrorCode::E005)
                    .message("trailing prime in number literal")
                    .primary_span(self.span())
                    .report(self.reporter);
            }

            // @Task don't return early here, just taint the health and
            // return an InvalidNumberLiteral token
            // @Task don't use unchecked here, smh get the token from above
            return Err(ErasedReportedError::new_unchecked());
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
            true => Ok(self.file[content_span].into()),
            false => Err(UnterminatedTextLiteral),
        }));
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

    fn consume(&mut self, token: BareToken) {
        self.take();
        self.advance();
        self.add(token);
    }
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
    const fn opening(self) -> BareToken {
        match self {
            Self::Round => OpeningRoundBracket,
            Self::Square => OpeningSquareBracket,
            Self::Curly => OpeningCurlyBracket(Provenance::Source),
        }
    }

    const fn closing(self) -> BareToken {
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
                if opening_bracket.bare == closing_bracket.bare {
                    Ok(())
                } else {
                    // @Beacon @Bug we are not smart enough here yet, the error messages are too confusing
                    // or even incorrect!
                    Err(Diagnostic::error()
                        .code(ErrorCode::E044)
                        .message(format!("unbalanced {closing_bracket} bracket"))
                        .labeled_primary_span(
                            closing_bracket,
                            format!("has no matching opening {closing_bracket} bracket"),
                        ))
                }
            }
            // @Beacon @Bug we are not smart enough here yet, the error messages are too confusing
            // or even incorrect!
            None => Err(Diagnostic::error()
                .code(ErrorCode::E044)
                .message(format!("unbalanced {closing_bracket} bracket"))
                .labeled_primary_span(
                    closing_bracket,
                    format!("has no matching opening {closing_bracket} bracket"),
                )),
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

fn parse_keyword(source: &str) -> Option<BareToken> {
    Some(match source {
        "_" => Underscore,
        "as" => As,
        "topmost" => Topmost,
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

fn parse_reserved_punctuation(source: &str) -> Option<BareToken> {
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
