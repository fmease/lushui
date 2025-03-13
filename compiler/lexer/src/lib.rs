//! The lexical analyzer (lexer).
#![feature(decl_macro, int_roundings, let_chains, stmt_expr_attributes)]

use BareToken::*;
use span::{FileName, LocalByteIndex, LocalSpan, SourceFile, SourceMap, Span, Spanned};
use std::{cmp::Ordering, mem, str::CharIndices, sync::Arc};
use token::{
    BareToken, Bracket, BracketKind, BracketOrientation, Indentation, IndentationError, Spaces,
    Token,
};
use utility::{self as _, Atom, GetFromEndExt, default};

#[cfg(test)]
mod test;

pub mod token;
// @Task move this to crate `utilities` once word parsing is independent
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
    chars: CharIndices<'a>,
    // We don't use Peekable<CharIndices<'_>> for Self::chars because that would
    // prevent us from accessing the current index at *all* times (without adding
    // extra state (which would lead to the index getting tracked twice)).
    #[allow(clippy::option_option)] // I disagree; FIXME: move into global config
    peeked: Option<Option<(usize, char)>>,
    file: &'a SourceFile,
    options: &'a Options,
    tokens: Vec<Token>,
    errors: Vec<Error>,
    // FIXME: Try to get rid of this field by making `Self::add{,_with}`
    //        take the local span as a parameter instead.
    local_span: LocalSpan,
    indentation: Spaces,
    sections: Sections,
    brackets: Brackets,
}

impl<'a> Lexer<'a> {
    fn new(file: &'a SourceFile, options: &'a Options) -> Self {
        Self {
            chars: file.content().char_indices(),
            peeked: None,
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
        while let Some(char) = self.peek() {
            let index = self.index();
            self.local_span = LocalSpan::empty(index);

            match char {
                '#' if index == default() => self.lex_shebang_candidate(char),
                ' ' if index == default() => self.lex_indentation(),
                ' ' => self.lex_whitespace(),
                ';' => {
                    self.take(char);
                    self.advance();

                    if let Some(char @ ';') = self.peek() {
                        self.lex_comment(char);
                    } else {
                        self.add(Semicolon);
                    }
                }
                char if char.is_word_segment_start() => self.lex_word(),
                '\n' => {
                    self.take(char);
                    self.advance();
                    self.lex_indentation();
                }
                '-' => self.lex_symbol_or_number_literal(char),
                char if char.is_ascii_digit() => {
                    self.take(char);
                    self.advance();
                    self.lex_number_literal();
                }
                char if char.is_symbol() => {
                    self.take(char);
                    self.advance();
                    self.lex_symbol();
                }
                '"' => self.lex_text_literal(char),
                '(' => self.add_opening_bracket(char, BracketKind::Round),
                '[' => self.add_opening_bracket(char, BracketKind::Square),
                '{' => self.add_opening_bracket(char, BracketKind::Curly),
                ')' => self.add_closing_bracket(char, BracketKind::Round),
                ']' => self.add_closing_bracket(char, BracketKind::Square),
                '}' => self.add_closing_bracket(char, BracketKind::Curly),
                ',' => self.consume(char, Comma),
                '\'' => self.consume(char, Apostrophe),
                char => {
                    self.take(char);
                    self.advance();
                    self.error(BareError::InvalidToken(char));
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
                self.add(Dedentation);
                self.add(LineBreak);
            }
        }
        self.add(EndOfInput);

        Outcome { tokens: self.tokens, errors: self.errors }
    }

    fn add_opening_bracket(&mut self, char: char, bracket: BracketKind) {
        self.take(char);
        self.add(bracket.opening());

        self.brackets.open(Spanned::new(self.span(), bracket));
        self.advance();
    }

    fn add_closing_bracket(&mut self, char: char, bracket: BracketKind) {
        self.take(char);

        if let (Section::Indented { brackets }, size) = self.sections.current_continued()
            && self.brackets.stack.len() <= brackets
        {
            self.add(Dedentation);
            // @Question can this panic??
            let dedentation = self.sections.0.len() - size;
            self.sections.0.truncate(size);
            self.indentation -= Indentation(dedentation);
        }

        self.add(bracket.closing());

        if let Err(error) = self.brackets.close(Spanned::new(self.span(), bracket)) {
            self.errors.push(error);
        }
        self.advance();
    }

    fn lex_shebang_candidate(&mut self, char: char) {
        self.take(char);
        self.advance();

        if let Some('!') = self.peek() {
            while let Some(char) = self.peek() {
                if self.options.keep_comments {
                    self.take(char);
                }
                self.advance();

                if char == '\n' {
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
        while let Some(char) = self.peek() {
            if char != ' ' {
                break;
            }
            self.advance();
        }
    }

    fn lex_comment(&mut self, char: char) {
        self.take(char);
        self.advance();
        let mut is_doc_comment = true;

        if let Some(char) = self.peek() {
            self.advance();

            if char == ';' {
                is_doc_comment = false;
            }

            if char != '\n' {
                self.take(char);

                while let Some(char) = self.peek() {
                    if char == '\n' {
                        self.advance();
                        break;
                    }

                    if is_doc_comment || self.options.keep_comments {
                        self.take(char);
                    }

                    self.advance();
                }
            }
        }

        if is_doc_comment {
            self.add(DocumentationComment);
        } else if self.options.keep_comments {
            self.add(Comment);
        }
    }

    fn lex_word(&mut self) {
        self.lex_first_word_segment();
        while let Some(char @ '-') = self.peek() {
            self.take(char);
            self.advance();
            self.lex_word_segment();
        }

        self.add(match self.source().into() {
            Atom::UNDERSCORE => Underscore,
            Atom::AS => As,
            Atom::CASE => Case,
            Atom::DATA => Data,
            Atom::DO => Do,
            Atom::EXTERN => Extern,
            Atom::FOR_LOWER => ForLower,
            Atom::FOR_UPPER => ForUpper,
            Atom::GIVEN => Given,
            Atom::IN => In,
            Atom::LET => Let,
            Atom::MODULE => Module,
            Atom::OF => Of,
            Atom::RECORD => Record,
            Atom::SELF => Self_,
            Atom::SUPER => Super,
            Atom::TOPMOST => Topmost,
            Atom::TRAIT => Trait,
            Atom::USE => Use,
            word => Word(word),
        });

        if let Some(char @ '.') = self.peek() {
            #[allow(clippy::cast_possible_truncation)] // always within 1..=4
            let length = char.len_utf8() as u32;
            self.local_span = LocalSpan::with_length(self.index(), length);
            self.add(Dot);
            self.advance();
        }
    }

    fn lex_first_word_segment(&mut self) {
        if let Some(char) = self.peek()
            && char.is_word_segment_start()
        {
            self.take(char);
            self.advance();
            self.take_while(char::is_word_segment_middle);
        }
    }

    fn lex_word_segment(&mut self) {
        self.take_while(char::is_word_segment_middle);
    }

    // @Task recover from tabs (treat them as 4 spaces) and emit a custom error
    fn lex_indentation(&mut self) {
        let is_start_of_indented_section =
            self.tokens.last().is_some_and(|token| token.bare.introduces_indented_section());

        // Squash consecutive line breaks into a single one.
        // This leads to more legible and fewer diagnostics later in the parser in case the
        // line break wasn't expected. Further, it might lead to less churn in the parser.
        self.take_while(|char| char == '\n');
        // Tentatively register the line break.
        // If certain conditions are met later on (*), we will remove it again.
        self.add(LineBreak);
        let mut has_removed_line_break = false;

        self.local_span = LocalSpan::empty(self.index());

        let mut spaces = Spaces(0);
        self.take_while_with(|char| char == ' ', || spaces.0 += 1);

        // If the line is empty ignore it. While most places in the parser can deal with
        // “empty declarations”, in the future (once `line_is_empty` can detect comments
        // as well) we might want to simplify the parser by throwing away the special
        // handling (i.e. skipping) of empty lines.
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
                // (*) Remove the line break again.
                self.tokens.pop();
                has_removed_line_break = true;
                self.add(BareToken::Indentation);
                self.sections.enter(Section::Indented { brackets: self.brackets.stack.len() });
            }
        } else {
            // Remove the line break again if the next line is indented or if we are in a section
            // where line breaks are not terminators or if we dedent by some amount and the section
            // reached treats line breaks as terminators.
            if change == Ordering::Greater
                || change == Ordering::Equal && !section.line_breaks_are_terminators()
                || change == Ordering::Less
                    && !self.sections.get(indentation.0).line_breaks_are_terminators()
            {
                // (*) Remove the line break again.
                self.tokens.pop();
                has_removed_line_break = true;
            }

            if change == Ordering::Greater {
                self.sections.enter(Section::Continued);
            }
        }

        if change == Ordering::Less {
            // Remove syntactically legal but superfluous line breaks that
            // come before dedendentation (which also act as terminators).
            if self.sections.current_continued().0.is_indented()
                && let Some(Spanned!(_, BareToken::LineBreak)) = self.tokens.last()
            {
                // (*) Remove the line break again.
                self.tokens.pop();
                has_removed_line_break = true;
            }

            for _ in 0..indentation.0 {
                let section = self.sections.exit();

                // @Task handle the case where `!section.brackets.is_empty()`
                if section.is_indented() {
                    self.add(Dedentation);
                    self.add(LineBreak);
                }
            }
        }

        self.indentation = spaces;

        // @Task only remove the line break if the indentation of the closing bracket is
        // greater or equal to the one of the corresponding opening bracket.
        // @Note actually it would be even better if we could emit a nice diagnostic for
        // closing brackets that are dedented too far.
        if let Some(')' | ']' | '}') = self.peek()
            && !has_removed_line_break
        {
            // (*) Remove the line break again.
            self.tokens.pop();
        }
    }

    fn line_is_empty(&mut self) -> bool {
        self.peek() == Some('\n')
        // @Bug has unforseen consequences (investigation needed)
        // || self.peek() == Some(';') && {
        //     let mut chars = self.chars.clone();
        //     chars.next();
        //     matches!(chars.next(), Some((_, ';')))
        //         && matches!(chars.next(), Some((_, ';')))
        // }
    }

    fn lex_symbol_or_number_literal(&mut self, char: char) {
        self.take(char);
        self.advance();

        match self.peek() {
            Some(char) if char.is_ascii_digit() => self.lex_number_literal(),
            _ => self.lex_symbol(),
        }
    }

    fn lex_symbol(&mut self) {
        self.take_while(char::is_symbol);

        self.add(match self.source() {
            "." => Dot,
            ":" => Colon,
            "=" => Equals,
            "?" => QuestionMark,
            "@" => At,
            "->" => ThinArrowRight,
            "<-" => ThinArrowLeft,
            "=>" => WideArrowRight,
            "::" => DoubleColon,
            "::=" => DoubleColonEquals,
            "**" => DoubleAsterisk,
            symbol => Symbol(symbol.into()),
        });
    }

    fn lex_number_literal(&mut self) {
        let mut number = self.source().to_owned();

        while let Some(char) = self.peek() {
            if !char.is_number_literal_middle() {
                break;
            }
            self.take(char);
            self.advance();

            if char != char::NUMERIC_SEPARATOR {
                number.push(char);
            }
        }

        self.add(NumberLiteral(number.into()));
    }

    // @Task escape sequences
    fn lex_text_literal(&mut self, char: char) {
        self.take(char);
        self.advance();

        let mut is_terminated = false;

        const QUOTE: char = '"';
        while let Some(char) = self.peek() {
            self.take(char);
            self.advance();

            if char == QUOTE {
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
        if self.peeked.take().is_none() {
            self.chars.next();
        }
    }

    fn index(&self) -> LocalByteIndex {
        let index = match self.peeked {
            Some(Some((index, _))) => index,
            _ => self.chars.offset(),
        };
        index.try_into().unwrap()
    }

    /// Include the span of the current token in the span of the token-to-be-added.
    ///
    /// Preparation for [`Self::add`] and variants.
    fn take(&mut self, char: char) {
        self.local_span.set_end(self.index() + char);
    }

    fn peek(&mut self) -> Option<char> {
        self.peeked.get_or_insert_with(|| self.chars.next()).map(|(_, char)| char)
    }

    /// [Take](Self::take) the span of all succeeding tokens where the predicate holds and step.
    fn take_while(&mut self, predicate: fn(char) -> bool) {
        self.take_while_with(predicate, || ());
    }

    /// [Take](Self::take) the span of all succeeding tokens where the predicate holds, step and perform the given action.
    fn take_while_with(&mut self, predicate: fn(char) -> bool, mut action: impl FnMut()) {
        while let Some(char) = self.peek() {
            if !predicate(char) {
                break;
            }
            self.take(char);
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

    fn consume(&mut self, char: char, token: BareToken) {
        self.take(char);
        self.advance();
        self.add(token);
    }
}

fn lex_string(source: String) -> Outcome {
    let mut map = SourceMap::default();
    let file = map.add(FileName::Anonymous, Arc::new(source), None);
    Lexer::new(&map[file], &Options { keep_comments: true }).lex()
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
enum Section {
    /// The top-level section.
    ///
    /// Basically the same as [`Section::Indented`] except that it does not call
    /// for adding any dendentation tokens since the indentation level is zero.
    #[default]
    Top,
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

    fn exit_all(&mut self) -> impl Iterator<Item = Section> + use<> {
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

pub trait CharExt: Copy {
    const NUMERIC_SEPARATOR: Self;

    fn is_word_segment_start(self) -> bool;
    fn is_word_segment_middle(self) -> bool;
    fn is_number_literal_middle(self) -> bool;
    fn is_symbol(self) -> bool;
}

impl CharExt for char {
    const NUMERIC_SEPARATOR: Self = '\'';

    fn is_word_segment_start(self) -> bool {
        self.is_ascii_alphabetic() || self == '_'
    }

    fn is_word_segment_middle(self) -> bool {
        self.is_ascii_alphanumeric() || self == '_'
    }

    fn is_number_literal_middle(self) -> bool {
        self.is_ascii_digit() || self == Self::NUMERIC_SEPARATOR
    }

    fn is_symbol(self) -> bool {
        #[rustfmt::skip]
        matches!(
            self,
            '.' | ':' | '+' | '-' | '~' | '=' | '<' | '>' | '*' | '^' |
            '!' | '?' | '|' | '/' | '\\' | '&' | '#' | '%' | '$' | '@'
        )
    }
}

pub type Error = Spanned<BareError>;

#[derive(PartialEq, Eq, Debug)]
pub enum BareError {
    InvalidIndentation(Spaces, IndentationError),
    InvalidToken(char),
    UnbalancedBracket(Bracket),
    UnterminatedTextLiteral,
}
