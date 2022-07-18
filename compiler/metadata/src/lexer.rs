use derivation::Discriminant;
use diagnostics::Diagnostic;
use error::{Health, Outcome};
use span::{LocalByteIndex, LocalSpan, SourceFile, Span, Spanned};
use std::{fmt, iter::Peekable, str::CharIndices};
use utilities::{obtain, quoted};
use BareToken::*;

pub type Token = Spanned<BareToken>;

pub trait TokenExt {
    fn name(&self) -> TokenName;
    fn into_identifier(self) -> Option<String>;
    fn into_text(self) -> Option<Result<String, Diagnostic>>;
    fn into_integer(self) -> Option<Result<i64, IntLexingError>>;
}

impl TokenExt for Token {
    fn name(&self) -> TokenName {
        self.bare.name()
    }

    fn into_identifier(self) -> Option<String> {
        obtain!(self.bare, Identifier(identifier) => identifier)
    }

    fn into_text(self) -> Option<Result<String, Diagnostic>> {
        match self.bare {
            Text(text) => Some(match text {
                Ok(content) => Ok(content),
                // @Task code
                Err(TextLexingError::Unterminated) => Err(Diagnostic::error()
                    .message("unterminated text literal")
                    .primary_span(self.span)),
            }),
            _ => None,
        }
    }

    // @Task turn this into a diagnostic here
    fn into_integer(self) -> Option<Result<i64, IntLexingError>> {
        obtain!(self.bare, Integer(integer) => integer)
    }
}

#[derive(Clone, Debug, Discriminant)]
#[discriminant(name: TokenName)]
pub enum BareToken {
    Comma,
    Colon,
    OpeningSquareBracket,
    ClosingSquareBracket,
    OpeningCurlyBracket,
    ClosingCurlyBracket,
    False,
    True,
    Identifier(String),
    Text(Result<String, TextLexingError>),
    Integer(Result<i64, IntLexingError>),
    EndOfInput,
    Illegal(char),
}

impl fmt::Display for BareToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.name();

        match self {
            &Self::Illegal(character) => {
                write!(f, "{} U+{:04X} ‘{}’", name, character as u32, character)
            }
            _ => write!(f, "{name}"),
        }
    }
}

impl fmt::Display for TokenName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenName::*;

        f.write_str(match self {
            Comma => quoted!(","),
            Colon => quoted!(":"),
            OpeningSquareBracket => quoted!("["),
            ClosingSquareBracket => quoted!("]"),
            OpeningCurlyBracket => quoted!("{"),
            ClosingCurlyBracket => quoted!("}"),
            False => "keyword ‘false’",
            True => "keyword ‘true’",
            Identifier => "identifier",
            Text => "text",
            Integer => "integer",
            EndOfInput => "end of input",
            Illegal => "illegal character",
        })
    }
}

pub(super) struct Lexer<'a> {
    file: &'a SourceFile,
    characters: Peekable<CharIndices<'a>>,
    tokens: Vec<Token>,
    local_span: LocalSpan,
    health: Health,
}

impl<'a> Lexer<'a> {
    pub(super) fn new(file: &'a SourceFile) -> Self {
        Self {
            characters: file.content().char_indices().peekable(),
            file,
            tokens: Vec::new(),
            local_span: LocalSpan::default(),
            health: Health::Untainted,
        }
    }

    /// Lex source code into an array of tokens.
    ///
    /// The health of the tokens can be ignored if the tokens are fed into the parser
    /// immediately after lexing since the parser will handle invalid tokens.
    pub(super) fn lex(mut self) -> Outcome<Vec<Token>> {
        while let Some((index, character)) = self.peek_with_index() {
            self.local_span = LocalSpan::empty(index);

            match character {
                '#' => self.lex_comment(),
                character if character.is_ascii_whitespace() => self.lex_whitespace(),
                '"' => self.lex_text(),
                character if is_identifier_segment_start(character) => self.lex_identifier(),
                character if character.is_ascii_digit() => self.lex_number(),
                '-' => todo!(),
                '[' => self.consume(OpeningSquareBracket),
                ']' => self.consume(ClosingSquareBracket),
                '{' => self.consume(OpeningCurlyBracket),
                '}' => self.consume(ClosingCurlyBracket),
                ',' => self.consume(Comma),
                ':' => self.consume(Colon),
                character => {
                    self.consume(Illegal(character));
                    self.health.taint();
                }
            }
        }

        self.local_span = self.file.local_span().end();
        self.add(EndOfInput);
        Outcome::new(self.tokens, self.health)
    }

    fn lex_comment(&mut self) {
        self.advance();

        while let Some(character) = self.peek() {
            self.advance();

            if character == '\n' {
                break;
            }
        }
    }

    fn lex_whitespace(&mut self) {
        self.advance();
        while let Some(character) = self.peek() {
            if !character.is_ascii_whitespace() {
                break;
            }
            self.advance();
        }
    }

    fn lex_text(&mut self) {
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

        match is_terminated {
            true => {
                // @Note once we implement escaping, this won't cut it and we need to build our own string
                let content = self.file[self.local_span.trim(1)].to_owned();

                self.add(Text(Ok(content)));
            }
            false => {
                self.health.taint();
                self.add(Text(Err(TextLexingError::Unterminated)));
            }
        };
    }

    fn lex_identifier(&mut self) {
        self.lex_identifier_segment();

        while self.peek() == Some('-') {
            let dash = self.index().unwrap();
            self.take();
            self.advance();
            let previous = self.local_span;
            self.lex_identifier_segment();
            if self.local_span == previous {
                self.local_span = LocalSpan::with_length(dash, 1);
                // @Task add illegal
                todo!();
            }
        }

        self.add(match self.source() {
            "false" => False,
            "true" => True,
            identifier => Identifier(identifier.to_owned()),
        });
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

    fn lex_number(&mut self) {
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
                if self.peek().map_or(true, |character| {
                    !character.is_ascii_digit() || character == NUMERIC_SEPARATOR
                }) {
                    trailing_separator = true;
                }
            }
        }

        if consecutive_separators {
            self.health.taint();
            self.add(Integer(Err(IntLexingError::ConsecutiveSeparators)));
        } else if trailing_separator {
            self.health.taint();
            self.add(Integer(Err(IntLexingError::TrailingSeparators)));
        } else {
            match number.parse::<i64>() {
                Ok(number) => {
                    self.add(Integer(Ok(number)));
                }
                Err(_) => {
                    self.health.taint();
                    self.add(Integer(Err(IntLexingError::SizeExceedance)));
                }
            };
        }
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

#[derive(Clone, Copy, Debug)]
pub enum TextLexingError {
    Unterminated,
}

#[derive(Clone, Copy, Debug)]
pub enum IntLexingError {
    ConsecutiveSeparators,
    TrailingSeparators,
    SizeExceedance,
}

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
