use derivation::Discriminant;
use diagnostics::Diagnostic;
use span::{LocalByteIndex, LocalSpan, SourceFile, Span, Spanned};
use std::{fmt, iter::Peekable, num::ParseIntError, str::CharIndices};
use utilities::{obtain, quoted};
use BareToken::*;

// @Task add "unbalanced bracket" errors
pub fn lex(file: &SourceFile, options: &Options) -> Outcome {
    Lexer::new(file, options).lex()
}

#[derive(Default)]
pub struct Options {
    pub keep_comments: bool,
}

pub struct Outcome {
    pub tokens: Vec<Token>,
    pub errors: Vec<Error>,
}

struct Lexer<'a> {
    characters: Peekable<CharIndices<'a>>,
    file: &'a SourceFile,
    options: &'a Options,
    tokens: Vec<Token>,
    errors: Vec<Error>,
    local_span: LocalSpan,
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
        }
    }

    fn lex(mut self) -> Outcome {
        while let Some((index, character)) = self.peek_with_index() {
            self.local_span = LocalSpan::empty(index);

            match character {
                '#' => self.lex_comment(),
                character if character.is_ascii_whitespace() => self.lex_whitespace(),
                '"' => self.lex_text(),
                character if is_identifier_segment_start(character) => self.lex_identifier(),
                character if character.is_ascii_digit() => self.lex_number(),
                '-' => self.lex_number(),
                '[' => self.consume(OpeningSquareBracket),
                ']' => self.consume(ClosingSquareBracket),
                '{' => self.consume(OpeningCurlyBracket),
                '}' => self.consume(ClosingCurlyBracket),
                ',' => self.consume(Comma),
                ':' => self.consume(Colon),
                character => {
                    self.take();
                    self.advance();
                    let token = Spanned::new(self.span(), character);
                    self.errors.push(Error::IllegalToken(token));
                }
            }
        }

        self.local_span = self.file.local_span().end();
        self.add(EndOfInput);

        Outcome {
            tokens: self.tokens,
            errors: self.errors,
        }
    }

    fn lex_comment(&mut self) {
        self.advance();

        while let Some(character) = self.peek() {
            self.advance();

            if character == '\n' {
                break;
            }

            if self.options.keep_comments {
                self.take();
            }
        }

        if self.options.keep_comments {
            self.add(Comment);
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

        // @Note this naive trimming and indexing into the source file does not scale to
        //       escape sequences (once we implement them)
        let content_span = if is_terminated {
            self.local_span.trim(1)
        } else {
            self.errors.push(Error::UnterminatedText(self.span()));
            self.local_span.trim_start(1)
        };

        self.add(Text(self.file[content_span].into()));
    }

    fn lex_identifier(&mut self) {
        self.lex_first_identifier_segment();

        while self.peek() == Some('-') {
            self.take();
            self.advance();
            self.lex_identifier_segment();
        }

        self.add(match self.source() {
            "false" => False,
            "true" => True,
            identifier => Identifier(identifier.to_owned()),
        });
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

    fn lex_number(&mut self) {
        self.take();
        self.advance();
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

        match number.parse() {
            Ok(number) => {
                self.add(Integer(number));
            }
            Err(error) => {
                use std::num::IntErrorKind::*;

                self.add(Integer(match error.kind() {
                    PosOverflow => i64::MAX,
                    NegOverflow => i64::MIN,
                    _ => 0,
                }));
                self.errors.push(Error::NumberExceedsSizeLimit(Spanned::new(
                    self.span(),
                    error,
                )));
            }
        };
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

pub type Token = Spanned<BareToken>;

pub trait TokenExt {
    fn name(&self) -> TokenName;
    fn into_identifier(self) -> Option<String>;
    fn into_text(self) -> Option<String>;
    fn into_integer(self) -> Option<i64>;
}

impl TokenExt for Token {
    fn name(&self) -> TokenName {
        self.bare.name()
    }

    fn into_identifier(self) -> Option<String> {
        obtain!(self.bare, Identifier(identifier) => identifier)
    }

    fn into_text(self) -> Option<String> {
        obtain!(self.bare, Text(text) => text)
    }

    fn into_integer(self) -> Option<i64> {
        obtain!(self.bare, Integer(integer) => integer)
    }
}

#[derive(Clone, Debug, Discriminant)]
#[discriminant(name: TokenName)]
pub enum BareToken {
    Comment,
    Comma,
    Colon,
    OpeningSquareBracket,
    ClosingSquareBracket,
    OpeningCurlyBracket,
    ClosingCurlyBracket,
    False,
    True,
    Identifier(String),
    Text(String),
    Integer(i64),
    EndOfInput,
}

impl fmt::Display for BareToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl fmt::Display for TokenName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenName::*;

        f.write_str(match self {
            Comment => "comment",
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
        })
    }
}

#[derive(Debug)]
pub enum Error {
    IllegalToken(Spanned<char>),
    UnterminatedText(Span),
    NumberExceedsSizeLimit(Spanned<ParseIntError>),
}

impl From<Error> for Diagnostic {
    fn from(error: Error) -> Self {
        match error {
            Error::IllegalToken(token) => {
                let message = format!(
                    "found illegal character U+{:04X} ‘{token}’",
                    token.bare as u32,
                );

                Diagnostic::error()
                    .message(message)
                    .labeled_primary_span(token, "unexpected token")
            }
            // @Task improve message, mention closing it with quotes
            Error::UnterminatedText(span) => Diagnostic::error()
                .message("unterminated text")
                .primary_span(span),
            Error::NumberExceedsSizeLimit(parse_error) => {
                use std::num::IntErrorKind::*;

                Diagnostic::error()
                    .message("the number exceeds the size limit")
                    .with(|error| match parse_error.bare.kind() {
                        PosOverflow => error.note("numbers must not be larger than 2^63 - 1"),
                        NegOverflow => error.note("numbers must not be smaller than -2^63"),
                        _ => error,
                    })
                    .primary_span(parse_error)
            }
        }
    }
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
