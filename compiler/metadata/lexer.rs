use crate::{
    diagnostics::Diagnostic,
    error::{Health, Outcome},
    span::{LocalSpan, SourceFile, Spanned},
    syntax::lexer::{
        is_identifier_segment_middle, is_identifier_segment_start, is_number_literal_middle,
        NUMERIC_SEPARATOR,
    },
    utility::{lexer::Lexer as _, obtain, quoted},
};
use derivation::Discriminant;
use std::{fmt, iter::Peekable, str::CharIndices};
use BareToken::*;

pub(super) type Token = Spanned<BareToken>;

impl Token {
    pub(crate) const fn name(&self) -> TokenName {
        self.bare.name()
    }

    pub(super) fn into_identifier(self) -> Option<String> {
        obtain!(self.bare, Identifier(identifier) => identifier)
    }

    pub(super) fn into_text(self) -> Option<Result<String, Diagnostic>> {
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
    pub(super) fn into_integer(self) -> Option<Result<i64, IntLexingError>> {
        obtain!(self.bare, Integer(integer) => integer)
    }
}

#[derive(Clone, Debug, Discriminant)]
#[discriminant(name: TokenName)]
pub(super) enum BareToken {
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
    // @Task lex -infinity, +infinity, +nan, -nan as well
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
}

impl<'a> crate::utility::lexer::Lexer<'a, BareToken> for Lexer<'a> {
    fn file(&self) -> &'a SourceFile {
        self.file
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

#[derive(Clone, Copy, Debug)]
pub(crate) enum TextLexingError {
    Unterminated,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum IntLexingError {
    ConsecutiveSeparators,
    TrailingSeparators,
    SizeExceedance,
}
