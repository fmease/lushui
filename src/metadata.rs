//! A custom human-readable metadata file format.

// @Task raw text, multi lines text with indentation awareness
// @Task unit tests

// @Task support keywords as keys e.g. `false: false`

use std::{convert::TryFrom, fmt};

use crate::{
    diagnostics::Reporter,
    error::{outcome, Result},
    span::{SharedSourceMap, SourceFileIndex, Span, Spanned},
    util::{obtain, HashMap},
};

pub type Value = Spanned<ValueKind>;

// @Task version, version requirement
#[derive(Debug)]
pub enum ValueKind {
    Bool(bool),
    Integer(i64),
    Float(f64),
    Text(String),
    Array(Vec<Value>),
    Map(HashMap<String, MapEntry>),
}

impl ValueKind {
    pub fn into_bool(self) -> Option<bool> {
        obtain!(self, Self::Bool(bool) => bool)
    }

    pub fn into_integer(self) -> Option<i64> {
        obtain!(self, Self::Integer(int) => int)
    }

    pub fn into_float(self) -> Option<f64> {
        obtain!(self, Self::Float(float) => float)
    }

    pub fn into_text(self) -> Option<String> {
        obtain!(self, Self::Text(text) => text)
    }

    pub fn into_array(self) -> Option<Vec<Value>> {
        obtain!(self, Self::Array(array) => array)
    }

    pub fn into_map(self) -> Option<HashMap<String, MapEntry>> {
        obtain!(self, Self::Map(map) => map)
    }

    pub fn type_(&self) -> Type {
        match self {
            Self::Bool(_) => Type::Bool,
            Self::Integer(_) => Type::Int,
            Self::Float(_) => Type::Float,
            Self::Text(_) => Type::Text,
            Self::Array(_) => todo!(),
            Self::Map(_) => todo!(),
        }
    }
}

impl TryFrom<ValueKind> for bool {
    type Error = TypeError;

    fn try_from(value: ValueKind) -> Result<Self, Self::Error> {
        let type_ = value.type_();

        value.into_bool().ok_or(TypeError {
            expected: Type::Bool,
            actual: type_,
        })
    }
}

impl TryFrom<ValueKind> for i64 {
    type Error = TypeError;

    fn try_from(value: ValueKind) -> Result<Self, Self::Error> {
        let type_ = value.type_();

        value.into_integer().ok_or(TypeError {
            expected: Type::Int,
            actual: type_,
        })
    }
}

impl TryFrom<ValueKind> for f64 {
    type Error = TypeError;

    fn try_from(value: ValueKind) -> Result<Self, Self::Error> {
        let type_ = value.type_();

        value.into_float().ok_or(TypeError {
            expected: Type::Float,
            actual: type_,
        })
    }
}

impl TryFrom<ValueKind> for String {
    type Error = TypeError;

    fn try_from(value: ValueKind) -> Result<Self, Self::Error> {
        let type_ = value.type_();

        value.into_text().ok_or(TypeError {
            expected: Type::Text,
            actual: type_,
        })
    }
}

pub struct TypeError {
    pub expected: Type,
    pub actual: Type,
}

pub enum Type {
    Bool,
    Int,
    Float,
    Text,
    Array(Box<Type>),
    Map(Box<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Bool => write!(f, "boolean"),
            Type::Int => write!(f, "integer"),
            Type::Float => write!(f, "floating-point number"),
            Type::Text => write!(f, "text"),
            // @Task opt "of XXX"
            Type::Array(_) => write!(f, "array"),
            // @Task opt "of XXX"
            Type::Map(_) => write!(f, "map"),
        }
    }
}

#[derive(Debug)]
pub struct MapEntry {
    pub key: Span,
    pub value: Value,
}

pub fn parse(
    source_file_index: SourceFileIndex,
    map: SharedSourceMap,
    reporter: &Reporter,
) -> Result<Value> {
    let source_file = &map.borrow()[source_file_index];
    let lexer = lexer::Lexer::new(source_file);
    let outcome!(tokens, health_of_lexer) = lexer.lex();
    let mut parser = parser::Parser::new(source_file_index, &tokens, map.clone(), reporter);
    let value = parser.parse()?;
    assert!(health_of_lexer.is_untainted()); // the parser succeeded
    Ok(value)
}

mod lexer {
    use crate::{
        diagnostics::Diagnostic,
        error::{Health, Outcome},
        format::quoted,
        lexer::token,
        span::{LocalByteIndex, LocalSpan, SourceFile, Spanned},
        util::{lexer::Lexer as _, obtain},
    };
    use discriminant::Discriminant;
    use std::{fmt, iter::Peekable, str::CharIndices};
    use TokenKind::*;

    pub(super) type Token = Spanned<TokenKind>;

    impl Token {
        pub fn name(&self) -> TokenName {
            self.kind.discriminant()
        }

        pub(super) fn into_identifier(self) -> Option<String> {
            obtain!(self.kind, Identifier(identifier) => identifier)
        }

        pub(super) fn into_text(self) -> Option<Result<String, Diagnostic>> {
            match self.kind {
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
        pub(super) fn into_int(self) -> Option<Result<i64, IntLexingError>> {
            obtain!(self.kind, Integer(integer) => integer)
        }
    }

    #[derive(Clone, Debug, Discriminant)]
    #[discriminant(TokenName)]
    pub(super) enum TokenKind {
        Comma,
        Colon,
        OpeningSquareBracket,
        ClosingSquareBracket,
        OpeningCurlyBracket,
        ClosingCurlyBracket,
        False,
        True,
        Nan,
        Infinity,
        Identifier(String),
        Text(Result<String, TextLexingError>),
        Integer(Result<i64, IntLexingError>),
        EndOfInput,
        Illegal(char),
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
                False => "keyword `false`",
                True => "keyword `true`",
                Nan => "keyword `nan`",
                Infinity => "keyword `infinity`",
                Identifier => "identifier",
                Text => "text",
                Integer => "integer",
                EndOfInput => "end of input",
                Illegal => "illegal character",
            })
        }
    }

    pub(super) struct Lexer<'a> {
        source_file: &'a SourceFile,
        characters: Peekable<CharIndices<'a>>,
        tokens: Vec<Token>,
        local_span: LocalSpan,
        health: Health,
    }

    impl<'a> Lexer<'a> {
        pub(super) fn new(source_file: &'a SourceFile) -> Self {
            Self {
                characters: source_file.content().char_indices().peekable(),
                source_file,
                tokens: Vec::new(),
                local_span: LocalSpan::zero(),
                health: Health::Untainted,
            }
        }

        // @Task lex -infinity, +infinity, +nan, -nan as well
        pub(super) fn lex(mut self) -> Outcome<Vec<Token>> {
            while let Some(character) = self.peek() {
                let index = self.index().unwrap();
                self.local_span = LocalSpan::from(index);

                match character {
                    '#' => self.lex_comment(),
                    character if character.is_ascii_whitespace() => self.lex_whitespace(),
                    '"' => self.lex_text(),
                    character if token::is_identifier_segment_start(character) => {
                        self.lex_identifier()
                    }
                    character if character.is_ascii_digit() => self.lex_number(),
                    '-' => todo!(),
                    '[' => {
                        self.add(OpeningSquareBracket);
                        self.advance();
                    }
                    ']' => {
                        self.add(ClosingSquareBracket);
                        self.advance();
                    }
                    '{' => {
                        self.add(OpeningCurlyBracket);
                        self.advance();
                    }
                    '}' => {
                        self.add(ClosingCurlyBracket);
                        self.advance();
                    }
                    ',' => {
                        self.add(Comma);
                        self.advance();
                    }
                    ':' => {
                        self.add(Colon);
                        self.advance();
                    }
                    character => {
                        self.take();
                        self.add(Illegal(character));
                        self.advance();
                    }
                }
            }

            let last =
                LocalByteIndex::from_usize(self.source_file.content().len().saturating_sub(1));
            self.local_span = LocalSpan::from(last);
            self.add(EndOfInput);
            self.health.of(self.tokens)
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
                    let content =
                        self.source_file[self.local_span.trim_start(1).trim_end(1)].to_owned();

                    self.add(Text(Ok(content)))
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
                    self.local_span = dash.into();
                    // @Task add illegal
                    todo!();
                }
            }

            self.add(match self.source() {
                "false" => False,
                "true" => True,
                "nan" => Nan,
                "infinity" => Infinity,
                identifier => Identifier(identifier.to_owned()),
            });
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

        // @Task support floats
        fn lex_number(&mut self) {
            let mut number = self.source().to_owned();

            let mut trailing_separator = false;
            let mut consecutive_separators = false;

            while let Some(character) = self.peek() {
                if !token::is_number_literal_middle(character) {
                    break;
                }
                self.take();
                self.advance();

                if character != token::NUMERIC_SEPARATOR {
                    number.push(character);
                } else {
                    if let Some(token::NUMERIC_SEPARATOR) = self.peek() {
                        consecutive_separators = true;
                    }
                    if self.peek().map_or(true, |character| {
                        !character.is_ascii_digit() || character == token::NUMERIC_SEPARATOR
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

    impl<'a> crate::util::lexer::Lexer<'a, TokenKind> for Lexer<'a> {
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
}

mod parser {
    use super::{
        lexer::{
            Token,
            TokenName::{self, *},
        },
        MapEntry, Value, ValueKind,
    };
    use crate::{
        diagnostics::{Diagnostic, Reporter},
        error::{ReportedExt, Result},
        span::{SharedSourceMap, SourceFileIndex, Span, Spanned, Spanning},
        util::HashMap,
    };
    pub(super) struct Parser<'a> {
        file: SourceFileIndex,
        tokens: &'a [Token],
        index: usize,
        map: SharedSourceMap,
        reporter: &'a Reporter,
    }

    impl<'a> Parser<'a> {
        pub(super) fn new(
            file: SourceFileIndex,
            tokens: &'a [Token],
            map: SharedSourceMap,
            reporter: &'a Reporter,
        ) -> Self {
            Self {
                file,
                tokens,
                index: 0,
                map,
                reporter,
            }
        }

        fn current_token(&self) -> &Token {
            &self.tokens[self.index]
        }

        fn succeeding_token(&self) -> &Token {
            &self.tokens[self.index + 1]
        }

        fn advance(&mut self) {
            self.index += 1;
        }

        fn expect(&self, expected: TokenName) -> Result<Token> {
            let token = self.current_token();
            if token.name() == expected {
                Ok(token.clone())
            } else {
                // @Task flesh out
                Diagnostic::error()
                    .message(format!("expected {expected}, but got {}", token.name()))
                    .primary_span(token)
                    .report(self.reporter);
                Err(())
            }
        }

        fn consume(&mut self, token: TokenName) -> Result<Token> {
            let token = self.expect(token)?;
            self.advance();
            Ok(token)
        }

        /// Parse a metadata document.
        ///
        /// ## Grammar
        ///
        /// ```ebnf
        /// Document ::= (Top-Level-Map-Entries | Value) #End-Of-Input
        /// ```
        pub(super) fn parse(&mut self) -> Result<Value> {
            let value = if matches!(self.current_token().name(), Identifier | Text)
                && self.succeeding_token().name() == Colon
            {
                self.parse_top_level_map_entries()
            } else {
                self.parse_value()
            }?;
            self.consume(EndOfInput)?;
            Ok(value)
        }

        /// Parse top-level map entries.
        ///
        /// ## Grammar
        ///
        /// ```ebnf
        /// Top-Level-Map-Entries ::= (Map-Entry ",")* Map-Entry? (> #End-Of-Input)
        /// ```
        fn parse_top_level_map_entries(&mut self) -> Result<Value> {
            let mut span: Option<Span> = None;
            let mut entries = HashMap::default();

            while self.current_token().name() != EndOfInput {
                let (key, entry) = self.parse_map_entry()?;

                let span_of_entry = entry.key.merge(entry.value.span());
                let span = span.get_or_insert(span_of_entry);
                span.merging(span_of_entry);

                entries.extend_one((key, entry));

                if self.current_token().name() != EndOfInput {
                    span.merging(self.consume(Comma)?);
                }
            }

            Ok(Value::new(
                span.unwrap_or_else(|| self.map.borrow()[self.file].span),
                ValueKind::Map(entries),
            ))
        }

        /// Parse a value.
        ///
        /// ## Grammar
        ///
        /// ```ebnf
        /// Value ::=
        ///     | "null"
        ///     | "false"
        ///     | "true"
        ///     | #Text
        ///     | #Number
        ///     | Array
        ///     | Map
        /// ```
        fn parse_value(&mut self) -> Result<Value> {
            let span = self.current_token().span;
            match self.current_token().name() {
                False => {
                    self.advance();
                    Ok(Value::new(span, ValueKind::Bool(false)))
                }
                True => {
                    self.advance();
                    Ok(Value::new(span, ValueKind::Bool(true)))
                }
                Nan => {
                    self.advance();
                    Ok(Value::new(span, ValueKind::Float(f64::NAN)))
                }
                Infinity => {
                    self.advance();
                    Ok(Value::new(span, ValueKind::Float(f64::INFINITY)))
                }
                Text => {
                    // @Task avoid cloning!
                    let content = self
                        .current_token()
                        .clone()
                        .into_text()
                        .unwrap()
                        .reported(self.reporter)?;
                    self.advance();

                    Ok(Value::new(span, ValueKind::Text(content)))
                }
                Integer => {
                    let value = self.current_token().clone().into_int().unwrap();
                    self.advance();

                    let value = match value {
                        Ok(content) => content,
                        Err(error) => {
                            use super::lexer::IntLexingError::*;

                            // @Beacon @Task
                            let diagnostic = match error {
                                ConsecutiveSeparators => Diagnostic::error().message("@Task"),
                                TrailingSeparators => Diagnostic::error().message("@Task"),
                                SizeExceedance => Diagnostic::error().message("@Task"),
                            };
                            diagnostic.report(self.reporter);
                            return Err(());
                        }
                    };
                    Ok(Value::new(span, ValueKind::Integer(value)))
                }
                OpeningSquareBracket => {
                    self.advance();
                    self.finish_parse_array(span)
                }
                OpeningCurlyBracket => {
                    self.advance();
                    self.finish_parse_map(span)
                }
                name => {
                    // @Temporary
                    Diagnostic::error()
                        .message(format!("expected value, but found {name:?}"))
                        .primary_span(span)
                        .report(self.reporter);
                    Err(())
                }
            }
        }

        /// Finish parsing an array.
        ///
        /// The opening square bracket should have already parsed beforehand.
        ///
        /// ## Grammar
        ///
        /// ```ebnf
        /// Array ::= "[" (Value ",")* Value? "]"
        /// ```
        fn finish_parse_array(&mut self, opening_bracket_span: Span) -> Result<Value> {
            let mut span = opening_bracket_span;
            let mut elements = Vec::new();

            while self.current_token().name() != ClosingSquareBracket {
                elements.push(self.parse_value()?);

                if self.current_token().name() != ClosingSquareBracket {
                    self.consume(Comma)?;
                }
            }

            span.merging(self.current_token());
            self.advance();

            Ok(Value::new(span, ValueKind::Array(elements)))
        }

        /// Finish parsing a map.
        ///
        /// The opening curly bracket should have already parsed beforehand.
        ///
        /// ## Grammar
        ///
        /// ```ebnf
        /// Map ::= "{" (Map-Entry ",")* Map-Entry? "}"
        /// ```
        fn finish_parse_map(&mut self, opening_bracket_span: Span) -> Result<Value> {
            let mut span = opening_bracket_span;
            let mut entries = HashMap::default();

            while self.current_token().name() != ClosingCurlyBracket {
                entries.extend_one(self.parse_map_entry()?);

                if self.current_token().name() != ClosingCurlyBracket {
                    self.consume(Comma)?;
                }
            }

            span.merging(self.current_token());
            self.advance();

            Ok(Value::new(span, ValueKind::Map(entries)))
        }

        /// Parse a map entry.
        ///
        /// ## Grammar
        ///
        /// ```ebnf
        /// Map-Entry ::= Map-Key ":" Value
        /// ```
        fn parse_map_entry(&mut self) -> Result<(String, MapEntry)> {
            let key = self.parse_map_key()?;
            self.consume(Colon)?;
            let value = self.parse_value()?;

            Ok((
                key.kind,
                MapEntry {
                    key: key.span,
                    value,
                },
            ))
        }

        /// Parse a map key.
        ///
        /// ## Grammar
        ///
        /// ```ebnf
        /// Map-Key ::= #Identifier | #Text
        /// ```
        fn parse_map_key(&mut self) -> Result<Spanned<String>> {
            let span = self.current_token().span;
            let key = match self.current_token().name() {
                Identifier => {
                    // @Task avoid cloning
                    let key = self.current_token().clone().into_identifier().unwrap();
                    self.advance();
                    key
                }
                Text => {
                    // @Task avoid cloning
                    let key = self
                        .current_token()
                        .clone()
                        .into_text()
                        .unwrap()
                        .reported(self.reporter)?;
                    self.advance();
                    key
                }
                _ => {
                    // @Task
                    Diagnostic::error()
                        .message(format!(
                            "expected map key, but got {:?}",
                            self.current_token().name()
                        ))
                        .primary_span(span)
                        .report(self.reporter);
                    return Err(());
                }
            };

            Ok(Spanned::new(span, key))
        }
    }
}
