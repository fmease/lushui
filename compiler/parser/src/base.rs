use ast::Identifier;
use diagnostics::{error::Result, Diagnostic, ErrorCode, Reporter};
use span::{SourceFileIndex, SourceMap, Span};
use token::{IndentationError, Token, TokenExt, TokenName, TokenName::*, INDENTATION};
use utilities::{Conjunction, ListingExt};

/// The parser.
pub(crate) struct Parser<'a> {
    tokens: Vec<Token>,
    pub(crate) file: SourceFileIndex,
    index: usize,
    pub(crate) map: &'a SourceMap,
    pub(crate) reporter: &'a Reporter,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(
        tokens: Vec<Token>,
        file: SourceFileIndex,
        map: &'a SourceMap,
        reporter: &'a Reporter,
    ) -> Self {
        Self {
            tokens,
            file,
            index: 0,
            map,
            reporter,
        }
    }

    pub(crate) fn expect(&self, expected: TokenName) -> Result<Token> {
        let token = self.token();
        if token.name() == expected {
            Ok(token.clone())
        } else {
            Err(Expected::Token(expected)
                .but_actual_is(token)
                .report(self.reporter))
        }
    }

    // @Note horrible name
    // @Task get rid of this
    pub(crate) fn expect_among_others(
        &self,
        expected: TokenName,
        other_expected: Expected,
    ) -> Result<Token> {
        let token = self.token();
        if token.name() == expected {
            Ok(token.clone())
        } else {
            Err(other_expected
                .added(expected)
                .but_actual_is(token)
                .report(self.reporter))
        }
    }

    /// [Expect] the current token to match the given name and [advance] on success.
    ///
    /// [Expect]: Self::expect
    /// [advance]: Self::advance
    pub(crate) fn consume(&mut self, token: TokenName) -> Result<Token> {
        let token = self.expect(token)?;
        self.advance();
        Ok(token)
    }

    // @Note bad name
    // @Task get rid of this
    pub(crate) fn consume_after_expecting(
        &mut self,
        token: TokenName,
        other_expected: Expected,
    ) -> Result<Token> {
        let token = self.expect_among_others(token, other_expected)?;
        self.advance();
        Ok(token)
    }

    pub(crate) fn consume_word(&mut self) -> Result<Identifier> {
        self.consume(Word)
            .map(|identifier| identifier.try_into().unwrap())
    }

    /// Consume the current token if it matches the given name.
    ///
    /// Returns whether the token was found and skipped.
    #[must_use]
    pub(crate) fn maybe_consume(&mut self, token: TokenName) -> bool {
        if self.token().name() == token {
            self.advance();
            true
        } else {
            false
        }
    }

    // @Beacon @Temporary
    pub(crate) fn maybe_consume_span(&mut self, token: TokenName) -> Option<Span> {
        let span = self.token().span;
        if self.token().name() == token {
            self.advance();
            Some(span)
        } else {
            None
        }
    }

    /// Try to turn the current token into an identifier.
    ///
    /// # Panics
    ///
    /// Panics if the token is neither a [word] nor a [symbol].
    ///
    /// [word]: token::BareToken::Word
    /// [symbol]: token::BareToken::Symbol
    pub(crate) fn token_into_identifier(&self) -> Identifier {
        self.token().clone().try_into().unwrap()
    }

    /// Step to the next token.
    ///
    /// Don't advance past [`EndOfInput`].
    pub(crate) fn advance(&mut self) {
        self.index += 1;
    }

    /// Get the current token.
    pub(crate) fn token(&self) -> &Token {
        &self.tokens[self.index]
    }

    /// Look ahead by the given amount of tokens.
    ///
    /// # Panics
    ///
    /// Panics on out of bounds accesses.
    pub(crate) fn look_ahead(&self, amount: usize) -> &Token {
        &self.tokens[self.index + amount]
    }

    /// Indicates whether the current token is one of the given delimiters.
    // @Task get rid of this
    pub(crate) fn token_is_delimiter(&self, delimiters: &[Delimiter]) -> bool {
        let token = self.token().name();
        delimiters.iter().any(|delimiter| delimiter.matches(token))
    }

    pub(crate) fn preceeding_token(&self) -> Option<&Token> {
        Some(&self.tokens[self.index.checked_sub(1)?])
    }
}

// @Task heavily simplify this! for once-off tags, just use &'static str!
pub(crate) enum Expected {
    Token(TokenName),
    Identifier,
    Path,
    Parameter,
    Declaration,
    Category(&'static str),
    Delimiter(Delimiter),
    // @Bug nested OneOf's
    OneOf(Vec<Self>),
}

impl Expected {
    fn added(mut self, extra: impl Into<Self>) -> Self {
        let extra = extra.into();

        if let Self::OneOf(expected) = &mut self {
            expected.push(extra);
        } else {
            self = expected_one_of![self, extra];
        }

        self
    }
}

// @Task improve API
pub(crate) macro expected_one_of($( $expected:expr ),+ $(,)?) {
    Expected::OneOf(vec![$( $expected.into() ),+])
}

// @Task improve API
pub(crate) fn delimiters_with_expected(
    delimiters: &[Delimiter],
    expected: impl IntoIterator<Item = Expected>,
) -> Expected {
    let delimiters = delimiters.iter().copied().map(Expected::Delimiter);
    Expected::OneOf(expected.into_iter().chain(delimiters).collect())
}

impl Expected {
    pub(crate) fn but_actual_is(self, actual: &Token) -> Diagnostic {
        Diagnostic::error()
            .code(ErrorCode::E010)
            .message(format!("found {actual} but expected {self}"))
            .labeled_primary_span(actual, "unexpected token")
    }
}

impl From<TokenName> for Expected {
    fn from(token: TokenName) -> Self {
        Self::Token(token)
    }
}

impl From<Delimiter> for Expected {
    fn from(delimiter: Delimiter) -> Self {
        Self::Delimiter(delimiter)
    }
}

use std::fmt;

impl fmt::Display for Expected {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expected::*;

        match self {
            Token(token) => write!(f, "{token}"),
            Identifier => f.write_str("identifier"),
            Path => f.write_str("path"),
            Parameter => f.write_str("parameter"),
            Declaration => f.write_str("declaration"),
            Category(name) => f.write_str(name),
            Delimiter(delimiter) => write!(f, "{delimiter}"),
            OneOf(expected) => write!(f, "{}", expected.iter().list(Conjunction::Or)),
        }
    }
}

// @Task get rid of this!!
#[derive(Clone, Copy)]
pub(crate) enum Delimiter {
    TypeAnnotationPrefix,
    #[allow(dead_code)]
    DefinitionPrefix,
    Terminator,
    Token(TokenName),
}

impl Delimiter {
    fn matches(self, token: TokenName) -> bool {
        match (self, token) {
            (Self::TypeAnnotationPrefix, Colon)
            | (Self::DefinitionPrefix, Equals)
            | (Self::Terminator, Semicolon | ClosingCurlyBracket | EndOfInput) => true,
            (Self::Token(expected), actual) => expected == actual,
            _ => false,
        }
    }
}

impl fmt::Display for Delimiter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TypeAnnotationPrefix => write!(f, "type annotation"),
            Self::DefinitionPrefix => write!(f, "definition with ‘=’"),
            // @Question or spell it out? `;`, line break, `}`, dedentation, end of input?
            Self::Terminator => write!(f, "terminator"),
            Self::Token(token) => write!(f, "{token}"),
        }
    }
}

impl From<TokenName> for Delimiter {
    fn from(token: TokenName) -> Self {
        Self::Token(token)
    }
}

#[derive(PartialEq, Eq)]
pub(crate) enum SkipLineBreaks {
    Yes,
    No,
}

pub(crate) trait LexerErrorExt {
    fn into_diagnostic(self) -> Diagnostic;
}

impl LexerErrorExt for lexer::Error {
    fn into_diagnostic(self) -> Diagnostic {
        use lexer::BareError::*;

        match self.bare {
            InvalidIndentation(difference, error) => Diagnostic::error()
                .code(ErrorCode::E046)
                .message(format!(
                    "invalid indentation consisting of {} spaces",
                    difference.0
                ))
                .primary_span(self.span)
                .note(match error {
                    IndentationError::Misaligned => {
                        format!("indentation needs to be a multiple of {}", INDENTATION.0)
                    }
                    IndentationError::TooDeep => format!(
                        "indentation is greater than {} and therefore too deep",
                        INDENTATION.0
                    ),
                }),
            InvalidToken(token) => {
                let message = format!("found invalid character U+{:04X} ‘{token}’", token as u32);

                // @Task code
                Diagnostic::error()
                    .message(message)
                    .labeled_primary_span(self.span, "unexpected token")
            }
            UnbalancedBracket(bracket) => Diagnostic::error()
                .code(ErrorCode::E044)
                .message(format!("unbalanced {} bracket", bracket.kind))
                .labeled_primary_span(
                    self.span,
                    format!(
                        "has no matching {} {} bracket",
                        !bracket.orientation, bracket.kind
                    ),
                ),
            // @Task improve message, mention closing it with quotes
            UnterminatedTextLiteral => Diagnostic::error()
                .code(ErrorCode::E047)
                .message("unterminated text literal")
                .primary_span(self.span),
        }
    }
}
