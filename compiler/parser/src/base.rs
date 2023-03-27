use ast::Identifier;
use diagnostics::{error::Result, Diagnostic, ErrorCode, Reporter};
use span::{SourceFileIndex, SourceMap, Span};
use std::fmt;
use token::{IndentationError, Token, TokenExt, TokenName, TokenName::*, INDENTATION};
use utilities::{Conjunction, ListingExt};

/// The parser.
pub(crate) struct Parser<'a> {
    tokens: Vec<Token>,
    pub(crate) file: SourceFileIndex,
    index: usize,
    expectations: Vec<Expectation>,
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
            expectations: Vec::new(),
            map,
            reporter,
        }
    }

    pub(crate) fn error<T>(&self) -> Result<T> {
        Err(self.unexpected_token_error().report(self.reporter))
    }

    pub(crate) fn error_with<T>(
        &self,
        builder: impl FnOnce(Diagnostic) -> Diagnostic,
    ) -> Result<T> {
        Err(self
            .unexpected_token_error()
            .with(builder)
            .report(self.reporter))
    }

    fn unexpected_token_error(&self) -> Diagnostic {
        assert!(!self.expectations.is_empty());

        let token = self.token();

        // @Task for the actual token, also print its token "category", e.g.
        // print `keyword ‘case’` instead of just `‘case’`. NB: Don't do that
        // for token expectations!
        Diagnostic::error()
            .code(ErrorCode::E010)
            .message(format!(
                "found {} but expected {}",
                token.name(),
                self.expectations.iter().list(Conjunction::Or),
            ))
            .span(token, "unexpected token")
    }

    /// Add the given expectation to the list of expectations.
    ///
    /// Once we encounter an unexpected token, we list all relevant expectations.
    /// Expectations become irrelevant once a token was successfully consumed or
    /// more precisely once we have [advanced].
    ///
    /// [advance]: Self::advance
    // @Task Enhancement: Introduce some kind of ranking mechanism. Motivation: We'd like to
    // list "declaration" first, that is before "line break" and "end of input".
    pub(crate) fn expected(&mut self, expectation: impl Into<Expectation>) {
        self.expectations.push(expectation.into());
    }

    fn expect(&mut self, expectation: TokenName) -> Result<Token> {
        let token = self.token();
        if token.name() == expectation {
            Ok(token.clone())
        } else {
            self.expected(expectation);
            self.error()
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
            self.expected(token);
            false
        }
    }

    // @Task find a way to replace this overly specific API
    pub(crate) fn maybe_consume_span(&mut self, token: TokenName) -> Option<Span> {
        let span = self.token().span;
        if self.token().name() == token {
            self.advance();
            Some(span)
        } else {
            self.expected(token);
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
    // @Task try to create a more general API
    pub(crate) fn token_into_identifier(&self) -> Identifier {
        self.token().clone().try_into().unwrap()
    }

    /// Step to the next token.
    ///
    /// Clears any [expectations]. Don't advance past [`EndOfInput`].
    ///
    /// [expectations]: Expectation
    pub(crate) fn advance(&mut self) {
        self.expectations.clear();
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

    pub(crate) fn look_behind(&self, amount: usize) -> Option<&Token> {
        Some(&self.tokens[self.index.checked_sub(amount)?])
    }
}

pub(crate) enum Expectation {
    Token(TokenName),
    Category(&'static str),
}

impl From<TokenName> for Expectation {
    fn from(token: TokenName) -> Self {
        Self::Token(token)
    }
}

impl From<&'static str> for Expectation {
    fn from(category: &'static str) -> Self {
        Self::Category(category)
    }
}

impl fmt::Display for Expectation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Token(token) => write!(f, "{token}"),
            Self::Category(name) => f.write_str(name),
        }
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
                .unlabeled_span(self.span)
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
                    .span(self.span, "unexpected token")
            }
            UnbalancedBracket(bracket) => Diagnostic::error()
                .code(ErrorCode::E044)
                .message(format!("unbalanced {} bracket", bracket.kind))
                .span(
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
                .unlabeled_span(self.span),
        }
    }
}
