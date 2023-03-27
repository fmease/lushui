use ast::Identifier;
use diagnostics::{error::Result, Diagnostic, ErrorCode, Reporter};
use span::{SourceFileIndex, SourceMap, Span};
use std::{fmt, mem};
use token::{IndentationError, Token, TokenExt, TokenName, TokenName::*, INDENTATION};
use utilities::{Conjunction, ListingExt};

/// The parser.
pub(crate) struct Parser<'a> {
    tokens: Vec<Token>,
    pub(crate) file: SourceFileIndex,
    index: usize,
    expectations: Vec<Expectation>,
    contexts: Vec<Box<dyn FnOnce(Diagnostic) -> Diagnostic>>,
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
            contexts: Vec::new(),
            map,
            reporter,
        }
    }

    pub(crate) fn error<T>(&mut self) -> Result<T> {
        Err(self.unexpected_token_error().report(self.reporter))
    }

    pub(crate) fn error_with<T>(
        &mut self,
        builder: impl FnOnce(&mut Self, Diagnostic) -> Diagnostic,
    ) -> Result<T> {
        Err(self
            .unexpected_token_error()
            .with(|it| builder(self, it))
            .report(self.reporter))
    }

    fn unexpected_token_error(&mut self) -> Diagnostic {
        let expectations = mem::take(&mut self.expectations);
        let commands = mem::take(&mut self.contexts);

        assert!(!expectations.is_empty());

        let token = self.token();

        // @Task for the actual token, also print its token "category", e.g.
        // print `keyword ‘case’` instead of just `‘case’`. NB: Don't do that
        // for token expectations!
        Diagnostic::error()
            .code(ErrorCode::E010)
            .message(format!(
                "found {} but expected {}",
                token.name(),
                expectations.iter().list(Conjunction::Or),
            ))
            .with(|it| commands.into_iter().fold(it, |it, command| command(it)))
            .span(token, "unexpected token")
    }

    /// Register the given expectation.
    ///
    /// Once we encounter an unexpected token, we list all *relevant* expectations where
    /// existing expectations become irrelevant once we [advance] the cursor of the parser.
    /// Most often, this happens through [`Self::consume`].
    ///
    /// [advance]: Self::advance
    // @Task Enhancement: Introduce some kind of ranking mechanism. Motivation: We'd like to
    // list "declaration" first, that is before "line break" and "end of input".
    pub(crate) fn expected(&mut self, expectation: impl Into<Expectation>) {
        self.expectations.push(expectation.into());
    }

    /// Register the given diagnostic context.
    ///
    /// Once we encounter an unexpected token, we report a diagnostic with all *relevant* contexts
    /// added onto where existing contexts becomes irrelevant once we [advance] the cursor of the
    /// parser. Most often, this happens through [`Self::consume`].
    ///
    /// Taking a command instead of a boxed closure is less general and less ergonomic.
    /// However, the latter would incur a performance cost in the happy path.
    /// Hence this defunctionalized API.
    ///
    /// [advance]: Self::advance
    pub(crate) fn context(&mut self, context: impl FnOnce(Diagnostic) -> Diagnostic + 'static) {
        self.contexts.push(Box::new(context));
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
    pub(crate) fn maybe_consume(&mut self, expectation: TokenName) -> bool {
        if self.token().name() == expectation {
            self.advance();
            true
        } else {
            self.expected(expectation);
            false
        }
    }

    // @Task find a way to replace this overly specific API
    pub(crate) fn maybe_consume_span(&mut self, expectation: TokenName) -> Option<Span> {
        let token = self.token();
        let span = token.span;
        if token.name() == expectation {
            self.advance();
            Some(span)
        } else {
            self.expected(expectation);
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
    /// Clears any [expectations] and [contexts].
    /// Don't advance past [`EndOfInput`].
    ///
    /// [expectations]: Self::expected
    /// [contexts]: Self::context
    pub(crate) fn advance(&mut self) {
        self.index += 1;
        self.expectations.clear();
        self.contexts.clear();
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
