use diagnostics::{error::Result, Diag, ErrorCode, Reporter};
use lexer::token::{BareToken, Token};
use span::{SourceMap, Span, SrcFileIdx};
use std::{fmt, mem};
use utility::{Atom, Conjunction, ListingExt};

/// The parser.
pub(crate) struct Parser<'a> {
    tokens: Vec<Token>,
    pub(crate) file: SrcFileIdx,
    index: usize,
    expectations: Vec<Expectation>,
    annotations: Vec<Annotation>,
    pub(crate) map: &'a SourceMap,
    pub(crate) rep: &'a Reporter,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(
        tokens: Vec<Token>,
        file: SrcFileIdx,
        map: &'a SourceMap,
        rep: &'a Reporter,
    ) -> Self {
        Self {
            tokens,
            file,
            index: 0,
            expectations: Vec::new(),
            annotations: Vec::new(),
            map,
            rep,
        }
    }

    pub(crate) fn error<T>(&mut self) -> Result<T> {
        let expectations = mem::take(&mut self.expectations);
        let annotations = mem::take(&mut self.annotations);

        let error =
            error::unexpected_token(self.current(), &expectations, annotations).report(self.rep);

        Err(error)
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

    /// Register the given diagnostic annotation.
    ///
    /// Once we encounter an unexpected token, we report a diagnostic with all *relevant* annotations
    /// added onto where existing annotations becomes irrelevant once we [advance] the cursor of the
    /// parser. Most often, this happens through [`Self::consume`].
    ///
    /// [advance]: Self::advance
    pub(crate) fn annotate(&mut self, annotation: Annotation) {
        self.annotations.push(annotation);
    }

    /// Expect the current token to match the given name, [advance] on success and emit an error on failure.
    ///
    /// [advance]: Self::advance
    pub(crate) fn expect(&mut self, expectation: BareToken) -> Result<Span> {
        if self.token() == expectation {
            let span = self.span();
            self.advance();
            Ok(span)
        } else {
            self.expected(expectation);
            self.error()
        }
    }

    /// Consume the current token if it matches the given name.
    ///
    /// Returns whether the token was found and skipped.
    #[must_use]
    pub(crate) fn consume(&mut self, expectation: BareToken) -> bool {
        if self.check(expectation) {
            self.advance();
            true
        } else {
            false
        }
    }

    // @Beacon @Task give this a better name
    pub(crate) fn consume_span(&mut self, expectation: BareToken) -> Option<Span> {
        if self.check(expectation) {
            let span = self.span();
            self.advance();
            Some(span)
        } else {
            None
        }
    }

    pub(crate) fn check(&mut self, expectation: BareToken) -> bool {
        if self.token() == expectation {
            true
        } else {
            self.expected(expectation);
            false
        }
    }

    /// Step to the next token.
    ///
    /// Clears any [expectations] and [annotations].
    /// Don't advance past [`EndOfInput`].
    ///
    /// [expectations]: Self::expected
    /// [annotations]: Self::annotate
    pub(crate) fn advance(&mut self) {
        self.index += 1;
        self.expectations.clear();
        self.annotations.clear();
    }

    /// Obtain the current token.
    fn current(&self) -> Token {
        self.tokens[self.index]
    }

    /// Obtain the span of the current token.
    pub(crate) fn span(&self) -> Span {
        self.current().span
    }

    /// Obtain the current token without span.
    pub(crate) fn token(&self) -> BareToken {
        self.current().bare
    }

    /// Obtain the nth succeeding token.
    pub(crate) fn succeeding(&self, amount: usize) -> Option<&Token> {
        self.tokens.get(self.index + amount)
    }

    pub(crate) fn look_ahead(&self, amount: usize) -> Option<BareToken> {
        Some(self.succeeding(amount)?.bare)
    }

    pub(crate) fn look_behind(&self, amount: usize) -> Option<BareToken> {
        Some(self.tokens[self.index.checked_sub(amount)?].bare)
    }
}

pub(crate) enum Expectation {
    Token(BareToken),
    Decl,
    Expr,
    Statement,
    Pat,
    Param,
    Argument,
    Word,
    Identifier,
    NumberLiteral,
    TextLiteral,
    Path,
}

impl From<BareToken> for Expectation {
    fn from(token: BareToken) -> Self {
        Self::Token(token)
    }
}

impl fmt::Display for Expectation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Token(token) => return token.fmt(f),
            Self::Decl => "declaration",
            Self::Expr => "expression",
            Self::Statement => "statement",
            Self::Pat => "pattern",
            Self::Param => "parameter",
            Self::Argument => "argument",
            Self::Word => "word",
            Self::Identifier => "identifier",
            Self::NumberLiteral => "number literal",
            Self::TextLiteral => "text literal",
            Self::Path => "path",
        })
    }
}

pub(crate) enum Annotation {
    LabelWhileParsing { span: Span, name: &'static str },
    LabelApostrophe { span: Span },
    SuggestWideArrow { span: Span },
    SuggestThinArrowForPiType { span: Span },
    SuggestBracketsAroundLetBindingPattern { span: Span, binder: Atom },
}

impl Annotation {
    fn annotate(self, it: Diag) -> Diag {
        match self {
            Self::LabelWhileParsing { span, name } => {
                it.label(span, format!("while parsing this {name} starting here"))
            }
            Self::LabelApostrophe { span } => it.label(
                span,
                "this apostrophe marks the start of an implicit argument",
            ),
            Self::SuggestWideArrow { span } => it.suggest(
                span,
                "consider replacing the thin arrow with a wide one",
                "=>",
            ),
            Self::SuggestThinArrowForPiType { span } => it.suggest(
                span,
                "consider replacing the wide arrow with a \
                 thin one to denote a function type",
                "->",
            ),
            Self::SuggestBracketsAroundLetBindingPattern { span, binder } => {
                // @Task use a multi-part suggestion once we have support for that
                it.suggest(
                    span,
                    "surround the let-binding with round brackets",
                    format!("(let {binder})"),
                )
            }
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub(crate) enum SkipLineBreaks {
    Yes,
    No,
}

mod error {
    #[allow(clippy::wildcard_imports)] // private inline module
    use super::*;

    pub(super) fn unexpected_token(
        token: Token,
        expectations: &[Expectation],
        annotations: Vec<Annotation>,
    ) -> Diag {
        assert!(!expectations.is_empty());

        // @Task for the actual token, also print its token "category", e.g.
        // print `keyword ‘case’` instead of just `‘case’`. NB: Don't do that
        // for token expectations!
        Diag::error()
            .code(ErrorCode::E010)
            .message(format!(
                "found {token} but expected {}",
                expectations.iter().list(Conjunction::Or),
            ))
            .with(|it| {
                annotations
                    .into_iter()
                    .fold(it, |it, annotation| annotation.annotate(it))
            })
            .span(token, "unexpected token")
    }
}
