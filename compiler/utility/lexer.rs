use crate::span::{LocalByteIndex, LocalSpan, SourceFile, Span, Spanned};
use std::{iter::Peekable, str::CharIndices};

pub(crate) trait Lexer<'source, TokenKind> {
    fn source_file(&self) -> &'source SourceFile;

    fn characters(&mut self) -> &mut Peekable<CharIndices<'source>>;

    fn tokens(&mut self) -> &mut Vec<Spanned<TokenKind>>;

    fn local_span(&self) -> LocalSpan;

    fn local_span_mut(&mut self) -> &mut LocalSpan;

    fn span(&self) -> Span {
        self.local_span().global(self.source_file())
    }

    fn source(&self) -> &'source str {
        &self.source_file()[self.local_span()]
    }

    /// Step to the next token in the input stream.
    fn advance(&mut self) {
        self.characters().next();
    }

    /// Include the span of the current token in the span of the token-to-be-added.
    ///
    /// Preparation for [`Self::add`] and variants.
    fn take(&mut self) {
        let (index, character) = self.peek_with_index().unwrap();
        self.local_span_mut().set_end(index + character);
    }

    fn peek(&mut self) -> Option<char> {
        self.peek_with_index().map(|(_, character)| character)
    }

    fn peek_with_index(&mut self) -> Option<(LocalByteIndex, char)> {
        self.characters()
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
    fn add(&mut self, token: TokenKind) {
        self.add_with(|span| Spanned::new(span, token));
    }

    /// [Add](Self::add) a token given a constructor.
    fn add_with(&mut self, constructor: impl FnOnce(Span) -> Spanned<TokenKind>) {
        let span = self.span();
        self.tokens().push(constructor(span));
    }

    fn consume(&mut self, token: TokenKind) {
        self.take();
        self.advance();
        self.add(token);
    }
}
