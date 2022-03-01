use super::{
    lexer::{
        Token,
        TokenName::{self, *},
    },
    Map, Value, ValueKind,
};
use crate::{
    diagnostics::{Code, Diagnostic, Reporter},
    error::{Health, OkIfUntaintedExt, ReportedExt, Result},
    span::{SourceFileIndex, SourceMapCell, Span, Spanning, WeaklySpanned},
};

#[cfg(test)]
mod test;

pub(super) struct Parser<'a> {
    file: SourceFileIndex,
    tokens: &'a [Token],
    index: usize,
    health: Health,
    map: SourceMapCell,
    reporter: &'a Reporter,
}

impl<'a> Parser<'a> {
    pub(super) fn new(
        file: SourceFileIndex,
        tokens: &'a [Token],
        map: SourceMapCell,
        reporter: &'a Reporter,
    ) -> Self {
        Self {
            file,
            tokens,
            index: 0,
            health: Health::Untainted,
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
            Err(Diagnostic::error()
                .message(format!("found {token}, but expected {expected}"))
                .primary_span(token)
                .report(self.reporter))
        }
    }

    fn consume(&mut self, token: TokenName) -> Result<Token> {
        let token = self.expect(token)?;
        self.advance();
        Ok(token)
    }

    /// Parse a metadata document.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Document ::= (Top-Level-Map-Entries | Value) #End-Of-Input
    /// ```
    pub(super) fn parse(&mut self) -> Result<Value> {
        let value = match self.has_top_level_map_entries() {
            true => self.parse_top_level_map_entries(),
            false => self.parse_value(),
        }?;
        self.consume(EndOfInput)?;

        Result::ok_if_untainted(value, self.health)
    }

    fn has_top_level_map_entries(&self) -> bool {
        matches!(self.current_token().name(), Identifier | Text)
            && self.succeeding_token().name() == Colon
            || self.current_token().name() == EndOfInput
    }

    /// Parse top-level map entries.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Top-Level-Map-Entries ::= (Map-Entry ",")* Map-Entry? (> #End-Of-Input)
    /// ```
    fn parse_top_level_map_entries(&mut self) -> Result<Value> {
        let map = self.parse_map_entries(EndOfInput)?;

        Ok(Value::new(
            self.map.borrow()[self.file].span(),
            ValueKind::Map(map),
        ))
    }

    /// Parse a value.
    ///
    /// # Grammar
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
                Ok(Value::new(span, ValueKind::Boolean(false)))
            }
            True => {
                self.advance();
                Ok(Value::new(span, ValueKind::Boolean(true)))
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
                let value = self.current_token().clone().into_integer().unwrap();
                self.advance();

                let value = match value {
                    Ok(content) => content,
                    Err(error) => {
                        use super::lexer::IntLexingError::*;

                        // @Beacon @Task
                        #[allow(clippy::match_same_arms)]
                        let diagnostic = match error {
                            ConsecutiveSeparators => Diagnostic::error().message("@Task"),
                            TrailingSeparators => Diagnostic::error().message("@Task"),
                            SizeExceedance => Diagnostic::error().message("@Task"),
                        };
                        return Err(diagnostic.report(self.reporter));
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
            _ => Err(Diagnostic::error()
                .message(format!(
                    "found {}, but expected value",
                    self.current_token()
                ))
                .primary_span(span)
                .report(self.reporter)),
        }
    }

    /// Finish parsing an array.
    ///
    /// The opening square bracket should have already parsed beforehand.
    ///
    /// # Grammar
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

        Ok(Value::new(span, ValueKind::List(elements)))
    }

    /// Finish parsing a map.
    ///
    /// The opening curly bracket should have already parsed beforehand.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Map ::= "{" (Map-Entry ",")* Map-Entry? "}"
    /// ```
    fn finish_parse_map(&mut self, opening_bracket_span: Span) -> Result<Value> {
        let mut span = opening_bracket_span;

        let map = self.parse_map_entries(ClosingCurlyBracket)?;

        span.merging(self.current_token());
        self.advance();

        Ok(Value::new(span, ValueKind::Map(map)))
    }

    fn parse_map_entries(&mut self, delimiter: TokenName) -> Result<Map> {
        let mut map = Map::default();

        while self.current_token().name() != delimiter {
            let (key, value) = self.parse_map_entry()?;

            if let Some(previous_key) = map.keys().find(|&some_key| some_key == &key) {
                // @Task "is defined multiple times in map `PATH`"
                Diagnostic::error()
                    .code(Code::E803)
                    .message(format!("the key `{}` is defined multiple times", key))
                    .labeled_primary_span(key.span, "redefinition")
                    .labeled_secondary_span(previous_key.span, "previous definition")
                    .report(self.reporter);
                self.health.taint();
            } else {
                map.insert(key, value);
            }

            if self.current_token().name() != delimiter {
                self.consume(Comma)?;
            }
        }

        Ok(map)
    }

    /// Parse a map entry.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Map-Entry ::= Map-Key ":" Value
    /// ```
    fn parse_map_entry(&mut self) -> Result<(WeaklySpanned<String>, Value)> {
        let key = self.parse_map_key()?;
        self.consume(Colon)?;
        let value = self.parse_value()?;

        Ok((key, value))
    }

    /// Parse a map key.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Map-Key ::= #Identifier | #Text
    /// ```
    fn parse_map_key(&mut self) -> Result<WeaklySpanned<String>> {
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
                return Err(Diagnostic::error()
                    .message(format!(
                        "expected map key, but got {:?}",
                        self.current_token().name()
                    ))
                    .primary_span(span)
                    .report(self.reporter));
            }
        };

        Ok(WeaklySpanned::new(span, key))
    }
}
