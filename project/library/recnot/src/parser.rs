use crate::{
    lexer::{
        self, Token, TokenExt,
        TokenName::{self, *},
    },
    BareValue, Record, Value,
};
use diagnostics::{
    error::{Health, Outcome, Result},
    Diag, ErrorCode, Reporter,
};
use span::{SourceMap, Span, Spanning, SrcFileIdx, WeaklySpanned};
use std::sync::RwLock;

#[cfg(test)]
mod test;

pub fn parse(
    tokens: lexer::Outcome,
    file: SrcFileIdx,
    map: &RwLock<SourceMap>,
    rep: &Reporter,
) -> Result<Value> {
    let mut health = Ok(());

    for error in tokens.errors {
        let error = Diag::from(error).report(rep);

        if health.is_ok() {
            health = Err(error);
        }
    }

    let result = Parser::new(tokens.tokens, file, map, rep).parse();

    health.and(result)
}

struct Parser<'a> {
    tokens: Vec<Token>,
    file: SrcFileIdx,
    index: usize,
    health: Health,
    map: &'a RwLock<SourceMap>,
    rep: &'a Reporter,
}

impl<'a> Parser<'a> {
    fn new(
        tokens: Vec<Token>,
        file: SrcFileIdx,
        map: &'a RwLock<SourceMap>,
        rep: &'a Reporter,
    ) -> Self {
        Self {
            file,
            tokens,
            index: 0,
            health: Health::Untainted,
            map,
            rep,
        }
    }

    fn curr_token(&self) -> &Token {
        &self.tokens[self.index]
    }

    fn succ_token(&self) -> &Token {
        &self.tokens[self.index + 1]
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    fn expect(&self, expected: TokenName) -> Result<Token> {
        let token = self.curr_token();
        if token.name() == expected {
            Ok(token.clone())
        } else {
            Err(Diag::error()
                .message(format!("found {token} but expected {expected}"))
                .unlabeled_span(token)
                .report(self.rep))
        }
    }

    fn consume(&mut self, token: TokenName) -> Result<Token> {
        let token = self.expect(token)?;
        self.advance();
        Ok(token)
    }

    /// Parse a document.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Document ::= (Top-Level-Record-Entries | Value) #End-Of-Input
    /// ```
    fn parse(&mut self) -> Result<Value> {
        let value = match self.has_top_level_record_entries() {
            true => self.parse_top_level_record_entries(),
            false => self.parse_value(),
        }?;
        // @Task mention `:` if previous token was a record key
        self.consume(EndOfInput)?;

        Outcome::new(value, self.health).into()
    }

    fn has_top_level_record_entries(&self) -> bool {
        matches!(self.curr_token().name(), Ident | False | True | Text)
            && self.succ_token().name() == Colon
            || self.curr_token().name() == EndOfInput
    }

    /// Parse top-level record entries.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Top-Level-Record-Entries ::= (Record-Entry ",")* Record-Entry? (> #End-Of-Input)
    /// ```
    fn parse_top_level_record_entries(&mut self) -> Result<Value> {
        let record = self.parse_record_entries(EndOfInput)?;

        Ok(Value::new(
            self.map.read().unwrap()[self.file].span(),
            BareValue::Record(record),
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
    ///     | #Ident
    ///     | #Number
    ///     | Array
    ///     | Record
    /// ```
    fn parse_value(&mut self) -> Result<Value> {
        let span = self.curr_token().span;
        match self.curr_token().name() {
            False => {
                self.advance();
                Ok(Value::new(span, false.into()))
            }
            True => {
                self.advance();
                Ok(Value::new(span, true.into()))
            }
            Text => {
                // @Task avoid cloning!
                let content = self.curr_token().clone().into_text().unwrap();
                self.advance();
                Ok(Value::new(span, content.into()))
            }
            Ident => {
                // @Task avoid cloning!
                let content = self.curr_token().clone().into_ident().unwrap();
                self.advance();
                Ok(Value::new(span, content.into()))
            }
            Integer => {
                let value = self.curr_token().clone().into_integer().unwrap();
                self.advance();

                Ok(Value::new(span, value.into()))
            }
            OpeningSquareBracket => {
                self.advance();
                self.finish_parse_array(span)
            }
            OpeningCurlyBracket => {
                self.advance();
                self.finish_parse_record(span)
            }
            _ => Err(Diag::error()
                .message(format!("found {} but expected value", self.curr_token()))
                .unlabeled_span(span)
                .report(self.rep)),
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

        while self.curr_token().name() != ClosingSquareBracket {
            elements.push(self.parse_value()?);

            if self.curr_token().name() != ClosingSquareBracket {
                self.consume(Comma)?;
            }
        }

        span.merging(self.curr_token());
        self.advance();

        Ok(Value::new(span, elements.into()))
    }

    /// Finish parsing a record.
    ///
    /// The opening curly bracket should have already parsed beforehand.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Record ::= "{" (Record-Entry ",")* Record-Entry? "}"
    /// ```
    fn finish_parse_record(&mut self, opening_bracket_span: Span) -> Result<Value> {
        let mut span = opening_bracket_span;

        let record = self.parse_record_entries(ClosingCurlyBracket)?;

        span.merging(self.curr_token());
        self.advance();

        Ok(Value::new(span, record.into()))
    }

    fn parse_record_entries(&mut self, delimiter: TokenName) -> Result<Record> {
        let mut record = Record::default();

        while self.curr_token().name() != delimiter {
            let (key, value) = self.parse_record_entry()?;

            if let Some(previous_key) = record.keys().find(|&some_key| some_key == &key) {
                // @Task make *all* duplicate entries *primary* highlights
                let error = Diag::error()
                    .code(ErrorCode::E803)
                    .message(format!("the entry ‘{key}’ is defined multiple times"))
                    .span(key.span, "redefinition")
                    .label(previous_key.span, "previous definition")
                    .report(self.rep);
                self.health.taint(error);
            } else {
                record.insert(key, value);
            }

            if self.curr_token().name() != delimiter {
                self.consume(Comma)?;
            }
        }

        Ok(record)
    }

    /// Parse a record entry.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Record-Entry ::= Record-Key ":" Value
    /// ```
    fn parse_record_entry(&mut self) -> Result<(WeaklySpanned<String>, Value)> {
        let key = self.parse_record_key()?;
        self.consume(Colon)?;
        let value = self.parse_value()?;

        Ok((key, value))
    }

    /// Parse a record key.
    ///
    /// # Grammar
    ///
    /// ```ebnf
    /// Record-Key ::= #Ident | Keyword | #Text
    /// Keyword ::= "false" | "true"
    /// ```
    fn parse_record_key(&mut self) -> Result<WeaklySpanned<String>> {
        let span = self.curr_token().span;
        let key = match self.curr_token().name() {
            Ident => {
                // @Task avoid cloning
                let key = self.curr_token().clone().into_ident().unwrap();
                self.advance();
                key
            }
            Text => {
                // @Task avoid cloning
                let key = self.curr_token().clone().into_text().unwrap();
                self.advance();
                key
            }
            False => {
                self.advance();
                "false".into()
            }
            True => {
                self.advance();
                "true".into()
            }
            token => {
                return Err(Diag::error()
                    .message(format!("found {token} but expected record key"))
                    .unlabeled_span(span)
                    .report(self.rep));
            }
        };

        Ok(WeaklySpanned::new(span, key))
    }
}
