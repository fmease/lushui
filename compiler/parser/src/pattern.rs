use crate::{
    base::{Expectation, Parser, SkipLineBreaks},
    synonym::PathHead,
};
use ast::Pattern;
use diagnostics::error::Result;
use lexer::token::BareToken::*;
use span::Spanned;

impl Parser<'_> {
    /// Parse a pattern.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Pattern ::=
    ///     | Binder
    ///     | Lower-Pattern Pattern-Argument*
    /// Binder ::= "let" Local-Binder
    /// Pattern-Argument ::=
    ///     "'"?
    ///     (Lower-Pattern | "(" (#Word "=")? Pattern ")")
    /// ```
    pub(crate) fn parse_pattern(&mut self) -> Result<Pattern> {
        if let Let = self.token() {
            // We don't add ‘let’ to the list of expectations, so we don't end up with
            // “expected ‘let’ or pattern” in diagnostics which would be awkward.
            let span = self.span();
            self.advance();

            let binder = self.parse_local_binder()?;
            return Ok(Pattern::common(span.merge(&binder), binder.into()));
        }

        self.parse_application_or_lower()
    }

    /// Parse a lower pattern.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Lower-Pattern ::= Attribute* Bare-Lower-Pattern
    /// Bare-Lower-Pattern ::=
    ///     | Wildcard
    ///     | #Number-Literal
    ///     | #Text-Literal
    ///     | Sequence-Literal-Or-Bracketed-Pattern
    ///     | Record-Literal-Pattern
    ///     | Path-Or-Namespaced-Pattern-Literal
    /// ```
    pub(crate) fn parse_lower_pattern(&mut self) -> Result<Pattern> {
        //
        // IMPORTANT: To be kept in sync with `pattern::LowerPatternPrefix`.
        //

        let attributes = self.parse_attributes(SkipLineBreaks::No)?;

        let span = self.span();
        let mut pattern = match self.token() {
            Underscore => {
                self.advance();

                Pattern::common(span, ast::Wildcard::Silent.into())
            }
            QuestionMark => {
                self.advance();
                self.finish_parse_signaling_wildcard(span)?
            }
            NumberLiteral(literal) => {
                self.advance();

                Pattern::common(
                    span,
                    ast::NumberLiteral { path: None, literal: Spanned::new(span, literal) }.into(),
                )
            }
            TextLiteral(literal) => {
                self.advance();

                Pattern::common(
                    span,
                    ast::TextLiteral { path: None, literal: Spanned::new(span, literal) }.into(),
                )
            }
            PathHead!() => self.parse_path_or_namespaced_literal()?,
            OpeningCurlyBracket => {
                self.advance();
                self.finish_parse_record_literal(None, span)?
            }
            OpeningRoundBracket => {
                self.advance();
                self.finish_parse_sequence_literal_or_bracketed_item(None, span)?
            }
            _ => {
                self.expected(Expectation::Pattern);
                return self.error();
            }
        };

        pattern.attributes.extend(attributes);

        Ok(pattern)
    }
}
