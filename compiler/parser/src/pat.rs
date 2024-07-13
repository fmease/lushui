use crate::{
    base::{Expectation, Parser, SkipLineBreaks},
    synonym::PathHead,
};
use ast::Pat;
use diagnostics::error::Result;
use lexer::token::BareToken::*;
use span::Spanned;

impl Parser<'_> {
    /// Parse a pattern.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Pat ::=
    ///     | Binder
    ///     | Lower-Pat Pat-Argument*
    /// Binder ::= "let" Local-Binder
    /// Pat-Argument ::=
    ///     "'"?
    ///     (Lower-Pat | "(" (#Word "=")? Pat ")")
    /// ```
    pub(crate) fn parse_pattern(&mut self) -> Result<Pat> {
        if let Let = self.token() {
            // We don't add ‘let’ to the list of expectations, so we don't end up with
            // “expected ‘let’ or pattern” in diagnostics which would be awkward.
            let span = self.span();
            self.advance();

            let binder = self.parse_local_binder()?;
            return Ok(Pat::common(span.merge(&binder), binder.into()));
        }

        self.parse_app_or_lower()
    }

    /// Parse a lower pattern.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Lower-Pat ::= Attr* Bare-Lower-Pat
    /// Bare-Lower-Pat ::=
    ///     | Wildcard
    ///     | #Number-Literal
    ///     | #Text-Literal
    ///     | Sequence-Literal-Or-Bracketed-Pat
    ///     | Record-Literal-Pat
    ///     | Path-Or-Namespaced-Pat-Literal
    /// ```
    pub(crate) fn parse_lower_pattern(&mut self) -> Result<Pat> {
        //
        // IMPORTANT: To be kept in sync with `pattern::LowerPatternPrefix`.
        //

        let attributes = self.parse_attrs(SkipLineBreaks::No)?;

        let span = self.span();
        let mut pattern = match self.token() {
            Underscore => {
                self.advance();

                Pat::common(span, ast::Wildcard::Silent.into())
            }
            QuestionMark => {
                self.advance();
                self.fin_parse_signaling_wildcard(span)?
            }
            NumLit(num) => {
                self.advance();

                Pat::common(
                    span,
                    ast::NumLit {
                        path: None,
                        lit: Spanned::new(span, num),
                    }
                    .into(),
                )
            }
            TextLit(literal) => {
                self.advance();

                Pat::common(
                    span,
                    ast::TextLit {
                        path: None,
                        lit: Spanned::new(span, literal),
                    }
                    .into(),
                )
            }
            PathHead!() => self.parse_path_or_namespaced_lit()?,
            OpeningCurlyBracket => {
                self.advance();
                self.fin_parse_rec_lit(None, span)?
            }
            OpeningRoundBracket => {
                self.advance();
                self.fin_parse_seq_lit_or_bracketed_item(None, span)?
            }
            _ => {
                self.expected(Expectation::Pat);
                return self.error();
            }
        };

        pattern.attrs.extend(attributes);

        Ok(pattern)
    }
}
