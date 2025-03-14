use crate::base::{Annotation, Expectation, Parser, SkipLineBreaks};
#[allow(clippy::wildcard_imports)] // To mirror `BareToken`
use crate::synonym::*;
use ast::Identifier;
use diagnostics::error::Result;
use lexer::token::BareToken::{self, *};
use span::{Span, Spanned, Spanning};
use utility::SmallVec;

impl Parser<'_> {
    /// Parse a use-path tree.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Use-Path-Tree ::=
    ///     | Path
    ///     | Path "." "(" (Use-Path-Tree ",")* ")"
    ///     | Path "as" Identifier
    /// ```
    // @Beacon @Task update grammar to use commas properly
    pub(crate) fn parse_use_path_tree(&mut self) -> Result<ast::UsePathTree> {
        let mut path = self.parse_path_head()?;

        while self.consume(Dot) {
            let mut span = self.span();
            match self.token() {
                Identifier!(segment) => {
                    self.advance();

                    path.segments.push(Identifier::new_unchecked(span, segment));
                }
                OpeningRoundBracket => {
                    self.advance();

                    let mut subpaths = Vec::new();

                    loop {
                        if let Some(bracket) = self.consume_span(ClosingRoundBracket) {
                            span.merging(bracket);
                            break;
                        }

                        subpaths.push(self.parse_use_path_tree()?);

                        if let Some(bracket) = self.consume_span(ClosingRoundBracket) {
                            span.merging(bracket);
                            break;
                        }

                        self.expect(Comma)?;
                    }

                    return Ok(ast::UsePathTree::new(
                        path.span().merge(&span),
                        ast::BareUsePathTree::Multiple { path, subpaths },
                    ));
                }
                _ => {
                    self.expected(Expectation::Identifier);
                    self.expected(OpeningRoundBracket);

                    return self.error();
                }
            }
        }

        let binder = if self.consume(As) { Some(self.parse_identifier()?) } else { None };

        Ok(ast::UsePathTree::new(
            path.span().merge(&binder),
            ast::BareUsePathTree::Single { target: path, binder },
        ))
    }

    /// Parse a path.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Path ::= Path-Head ("." Identifier)*
    /// ```
    pub(crate) fn parse_path(&mut self) -> Result<ast::Path> {
        let mut path = self.parse_path_head()?;

        while self.consume(Dot) {
            path.segments.push(self.parse_identifier()?);
        }

        Ok(path)
    }

    /// Parse the head of a path.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Path-Head ::= Path-Hanger | Identifier
    /// Path-Hanger ::= "extern" | "topmost" | "super" | "self"
    /// ```
    fn parse_path_head(&mut self) -> Result<ast::Path> {
        let span = self.span();
        match self.token() {
            Identifier!(head) => {
                self.advance();
                Ok(Identifier::new_unchecked(span, head).into())
            }
            token @ PathHanger!() => {
                self.advance();

                let head = match token {
                    Extern => ast::BareHanger::Extern,
                    Topmost => ast::BareHanger::Topmost,
                    Super => ast::BareHanger::Super,
                    Self_ => ast::BareHanger::Self_,
                    _ => unreachable!(),
                };

                Ok(ast::Hanger::new(span, head).into())
            }
            _ => {
                self.expected(Expectation::Path);
                self.error()
            }
        }
    }

    /// Parse a sequence of parameters.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Parameters ::= Parameter*
    /// Parameter ::=
    ///     | "'"? Local-Binder
    ///     | "'"? (Local-Binder Type-Annotation? ")"
    ///     | "[" (Local-Binder ":")? Expression "]"
    /// ```
    // @Task update grammar
    pub(crate) fn parse_parameters(&mut self) -> Result<ast::Parameters> {
        let mut parameters = SmallVec::new();

        loop {
            let apostrophe = self.consume_span(Apostrophe);
            let mut span = self.span().merge_into(&apostrophe);

            let parameter = match self.token() {
                Word(binder) => {
                    let binder = Identifier::new_unchecked(self.span(), binder).into();
                    self.advance();

                    ast::Parameter::new(
                        span,
                        ast::BareParameter {
                            kind: ast::ParameterKind::from_apostrophe(apostrophe),
                            binder: Some(binder),
                            type_: None,
                        },
                    )
                }
                Underscore => {
                    let binder = ast::LocalBinder::Discarded(self.span());
                    self.advance();

                    ast::Parameter::new(
                        span,
                        ast::BareParameter {
                            kind: ast::ParameterKind::from_apostrophe(apostrophe),
                            binder: Some(binder),
                            type_: None,
                        },
                    )
                }
                OpeningRoundBracket => {
                    self.advance();

                    let binder = self.parse_local_binder()?;
                    let type_ = self.parse_optional_type_annotation()?;

                    span.merging(self.expect(ClosingRoundBracket)?);

                    ast::Parameter::new(
                        span,
                        ast::BareParameter {
                            kind: ast::ParameterKind::from_apostrophe(apostrophe),
                            binder: Some(binder),
                            type_,
                        },
                    )
                }
                OpeningSquareBracket => {
                    self.advance();

                    let binder = if let Some(Colon) = self.look_ahead(1) {
                        match self.token() {
                            Word(word) => {
                                let binder = Identifier::new_unchecked(self.span(), word).into();
                                self.advance(); // #Word
                                self.advance(); // ":"

                                Some(binder)
                            }
                            Underscore => {
                                let binder = ast::LocalBinder::Discarded(self.span());
                                self.advance(); // "_"
                                self.advance(); // ":"

                                Some(binder)
                            }
                            _ => None,
                        }
                    } else {
                        None
                    };

                    let type_ = self.parse_expression()?;

                    span.merging(self.expect(ClosingSquareBracket)?);

                    if let Some(_apostrophe) = apostrophe {
                        // @Beacon @Task parse error
                        todo!()
                    }

                    ast::Parameter::new(
                        span,
                        ast::BareParameter {
                            kind: ast::ParameterKind::Context,
                            binder,
                            type_: Some(type_),
                        },
                    )
                }
                _ => {
                    self.expected(Expectation::Parameter);
                    break;
                }
            };

            parameters.push(parameter);
        }

        Ok(parameters)
    }

    /// Finish parsing a [sequence literal] given the already parsed leading path and the
    /// span of the already parsed leading `(`.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Sequence-Literal-Or-Bracketed-⟪Item⟫ ::= (Path ".")? "(" (⟪Item⟫ ",")* ")"
    /// ```
    ///
    /// [sequence literal]: ast::SequenceLiteral
    // @Task update docs + grammar wrt comma
    pub(crate) fn finish_parse_sequence_literal_or_bracketed_item<T>(
        &mut self,
        path: Option<ast::Path>,
        mut span: Span,
    ) -> Result<ast::Item<T>>
    where
        T: Item + From<ast::SequenceLiteral<ast::Item<T>>>,
    {
        let mut elements = Vec::new();

        loop {
            if let Some(bracket) = self.consume_span(ClosingRoundBracket) {
                span.merging(bracket);
                break;
            }

            let mut item = self.parse_item()?;

            let comma = self.consume(Comma);
            let bracket = self.consume_span(ClosingRoundBracket);

            if let Some(bracket) = bracket {
                span.merging(bracket);

                // We don't actually have a sequence literal but a bracketed item.
                if elements.is_empty() && !comma && path.is_none() {
                    item.span = span;

                    return Ok(item);
                }
            }

            elements.push(item);

            if bracket.is_some() {
                break;
            }
        }

        Ok(ast::Item::common(
            span.merge_into(&path),
            ast::SequenceLiteral { path, elements: Spanned::new(span, elements) }.into(),
        ))
    }

    /// Finish parsing a [record literal] given the already parsed leading path and the
    /// span of the already parsed leading `{`.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Record-Literal-⟪Item⟫ ::=
    ///     "{"
    ///     ; @Task generalize #Word to "paths" (i.e. identifiers with "." OR "::" seps!)
    ///     (#Word ("=" ⟪Item⟫)? ",")* ; @Task make trailing comma optional & don't allow it if ";" follows
    ///     (";" ⟪Item⟫)?
    ///     "}"
    /// ```
    ///
    /// [record literal]: ast::RecordLiteral
    pub(crate) fn finish_parse_record_literal<T>(
        &mut self,
        path: Option<ast::Path>,
        mut span: Span,
    ) -> Result<ast::Item<T>>
    where
        T: Item + From<ast::RecordLiteral<ast::Item<T>>>,
    {
        let mut fields = Vec::new();
        let mut base = None;

        loop {
            if let Some(bracket) = self.consume_span(ClosingCurlyBracket) {
                span.merging(bracket);
                break;
            }

            let binder = self.parse_word()?;
            let mut body = None;

            if self.consume(Equals) {
                body = Some(self.parse_item()?);
            }

            fields.push(ast::Field { binder, body });

            // FIXME: Temporary syntax. This used to be a `;` but now is `,,`
            //        because (single) semicolons now denote doc comments.
            //        Find a proper replacement!
            if self.consume(Comma) && self.consume(Comma) {
                base = Some(self.parse_item()?);
                span.merging(self.expect(ClosingCurlyBracket)?);
                break;
            }

            if let Some(bracket) = self.consume_span(ClosingCurlyBracket) {
                span.merging(bracket);
                break;
            }

            self.expect(Comma)?;
        }

        Ok(ast::Item::common(
            span,
            ast::RecordLiteral { path, fields: Spanned::new(span, fields), base }.into(),
        ))
    }

    /// Parse an [application] or a lower item.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Application-⟪Item⟫-Or-Lower ::= Lower-⟪Item⟫ ⟪Item⟫-Argument*
    /// ⟪Item⟫-Argument ::=
    ///     "'"?
    ///     (Lower-⟪Item⟫ | "(" (#Word "=")? ⟪Item⟫ ")")
    /// ```
    ///
    /// [application]: ast::Application
    // @Task update grammar
    pub(crate) fn parse_application_or_lower<T>(&mut self) -> Result<ast::Item<T>>
    where
        T: Item + From<ast::Application<ast::Item<T>>>,
    {
        let mut callee = self.parse_lower_item()?;
        let mut span = callee.span;

        // Using iteration to parse left-associatively.
        loop {
            let apostrophe = self.consume_span(Apostrophe);

            //
            // Parse an optional argument.
            //
            let (binder, kind, argument) = if self.token() == OpeningSquareBracket {
                self.advance();

                let binder = if let Word(binder) = self.token()
                    && self.look_ahead(1) == Some(Equals)
                {
                    let span = self.span();
                    self.advance(); // #Word
                    self.advance(); // "="

                    Some(Identifier::new_unchecked(span, binder))
                } else {
                    None
                };

                let argument = self.parse_item()?;

                span.merging(self.expect(ClosingSquareBracket)?);

                if let Some(_apostrophe) = apostrophe {
                    // @Beacon @Task parse error
                    todo!()
                }

                (binder, ast::ParameterKind::Context, argument)
            } else if self.token() == OpeningRoundBracket
                && let Some(Word(binder)) = self.look_ahead(1)
                && self.look_ahead(2) == Some(Equals)
            {
                self.advance(); // "("
                let binder = Identifier::new_unchecked(self.span(), binder);
                self.advance(); // #Word
                self.advance(); // "="

                let argument = self.parse_item()?;

                span.merging(self.expect(ClosingRoundBracket)?);

                let kind = ast::ParameterKind::from_apostrophe(apostrophe);

                (Some(binder), kind, argument)
            } else if T::is_lower_prefix(self.token()) {
                let kind = ast::ParameterKind::from_apostrophe(apostrophe);

                (None, kind, span.merging(self.parse_lower_item()?))
            } else {
                self.expected(Expectation::Argument);
                self.annotate(Annotation::LabelWhileParsing {
                    span: callee.span,
                    name: T::KIND.name(),
                });

                if let ItemKind::Pattern = T::KIND
                    && let Let = self.token()
                    && let Some(Word(binder)) = self.look_ahead(1)
                {
                    let span = self.span().merge(&self.succeeding(1));

                    self.annotate(Annotation::SuggestBracketsAroundLetBindingPattern {
                        span,
                        binder,
                    });

                    return self.error();
                } else if let Some(span) = apostrophe {
                    self.annotate(Annotation::LabelApostrophe { span });
                    return self.error();
                }

                // The current token is not the start of an argument.
                // Hence we are done here.
                return Ok(callee);
            };

            callee =
                ast::Item::common(span, ast::Application { callee, kind, binder, argument }.into());
        }
    }

    /// Parse a path or a number, text or sequence literal prefixed with a path.
    ///
    /// ## Grammar
    ///
    /// ```grammar
    /// Path-Or-Namespaced-⟪Item⟫-Literal ::= Path ("." ⟪Item⟫-Literal)?
    /// ⟪Item⟫-Literal ::=
    ///     | #Number-Literal
    ///     | #Text-Literal
    ///     | Sequence-Literal-⟪Item⟫
    ///     | Record-Literal-⟪Item⟫
    /// ```
    pub(crate) fn parse_path_or_namespaced_literal<T>(&mut self) -> Result<ast::Item<T>>
    where
        T: Item
            + From<ast::NumberLiteral>
            + From<ast::TextLiteral>
            + From<ast::SequenceLiteral<ast::Item<T>>>
            + From<ast::RecordLiteral<ast::Item<T>>>
            + From<ast::Path>,
    {
        let mut path = self.parse_path_head()?;

        while self.consume(Dot) {
            let span = self.span();
            match self.token() {
                Identifier!(segment) => {
                    self.advance();
                    path.segments.push(Identifier::new_unchecked(span, segment));
                }
                NumberLiteral(literal) => {
                    self.advance();

                    return Ok(ast::Item::common(
                        path.span().merge(&span),
                        ast::NumberLiteral {
                            path: Some(path),
                            literal: Spanned::new(span, literal),
                        }
                        .into(),
                    ));
                }
                TextLiteral(literal) => {
                    self.advance();

                    return Ok(ast::Item::common(
                        path.span().merge(&span),
                        ast::TextLiteral { path: Some(path), literal: Spanned::new(span, literal) }
                            .into(),
                    ));
                }
                OpeningRoundBracket => {
                    self.advance();
                    return self.finish_parse_sequence_literal_or_bracketed_item(Some(path), span);
                }
                OpeningCurlyBracket => {
                    self.advance();
                    return self.finish_parse_record_literal(Some(path), span);
                }
                _ => {
                    self.expected(Expectation::Identifier);
                    self.expected(Expectation::NumberLiteral);
                    self.expected(Expectation::TextLiteral);
                    self.expected(OpeningRoundBracket);
                    self.expected(OpeningCurlyBracket);

                    return self.error();
                }
            }
        }

        Ok(ast::Item::common(path.span(), path.into()))
    }

    /// Finish parsing a signaling wildcard.
    pub(crate) fn finish_parse_signaling_wildcard<T>(&mut self, span: Span) -> Result<ast::Item<T>>
    where
        T: From<ast::Wildcard>,
    {
        let tag = self.parse_word()?;

        Ok(ast::Item::common(span.merge(&tag), ast::Wildcard::Signaling { tag }.into()))
    }

    fn parse_item<T: Item>(&mut self) -> Result<ast::Item<T>> {
        T::parse(self)
    }

    fn parse_lower_item<T: Item>(&mut self) -> Result<ast::Item<T>> {
        T::parse_lower(self)
    }

    /// Parse an optional type annotation.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Type-Annotation ::= ":" Expression
    /// ```
    pub(crate) fn parse_optional_type_annotation(&mut self) -> Result<Option<ast::Expression>> {
        self.consume(Colon).then(|| self.parse_expression()).transpose()
    }

    /// Parse attributes.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Attribute ::= Regular-Attribute | #Documentation-Comment
    /// ```
    pub(crate) fn parse_attributes(
        &mut self,
        skip_line_breaks: SkipLineBreaks,
    ) -> Result<ast::Attributes> {
        let mut attributes = ast::Attributes::default();

        loop {
            let span = self.span();
            attributes.push(match self.token() {
                At => {
                    self.advance();
                    let attribute = self.finish_parse_regular_attribute(span)?;
                    if skip_line_breaks == SkipLineBreaks::Yes {
                        while self.consume(LineBreak) {}
                    }
                    attribute
                }
                DocComment => {
                    self.advance();
                    if skip_line_breaks == SkipLineBreaks::Yes {
                        while self.consume(LineBreak) {}
                    }
                    ast::Attribute::new(span, ast::BareAttribute::Documentation)
                }
                _ => {
                    // We could technically add the expectation of *attributes* here but
                    // since they can be ascribed to "almost anything", I figure it would
                    // just add noise to the diagnostics.
                    break;
                }
            });
        }

        Ok(attributes)
    }

    /// Finish parsing a regular attribute given the span of the already parsed leading `@` symbol.
    ///
    /// # Grammar
    ///
    /// Note: The grammar is not complete yet since we cannot represent the
    /// arguments of `@if` yet which are the most complex.
    ///
    /// ```grammar
    /// Regular-Attribute ::= "@" (#Word | "(" #Word Attribute-Argument* ")")
    /// ```
    fn finish_parse_regular_attribute(&mut self, mut span: Span) -> Result<ast::Attribute> {
        let mut arguments = SmallVec::new();

        let binder = match self.token() {
            Word(binder) => {
                let binder = Identifier::new_unchecked(self.span(), binder);

                span.merging(&binder);
                self.advance();

                binder
            }
            OpeningRoundBracket => {
                self.advance();
                let binder = self.parse_word()?;

                // @Task create & use higher-order parse_delimited
                while self.token() != ClosingRoundBracket {
                    arguments.push(self.parse_attribute_argument()?);
                }

                span.merging(self.span());
                self.advance(); // ")"

                binder
            }
            _ => {
                self.expected(Expectation::Word);
                self.expected(OpeningRoundBracket);
                self.annotate(Annotation::LabelWhileParsing { span, name: "attribute" });

                return self.error();
            }
        };

        Ok(ast::Attribute::new(span, ast::BareAttribute::Regular { binder, arguments }))
    }

    /// Parse an argument of an attribute.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Attribute-Argument ::= Lower-Attribute-Argument | "(" #Word Lower-Attribute-Argument ")"
    /// Lower-Attribute-Argument ::= Path | #Number-Literal | #Text-Literal
    /// ```
    fn parse_attribute_argument(&mut self) -> Result<ast::AttributeArgument> {
        let mut span;
        let argument = match self.token() {
            PathHead!() => {
                let path = self.parse_path()?;
                span = path.span();

                ast::BareAttributeArgument::Path(Box::new(path))
            }
            NumberLiteral(literal) => {
                span = self.span();
                self.advance();

                ast::BareAttributeArgument::NumberLiteral(literal)
            }
            TextLiteral(literal) => {
                span = self.span();
                self.advance();

                ast::BareAttributeArgument::TextLiteral(literal)
            }
            OpeningRoundBracket => {
                span = self.span();
                self.advance();
                let binder = self.parse_word()?;
                let value = self.parse_attribute_argument()?;
                span.merging(self.expect(ClosingRoundBracket)?);

                ast::BareAttributeArgument::Named(Box::new(ast::NamedAttributeArgument {
                    binder,
                    value,
                }))
            }
            _ => {
                self.expected(Expectation::Argument);
                return self.error();
            }
        };

        Ok(ast::AttributeArgument::new(span, argument))
    }

    /// Parse an identifier.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Identifier ::= #Word | #Symbol
    /// ```
    pub(crate) fn parse_identifier(&mut self) -> Result<Identifier> {
        if let Identifier!(identifier) = self.token() {
            let span = self.span();
            self.advance();
            Ok(Identifier::new_unchecked(span, identifier))
        } else {
            self.expected(Expectation::Identifier);
            self.error()
        }
    }

    pub(crate) fn parse_word(&mut self) -> Result<Identifier> {
        if let Word(word) = self.token() {
            let span = self.span();
            self.advance();
            Ok(Identifier::new_unchecked(span, word))
        } else {
            self.expected(Expectation::Word);
            self.error()
        }
    }

    /// Parse a local binder.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Local-Binder ::= #Word | "_"
    /// ```
    pub(crate) fn parse_local_binder(&mut self) -> Result<ast::LocalBinder> {
        match self.token() {
            Word(word) => {
                let binder = Identifier::new_unchecked(self.span(), word).into();
                self.advance();
                Ok(binder)
            }
            Underscore => {
                let binder = ast::LocalBinder::Discarded(self.span());
                self.advance();
                Ok(binder)
            }
            _ => {
                self.expected(Expectation::Word);
                self.expected(Underscore);

                self.error()
            }
        }
    }

    pub(crate) fn parse_optional_block<T>(
        &mut self,
        mut parser: impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<Vec<T>> {
        let mut contents = Vec::new();

        if self.consume(Indentation) {
            loop {
                while self.consume(LineBreak) {}

                if self.consume(Dedentation) {
                    break;
                }

                contents.push(parser(self)?);
            }
        }

        Ok(contents)
    }

    /// Parse a terminator.
    ///
    /// If the terminator is a line break, [advance] past it. Otherwise, don't.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// ; #Start-Of-Input is not actually emitted by the lexer, the parser needs to bound-check instead.
    /// ;
    /// Terminator ::= #Line-Break
    ///     | (> #Dedentation | #End-Of-Input)
    ///     | (< #Start-Of-Input | #Line-Break | #Dedentation)
    /// ```
    ///
    /// [advance]: Self::advance
    // @Task now with delimited sections being gone, we might be able to simplify the
    // definition of a terminator, maybe?
    pub(crate) fn parse_terminator(&mut self) -> Result<Option<Span>> {
        if let Terminator!() = self.token() {
            return Ok(self.consume_span(LineBreak));
        }
        if let None | Some(Terminator!()) = self.look_behind(1) {
            return Ok(self.consume_span(LineBreak));
        }

        // Although technically we expected a terminator and not a simple line break,
        // the distinction is minor and "terminator" is too jargony in diagnostics.
        self.expected(LineBreak);
        self.error()
    }

    pub(crate) fn parse_wide_arrow(&mut self) -> Result<()> {
        if let ThinArrowRight = self.token() {
            self.annotate(Annotation::SuggestWideArrow { span: self.span() });
        }

        self.expect(WideArrowRight)?;
        Ok(())
    }
}

/// # Grammar
///
/// ```grammar
/// Wildcard ::= Silent-Wildcard | Signaling-Wildcard
/// Silent-Wildcard ::= "_"
/// Signaling-Wildcard ::= "?" #Word
///
/// ⟪⟪ Item ::= "Expression" | "Pattern" ⟫⟫
/// ```
impl Parser<'_> {}

/// An [item] in the narrowest sense, i.e. an [`Expression`] or a [`Pattern`].
///
/// This is the trait pendant to [`ItemKind`] and exists to allow for better-looking
/// parser APIs that are explicitly tailored towards constant folding & inlining.
///
/// [item]: span::item::Item
pub(crate) trait Item: Sized {
    const KIND: ItemKind;

    /// Parse the item. Do not invoke this directly. Prefer `Parser::parse_item`.
    fn parse(parser: &mut Parser<'_>) -> Result<ast::Item<Self>>;
    /// Parse the lower form of the item. Do not invoke this directly. Prefer `Parser::parse_lower_item`.
    fn parse_lower(parser: &mut Parser<'_>) -> Result<ast::Item<Self>>;
    fn is_lower_prefix(token: BareToken) -> bool;
}

impl Item for ast::BareExpression {
    const KIND: ItemKind = ItemKind::Expression;

    fn parse(parser: &mut Parser<'_>) -> Result<ast::Expression> {
        parser.parse_expression()
    }
    fn parse_lower(parser: &mut Parser<'_>) -> Result<ast::Expression> {
        parser.parse_lower_expression()
    }

    fn is_lower_prefix(token: BareToken) -> bool {
        matches!(token, LowerExpressionPrefix!())
    }
}

impl Item for ast::BarePattern {
    const KIND: ItemKind = ItemKind::Pattern;

    fn parse(parser: &mut Parser<'_>) -> Result<ast::Pattern> {
        parser.parse_pattern()
    }
    fn parse_lower(parser: &mut Parser<'_>) -> Result<ast::Pattern> {
        parser.parse_lower_pattern()
    }

    fn is_lower_prefix(token: BareToken) -> bool {
        matches!(token, LowerPatternPrefix!())
    }
}

#[derive(Clone, Copy)]
pub(crate) enum ItemKind {
    Expression,
    Pattern,
}

impl ItemKind {
    pub(crate) const fn name(self) -> &'static str {
        match self {
            Self::Expression => "expression",
            Self::Pattern => "pattern",
        }
    }
}
