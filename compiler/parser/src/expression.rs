use crate::{
    base::{Annotation, Expectation, Parser, SkipLineBreaks},
    synonym::PathHead,
};
use ast::{Expression, ParameterKind};
use diagnostics::error::Result;
use lexer::token::BareToken::*;
use span::{Span, Spanned};
use utility::smallvec;

// @Task parse given bindings ("given/in")

impl Parser<'_> {
    /// Parse an expression.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Expression ::= Shorthand-Quantified-Type-Or-Lower
    /// ```
    pub(crate) fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_shorthand_quantified_type_or_lower()
    }

    /// Parse a shorthand [quantified type] or a lower expression.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// ; Among other things, the grammar for pretty printers differs from the one for parsers
    /// ; in that `Shorthand-Quantified-Type-Or-Lower` also includes several complex (in the sense of
    /// ; containing further expressions) `Lower-Expression`s namely let- and use-bindings, lambda literals,
    /// ; case analyses and do blocks.
    /// ;
    /// Shorthand-Quantified-Type-Or-Lower ::=
    ///     Application-Expression-Or-Lower
    ///     Quantifier
    ///     Shorthand-Quantified-Type-Or-Lower
    /// Quantifier ::= "->" | "**"
    /// ```
    ///
    /// [quantified type]: ast::QuantifiedType
    fn parse_shorthand_quantified_type_or_lower(&mut self) -> Result<Expression> {
        let domain = self.parse_application_expression_or_lower()?;
        let mut span = domain.span;

        let quantifier = match self.token() {
            ThinArrowRight => ast::Quantifier::Pi,
            DoubleAsterisk => ast::Quantifier::Sigma,
            // We don't actually have a quantified type but merely an application or lower.
            _ => return Ok(domain),
        };

        self.advance();

        // Using recursion to parse right-associatively.
        let codomain = span.merging(self.parse_shorthand_quantified_type_or_lower()?);

        Ok(Expression::common(
            span,
            ast::QuantifiedType {
                quantifier,
                parameters: smallvec![ast::Parameter::new(
                    domain.span,
                    ast::BareParameter {
                        kind: ParameterKind::Explicit,
                        binder: None,
                        type_: Some(domain),
                    },
                )],
                codomain,
            }
            .into(),
        ))
    }

    /// Parse an [application] (expression) or a lower expression.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Application-Expression-Or-Lower ::= Lower-Expression Expression-Argument*
    /// Expression-Argument ::=
    ///     "'"?
    ///     (Lower-Expression | "(" (#Word "=")? Expression ")")
    ///
    /// ; ; The left-recursive version of the rule above is unsuitable for a recursive descent parser.
    /// ; ; However, it is usable for pretty printers.
    /// ;
    /// ; Application-Expression-Or-Lower ::= Application-Expression-Or-Lower? Expression-Argument*
    /// ; Expression-Argument ::=
    /// ;     | Lower-Expression
    /// ;     | "'"? "(" (#Word "=")? Expression ")"
    /// ```
    ///
    /// [application]: ast::Application
    // @Task update grammar
    fn parse_application_expression_or_lower(&mut self) -> Result<Expression> {
        self.parse_application_or_lower()
    }

    /// Parse a lower expression.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Lower-Expression ::= Attribute* Bare-Lower-Expression
    /// Bare-Lower-Expression ::= Lowest-Expression ("::" Identifier)*
    /// Lowest-Expression ::=
    ///     | Wildcard
    ///     | #Number-Literal
    ///     | #Text-Literal
    ///     | Let-Binding
    ///     | Use-Binding
    ///     | Lambda-Literal
    ///     | Case-Analysis
    ///     | Do-Block
    ///     | Quantified-Type
    ///     | Sequence-Literal-Or-Bracketed-Expression
    ///     | Record-Literal-Expression
    ///     | Path-Or-Namespaced-Expression-Literal
    /// ```
    pub(crate) fn parse_lower_expression(&mut self) -> Result<Expression> {
        //
        // IMPORTANT: To be kept in sync with `pattern::LowerExpressionPrefix`.
        //

        let attributes = self.parse_attributes(SkipLineBreaks::No)?;

        let span = self.span();
        let mut expression = match self.token() {
            Underscore => {
                self.advance();

                Expression::common(span, ast::Wildcard::Silent.into())
            }
            QuestionMark => {
                self.advance();
                self.finish_parse_signaling_wildcard(span)?
            }
            NumberLiteral(literal) => {
                self.advance();

                Expression::common(
                    span,
                    ast::NumberLiteral {
                        path: None,
                        literal: Spanned::new(span, literal),
                    }
                    .into(),
                )
            }
            TextLiteral(literal) => {
                self.advance();

                Expression::common(
                    span,
                    ast::TextLiteral {
                        path: None,
                        literal: Spanned::new(span, literal),
                    }
                    .into(),
                )
            }
            PathHead!() => self.parse_path_or_namespaced_literal()?,
            ForUpper => {
                self.advance();
                self.finish_parse_quantified_type(span)?
            }
            ForLower => {
                self.advance();
                self.finish_parse_lambda_literal(span)?
            }
            Case => {
                self.advance();
                self.finish_parse_case_analysis(span)?
            }
            Do => {
                self.advance();
                self.finish_parse_do_block(span)?
            }
            Let => {
                self.advance();
                self.finish_parse_let_binding(span)?
            }
            Use => {
                self.advance();
                self.finish_parse_use_binding(span)?
            }
            OpeningCurlyBracket => {
                self.advance();
                self.finish_parse_record_literal(None, span)?
            }
            OpeningRoundBracket => {
                self.advance();
                self.finish_parse_sequence_literal_or_bracketed_item(None, span)?
            }
            _ => {
                self.expected(Expectation::Expression);
                return self.error();
            }
        };

        let mut attributes = Some(attributes);

        while self.consume(DoubleColon) {
            let field = self.parse_word()?;

            expression = Expression::new(
                attributes.take().unwrap_or_default(),
                expression.span.merge(&field),
                ast::Projection {
                    basis: expression,
                    field,
                }
                .into(),
            );
        }

        if let Some(attributes) = attributes {
            expression.attributes.extend(attributes);
        }

        Ok(expression)
    }

    /// Finish parsing a [lambda literal] given the span of the already parsed leading `for` keyword.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Lambda-Literal ::= "for" Parameters Type-Annotation? "=>" Expression
    /// ```
    ///
    /// [lambda literal]: ast::LambdaLiteral
    fn finish_parse_lambda_literal(&mut self, mut span: Span) -> Result<Expression> {
        let parameters = self.parse_parameters()?;
        let codomain = self.parse_optional_type_annotation()?;
        self.annotate(Annotation::LabelWhileParsing {
            span,
            name: "lambda literal",
        });
        self.parse_wide_arrow()?;
        let body = span.merging(self.parse_expression()?);

        Ok(Expression::common(
            span,
            ast::LambdaLiteral {
                parameters,
                codomain,
                body,
            }
            .into(),
        ))
    }

    /// Finish parsing a [quantified type] given the span of the already parsed leading `For` keyword.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Quantified-Type ::= "For" Parameter* Quantifier Expression
    /// ```
    ///
    /// [quantified type]: ast::QuantifiedType
    fn finish_parse_quantified_type(&mut self, mut span: Span) -> Result<Expression> {
        let parameters = self.parse_parameters()?;

        let quantifier = match self.token() {
            ThinArrowRight => ast::Quantifier::Pi,
            DoubleAsterisk => ast::Quantifier::Sigma,
            _ => {
                self.expected(ThinArrowRight);
                self.expected(DoubleAsterisk);

                self.annotate(Annotation::LabelWhileParsing {
                    span,
                    name: "quantified type",
                });

                if let WideArrowRight = self.token() {
                    // @Note users might have also thought that this was the way to denote
                    // a function type with context arguments.
                    // @Task add a note or suggestion to teach the actual syntax for those
                    // if there are apostrophes present, only suggest context parameters
                    self.annotate(Annotation::SuggestThinArrowForPiType { span: self.span() });
                }

                return self.error();
            }
        };

        self.advance();

        let codomain = span.merging(self.parse_expression()?);

        Ok(Expression::common(
            span,
            ast::QuantifiedType {
                quantifier,
                parameters,
                codomain,
            }
            .into(),
        ))
    }

    /// Finish parsing a [let-binding] given the span of the already parsed leading `let` keyword.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Let-Binding ::=
    ///     "let" Local-Binder Parameters Type-Annotation?
    ///     ("=" Expression)?
    ///     #Line-Break?
    ///     "in" Expression
    /// ```
    ///
    /// [let-binding]: ast::LetBinding
    fn finish_parse_let_binding(&mut self, mut span: Span) -> Result<Expression> {
        let binder = self.parse_local_binder()?;
        let parameters = self.parse_parameters()?;
        let type_ = self.parse_optional_type_annotation()?;

        let body = if self.consume(Equals) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        if let LineBreak = self.token() {
            self.advance();
        }

        self.expect(In)?;

        let scope = span.merging(self.parse_expression()?);

        Ok(Expression::common(
            span,
            ast::LetBinding {
                binder,
                parameters,
                type_,
                body,
                scope,
            }
            .into(),
        ))
    }

    /// Finish parsing a [use-binding] given the span of the already parsed leading `use` keyword.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Use-Binding ::=
    ///     "use" Use-Path-Tree
    ///     #Line-Break?
    ///     "in" Expression
    /// ```
    ///
    /// [use-binding]: ast::UseBinding
    fn finish_parse_use_binding(&mut self, span: Span) -> Result<Expression> {
        let bindings = self.parse_use_path_tree()?;

        if let LineBreak = self.token() {
            self.advance();
        }

        self.expect(In)?;

        let scope = self.parse_expression()?;

        Ok(Expression::common(
            span.merge(&scope),
            ast::UseBinding { bindings, scope }.into(),
        ))
    }

    /// Finish parsing a [case analysis] given the span of the already parsed leading `case` keyword.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Case-Analysis ::= "case" Expression "of" (#Indentation Case* #Dedentation)?
    /// Case ::= Pattern "=>" Expression Terminator
    /// ```
    ///
    /// [case analysis]: ast::CaseAnalysis
    fn finish_parse_case_analysis(&mut self, mut span: Span) -> Result<Expression> {
        let scrutinee = self.parse_expression()?;
        span.merging(self.expect(Of)?);

        let mut cases = Vec::new();

        if self.consume(Indentation) {
            // @Task use parse_block function for this (but don't trash the span!)

            while self.token() != Dedentation {
                let pattern = self.parse_pattern()?;
                self.parse_wide_arrow()?;
                let body = self.parse_expression()?;
                self.parse_terminator()?;

                cases.push(ast::Case { pattern, body });
            }

            span.merging(self.span());
            self.advance();
        }

        Ok(Expression::common(
            span,
            ast::CaseAnalysis { scrutinee, cases }.into(),
        ))
    }

    /// Finish parsing a [do-block] given the span of the already parsed leading `do` keyword.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Do-Block ::= "do" #Indentation Statement* #Dedentation
    /// Statement ::= Let-Statement | Use-Declaration | Expression-Statement
    /// Let-Statement ::=
    ///     "let" Local-Binder Parameter* Type-Annotation?
    ///     Binding-Mode
    ///     Expression Terminator
    /// Expression-Statement ::= Expression Terminator
    /// Binding-Mode ::=  "=" | "<-"
    /// ```
    ///
    /// [do-block]: ast::DoBlock
    fn finish_parse_do_block(&mut self, mut span: Span) -> Result<Expression> {
        let mut statements = Vec::new();

        self.expect(Indentation)?;

        while self.token() != Dedentation {
            // @Note necessary I guess in cases where we have #Line-Break ##Comment+ #Line-Break
            if self.consume(LineBreak) {
                continue;
            }

            statements.push(match self.token() {
                // @Task move to its own function
                Let => {
                    self.advance();
                    let binder = self.parse_local_binder()?;

                    let parameters = self.parse_parameters()?;
                    let type_ = self.parse_optional_type_annotation()?;

                    let body = match self.token() {
                        token @ (Equals | ThinArrowLeft) => {
                            let mode = match token {
                                Equals => ast::BindingMode::Plain,
                                ThinArrowLeft => ast::BindingMode::Effectful,
                                _ => unreachable!(),
                            };
                            self.advance();

                            Some((mode, self.parse_expression()?))
                        }
                        _ => None,
                    };

                    self.parse_terminator()?;

                    ast::Statement::Let(ast::LetStatement {
                        binder,
                        parameters,
                        type_,
                        body,
                    })
                }
                Use => {
                    self.advance();
                    let bindings = self.parse_use_path_tree()?;
                    self.parse_terminator()?;

                    ast::Statement::Use(ast::Use { bindings })
                }
                _ => {
                    self.expected(Expectation::Statement);

                    let expression = self.parse_expression()?;
                    self.parse_terminator()?;

                    ast::Statement::Expression(expression)
                }
            });
        }

        span.merging(self.span());
        self.advance();

        Ok(Expression::common(span, ast::DoBlock { statements }.into()))
    }
}
