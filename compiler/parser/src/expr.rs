use crate::{
    base::{Annotation, Expectation, Parser, SkipLineBreaks},
    synonym::PathHead,
};
use ast::{Expr, ParamKind};
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
    /// Expr ::= Shorthand-Quantified-Type-Or-Lower
    /// ```
    pub(crate) fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_shorthand_quantified_ty_or_lower()
    }

    /// Parse a shorthand [quantified type] or a lower expression.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// ; Among other things, the grammar for pretty printers differs from the one for parsers
    /// ; in that `Shorthand-Quantified-Type-Or-Lower` also includes several complex (in the sense of
    /// ; containing further expressions) `Lower-Expr`s namely let- and use-bindings, lambda literals,
    /// ; case analyses and do blocks.
    /// ;
    /// Shorthand-Quantified-Type-Or-Lower ::=
    ///     Application-Expr-Or-Lower
    ///     Quantifier
    ///     Shorthand-Quantified-Type-Or-Lower
    /// Quantifier ::= "->" | "**"
    /// ```
    ///
    /// [quantified type]: ast::QuantifiedType
    fn parse_shorthand_quantified_ty_or_lower(&mut self) -> Result<Expr> {
        let domain = self.parse_app_expr_or_lower()?;
        let mut span = domain.span;

        let quantifier = match self.token() {
            ThinArrowRight => ast::Quantifier::Pi,
            DoubleAsterisk => ast::Quantifier::Sigma,
            // We don't actually have a quantified type but merely an application or lower.
            _ => return Ok(domain),
        };

        self.advance();

        // Using recursion to parse right-associatively.
        let codomain = span.merging(self.parse_shorthand_quantified_ty_or_lower()?);

        Ok(Expr::common(
            span,
            ast::QuantifiedTy {
                quantifier,
                params: smallvec![ast::Param::new(
                    domain.span,
                    ast::BareParam {
                        kind: ParamKind::Explicit,
                        binder: None,
                        ty: Some(domain),
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
    /// Application-Expr-Or-Lower ::= Lower-Expr Expr-Argument*
    /// Expr-Argument ::=
    ///     "'"?
    ///     (Lower-Expr | "(" (#Word "=")? Expr ")")
    ///
    /// ; ; The left-recursive version of the rule above is unsuitable for a recursive descent parser.
    /// ; ; However, it is usable for pretty printers.
    /// ;
    /// ; Application-Expr-Or-Lower ::= Application-Expr-Or-Lower? Expr-Argument*
    /// ; Expr-Argument ::=
    /// ;     | Lower-Expr
    /// ;     | "'"? "(" (#Word "=")? Expr ")"
    /// ```
    ///
    /// [application]: ast::App
    // @Task update grammar
    fn parse_app_expr_or_lower(&mut self) -> Result<Expr> {
        self.parse_app_or_lower()
    }

    /// Parse a lower expression.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Lower-Expr ::= Attr* Bare-Lower-Expr
    /// Bare-Lower-Expr ::= Lowest-Expr ("::" Identifier)*
    /// Lowest-Expr ::=
    ///     | Wildcard
    ///     | #Number-Literal
    ///     | #Text-Literal
    ///     | Let-Binding
    ///     | Use-Binding
    ///     | Lambda-Literal
    ///     | Case-Analysis
    ///     | Do-Block
    ///     | Quantified-Type
    ///     | Sequence-Literal-Or-Bracketed-Expr
    ///     | Record-Literal-Expr
    ///     | Path-Or-Namespaced-Expr-Literal
    /// ```
    pub(crate) fn parse_lower_expr(&mut self) -> Result<Expr> {
        //
        // IMPORTANT: To be kept in sync with `pattern::LowerExpressionPrefix`.
        //

        let attrs = self.parse_attrs(SkipLineBreaks::No)?;

        let span = self.span();
        let mut expr = match self.token() {
            Underscore => {
                self.advance();

                Expr::common(span, ast::Wildcard::Silent.into())
            }
            QuestionMark => {
                self.advance();
                self.fin_parse_signaling_wildcard(span)?
            }
            NumLit(num) => {
                self.advance();

                Expr::common(
                    span,
                    ast::NumLit {
                        path: None,
                        lit: Spanned::new(span, num),
                    }
                    .into(),
                )
            }
            TextLit(text) => {
                self.advance();

                Expr::common(
                    span,
                    ast::TextLit {
                        path: None,
                        lit: Spanned::new(span, text),
                    }
                    .into(),
                )
            }
            PathHead!() => self.parse_path_or_namespaced_lit()?,
            ForUpper => {
                self.advance();
                self.finish_parse_quantified_ty(span)?
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
                self.fin_parse_rec_lit(None, span)?
            }
            OpeningRoundBracket => {
                self.advance();
                self.fin_parse_seq_lit_or_bracketed_item(None, span)?
            }
            _ => {
                self.expected(Expectation::Expr);
                return self.error();
            }
        };

        let mut attributes = Some(attrs);

        while self.consume(DoubleColon) {
            let field = self.parse_word()?;

            expr = Expr::new(
                attributes.take().unwrap_or_default(),
                expr.span.merge(&field),
                ast::Proj { basis: expr, field }.into(),
            );
        }

        if let Some(attributes) = attributes {
            expr.attrs.extend(attributes);
        }

        Ok(expr)
    }

    /// Finish parsing a [lambda literal] given the span of the already parsed leading `for` keyword.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Lambda-Literal ::= "for" Params Type-Annotation? "=>" Expr
    /// ```
    ///
    /// [lambda literal]: ast::LambdaLiteral
    fn finish_parse_lambda_literal(&mut self, mut span: Span) -> Result<Expr> {
        let parameters = self.parse_params()?;
        let codomain = self.parse_opt_ty_ann()?;
        self.annotate(Annotation::LabelWhileParsing {
            span,
            name: "lambda literal",
        });
        self.parse_wide_arrow()?;
        let body = span.merging(self.parse_expr()?);

        Ok(Expr::common(
            span,
            ast::LamLit {
                params: parameters,
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
    /// Quantified-Type ::= "For" Param* Quantifier Expr
    /// ```
    ///
    /// [quantified type]: ast::QuantifiedType
    fn finish_parse_quantified_ty(&mut self, mut span: Span) -> Result<Expr> {
        let parameters = self.parse_params()?;

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

        let codomain = span.merging(self.parse_expr()?);

        Ok(Expr::common(
            span,
            ast::QuantifiedTy {
                quantifier,
                params: parameters,
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
    ///     "let" Local-Binder Params Type-Annotation?
    ///     ("=" Expr)?
    ///     #Line-Break?
    ///     "in" Expr
    /// ```
    ///
    /// [let-binding]: ast::LetBinding
    fn finish_parse_let_binding(&mut self, mut span: Span) -> Result<Expr> {
        let binder = self.parse_local_binder()?;
        let parameters = self.parse_params()?;
        let ty = self.parse_opt_ty_ann()?;

        let body = if self.consume(Equals) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        if let LineBreak = self.token() {
            self.advance();
        }

        self.expect(In)?;

        let scope = span.merging(self.parse_expr()?);

        Ok(Expr::common(
            span,
            ast::LetBinding {
                binder,
                params: parameters,
                ty,
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
    ///     "in" Expr
    /// ```
    ///
    /// [use-binding]: ast::UseBinding
    fn finish_parse_use_binding(&mut self, span: Span) -> Result<Expr> {
        let bindings = self.parse_use_path_tree()?;

        if let LineBreak = self.token() {
            self.advance();
        }

        self.expect(In)?;

        let scope = self.parse_expr()?;

        Ok(Expr::common(
            span.merge(&scope),
            ast::UseBinding { bindings, scope }.into(),
        ))
    }

    /// Finish parsing a [case analysis] given the span of the already parsed leading `case` keyword.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Case-Analysis ::= "case" Expr "of" (#Indentation Case* #Dedentation)?
    /// Case ::= Pat "=>" Expr Terminator
    /// ```
    ///
    /// [case analysis]: ast::CaseAnalysis
    fn finish_parse_case_analysis(&mut self, mut span: Span) -> Result<Expr> {
        let scrutinee = self.parse_expr()?;
        span.merging(self.expect(Of)?);

        let mut cases = Vec::new();

        if self.consume(Indentation) {
            // @Task use parse_block function for this (but don't trash the span!)

            while self.token() != Dedentation {
                let pattern = self.parse_pattern()?;
                self.parse_wide_arrow()?;
                let body = self.parse_expr()?;
                self.parse_terminator()?;

                cases.push(ast::Case { pattern, body });
            }

            span.merging(self.span());
            self.advance();
        }

        Ok(Expr::common(
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
    /// Statement ::= Let-Statement | Use-Decl | Expr-Statement
    /// Let-Statement ::=
    ///     "let" Local-Binder Param* Type-Annotation?
    ///     Binding-Mode
    ///     Expr Terminator
    /// Expr-Statement ::= Expr Terminator
    /// Binding-Mode ::=  "=" | "<-"
    /// ```
    ///
    /// [do-block]: ast::DoBlock
    fn finish_parse_do_block(&mut self, mut span: Span) -> Result<Expr> {
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

                    let parameters = self.parse_params()?;
                    let ty = self.parse_opt_ty_ann()?;

                    let body = match self.token() {
                        token @ (Equals | ThinArrowLeft) => {
                            let mode = match token {
                                Equals => ast::BindingMode::Plain,
                                ThinArrowLeft => ast::BindingMode::Effectful,
                                _ => unreachable!(),
                            };
                            self.advance();

                            Some((mode, self.parse_expr()?))
                        }
                        _ => None,
                    };

                    self.parse_terminator()?;

                    ast::Statement::Let(ast::LetStatement {
                        binder,
                        params: parameters,
                        ty,
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

                    let expression = self.parse_expr()?;
                    self.parse_terminator()?;

                    ast::Statement::Expr(expression)
                }
            });
        }

        span.merging(self.span());
        self.advance();

        Ok(Expr::common(span, ast::DoBlock { statements }.into()))
    }
}
