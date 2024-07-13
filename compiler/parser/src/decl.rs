use crate::{
    base::{Expectation, Parser, SkipLineBreaks},
    synonym::Terminator,
};
use ast::{Attrs, Decl, Ident};
use diagnostics::error::Result;
use lexer::token::BareToken::*;
use span::{Span, Spanning};

impl Parser<'_> {
    /// Parse the *top level*, i.e. the body of a module file.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Top-Level ::= (#Line-Break | Decl)* #End-Of-Input
    /// ```
    pub(crate) fn parse_top_level(&mut self, module: Ident) -> Result<Decl> {
        let mut decls = Vec::new();

        loop {
            // @Question shouldn't this be a `while` (+ grammar change)?
            if self.consume(LineBreak) {
                continue;
            }

            if self.consume(EndOfInput) {
                break Ok(Decl::common(
                    self.map[self.file].span(),
                    ast::Module {
                        binder: module,
                        file: self.file,
                        decls: Some(decls),
                    }
                    .into(),
                ));
            }

            decls.push(self.parse_decl()?);
        }
    }

    /// Parse a declaration.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Decl ::= (Attr #Line-Break*)* Bare-Decl
    /// Bare-Decl ::=
    ///     | Function-Decl
    ///     | Data-Decl
    ///     | Module-Decl
    ///     | Use-Decl
    ///     | Given-Decl
    /// ```
    fn parse_decl(&mut self) -> Result<Decl> {
        let attrs = self.parse_attrs(SkipLineBreaks::Yes)?;

        let span = self.span();
        match self.token() {
            Word(binder) => {
                let binder = ast::Ident::new_unchecked(span, binder);
                self.advance();
                self.fin_parse_function_decl(binder, attrs)
                    .map(|function| function.map(Into::into))
            }
            token @ (Data | Record | Trait) => {
                self.advance();

                let kind = match token {
                    Data => ast::DataKind::Data,
                    Record => ast::DataKind::Record,
                    Trait => ast::DataKind::Trait,
                    _ => unreachable!(),
                };

                self.fin_parse_data_decl(kind, span, attrs)
            }
            Module => {
                self.advance();
                self.fin_parse_module_decl(span, attrs)
            }
            Use => {
                self.advance();
                self.fin_parse_use_decl(span, attrs)
            }
            Given => {
                self.advance();
                self.fin_parse_given_decl(span, attrs)
            }
            _ => {
                self.expected(Expectation::Decl);
                self.error()
            }
        }
    }

    /// Finish parsing a [function declaration] given the already parsed leading word.
    ///
    /// The span of the parsed declaration does not include the terminator.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Function-Decl ::=
    ///     #Word
    ///     Params Type-Annotation?
    ///     ("=" Expr)?
    ///     Terminator
    /// ```
    ///
    /// [function declaration]: ast::Function
    pub(crate) fn fin_parse_function_decl(
        &mut self,
        binder: Ident,
        attrs: Attrs,
    ) -> Result<ast::Item<ast::Func>> {
        let mut span = binder.span();

        let params = span.merging(self.parse_params()?);
        let ty = span.merging(self.parse_opt_ty_ann()?);

        let body = if self.consume(Equals) {
            Some(span.merging(self.parse_expr()?))
        } else {
            None
        };

        self.parse_terminator()?;

        Ok(ast::Item::new(
            attrs,
            span,
            ast::Func {
                binder,
                params,
                ty,
                body,
            },
        ))
    }

    /// Finish parsing a [data declaration] given the span of the already parsed leading keyword.
    ///
    /// The span of the parsed declaration does not include the terminator.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Data-Decl ::=
    ///     Data-Kind
    ///     #Word
    ///     Params Type-Annotation?
    ///     ("of" (#Indentation (Terminator | Decl)* #Dedentation)?)?
    ///     Terminator
    /// Data-Kind ::= "data" | "record" | "trait"
    /// ```
    ///
    /// [data declaration]: ast::Data
    fn fin_parse_data_decl(
        &mut self,
        kind: ast::DataKind,
        mut span: Span,
        attrs: Attrs,
    ) -> Result<Decl> {
        let binder = span.merging(self.parse_word()?);
        let params = span.merging(self.parse_params()?);
        let ty = span.merging(self.parse_opt_ty_ann()?);

        let decls = match self.token() {
            Of => {
                span.merging(self.span());
                self.advance();

                let decls = self.parse_opt_block(Self::parse_decl)?;

                span.merging(decls.last());
                self.parse_terminator()?;

                Some(decls)
            }
            name @ Terminator!() => {
                if name == LineBreak {
                    self.advance();
                }
                None
            }
            _ => {
                self.expected(Of);
                // Although technically we expected a terminator and not a simple line break,
                // the distinction is minor and "terminator" is too jargony in diagnostics.
                self.expected(LineBreak);

                return self.error();
            }
        };

        Ok(Decl::new(
            attrs,
            span.merge(&decls),
            ast::DataTy {
                kind,
                binder,
                params,
                ty,
                decls,
            }
            .into(),
        ))
    }

    /// Finish parsing [module declaration] given the span of the already parsed leading `module` keyword.
    ///
    /// This is either a module header, an inline or an out-of-line module declaration.
    /// The span of the parsed declaration does not include the terminator.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Module-Decl ::=
    ///     | Module-Header
    ///     | "module" #Word ("of" (#Indentation (Terminator | Decl)* #Dedentation)?)? Terminator
    /// Module-Header ::= "module" Terminator
    /// ```
    ///
    /// [module declaration]: ast::Module
    fn fin_parse_module_decl(&mut self, mut span: Span, attrs: Attrs) -> Result<Decl> {
        // @Task abstract over this (used below as well), good idea?
        if let name @ Terminator!() = self.token() {
            if name == LineBreak {
                self.advance();
            }

            return Ok(Decl::new(attrs, span, ast::BareDecl::ModuleHeader));
        }

        let binder = span.merging(self.parse_word()?);

        match self.token() {
            // Out-of-line module declaration.
            // @Task abstract over this (used above as well), good idea?
            name @ Terminator!() => {
                if name == LineBreak {
                    self.advance();
                }

                Ok(Decl::new(
                    attrs,
                    span,
                    ast::Module {
                        binder,
                        file: self.file,
                        decls: None,
                    }
                    .into(),
                ))
            }
            Of => {
                span.merging(self.span());
                self.advance();

                let decls = self.parse_opt_block(Self::parse_decl)?;
                span.merging(decls.last());
                self.parse_terminator()?;

                Ok(Decl::new(
                    attrs,
                    span,
                    ast::Module {
                        binder,
                        file: self.file,
                        decls: Some(decls),
                    }
                    .into(),
                ))
            }
            _ => {
                self.expected(Of);
                // Although technically we expected a terminator and not a simple line break,
                // the distinction is minor and "terminator" is too jargony in diagnostics.
                self.expected(LineBreak);

                self.error()
            }
        }
    }

    /// Finish parsing a [use-declaration] given the span of the already parsed leading `use` keyword.
    ///
    /// The span of the parsed declaration does not contain the terminator.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Use-Decl ::= "use" Use-Path-Tree Terminator
    /// ```
    ///
    /// [use-declaration]: ast::Use
    fn fin_parse_use_decl(&mut self, span: Span, attrs: Attrs) -> Result<Decl> {
        let bindings = self.parse_use_path_tree()?;
        self.parse_terminator()?;

        Ok(Decl::new(
            attrs,
            span.merge(&bindings),
            ast::Use { bindings }.into(),
        ))
    }

    /// Finish parsing a given-declaration given the span of the already parsed leading `given` keyword.
    ///
    /// The span of the parsed declaration does not include the terminator.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Given-Decl ::=
    ///     "given"
    ///     #Word
    ///     Params Type-Annotation?
    ///     Given-Body?
    ///     Terminator
    /// Given-Body ::=
    ///     | "of" (#Indentation (Terminator | Decl)* #Dedentation)?
    ///     | "=" Expr
    /// ```
    fn fin_parse_given_decl(&mut self, mut span: Span, attrs: Attrs) -> Result<Decl> {
        let binder = span.merging(self.parse_word()?);
        let params = span.merging(self.parse_params()?);
        let ty = span.merging(self.parse_opt_ty_ann()?);

        let body = match self.token() {
            Of => {
                span.merging(self.span());
                self.advance();

                let fields = self.parse_opt_block(Self::parse_decl)?;

                span.merging(fields.last());
                self.parse_terminator()?;

                Some(ast::Body::Block { fields })
            }
            name @ Terminator!() => {
                if name == LineBreak {
                    self.advance();
                }
                None
            }
            Equals => {
                self.advance();

                let body = span.merging(self.parse_expr()?);

                Some(ast::Body::Expr { body })
            }
            _ => {
                self.expected(Of);
                // Although technically we expected a terminator and not a simple line break,
                // the distinction is minor and "terminator" is too jargony in diagnostics.
                self.expected(LineBreak);

                return self.error();
            }
        };

        Ok(Decl::new(
            attrs,
            span.merge(&body),
            ast::Given {
                binder,
                params,
                ty,
                body,
            }
            .into(),
        ))
    }
}
