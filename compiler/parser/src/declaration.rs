use crate::{
    base::{Expectation, Parser, SkipLineBreaks},
    synonym::Terminator,
};
use ast::{Attributes, Declaration, Identifier};
use diagnostics::{Diagnostic, error::Result};
use lexer::token::BareToken::*;
use span::{PossiblySpanning, Span, Spanning};

impl Parser<'_> {
    /// Parse the *top level*, i.e. the body of a module file.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Top-Level ::= (#Line-Break | Declaration)* #End-Of-Input
    /// ```
    pub(crate) fn parse_top_level(&mut self, module: Identifier) -> Result<Declaration> {
        let mut declarations = Vec::new();

        loop {
            // @Question shouldn't this be a `while` (+ grammar change)?
            if self.consume(LineBreak) {
                continue;
            }

            if self.consume(EndOfInput) {
                break Ok(Declaration::common(
                    self.map[self.file].span(),
                    ast::Module {
                        binder: module,
                        file: self.file,
                        declarations: Some(declarations),
                    }
                    .into(),
                ));
            }

            declarations.push(self.parse_declaration()?);
        }
    }

    /// Parse a declaration.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Declaration ::= (Attribute #Line-Break*)* Bare-Declaration
    /// Bare-Declaration ::=
    ///     | Function-Declaration
    ///     | Data-Declaration
    ///     | Module-Declaration
    ///     | Use-Declaration
    ///     | Given-Declaration
    /// ```
    fn parse_declaration(&mut self) -> Result<Declaration> {
        let attributes = self.parse_attributes(SkipLineBreaks::Yes)?;

        let span = self.span();
        match self.token() {
            Word(binder) => {
                let binder = ast::Identifier::new_unchecked(span, binder);
                self.advance();
                self.finish_parse_function_declaration(binder, attributes)
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

                self.finish_parse_data_declaration(kind, span, attributes)
            }
            Module => {
                self.advance();
                self.finish_parse_module_declaration(span, attributes)
            }
            Use => {
                self.advance();
                self.finish_parse_use_declaration(span, attributes)
            }
            Given => {
                self.advance();
                self.finish_parse_given_declaration(span, attributes)
            }
            _ => {
                self.expected(Expectation::Declaration);
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
    /// Function-Declaration ::=
    ///     #Word
    ///     Parameters Type-Annotation?
    ///     ("=" Expression)?
    ///     Terminator
    /// ```
    ///
    /// [function declaration]: ast::Function
    pub(crate) fn finish_parse_function_declaration(
        &mut self,
        binder: Identifier,
        attributes: Attributes,
    ) -> Result<ast::Item<ast::Function>> {
        let mut span = binder.span();

        let parameters = span.merging(self.parse_parameters()?);
        let type_ = span.merging(self.parse_optional_type_annotation()?);

        let body =
            if self.consume(Equals) { Some(span.merging(self.parse_expression()?)) } else { None };

        self.parse_terminator()?;

        Ok(ast::Item::new(attributes, span, ast::Function { binder, parameters, type_, body }))
    }

    /// Finish parsing a [data declaration] given the span of the already parsed leading keyword.
    ///
    /// The span of the parsed declaration does not include the terminator.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Data-Declaration ::=
    ///     Data-Kind
    ///     #Word
    ///     Parameters Type-Annotation?
    ///     ("of" (#Indentation (Terminator | Declaration)* #Dedentation)?)?
    ///     Terminator
    /// Data-Kind ::= "data" | "record" | "trait"
    /// ```
    ///
    /// [data declaration]: ast::Data
    fn finish_parse_data_declaration(
        &mut self,
        kind: ast::DataKind,
        mut span: Span,
        attributes: Attributes,
    ) -> Result<Declaration> {
        let binder = span.merging(self.parse_word()?);
        let parameters = span.merging(self.parse_parameters()?);
        let type_ = span.merging(self.parse_optional_type_annotation()?);

        let declarations = match self.token() {
            Of => {
                span.merging(self.span());
                self.advance();

                let declarations = self.parse_optional_block(Self::parse_declaration)?;

                span.merging(declarations.last());
                self.parse_terminator()?;

                Some(declarations)
            }
            terminator @ Terminator!() => {
                if terminator == LineBreak {
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

        Ok(Declaration::new(
            attributes,
            span.merge(&declarations),
            ast::Data { kind, binder, parameters, type_, declarations }.into(),
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
    /// Module-Declaration ::=
    ///     | Module-Header
    ///     | "module" #Word ("of" (#Indentation (Terminator | Declaration)* #Dedentation)?)? Terminator
    /// Module-Header ::= "module" Terminator
    /// ```
    ///
    /// [module declaration]: ast::Module
    fn finish_parse_module_declaration(
        &mut self,
        mut span: Span,
        attributes: Attributes,
    ) -> Result<Declaration> {
        // @Task abstract over this (used below as well), good idea?
        if let name @ Terminator!() = self.token() {
            if name == LineBreak {
                self.advance();
            }

            return Ok(Declaration::new(attributes, span, ast::BareDeclaration::ModuleHeader));
        }

        let binder = span.merging(self.parse_word()?);

        self.recover_from_module_parameters()?;

        match self.token() {
            // Out-of-line module declaration.
            // @Task abstract over this (used above as well), good idea?
            terminator @ Terminator!() => {
                if terminator == LineBreak {
                    self.advance();
                }

                Ok(Declaration::new(
                    attributes,
                    span,
                    ast::Module { binder, file: self.file, declarations: None }.into(),
                ))
            }
            Of => {
                span.merging(self.span());
                self.advance();

                let declarations = self.parse_optional_block(Self::parse_declaration)?;
                span.merging(declarations.last());
                self.parse_terminator()?;

                Ok(Declaration::new(
                    attributes,
                    span,
                    ast::Module { binder, file: self.file, declarations: Some(declarations) }
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

    fn recover_from_module_parameters(&mut self) -> Result {
        if let Some(parameters) = self.parse_parameters()?.possible_span() {
            // FIXME: Don't actually abort parsing (still taint tho), actually *recover* (i.e., continue).
            //        Requires adding "health" to the parser state.
            return Err(Diagnostic::error()
                .message("modules may not have parameters")
                .unlabeled_span(parameters)
                .report(self.reporter));
        };
        Ok(())
    }

    /// Finish parsing a [use-declaration] given the span of the already parsed leading `use` keyword.
    ///
    /// The span of the parsed declaration does not contain the terminator.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Use-Declaration ::= "use" Use-Path-Tree Terminator
    /// ```
    ///
    /// [use-declaration]: ast::Use
    fn finish_parse_use_declaration(
        &mut self,
        span: Span,
        attributes: Attributes,
    ) -> Result<Declaration> {
        let bindings = self.parse_use_path_tree()?;
        self.parse_terminator()?;

        Ok(Declaration::new(attributes, span.merge(&bindings), ast::Use { bindings }.into()))
    }

    /// Finish parsing a given-declaration given the span of the already parsed leading `given` keyword.
    ///
    /// The span of the parsed declaration does not include the terminator.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Given-Declaration ::=
    ///     "given"
    ///     #Word
    ///     Parameters Type-Annotation?
    ///     Given-Body?
    ///     Terminator
    /// Given-Body ::=
    ///     | "of" (#Indentation (Terminator | Declaration)* #Dedentation)?
    ///     | "=" Expression
    /// ```
    fn finish_parse_given_declaration(
        &mut self,
        mut span: Span,
        attributes: Attributes,
    ) -> Result<Declaration> {
        let binder = span.merging(self.parse_word()?);
        let parameters = span.merging(self.parse_parameters()?);
        let type_ = span.merging(self.parse_optional_type_annotation()?);

        let body = match self.token() {
            Of => {
                span.merging(self.span());
                self.advance();

                let fields = self.parse_optional_block(Self::parse_declaration)?;

                span.merging(fields.last());
                self.parse_terminator()?;

                Some(ast::Body::Block { fields })
            }
            terminator @ Terminator!() => {
                if terminator == LineBreak {
                    self.advance();
                }
                None
            }
            Equals => {
                self.advance();

                let body = span.merging(self.parse_expression()?);

                Some(ast::Body::Expression { body })
            }
            _ => {
                self.expected(Of);
                // Although technically we expected a terminator and not a simple line break,
                // the distinction is minor and "terminator" is too jargony in diagnostics.
                self.expected(LineBreak);

                return self.error();
            }
        };

        Ok(Declaration::new(
            attributes,
            span.merge(&body),
            ast::Given { binder, parameters, type_, body }.into(),
        ))
    }
}
