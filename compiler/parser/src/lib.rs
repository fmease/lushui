//! The syntactic analyzer (parser).
//!
//! It is a handwritten top-down recursive-descent parser with arbitrary look-ahead
//! and backtracking. Backtracking is only used to parse expressions and patterns.
//!
//! # Grammar Notation
//!
//! Most parsing functions in this module are accompanied by a grammar snippet.
//! These snippets are written in an EBNF-flavor explained below:
//!
//! | Notation  | Name                                | Definition or Remark                                          |
//! |-----------|-------------------------------------|---------------------------------------------------------------|
//! | `; C`     | Comment                             | Stretches until the end of the line                           |
//! | `N ::= R` | Definition                          | Defines non-terminal `A` by rule `R`                          |
//! | `A B`     | Sequence                            | Rule `B` immediately followed by rule `A` modulo lexed tokens |
//! | `(A)`     | Grouping                            | To escape default precedence                                  |
//! | <code>A &vert; B</code>   | Ordered Alternative                 | Either `A` or `B` first trying `A` then `B`                   |
//! | `A?`      | Option                              | `A` or nothing (ε)                                            |
//! | `A*`      | Kleene Star (Multiplicity)          | Arbitrarily long sequence of `A`s                             |
//! | `A+`      | Kleene Plus (Positive Multiplicity) | Arbitrarily long non-empty sequence of `A`s                   |
//! | `"T"`     | Terminal                            | Lexed token by textual content                                |
//! | `#T`      | Named Terminal                      | Lexed token by name                                           |
//! | `(> A)`   | Positive Look-Ahead                 |                                                               |
//! | `(< A)`   | Positive Look-Behind                |                                                               |
#![feature(decl_macro, default_free_fn, let_chains)]
#![allow(clippy::unnested_or_patterns)] // false positive with macros, see #9899

use ast::{Attributes, Declaration, Explicitness::Explicit, Expression, Identifier, Pattern};
use base::{
    delimiters_with_expected, expected_one_of, Delimiter, Expected, LexerErrorExt, Parser,
    SkipLineBreaks,
};
use diagnostics::{error::Result, Diagnostic, ErrorCode, Reporter};
use lexer::word::WordExt;
#[allow(clippy::wildcard_imports)] // it's an inline module which only exists for shared docs
use pattern::*;
use span::{SourceFileIndex, SourceMap, Span, Spanned, Spanning};
use std::default::default;
use token::{
    Token, TokenExt as _,
    TokenName::{self, *},
    Word,
};
use utilities::{smallvec, SmallVec};

mod base;
#[cfg(test)]
mod test;

/// Parse the file of a root module / component root.
pub fn parse_root_module_file(
    tokens: lexer::Outcome,
    file: SourceFileIndex,
    map: &SourceMap,
    reporter: &Reporter,
) -> Result<Declaration> {
    // @Task don't use unwrap(), handle errors properly
    let name = map[file]
        .name()
        .path()
        .unwrap()
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap();

    let binder = Word::parse(name.to_owned()).map_err(|_| {
        Diagnostic::error()
            .code(ErrorCode::E036)
            .message(format!(
                "the name of the root module ‘{name}’ is not a valid word"
            ))
            .report(reporter)
    })?;
    let binder = Spanned::bare(binder).into();

    parse_module_file(tokens, file, binder, map, reporter)
}

/// Parse the file of a root module or an out-of-line module.
pub fn parse_module_file(
    tokens: lexer::Outcome,
    file: SourceFileIndex,
    binder: Identifier,
    map: &SourceMap,
    reporter: &Reporter,
) -> Result<Declaration> {
    parse(
        tokens,
        |parser| parser.parse_top_level(binder),
        file,
        map,
        reporter,
    )
}

pub fn parse_path(
    tokens: lexer::Outcome,
    file: SourceFileIndex,
    map: &SourceMap,
    reporter: &Reporter,
) -> Result<ast::Path> {
    #[allow(clippy::redundant_closure_for_method_calls)] // false positive, #9335
    parse(tokens, |parser| parser.parse_path(), file, map, reporter)
}

fn parse<T>(
    tokens: lexer::Outcome,
    parser: impl FnOnce(&mut Parser<'_>) -> Result<T>,
    file: SourceFileIndex,
    map: &SourceMap,
    reporter: &Reporter,
) -> Result<T> {
    let mut health = Ok(());

    for error in tokens.errors {
        let error = error.into_diagnostic().report(reporter);

        if health.is_ok() {
            health = Err(error);
        }
    }

    let result = parser(&mut Parser::new(tokens.tokens, file, map, reporter));

    health.and(result)
}

impl Parser<'_> {
    /// Parse the *top level* (the body of a module file).
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Top-Level ::= (#Line-Break | Declaration)* #End-Of-Input
    /// ```
    fn parse_top_level(&mut self, module: Identifier) -> Result<Declaration> {
        let mut declarations = Vec::new();

        loop {
            if self.maybe_consume(LineBreak) {
                continue;
            }

            if self.maybe_consume(EndOfInput) {
                break Ok(Declaration::new(
                    Attributes::new(),
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
    /// ```
    // @Task re-add attribute groups (needs syntax proposals)
    fn parse_declaration(&mut self) -> Result<Declaration> {
        let attributes = self.parse_attributes(SkipLineBreaks::Yes)?;

        let span = self.token().span;
        match self.token().name() {
            Word => {
                let identifier = self.token_into_identifier();
                self.advance();
                self.finish_parse_function_declaration(identifier, attributes)
            }
            Data => {
                self.advance();
                self.finish_parse_data_declaration(span, attributes)
            }
            Module => {
                self.advance();
                self.finish_parse_module_declaration(span, attributes)
            }
            Use => {
                self.advance();
                self.finish_parse_use_declaration(span, attributes)
            }
            _ => Err(Expected::Declaration
                .but_actual_is(self.token())
                .report(self.reporter)),
        }
    }

    /// Finish parsing a [function declaration] given the already parsed leading word.
    ///
    /// The span of the result does not include the terminator.
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
    fn finish_parse_function_declaration(
        &mut self,
        binder: Identifier,
        attributes: Attributes,
    ) -> Result<Declaration> {
        let mut span = binder.span();

        let parameters = span.merging(self.parse_parameters()?);

        let type_ = span.merging(self.parse_optional_type_annotation()?);

        let body = if self.maybe_consume(Equals) {
            Some(span.merging(self.parse_expression()?))
        } else {
            None
        };

        // @Bug this only says "expected terminator", @Task it should say it
        // expected_one_of![
        //     Expected::Parameter, // only before ty & def
        //     Delimiter::TypeAnnotationPrefix, // only if we dont have a ty
        //     Delimiter::DefinitionPrefix, // only if we dont have a def
        //     Delimiter::Terminator,
        // ]
        self.parse_terminator()?;

        Ok(Declaration::new(
            attributes,
            span,
            ast::Function {
                binder,
                parameters,
                type_,
                body,
            }
            .into(),
        ))
    }

    /// Finish parsing a [data declaration] given the span of the already parsed leading `data` keyword.
    ///
    /// The span of the result does not include the terminator.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Data-Declaration ::=
    ///     "data" #Word
    ///     Parameters Type-Annotation?
    ///     ("of" (#Indentation (Terminator | Constructor)* #Dedentation)?)?
    ///     Terminator
    /// ```
    ///
    /// [data declaration]: ast::Data
    fn finish_parse_data_declaration(
        &mut self,
        mut span: Span,
        attributes: Attributes,
    ) -> Result<Declaration> {
        let binder = span.merging(self.consume_word()?);
        let parameters = span.merging(self.parse_parameters()?);
        let type_ = span.merging(self.parse_optional_type_annotation()?);

        let constructors = match self.token().name() {
            name @ Terminator!() => {
                if name == LineBreak {
                    self.advance();
                }
                None
            }
            Of => {
                span.merging(self.token().span);
                self.advance();

                let mut constructors = Vec::new();

                self.parse_optional_block(|this| {
                    constructors.push(this.parse_constructor()?);
                    Ok(())
                })?;

                span.merging(constructors.last());
                self.parse_terminator()?;

                Some(constructors)
            }
            _ => {
                return Err(expected_one_of![
                    Expected::Parameter,             // @Bug only before ty & of!
                    Delimiter::TypeAnnotationPrefix, // @Bug only if we don't have a type
                    Of,
                    Delimiter::Terminator,
                ]
                .but_actual_is(self.token())
                .report(self.reporter));
            }
        };

        Ok(Declaration::new(
            attributes,
            span.merge(span),
            ast::Data {
                binder,
                parameters,
                type_,
                constructors,
            }
            .into(),
        ))
    }

    /// Finish parsing [module declaration] given the span of the already parsed leading `module` keyword.
    ///
    /// This is either an inline or an out-of-line module declaration.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Module-Declaration ::=
    ///     | Header
    ///     | "module" #Word ("of" (#Indentation (Terminator | Declaration)* #Dedentation)?)? Terminator
    /// Header ::= "module" Terminator
    /// ```
    ///
    /// [module declaration]: ast::Module
    fn finish_parse_module_declaration(
        &mut self,
        mut span: Span,
        attributes: Attributes,
    ) -> Result<Declaration> {
        // @Task abstract over this (used below as well), good idea?
        if let name @ Terminator!() = self.token().name() {
            if name == LineBreak {
                self.advance();
            }

            return Ok(Declaration::new(
                attributes,
                span,
                ast::BareDeclaration::ModuleHeader,
            ));
        }

        let binder = span.merging(self.consume_word()?);

        match self.token().name() {
            // Out-of-line module declaration.
            // @Task abstract over this (used above as well), good idea?
            name @ Terminator!() => {
                if name == LineBreak {
                    self.advance();
                }

                Ok(Declaration::new(
                    attributes,
                    span,
                    ast::Module {
                        binder,
                        file: self.file,
                        declarations: None,
                    }
                    .into(),
                ))
            }
            Of => {
                self.advance();
                let mut declarations = Vec::new();

                self.parse_optional_block(|this| {
                    declarations.push(this.parse_declaration()?);
                    Ok(())
                })?;

                span.merging(self.parse_terminator()?);

                Ok(Declaration::new(
                    attributes,
                    span,
                    ast::Module {
                        binder,
                        file: self.file,
                        declarations: Some(declarations),
                    }
                    .into(),
                ))
            }
            _ => Err(expected_one_of![Delimiter::Terminator, Of]
                .but_actual_is(self.token())
                .report(self.reporter)),
        }
    }

    fn parse_optional_block(&mut self, mut parser: impl FnMut(&mut Self) -> Result) -> Result {
        if self.maybe_consume(Indentation) {
            loop {
                while self.maybe_consume(LineBreak) {}

                if self.maybe_consume(Dedentation) {
                    break;
                }

                parser(self)?;
            }
        }

        Ok(())
    }

    /// Finish parsing a [use-declaration] given the span of the already parsed leading `use` keyword.
    ///
    /// The span of the result does not contain the terminator.
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
        let bindings = self.parse_use_path_tree(&[Delimiter::Terminator])?;
        self.parse_terminator()?;

        Ok(Declaration::new(
            attributes,
            span.merge(&bindings),
            ast::Use { bindings }.into(),
        ))
    }

    /// Parse a use-path tree.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Use-Path-Tree ::=
    ///     | Path
    ///     | Path "." "(" (Use-Path-Tree | "(" Renaming ")")* ")"
    ///     | Renaming
    /// Renaming ::= Path "as" Identifier
    /// ```
    // @Task rewrite this following a simpler grammar mirroring expression applications
    fn parse_use_path_tree(&mut self, delimiters: &[Delimiter]) -> Result<ast::UsePathTree> {
        let mut path = self.parse_first_path_segment()?;

        while self.maybe_consume(Dot) {
            match self.token().name() {
                Identifier!() => {
                    let identifier = self.token_into_identifier();
                    self.advance();
                    path.segments.push(identifier);
                }
                OpeningRoundBracket => {
                    let mut span = self.token().span;
                    self.advance();

                    let mut bindings = Vec::new();

                    while self.token().name() != ClosingRoundBracket {
                        if let Some(mut span) = self.maybe_consume_span(OpeningRoundBracket) {
                            let target = self.parse_path()?;
                            self.consume(As)?;
                            let binder = self.parse_identifier()?;
                            span.merging(&self.consume(ClosingRoundBracket)?);

                            bindings.push(ast::UsePathTree::new(
                                span,
                                ast::BareUsePathTree::Single {
                                    target,
                                    binder: Some(binder),
                                },
                            ));
                        } else {
                            // @Note @Bug this is really really fragile=non-extensible!
                            bindings.push(self.parse_use_path_tree(&[
                                OpeningRoundBracket.into(),
                                ClosingRoundBracket.into(),
                                // @Question Expected::Identifier?
                                Word.into(),
                                Symbol.into(),
                                Self_.into(),
                                Super.into(),
                                Topmost.into(),
                            ])?);
                        }
                    }

                    // @Note constructs and wastes a diagnostic
                    // @Task transform above while into a loop
                    // @Beacon :while_current_not
                    span.merging(&self.consume(ClosingRoundBracket).unwrap());

                    return Ok(ast::UsePathTree::new(
                        path.span().merge(span),
                        ast::BareUsePathTree::Multiple { path, bindings },
                    ));
                }
                _ => {
                    return Err(expected_one_of![Expected::Identifier, OpeningRoundBracket]
                        .but_actual_is(self.token())
                        .report(self.reporter));
                }
            }
        }

        // @Question is there a grammar transformation to a self-contained construct
        // instead of a delimited one?
        let binder = if self.token_is_delimiter(delimiters) {
            None
        } else if self.maybe_consume(As) {
            self.token();
            Some(self.parse_identifier()?)
        } else {
            return Err(delimiters_with_expected(delimiters, Some(As.into()))
                .but_actual_is(self.token())
                .report(self.reporter));
        };

        Ok(ast::UsePathTree::new(
            path.span().merge(&binder),
            ast::BareUsePathTree::Single {
                target: path,
                binder,
            },
        ))
    }

    /// Parse a (term) [constructor].
    ///
    /// The span of the result does not include the terminator.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Constructor ::=
    ///     (Attribute #Line-Break*)*
    ///     #Word Parameters Type-Annotation?
    ///     ("=" Expression)?
    ///     Terminator
    /// ```
    ///
    /// [constructor]: ast::Constructor
    fn parse_constructor(&mut self) -> Result<Declaration> {
        let attributes = self.parse_attributes(SkipLineBreaks::Yes)?;

        let binder = self.consume_word()?;
        let mut span = binder.span();

        let parameters = span.merging(self.parse_parameters()?);
        let type_ = span.merging(self.parse_optional_type_annotation()?);

        let body = if self.maybe_consume(Equals) {
            Some(span.merging(self.parse_expression()?))
        } else {
            None
        };

        // @Bug only mentions "expected terminator"
        // @Task it should say
        // expected_one_of![
        //     Expected::Parameter, // Only before ty & def!
        //     Delimiter::TypeAnnotationPrefix, // Or Colon // Only if we don't have a type ann
        //     Delimiter::DefinitionPrefix, // Or Equals // Only if we don't have a definition
        //     Delimiter::Terminator,
        // ]
        self.parse_terminator()?;

        Ok(Declaration::new(
            attributes,
            span,
            ast::Constructor {
                binder,
                parameters,
                type_,
                body,
            }
            .into(),
        ))
    }

    /// Parse an expression.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Expression ::= Shorthand-Quantified-Type-Or-Lower
    /// ```
    fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_shorthand_quantified_type_or_lower()
    }

    /// Parse a shorthand [quantified type] or a lower expression.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// ; Among other things, the grammar for pretty printers differs from the one for parsers
    /// ; in that `Shorthand-Quantified-Type-Or-Lower` also includes several complex (in the sense that they
    /// ; contain further expressions) `Lower-Expression`s namely let- and use-bindings, lambda literals,
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

        let quantifier = match self.token().name() {
            ThinArrowRight => ast::Quantifier::Pi,
            DoubleAsterisk => ast::Quantifier::Sigma,
            // We don't actually have a quantified type but merely an application or lower.
            _ => return Ok(domain),
        };

        self.advance();

        // Using recursion to parse right-associatively.
        let codomain = span.merging(self.parse_shorthand_quantified_type_or_lower()?);

        Ok(Expression::new(
            Attributes::new(),
            span,
            ast::QuantifiedType {
                quantifier,
                parameters: smallvec![ast::Parameter::new(
                    domain.span,
                    ast::BareParameter {
                        explicitness: Explicit,
                        // @Task use an pre-interned underscore!
                        binder: Identifier::new_unchecked("_".into(), domain.span.start()),
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
    ///     Explicitness
    ///     (Lower-Expression | "(" (#Word "=")? Expression ")")
    ///
    /// ; ; The left-recursive version of the rule above is unsuitable for a recursive descent parser.
    /// ; ; However, it is usable for pretty printers.
    /// ;
    /// ; Application-Expression-Or-Lower ::= Application-Expression-Or-Lower? Expression-Argument*
    /// ; Expression-Argument ::=
    /// ;     | Lower-Expression
    /// ;     | Explicitness "(" (#Word "=")? Expression ")"
    /// ```
    ///
    /// [application]: ast::Application
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
    ///     | #Number-Literal
    ///     | #Text-Literal
    ///     | Typed-Hole
    ///     | Let-Binding
    ///     | Use-Binding
    ///     | Lambda-Literal
    ///     | Case-Analysis
    ///     | Do-Block
    ///     | Quantified-Type
    ///     | Sequence-Literal-Expression
    ///     | Path-Or-Namespaced-Literal-Expression
    ///     | "(" Expression ")"
    /// Typed-Hole ::= "?" #Word
    /// ```
    fn parse_lower_expression(&mut self) -> Result<Expression> {
        //
        // IMPORTANT: To be kept in sync with `pattern::LowerExpressionPrefix`.
        //

        let attributes = self.parse_attributes(SkipLineBreaks::No)?;

        let mut span = self.token().span;
        let mut expression = match self.token().name() {
            NumberLiteral => {
                let token = self.token().clone();
                self.advance();

                Expression::new(
                    default(),
                    token.span,
                    ast::NumberLiteral {
                        path: None,
                        literal: Spanned::new(token.span, token.into_number_literal().unwrap()),
                    }
                    .into(),
                )
            }
            TextLiteral => {
                let token = self.token().clone();
                self.advance();

                Expression::new(
                    default(),
                    token.span,
                    ast::TextLiteral {
                        path: None,
                        literal: Spanned::new(token.span, token.into_text_literal().unwrap()),
                    }
                    .into(),
                )
            }
            QuestionMark => {
                self.advance();
                let tag = self.consume_word()?;

                Expression::new(default(), span.merge(&tag), ast::TypedHole { tag }.into())
            }
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
            OpeningSquareBracket => {
                self.advance();
                self.finish_parse_sequence_literal(None, span)?
            }
            OpeningRoundBracket => {
                self.advance();

                let mut expression = self.parse_expression()?;

                span.merging(self.consume(ClosingRoundBracket)?);
                expression.span = span;

                expression
            }
            PathHead!() => self.parse_path_or_namespaced_literal()?,
            _ => {
                return Err(Expected::Category("expression")
                    .but_actual_is(self.token())
                    .with(|error| {
                        // @Beacon @Note this is a prime example for a situation where we can
                        // make a parsing error non-fatal: we can just skip the `->` and keep
                        // parsing w/o introducing too many (any?) useless/confusing consequential
                        // errors!
                        if self.token().name() == ThinArrowRight {
                            error.help(
                                "consider adding round brackets around the potential \
                                 function type to disambiguate the expression",
                            )
                        } else {
                            error
                        }
                    })
                    .report(self.reporter));
            }
        };

        let mut attributes = Some(attributes);

        while self.maybe_consume(DoubleColon) {
            let member = self.consume_word()?;

            expression = Expression::new(
                attributes.take().unwrap_or_default(),
                expression.span.merge(&member),
                ast::Projection {
                    basis: expression,
                    field: member,
                }
                .into(),
            );
        }

        if let Some(attributes) = attributes {
            expression.attributes.extend(attributes);
        }

        Ok(expression)
    }

    /// Parse a path.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Path ::= Path-Head ("." Identifier)*
    /// Path-Head ::= Path-Hanger | Identifier
    /// Path-Hanger ::= "extern" | "topmost" | "super" | "self"
    /// ```
    fn parse_path(&mut self) -> Result<ast::Path> {
        let mut path = self.parse_first_path_segment()?;

        while self.maybe_consume(Dot) {
            path.segments.push(self.parse_identifier()?);
        }

        Ok(path)
    }

    /// Parse the first segment of a path.
    // @Task grammar
    fn parse_first_path_segment(&mut self) -> Result<ast::Path> {
        let token = self.token();
        let path = match token.name() {
            Identifier!() => Identifier::try_from(token.clone()).unwrap().into(),
            PathHanger!() => token
                .clone()
                .map(|token| ast::BareHanger::try_from(token).unwrap())
                .into(),
            _ => return Err(Expected::Path.but_actual_is(token).report(self.reporter)),
        };
        self.advance();
        Ok(path)
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
        // self.parse_parameters(&[Delimiter::TypeAnnotationPrefix, WideArrowRight.into()])?;
        // @Temporary
        let parameters = self.parse_parameters()?;
        let codomain = self.parse_optional_type_annotation()?;
        self.consume(WideArrowRight)?;
        let body = span.merging(self.parse_expression()?);

        Ok(Expression::new(
            Attributes::new(),
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

        let quantifier = match self.token().name() {
            ThinArrowRight => ast::Quantifier::Pi,
            DoubleAsterisk => ast::Quantifier::Sigma,
            _ => {
                // @Task add UI test for this!
                return Err(
                    expected_one_of![Expected::Parameter, ThinArrowRight, DoubleAsterisk]
                        .but_actual_is(self.token())
                        .label(span, "while parsing this quantified type starting here")
                        .report(self.reporter),
                );
            }
        };
        self.advance();

        let codomain = span.merging(self.parse_expression()?);

        Ok(Expression::new(
            Attributes::new(),
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
    ///     "let" #Word Parameters Type-Annotation?
    ///     ("=" Expression)?
    ///     #LineBreak?
    ///     "in" Expression
    /// ```
    ///
    /// [let-binding]: ast::LetBinding
    fn finish_parse_let_binding(&mut self, mut span: Span) -> Result<Expression> {
        let binder = self.consume_word()?;

        // let parameters = self.parse_parameters(&[
        //     Delimiter::TypeAnnotationPrefix,
        //     Delimiter::DefinitionPrefix,
        //     In.into(),
        // ])?;
        // @Temporary
        let parameters = self.parse_parameters()?;
        let type_ = self.parse_optional_type_annotation()?;

        let body = if self.maybe_consume(Equals) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        if let LineBreak = self.token().name() {
            self.advance();
        }

        self.consume(In)?;

        let scope = span.merging(self.parse_expression()?);

        Ok(Expression::new(
            Attributes::new(),
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
    ///     #LineBreak?
    ///     "in" Expression
    /// ```
    ///
    /// [use-binding]: ast::UseBinding
    fn finish_parse_use_binding(&mut self, span: Span) -> Result<Expression> {
        let bindings = self.parse_use_path_tree(&[In.into()])?;

        if let LineBreak = self.token().name() {
            self.advance();
        }

        self.consume(In)?;

        let scope = self.parse_expression()?;

        Ok(Expression::new(
            Attributes::new(),
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
        span.merging(self.consume(Of)?);

        let mut cases = Vec::new();

        if self.maybe_consume(Indentation) {
            // @Task use parse_block function for this (but don't trash the span!)

            while self.token().name() != Dedentation {
                let pattern = self.parse_pattern()?;
                self.consume(WideArrowRight)?;
                let body = self.parse_expression()?;
                self.parse_terminator()?;

                cases.push(ast::Case { pattern, body });
            }

            // @Beacon :while_current_not
            span.merging(self.token());
            self.advance();
        }

        Ok(Expression::new(
            Attributes::new(),
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
    /// Let-Statement ::= "let" #Word Parameter* Type-Annotation? Binding-Mode Expression Terminator
    /// Expression-Statement ::= Expression Terminator
    /// Binding-Mode ::=  "=" | "<-"
    /// ```
    ///
    /// [do-block]: ast::DoBlock
    fn finish_parse_do_block(&mut self, mut span: Span) -> Result<Expression> {
        let mut statements = Vec::new();

        self.consume(Indentation)?;

        while self.token().name() != Dedentation {
            // @Note necessary I guess in cases where we have #Line-Break ##Comment+ #Line-Break
            if self.maybe_consume(LineBreak) {
                continue;
            }

            statements.push(match self.token().name() {
                // @Task move to its own function
                Let => {
                    self.advance();
                    let binder = self.consume_word()?;

                    let parameters = self.parse_parameters()?;
                    let type_ = self.parse_optional_type_annotation()?;

                    let body = match self.token().name() {
                        token @ (Equals | ThinArrowLeft) => {
                            self.advance();

                            let mode = match token {
                                Equals => ast::BindingMode::Plain,
                                ThinArrowLeft => ast::BindingMode::Effectful,
                                _ => unreachable!(),
                            };

                            Some((mode, self.parse_expression()?))
                        }
                        _ => None,
                    };

                    // @Task
                    // let parameters = self.parse_parameters(&[
                    //     Delimiter::TypeAnnotationPrefix,
                    //     Delimiter::DefinitionPrefix,
                    //     `<-`
                    // ])?;
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
                    let bindings = self.parse_use_path_tree(&[Delimiter::Terminator])?;
                    self.parse_terminator()?;

                    ast::Statement::Use(ast::Use { bindings })
                }
                _ => {
                    // @Beacon @Task improve error diagnostics for an unexpected token to not only mention an
                    // expression was expected but also statements were
                    let expression = self.parse_expression()?;
                    self.parse_terminator()?;

                    ast::Statement::Expression(expression)
                }
            });
        }

        // @Beacon :while_current_not
        span.merging(self.token());
        self.advance();

        Ok(Expression::new(
            Attributes::new(),
            span,
            ast::DoBlock { statements }.into(),
        ))
    }

    /// Parse a sequence of parameters.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Parameters ::= Parameter*
    /// ```
    fn parse_parameters(&mut self) -> Result<ast::Parameters> {
        let mut parameters = SmallVec::new();

        while let ParameterPrefix!() = self.token().name() {
            // @Question inline? this would enable use getting rid of `is_param_prefix`
            parameters.push(self.parse_parameter()?);
        }

        Ok(parameters)
    }

    /// Parse a parameter.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Parameter ::= Explicitness Bare-Parameter
    /// Bare-Parameter ::= #Word | "(" #Word Type-Annotation? ")"
    /// ```
    fn parse_parameter(&mut self) -> Result<ast::Parameter> {
        let explicitness = self.parse_optional_implicitness();
        let mut span = self.token().span.merge_into(explicitness);

        match self.token().name() {
            Word => {
                let binder = self.token_into_identifier();
                self.advance();

                Ok(ast::Parameter::new(
                    span,
                    ast::BareParameter {
                        explicitness: explicitness.into(),
                        binder,
                        type_: None,
                    },
                ))
            }
            OpeningRoundBracket => {
                self.advance();

                let binder = self.consume_word()?;
                let type_ = self.parse_optional_type_annotation()?;

                // @Task rewrite to use a parser-internal list of expected tokens
                span.merging(self.consume_after_expecting(
                    ClosingRoundBracket,
                    Delimiter::TypeAnnotationPrefix.into(),
                )?);

                Ok(ast::Parameter::new(
                    span,
                    ast::BareParameter {
                        explicitness: explicitness.into(),
                        binder,
                        type_,
                    },
                ))
            }
            _ => Err(Expected::Parameter
                .but_actual_is(self.token())
                .report(self.reporter)),
        }
    }

    /// Parse a pattern.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Pattern ::= Lower-Pattern Pattern-Argument*
    /// Pattern-Argument ::=
    ///     Explicitness
    ///     (Lower-Pattern | "(" (#Word "=")? Pattern ")")
    /// ```
    fn parse_pattern(&mut self) -> Result<Pattern> {
        self.parse_application_or_lower()
    }

    /// Parse a lower pattern.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Lower-Pattern ::= Attribute* Bare-Lower-Pattern
    /// Bare-Lower-Pattern ::=
    ///     | #Number-Literal
    ///     | #Text-Literal
    ///     | Binder
    ///     | Sequence-Literal-Pattern
    ///     | Path-Or-Namespaced-Literal-Pattern
    ///     | "(" Pattern ")"
    /// Binder ::= "\" #Word
    /// ```
    fn parse_lower_pattern(&mut self) -> Result<Pattern> {
        //
        // IMPORTANT: To be kept in sync with `pattern::LowerPatternPrefix`.
        //

        let attributes = self.parse_attributes(SkipLineBreaks::No)?;

        let mut span = self.token().span;
        let mut pattern = match self.token().name() {
            NumberLiteral => {
                let token = self.token().clone();
                self.advance();

                Pattern::new(
                    default(),
                    token.span,
                    ast::NumberLiteral {
                        path: None,
                        literal: Spanned::new(token.span, token.into_number_literal().unwrap()),
                    }
                    .into(),
                )
            }
            TextLiteral => {
                let token = self.token().clone();
                self.advance();

                Pattern::new(
                    default(),
                    token.span,
                    ast::TextLiteral {
                        path: None,
                        literal: Spanned::new(token.span, token.into_text_literal().unwrap()),
                    }
                    .into(),
                )
            }
            Backslash => {
                self.advance();
                self.consume_word()
                    .map(|binder| Pattern::new(default(), span.merge(&binder), binder.into()))?
            }
            OpeningSquareBracket => {
                self.advance();

                let mut elements = Vec::new();

                while self.token().name() != ClosingSquareBracket {
                    elements.push(self.parse_lower_pattern()?);
                }

                span.merging(self.token());
                self.advance(); // ClosingSquareBracket

                Pattern::new(
                    default(),
                    span,
                    ast::SequenceLiteral {
                        path: None,
                        elements: Spanned::new(span, elements),
                    }
                    .into(),
                )
            }
            OpeningRoundBracket => {
                self.advance();
                let mut pattern = self.parse_pattern()?;
                span.merging(self.consume(ClosingRoundBracket)?);
                pattern.span = span;

                pattern
            }
            PathHead!() => self.parse_path_or_namespaced_literal()?,
            _ => {
                return Err(Expected::Category("pattern")
                    .but_actual_is(self.token())
                    .report(self.reporter))
            }
        };

        pattern.attributes.extend(attributes);

        Ok(pattern)
    }

    /// Finish parsing a [sequence literal] given the already parsed leading path and the
    /// span of the already parsed leading `[` symbol.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Sequence-Literal-Expression ::= (Path ".")? "[" Lower-Expression* "]"
    /// Sequence-Literal-Pattern ::= (Path ".")? "[" Lower-Pattern* "]"
    /// ```
    ///
    /// [sequence literal]: ast::SequenceLiteral
    fn finish_parse_sequence_literal<T>(
        &mut self,
        path: Option<ast::Path>,
        mut span: Span,
    ) -> Result<ast::Item<T>>
    where
        T: Parse + From<ast::SequenceLiteral<ast::Item<T>>>,
    {
        let mut elements = Vec::new();

        while self.token().name() != ClosingSquareBracket {
            elements.push(T::parse_lower(self)?);
        }

        span.merging(self.token());
        self.advance(); // ClosingSquareBracket

        Ok(ast::Item::new(
            default(),
            span.merge_into(&path),
            ast::SequenceLiteral {
                path,
                elements: Spanned::new(span, elements),
            }
            .into(),
        ))
    }

    /// Parse an [application] or a lower item.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Application-⟪Item⟫-Or-Lower ::= Lower-⟪Item⟫ ⟪Item⟫-Argument*
    /// ⟪Item⟫-Argument ::=
    ///     Explicitness
    ///     (Lower-⟪Item⟫ | "(" (#Word "=")? ⟪Item⟫ ")")
    /// ```
    ///
    /// [application]: ast::Application
    fn parse_application_or_lower<T>(&mut self) -> Result<ast::Item<T>>
    where
        T: Parse + From<ast::Application<ast::Item<T>>> + 'static,
    {
        let mut callee = T::parse_lower(self)?;
        let mut span = callee.span;

        // Using iteration to parse left-associatively.
        loop {
            let explicitness = self.parse_optional_implicitness();
            let binder;
            let argument;

            //
            // Parse an optional argument.
            //
            if self.token().name() == OpeningRoundBracket
                && self.look_ahead(1).name() == Word
                && self.look_ahead(2).name() == Equals
            {
                self.advance(); // "("
                binder = Some(self.token_into_identifier());
                self.advance(); // #Word
                self.advance(); // "="

                argument = T::parse(self)?;

                span.merging(self.consume(ClosingRoundBracket)?);
            } else if T::is_lower_prefix(self.token().name()) {
                binder = None;
                argument = span.merging(T::parse_lower(self)?);
            } else if let Some(explicitness) = explicitness {
                return Err(Expected::Category("function argument")
                    .but_actual_is(self.token())
                    .label(&callee, "while parsing this function application")
                    .label(
                        explicitness,
                        "this apostrophe marks the start of an implicit argument",
                    )
                    .report(self.reporter));
            } else {
                // The current token is not the start of an argument.
                // Hence we are done here.

                return Ok(callee);
            }

            callee = ast::Item::new(
                default(),
                span,
                ast::Application {
                    callee,
                    explicitness: explicitness.into(),
                    binder,
                    argument,
                }
                .into(),
            );
        }
    }

    /// Parse a path or a number, text or sequence literal prefixed with a path.
    ///
    /// ## Grammar
    ///
    /// ```grammar
    /// Path-Or-Namespaced-Literal-Expression ::=
    ///     Path
    ///     ("." (#Number-Literal | #Text-Literal | Sequence-Literal-Expression))?
    /// Path-Or-Namespaced-Literal-Pattern ::=
    ///     Path
    ///     ("." (#Number-Literal | #Text-Literal | Sequence-Literal-Pattern))?
    /// ```
    fn parse_path_or_namespaced_literal<T>(&mut self) -> Result<ast::Item<T>>
    where
        T: Parse
            + From<ast::NumberLiteral>
            + From<ast::TextLiteral>
            + From<ast::SequenceLiteral<ast::Item<T>>>
            + From<ast::Path>,
    {
        let mut path = self.parse_first_path_segment()?;

        while self.maybe_consume(Dot) {
            match self.token().name() {
                Identifier!() => {
                    let identifier = self.token_into_identifier();
                    self.advance();
                    path.segments.push(identifier);
                }
                NumberLiteral => {
                    let token = self.token().clone();
                    self.advance();

                    return Ok(ast::Item::new(
                        default(),
                        path.span().merge(&token),
                        ast::NumberLiteral {
                            path: Some(path),
                            literal: Spanned::new(token.span, token.into_number_literal().unwrap()),
                        }
                        .into(),
                    ));
                }
                TextLiteral => {
                    let token = self.token().clone();
                    self.advance();

                    return Ok(ast::Item::new(
                        default(),
                        path.span().merge(&token),
                        ast::TextLiteral {
                            path: Some(path),
                            literal: Spanned::new(token.span, token.into_text_literal().unwrap()),
                        }
                        .into(),
                    ));
                }
                OpeningSquareBracket => {
                    let span = self.token().span();
                    self.advance();
                    return self.finish_parse_sequence_literal(Some(path), span);
                }
                _ => {
                    return Err(expected_one_of![
                        Expected::Identifier,
                        NumberLiteral,
                        TextLiteral,
                        OpeningSquareBracket
                    ]
                    .but_actual_is(self.token())
                    .report(self.reporter))
                }
            }
        }

        Ok(ast::Item::new(default(), path.span(), path.into()))
    }

    /// Parse an optional type annotation.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Type-Annotation ::= ":" Expression
    /// ```
    fn parse_optional_type_annotation(&mut self) -> Result<Option<Expression>> {
        self.maybe_consume(Colon)
            .then(|| self.parse_expression())
            .transpose()
    }

    /// Parse attributes.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Attribute ::= Regular-Attribute | Documentation-Comment
    /// ```
    fn parse_attributes(&mut self, skip_line_breaks: SkipLineBreaks) -> Result<Attributes> {
        let mut attributes = Attributes::default();

        loop {
            let span = self.token().span;
            attributes.push(match self.token().name() {
                At => {
                    self.advance();
                    let attribute = self.finish_parse_regular_attribute(span)?;
                    if skip_line_breaks == SkipLineBreaks::Yes {
                        while self.maybe_consume(LineBreak) {}
                    }
                    attribute
                }
                DocumentationComment => {
                    self.advance();
                    if skip_line_breaks == SkipLineBreaks::Yes {
                        while self.maybe_consume(LineBreak) {}
                    }
                    ast::Attribute::new(span, ast::BareAttribute::Documentation)
                }
                _ => break,
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
        let binder;
        let mut arguments = SmallVec::new();

        match self.token().name() {
            Word => {
                binder = span.merging(self.token_into_identifier());
                self.advance();
            }
            OpeningRoundBracket => {
                self.advance();
                binder = self.consume_word()?;

                while self.token().name() != ClosingRoundBracket {
                    arguments.push(self.parse_attribute_argument()?);
                }

                // @Note constructs and wastes a diagnostic
                // @Task transform above while into a loop
                // @Beacon :while_current_not
                span.merging(self.consume(ClosingRoundBracket).unwrap());
            }
            _ => {
                return Err(expected_one_of![Word, OpeningRoundBracket]
                    .but_actual_is(self.token())
                    .label(span, "while parsing this attribute starting here")
                    .report(self.reporter));
            }
        }

        Ok(ast::Attribute::new(
            span,
            ast::BareAttribute::Regular { binder, arguments },
        ))
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
        let argument = match self.token().name() {
            PathHead!() => {
                let path = self.parse_path()?;
                span = path.span();

                ast::BareAttributeArgument::Path(Box::new(path))
            }
            NumberLiteral => {
                let token = self.token().clone();
                span = token.span;
                self.advance();

                ast::BareAttributeArgument::NumberLiteral(token.into_number_literal().unwrap())
            }
            TextLiteral => {
                let token = self.token().clone();
                span = token.span;
                self.advance();

                ast::BareAttributeArgument::TextLiteral(token.into_text_literal().unwrap())
            }
            OpeningRoundBracket => {
                span = self.token().span;
                self.advance();
                let binder = self.consume_word()?;
                let value = self.parse_attribute_argument()?;
                span.merging(&self.consume(ClosingRoundBracket)?);

                ast::BareAttributeArgument::Named(Box::new(ast::NamedAttributeArgument {
                    binder,
                    value,
                }))
            }
            _ => {
                return Err(Expected::Category("attribute argument")
                    .but_actual_is(self.token())
                    .report(self.reporter))
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
    fn parse_identifier(&mut self) -> Result<Identifier> {
        match self.token().name() {
            Identifier!() => {
                let identifier = self.token_into_identifier();
                self.advance();
                Ok(identifier)
            }
            _ => Err(Expected::Identifier
                .but_actual_is(self.token())
                .report(self.reporter)),
        }
    }

    /// Parse a terminator.
    ///
    /// If the terminator is a line break, [consume] it. Otherwise, don't.
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
    /// [consume]: Self::consume
    // @Task now with delimited sections being gone, we might be able to simplify the
    // definition of a terminator, maybe?
    fn parse_terminator(&mut self) -> Result<Option<Token>> {
        let token = self.token();
        if matches!(token.name(), Terminator!())
            || self
                .preceeding_token()
                .map_or(true, |token| matches!(token.name(), Terminator!()))
        {
            Ok(if token.name() == LineBreak {
                let token = token.clone();
                self.advance();

                Some(token)
            } else {
                None
            })
        } else {
            Err(Expected::Delimiter(Delimiter::Terminator)
                .but_actual_is(self.token())
                .report(self.reporter))
        }
    }

    /// Parse an optional [implicitness] symbol.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Explicitness ::= "'"?
    /// ```
    ///
    /// [implicitness]: ast::Explicitness
    fn parse_optional_implicitness(&mut self) -> Option<Span> {
        if self.token().name() == Apostrophe {
            let span = self.token().span;
            self.advance();
            Some(span)
        } else {
            None
        }
    }
}

trait Parse: Sized {
    fn parse(parser: &mut Parser<'_>) -> Result<ast::Item<Self>>;
    fn parse_lower(parser: &mut Parser<'_>) -> Result<ast::Item<Self>>;
    fn is_lower_prefix(token: TokenName) -> bool;
}

impl Parse for ast::BareExpression {
    fn parse(parser: &mut Parser<'_>) -> Result<Expression> {
        parser.parse_expression()
    }
    fn parse_lower(parser: &mut Parser<'_>) -> Result<Expression> {
        parser.parse_lower_expression()
    }

    fn is_lower_prefix(token: TokenName) -> bool {
        matches!(token, LowerExpressionPrefix!())
    }
}

impl Parse for ast::BarePattern {
    fn parse(parser: &mut Parser<'_>) -> Result<Pattern> {
        parser.parse_pattern()
    }
    fn parse_lower(parser: &mut Parser<'_>) -> Result<Pattern> {
        parser.parse_lower_pattern()
    }

    fn is_lower_prefix(token: TokenName) -> bool {
        matches!(token, LowerPatternPrefix!())
    }
}

mod pattern {

    //! A collection of *pattern synonyms*.
    //!
    //! Contrary to methods or arrays, they preserve `match` exhaustiveness & overlap checks at use sites.

    use token::TokenName::*;

    /// An [identifier]
    ///
    /// [identifier]: ast::Identifier
    pub(crate) macro Identifier() {
        Word | Symbol
    }

    /// The prefix of an [attribute].
    ///
    /// [attribute]: ast::Attribute
    pub(crate) macro AttributePrefix() {
        At | DocumentationComment
    }

    /// The prefix of a lower expression.
    pub(crate) macro LowerExpressionPrefix() {
        //
        // IMPORTANT: To be kept in sync with `crate::Parser::parse_lower_expression`.
        //

        AttributePrefix!()
            | NumberLiteral
            | TextLiteral
            | QuestionMark
            | ForUpper
            | ForLower
            | Case
            | Do
            | Let
            | Use
            | OpeningSquareBracket
            | OpeningRoundBracket
            | PathHead!()
    }

    /// The prefix of a lower pattern.
    pub(crate) macro LowerPatternPrefix() {
        //
        // IMPORTANT: To be kept in sync with `crate::Parser::parse_lower_pattern`.
        //

        AttributePrefix!()
            | NumberLiteral
            | TextLiteral
            | Backslash
            | OpeningSquareBracket
            | OpeningRoundBracket
            | PathHead!()
    }

    // @Task inline this!
    /// The prefix of a [parameter].
    ///
    /// [parameter]: ast::Parameter
    pub(crate) macro ParameterPrefix() {
        Apostrophe | Word | OpeningRoundBracket
    }

    /// The head (i.e. prefix) of a [path].
    ///
    /// [path]: ast::Path
    pub(crate) macro PathHead() {
        Identifier!() | PathHanger!()
    }

    /// The [hanger] of a [path].
    ///
    /// [hanger]: ast::Hanger
    /// [path]: ast::Path
    pub(crate) macro PathHanger() {
        Extern | Topmost | Super | Self_
    }

    /// A declaration terminator.
    pub(crate) macro Terminator() {
        LineBreak | Dedentation | EndOfInput
    }
}
