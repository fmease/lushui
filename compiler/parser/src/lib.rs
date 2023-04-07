//! The syntactic analyzer (parser).
//!
//! It is a handwritten top-down recursive-descent parser with bounded look-ahead & look-behind and
//! no backtracking.
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
use base::{LexerErrorExt, Parser, SkipLineBreaks};
use diagnostics::{error::Result, Diagnostic, ErrorCode, Reporter};
use span::{SourceFileIndex, SourceMap, Span, Spanned, Spanning};
use std::default::default;
// It's a small *inline* module allowing us unify the docs of its items.
use lexer::token::BareToken::{self, *};
#[allow(clippy::wildcard_imports)]
use synonym::*;
use utilities::{smallvec, Atom, SmallVec};

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

    let binder = lexer::word::Word::parse(name.to_owned()).map_err(|_| {
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
            if self.consume(LineBreak) {
                continue;
            }

            if self.consume(EndOfInput) {
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

        let span = self.span();
        match self.token() {
            Word(binder) => {
                let binder = ast::Identifier::new_unchecked(binder, span);
                self.advance();
                self.finish_parse_function_declaration(binder, attributes)
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
            _ => {
                self.expected("declaration");
                self.error()
            }
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

        let body = if self.consume(Equals) {
            Some(span.merging(self.parse_expression()?))
        } else {
            None
        };

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
        let binder = span.merging(self.parse_word()?);
        let parameters = span.merging(self.parse_parameters()?);
        let type_ = span.merging(self.parse_optional_type_annotation()?);

        let constructors = match self.token() {
            Of => {
                span.merging(self.span());
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
            name @ Terminator!() => {
                if name == LineBreak {
                    self.advance();
                }
                None
            }
            _ => {
                self.expected(Of);
                self.expected(TERMINATOR);
                return self.error();
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
        if let name @ Terminator!() = self.token() {
            if name == LineBreak {
                self.advance();
            }

            return Ok(Declaration::new(
                attributes,
                span,
                ast::BareDeclaration::ModuleHeader,
            ));
        }

        let binder = span.merging(self.parse_word()?);

        match self.token() {
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
            _ => {
                self.expected(TERMINATOR);
                self.expected(Of);
                self.error()
            }
        }
    }

    fn parse_optional_block(&mut self, mut parser: impl FnMut(&mut Self) -> Result) -> Result {
        if self.consume(Indentation) {
            loop {
                while self.consume(LineBreak) {}

                if self.consume(Dedentation) {
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
        let bindings = self.parse_use_path_tree()?;
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
    fn parse_use_path_tree(&mut self) -> Result<ast::UsePathTree> {
        let mut path = self.parse_path_head()?;

        while self.consume(Dot) {
            let mut span = self.span();
            match self.token() {
                Identifier!(segment) => {
                    self.advance();

                    path.segments
                        .push(ast::Identifier::new_unchecked(segment, span));
                }
                OpeningRoundBracket => {
                    self.advance();

                    let mut bindings = Vec::new();

                    while self.token() != ClosingRoundBracket {
                        let mut span = self.span();
                        if self.consume(OpeningRoundBracket) {
                            let target = self.parse_path()?;
                            self.expect(As)?;
                            let binder = self.parse_identifier()?;
                            span.merging(self.expect(ClosingRoundBracket)?);

                            bindings.push(ast::UsePathTree::new(
                                span,
                                ast::BareUsePathTree::Single {
                                    target,
                                    binder: Some(binder),
                                },
                            ));
                        } else {
                            bindings.push(self.parse_use_path_tree()?);
                        }
                    }

                    span.merging(self.span());
                    self.advance(); // ")"

                    return Ok(ast::UsePathTree::new(
                        path.span().merge(span),
                        ast::BareUsePathTree::Multiple { path, bindings },
                    ));
                }
                _ => {
                    self.expected(IDENTIFIER);
                    self.expected(OpeningRoundBracket);
                    return self.error();
                }
            }
        }

        let binder = if self.consume(As) {
            Some(self.parse_identifier()?)
        } else {
            None
        };

        Ok(ast::UsePathTree::new(
            path.span().merge(binder),
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

        let binder = self.parse_word()?;
        let mut span = binder.span();

        let parameters = span.merging(self.parse_parameters()?);
        let type_ = span.merging(self.parse_optional_type_annotation()?);

        let body = if self.consume(Equals) {
            Some(span.merging(self.parse_expression()?))
        } else {
            None
        };

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

        let quantifier = match self.token() {
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
                        // @Temporary hack until we properly support discards
                        binder: Identifier::new_unchecked(Atom::__, domain.span.start()),
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

        let mut span = self.span();
        let mut expression = match self.token() {
            NumberLiteral(literal) => {
                self.advance();

                Expression::new(
                    default(),
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

                Expression::new(
                    default(),
                    span,
                    ast::TextLiteral {
                        path: None,
                        literal: Spanned::new(span, literal),
                    }
                    .into(),
                )
            }
            QuestionMark => {
                self.advance();
                let tag = self.parse_word()?;

                Expression::new(default(), span.merge(tag), ast::TypedHole { tag }.into())
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

                span.merging(self.expect(ClosingRoundBracket)?);
                expression.span = span;

                expression
            }
            PathHead!() => self.parse_path_or_namespaced_literal()?,
            _ => {
                self.expected("expression");
                return self.error_with(|this, it| {
                    if this.token() == ThinArrowRight {
                        it.help(
                            "consider adding round brackets around the potential \
                             function type to disambiguate the expression",
                        )
                    } else {
                        it
                    }
                });
            }
        };

        let mut attributes = Some(attributes);

        while self.consume(DoubleColon) {
            let field = self.parse_word()?;

            expression = Expression::new(
                attributes.take().unwrap_or_default(),
                expression.span.merge(field),
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

    /// Parse a path.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// Path ::= Path-Head ("." Identifier)*
    /// ```
    fn parse_path(&mut self) -> Result<ast::Path> {
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
        let path = match self.token() {
            Identifier!(head) => Identifier::new_unchecked(head, self.span()).into(),
            PathHanger!() => self
                .current()
                .map(|token| ast::BareHanger::try_from(token).unwrap())
                .into(),
            _ => {
                self.expected("path");
                return self.error();
            }
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
        let parameters = self.parse_parameters()?;
        let codomain = self.parse_optional_type_annotation()?;
        self.parse_wide_arrow()?;
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

        let quantifier = match self.token() {
            ThinArrowRight => ast::Quantifier::Pi,
            DoubleAsterisk => ast::Quantifier::Sigma,
            _ => {
                self.expected(ThinArrowRight);
                self.expected(DoubleAsterisk);

                return self.error_with(|this, it| {
                    let it = it.label(span, "while parsing this quantified type starting here");
                    let span = this.span();

                    if let WideArrowRight = this.token() {
                        // @Note users might have also thought that this was the way to denote
                        // a function type with implicit or auto-implicit arguments.
                        // @Task add a note or suggestion to teach the actual syntax for those
                        // if there are apostrophes present, only suggest auto-implicit parameters

                        it.suggest(
                            span,
                            "consider replacing the wide arrow with a \
                             thin one to denote a function type",
                            "->",
                        )
                    } else {
                        it
                    }
                });
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
        let binder = self.parse_word()?;
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
        let bindings = self.parse_use_path_tree()?;

        if let LineBreak = self.token() {
            self.advance();
        }

        self.expect(In)?;

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
                    let binder = self.parse_word()?;

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
                    self.expected("statement");

                    let expression = self.parse_expression()?;
                    self.parse_terminator()?;

                    ast::Statement::Expression(expression)
                }
            });
        }

        span.merging(self.span());
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
    /// Parameter ::= Explicitness Bare-Parameter
    /// Bare-Parameter ::= #Word | "(" #Word Type-Annotation? ")"
    /// ```
    fn parse_parameters(&mut self) -> Result<ast::Parameters> {
        let mut parameters = SmallVec::new();

        loop {
            let explicitness = self.parse_optional_implicitness();
            let mut span = self.span().merge_into(explicitness);

            let parameter = match self.token() {
                Word(binder) => {
                    let binder = ast::Identifier::new_unchecked(binder, self.span());
                    self.advance();

                    ast::Parameter::new(
                        span,
                        ast::BareParameter {
                            explicitness: explicitness.into(),
                            binder,
                            type_: None,
                        },
                    )
                }
                OpeningRoundBracket => {
                    self.advance();

                    let binder = self.parse_word()?;
                    let type_ = self.parse_optional_type_annotation()?;

                    span.merging(self.expect(ClosingRoundBracket)?);

                    ast::Parameter::new(
                        span,
                        ast::BareParameter {
                            explicitness: explicitness.into(),
                            binder,
                            type_,
                        },
                    )
                }
                _ => {
                    self.expected("parameter");
                    break;
                }
            };

            parameters.push(parameter);
        }

        Ok(parameters)
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

        let mut span = self.span();
        let mut pattern = match self.token() {
            NumberLiteral(literal) => {
                self.advance();

                Pattern::new(
                    default(),
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

                Pattern::new(
                    default(),
                    span,
                    ast::TextLiteral {
                        path: None,
                        literal: Spanned::new(span, literal),
                    }
                    .into(),
                )
            }
            Backslash => {
                self.advance();
                let binder = self.parse_word()?;
                Pattern::new(default(), span.merge(binder), binder.into())
            }
            OpeningSquareBracket => {
                self.advance();

                let mut elements = Vec::new();

                while self.token() != ClosingSquareBracket {
                    elements.push(self.parse_lower_pattern()?);
                }

                span.merging(self.span());
                self.advance(); // "]"

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
                span.merging(self.expect(ClosingRoundBracket)?);
                pattern.span = span;

                pattern
            }
            PathHead!() => self.parse_path_or_namespaced_literal()?,
            _ => {
                self.expected("pattern");
                return self.error();
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

        while self.token() != ClosingSquareBracket {
            elements.push(T::parse_lower(self)?);
        }

        span.merging(self.span());
        self.advance(); // "]"

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

            //
            // Parse an optional argument.
            //
            let (binder, argument) = if self.token() == OpeningRoundBracket
                && let Spanned!(binder_span, Word(binder)) = self.look_ahead(1)
                && self.look_ahead(2).bare == Equals
            {
                self.advance(); // "("
                self.advance(); // #Word
                self.advance(); // "="

                let argument = T::parse(self)?;

                span.merging(self.expect(ClosingRoundBracket)?);

                (Some(ast::Identifier::new_unchecked(binder, binder_span)), argument)
            } else if T::is_lower_prefix(self.token()) {
                (None, span.merging(T::parse_lower(self)?))
            } else {
                self.expected("function argument");
                // @Task avoid doing this work in the happy path (vtable construction & heap allocation)
                self.context(move |it| {
                    it.label(callee.span, format!("while parsing this {}", T::NAME))
                });

                if let Some(explicitness) = explicitness {
                    return self.error_with(|_, it| {
                        it.label(
                            explicitness,
                            "this apostrophe marks the start of an implicit argument",
                        )
                    });
                }

                // The current token is not the start of an argument.
                // Hence we are done here.
                return Ok(callee);
            };

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
        let mut path = self.parse_path_head()?;

        while self.consume(Dot) {
            let span = self.span();
            match self.token() {
                Identifier!(segment) => {
                    self.advance();
                    path.segments
                        .push(ast::Identifier::new_unchecked(segment, span));
                }
                NumberLiteral(literal) => {
                    self.advance();

                    return Ok(ast::Item::new(
                        default(),
                        path.span().merge(span),
                        ast::NumberLiteral {
                            path: Some(path),
                            literal: Spanned::new(span, literal),
                        }
                        .into(),
                    ));
                }
                TextLiteral(literal) => {
                    self.advance();

                    return Ok(ast::Item::new(
                        default(),
                        path.span().merge(span),
                        ast::TextLiteral {
                            path: Some(path),
                            literal: Spanned::new(span, literal),
                        }
                        .into(),
                    ));
                }
                OpeningSquareBracket => {
                    self.advance();
                    return self.finish_parse_sequence_literal(Some(path), span);
                }
                _ => {
                    self.expected(IDENTIFIER);
                    self.expected("number literal");
                    self.expected("text literal");
                    self.expected(OpeningSquareBracket);
                    return self.error();
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
        self.consume(Colon)
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
                DocumentationComment => {
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
                let binder = ast::Identifier::new_unchecked(binder, self.span());

                span.merging(&binder);
                self.advance();

                binder
            }
            OpeningRoundBracket => {
                self.advance();
                let binder = self.parse_word()?;

                while self.token() != ClosingRoundBracket {
                    arguments.push(self.parse_attribute_argument()?);
                }

                span.merging(self.span());
                self.advance(); // ")"

                binder
            }
            _ => {
                self.expected(WORD);
                self.expected(OpeningRoundBracket);
                return self.error_with(|_, it| {
                    it.label(span, "while parsing this attribute starting here")
                });
            }
        };

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
                self.expected("attribute argument");
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
    fn parse_identifier(&mut self) -> Result<Identifier> {
        if let Spanned!(span, Identifier!(identifier)) = self.current() {
            self.advance();
            Ok(ast::Identifier::new_unchecked(identifier, span))
        } else {
            self.expected(IDENTIFIER);
            self.error()
        }
    }

    fn parse_word(&mut self) -> Result<Identifier> {
        if let Spanned!(span, Word(word)) = self.current() {
            self.advance();
            Ok(ast::Identifier::new_unchecked(word, span))
        } else {
            self.expected(WORD);
            self.error()
        }
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
    fn parse_terminator(&mut self) -> Result<Option<Span>> {
        let token = self.token();
        if matches!(token, Terminator!())
            || self
                .look_behind(1)
                .map_or(true, |token| matches!(token.bare, Terminator!()))
        {
            Ok(if token == LineBreak {
                let span = self.span();
                self.advance();

                Some(span)
            } else {
                None
            })
        } else {
            self.expected(TERMINATOR);
            self.error()
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
        if self.token() == Apostrophe {
            let span = self.span();
            self.advance();
            Some(span)
        } else {
            None
        }
    }

    fn parse_wide_arrow(&mut self) -> Result<()> {
        if let ThinArrowRight = self.token() {
            let span = self.span();
            self.context(move |it| {
                it.suggest(
                    span,
                    "consider replacing the thin arrow with a wide one",
                    "=>",
                )
            });
        }

        self.expect(WideArrowRight)?;
        Ok(())
    }
}

trait Parse: Sized {
    const NAME: &'static str;

    fn parse(parser: &mut Parser<'_>) -> Result<ast::Item<Self>>;
    fn parse_lower(parser: &mut Parser<'_>) -> Result<ast::Item<Self>>;
    fn is_lower_prefix(token: BareToken) -> bool;
}

impl Parse for ast::BareExpression {
    const NAME: &'static str = "expression";

    fn parse(parser: &mut Parser<'_>) -> Result<Expression> {
        parser.parse_expression()
    }
    fn parse_lower(parser: &mut Parser<'_>) -> Result<Expression> {
        parser.parse_lower_expression()
    }

    fn is_lower_prefix(token: BareToken) -> bool {
        matches!(token, LowerExpressionPrefix!())
    }
}

impl Parse for ast::BarePattern {
    const NAME: &'static str = "pattern";

    fn parse(parser: &mut Parser<'_>) -> Result<Pattern> {
        parser.parse_pattern()
    }
    fn parse_lower(parser: &mut Parser<'_>) -> Result<Pattern> {
        parser.parse_lower_pattern()
    }

    fn is_lower_prefix(token: BareToken) -> bool {
        matches!(token, LowerPatternPrefix!())
    }
}

const IDENTIFIER: &str = "identifier";
const TERMINATOR: &str = "terminator";
const WORD: &str = "word";

mod synonym {
    //! A collection of *pattern synonyms*.
    //!
    //! Contrary to methods or arrays, they preserve `match` exhaustiveness & overlap checks at use sites.

    use lexer::token::BareToken::*;

    /// An [identifier]
    ///
    /// [identifier]: ast::Identifier
    pub(crate) macro Identifier($identifier:pat) {
        Word($identifier) | Symbol($identifier)
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
            | NumberLiteral(_)
            | TextLiteral(_)
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
            | NumberLiteral(_)
            | TextLiteral(_)
            | Backslash
            | OpeningSquareBracket
            | OpeningRoundBracket
            | PathHead!()
    }

    /// The head (i.e. prefix) of a [path].
    ///
    /// [path]: ast::Path
    pub(crate) macro PathHead() {
        Identifier!(_) | PathHanger!()
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
