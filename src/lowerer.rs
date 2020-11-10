//! The lowering stage.
//!
//! Higher-level syntactic constructs in the AST are simplified and rewritten
//! in terms of lower-level language primitives and undergo several checks since
//! they might be more liberal than the intermediate languages allow.
//!
//! This pass does the following:
//!
//! * lower let/in expressions to lambda literals
//! * lower parameters to simple lambda literals
//! * open external modules (this will probably move to the parser in the future
//!   for parallel reading and independent error reporting)
//! * simplifiy use declarations by unfolding path trees
//! * apply attribute groups (unimplemented right now)
//! * parse number literals according to their type indicated by attributes (unsure
//!   if this is the right place or whether it should be moved to a later stage)
//! * parses general attributes into concrete ones and
//! * validates their location (item target), uniqueness if applicable,
//!   exclusivity rules
//! * checks if the mandatory type annotations on top-level declarations and their
//!   parameters are present
//! * validates parameters marked as record fields
//! * gates a lot of unsupported features

// @Task ungate named arguments but validate them in the resolver (and/or typer)

pub mod lowered_ast;

use crate::{
    ast::{self, Explicit, Path},
    diagnostic::{Code, Diagnostic, Diagnostics, Result, Results},
    lowered_ast::{decl, expr, pat, AttributeKeys, Attributes, Number},
    smallvec,
    span::{SourceMap, Span, Spanning},
    support::listing,
    support::{accumulate_errors, s_pluralize, InvalidFallback, ManyErrExt, TryIn},
    SmallVec, Str,
};
use joinery::JoinableIterator;
use std::iter::once;

#[derive(Clone, Copy)]
struct Context {
    in_constructor: bool,
    declaration: Span,
}

impl Context {
    fn new(declaration: Span) -> Self {
        Self {
            in_constructor: false,
            declaration,
        }
    }
}

/// The state of the lowering pass.
pub struct Lowerer<'a> {
    map: &'a mut SourceMap,
    warnings: &'a mut Diagnostics,
}

impl<'a> Lowerer<'a> {
    pub fn new(map: &'a mut SourceMap, warnings: &'a mut Diagnostics) -> Self {
        Self { map, warnings }
    }

    #[allow(dead_code)]
    fn warn(&mut self, diagnostic: Diagnostic) {
        self.warnings.insert(diagnostic);
    }

    /// Lower a declaration.
    pub fn lower_declaration(
        &mut self,
        declaration: ast::Declaration,
    ) -> Results<SmallVec<lowered_ast::Declaration, 1>> {
        use ast::DeclarationKind::*;

        let mut errors = Diagnostics::default();

        let attributes = self
            .lower_attributes(&declaration.attributes, &declaration)
            .try_in(&mut errors);

        match declaration.kind {
            Value(value) => {
                let context = Context::new(declaration.span);

                let declaration_type_annotation = match value.type_annotation {
                    Some(type_annotation) => type_annotation,
                    None => Err(missing_mandatory_type_annotation(
                        &declaration.span,
                        AnnotationTarget::Declaration,
                    ))
                    .try_in(&mut errors),
                };

                // @Note type_annotation is currently lowered twice @Task remove duplicate work
                // @Task find a way to use `Option::map` (currently does not work because of
                // partial moves, I hate those), use local bindings
                let expression = match value.expression {
                    Some(expression) => {
                        let mut expression = self
                            .lower_expression(expression, context)
                            .try_in(&mut errors);
                        {
                            let mut type_annotation = once(
                                self.lower_expression(declaration_type_annotation.clone(), context)
                                    .try_in(&mut errors),
                            );

                            for parameter_group in value.parameters.iter().rev() {
                                let parameter_type_annotation = match &parameter_group
                                    .type_annotation
                                {
                                    Some(type_annotation) => {
                                        self.lower_expression(type_annotation.clone(), context)
                                    }
                                    None => Err(missing_mandatory_type_annotation(
                                        parameter_group,
                                        AnnotationTarget::Parameters(&parameter_group.parameters),
                                    ))
                                    .many_err(),
                                }
                                .try_in(&mut errors);

                                for binder in parameter_group.parameters.iter().rev() {
                                    expression = expr! {
                                        Lambda {
                                            Attributes::default(),
                                            Span::SHAM;
                                            parameter: binder.clone(),
                                            parameter_type_annotation: Some(parameter_type_annotation.clone()),
                                            explicitness: parameter_group.explicitness,
                                            body_type_annotation: type_annotation.next(),
                                            body: expression,
                                        }
                                    }
                                }
                            }
                        }
                        Some(expression)
                    }
                    None => None,
                };

                let type_annotation = self
                    .lower_parameters_to_annotated_ones(
                        value.parameters,
                        declaration_type_annotation,
                        context,
                    )
                    .try_in(&mut errors);

                errors.err_or(smallvec![decl! {
                    Value {
                        attributes,
                        declaration.span;
                        binder: value.binder,
                        type_annotation,
                        expression,
                    }
                }])
            }
            Data(data) => {
                let data_type_annotation = match data.type_annotation {
                    Some(type_annotation) => type_annotation,
                    None => Err(missing_mandatory_type_annotation(
                        &declaration.span,
                        AnnotationTarget::Declaration,
                    ))
                    .try_in(&mut errors),
                };

                let type_annotation = self
                    .lower_parameters_to_annotated_ones(
                        data.parameters,
                        data_type_annotation,
                        Context::new(declaration.span),
                    )
                    .try_in(&mut errors);

                let constructors = data.constructors.map(|constructors| {
                    constructors
                        .into_iter()
                        .flat_map(|constructor| {
                            self.lower_declaration(constructor).try_in(&mut errors)
                        })
                        .collect()
                });

                errors.err_or(smallvec![decl! {
                    Data{
                        attributes,
                        declaration.span;
                        binder: data.binder,
                        type_annotation,
                        constructors,
                    }
                }])
            }
            // @Beacon @Task check multiple record constructors
            Constructor(constructor) => {
                let constructor_type_annotation = match constructor.type_annotation {
                    Some(type_annotation) => type_annotation,
                    None => Err(missing_mandatory_type_annotation(
                        &declaration.span,
                        AnnotationTarget::Declaration,
                    ))
                    .try_in(&mut errors),
                };

                let type_annotation = self
                    .lower_parameters_to_annotated_ones(
                        constructor.parameters,
                        constructor_type_annotation,
                        Context {
                            in_constructor: true,
                            declaration: declaration.span,
                        },
                    )
                    .try_in(&mut errors);

                errors.err_or(smallvec![decl! {
                    Constructor {
                        attributes,
                        declaration.span;
                        binder: constructor.binder,
                        type_annotation,
                    }
                }])
            }
            Module(module) => {
                let declarations =
                    match module.declarations {
                        Some(declarations) => declarations,
                        // @Bug @Task disallow external module declarations inside of non-file modules
                        None => {
                            use crate::{lexer::Lexer, parser::Parser, span};

                            let path = module
                                .file
                                .path
                                .parent()
                                .unwrap()
                                .join(module.binder.as_str())
                                .with_extension(crate::FILE_EXTENSION);

                            let declaration_span = declaration.span;

                            let file = self
                                .map
                                .load(&path)
                                .map_err(|error| match error {
                                    span::Error::LoadFailure(_) => Diagnostic::error()
                                        .with_code(Code::E016)
                                        .with_message(format!(
                                            "could not load module `{}`",
                                            module.binder
                                        ))
                                        .with_span(&declaration_span)
                                        .with_note(error.message(Some(&path))),
                                    // @Task add context information
                                    error => error.into(),
                                })
                                .map_err(|error| errors.clone().inserted(error))?;

                            let tokens = Lexer::new(&file, &mut self.warnings).lex()?;
                            let node = Parser::new(file, &tokens, &mut self.warnings)
                                .parse_top_level(module.binder.clone())
                                .map_err(|error| errors.clone().inserted(error))?;
                            let module = match node.kind {
                                Module(module) => module,
                                _ => unreachable!(),
                            };
                            // @Temporary
                            if !node.attributes.is_empty() {
                                self.warn(Diagnostic::warning().with_message(
                                    "attributes on module headers are ignored right now",
                                ))
                            }
                            module.declarations.unwrap()
                        }
                    };

                let declarations = declarations
                    .into_iter()
                    .flat_map(|declaration| self.lower_declaration(declaration).try_in(&mut errors))
                    .collect();

                errors.err_or(smallvec![decl! {
                    Module {
                        attributes,
                        declaration.span;
                        binder: module.binder,
                        file: module.file,
                        declarations,
                    }
                }])
            }
            // @Temporary
            Crate(_) => Err(errors.inserted(
                Diagnostic::bug()
                    .with_message("crate declarations not supported yet")
                    .with_span(&declaration.span),
            )),
            // @Beacon @Task merge attributes with parent module
            // (through a `Context`) and ensure it's the first declaration in the whole module
            Header => Err(errors.inserted(
                Diagnostic::bug()
                    .with_message("module headers not supported yet")
                    .with_span(&declaration.span),
            )),
            // overarching attributes be placed *above* the subdeclaration attributes
            Group(group) => {
                let group_attributes = declaration.attributes;

                let group = group
                    .declarations
                    .into_iter()
                    .map(|mut declaration| {
                        let mut group_attributes = group_attributes.clone();
                        group_attributes.append(&mut declaration.attributes);
                        declaration.attributes = group_attributes;
                        declaration
                    })
                    .flat_map(|declaration| self.lower_declaration(declaration).try_in(&mut errors))
                    .collect();

                errors.err_or(group)
            }
            Use(use_) => {
                use ast::PathTree::{self, *};

                let mut declarations = SmallVec::new();

                fn lower_path_tree_multiple_paths(
                    path: Path,
                    bindings: Vec<PathTree>,
                    span: Span,
                    attributes: lowered_ast::Attributes,
                    declarations: &mut SmallVec<lowered_ast::Declaration, 1>,
                ) -> Result<()> {
                    for binding in bindings {
                        match binding {
                            ast::PathTree::Single { target, binder } => {
                                // if the binder is not explicitly set, look for the most-specific/last/right-most
                                // identifier of the target but if that one is `self`, look up the last identifier of
                                // the parent path
                                declarations.push(decl! {
                                    Use {
                                        attributes.clone(),
                                        span;
                                        binder: binder.or_else(|| if !target.is_self() { &target } else { &path }
                                            .last_identifier().cloned()),
                                        target: path.clone().join(target)?,
                                    }
                                });
                            }
                            ast::PathTree::Multiple {
                                path: inner_path,
                                bindings,
                            } => {
                                lower_path_tree_multiple_paths(
                                    path.clone().join(inner_path)?,
                                    bindings,
                                    span,
                                    attributes.clone(),
                                    declarations,
                                )?;
                            }
                        }
                    }

                    Ok(())
                };

                match use_.bindings {
                    Single { target, binder } => declarations.push(decl! {
                        Use {
                            attributes,
                            declaration.span;
                            binder: binder.or_else(|| target.last_identifier().cloned()),
                            target,
                        }
                    }),
                    Multiple { path, bindings } => {
                        lower_path_tree_multiple_paths(
                            path,
                            bindings,
                            declaration.span,
                            attributes,
                            &mut declarations,
                        )
                        .try_in(&mut errors);
                    }
                }

                errors.err_or(declarations)
            }
        }
    }

    /// Lower an expression.
    fn lower_expression(
        &mut self,
        expression: ast::Expression,
        context: Context,
    ) -> Results<lowered_ast::Expression> {
        use ast::ExpressionKind::*;

        let mut errors = Diagnostics::default();

        let attributes = self
            .lower_attributes(&expression.attributes, &expression)
            .try_in(&mut errors);

        match expression.kind {
            PiTypeLiteral(pi) => {
                self.check_fieldness_location(pi.fieldness, context)
                    .try_in(&mut errors);

                let ((), domain, codomain) = accumulate_errors!(
                    errors.err_or(()),
                    self.lower_expression(pi.domain, context),
                    self.lower_expression(pi.codomain, context),
                )?;

                Ok(expr! {
                    PiType {
                        attributes,
                        expression.span;
                        parameter: pi.binder.clone(),
                        domain,
                        codomain,
                        explicitness: pi.explicitness,
                        is_field: pi.fieldness.is_some(),
                    }
                })
            }
            Application(application) => {
                // @Temporary
                if let Some(binder) = &application.binder {
                    self.warn(
                        Diagnostic::warning()
                            .with_message("named arguments not supported yet")
                            .with_span(binder),
                    )
                }

                let ((), callee, argument) = accumulate_errors!(
                    errors.err_or(()),
                    self.lower_expression(application.callee, context),
                    self.lower_expression(application.argument, context),
                )?;

                Ok(expr! {
                    Application {
                        attributes,
                        expression.span;
                        callee,
                        argument,
                        explicitness: application.explicitness,
                    }
                })
            }
            TypeLiteral => errors.err_or(expr! { Type { attributes, expression.span } }),
            NumberLiteral(literal) => {
                // @Note *very* awkward API!
                let ((), literal) = accumulate_errors!(
                    errors.err_or(()),
                    self.lower_number_literal(*literal, expression.span, &attributes)
                )?;

                Ok(expr! { Number(attributes, expression.span; literal) })
            }
            TextLiteral(text) => errors.err_or(expr! {
                Text(attributes, expression.span; text)
            }),
            TypedHole(_hole) => Err(errors.inserted(
                Diagnostic::bug()
                    .with_message("typed holes not supported yet")
                    .with_span(&expression.span),
            )),
            Path(path) => errors.err_or(expr! {
                Binding {
                    attributes,
                    expression.span;
                    binder: *path,
                }
            }),
            LambdaLiteral(lambda) => {
                let mut expression = self
                    .lower_expression(lambda.body, context)
                    .try_in(&mut errors);

                let mut type_annotation = lambda
                    .body_type_annotation
                    .map(|type_annotation| {
                        self.lower_expression(type_annotation, context)
                            .try_in(&mut errors)
                    })
                    .into_iter();

                for parameter_group in lambda.parameters.iter().rev() {
                    let parameter =
                        parameter_group
                            .type_annotation
                            .clone()
                            .map(|type_annotation| {
                                self.lower_expression(type_annotation, context)
                                    .try_in(&mut errors)
                            });

                    for binder in parameter_group.parameters.iter().rev() {
                        expression = expr! {
                            Lambda {
                                Attributes::default(),
                                Span::SHAM;
                                parameter: binder.clone(),
                                parameter_type_annotation: parameter.clone(),
                                explicitness: parameter_group.explicitness,
                                body_type_annotation: type_annotation.next(),
                                body: expression,
                            }
                        };
                    }
                }

                errors.err_or(expression)
            }
            LetIn(let_in) => {
                let mut expression = self
                    .lower_expression(let_in.expression, context)
                    .try_in(&mut errors);

                let mut type_annotation = let_in
                    .type_annotation
                    .map(|type_annotation| {
                        self.lower_expression(type_annotation, context)
                            .try_in(&mut errors)
                    })
                    .into_iter();

                for parameter_group in let_in.parameters.iter().rev() {
                    let parameter = parameter_group.type_annotation.clone().map(|expression| {
                        self.lower_expression(expression, context)
                            .try_in(&mut errors)
                    });

                    for binder in parameter_group.parameters.iter().rev() {
                        expression = expr! {
                            Lambda {
                                Attributes::default(),
                                Span::SHAM;
                                parameter: binder.clone(),
                                parameter_type_annotation: parameter.clone(),
                                explicitness: parameter_group.explicitness,
                                body_type_annotation: type_annotation.next(),
                                body: expression,
                            }
                        };
                    }
                }

                let body = self
                    .lower_expression(let_in.scope, context)
                    .try_in(&mut errors);

                errors.err_or(expr! {
                    Application {
                        Attributes::default(),
                        Span::SHAM;
                        callee: expr! {
                            Lambda {
                                Attributes::default(),
                                Span::SHAM;
                                parameter: let_in.binder,
                                // @Note we cannot simply lower parameters and a type annotation because
                                // in the chain (`->`) of parameters, there might always be one missing and
                                // we don't support partial type annotations yet (using `_`)
                                // @Temporary @Update @Bug -gy because we ignore above message
                                // @Task verify correct semantics
                                parameter_type_annotation: type_annotation.next(),
                                explicitness: Explicit,
                                body_type_annotation: None,
                                body,
                            }
                        },
                        argument: expression,
                        explicitness: Explicit,
                    }
                })
            }
            // @Beacon @Task
            UseIn(_use_in) => Err(errors.inserted(
                Diagnostic::bug()
                    .with_message("use/in expressions not supported yet")
                    .with_span(&expression.span),
            )),
            CaseAnalysis(analysis) => {
                let mut cases = Vec::new();

                for case_group in analysis.cases {
                    cases.push(lowered_ast::Case {
                        pattern: self.lower_pattern(case_group.pattern).try_in(&mut errors),
                        body: self
                            .lower_expression(case_group.expression.clone(), context)
                            .try_in(&mut errors),
                    });
                }

                let subject = self
                    .lower_expression(analysis.expression, context)
                    .try_in(&mut errors);

                errors.err_or(expr! {
                    CaseAnalysis {
                        attributes,
                        expression.span;
                        subject,
                        cases,
                    }
                })
            }
            DoBlock(_block) => Err(errors.inserted(
                Diagnostic::bug()
                    .with_message("do blocks not fully implemented yet")
                    .with_span(&expression.span),
            )),
            SequenceLiteral(_sequence) => Err(errors.inserted(
                Diagnostic::bug()
                    .with_message("sequence literals not fully implemented yet")
                    .with_span(&expression.span),
            )),
            Invalid => errors.err_or(InvalidFallback::invalid()),
        }
    }

    /// Lower a pattern.
    fn lower_pattern(&mut self, pattern: ast::Pattern) -> Results<lowered_ast::Pattern> {
        use ast::PatternKind::*;

        let mut errors = Diagnostics::default();

        let attributes = self
            .lower_attributes(&pattern.attributes, &pattern)
            .try_in(&mut errors);

        match pattern.kind {
            NumberLiteral(literal) => {
                // @Note *very* awkward API!
                let ((), literal) = accumulate_errors!(
                    errors.err_or(()),
                    self.lower_number_literal(*literal, pattern.span, &attributes)
                )?;

                Ok(pat! {
                    Number(attributes, pattern.span; literal)
                })
            }
            TextLiteral(literal) => errors.err_or(pat! {
                Text(attributes, pattern.span; literal)
            }),
            Path(path) => errors.err_or(pat! {
                Binding {
                    attributes,
                    pattern.span;
                    binder: *path,
                }
            }),
            Binder(binding) => errors.err_or(pat! {
                Binder {
                    attributes,
                    pattern.span;
                    binder: binding.binder,
                }
            }),
            Deapplication(application) => {
                // @Temporary
                if let Some(binder) = &application.binder {
                    self.warn(
                        Diagnostic::warning()
                            .with_message("named arguments not supported yet")
                            .with_span(binder),
                    )
                }

                let ((), callee, argument) = accumulate_errors!(
                    errors.err_or(()),
                    self.lower_pattern(application.callee),
                    self.lower_pattern(application.argument),
                )?;

                Ok(pat! {
                    Deapplication {
                        attributes,
                        pattern.span;
                        callee,
                        argument,
                    }
                })
            }
            SequenceLiteralPattern(_sequence) => Err(errors.inserted(
                Diagnostic::bug()
                    .with_message("sequence literal patterns not supported yet")
                    .with_span(&pattern.span),
            )),
        }
    }

    /// Lower attributes.
    // @Task use accumulate_errors instead of TryIn
    fn lower_attributes(
        &mut self,
        attributes: &[ast::Attribute],
        target: &impl ast::AttributeTarget,
    ) -> Results<Attributes> {
        use lowered_ast::Attribute;

        let targets = target.as_attribute_targets();
        let mut errors = Diagnostics::default();
        let mut lowered_attributes = Vec::new();

        for attribute in attributes {
            // @Task move the parse function to this module? (maybe)
            let attribute = match Attribute::parse(attribute) {
                Ok(attribute) => attribute,
                Err(parse_errors) => {
                    errors.extend(parse_errors);
                    continue;
                }
            };

            // non-conforming attributes
            if !attribute.kind.targets().contains(targets) {
                errors.insert(
                    Diagnostic::error()
                        .with_code(Code::E013)
                        .with_message(format!(
                            "attribute {} cannot be ascribed to {}",
                            attribute.kind.quoted_name(),
                            target.name()
                        ))
                        .with_labeled_span(&attribute, "misplaced attribute")
                        .with_labeled_span(target, "incompatible item")
                        .with_note(format!(
                            "attribute {} can only be ascribed to {}",
                            attribute.kind.quoted_name(),
                            attribute.kind.targets_as_str()
                        )),
                );
                continue;
            }

            lowered_attributes.push(attribute);
        }

        let mut keys = AttributeKeys::empty();
        let mut attributes = Vec::new();

        // conflicting or duplicate attributes
        for attribute in lowered_attributes.iter() {
            let key = attribute.kind.key();
            let coexistable = AttributeKeys::COEXISTABLE.contains(key);

            if !keys.contains(key) || coexistable {
                attributes.push(attribute.clone());
            }

            keys |= key;

            if coexistable {
                continue;
            }

            let matching_attributes: Vec<_> = lowered_attributes
                .iter()
                .filter(|attribute| attribute.matches(key))
                .collect();

            if matching_attributes.len() > 1 {
                errors.extend(Diagnostic::error().multiple_labeled(
                    matching_attributes,
                    "duplicate or conflicting attribute",
                    |faulty_attributes| {
                        format!(
                            "multiple {} attributes",
                            faulty_attributes.first().unwrap().kind.quoted_name(),
                        )
                    },
                ));
            }
        }

        let attributes = Attributes {
            keys,
            data: attributes.into_boxed_slice(),
        };

        target.check_attributes(&attributes).try_in(&mut errors);

        if attributes.keys.is_empty() {
            return errors.err_or(attributes);
        }

        let check_mutual_exclusivity =
            |mutually_exclusive_attributes: AttributeKeys| -> Result<(), Diagnostics> {
                if (attributes.keys & mutually_exclusive_attributes)
                    .bits()
                    .count_ones()
                    > 1
                {
                    return Err(Diagnostic::error().with_code(Code::E014).multiple_labeled(
                        attributes.get(mutually_exclusive_attributes).collect(),
                        "conflicting attribute",
                        |faulty_attributes| {
                            format!(
                                "attributes {} are mutually exclusive",
                                listing(
                                    faulty_attributes
                                        .iter()
                                        .map(|attribute| attribute.kind.quoted_name()),
                                    "and"
                                )
                            )
                        },
                    ));
                }

                Ok(())
            };

        check_mutual_exclusivity(AttributeKeys::FOREIGN | AttributeKeys::INHERENT)
            .try_in(&mut errors);
        check_mutual_exclusivity(AttributeKeys::MOVING | AttributeKeys::OPAQUE).try_in(&mut errors);
        check_mutual_exclusivity(
            AttributeKeys::INT
                | AttributeKeys::INT32
                | AttributeKeys::INT64
                | AttributeKeys::NAT
                | AttributeKeys::NAT32
                | AttributeKeys::NAT64,
        )
        .try_in(&mut errors);
        check_mutual_exclusivity(AttributeKeys::RUNE | AttributeKeys::TEXT).try_in(&mut errors);
        check_mutual_exclusivity(AttributeKeys::LIST | AttributeKeys::VECTOR).try_in(&mut errors);

        if attributes.within(AttributeKeys::UNSUPPORTED) {
            errors.extend(attributes.get(AttributeKeys::UNSUPPORTED).map(|attribute| {
                Diagnostic::bug()
                    .with_message(format!(
                        "attribute {} not supported yet",
                        attribute.kind.quoted_name()
                    ))
                    .with_span(attribute)
            }))
        }

        errors.err_or(attributes)
    }

    // @Question should this actually happen in the lowering stage?
    fn lower_number_literal(
        &mut self,
        number: String,
        span: Span,
        attributes: &Attributes,
    ) -> Results<Number> {
        (if attributes.has(AttributeKeys::NAT32) {
            number
                .parse()
                .map_err(|_| ("Nat32", "[0, 2^32-1]"))
                .map(Number::Nat32)
        } else if attributes.has(AttributeKeys::NAT64) {
            number
                .parse()
                .map_err(|_| ("Nat64", "[0, 2^64-1]"))
                .map(Number::Nat64)
        } else if attributes.has(AttributeKeys::INT) {
            Ok(Number::Int(number.parse().unwrap()))
        } else if attributes.has(AttributeKeys::INT32) {
            number
                .parse()
                .map_err(|_| ("Int32", "[-2^31, 2^31-1]"))
                .map(Number::Int32)
        } else if attributes.has(AttributeKeys::INT64) {
            number
                .parse()
                .map_err(|_| ("Int64", "[-2^63, 2^63-1]"))
                .map(Number::Int64)
        } else {
            // optionally attributes.has(AttributeKeys::NAT)
            number
                .parse()
                .map_err(|_| ("Nat", "[0, infinity)"))
                .map(Number::Nat)
        })
        .map_err(|(type_name, interval)| {
            Diagnostic::error()
                .with_code(Code::E007)
                .with_message(format!(
                    "number literal `{}` does not fit type `{}`",
                    number, type_name
                ))
                .with_span(&span)
                .with_note(format!(
                    "values of this type must fit integer interval {}",
                    interval
                ))
        })
        .many_err()
    }

    /// Lower annotated parameters.
    fn lower_parameters_to_annotated_ones(
        &mut self,
        parameters: ast::Parameters,
        type_annotation: ast::Expression,
        context: Context,
    ) -> Results<lowered_ast::Expression> {
        let mut errors = Diagnostics::default();

        let mut expression = self
            .lower_expression(type_annotation, context)
            .try_in(&mut errors);

        for parameter_group in parameters.into_iter().rev() {
            let parameter_type_annotation = match parameter_group.type_annotation {
                Some(type_annotation) => self.lower_expression(type_annotation, context),
                None => Err(missing_mandatory_type_annotation(
                    &parameter_group,
                    AnnotationTarget::Parameters(&parameter_group.parameters),
                ))
                .many_err(),
            }
            .try_in(&mut errors);

            self.check_fieldness_location(parameter_group.fieldness, context)
                .try_in(&mut errors);

            for binder in parameter_group.parameters.iter().rev() {
                expression = expr! {
                    PiType {
                        Attributes::default(),
                        Span::SHAM;
                        parameter: Some(binder.clone()),
                        domain: parameter_type_annotation.clone(),
                        codomain: expression,
                        explicitness: parameter_group.explicitness,
                        is_field: parameter_group.fieldness.is_some(),
                    }
                };
            }
        }

        errors.err_or(expression)
    }

    fn check_fieldness_location(
        &mut self,
        fieldness: Option<Span>,
        context: Context,
    ) -> Result<()> {
        if let Some(field) = fieldness {
            if !context.in_constructor {
                // @Note it would be helpful to also say the name of the actual declaration
                // but I think we lack a method for this right now
                return Err(Diagnostic::error()
                    .with_code(Code::E017)
                    .with_message("`field` used outside of a constructor declaration")
                    .with_span(&field)
                    .with_labeled_span(&context.declaration, "not a constructor"));
            }
        }

        Ok(())
    }
}

fn missing_mandatory_type_annotation(
    spanning: &impl Spanning,
    target: AnnotationTarget<'_>,
) -> Diagnostic {
    let type_annotation_suggestion: Str = match target {
        AnnotationTarget::Parameters(parameters) => {
            format!("`({}: TYPE)`", parameters.iter().join_with(' ')).into()
        }
        AnnotationTarget::Declaration => "`: TYPE`".into(),
    };

    Diagnostic::error()
        .with_code(Code::E015)
        .with_message(format!(
            "missing mandatory type annotation on {}",
            target.name()
        ))
        .with_span(spanning)
        .with_help(format!(
            "provide a type annotation for the {} with {}",
            target.name(),
            type_annotation_suggestion,
        ))
}

/// A place in the AST which can have a syntactically optional type annotation.
///
/// Used for error reporting exclusively.
enum AnnotationTarget<'a> {
    Parameters(&'a [ast::Identifier]),
    Declaration,
}

impl AnnotationTarget<'_> {
    fn name(&self) -> &'static str {
        match self {
            Self::Parameters(declarations) => s_pluralize!(declarations.len(), "parameter"),
            Self::Declaration => "declaration",
        }
    }
}
