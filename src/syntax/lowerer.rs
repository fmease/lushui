//! The lowering stage (AST simplification and early validation).
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
//! * simplify use-declarations by unfolding use-path trees
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

use super::{
    ast::{self, Explicit, HangerKind, ParameterGroup, Path},
    lowered_ast::{
        self, decl, expr, pat, AttributeKeys, AttributeKind, AttributeTarget, Attributes, Number,
    },
};
use crate::{
    diagnostics::{Code, Diagnostic, Reporter},
    error::{map_outcome_from_result, Health, Outcome, PossiblyErroneous, Result},
    format::{ordered_listing, pluralize, Conjunction, IOError, QuoteExt},
    span::{SharedSourceMap, SourceMap, Span, Spanning},
    utility::{obtain, SmallVec, Str},
};
use joinery::JoinableIterator;
use smallvec::smallvec;
use std::iter::once;

/// The state of the lowering pass.
pub struct Lowerer<'a> {
    options: LoweringOptions,
    map: SharedSourceMap,
    reporter: &'a Reporter,
}

impl<'a> Lowerer<'a> {
    pub fn new(options: LoweringOptions, map: SharedSourceMap, reporter: &'a Reporter) -> Self {
        Self {
            options,
            map,
            reporter,
        }
    }

    /// Lower a declaration.
    pub fn lower_declaration(
        &mut self,
        declaration: ast::Declaration,
    ) -> Outcome<SmallVec<lowered_ast::Declaration, 1>> {
        self.lower_declaration_with_context(
            declaration,
            &DeclarationContext {
                is_root: true,
                internal_modules: Vec::new(),
            },
        )
    }

    fn lower_declaration_with_context(
        &mut self,
        declaration: ast::Declaration,
        context: &DeclarationContext,
    ) -> Outcome<SmallVec<lowered_ast::Declaration, 1>> {
        use ast::DeclarationKind::*;

        let mut health = Health::Untainted;

        let attributes =
            Outcome::from(self.lower_attributes(&declaration.attributes, &declaration))
                .unwrap(&mut health);

        let declarations = match declaration.value {
            Function(function) => {
                let context = ExpressionContext::new(declaration.span);

                if function.type_annotation.is_none() {
                    Diagnostic::missing_mandatory_type_annotation(
                        function.binder.span().fit_end(&function.parameters).end(),
                        AnnotationTarget::Declaration(&function.binder),
                    )
                    .report(self.reporter);
                    health.taint();
                }

                // @Note awkward API! + check above ^
                let declaration_type_annotation =
                    Outcome::from(function.type_annotation).unwrap(&mut health);

                // @Note type_annotation is currently lowered twice @Task remove duplicate work
                // @Task find a way to use `Option::map` (currently does not work because of
                // partial moves, I hate those), use local bindings
                let body = match function.body {
                    Some(body) => {
                        let mut body = self.lower_expression(body, context).unwrap(&mut health);

                        {
                            let mut type_annotation = once(
                                self.lower_expression(declaration_type_annotation.clone(), context)
                                    .unwrap(&mut health),
                            );

                            for parameter_group in function.parameters.iter().rev() {
                                if parameter_group.type_annotation.is_none() {
                                    Diagnostic::missing_mandatory_type_annotation(
                                        parameter_group,
                                        AnnotationTarget::Parameters(parameter_group),
                                    )
                                    .report(self.reporter);
                                    health.taint();
                                }
                                // @Note awkward API! + check above ^
                                let parameter_type_annotation =
                                    Outcome::from(parameter_group.type_annotation.clone())
                                        .unwrap(&mut health);

                                let parameter_type_annotation = self
                                    .lower_expression(parameter_type_annotation, context)
                                    .unwrap(&mut health);

                                for binder in parameter_group.parameters.iter().rev() {
                                    body = expr! {
                                        Lambda {
                                            Attributes::default(),
                                            Span::default();
                                            parameter: binder.clone(),
                                            parameter_type_annotation: Some(parameter_type_annotation.clone()),
                                            explicitness: parameter_group.explicitness,
                                            laziness: parameter_group.aspect.laziness,
                                            body_type_annotation: type_annotation.next(),
                                            body,
                                        }
                                    }
                                }
                            }
                        }
                        Some(body)
                    }
                    None => None,
                };

                let type_annotation = self
                    .lower_parameters_to_annotated_ones(
                        function.parameters,
                        declaration_type_annotation,
                        context,
                    )
                    .unwrap(&mut health);

                smallvec![decl! {
                    Function {
                        attributes,
                        declaration.span;
                        binder: function.binder,
                        type_annotation,
                        expression: body,
                    }
                }]
            }
            Data(type_) => {
                let data_type_annotation = match type_.type_annotation {
                    Some(type_annotation) => type_annotation,
                    None => {
                        Diagnostic::missing_mandatory_type_annotation(
                            type_.binder.span().fit_end(&type_.parameters).end(),
                            AnnotationTarget::Declaration(&type_.binder),
                        )
                        .report(self.reporter);
                        health.taint();
                        PossiblyErroneous::error()
                    }
                };

                let type_annotation = self
                    .lower_parameters_to_annotated_ones(
                        type_.parameters,
                        data_type_annotation,
                        ExpressionContext::new(declaration.span),
                    )
                    .unwrap(&mut health);

                let context = DeclarationContext {
                    is_root: false,
                    internal_modules: Vec::new(),
                };

                let constructors = type_.constructors.map(|constructors| {
                    constructors
                        .into_iter()
                        .flat_map(|constructor| {
                            self.lower_declaration_with_context(constructor, &context)
                                .unwrap(&mut health)
                        })
                        .collect()
                });

                smallvec![decl! {
                    Data{
                        attributes,
                        declaration.span;
                        binder: type_.binder,
                        type_annotation,
                        constructors,
                    }
                }]
            }
            // @Beacon @Task check multiple record constructors
            Constructor(constructor) => {
                let constructor_type_annotation = match constructor.type_annotation {
                    Some(type_annotation) => type_annotation,
                    None => {
                        // @Note awkward API!
                        Diagnostic::missing_mandatory_type_annotation(
                            constructor
                                .binder
                                .span()
                                .fit_end(&constructor.parameters)
                                .end(),
                            AnnotationTarget::Declaration(&constructor.binder),
                        )
                        .report(self.reporter);
                        health.taint();
                        PossiblyErroneous::error()
                    }
                };

                let type_annotation = self
                    .lower_parameters_to_annotated_ones(
                        constructor.parameters,
                        constructor_type_annotation,
                        ExpressionContext {
                            in_constructor: true,
                            declaration: declaration.span,
                        },
                    )
                    .unwrap(&mut health);

                if let Some(body) = constructor.body {
                    let body = self
                        .lower_expression(body, ExpressionContext::new(declaration.span))
                        .unwrap(&mut health);

                    Diagnostic::error()
                        .code(Code::E020)
                        .message(format!(
                            "`{}` is defined multiple times in this scope",
                            constructor.binder
                        ))
                        .labeled_primary_span(&body, "conflicting definition")
                        .note(
                            "the body of the constructor is implied but it also has a body introduced by `=`",
                        ).report(self.reporter);
                    health.taint();
                }

                smallvec![decl! {
                    Constructor {
                        attributes,
                        declaration.span;
                        binder: constructor.binder,
                        type_annotation,
                    }
                }]
            }
            Module(module) => {
                let is_external_module = module.declarations.is_none();

                let declarations = match module.declarations {
                    Some(declarations) => declarations,
                    None => {
                        let path_suffix = if attributes.has(AttributeKeys::LOCATION) {
                            attributes
                                .get(|kind| obtain!(kind, AttributeKind::Location { path } => path))
                        } else {
                            module.binder.as_str()
                        };

                        let mut path = self.map.borrow()[module.file]
                            .path()
                            .unwrap()
                            .parent()
                            .unwrap()
                            .to_owned();
                        path.extend(&context.internal_modules);
                        let path = path.join(path_suffix).with_extension(crate::FILE_EXTENSION);

                        // @Task instead of a note saying the error, print a help message
                        // saying to create the missing file or change the access rights etc.
                        // @Note awkward API!

                        let file = match self.map.borrow_mut().load(path.clone()) {
                            Ok(file) => file,
                            Err(error) => {
                                Diagnostic::error()
                                    .code(Code::E016)
                                    .message(format!(
                                        "could not load the module `{}`",
                                        module.binder
                                    ))
                                    .primary_span(declaration.span)
                                    .note(IOError(error, &path).to_string())
                                    .report(self.reporter);
                                return PossiblyErroneous::error();
                            }
                        };

                        // @Note awkward error handling API
                        let node = match crate::syntax::parse(
                            file,
                            module.binder.clone(),
                            self.map.clone(),
                            self.reporter,
                        ) {
                            Ok(node) => node,
                            Err(()) => return PossiblyErroneous::error(),
                        };

                        let module: ast::Module = node.value.try_into().unwrap();

                        if !node.attributes.is_empty() {
                            Diagnostic::unimplemented("attributes on module headers")
                                .report(self.reporter);
                            health.taint();
                        }

                        module.declarations.unwrap()
                    }
                };

                let mut internal_modules = if is_external_module {
                    Vec::new()
                } else {
                    context.internal_modules.clone()
                };
                if !context.is_root {
                    internal_modules.push(module.binder.to_string());
                }

                let context = DeclarationContext {
                    is_root: false,
                    internal_modules,
                };

                let declarations = declarations
                    .into_iter()
                    .flat_map(|declaration| {
                        self.lower_declaration_with_context(declaration, &context)
                            .unwrap(&mut health)
                    })
                    .collect();

                smallvec![decl! {
                    Module {
                        attributes,
                        declaration.span;
                        binder: module.binder,
                        file: module.file,
                        declarations,
                    }
                }]
            }
            // @Beacon @Task merge attributes with parent module
            // (through a `Context`) and ensure it's the first declaration in the whole module
            ModuleHeader => {
                // @Note awkward API!
                Diagnostic::unimplemented("module headers")
                    .primary_span(declaration.span)
                    .report(self.reporter);
                health.taint();
                PossiblyErroneous::error()
            }
            // overarching attributes be placed *above* the subdeclaration attributes
            Group(group) => {
                let group_attributes = declaration.attributes;

                group
                    .declarations
                    .into_iter()
                    .map(|mut declaration| {
                        let mut group_attributes = group_attributes.clone();
                        group_attributes.append(&mut declaration.attributes);
                        declaration.attributes = group_attributes;
                        declaration
                    })
                    .flat_map(|declaration| self.lower_declaration(declaration).unwrap(&mut health))
                    .collect()
            }
            // @Task verify that the resulting spans are correct
            Use(use_) => {
                use ast::{UsePathTree, UsePathTreeKind::*};

                let mut declarations = SmallVec::new();

                // @Beacon @Task we should improve this error handling here!!!!
                fn lower_use_path_tree(
                    path: Path,
                    bindings: Vec<UsePathTree>,
                    span: Span,
                    attributes: lowered_ast::Attributes,
                    declarations: &mut SmallVec<lowered_ast::Declaration, 1>,
                    reporter: &Reporter,
                ) -> Result {
                    let mut health = Health::Untainted;

                    // @Note awkward API!
                    pub macro try_($subject:expr) {
                        match $subject {
                            Ok(subject) => subject,
                            Err(error) => {
                                error.report(reporter); // @Temporary (upstream should return () in the future)
                                health.taint();
                                continue;
                            }
                        }
                    }

                    for binding in bindings {
                        match binding.value {
                            Single { target, binder } => {
                                let combined_target = try_!(path.clone().join(target.clone()));

                                // if the binder is not explicitly set, look for the most-specific/last/right-most
                                // identifier of the target but if that one is `self`, look up the last identifier of
                                // the parent path
                                let binder = binder
                                    .or_else(|| {
                                        if target.bare_hanger(HangerKind::Self_).is_some() {
                                            &path
                                        } else {
                                            &target
                                        }
                                        .last_identifier()
                                        .cloned()
                                    })
                                    .ok_or_else(|| {
                                        // @Task improve the message for `use crate.(self)`: hint that `self`
                                        // is effectively unnamed because `crate` is unnamed
                                        Diagnostic::invalid_unnamed_path_hanger(
                                            target.hanger.unwrap(),
                                        )
                                    });
                                let binder = try_!(binder);

                                declarations.push(decl! {
                                    Use {
                                        attributes.clone(),
                                        span;
                                        binder,
                                        target: combined_target,
                                    }
                                });
                            }
                            Multiple {
                                path: inner_path,
                                bindings,
                            } => {
                                lower_use_path_tree(
                                    try_!(path.clone().join(inner_path)),
                                    bindings,
                                    span,
                                    attributes.clone(),
                                    declarations,
                                    reporter,
                                )?;
                            }
                        }
                    }

                    health.into()
                }

                'discriminate: {
                    match use_.bindings.value {
                        Single { target, binder } => {
                            let binder = binder.or_else(|| target.last_identifier().cloned());
                            let Some(binder) = binder else {
                                // @Task improve the message for `use crate.(self)`: hint that `self`
                                // is effectively unnamed because `crate` is unnamed
                                // @Task the message is even worse (it is misleading!) with `use extern.(self)`
                                // currently leads to the suggestion to bind `self` to an identifier but
                                // for `extern` that is illegal, too
                                Diagnostic::invalid_unnamed_path_hanger(target.hanger.unwrap())
                                    .report(self.reporter);
                                health.taint();
                                break 'discriminate;
                            };

                            declarations.push(decl! {
                                Use {
                                    attributes,
                                    declaration.span;
                                    binder,
                                    target,
                                }
                            });
                        }
                        Multiple { path, bindings } => Outcome::from(lower_use_path_tree(
                            path,
                            bindings,
                            declaration.span,
                            attributes,
                            &mut declarations,
                            self.reporter,
                        ))
                        .unwrap(&mut health),
                    }
                }

                declarations
            }
        };

        health.of(declarations)
    }

    /// Lower an expression.
    fn lower_expression(
        &mut self,
        expression: ast::Expression,
        context: ExpressionContext,
    ) -> Outcome<lowered_ast::Expression> {
        use ast::ExpressionKind::*;

        let mut health = Health::Untainted;

        let attributes = Outcome::from(self.lower_attributes(&expression.attributes, &expression))
            .unwrap(&mut health);

        let expression = match expression.value {
            PiTypeLiteral(pi) => {
                health &= self.check_fieldness_location(pi.domain.aspect.fieldness, context);

                let domain = self
                    .lower_expression(pi.domain.expression, context)
                    .unwrap(&mut health);
                let codomain = self
                    .lower_expression(pi.codomain, context)
                    .unwrap(&mut health);

                expr! {
                    PiType {
                        attributes,
                        expression.span;
                        explicitness: pi.domain.explicitness,
                        aspect: pi.domain.aspect,
                        parameter: pi.domain.binder.clone(),
                        domain,
                        codomain,
                    }
                }
            }
            Application(application) => {
                if let Some(binder) = &application.binder {
                    Diagnostic::unimplemented("named arguments")
                        .primary_span(binder)
                        .report(self.reporter);
                    health.taint();
                }

                let callee = self
                    .lower_expression(application.callee, context)
                    .unwrap(&mut health);
                let argument = self
                    .lower_expression(application.argument, context)
                    .unwrap(&mut health);

                expr! {
                    Application {
                        attributes,
                        expression.span;
                        callee,
                        argument,
                        explicitness: application.explicitness,
                    }
                }
            }
            TypeLiteral => expr! { Type { attributes, expression.span } },
            // @Note awkward API!
            NumberLiteral(literal) => {
                let span = expression.span;
                map_outcome_from_result(
                    self.lower_number_literal(*literal, span, &attributes),
                    |literal| {
                        expr! { Number(attributes, span; literal) }
                    },
                )
                .unwrap(&mut health)
            }
            TextLiteral(text) => expr! {
                Text(attributes, expression.span; text)
            },
            TypedHole(_hole) => {
                Diagnostic::unimplemented("typed holes")
                    .primary_span(expression.span)
                    .report(self.reporter);
                // @Note awkward API!
                health.taint();
                PossiblyErroneous::error()
            }
            Path(path) => expr! {
                Binding {
                    attributes,
                    expression.span;
                    binder: *path,
                }
            },
            Field(_field) => {
                Diagnostic::unimplemented("record fields")
                    .primary_span(expression.span)
                    .report(self.reporter);
                // @Note awkward API
                health.taint();
                PossiblyErroneous::error()
            }
            LambdaLiteral(lambda) => {
                let mut expression = self
                    .lower_expression(lambda.body, context)
                    .unwrap(&mut health);

                let mut type_annotation = lambda
                    .body_type_annotation
                    .map(|type_annotation| {
                        self.lower_expression(type_annotation, context)
                            .unwrap(&mut health)
                    })
                    .into_iter();

                for parameter_group in lambda.parameters.iter().rev() {
                    let parameter =
                        parameter_group
                            .type_annotation
                            .clone()
                            .map(|type_annotation| {
                                self.lower_expression(type_annotation, context)
                                    .unwrap(&mut health)
                            });

                    for binder in parameter_group.parameters.iter().rev() {
                        expression = expr! {
                            Lambda {
                                Attributes::default(),
                                Span::default();
                                parameter: binder.clone(),
                                parameter_type_annotation: parameter.clone(),
                                explicitness: parameter_group.explicitness,
                                laziness: parameter_group.aspect.laziness,
                                body_type_annotation: type_annotation.next(),
                                body: expression,
                            }
                        };
                    }
                }

                expression
            }
            LetIn(let_in) => {
                let expression = {
                    let binder = &let_in.binder;

                    match let_in.expression {
                        Some(expression) => expression,
                        None => {
                            // @Note awkward API!
                            Diagnostic::error()
                                .code(Code::E012)
                                .message(format!("let-binding `{}` has no definition", binder))
                                .primary_span(
                                    let_in
                                        .binder
                                        .span()
                                        .fit_end(&let_in.parameters)
                                        .fit_end(&let_in.type_annotation)
                                        .end(),
                                )
                                .help("provide a definition with `=`")
                                .report(self.reporter);
                            health.taint();
                            PossiblyErroneous::error()
                        }
                    }
                };

                let mut expression = self
                    .lower_expression(expression, context)
                    .unwrap(&mut health);

                let mut type_annotation = let_in
                    .type_annotation
                    .map(|type_annotation| {
                        self.lower_expression(type_annotation, context)
                            .unwrap(&mut health)
                    })
                    .into_iter();

                for parameter_group in let_in.parameters.iter().rev() {
                    let parameter = parameter_group.type_annotation.clone().map(|expression| {
                        self.lower_expression(expression, context)
                            .unwrap(&mut health)
                    });

                    for binder in parameter_group.parameters.iter().rev() {
                        expression = expr! {
                            Lambda {
                                Attributes::default(), Span::default();
                                parameter: binder.clone(),
                                parameter_type_annotation: parameter.clone(),
                                explicitness: parameter_group.explicitness,
                                laziness: parameter_group.aspect.laziness,
                                body_type_annotation: type_annotation.next(),
                                body: expression,
                            }
                        };
                    }
                }

                let body = self
                    .lower_expression(let_in.scope, context)
                    .unwrap(&mut health);

                expr! {
                    Application {
                        Attributes::default(), Span::default();
                        callee: expr! {
                            Lambda {
                                Attributes::default(), Span::default();
                                parameter: let_in.binder,
                                // @Note we cannot simply lower parameters and a type annotation because
                                // in the chain (`->`) of parameters, there might always be one missing and
                                // we don't support partial type annotations yet (using `_`)
                                // @Temporary @Update @Bug -gy because we ignore above message
                                // @Task verify correct semantics
                                parameter_type_annotation: type_annotation.next(),
                                explicitness: Explicit,
                                laziness: None,
                                body_type_annotation: None,
                                body,
                            }
                        },
                        argument: expression,
                        explicitness: Explicit,
                    }
                }
            }
            UseIn(_use_in) => {
                // @Note awkward API
                Diagnostic::unimplemented("use/in expressions")
                    .primary_span(expression.span)
                    .report(self.reporter);
                health.taint();
                PossiblyErroneous::error()
            }
            CaseAnalysis(analysis) => {
                let mut cases = Vec::new();

                for case_group in analysis.cases {
                    cases.push(lowered_ast::Case {
                        pattern: self.lower_pattern(case_group.pattern).unwrap(&mut health),
                        body: self
                            .lower_expression(case_group.body.clone(), context)
                            .unwrap(&mut health),
                    });
                }

                let subject = self
                    .lower_expression(analysis.scrutinee, context)
                    .unwrap(&mut health);

                expr! {
                    CaseAnalysis {
                        attributes,
                        expression.span;
                        subject,
                        cases,
                    }
                }
            }
            DoBlock(_block) => {
                // @Note awkward API!
                Diagnostic::unimplemented("do blocks")
                    .primary_span(expression.span)
                    .report(self.reporter);
                health.taint();
                PossiblyErroneous::error()
            }
            SequenceLiteral(_sequence) => {
                // @Note awkward API!
                Diagnostic::unimplemented("sequence literals")
                    .primary_span(expression.span)
                    .report(self.reporter);
                health.taint();
                PossiblyErroneous::error()
            }
            Error => PossiblyErroneous::error(),
        };

        health.of(expression)
    }

    /// Lower a pattern.
    fn lower_pattern(&mut self, pattern: ast::Pattern) -> Outcome<lowered_ast::Pattern> {
        use ast::PatternKind::*;

        let mut health = Health::Untainted;

        let attributes =
            Outcome::from(self.lower_attributes(&pattern.attributes, &pattern)).unwrap(&mut health);

        let pattern = match pattern.value {
            // @Note awkward API!
            NumberLiteral(literal) => {
                let span = pattern.span;
                map_outcome_from_result(
                    self.lower_number_literal(*literal, span, &attributes),
                    |literal| {
                        pat! {
                            Number(attributes, span; literal)
                        }
                    },
                )
            }
            .unwrap(&mut health),
            TextLiteral(literal) => pat! {
                Text(attributes, pattern.span; literal)
            },
            Path(path) => pat! {
                Binding {
                    attributes,
                    pattern.span;
                    binder: *path,
                }
            },
            Binder(binding) => pat! {
                Binder {
                    attributes,
                    pattern.span;
                    binder: binding.binder,
                }
            },
            Deapplication(application) => {
                if let Some(binder) = &application.binder {
                    Diagnostic::unimplemented("named arguments")
                        .primary_span(binder)
                        .report(self.reporter);
                    health.taint();
                }

                let callee = self.lower_pattern(application.callee).unwrap(&mut health);
                let argument = self.lower_pattern(application.argument).unwrap(&mut health);

                pat! {
                    Deapplication {
                        attributes,
                        pattern.span;
                        callee,
                        argument,
                    }
                }
            }
            SequenceLiteralPattern(_sequence) => {
                Diagnostic::unimplemented("sequence literal patterns")
                    .primary_span(pattern.span)
                    .report(self.reporter);
                // @Note awkward API!
                health.taint();
                PossiblyErroneous::error()
            }
        };

        health.of(pattern)
    }

    /// Lower attributes.
    // @Task filter out documentation attributes if !options.keep_documentation_comments
    fn lower_attributes(
        &mut self,
        attributes: &[ast::Attribute],
        target: &impl AttributeTarget,
    ) -> Result<Attributes> {
        use lowered_ast::Attribute;

        let actual_targets = target.as_attribute_targets();
        let mut health = Health::Untainted;
        let mut lowered_attributes = Vec::new();

        for attribute in attributes {
            let attribute =
                match Attribute::parse(attribute, &self.options, &self.map.borrow(), self.reporter)
                {
                    Ok(attribute) => attribute,
                    Err(()) => {
                        health.taint();
                        continue;
                    }
                };

            // non-conforming attributes
            let expected_targets = attribute.value.targets();

            if !expected_targets.contains(actual_targets) {
                Diagnostic::error()
                    .code(Code::E013)
                    .message(format!(
                        "attribute {} is ascribed to {}",
                        attribute.value.quoted_name(),
                        target.name()
                    ))
                    .labeled_primary_span(&attribute, "misplaced attribute")
                    .labeled_secondary_span(target, "incompatible item")
                    .note(format!(
                        "attribute {} can only be ascribed to {}",
                        attribute.value.quoted_name(),
                        expected_targets.description(),
                    ))
                    .report(self.reporter);
                health.taint();
                continue;
            }

            lowered_attributes.push(attribute);
        }

        let mut keys = AttributeKeys::empty();
        let mut attributes = Vec::new();

        // search for conflicting or duplicate attributes
        for attribute in &lowered_attributes {
            let key = attribute.value.key();
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
                let faulty_attributes = matching_attributes;

                Diagnostic::error()
                    .code(Code::E006)
                    .message(format!(
                        "multiple {} attributes",
                        faulty_attributes.first().unwrap().value.quoted_name(),
                    ))
                    .labeled_primary_spans(
                        faulty_attributes.into_iter(),
                        "duplicate or conflicting attribute",
                    )
                    .report(self.reporter);
                health.taint();
            }
        }

        let attributes = Attributes {
            keys,
            data: attributes.into_boxed_slice(),
        };

        health &= target.check_attributes(&attributes, self.reporter);

        if attributes.keys.is_empty() {
            return health.of(attributes).into();
        }

        let check_mutual_exclusivity = |mutually_exclusive_attributes: AttributeKeys,
                                        reporter: &Reporter|
         -> Result {
            if (attributes.keys & mutually_exclusive_attributes)
                .bits()
                .count_ones()
                > 1
            {
                let faulty_attributes = attributes
                    .filter(mutually_exclusive_attributes)
                    .collect::<Vec<_>>();
                let listing = ordered_listing(
                    faulty_attributes
                        .iter()
                        .map(|attribute| attribute.value.quoted_name()),
                    Conjunction::And,
                );
                Diagnostic::error()
                    .code(Code::E014)
                    .message(format!("attributes {} are mutually exclusive", listing))
                    .labeled_primary_spans(faulty_attributes.into_iter(), "conflicting attribute")
                    .report(reporter);
                return Err(());
            }

            Ok(())
        };

        health &= check_mutual_exclusivity(
            AttributeKeys::INTRINSIC | AttributeKeys::KNOWN,
            self.reporter,
        );

        health &=
            check_mutual_exclusivity(AttributeKeys::MOVING | AttributeKeys::OPAQUE, self.reporter);

        health &= check_mutual_exclusivity(
            AttributeKeys::INT
                | AttributeKeys::INT32
                | AttributeKeys::INT64
                | AttributeKeys::NAT
                | AttributeKeys::NAT32
                | AttributeKeys::NAT64,
            self.reporter,
        );

        health &=
            check_mutual_exclusivity(AttributeKeys::RUNE | AttributeKeys::TEXT, self.reporter);

        health &=
            check_mutual_exclusivity(AttributeKeys::LIST | AttributeKeys::VECTOR, self.reporter);

        if attributes.keys.intersects(AttributeKeys::UNSUPPORTED) {
            for attribute in attributes.filter(AttributeKeys::UNSUPPORTED) {
                Diagnostic::unimplemented(format!("attribute {}", attribute.value.quoted_name()))
                    .primary_span(attribute)
                    .report(self.reporter);
            }
            health.taint();
        }

        // @Task replace this concept with a feature system
        if attributes.keys.intersects(AttributeKeys::INTERNAL)
            && !self.options.internal_features_enabled
        {
            for attribute in attributes.filter(AttributeKeys::INTERNAL) {
                Diagnostic::error()
                    .code(Code::E038)
                    .message(format!(
                        "attribute {} is an internal feature",
                        attribute.value.quoted_name()
                    ))
                    // @Task improve the phrasing (this action should be discouraged)
                    // @Question should this be shown at all?
                    .help(
                        "add the command-line option `-Z internals` to use the attribute anyway\n\
                         opting out of any stability guarantees in the process!",
                    )
                    .primary_span(attribute)
                    .report(self.reporter);
            }
            health.taint();
        }

        health.of(attributes).into()
    }

    fn lower_number_literal(
        &mut self,
        number: String,
        span: Span,
        attributes: &Attributes,
    ) -> Result<Number> {
        (if attributes.has(AttributeKeys::NAT32) {
            number
                .parse()
                .map_err(|_| ("Nat32", NAT32_INTERVAL_REPRESENTATION))
                .map(Number::Nat32)
        } else if attributes.has(AttributeKeys::NAT64) {
            number
                .parse()
                .map_err(|_| ("Nat64", NAT64_INTERVAL_REPRESENTATION))
                .map(Number::Nat64)
        } else if attributes.has(AttributeKeys::INT) {
            Ok(Number::Int(number.parse().unwrap()))
        } else if attributes.has(AttributeKeys::INT32) {
            number
                .parse()
                .map_err(|_| ("Int32", INT32_INTERVAL_REPRESENTATION))
                .map(Number::Int32)
        } else if attributes.has(AttributeKeys::INT64) {
            number
                .parse()
                .map_err(|_| ("Int64", INT64_INTERVAL_REPRESENTATION))
                .map(Number::Int64)
        } else {
            // optionally attributes.has(AttributeKeys::NAT)
            number
                .parse()
                .map_err(|_| ("Nat", NAT_INTERVAL_REPRESENTATION))
                .map(Number::Nat)
        })
        .map_err(|(type_name, interval)| {
            Diagnostic::error()
                .code(Code::E007)
                .message(format!(
                    "number literal `{}` does not fit type `{}`",
                    number, type_name
                ))
                .primary_span(span)
                .note(format!(
                    "values of this type must fit integer interval {}",
                    interval
                ))
                .report(self.reporter);
        })
    }

    /// Lower annotated parameters.
    fn lower_parameters_to_annotated_ones(
        &mut self,
        parameters: ast::Parameters,
        type_annotation: ast::Expression,
        context: ExpressionContext,
    ) -> Outcome<lowered_ast::Expression> {
        let mut health = Health::Untainted;

        let mut expression = self
            .lower_expression(type_annotation, context)
            .unwrap(&mut health);

        for parameter_group in parameters.into_iter().rev() {
            if parameter_group.type_annotation.is_none() {
                Diagnostic::missing_mandatory_type_annotation(
                    &parameter_group,
                    AnnotationTarget::Parameters(&parameter_group),
                )
                .report(self.reporter);
                health.taint();
            }
            // @Note awkward API! + the code above ^
            let parameter_type_annotation =
                Outcome::from(parameter_group.type_annotation).unwrap(&mut health);

            let parameter_type_annotation = self
                .lower_expression(parameter_type_annotation, context)
                .unwrap(&mut health);

            health &= self.check_fieldness_location(parameter_group.aspect.fieldness, context);

            for binder in parameter_group.parameters.iter().rev() {
                expression = expr! {
                    PiType {
                        Attributes::default(),
                        Span::default();
                        explicitness: parameter_group.explicitness,
                        aspect: parameter_group.aspect,
                        parameter: Some(binder.clone()),
                        domain: parameter_type_annotation.clone(),
                        codomain: expression,
                    }
                };
            }
        }

        health.of(expression)
    }

    fn check_fieldness_location(
        &mut self,
        fieldness: Option<Span>,
        context: ExpressionContext,
    ) -> Result {
        if let Some(field) = fieldness {
            if !context.in_constructor {
                // @Note it would be helpful to also say the name of the actual declaration
                // but I think we lack a method for this right now
                Diagnostic::error()
                    .code(Code::E017)
                    .message("field marker `::` used outside of a constructor declaration")
                    .primary_span(field)
                    .labeled_secondary_span(context.declaration, "not a constructor")
                    .report(self.reporter);
                return Err(());
            }
        }

        Ok(())
    }
}

#[derive(Default)]
pub struct LoweringOptions {
    /// Specifies if internal language and library features are enabled.
    pub internal_features_enabled: bool,
    /// Specifies if documentation comments should be kept in the lowered AST.
    pub keep_documentation_comments: bool,
}

struct DeclarationContext {
    is_root: bool,
    /// The path of internal module declarations leading to a declaration.
    ///
    /// The path starts either at the root module declaration (excluding it) or
    /// at the closest external module (including it).
    ///
    /// Exclusively used to compute the path of external modules inside of internal
    /// modules!
    internal_modules: Vec<String>,
}

#[derive(Clone, Copy)]
struct ExpressionContext {
    in_constructor: bool,
    declaration: Span,
}

impl ExpressionContext {
    fn new(declaration: Span) -> Self {
        Self {
            in_constructor: false,
            declaration,
        }
    }
}

const NAT32_INTERVAL_REPRESENTATION: &str = "[0, 2^32-1]";
const NAT64_INTERVAL_REPRESENTATION: &str = "[0, 2^64-1]";
const NAT_INTERVAL_REPRESENTATION: &str = "[0, infinity)";
const INT32_INTERVAL_REPRESENTATION: &str = "[-2^31, 2^31-1]";
const INT64_INTERVAL_REPRESENTATION: &str = "[-2^63, 2^63-1]";

// @Beacon @Beacon @Task don't use reporter+Result<_> here but
// a custom error type (w/o ::Unrecoverable)
// and turn that stuff into stuff later

impl lowered_ast::AttributeKind {
    // @Task allow unordered named attributes e.g. `@(unstable (reason "x") (feature thing))`
    pub fn parse(
        attribute: &ast::Attribute,
        options: &LoweringOptions,
        map: &SourceMap,
        reporter: &Reporter,
    ) -> Result<Self> {
        let arguments = &mut &*attribute.arguments;

        fn optional_argument<'a>(
            arguments: &mut &'a [ast::AttributeArgument],
        ) -> Option<&'a ast::AttributeArgument> {
            arguments.first().map(|argument| {
                *arguments = &arguments[1..];
                argument
            })
        }

        // @Task improve API
        fn argument<'a>(
            arguments: &mut &'a [ast::AttributeArgument],
            span: Span,
            reporter: &Reporter,
        ) -> Result<&'a ast::AttributeArgument, AttributeParsingError> {
            let argument = arguments.first().ok_or_else(|| {
                // @Task add more information about the arity and the argument types
                Diagnostic::error()
                    .code(Code::E019)
                    .message("too few attribute arguments provided")
                    .primary_span(span)
                    .report(reporter);
                AttributeParsingError::Unrecoverable
            })?;
            *arguments = &arguments[1..];
            Ok(argument)
        }

        let result = (|| {
            Ok(match attribute.binder.as_str() {
                "allow" => Self::Allow {
                    lint: lowered_ast::Lint::parse(
                        argument(arguments, attribute.span, reporter)?
                            .path(Some("lint"), reporter)?,
                        reporter,
                    )?,
                },
                "deny" => Self::Deny {
                    lint: lowered_ast::Lint::parse(
                        argument(arguments, attribute.span, reporter)?
                            .path(Some("lint"), reporter)?,
                        reporter,
                    )?,
                },
                "deprecated" => {
                    Diagnostic::unimplemented("attribute `deprecated`")
                        .primary_span(attribute)
                        .report(reporter);
                    return Err(AttributeParsingError::Unrecoverable);
                }
                "documentation" => Self::Documentation {
                    content: if options.keep_documentation_comments {
                        argument(arguments, attribute.span, reporter)?
                            .text_literal_or_encoded_text(map, reporter)?
                    } else {
                        *arguments = &arguments[1..];
                        String::new()
                    },
                },
                "forbid" => Self::Forbid {
                    lint: lowered_ast::Lint::parse(
                        argument(arguments, attribute.span, reporter)?
                            .path(Some("lint"), reporter)?,
                        reporter,
                    )?,
                },
                "intrinsic" => Self::Intrinsic,
                "if" => {
                    Diagnostic::unimplemented("attribute `if`")
                        .primary_span(attribute)
                        .report(reporter);
                    return Err(AttributeParsingError::Unrecoverable);
                }
                "ignore" => Self::Ignore,
                "include" => Self::Include,
                "known" => Self::Known,
                "Int" => Self::Int,
                "Int32" => Self::Int32,
                "Int64" => Self::Int64,
                "List" => Self::List,
                "location" => {
                    let path = argument(arguments, attribute.span, reporter)?
                        .text_literal(Some("path"), reporter)?;

                    Self::Location { path }
                }
                "moving" => Self::Moving,
                "Nat" => Self::Nat,
                "Nat32" => Self::Nat32,
                "Nat64" => Self::Nat64,
                "opaque" => Self::Opaque,
                "public" => {
                    let reach = optional_argument(arguments)
                        .map(|argument| argument.path(Some("reach"), reporter))
                        .transpose()?;

                    Self::Public { reach }
                }
                "recursion-limit" => {
                    let depth = argument(arguments, attribute.span, reporter)?;
                    let depth = depth
                        .number_literal(Some("depth"), reporter)?
                        .parse::<u32>()
                        .map_err(|_| {
                            Diagnostic::error()
                                .code(Code::E008)
                                .message(format!(
                                    "attribute argument does not fit integer interval {}",
                                    NAT32_INTERVAL_REPRESENTATION
                                ))
                                .primary_span(depth)
                                .report(reporter);
                            AttributeParsingError::Unrecoverable
                        })?;

                    Self::RecursionLimit { depth }
                }
                "Rune" => Self::Rune,
                "static" => Self::Static,
                "test" => Self::Test,
                "Text" => Self::Text,
                "unsafe" => Self::Unsafe,
                "unstable" => {
                    Diagnostic::unimplemented("attribute `unstable`")
                        .primary_span(attribute)
                        .report(reporter);

                    return Err(AttributeParsingError::Unrecoverable);
                }
                "Vector" => Self::Vector,
                "warn" => Self::Warn {
                    lint: lowered_ast::Lint::parse(
                        argument(arguments, attribute.span, reporter)?
                            .path(Some("lint"), reporter)?,
                        reporter,
                    )?,
                },
                _ => {
                    return Err(AttributeParsingError::UndefinedAttribute(
                        attribute.binder.clone(),
                    ))
                }
            })
        })();

        let mut health = Health::Untainted;

        if !matches!(result, Err(AttributeParsingError::UndefinedAttribute(_))) {
            // if there are still unparsed arguments it means too many arguments were provided
            // unless the attribute does not exist in the first place since in such case,
            // no argument was parsed either
            if let Some(argument) = arguments.first() {
                Diagnostic::error()
                    .code(Code::E019)
                    .message("too many attribute arguments provided")
                    .primary_span(argument.span.merge(arguments.last()))
                    .report(reporter);
                health.taint();
            }
        }

        result
            .map_err(|error| {
                if let AttributeParsingError::UndefinedAttribute(binder) = error {
                    Diagnostic::error()
                        .code(Code::E011)
                        .message(format!("attribute `{}` does not exist", binder))
                        .primary_span(&binder)
                        .report(reporter);
                }
            })
            .and_then(|attributes| health.of(attributes).into())
    }
}

enum AttributeParsingError {
    Unrecoverable,
    UndefinedAttribute(ast::Identifier),
}

// @Beacon @Note the following attribute argument parsing logic is soo hideous!

impl ast::AttributeArgument {
    extractor!(number_literal "number literal": NumberLiteral => String);
    extractor!(text_literal "text literal": TextLiteral => String);
    extractor!(path "path": Path => Path);

    // @Note argh! the macro cannot handle this
    fn text_literal_or_encoded_text(
        &self,
        map: &SourceMap,
        reporter: &Reporter,
    ) -> Result<String, AttributeParsingError> {
        match &self.value {
            ast::AttributeArgumentKind::TextLiteral(literal) => Ok(literal.as_ref().clone()),
            // @Note horrendous!
            // @Beacon @Note we assume this is meant for documentation comments
            // (the only user of TextEncodedInSpan) to cut off the leading `;;`
            // Honestly, we should replace this stupid system
            ast::AttributeArgumentKind::TextEncodedInSpan => {
                Ok(map.snippet(self.span.trim_start(2)).to_owned())
            }
            ast::AttributeArgumentKind::Named(_) => {
                // @Beacon @Beacon @Task span
                Diagnostic::error()
                    .message("unexpected named attribute argument")
                    .report(reporter);
                Err(AttributeParsingError::Unrecoverable)
            }
            kind => {
                Diagnostic::invalid_attribute_argument_type(
                    (self.span, kind.name()),
                    "positional or named text literal",
                )
                .report(reporter);
                Err(AttributeParsingError::Unrecoverable)
            }
        }
    }
}

// @Note not that extensible and well worked out API
macro extractor($name:ident $repr:literal: $variant:ident => $ty:ty) {
    fn $name(
        &self,
        name: Option<&'static str>,
        reporter: &Reporter,
    ) -> Result<$ty, AttributeParsingError> {
        match &self.value {
            ast::AttributeArgumentKind::$variant(literal) => Ok(literal.as_ref().clone()),
            ast::AttributeArgumentKind::Named(named) => named.handle(
                name,
                |argument| match &argument.value {
                    ast::AttributeArgumentKind::$variant(literal) => Ok(literal.as_ref().clone()),
                    kind => {
                        Diagnostic::invalid_attribute_argument_type(
                            (argument.span, kind.name()),
                            $repr,
                        )
                        .report(reporter);
                        Err(AttributeParsingError::Unrecoverable)
                    }
                },
                reporter,
            ),
            kind => {
                Diagnostic::invalid_attribute_argument_type(
                    (self.span, kind.name()),
                    concat!("positional or named ", $repr),
                )
                .report(reporter);
                Err(AttributeParsingError::Unrecoverable)
            }
        }
    }
}

impl ast::NamedAttributeArgument {
    fn handle<T>(
        &self,
        name: Option<&'static str>,
        handle: impl FnOnce(&ast::AttributeArgument) -> Result<T, AttributeParsingError>,
        reporter: &Reporter,
    ) -> Result<T, AttributeParsingError> {
        match name {
            Some(name) => {
                if self.binder.as_str() == name {
                    handle(&self.value)
                } else {
                    Diagnostic::unexpected_named_attribute_argument(&self.binder, name)
                        .report(reporter);
                    Err(AttributeParsingError::Unrecoverable)
                }
            }
            None => {
                // @Beacon @Beacon @Task span
                Diagnostic::error()
                    .message("unexpected named attribute argument")
                    .report(reporter);
                Err(AttributeParsingError::Unrecoverable)
            }
        }
    }
}

impl lowered_ast::Lint {
    fn parse(binder: Path, reporter: &Reporter) -> Result<Self, AttributeParsingError> {
        Diagnostic::error()
            .code(Code::E018)
            .message(format!("lint `{}` does not exist", binder))
            .primary_span(binder.span())
            .report(reporter);
        Err(AttributeParsingError::Unrecoverable)
    }
}

impl Diagnostic {
    fn invalid_unnamed_path_hanger(hanger: ast::Hanger) -> Self {
        Self::error()
            .code(Code::E025)
            .message(format!("path `{hanger}` is not bound to an identifier"))
            .primary_span(&hanger)
            .note("a use-declaration has to introduce at least one new binder")
            .help("bind the path to a name with `as`")
    }

    // @Temporary signature
    fn unexpected_named_attribute_argument(
        actual: &ast::Identifier,
        expected: &'static str,
    ) -> Self {
        Self::error()
            .code(Code::E028)
            .message(format!(
                "found named argument `{}`, but expected `{}`",
                actual, expected
            ))
            .primary_span(actual)
    }

    // @Temporary signature
    fn invalid_attribute_argument_type(
        actual: (Span, &'static str),
        expected: &'static str,
    ) -> Self {
        Self::error()
            .code(Code::E027)
            .message(format!("found {}, but expected {}", actual.1, expected))
            .primary_span(actual.0)
    }

    fn missing_mandatory_type_annotation(
        spanning: impl Spanning,
        target: AnnotationTarget<'_>,
    ) -> Self {
        use AnnotationTarget::*;

        let type_annotation_suggestion: Str = match target {
            Parameters(parameter_group) => format!(
                "`{}({}: ?type)`",
                parameter_group.explicitness,
                parameter_group.parameters.iter().join_with(' ')
            )
            .into(),
            Declaration(_) => "`: ?type`".into(),
        };

        let binders = match target {
            Parameters(parameter_group) => ordered_listing(
                parameter_group.parameters.iter().map(QuoteExt::quote),
                Conjunction::And,
            ),
            Declaration(binder) => binder.quote(),
        };

        Self::error()
            .code(Code::E015)
            .message(format!(
                "a mandatory type annotation is missing on the {} {}",
                target.name(),
                binders,
            ))
            .primary_span(spanning)
            .help(format!(
                "provide a type annotation for the {} with {}",
                target.name(),
                type_annotation_suggestion,
            ))
    }
}

/// A place in the AST which can have a syntactically optional type annotation.
///
/// Exclusively used for error reporting.
enum AnnotationTarget<'a> {
    Parameters(&'a ParameterGroup),
    Declaration(&'a ast::Identifier),
}

impl AnnotationTarget<'_> {
    fn name(&self) -> &'static str {
        match self {
            Self::Parameters(parameter_group) => {
                pluralize!(parameter_group.parameters.len(), "parameter")
            }
            Self::Declaration(_) => "declaration",
        }
    }
}
