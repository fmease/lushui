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
//! * open out-of-line modules (this will probably move to the parser in the future
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
    ast::{self, Explicit, HangerKind, Parameter, Path},
    lowered_ast::{
        self, attributes::Target, decl, expr, pat, AttributeKind, AttributeName, Attributes, Number,
    },
};
use crate::{
    diagnostics::{Code, Diagnostic, Reporter},
    error::{map_outcome_from_result, Health, Outcome, PossiblyErroneous, Result},
    format::{ordered_listing, Conjunction, IOError, QuoteExt},
    span::{SharedSourceMap, SourceMap, Span, Spanning},
    syntax::lowered_ast::attributes::{Predicate, Public, Query},
    utility::{Atom, SmallVec, Str},
};
use smallvec::smallvec;
use std::{default::default, iter::once};

/// Lower a declaration.
pub fn lower(
    declaration: ast::Declaration,
    options: Options,
    map: SharedSourceMap,
    reporter: &Reporter,
) -> Outcome<SmallVec<lowered_ast::Declaration, 1>> {
    Lowerer::new(options, map, reporter).lower_declaration(declaration)
}

/// The state of the lowering pass.
struct Lowerer<'a> {
    options: Options,
    map: SharedSourceMap,
    reporter: &'a Reporter,
}

impl<'a> Lowerer<'a> {
    fn new(options: Options, map: SharedSourceMap, reporter: &'a Reporter) -> Self {
        Self {
            options,
            map,
            reporter,
        }
    }

    fn lower_declaration(
        &mut self,
        declaration: ast::Declaration,
    ) -> Outcome<SmallVec<lowered_ast::Declaration, 1>> {
        self.lower_declaration_with_context(
            declaration,
            &DeclarationContext {
                is_root: true,
                inline_modules: Vec::new(),
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

        let mut attributes = self
            .lower_attributes(&declaration.attributes, &declaration)
            .unwrap(&mut health);

        let declarations = match declaration.value {
            Function(function) => {
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
                        let mut body = self.lower_expression(body).unwrap(&mut health);

                        {
                            let mut type_annotation = once(
                                self.lower_expression(declaration_type_annotation.clone())
                                    .unwrap(&mut health),
                            );

                            for parameter in function.parameters.iter().rev() {
                                if parameter.value.type_annotation.is_none() {
                                    Diagnostic::missing_mandatory_type_annotation(
                                        parameter,
                                        AnnotationTarget::Parameter(parameter),
                                    )
                                    .report(self.reporter);
                                    health.taint();
                                }
                                // @Note awkward API! + check above ^
                                let parameter_type_annotation =
                                    Outcome::from(parameter.value.type_annotation.clone())
                                        .unwrap(&mut health);

                                let parameter_type_annotation = self
                                    .lower_expression(parameter_type_annotation)
                                    .unwrap(&mut health);

                                body = expr! {
                                    Lambda {
                                        Attributes::default(),
                                        Span::default();
                                        parameter: parameter.value.binder.clone(),
                                        parameter_type_annotation: Some(parameter_type_annotation.clone()),
                                        explicitness: parameter.value.explicitness,
                                        laziness: parameter.value.laziness,
                                        body_type_annotation: type_annotation.next(),
                                        body,
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
                    .lower_parameters_to_annotated_ones(type_.parameters, data_type_annotation)
                    .unwrap(&mut health);

                let context = DeclarationContext {
                    is_root: false,
                    inline_modules: Vec::new(),
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
                    )
                    .unwrap(&mut health);

                if let Some(body) = constructor.body {
                    let body = self.lower_expression(body).unwrap(&mut health);

                    // @Task improve the labels etc, don't say "conflicting definition"
                    // it's technically true, but we can do so much better!
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
                let is_inline_module = module.declarations.is_some();

                let declarations = match module.declarations {
                    Some(declarations) => declarations,
                    None => {
                        let path_suffix = attributes
                            .get::<{ AttributeName::Location }>()
                            .unwrap_or_else(|| module.binder.as_str());

                        let mut path = self.map.borrow()[module.file]
                            .path()
                            .unwrap()
                            .parent()
                            .unwrap()
                            .to_owned();
                        path.extend(&context.inline_modules);
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

                        let Ok(declaration) = crate::syntax::parse_file(
                            file,
                            module.binder.clone(),
                            self.map.clone(),
                            self.reporter,
                        ) else {
                            return PossiblyErroneous::error();
                        };

                        // at this point in time, they are still on the module header if at all
                        assert!(declaration.attributes.is_empty());

                        let module: ast::Module = declaration.value.try_into().unwrap();
                        module.declarations.unwrap()
                    }
                };

                let mut inline_modules = if is_inline_module {
                    context.inline_modules.clone()
                } else {
                    Vec::new()
                };
                if !context.is_root {
                    // @Bug does not respect @location
                    inline_modules.push(module.binder.to_string());
                }

                let context = DeclarationContext {
                    is_root: false,
                    inline_modules,
                };

                let mut lowered_declarations = Vec::new();
                let mut possesses_header = false;

                for (index, declaration) in declarations.into_iter().enumerate() {
                    if matches!(declaration.value, ModuleHeader) {
                        if index == 0 {
                            // @Bug this sequence may lead to some unnecessary diagnostics being emitted
                            // since the "synergy check" (which filters duplicate attribute) is run too late

                            let module_header_attributes = self
                                .lower_attributes(&declaration.attributes, &declaration)
                                .unwrap(&mut health);
                            attributes.0.extend(module_header_attributes.0);
                            attributes =
                                self.check_attribute_synergy(attributes).unwrap(&mut health);
                        } else {
                            Diagnostic::error()
                                .code(Code::E041)
                                .message(
                                    "the module header has to be the first declaration of the module",
                                )
                                .primary_span(&declaration)
                                .if_(possesses_header, |this| {
                                    // @Task make this a note with a span/highlight!
                                    this.note("however, the current module already has a module header")
                                })
                                .report(self.reporter);
                            health.taint();
                        }

                        possesses_header = true;
                        continue;
                    }

                    lowered_declarations.extend(
                        self.lower_declaration_with_context(declaration, &context)
                            .unwrap(&mut health),
                    );
                }

                smallvec![decl! {
                    Module {
                        attributes,
                        declaration.span;
                        binder: module.binder,
                        file: module.file,
                        declarations: lowered_declarations,
                    }
                }]
            }
            // handled in the module case
            ModuleHeader => unreachable!(),
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
                    attributes: lowered_ast::attributes::Attributes,
                    declarations: &mut SmallVec<lowered_ast::Declaration, 1>,
                    reporter: &Reporter,
                ) -> Result {
                    let mut health = Health::Untainted;

                    // @Note awkward API!
                    pub(crate) macro try_($subject:expr) {
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
                                        // @Task improve the message for `use capsule.(self)`: hint that `self`
                                        // is effectively unnamed because `capsule` is unnamed
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
                                // @Task improve the message for `use capsule.(self)`: hint that `self`
                                // is effectively unnamed because `capsule` is unnamed
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
    ) -> Outcome<lowered_ast::Expression> {
        use ast::ExpressionKind::*;

        let mut health = Health::Untainted;

        let attributes = self
            .lower_attributes(&expression.attributes, &expression)
            .unwrap(&mut health);

        let expression = match expression.value {
            PiTypeLiteral(pi) => {
                let domain = self
                    .lower_expression(pi.domain.expression)
                    .unwrap(&mut health);
                let codomain = self.lower_expression(pi.codomain).unwrap(&mut health);

                expr! {
                    PiType {
                        attributes,
                        expression.span;
                        explicitness: pi.domain.explicitness,
                        laziness: pi.domain.laziness,
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
                    .lower_expression(application.callee)
                    .unwrap(&mut health);
                let argument = self
                    .lower_expression(application.argument)
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
                todo!()
                // let span = expression.span;
                // map_outcome_from_result(
                //     self.lower_number_literal(*literal, span, &attributes),
                //     |literal| {
                //         expr! { NumberLiteral(attributes, span; literal) }
                //     },
                // )
                // .unwrap(&mut health)
            }
            TextLiteral(text) => expr! {
                TextLiteral(attributes, expression.span; text)
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
                let mut expression = self.lower_expression(lambda.body).unwrap(&mut health);

                let mut type_annotation = lambda
                    .body_type_annotation
                    .map(|type_annotation| {
                        self.lower_expression(type_annotation).unwrap(&mut health)
                    })
                    .into_iter();

                for parameter in lambda.parameters.iter().rev() {
                    let parameter_type_annotation =
                        parameter
                            .value
                            .type_annotation
                            .clone()
                            .map(|type_annotation| {
                                self.lower_expression(type_annotation).unwrap(&mut health)
                            });

                    expression = expr! {
                        Lambda {
                            Attributes::default(),
                            Span::default();
                            parameter: parameter.value.binder.clone(),
                            parameter_type_annotation,
                            explicitness: parameter.value.explicitness,
                            laziness: parameter.value.laziness,
                            body_type_annotation: type_annotation.next(),
                            body: expression,
                        }
                    };
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

                let mut expression = self.lower_expression(expression).unwrap(&mut health);

                let mut type_annotation = let_in
                    .type_annotation
                    .map(|type_annotation| {
                        self.lower_expression(type_annotation).unwrap(&mut health)
                    })
                    .into_iter();

                for parameter in let_in.parameters.iter().rev() {
                    let parameter_type_annotation =
                        parameter.value.type_annotation.clone().map(|expression| {
                            self.lower_expression(expression).unwrap(&mut health)
                        });

                    expression = expr! {
                        Lambda {
                            Attributes::default(), Span::default();
                            parameter: parameter.value.binder.clone(),
                            parameter_type_annotation,
                            explicitness: parameter.value.explicitness,
                            laziness: parameter.value.laziness,
                            body_type_annotation: type_annotation.next(),
                            body: expression,
                        }
                    };
                }

                let body = self.lower_expression(let_in.scope).unwrap(&mut health);

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

                for case in analysis.cases {
                    cases.push(lowered_ast::Case {
                        pattern: self.lower_pattern(case.pattern).unwrap(&mut health),
                        body: self.lower_expression(case.body.clone()).unwrap(&mut health),
                    });
                }

                let subject = self
                    .lower_expression(analysis.scrutinee)
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

        let attributes = self
            .lower_attributes(&pattern.attributes, &pattern)
            .unwrap(&mut health);

        let pattern = match pattern.value {
            // @Note awkward API!
            NumberLiteral(literal) => {
                todo!()
                //     {
                //     let span = pattern.span;
                //     map_outcome_from_result(
                //         self.lower_number_literal(*literal, span, &attributes),
                //         |literal| {
                //             pat! {
                //                 NumberLiteral(attributes, span; literal)
                //             }
                //         },
                //     )
                // }
                // .unwrap(&mut health)
            }
            TextLiteral(literal) => pat! {
                TextLiteral(attributes, pattern.span; literal)
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
            Application(application) => {
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
            SequenceLiteral(_sequence) => {
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
        &self,
        unchecked_attributes: &[ast::Attribute],
        target: &impl Target,
    ) -> Outcome<Attributes> {
        use lowered_ast::Attribute;

        let actual_targets = target.as_targets();
        let mut health = Health::Untainted;
        let mut conforming_attributes = Attributes::default();

        for attribute in unchecked_attributes {
            let Ok(attribute) = Attribute::parse(attribute, &self.options, &self.map.borrow(), self.reporter) else {
                health.taint();
                continue;
            };

            // search for non-conforming attributes
            {
                let expected_targets = attribute.value.targets();

                if !expected_targets.contains(actual_targets) {
                    Diagnostic::error()
                        .code(Code::E013)
                        .message(format!(
                            "attribute `{}` is ascribed to {}",
                            attribute.value.name(),
                            target.name()
                        ))
                        .labeled_primary_span(&attribute, "misplaced attribute")
                        .labeled_secondary_span(target, "incompatible item")
                        .note(format!(
                            "attribute `{}` can only be ascribed to {}",
                            attribute.value.name(),
                            expected_targets.description(),
                        ))
                        .report(self.reporter);
                    health.taint();
                    continue;
                }
            }

            conforming_attributes.0.push(attribute);
        }

        let attributes = self
            .check_attribute_synergy(conforming_attributes)
            .unwrap(&mut health);

        health &= target.check_attributes(&attributes, self.reporter);

        health.of(attributes)
    }

    fn check_attribute_synergy(&self, conforming_attributes: Attributes) -> Outcome<Attributes> {
        let mut attributes = Attributes::default();
        let mut health = Health::Untainted;

        // search for conflicting or duplicate attributes
        for attribute in &conforming_attributes.0 {
            if attribute.value.can_be_applied_multiple_times() {
                attributes.0.push(attribute.clone());
                continue;
            }

            let is_homonymous =
                Predicate(|some_attribute| some_attribute.name() == attribute.value.name());

            let homonymous_attributes: Vec<_> =
                conforming_attributes.filter(is_homonymous).collect();

            if !attributes.contains(is_homonymous) {
                attributes.0.push(attribute.clone());
            }

            if let [first, _second, ..] = &*homonymous_attributes {
                Diagnostic::error()
                    .code(Code::E006)
                    .message(format!("multiple `{}` attributes", first.value.name()))
                    .labeled_primary_spans(
                        homonymous_attributes.into_iter(),
                        "duplicate or conflicting attribute",
                    )
                    .report(self.reporter);
                health.taint();
            }
        }

        // no further checks necessary if empty
        if attributes.0.is_empty() {
            return health.of(attributes);
        }

        fn check_mutual_exclusivity<Q: Query>(
            query: Q,
            attributes: &Attributes,
            reporter: &Reporter,
        ) -> Result {
            let attributes = attributes.filter(query).collect::<Vec<_>>();

            if attributes.len() > 1 {
                let listing = ordered_listing(
                    attributes
                        .iter()
                        .map(|attribute| attribute.value.name().to_str().quote()),
                    Conjunction::And,
                );

                Diagnostic::error()
                    .code(Code::E014)
                    .message(format!("attributes {} are mutually exclusive", listing))
                    .labeled_primary_spans(attributes.into_iter(), "conflicting attribute")
                    .report(reporter);
                return Err(());
            }

            Ok(())
        }

        {
            use AttributeName::*;

            health &= check_mutual_exclusivity(Intrinsic.or(Known), &attributes, self.reporter);
            health &= check_mutual_exclusivity(
                DocAttributes.or(DocReservedIdentifiers),
                &attributes,
                self.reporter,
            );
            health &= check_mutual_exclusivity(
                DocAttribute.or(DocReservedIdentifier),
                &attributes,
                self.reporter,
            );
            health &= check_mutual_exclusivity(Moving.or(Abstract), &attributes, self.reporter);
            health &= check_mutual_exclusivity(
                Int.or(Int32).or(Int64).or(Nat).or(Nat32).or(Nat64),
                &attributes,
                self.reporter,
            );
            health &= check_mutual_exclusivity(Rune.or(Text), &attributes, self.reporter);
            health &= check_mutual_exclusivity(List.or(Vector), &attributes, self.reporter);
        }

        for attribute in attributes.filter(Predicate(|attribute| !attribute.is_fully_implemented()))
        {
            Diagnostic::unimplemented(format!("attribute `{}`", attribute.value.name()))
                .primary_span(attribute)
                .report(self.reporter);
            health.taint();
        }

        // @Task replace this concept with a feature system
        if !self.options.internal_features_enabled {
            for attribute in attributes.filter(Predicate(AttributeKind::is_internal)) {
                Diagnostic::error()
                    .code(Code::E038)
                    .message(format!(
                        "attribute `{}` is an internal feature",
                        attribute.value.name()
                    ))
                    .primary_span(attribute)
                    .report(self.reporter);
                health.taint();
            }
        }

        health.of(attributes)
    }

    fn lower_number_literal(
        &mut self,
        number: String,
        span: Span,
        attributes: &Attributes,
    ) -> Result<Number> {
        (if attributes.contains(AttributeName::Nat32) {
            number
                .parse()
                .map_err(|_| ("Nat32", NAT32_INTERVAL_REPRESENTATION))
                .map(Number::Nat32)
        } else if attributes.contains(AttributeName::Nat64) {
            number
                .parse()
                .map_err(|_| ("Nat64", NAT64_INTERVAL_REPRESENTATION))
                .map(Number::Nat64)
        } else if attributes.contains(AttributeName::Int) {
            Ok(Number::Int(number.parse().unwrap()))
        } else if attributes.contains(AttributeName::Int32) {
            number
                .parse()
                .map_err(|_| ("Int32", INT32_INTERVAL_REPRESENTATION))
                .map(Number::Int32)
        } else if attributes.contains(AttributeName::Int64) {
            number
                .parse()
                .map_err(|_| ("Int64", INT64_INTERVAL_REPRESENTATION))
                .map(Number::Int64)
        } else {
            // and optionally attributes.has(AttributeKind::nat)
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
    ) -> Outcome<lowered_ast::Expression> {
        let mut health = Health::Untainted;

        let mut expression = self.lower_expression(type_annotation).unwrap(&mut health);

        for parameter in parameters.into_iter().rev() {
            if parameter.value.type_annotation.is_none() {
                Diagnostic::missing_mandatory_type_annotation(
                    &parameter,
                    AnnotationTarget::Parameter(&parameter),
                )
                .report(self.reporter);
                health.taint();
            }
            // @Note awkward API! + the code above ^
            let parameter_type_annotation =
                Outcome::from(parameter.value.type_annotation).unwrap(&mut health);

            let parameter_type_annotation = self
                .lower_expression(parameter_type_annotation)
                .unwrap(&mut health);

            expression = expr! {
                PiType {
                    Attributes::default(),
                    Span::default();
                    explicitness: parameter.value.explicitness,
                    laziness: parameter.value.laziness,
                    parameter: Some(parameter.value.binder),
                    domain: parameter_type_annotation.clone(),
                    codomain: expression,
                }
            };
        }

        health.of(expression)
    }
}

#[derive(Default)]
pub struct Options {
    /// Specifies if internal language and library features are enabled.
    pub internal_features_enabled: bool,
    /// Specifies if documentation comments should be kept in the lowered AST.
    pub keep_documentation_comments: bool,
}

struct DeclarationContext {
    is_root: bool,
    /// The path of inline module declarations leading to a declaration.
    ///
    /// The path starts either at the root module declaration (excluding it) or
    /// at the closest out-of-line module (including it).
    ///
    /// Exclusively used to compute the path of out-of-line modules inside of inline
    /// modules!
    inline_modules: Vec<String>,
}

const NAT32_INTERVAL_REPRESENTATION: &str = "[0, 2^32-1]";
const NAT64_INTERVAL_REPRESENTATION: &str = "[0, 2^64-1]";
const NAT_INTERVAL_REPRESENTATION: &str = "[0, infinity)";
const INT32_INTERVAL_REPRESENTATION: &str = "[-2^31, 2^31-1]";
const INT64_INTERVAL_REPRESENTATION: &str = "[-2^63, 2^63-1]";

// @Beacon @Beacon @Task don't use reporter+Result<_> here but
// a custom error type (w/o ::Unrecoverable)
// and turn that stuff into stuff later

impl lowered_ast::attributes::Attribute {
    pub(crate) fn parse(
        attribute: &ast::Attribute,
        options: &Options,
        map: &SourceMap,
        reporter: &Reporter,
    ) -> Result<Self> {
        Ok(Self::new(
            attribute.span,
            AttributeKind::parse(attribute, options, map, reporter)?,
        ))
    }
}

impl lowered_ast::attributes::AttributeKind {
    // @Task allow unordered named attributes e.g. `@(unstable (reason "x") (feature thing))`
    pub(crate) fn parse(
        // @Task take by value and create parsing helpers on ast::Attribute and ast::Attributes
        attribute: &ast::Attribute,
        options: &Options,
        map: &SourceMap,
        reporter: &Reporter,
    ) -> Result<Self> {
        let ast::AttributeKind::Regular { binder, arguments } = &attribute.value else {
            return Ok(Self::Doc {
                content: if options.keep_documentation_comments {
                    map.snippet(attribute.span)
                        .trim_start_matches(";;")
                        .into()
                } else {
                    default()
                },
            })
        };

        let arguments: &mut &[_] = &mut &**arguments;

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
            use AttributeName::*;

            let name: AttributeName = binder
                .as_str()
                .parse()
                .map_err(|_| AttributeParsingError::UndefinedAttribute(binder.clone()))?;

            Ok(match name {
                Abstract => Self::Abstract,
                Allow | Deny | Forbid | Warn => {
                    let lint = lowered_ast::attributes::Lint::parse(
                        argument(arguments, attribute.span, reporter)?
                            .path(Some("lint"), reporter)?
                            .clone(),
                        reporter,
                    )?;

                    match name {
                        Allow => Self::Allow { lint },
                        Deny => Self::Deny { lint },
                        Forbid => Self::Forbid { lint },
                        Warn => Self::Warn { lint },
                        _ => unreachable!(),
                    }
                }
                Deprecated => Self::Deprecated(lowered_ast::attributes::Deprecated {
                    reason: optional_argument(arguments)
                        .map(|argument| argument.text_literal(Some("reason"), reporter))
                        .transpose()?
                        .cloned(),
                    // @Task parse version
                    since: None,
                    // @Task parse version
                    removal: None,
                    replacement: optional_argument(arguments)
                        .map(|argument| argument.text_literal(Some("replacement"), reporter))
                        .transpose()?
                        .cloned(),
                }),
                Doc => Self::Doc {
                    content: if options.keep_documentation_comments {
                        argument(arguments, attribute.span, reporter)?
                            .text_literal(None, reporter)?
                            .clone()
                    } else {
                        *arguments = &arguments[1..];
                        default()
                    },
                },
                DocAttribute => Self::DocAttribute {
                    name: argument(arguments, attribute.span, reporter)?
                        .text_literal(None, reporter)?
                        .clone(),
                },
                DocAttributes => Self::DocAttributes,
                DocReservedIdentifier => Self::DocReservedIdentifier {
                    name: argument(arguments, attribute.span, reporter)?
                        .text_literal(None, reporter)?
                        .clone(),
                },
                DocReservedIdentifiers => Self::DocReservedIdentifiers,
                Intrinsic => Self::Intrinsic,
                If => {
                    Diagnostic::unimplemented("attribute `if`")
                        .primary_span(attribute)
                        .report(reporter);
                    return Err(AttributeParsingError::Unrecoverable);
                }
                Ignore => Self::Ignore,
                Include => Self::Include,
                Known => Self::Known,
                Int => Self::Int,
                Int32 => Self::Int32,
                Int64 => Self::Int64,
                List => Self::List,
                Location => {
                    let path = argument(arguments, attribute.span, reporter)?
                        .text_literal(Some("path"), reporter)?
                        .clone();

                    Self::Location { path }
                }
                Moving => Self::Moving,
                Nat => Self::Nat,
                Nat32 => Self::Nat32,
                Nat64 => Self::Nat64,
                Public => {
                    let reach = optional_argument(arguments)
                        .map(|argument| argument.path(Some("reach"), reporter))
                        .transpose()?
                        .cloned();

                    Self::Public(self::Public { reach })
                }
                RecursionLimit => {
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
                Rune => Self::Rune,
                Static => Self::Static,
                Statistics => Self::Statistics,
                Test => Self::Test,
                Text => Self::Text,
                Unsafe => Self::Unsafe,
                Unstable => {
                    Diagnostic::unimplemented("attribute `unstable`")
                        .primary_span(attribute)
                        .report(reporter);

                    return Err(AttributeParsingError::Unrecoverable);
                }
                Vector => Self::Vector,
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

// @Beacon hideous accessors! @Task deduplicate!

impl ast::AttributeArgument {
    fn number_literal(
        &self,
        name: Option<&'static str>,
        reporter: &Reporter,
    ) -> Result<&Atom, AttributeParsingError> {
        use ast::AttributeArgumentKind::*;

        match &self.value {
            NumberLiteral(literal) => Ok(literal),
            Named(named) => named.handle(
                name,
                |argument| match &argument.value {
                    NumberLiteral(literal) => Ok(literal),
                    kind => {
                        Diagnostic::invalid_attribute_argument_type(
                            (argument.span, kind.name()),
                            "number literal",
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
                    "positional or named number literal",
                )
                .report(reporter);
                Err(AttributeParsingError::Unrecoverable)
            }
        }
    }

    fn text_literal(
        &self,
        name: Option<&'static str>,
        reporter: &Reporter,
    ) -> Result<&Atom, AttributeParsingError> {
        use ast::AttributeArgumentKind::*;

        match &self.value {
            TextLiteral(literal) => Ok(literal),
            Named(named) => named.handle(
                name,
                |argument| match &argument.value {
                    TextLiteral(literal) => Ok(literal),
                    kind => {
                        Diagnostic::invalid_attribute_argument_type(
                            (argument.span, kind.name()),
                            "text literal",
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
                    "positional or named text literal",
                )
                .report(reporter);
                Err(AttributeParsingError::Unrecoverable)
            }
        }
    }

    fn path(
        &self,
        name: Option<&'static str>,
        reporter: &Reporter,
    ) -> Result<&Path, AttributeParsingError> {
        use ast::AttributeArgumentKind::*;

        match &self.value {
            Path(literal) => Ok(literal),
            Named(named) => named
                .handle(
                    name,
                    |argument| match &argument.value {
                        Path(literal) => Ok(literal),
                        kind => {
                            Diagnostic::invalid_attribute_argument_type(
                                (argument.span, kind.name()),
                                "path",
                            )
                            .report(reporter);
                            Err(AttributeParsingError::Unrecoverable)
                        }
                    },
                    reporter,
                )
                .map(|path| &**path),
            kind => {
                Diagnostic::invalid_attribute_argument_type(
                    (self.span, kind.name()),
                    "positional or named path",
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
        handle: impl FnOnce(&ast::AttributeArgument) -> Result<&T, AttributeParsingError>,
        reporter: &Reporter,
    ) -> Result<&T, AttributeParsingError> {
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

impl lowered_ast::attributes::Lint {
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
            Parameter(parameter) => format!(
                "`{}({}: ?type)`",
                parameter.value.explicitness, parameter.value.binder
            )
            .into(),
            Declaration(_) => "`: ?type`".into(),
        };

        let binders = match target {
            Parameter(parameter) => (&parameter.value.binder).quote(),
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
    Parameter(&'a Parameter),
    Declaration(&'a ast::Identifier),
}

impl AnnotationTarget<'_> {
    fn name(&self) -> &'static str {
        match self {
            Self::Parameter(_) => "parameter",
            Self::Declaration(_) => "declaration",
        }
    }
}
