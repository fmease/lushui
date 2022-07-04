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
//! * simplify use-declarations by unfolding use-path trees (note: this should be removed)
//! * parses general attributes into concrete ones and
//! * validates their location (item target), uniqueness if applicable,
//!   exclusivity rules
//! * checks if the mandatory type annotations on top-level declarations and their
//!   parameters are present
//! * gates a lot of unsupported features
//!
//! To be implemented:
//!
//! * apply attribute groups

// @Task ungate named arguments but validate them in the resolver (and/or typer)

use super::{
    ast::{self, Explicit, HangerKind, Parameter, Path},
    lowered_ast::{
        self,
        attributes::{Predicate, Public, Query, Target},
        AttributeKind, AttributeName, Attributes,
    },
};
use crate::{
    component::Component,
    diagnostics::{reporter::ErasedReportedError, Diagnostic, ErrorCode, Reporter},
    error::{Health, OkIfUntaintedExt, PossiblyErroneous, Result, Stain},
    session::BuildSession,
    span::{Span, Spanned, Spanning},
    utility::{Atom, Conjunction, IOError, ListingExt, QuoteExt, SmallVec, Str},
};
use smallvec::smallvec;
use std::{default::default, fmt, iter::once};

/// Lower a file.
pub fn lower_file(
    declaration: ast::Declaration,
    options: Options,
    component: &Component,
    session: &BuildSession,
) -> Result<lowered_ast::Declaration> {
    let mut lowerer = Lowerer::new(options, component, session);
    let mut declaration = lowerer.lower_declaration(declaration);
    let root = declaration.pop().unwrap();
    Result::ok_if_untainted(root, lowerer.health)
}

/// The state of the lowering pass.
struct Lowerer<'a> {
    options: Options,
    component: &'a Component,
    session: &'a BuildSession,
    health: Health,
}

impl<'a> Lowerer<'a> {
    fn new(options: Options, component: &'a Component, session: &'a BuildSession) -> Self {
        Self {
            options,
            component,
            session,
            health: Health::Untainted,
        }
    }

    fn lower_declaration(
        &mut self,
        declaration: ast::Declaration,
    ) -> SmallVec<lowered_ast::Declaration, 1> {
        self.lower_declaration_with_context(declaration, &DeclarationContext::default())
    }

    fn lower_declaration_with_context(
        &mut self,
        declaration: ast::Declaration,
        context: &DeclarationContext,
    ) -> SmallVec<lowered_ast::Declaration, 1> {
        use ast::DeclarationKind::*;

        let attributes = self.lower_attributes(&declaration.attributes, &declaration);

        match declaration.bare {
            Function(function) => smallvec![lowered_ast::Declaration::new(
                attributes,
                declaration.span,
                self.lower_function_declaration(*function).into(),
            )],
            Data(type_) => smallvec![lowered_ast::Declaration::new(
                attributes,
                declaration.span,
                self.lower_data_declaration(*type_).into()
            )],
            Constructor(constructor) => {
                smallvec![lowered_ast::Declaration::new(
                    attributes,
                    declaration.span,
                    self.lower_constructor_declaration(*constructor).into(),
                )]
            }
            Module(module) => {
                smallvec![self.lower_module_declaration(
                    *module,
                    attributes,
                    declaration.span,
                    context
                )]
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
                    .flat_map(|declaration| self.lower_declaration(declaration))
                    .collect()
            }
            Use(use_) => self.lower_use_declaration(*use_, attributes, declaration.span),
        }
    }

    fn lower_function_declaration(&mut self, function: ast::Function) -> lowered_ast::Function {
        let declaration_type_annotation = match function.type_annotation {
            Some(type_annotation) => type_annotation,
            None => {
                Diagnostic::missing_mandatory_type_annotation(
                    function.binder.span().fit_end(&function.parameters).end(),
                    AnnotationTarget::Declaration(&function.binder),
                )
                .report(self.session.reporter());
                self.health.taint();
                PossiblyErroneous::error()
            }
        };

        // @Note type_annotation is currently lowered twice @Task remove duplicate work
        // @Task find a way to use `Option::map` (currently does not work because of
        // partial moves, I hate those), use local bindings
        let body = match function.body {
            Some(body) => {
                let mut body = self.lower_expression(body);

                {
                    let mut type_annotation =
                        once(self.lower_expression(declaration_type_annotation.clone()));

                    for parameter in function.parameters.iter().rev() {
                        let parameter_type_annotation = match &parameter.bare.type_annotation {
                            Some(type_annotation) => type_annotation.clone(),
                            None => {
                                Diagnostic::missing_mandatory_type_annotation(
                                    parameter,
                                    AnnotationTarget::Parameter(parameter),
                                )
                                .report(self.session.reporter());
                                self.health.taint();
                                PossiblyErroneous::error()
                            }
                        };

                        let parameter_type_annotation =
                            self.lower_expression(parameter_type_annotation);

                        body = lowered_ast::Expression::new(
                            default(),
                            default(),
                            lowered_ast::Lambda {
                                parameter: parameter.bare.binder.clone(),
                                parameter_type_annotation: Some(parameter_type_annotation.clone()),
                                explicitness: parameter.bare.explicitness,
                                laziness: parameter.bare.laziness,
                                body_type_annotation: type_annotation.next(),
                                body,
                            }
                            .into(),
                        );
                    }
                }
                Some(body)
            }
            None => None,
        };

        let type_annotation = self
            .lower_parameters_to_annotated_ones(function.parameters, declaration_type_annotation);

        lowered_ast::Function {
            binder: function.binder,
            type_annotation,
            expression: body,
        }
    }

    fn lower_data_declaration(&mut self, type_: ast::Data) -> lowered_ast::Data {
        let data_type_annotation = match type_.type_annotation {
            Some(type_annotation) => type_annotation,
            None => {
                Diagnostic::missing_mandatory_type_annotation(
                    type_.binder.span().fit_end(&type_.parameters).end(),
                    AnnotationTarget::Declaration(&type_.binder),
                )
                .report(self.session.reporter());
                self.health.taint();
                PossiblyErroneous::error()
            }
        };

        let type_annotation =
            self.lower_parameters_to_annotated_ones(type_.parameters, data_type_annotation);

        let constructors = type_.constructors.map(|constructors| {
            constructors
                .into_iter()
                .flat_map(|constructor| self.lower_declaration(constructor))
                .collect()
        });

        lowered_ast::Data {
            binder: type_.binder,
            type_annotation,
            constructors,
        }
    }

    fn lower_constructor_declaration(
        &mut self,
        constructor: ast::Constructor,
    ) -> lowered_ast::Constructor {
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
                .report(self.session.reporter());
                self.health.taint();
                PossiblyErroneous::error()
            }
        };

        let type_annotation = self.lower_parameters_to_annotated_ones(
            constructor.parameters,
            constructor_type_annotation,
        );

        if let Some(body) = constructor.body {
            let body = self.lower_expression(body);

            // @Task improve the labels etc, don't say "conflicting definition"
            // it's technically true, but we can do so much better!
            Diagnostic::error()
                .code(ErrorCode::E020)
                .message(format!(
                    "‘{}’ is defined multiple times in this scope",
                    constructor.binder
                ))
                .labeled_primary_span(&body, "conflicting definition")
                .note(
                    "the body of the constructor is implied but it also has a body introduced by ‘= ?value’",
                ).report(self.session.reporter());
            self.health.taint();
        }

        lowered_ast::Constructor {
            binder: constructor.binder,
            type_annotation,
        }
    }

    fn lower_module_declaration(
        &mut self,
        module: ast::Module,
        mut attributes: Attributes,
        span: Span,
        context: &DeclarationContext,
    ) -> lowered_ast::Declaration {
        let is_inline_module = module.declarations.is_some();

        let declarations = match module.declarations {
            Some(declarations) => declarations,
            None => {
                let mut path = self.session.shared_map()[module.file]
                    .path()
                    .unwrap()
                    .parent()
                    .unwrap()
                    .to_owned();

                match attributes.get::<{ AttributeName::Location }>() {
                    Some(location) => {
                        let mut inline_modules = &context.inline_modules[..];
                        let _ = inline_modules.take_last();

                        path.extend(inline_modules);
                        path.push(location);
                    }
                    None => {
                        path.extend(&context.inline_modules);
                        path.push(module.binder.as_str());
                    }
                };

                path.set_extension(crate::FILE_EXTENSION);

                let file = self
                    .session
                    .map()
                    .load(path.clone(), Some(self.component.index()));
                let file = match file {
                    Ok(file) => file,
                    Err(error) => {
                        // @Task instead of a note saying the error, print a help message
                        // saying to create the missing file or change the access rights etc.
                        // @Note awkward API!
                        Diagnostic::error()
                            .code(ErrorCode::E016)
                            .message(format!("could not load the module ‘{}’", module.binder))
                            .primary_span(span)
                            .note(IOError(error, &path).to_string())
                            .report(self.session.reporter());
                        self.health.taint();
                        return PossiblyErroneous::error();
                    }
                };

                let Ok(declaration) = crate::syntax::parse_module_file(file, module.binder.clone(), self.session) else {
                    self.health.taint();
                    return PossiblyErroneous::error();
                };

                // at this point in time, they are still on the module header if at all
                assert!(declaration.attributes.is_empty());

                let module: ast::Module = declaration.bare.try_into().unwrap();
                module.declarations.unwrap()
            }
        };

        let mut child_context = DeclarationContext::default();
        if is_inline_module {
            child_context
                .inline_modules
                .extend(context.inline_modules.clone());
        };
        child_context.inline_modules.push(module.binder.to_string());

        let mut lowered_declarations = Vec::new();
        let mut has_header = false;

        for (index, declaration) in declarations.into_iter().enumerate() {
            if matches!(declaration.bare, ast::DeclarationKind::ModuleHeader) {
                if index == 0 {
                    // @Bug this sequence may lead to some unnecessary diagnostics being emitted
                    // since the "synergy check" (which filters duplicate attribute) is run too late

                    let module_header_attributes =
                        self.lower_attributes(&declaration.attributes, &declaration);
                    attributes.0.extend(module_header_attributes.0);
                    attributes = self.check_attribute_synergy(attributes);
                } else {
                    Diagnostic::error()
                        .code(ErrorCode::E041)
                        .message("the module header has to be the first declaration of the module")
                        .primary_span(&declaration)
                        .with(|error| {
                            if has_header {
                                // @Task make this a note with a span/highlight!
                                error
                                    .note("however, the current module already has a module header")
                            } else {
                                error
                            }
                        })
                        .report(self.session.reporter());
                    self.health.taint();
                }

                has_header = true;
                continue;
            }

            lowered_declarations
                .extend(self.lower_declaration_with_context(declaration, &child_context));
        }

        lowered_ast::Declaration::new(
            attributes,
            span,
            lowered_ast::Module {
                binder: module.binder,
                file: module.file,
                declarations: lowered_declarations,
            }
            .into(),
        )
    }

    fn lower_use_declaration(
        &mut self,
        use_: ast::Use,
        attributes: Attributes,
        span: Span,
    ) -> SmallVec<lowered_ast::Declaration, 1> {
        // @Task verify that the resulting spans are correct

        use ast::{UsePathTree, UsePathTreeKind::*};

        let mut declarations = SmallVec::new();

        // @Beacon @Task we should improve this error handling here!!!!
        fn lower_use_path_tree(
            path: Path,
            bindings: Vec<UsePathTree>,
            span: Span,
            attributes: Attributes,
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
                match binding.bare {
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
                                // @Task improve the message for `use topmost.(self)`: hint that `self`
                                // is effectively unnamed because `topmost` is unnamed
                                Diagnostic::invalid_unnamed_path_hanger(target.hanger.unwrap())
                            });
                        let binder = try_!(binder);

                        declarations.push(lowered_ast::Declaration::new(
                            attributes.clone(),
                            span,
                            lowered_ast::Use {
                                binder,
                                target: combined_target,
                            }
                            .into(),
                        ));
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
            match use_.bindings.bare {
                Single { target, binder } => {
                    let binder = binder.or_else(|| target.last_identifier().cloned());
                    let Some(binder) = binder else {
                                // @Task improve the message for `use topmost.(self)`: hint that `self`
                                // is effectively unnamed because `topmost` is unnamed
                                // @Task the message is even worse (it is misleading!) with `use extern.(self)`
                                // currently leads to the suggestion to bind `self` to an identifier but
                                // for `extern` that is illegal, too
                                Diagnostic::invalid_unnamed_path_hanger(target.hanger.unwrap())
                                    .report(self.session.reporter());
                                self.health.taint();
                                break 'discriminate;
                            };

                    declarations.push(lowered_ast::Declaration::new(
                        attributes,
                        span,
                        lowered_ast::Use { binder, target }.into(),
                    ));
                }
                Multiple { path, bindings } => lower_use_path_tree(
                    path,
                    bindings,
                    span,
                    attributes,
                    &mut declarations,
                    self.session.reporter(),
                )
                .stain(&mut self.health),
            }
        }

        declarations
    }

    /// Lower an expression.
    fn lower_expression(&mut self, expression: ast::Expression) -> lowered_ast::Expression {
        use ast::ExpressionKind::*;

        let attributes = self.lower_attributes(&expression.attributes, &expression);

        let expression = match expression.bare {
            PiTypeLiteral(pi) => {
                let domain = self.lower_expression(pi.domain.expression);
                let codomain = self.lower_expression(pi.codomain);

                lowered_ast::Expression::new(
                    attributes,
                    expression.span,
                    lowered_ast::PiType {
                        explicitness: pi.domain.explicitness,
                        laziness: pi.domain.laziness,
                        parameter: pi.domain.binder.clone(),
                        domain,
                        codomain,
                    }
                    .into(),
                )
            }
            Application(application) => {
                if let Some(binder) = &application.binder {
                    Diagnostic::error()
                        .message("named arguments are not supported yet")
                        .primary_span(binder)
                        .report(self.session.reporter());
                    self.health.taint();
                }

                let callee = self.lower_expression(application.callee);
                let argument = self.lower_expression(application.argument);

                lowered_ast::Expression::new(
                    attributes,
                    expression.span,
                    lowered_ast::Application {
                        callee,
                        argument,
                        explicitness: application.explicitness,
                    }
                    .into(),
                )
            }
            TypeLiteral => lowered_ast::Expression::new(
                attributes,
                expression.span,
                lowered_ast::ExpressionKind::TypeLiteral,
            ),
            // @Task avoid re-boxing!
            NumberLiteral(number) => {
                lowered_ast::Expression::new(attributes, expression.span, (*number).into())
            }
            // @Task avoid re-boxing!
            TextLiteral(text) => {
                lowered_ast::Expression::new(attributes, expression.span, (*text).into())
            }
            TypedHole(_hole) => {
                Diagnostic::error()
                    .message("typed holes are not supported yet")
                    .primary_span(expression.span)
                    .report(self.session.reporter());
                self.health.taint();
                PossiblyErroneous::error()
            }
            // @Task avoid re-boxing!
            Path(path) => lowered_ast::Expression::new(attributes, expression.span, (*path).into()),
            Field(_field) => {
                Diagnostic::error()
                    .message("record fields are not supported yet")
                    .primary_span(expression.span)
                    .report(self.session.reporter());
                self.health.taint();
                PossiblyErroneous::error()
            }
            LambdaLiteral(lambda) => {
                let mut expression = self.lower_expression(lambda.body);

                let mut type_annotation = lambda
                    .body_type_annotation
                    .map(|type_annotation| self.lower_expression(type_annotation))
                    .into_iter();

                for parameter in lambda.parameters.iter().rev() {
                    let parameter_type_annotation = parameter
                        .bare
                        .type_annotation
                        .clone()
                        .map(|type_annotation| self.lower_expression(type_annotation));

                    expression = lowered_ast::Expression::new(
                        default(),
                        default(),
                        lowered_ast::Lambda {
                            parameter: parameter.bare.binder.clone(),
                            parameter_type_annotation,
                            explicitness: parameter.bare.explicitness,
                            laziness: parameter.bare.laziness,
                            body_type_annotation: type_annotation.next(),
                            body: expression,
                        }
                        .into(),
                    );
                }

                expression
            }
            LetIn(let_in) => {
                let expression = {
                    let binder = &let_in.binder;

                    match let_in.expression {
                        Some(expression) => expression,
                        None => {
                            Diagnostic::error()
                                .code(ErrorCode::E012)
                                .message(format!("the let-binding ‘{}’ has no definition", binder))
                                .primary_span(
                                    let_in
                                        .binder
                                        .span()
                                        .fit_end(&let_in.parameters)
                                        .fit_end(&let_in.type_annotation)
                                        .end(),
                                )
                                .help("provide a definition with ‘= ?value’")
                                .report(self.session.reporter());
                            self.health.taint();
                            PossiblyErroneous::error()
                        }
                    }
                };

                let mut expression = self.lower_expression(expression);

                let mut type_annotation = let_in
                    .type_annotation
                    .map(|type_annotation| self.lower_expression(type_annotation))
                    .into_iter();

                for parameter in let_in.parameters.iter().rev() {
                    let parameter_type_annotation = parameter
                        .bare
                        .type_annotation
                        .clone()
                        .map(|expression| self.lower_expression(expression));

                    expression = lowered_ast::Expression::new(
                        default(),
                        default(),
                        lowered_ast::Lambda {
                            parameter: parameter.bare.binder.clone(),
                            parameter_type_annotation,
                            explicitness: parameter.bare.explicitness,
                            laziness: parameter.bare.laziness,
                            body_type_annotation: type_annotation.next(),
                            body: expression,
                        }
                        .into(),
                    );
                }

                let body = self.lower_expression(let_in.scope);

                lowered_ast::Expression::new(
                    default(),
                    default(),
                    lowered_ast::Application {
                        callee: lowered_ast::Expression::new(
                            default(),
                            default(),
                            lowered_ast::Lambda {
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
                            .into(),
                        ),
                        argument: expression,
                        explicitness: Explicit,
                    }
                    .into(),
                )
            }
            UseIn(_use_in) => {
                Diagnostic::error()
                    .message("use/in-expressions are not supported yet")
                    .primary_span(expression.span)
                    .report(self.session.reporter());
                self.health.taint();
                PossiblyErroneous::error()
            }
            CaseAnalysis(analysis) => {
                let mut cases = Vec::new();

                for case in analysis.cases {
                    cases.push(lowered_ast::Case {
                        pattern: self.lower_pattern(case.pattern),
                        body: self.lower_expression(case.body.clone()),
                    });
                }

                let scrutinee = self.lower_expression(analysis.scrutinee);

                lowered_ast::Expression::new(
                    attributes,
                    expression.span,
                    lowered_ast::CaseAnalysis { scrutinee, cases }.into(),
                )
            }
            DoBlock(_block) => {
                Diagnostic::error()
                    .message("do blocks are not supported yet")
                    .primary_span(expression.span)
                    .report(self.session.reporter());
                self.health.taint();
                PossiblyErroneous::error()
            }
            SequenceLiteral(sequence) => lowered_ast::Expression::new(
                attributes,
                expression.span,
                ast::SequenceLiteral {
                    path: sequence.path,
                    elements: sequence.elements.map(|elements| {
                        elements
                            .into_iter()
                            .map(|element| self.lower_expression(element))
                            .collect()
                    }),
                }
                .into(),
            ),
            Error => PossiblyErroneous::error(),
        };

        expression
    }

    /// Lower a pattern.
    fn lower_pattern(&mut self, pattern: ast::Pattern) -> lowered_ast::Pattern {
        use ast::PatternKind::*;

        let attributes = self.lower_attributes(&pattern.attributes, &pattern);

        match pattern.bare {
            // @Task avoid re-boxing!
            NumberLiteral(literal) => {
                lowered_ast::Pattern::new(attributes, pattern.span, (*literal).into())
            }
            // @Task avoid re-boxing!
            TextLiteral(literal) => {
                lowered_ast::Pattern::new(attributes, pattern.span, (*literal).into())
            }
            // @Task avoid re-boxing!
            Path(path) => lowered_ast::Pattern::new(attributes, pattern.span, (*path).into()),
            // @Task avoid re-boxing!
            Binder(binder) => lowered_ast::Pattern::new(attributes, pattern.span, (*binder).into()),
            Application(application) => {
                if let Some(binder) = &application.binder {
                    Diagnostic::error()
                        .message("named arguments are not supported yet")
                        .primary_span(binder)
                        .report(self.session.reporter());
                    self.health.taint();
                }

                let callee = self.lower_pattern(application.callee);
                let argument = self.lower_pattern(application.argument);

                lowered_ast::Pattern::new(
                    attributes,
                    pattern.span,
                    lowered_ast::Application {
                        callee,
                        explicitness: application.explicitness,
                        argument,
                    }
                    .into(),
                )
            }
            SequenceLiteral(sequence) => lowered_ast::Pattern::new(
                attributes,
                pattern.span,
                ast::SequenceLiteral {
                    path: sequence.path,
                    elements: sequence.elements.map(|elements| {
                        elements
                            .into_iter()
                            .map(|element| self.lower_pattern(element))
                            .collect()
                    }),
                }
                .into(),
            ),
        }
    }

    /// Lower attributes.
    // @Task filter out documentation attributes if !options.keep_documentation_comments
    fn lower_attributes(
        &mut self,
        unchecked_attributes: &[ast::Attribute],
        target: &impl Target,
    ) -> Attributes {
        use lowered_ast::Attribute;

        let actual_targets = target.as_targets();
        let mut conforming_attributes = Attributes::default();

        for attribute in unchecked_attributes {
            let Ok(attribute) = Attribute::parse(attribute, &self.options, self.session) else {
                self.health.taint();
                continue;
            };

            // search for non-conforming attributes
            {
                let expected_targets = attribute.bare.targets();

                if !expected_targets.contains(actual_targets) {
                    // @Question wording: "cannot be ascribed to"?
                    Diagnostic::error()
                        .code(ErrorCode::E013)
                        .message(format!(
                            "attribute ‘{}’ is ascribed to {}",
                            attribute.bare.name(),
                            target.name()
                        ))
                        .labeled_primary_span(&attribute, "misplaced attribute")
                        .labeled_secondary_span(target, "incompatible item")
                        .note(format!(
                            "attribute ‘{}’ can only be ascribed to {}",
                            attribute.bare.name(),
                            expected_targets.description(),
                        ))
                        .report(self.session.reporter());
                    self.health.taint();
                    continue;
                }
            }

            conforming_attributes.0.push(attribute);
        }

        let attributes = self.check_attribute_synergy(conforming_attributes);

        target
            .check_attributes(&attributes, self.session)
            .stain(&mut self.health);

        attributes
    }

    fn check_attribute_synergy(&mut self, conforming_attributes: Attributes) -> Attributes {
        let mut attributes = Attributes::default();

        // search for conflicting or duplicate attributes
        for attribute in &conforming_attributes.0 {
            if attribute.bare.can_be_applied_multiple_times() {
                attributes.0.push(attribute.clone());
                continue;
            }

            let is_homonymous =
                Predicate(|some_attribute| some_attribute.name() == attribute.bare.name());

            let homonymous_attributes: Vec<_> =
                conforming_attributes.filter(is_homonymous).collect();

            if !attributes.contains(is_homonymous) {
                attributes.0.push(attribute.clone());
            }

            if let [first, _second, ..] = &*homonymous_attributes {
                Diagnostic::error()
                    .code(ErrorCode::E006)
                    .message(format!("multiple ‘{}’ attributes", first.bare.name()))
                    .labeled_primary_spans(
                        homonymous_attributes,
                        "duplicate or conflicting attribute",
                    )
                    .report(self.session.reporter());
                self.health.taint();
            }
        }

        // no further checks necessary if empty
        if attributes.0.is_empty() {
            return attributes;
        }

        fn check_mutual_exclusivity<Q: Query>(
            query: Q,
            attributes: &Attributes,
            reporter: &Reporter,
        ) -> Result {
            let attributes = attributes.filter(query).collect::<Vec<_>>();

            if attributes.len() > 1 {
                let listing = attributes
                    .iter()
                    .map(|attribute| attribute.bare.name().to_str().quote())
                    .list(Conjunction::And);

                return Err(Diagnostic::error()
                    .code(ErrorCode::E014)
                    .message(format!("attributes {listing} are mutually exclusive"))
                    .labeled_primary_spans(attributes, "conflicting attribute")
                    .report(reporter));
            }

            Ok(())
        }

        {
            use AttributeName::*;

            check_mutual_exclusivity(Intrinsic.or(Known), &attributes, self.session.reporter())
                .stain(&mut self.health);
            check_mutual_exclusivity(
                DocAttributes.or(DocReservedIdentifiers),
                &attributes,
                self.session.reporter(),
            )
            .stain(&mut self.health);
            check_mutual_exclusivity(
                DocAttribute.or(DocReservedIdentifier),
                &attributes,
                self.session.reporter(),
            )
            .stain(&mut self.health);

            check_mutual_exclusivity(Moving.or(Abstract), &attributes, self.session.reporter())
                .stain(&mut self.health);
        }

        for attribute in attributes.filter(Predicate(|attribute| !attribute.is_fully_implemented()))
        {
            Diagnostic::error()
                .message(format!(
                    "the attribute ‘{}’ is not supported yet",
                    attribute.bare.name()
                ))
                .primary_span(attribute)
                .report(self.session.reporter());
            self.health.taint();
        }

        // @Task replace this concept with a feature system
        if !self.options.internal_features_enabled {
            for attribute in attributes.filter(Predicate(AttributeKind::is_internal)) {
                Diagnostic::error()
                    .code(ErrorCode::E038)
                    .message(format!(
                        "the attribute ‘{}’ is an internal feature",
                        attribute.bare.name()
                    ))
                    .primary_span(attribute)
                    .report(self.session.reporter());
                self.health.taint();
            }
        }

        attributes
    }

    /// Lower annotated parameters.
    fn lower_parameters_to_annotated_ones(
        &mut self,
        parameters: ast::Parameters,
        type_annotation: ast::Expression,
    ) -> lowered_ast::Expression {
        let mut expression = self.lower_expression(type_annotation);

        for parameter in parameters.into_iter().rev() {
            let parameter_type_annotation = match parameter.bare.type_annotation {
                Some(type_annotation) => type_annotation,
                None => {
                    Diagnostic::missing_mandatory_type_annotation(
                        &parameter,
                        AnnotationTarget::Parameter(&parameter),
                    )
                    .report(self.session.reporter());
                    self.health.taint();
                    PossiblyErroneous::error()
                }
            };

            let parameter_type_annotation = self.lower_expression(parameter_type_annotation);

            expression = lowered_ast::Expression::new(
                default(),
                default(),
                lowered_ast::PiType {
                    explicitness: parameter.bare.explicitness,
                    laziness: parameter.bare.laziness,
                    parameter: Some(parameter.bare.binder),
                    domain: parameter_type_annotation.clone(),
                    codomain: expression,
                }
                .into(),
            );
        }

        expression
    }
}

// @Task get rid of this by smh. moving this information into the session
#[derive(Default)]
pub struct Options {
    /// Specifies if internal language and library features are enabled.
    pub internal_features_enabled: bool,
    /// Specifies if documentation comments should be kept in the lowered AST.
    pub keep_documentation_comments: bool,
}

#[derive(Default)]
struct DeclarationContext {
    /// The path of inline module declarations leading to a declaration.
    ///
    /// The path starts either at the root module declaration or at the closest out-of-line module.
    inline_modules: Vec<String>,
}

// @Task dedup! (also found in the resolver/literal)
const NAT32_INTERVAL_REPRESENTATION: &str = "[0, 2^32-1]";

// @Beacon @Beacon @Task don't use reporter+Result<_> here but
// a custom error type (w/o ::Unrecoverable)
// and turn that stuff into stuff later

impl lowered_ast::Attribute {
    pub(crate) fn parse(
        attribute: &ast::Attribute,
        options: &Options,
        session: &BuildSession,
    ) -> Result<Self> {
        Ok(Self::new(
            attribute.span,
            AttributeKind::parse(attribute, options, session)?,
        ))
    }
}

impl lowered_ast::AttributeKind {
    // @Task allow unordered named attributes e.g. `@(unstable (reason "x") (feature thing))`
    pub(crate) fn parse(
        // @Task take by value and create parsing helpers on ast::Attribute and ast::Attributes
        attribute: &ast::Attribute,
        options: &Options,
        session: &BuildSession,
    ) -> Result<Self> {
        let ast::AttributeKind::Regular { binder, arguments } = &attribute.bare else {
            return Ok(Self::Doc {
                content: if options.keep_documentation_comments {
                    session.shared_map().snippet(attribute.span)
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
                AttributeParsingError::Erased(
                    Diagnostic::error()
                        .code(ErrorCode::E019)
                        .message("too few attribute arguments provided")
                        .primary_span(span)
                        .report(reporter),
                )
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
                        argument(arguments, attribute.span, session.reporter())?
                            .path(Some("lint"), session.reporter())?
                            .clone(),
                        session.reporter(),
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
                        .map(|argument| argument.text_literal(Some("reason"), session.reporter()))
                        .transpose()?
                        .cloned(),
                    // @Task parse version
                    since: None,
                    // @Task parse version
                    removal: None,
                    replacement: optional_argument(arguments)
                        .map(|argument| {
                            argument.text_literal(Some("replacement"), session.reporter())
                        })
                        .transpose()?
                        .cloned(),
                }),
                Doc => Self::Doc {
                    content: if options.keep_documentation_comments {
                        argument(arguments, attribute.span, session.reporter())?
                            .text_literal(None, session.reporter())?
                            .clone()
                    } else {
                        *arguments = &arguments[1..];
                        default()
                    },
                },
                DocAttribute => Self::DocAttribute {
                    name: argument(arguments, attribute.span, session.reporter())?
                        .text_literal(None, session.reporter())?
                        .clone(),
                },
                DocAttributes => Self::DocAttributes,
                DocReservedIdentifier => Self::DocReservedIdentifier {
                    name: argument(arguments, attribute.span, session.reporter())?
                        .text_literal(None, session.reporter())?
                        .clone(),
                },
                DocReservedIdentifiers => Self::DocReservedIdentifiers,
                Intrinsic => Self::Intrinsic,
                If => {
                    return Err(AttributeParsingError::Erased(
                        Diagnostic::error()
                            .message("the attribute ‘if’ is not supported yet")
                            .primary_span(attribute)
                            .report(session.reporter()),
                    ));
                }
                Ignore => Self::Ignore,
                Include => Self::Include,
                Known => Self::Known,
                Location => {
                    let path = argument(arguments, attribute.span, session.reporter())?
                        .text_literal(Some("path"), session.reporter())?
                        .clone();

                    Self::Location { path }
                }
                Moving => Self::Moving,
                Public => {
                    let reach = optional_argument(arguments)
                        .map(|argument| argument.path(Some("reach"), session.reporter()))
                        .transpose()?
                        .cloned();

                    Self::Public(self::Public { reach })
                }
                RecursionLimit => {
                    let depth = argument(arguments, attribute.span, session.reporter())?;
                    let depth = depth
                        .number_literal(Some("depth"), session.reporter())?
                        .parse::<u32>()
                        .map_err(|_| {
                            AttributeParsingError::Erased(
                                Diagnostic::error()
                                    .code(ErrorCode::E008)
                                    .message(format!(
                                        "attribute argument does not fit integer interval {}",
                                        NAT32_INTERVAL_REPRESENTATION
                                    ))
                                    .primary_span(depth)
                                    .report(session.reporter()),
                            )
                        })?;

                    Self::RecursionLimit { depth }
                }
                Static => Self::Static,
                Statistics => Self::Statistics,
                Test => Self::Test,
                Unsafe => Self::Unsafe,
                Unstable => {
                    return Err(AttributeParsingError::Erased(
                        Diagnostic::error()
                            .message("the attribute ‘unstable’ is not supported yet")
                            .primary_span(attribute)
                            .report(session.reporter()),
                    ));
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
                    .code(ErrorCode::E019)
                    .message("too many attribute arguments provided")
                    .primary_span(argument.span.merge(arguments.last()))
                    .report(session.reporter());
                health.taint();
            }
        }

        result
            .map_err(|error| match error {
                AttributeParsingError::UndefinedAttribute(binder) => Diagnostic::error()
                    .code(ErrorCode::E011)
                    .message(format!("the attribute ‘{binder}’ is not defined"))
                    .primary_span(&binder)
                    .report(session.reporter()),
                AttributeParsingError::Erased(error) => error,
            })
            .and_then(|attributes| Result::ok_if_untainted(attributes, health))
    }
}

enum AttributeParsingError {
    /// Some opaque error that was already reported.
    Erased(ErasedReportedError),
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

        match &self.bare {
            NumberLiteral(literal) => Ok(literal),
            Named(named) => named.handle(
                name,
                |argument| match &argument.bare {
                    NumberLiteral(literal) => Ok(literal),
                    kind => Err(AttributeParsingError::Erased(
                        Diagnostic::invalid_attribute_argument_type(
                            Spanned::new(argument.span, kind.name()),
                            "number literal",
                        )
                        .report(reporter),
                    )),
                },
                reporter,
            ),
            kind => Err(AttributeParsingError::Erased(
                Diagnostic::invalid_attribute_argument_type(
                    Spanned::new(self.span, kind.name()),
                    "positional or named number literal",
                )
                .report(reporter),
            )),
        }
    }

    fn text_literal(
        &self,
        name: Option<&'static str>,
        reporter: &Reporter,
    ) -> Result<&Atom, AttributeParsingError> {
        use ast::AttributeArgumentKind::*;

        match &self.bare {
            TextLiteral(literal) => Ok(literal),
            Named(named) => named.handle(
                name,
                |argument| match &argument.bare {
                    TextLiteral(literal) => Ok(literal),
                    kind => Err(AttributeParsingError::Erased(
                        Diagnostic::invalid_attribute_argument_type(
                            Spanned::new(argument.span, kind.name()),
                            "text literal",
                        )
                        .report(reporter),
                    )),
                },
                reporter,
            ),
            kind => Err(AttributeParsingError::Erased(
                Diagnostic::invalid_attribute_argument_type(
                    Spanned::new(self.span, kind.name()),
                    "positional or named text literal",
                )
                .report(reporter),
            )),
        }
    }

    fn path(
        &self,
        name: Option<&'static str>,
        reporter: &Reporter,
    ) -> Result<&Path, AttributeParsingError> {
        use ast::AttributeArgumentKind::*;

        match &self.bare {
            Path(literal) => Ok(literal),
            Named(named) => named
                .handle(
                    name,
                    |argument| match &argument.bare {
                        Path(literal) => Ok(literal),
                        kind => Err(AttributeParsingError::Erased(
                            Diagnostic::invalid_attribute_argument_type(
                                Spanned::new(argument.span, kind.name()),
                                "path",
                            )
                            .report(reporter),
                        )),
                    },
                    reporter,
                )
                .map(|path| &**path),
            kind => Err(AttributeParsingError::Erased(
                Diagnostic::invalid_attribute_argument_type(
                    Spanned::new(self.span, kind.name()),
                    "positional or named path",
                )
                .report(reporter),
            )),
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
                    Err(AttributeParsingError::Erased(
                        Diagnostic::unexpected_named_attribute_argument(&self.binder, name)
                            .report(reporter),
                    ))
                }
            }
            None => {
                // @Beacon @Beacon @Task span
                Err(AttributeParsingError::Erased(
                    Diagnostic::error()
                        .message("unexpected named attribute argument")
                        .report(reporter),
                ))
            }
        }
    }
}

impl lowered_ast::attributes::Lint {
    fn parse(binder: Path, reporter: &Reporter) -> Result<Self, AttributeParsingError> {
        Err(AttributeParsingError::Erased(
            Diagnostic::error()
                .code(ErrorCode::E018)
                .message(format!("the lint ‘{}’ is not defined", binder))
                .primary_span(binder.span())
                .report(reporter),
        ))
    }
}

impl Diagnostic {
    fn invalid_unnamed_path_hanger(hanger: ast::Hanger) -> Self {
        Self::error()
            .code(ErrorCode::E025)
            .message(format!("path ‘{hanger}’ is not bound to an identifier"))
            .primary_span(&hanger)
            .note("a use-declaration has to introduce at least one new binder")
            .help("bind the path to a name with ‘as’")
    }

    // @Temporary signature
    fn unexpected_named_attribute_argument(
        actual: &ast::Identifier,
        expected: &'static str,
    ) -> Self {
        Self::error()
            .code(ErrorCode::E028)
            .message(format!(
                "found named argument ‘{actual}’ but expected ‘{expected}’"
            ))
            .primary_span(actual)
    }

    // @Temporary signature
    fn invalid_attribute_argument_type(
        actual: Spanned<&'static str>,
        expected: &'static str,
    ) -> Self {
        Self::error()
            .code(ErrorCode::E027)
            .message(format!("found {actual} but expected {expected}"))
            .primary_span(actual)
    }

    fn missing_mandatory_type_annotation(
        spanning: impl Spanning,
        target: AnnotationTarget<'_>,
    ) -> Self {
        use AnnotationTarget::*;

        // @Task change `?type` to `?Type` (to respect Lushui's naming convention) once
        // we successfully parse `?Type` (currently only words are allowed as tags)
        let suggestion: Str = match target {
            Parameter(parameter) => format!(
                "‘{}({}: ?type)’",
                parameter.bare.explicitness, parameter.bare.binder
            )
            .into(),
            Declaration(_) => "‘: ?type’".into(),
        };

        Self::error()
            .code(ErrorCode::E015)
            .message(format!("the {target} does not have a type annotation"))
            .primary_span(spanning)
            .note(format!(
                "type annotations are mandatory on {}",
                match target {
                    Parameter(_) => "parameters of declarations",
                    Declaration(_) => "declarations",
                }
            ))
            .help(format!(
                "provide a type annotation for the {} with {suggestion}",
                target.name()
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
    const fn name(&self) -> &'static str {
        match self {
            Self::Parameter(_) => "parameter",
            Self::Declaration(_) => "declaration",
        }
    }
}

impl fmt::Display for AnnotationTarget<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ", self.name())?;

        match self {
            Self::Parameter(parameter) => write!(f, "‘{}’", parameter.bare.binder),
            Self::Declaration(binder) => write!(f, "‘{binder}’"),
        }
    }
}
