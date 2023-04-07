//! The lowering stage (AST simplification and early validation).
//!
//! Higher-level syntactic constructs in the AST are simplified and rewritten
//! in terms of lower-level language primitives and undergo several checks since
//! they might be more liberal than the intermediate languages allow.
//!
//! This pass does the following:
//!
//! * lower let-bindings to lambda literals
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
#![feature(decl_macro, default_free_fn, slice_take, min_specialization)]

// @Task ungate named arguments but validate them in the resolver (and/or typer)

use ast::{BareHanger, Explicit, Parameter, Path};
use diagnostics::{
    error::{Handler, Health, Outcome, PossiblyErroneous, Result},
    reporter::ErasedReportedError,
    Diagnostic, ErrorCode, Reporter, Substitution,
};
use lowered_ast::{
    attribute::{Predicate, Public, Query, Special, Target},
    AttributeName, Attributes, BareAttribute,
};
use session::Session;
use span::{Span, Spanned, Spanning};
use std::{default::default, fmt, iter::once};
use utilities::{
    smallvec, Atom, Conjunction, FormatError, ListingExt, QuoteExt, SmallVec, FILE_EXTENSION,
};

/// Lower a file.
pub fn lower_file(
    declaration: ast::Declaration,
    options: Options,
    session: &Session<'_>,
) -> Result<lowered_ast::Declaration> {
    let mut lowerer = Lowerer::new(options, session);
    let mut declaration = lowerer.lower_declaration(declaration);
    let root = declaration.pop().unwrap();
    Outcome::new(root, lowerer.health).into()
}

/// The state of the lowering pass.
struct Lowerer<'a> {
    options: Options,
    session: &'a Session<'a>,
    health: Health,
}

impl<'a> Lowerer<'a> {
    fn new(options: Options, session: &'a Session<'a>) -> Self {
        Self {
            options,
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
        use ast::BareDeclaration::*;

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
        let type_ = match function.type_ {
            Some(type_) => type_,
            None => missing_mandatory_type_annotation_error(
                function.binder.span().fit_end(&function.parameters).end(),
                TypeAnnotationTarget::Declaration(function.binder),
            )
            .handle(&mut *self),
        };

        // @Note `type_` is currently lowered twice @Task remove duplicate work
        let body = match function.body {
            Some(body) => {
                let mut body = self.lower_expression(body);

                {
                    let mut type_ = once(self.lower_expression(type_.clone()));

                    for parameter in function.parameters.iter().rev() {
                        let parameter_type = match &parameter.bare.type_ {
                            Some(type_) => type_.clone(),
                            None => missing_mandatory_type_annotation_error(
                                parameter,
                                TypeAnnotationTarget::Parameter(parameter),
                            )
                            .handle(&mut *self),
                        };

                        let parameter_type = self.lower_expression(parameter_type);

                        body = lowered_ast::Expression::new(
                            default(),
                            default(),
                            lowered_ast::Lambda {
                                parameter: parameter.bare.binder,
                                domain: Some(parameter_type.clone()),
                                explicitness: parameter.bare.explicitness,
                                codomain: type_.next(),
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

        let type_ = self.lower_parameters_to_annotated_ones(function.parameters, type_);

        lowered_ast::Function {
            binder: function.binder,
            type_,
            expression: body,
        }
    }

    fn lower_data_declaration(&mut self, data_type: ast::Data) -> lowered_ast::Data {
        let type_ = match data_type.type_ {
            Some(type_) => type_,
            None => missing_mandatory_type_annotation_error(
                data_type.binder.span().fit_end(&data_type.parameters).end(),
                TypeAnnotationTarget::Declaration(data_type.binder),
            )
            .handle(&mut *self),
        };

        let type_ = self.lower_parameters_to_annotated_ones(data_type.parameters, type_);

        let constructors = data_type.constructors.map(|constructors| {
            constructors
                .into_iter()
                .flat_map(|constructor| self.lower_declaration(constructor))
                .collect()
        });

        lowered_ast::Data {
            binder: data_type.binder,
            type_,
            constructors,
        }
    }

    fn lower_constructor_declaration(
        &mut self,
        constructor: ast::Constructor,
    ) -> lowered_ast::Constructor {
        let type_ = match constructor.type_ {
            Some(type_) => type_,
            None => missing_mandatory_type_annotation_error(
                constructor
                    .binder
                    .span()
                    .fit_end(&constructor.parameters)
                    .end(),
                TypeAnnotationTarget::Declaration(constructor.binder),
            )
            .handle(&mut *self),
        };

        let type_ = self.lower_parameters_to_annotated_ones(constructor.parameters, type_);

        if let Some(body) = constructor.body {
            let body = self.lower_expression(body);

            // @Task improve the labels etc, don't say "conflicting definition"
            // it's technically true, but we can do so much better!
            let _: ErasedReportedError = Diagnostic::error()
                .code(ErrorCode::E020)
                .message(format!(
                    "‘{}’ is defined multiple times in this scope",
                    constructor.binder
                ))
                .span(&body, "conflicting definition")
                .note(
                    "the body of the constructor is implied but it also has a body introduced by ‘= ?value’",
                ).handle(&mut *self);
        }

        lowered_ast::Constructor {
            binder: constructor.binder,
            type_,
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
                    .name()
                    .path()
                    .unwrap()
                    .as_path()
                    .parent()
                    .unwrap()
                    .to_owned();

                match attributes.get::<{ AttributeName::Location }>() {
                    Some(location) => {
                        let mut inline_modules = &context.inline_modules[..];
                        _ = inline_modules.take_last();

                        path.extend(inline_modules);
                        path.push(location.bare.to_str());
                    }
                    None => {
                        path.extend(&context.inline_modules);
                        path.push(module.binder.to_str());
                    }
                };

                path.set_extension(FILE_EXTENSION);

                // @Task create & use a different API that doesn't "recanonicalize" the path
                let file = self
                    .session
                    .map()
                    .load(&path, Some(self.session.component().index()));
                let file = match file {
                    Ok(file) => file,
                    Err(error) => {
                        // @Task instead of a note saying the error, print a help message
                        // saying to create the missing file or change the access rights etc.
                        return Diagnostic::error()
                            .code(ErrorCode::E016)
                            .message(format!("could not load the module ‘{}’", module.binder))
                            .path(path)
                            .unlabeled_span(span)
                            .note(error.format())
                            .handle(&mut *self);
                    }
                };

                let declaration = match syntax::parse_module_file(file, module.binder, self.session)
                {
                    Ok(declaration) => declaration,
                    Err(error) => {
                        self.health.taint(error);
                        return PossiblyErroneous::error(error);
                    }
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
            if declaration.bare == ast::BareDeclaration::ModuleHeader {
                if index == 0 {
                    // @Bug this sequence may lead to some unnecessary diagnostics being emitted
                    // since the "synergy check" (which filters duplicate attribute) is run too late

                    let module_header_attributes =
                        self.lower_attributes(&declaration.attributes, &declaration);
                    attributes.0.extend(module_header_attributes.0);
                    attributes = self.check_attribute_synergy(attributes);
                } else {
                    let _: ErasedReportedError = Diagnostic::error()
                        .code(ErrorCode::E041)
                        .message("the module header has to be the first declaration of the module")
                        .unlabeled_span(&declaration)
                        .with(|it| {
                            if has_header {
                                // @Task make this a note with a span/highlight!
                                it.note("however, the current module already has a module header")
                            } else {
                                it
                            }
                        })
                        .handle(&mut *self);
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

    // @Task verify that the resulting spans are correct
    fn lower_use_declaration(
        &mut self,
        use_: ast::Use,
        attributes: Attributes,
        span: Span,
    ) -> SmallVec<lowered_ast::Declaration, 1> {
        let mut declarations = SmallVec::new();

        'discriminate: {
            match use_.bindings.bare {
                ast::BareUsePathTree::Single { target, binder } => {
                    let binder = binder.or_else(|| target.segments.last().copied());
                    let Some(binder) = binder else {
                                // @Task improve the message for `use topmost.(self)`: hint that `self`
                                // is effectively unnamed because `topmost` is unnamed
                                // @Task the message is even worse (it is misleading!) with `use extern.(self)`
                                // currently leads to the suggestion to bind `self` to an identifier but
                                // for `extern` that is invalid, too
                                let _: ErasedReportedError = invalid_unnamed_path_hanger_error(target.hanger.unwrap())
                                    .handle(&mut *self);
                                break 'discriminate;
                            };

                    declarations.push(lowered_ast::Declaration::new(
                        attributes,
                        span,
                        lowered_ast::Use { binder, target }.into(),
                    ));
                }
                ast::BareUsePathTree::Multiple { path, bindings } => {
                    self.lower_use_path_tree(path, bindings, span, attributes, &mut declarations);
                }
            }
        }

        declarations
    }

    fn lower_use_path_tree(
        &mut self,
        path: Path,
        bindings: Vec<ast::UsePathTree>,
        span: Span,
        attributes: Attributes,
        declarations: &mut SmallVec<lowered_ast::Declaration, 1>,
    ) {
        for binding in bindings {
            match binding.bare {
                ast::BareUsePathTree::Single { target, binder } => {
                    let combined_target = match path.clone().join(target.clone()) {
                        Ok(target) => target,
                        Err(hanger) => {
                            let _: ErasedReportedError =
                                incorrectly_positioned_path_hanger_error(hanger).handle(&mut *self);
                            continue;
                        }
                    };

                    // if the binder is not explicitly set, look for the most-specific/last/right-most
                    // identifier of the target but if that one is `self`, look up the last identifier of
                    // the parent path
                    let Some(binder) = binder.or_else(|| {
                        if target.is_bare_hanger(BareHanger::Self_) {
                            &path
                        } else {
                            &target
                        }
                        .segments
                        .last()
                        .copied()
                    }) else {
                        // @Task improve the message for `use topmost.(self)`: hint that `self`
                        // is effectively unnamed because `topmost` is unnamed
                        let _: ErasedReportedError = invalid_unnamed_path_hanger_error(target.hanger.unwrap())
                                .handle(&mut *self);
                        continue
                    };

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
                ast::BareUsePathTree::Multiple {
                    path: inner_path,
                    bindings,
                } => {
                    let path = match path.clone().join(inner_path) {
                        Ok(path) => path,
                        Err(hanger) => {
                            let _: ErasedReportedError =
                                incorrectly_positioned_path_hanger_error(hanger).handle(&mut *self);
                            continue;
                        }
                    };

                    self.lower_use_path_tree(
                        path,
                        bindings,
                        span,
                        attributes.clone(),
                        declarations,
                    );
                }
            }
        }
    }

    /// Lower an expression.
    fn lower_expression(&mut self, expression: ast::Expression) -> lowered_ast::Expression {
        use ast::BareExpression::*;

        let attributes = self.lower_attributes(&expression.attributes, &expression);

        match expression.bare {
            QuantifiedType(type_) => match type_.quantifier {
                ast::Quantifier::Pi => {
                    // @Task transfer expression.attributes to lowered form in some way or the other.
                    // Not clear yet whether I should duplicate across all lowered pi types or do sth. else.

                    let mut codomain = self.lower_expression(type_.codomain);

                    for parameter in type_.parameters.into_iter().rev() {
                        let domain = match parameter.bare.type_ {
                            Some(type_) => type_,
                            None => missing_mandatory_type_annotation_error(
                                &parameter,
                                TypeAnnotationTarget::Parameter(&parameter),
                            )
                            .handle(&mut *self),
                        };
                        let domain = self.lower_expression(domain);

                        // @Temporary hack until we properly support discards
                        let binder =
                            Some(parameter.bare.binder).filter(|binder| binder.bare() != Atom::__);

                        codomain = lowered_ast::Expression::new(
                            default(),
                            expression.span,
                            lowered_ast::PiType {
                                explicitness: parameter.bare.explicitness,
                                binder,
                                domain,
                                codomain,
                            }
                            .into(),
                        );
                    }

                    codomain
                }
                // @Beacon @Task add UI test for this
                ast::Quantifier::Sigma => Diagnostic::error()
                    .message("sigma-type expressions are not supported yet")
                    .unlabeled_span(expression.span)
                    .handle(&mut *self),
            },
            Application(application) => {
                if let Some(binder) = &application.binder {
                    let _: ErasedReportedError = Diagnostic::error()
                        .message("named arguments are not supported yet")
                        .unlabeled_span(binder)
                        .handle(&mut *self);
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
            // @Task avoid re-boxing!
            NumberLiteral(number) => {
                lowered_ast::Expression::new(attributes, expression.span, (*number).into())
            }
            // @Task avoid re-boxing!
            TextLiteral(text) => {
                lowered_ast::Expression::new(attributes, expression.span, (*text).into())
            }
            TypedHole(_hole) => Diagnostic::error()
                .message("typed holes are not supported yet")
                .unlabeled_span(expression.span)
                .handle(&mut *self),
            // @Task avoid re-boxing!
            Path(path) => lowered_ast::Expression::new(attributes, expression.span, (*path).into()),
            Projection(_projection) => Diagnostic::error()
                .message("record field projections are not supported yet")
                .unlabeled_span(expression.span)
                .handle(&mut *self),
            LambdaLiteral(lambda) => {
                // @Task transfer expression.attributes to lowered form in some way or the other.
                // Not clear yet whether I should duplicate across all lowered lambdas or do sth. else.

                let mut body = self.lower_expression(lambda.body);

                let mut codomain = lambda
                    .codomain
                    .map(|type_| self.lower_expression(type_))
                    .into_iter();

                for parameter in lambda.parameters.into_iter().rev() {
                    let parameter_type = parameter
                        .bare
                        .type_
                        .map(|type_| self.lower_expression(type_));

                    body = lowered_ast::Expression::new(
                        default(),
                        expression.span,
                        lowered_ast::Lambda {
                            parameter: parameter.bare.binder,
                            domain: parameter_type,
                            explicitness: parameter.bare.explicitness,
                            codomain: codomain.next(),
                            body,
                        }
                        .into(),
                    );
                }

                body
            }
            LetBinding(binding) => {
                let body = {
                    let binder = &binding.binder;

                    match binding.body {
                        Some(body) => body,
                        None => {
                            let span = binder
                                .span()
                                .fit_end(&binding.parameters)
                                .fit_end(&binding.type_)
                                .end();

                            Diagnostic::error()
                                .code(ErrorCode::E012)
                                .message(format!("the let-binding ‘{binder}’ does not have a body"))
                                .span(span, "missing definition")
                                .suggest(
                                    span,
                                    "provide a definition for the let-binding",
                                    Substitution::from(" = ").placeholder("value"),
                                )
                                .handle(&mut *self)
                        }
                    }
                };
                let mut body = self.lower_expression(body);

                let mut type_ = binding
                    .type_
                    .map(|type_| self.lower_expression(type_))
                    .into_iter();

                for parameter in binding.parameters.into_iter().rev() {
                    let domain = parameter
                        .bare
                        .type_
                        .map(|type_| self.lower_expression(type_));

                    body = lowered_ast::Expression::new(
                        default(),
                        default(),
                        lowered_ast::Lambda {
                            parameter: parameter.bare.binder,
                            domain,
                            explicitness: parameter.bare.explicitness,
                            codomain: type_.next(),
                            body,
                        }
                        .into(),
                    );
                }

                let scope = self.lower_expression(binding.scope);

                // @Task transfer the attributes & span from the let-binding to the lambda(s)

                lowered_ast::Expression::new(
                    default(),
                    default(),
                    lowered_ast::Application {
                        callee: lowered_ast::Expression::new(
                            default(),
                            default(),
                            lowered_ast::Lambda {
                                parameter: binding.binder,
                                // @Note we cannot simply lower parameters and a type annotation because
                                // in the chain (`->`) of parameters, there might always be one missing and
                                // we don't support partial type annotations yet (using `_`)
                                // @Temporary @Update @Bug -gy because we ignore above message
                                // @Task verify correct semantics
                                domain: type_.next(),
                                explicitness: Explicit,
                                codomain: None,
                                body: scope,
                            }
                            .into(),
                        ),
                        argument: body,
                        explicitness: Explicit,
                    }
                    .into(),
                )
            }
            UseBinding(_binding) => Diagnostic::error()
                .message("use-bindings are not supported yet")
                .unlabeled_span(expression.span)
                .handle(&mut *self),
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
            DoBlock(_block) => Diagnostic::error()
                .message("do blocks are not supported yet")
                .unlabeled_span(expression.span)
                .handle(&mut *self),
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
            Error(error) => PossiblyErroneous::error(error),
        }
    }

    /// Lower a pattern.
    fn lower_pattern(&mut self, pattern: ast::Pattern) -> lowered_ast::Pattern {
        use ast::BarePattern::*;

        let attributes = self.lower_attributes(&pattern.attributes, &pattern);

        match pattern.bare {
            NumberLiteral(literal) => {
                lowered_ast::Pattern::new(attributes, pattern.span, (*literal).into())
            }
            TextLiteral(literal) => {
                lowered_ast::Pattern::new(attributes, pattern.span, (*literal).into())
            }
            Path(path) => lowered_ast::Pattern::new(attributes, pattern.span, (*path).into()),
            Binder(binder) => lowered_ast::Pattern::new(attributes, pattern.span, (*binder).into()),
            Application(application) => {
                if let Some(binder) = &application.binder {
                    let _: ErasedReportedError = Diagnostic::error()
                        .message("named arguments are not supported yet")
                        .unlabeled_span(binder)
                        .handle(&mut *self);
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
        target: &impl TargetExt,
    ) -> Attributes {
        use lowered_ast::Attribute;

        let actual_targets = target.as_targets();
        let mut conforming_attributes = Attributes::default();

        for attribute in unchecked_attributes {
            let attribute = match Attribute::parse(attribute, &self.options, self.session) {
                Ok(attribute) => attribute,
                Err(error) => {
                    self.health.taint(error);
                    continue;
                }
            };

            // search for non-conforming attributes
            {
                let expected_targets = attribute.bare.targets();
                if !expected_targets.contains(actual_targets) {
                    // @Question wording: "cannot be ascribed to"?
                    let _: ErasedReportedError = Diagnostic::error()
                        .code(ErrorCode::E013)
                        .message(format!(
                            "attribute ‘{}’ is ascribed to {}",
                            attribute.bare.name(),
                            target.name()
                        ))
                        .span(&attribute, "misplaced attribute")
                        .label(target, "incompatible item")
                        .note(format!(
                            "attribute ‘{}’ can only be ascribed to {}",
                            attribute.bare.name(),
                            expected_targets.description(),
                        ))
                        .handle(&mut *self);
                    continue;
                }
            }

            conforming_attributes.0.push(attribute);
        }

        let attributes = self.check_attribute_synergy(conforming_attributes);
        target.check_attributes(&attributes, self);
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

            if !attributes.has(is_homonymous) {
                attributes.0.push(attribute.clone());
            }

            if let [first, _second, ..] = &*homonymous_attributes {
                let _: ErasedReportedError = Diagnostic::error()
                    .code(ErrorCode::E006)
                    .message(format!("multiple ‘{}’ attributes", first.bare.name()))
                    .spans(homonymous_attributes, "duplicate or conflicting attribute")
                    .handle(&mut *self);
            }
        }

        // no further checks necessary if empty
        if attributes.0.is_empty() {
            return attributes;
        }

        {
            use AttributeName::*;
            self.check_mutual_exclusivity(Intrinsic.or(Known), &attributes);
            self.check_mutual_exclusivity(Moving.or(Abstract), &attributes);
        }

        for attribute in attributes.filter(Predicate(|attribute| !attribute.is_implemented())) {
            let _: ErasedReportedError = Diagnostic::error()
                .message(format!(
                    "the attribute ‘{}’ is not supported yet",
                    attribute.bare.name()
                ))
                .unlabeled_span(attribute)
                .handle(&mut *self);
        }

        // @Task replace this concept with a feature system
        if !self.options.internal_features_enabled {
            for attribute in attributes.filter(Predicate(BareAttribute::is_internal)) {
                let _: ErasedReportedError = Diagnostic::error()
                    .code(ErrorCode::E038)
                    .message(format!(
                        "the attribute ‘{}’ is an internal feature",
                        attribute.bare.name()
                    ))
                    .unlabeled_span(attribute)
                    .handle(&mut *self);
            }
        }

        attributes
    }

    fn check_mutual_exclusivity<Q: Query>(&mut self, query: Q, attributes: &Attributes) {
        let attributes = attributes.filter(query).collect::<Vec<_>>();

        if attributes.len() > 1 {
            let listing = attributes
                .iter()
                .map(|attribute| attribute.bare.name().to_str().quote())
                .list(Conjunction::And);

            let _: ErasedReportedError = Diagnostic::error()
                .code(ErrorCode::E014)
                .message(format!("attributes {listing} are mutually exclusive"))
                .spans(attributes, "conflicting attribute")
                .handle(self);
        }
    }

    /// Lower annotated parameters.
    fn lower_parameters_to_annotated_ones(
        &mut self,
        parameters: ast::Parameters,
        type_: ast::Expression,
    ) -> lowered_ast::Expression {
        let mut type_ = self.lower_expression(type_);

        for parameter in parameters.into_iter().rev() {
            let parameter_type = match parameter.bare.type_ {
                Some(type_) => type_,
                None => missing_mandatory_type_annotation_error(
                    &parameter,
                    TypeAnnotationTarget::Parameter(&parameter),
                )
                .handle(&mut *self),
            };

            let parameter_type = self.lower_expression(parameter_type);

            type_ = lowered_ast::Expression::new(
                default(),
                default(),
                lowered_ast::PiType {
                    explicitness: parameter.bare.explicitness,
                    binder: Some(parameter.bare.binder),
                    domain: parameter_type.clone(),
                    codomain: type_,
                }
                .into(),
            );
        }

        type_
    }
}

impl Handler for &mut Lowerer<'_> {
    fn handle<T: PossiblyErroneous>(self, diagnostic: Diagnostic) -> T {
        let error = diagnostic.report(self.session.reporter());
        self.health.taint(error);
        T::error(error)
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

trait TargetExt: Target {
    fn check_attributes(&self, attributes: &Attributes, lowerer: &mut Lowerer<'_>);
}

impl<T: Target> TargetExt for T {
    default fn check_attributes(&self, _: &Attributes, _: &mut Lowerer<'_>) {}
}

impl TargetExt for ast::Declaration {
    fn check_attributes(&self, attributes: &Attributes, lowerer: &mut Lowerer<'_>) {
        use ast::BareDeclaration::*;

        let (binder, missing_definition_span, substitution, body) = match &self.bare {
            Function(function) => {
                let missing_definition_span = function
                    .binder
                    .span()
                    .fit_end(&function.parameters)
                    .fit_end(&function.type_)
                    .end();

                (
                    &function.binder,
                    missing_definition_span,
                    Substitution::from(" = ").placeholder("value"),
                    function.body.as_ref().map(|expression| {
                        let eq = missing_definition_span
                            .between(self.span.end())
                            .trim_start_matches(
                                |character| character.is_ascii_whitespace(),
                                &lowerer.session.shared_map(),
                            );

                        (
                            eq.merge(expression),
                            "the body conflicting with the attribute",
                        )
                    }),
                )
            }
            Data(type_) => {
                let missing_definition_span = type_
                    .binder
                    .span()
                    .fit_end(&type_.parameters)
                    .fit_end(&type_.type_)
                    .end();

                (
                    &type_.binder,
                    missing_definition_span,
                    // @Bug the placeholder does not really make sense
                    // @Task use a multi-line suggestion once we support that
                    Substitution::from(" of ").placeholder("…"),
                    type_.constructors.as_ref().map(|constructors| {
                        let of = missing_definition_span
                            .between(self.span.end())
                            .trim_start_matches(
                                |character| character.is_ascii_whitespace(),
                                &lowerer.session.shared_map(),
                            );

                        (
                            // @Bug span does not include trailing closing curly bracket
                            // @Task define & use Span::expand_end_matches to recover it
                            of.merge(constructors),
                            if constructors.is_empty() {
                                "\
the body specifying that the data type has no constructors and is therefore uninhabited
         conflicting with the attribute"
                            } else {
                                "\
the body containing a set of constructors
         conflicting with the attribute"
                            },
                        )
                    }),
                )
            }
            _ => return,
        };

        let _: ErasedReportedError = match (body, attributes.span(AttributeName::Intrinsic)) {
            (Some((body_span, body_label)), Some(intrinsic)) => Diagnostic::error()
                .code(ErrorCode::E042)
                .message(format!(
                    "the declaration ‘{binder}’ marked as ‘intrinsic’ has a body",
                ))
                .span(body_span, body_label)
                .label(
                    intrinsic,
                    "marks the declaration as being defined outside of the language",
                )
                .help("remove either the body or the attribute")
                .handle(lowerer),
            (None, None) => Diagnostic::error()
                .code(ErrorCode::E012)
                .message(format!("the declaration ‘{binder}’ does not have a body"))
                .span(missing_definition_span, "missing definition")
                .suggest(
                    missing_definition_span,
                    "provide a definition for the declaration",
                    substitution,
                )
                .handle(lowerer),
            _ => return,
        };
    }
}

trait AttributeExt: Sized {
    fn parse(attribute: &ast::Attribute, options: &Options, session: &Session<'_>) -> Result<Self>;
}

impl AttributeExt for lowered_ast::Attribute {
    fn parse(attribute: &ast::Attribute, options: &Options, session: &Session<'_>) -> Result<Self> {
        Ok(Self::new(
            attribute.span,
            BareAttribute::parse(attribute, options, session)?,
        ))
    }
}

trait BareAttributeExt: Sized {
    fn parse(attribute: &ast::Attribute, options: &Options, session: &Session<'_>) -> Result<Self>;
}

impl BareAttributeExt for lowered_ast::BareAttribute {
    // @Task allow unordered named attributes e.g. `@(unstable (reason "x") (feature thing))`
    fn parse(
        // @Task take by value and create parsing helpers on ast::Attribute and ast::Attributes
        attribute: &ast::Attribute,
        options: &Options,
        session: &Session<'_>,
    ) -> Result<Self> {
        let ast::BareAttribute::Regular { binder, arguments } = &attribute.bare else {
            return Ok(Self::Doc {
                content: if options.keep_documentation_comments {
                    session.shared_map().snippet(attribute.span)
                        .trim_start_matches(";;")
                        .to_owned().into()
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
                        .unlabeled_span(span)
                        .report(reporter),
                )
            })?;
            *arguments = &arguments[1..];
            Ok(argument)
        }

        let result = (|| {
            use AttributeName::*;

            let name = AttributeName::parse(binder.bare())
                .ok_or(AttributeParsingError::UndefinedAttribute(*binder))?;

            Ok(match name {
                Abstract => Self::Abstract,
                Allow | Deny | Forbid | Warn => {
                    let lint = lowered_ast::attribute::Lint::parse(
                        argument(arguments, attribute.span, session.reporter())?
                            .path(Some(Atom::lint), session.reporter())?
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
                Deprecated => Self::Deprecated(lowered_ast::attribute::Deprecated {
                    reason: optional_argument(arguments)
                        .map(|argument| {
                            argument.text_literal(Some(Atom::reason), session.reporter())
                        })
                        .transpose()?,
                    // @Task parse version
                    since: None,
                    // @Task parse version
                    removal: None,
                    replacement: optional_argument(arguments)
                        .map(|argument| {
                            argument.text_literal(Some(Atom::replacement), session.reporter())
                        })
                        .transpose()?,
                }),
                Doc => Self::Doc {
                    content: if options.keep_documentation_comments {
                        argument(arguments, attribute.span, session.reporter())?
                            .text_literal(None, session.reporter())?
                            .to_str()
                            .into()
                    } else {
                        *arguments = &arguments[1..];
                        default()
                    },
                },
                Intrinsic => {
                    let name = optional_argument(arguments)
                        .map(|argument| argument.path(Some(Atom::name), session.reporter()))
                        .transpose()?
                        .cloned();

                    Self::Intrinsic(Special { name })
                }
                If => {
                    return Err(AttributeParsingError::Erased(
                        Diagnostic::error()
                            .message("the attribute ‘if’ is not supported yet")
                            .unlabeled_span(attribute)
                            .report(session.reporter()),
                    ));
                }
                Ignore => Self::Ignore,
                Include => Self::Include,
                Known => {
                    let name = optional_argument(arguments)
                        .map(|argument| argument.path(Some(Atom::name), session.reporter()))
                        .transpose()?
                        .cloned();

                    Self::Known(Special { name })
                }
                Location => {
                    let path = argument(arguments, attribute.span, session.reporter())?
                        .text_literal(Some(Atom::path), session.reporter())?;

                    Self::Location { path }
                }
                Moving => Self::Moving,
                Public => {
                    let reach = optional_argument(arguments)
                        .map(|argument| argument.path(Some(Atom::reach), session.reporter()))
                        .transpose()?
                        .cloned();

                    Self::Public(self::Public { reach })
                }
                RecursionLimit => {
                    let depth = argument(arguments, attribute.span, session.reporter())?;
                    let depth = depth
                        .number_literal(Some(Atom::depth), session.reporter())?
                        .to_str()
                        .parse::<u32>()
                        .map_err(|_| {
                            AttributeParsingError::Erased(
                                Diagnostic::error()
                                    .code(ErrorCode::E008)
                                    .message(format!(
                                        "attribute argument does not fit integer interval \
                                        {NAT32_INTERVAL_REPRESENTATION}",
                                    ))
                                    .unlabeled_span(depth)
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
                            .unlabeled_span(attribute)
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
                let error = Diagnostic::error()
                    .code(ErrorCode::E019)
                    .message("too many attribute arguments provided")
                    .unlabeled_span(argument.span.merge(arguments.last()))
                    .report(session.reporter());
                health.taint(error);
            }
        }

        result
            .map_err(|error| match error {
                AttributeParsingError::UndefinedAttribute(binder) => Diagnostic::error()
                    .code(ErrorCode::E011)
                    .message(format!("the attribute ‘{binder}’ is not defined"))
                    .unlabeled_span(binder)
                    .report(session.reporter()),
                AttributeParsingError::Erased(error) => error,
            })
            .and_then(|attributes| Outcome::new(attributes, health).into())
    }
}

enum AttributeParsingError {
    /// Some opaque error that was already reported.
    Erased(ErasedReportedError),
    UndefinedAttribute(ast::Identifier),
}

// @Beacon hideous accessors! @Task deduplicate!
// @Beacon @Task don't use reporter+Result<_> here but
// a custom error type (w/o ::Unrecoverable)
// and turn that stuff into stuff later

trait AttributeArgumentExt {
    fn number_literal(
        &self,
        name: Option<Atom>,
        reporter: &Reporter,
    ) -> Result<Atom, AttributeParsingError>;

    fn text_literal(
        &self,
        name: Option<Atom>,
        reporter: &Reporter,
    ) -> Result<Atom, AttributeParsingError>;

    fn path(&self, name: Option<Atom>, reporter: &Reporter)
        -> Result<&Path, AttributeParsingError>;
}

impl AttributeArgumentExt for ast::AttributeArgument {
    fn number_literal(
        &self,
        name: Option<Atom>,
        reporter: &Reporter,
    ) -> Result<Atom, AttributeParsingError> {
        use ast::BareAttributeArgument::*;

        match &self.bare {
            &NumberLiteral(literal) => Ok(literal),
            Named(named) => named
                .handle(
                    name,
                    |argument| match &argument.bare {
                        NumberLiteral(literal) => Ok(literal),
                        bare => Err(AttributeParsingError::Erased(
                            invalid_attribute_argument_type_error(
                                Spanned::new(argument.span, bare.name()),
                                "number literal",
                            )
                            .report(reporter),
                        )),
                    },
                    reporter,
                )
                .copied(),
            bare => Err(AttributeParsingError::Erased(
                invalid_attribute_argument_type_error(
                    Spanned::new(self.span, bare.name()),
                    "positional or named number literal",
                )
                .report(reporter),
            )),
        }
    }

    fn text_literal(
        &self,
        name: Option<Atom>,
        reporter: &Reporter,
    ) -> Result<Atom, AttributeParsingError> {
        use ast::BareAttributeArgument::*;

        match &self.bare {
            &TextLiteral(literal) => Ok(literal),
            Named(named) => named
                .handle(
                    name,
                    |argument| match &argument.bare {
                        TextLiteral(literal) => Ok(literal),
                        bare => Err(AttributeParsingError::Erased(
                            invalid_attribute_argument_type_error(
                                Spanned::new(argument.span, bare.name()),
                                "text literal",
                            )
                            .report(reporter),
                        )),
                    },
                    reporter,
                )
                .copied(),
            bare => Err(AttributeParsingError::Erased(
                invalid_attribute_argument_type_error(
                    Spanned::new(self.span, bare.name()),
                    "positional or named text literal",
                )
                .report(reporter),
            )),
        }
    }

    fn path(
        &self,
        name: Option<Atom>,
        reporter: &Reporter,
    ) -> Result<&Path, AttributeParsingError> {
        use ast::BareAttributeArgument::*;

        match &self.bare {
            Path(literal) => Ok(literal),
            Named(named) => named
                .handle(
                    name,
                    |argument| match &argument.bare {
                        Path(literal) => Ok(literal),
                        bare => Err(AttributeParsingError::Erased(
                            invalid_attribute_argument_type_error(
                                Spanned::new(argument.span, bare.name()),
                                "path",
                            )
                            .report(reporter),
                        )),
                    },
                    reporter,
                )
                .map(|path| &**path),
            bare => Err(AttributeParsingError::Erased(
                invalid_attribute_argument_type_error(
                    Spanned::new(self.span, bare.name()),
                    "positional or named path",
                )
                .report(reporter),
            )),
        }
    }
}

trait NamedAttributeArgumentExt {
    fn handle<T>(
        &self,
        name: Option<Atom>,
        handle: impl FnOnce(&ast::AttributeArgument) -> Result<&T, AttributeParsingError>,
        reporter: &Reporter,
    ) -> Result<&T, AttributeParsingError>;
}

impl NamedAttributeArgumentExt for ast::NamedAttributeArgument {
    fn handle<T>(
        &self,
        name: Option<Atom>,
        handle: impl FnOnce(&ast::AttributeArgument) -> Result<&T, AttributeParsingError>,
        reporter: &Reporter,
    ) -> Result<&T, AttributeParsingError> {
        match name {
            Some(name) => {
                if self.binder.bare() == name {
                    handle(&self.value)
                } else {
                    Err(AttributeParsingError::Erased(
                        unexpected_named_attribute_argument_error(self.binder, name)
                            .report(reporter),
                    ))
                }
            }
            None => {
                // @Beacon @Task span
                Err(AttributeParsingError::Erased(
                    Diagnostic::error()
                        .message("unexpected named attribute argument")
                        .report(reporter),
                ))
            }
        }
    }
}

trait LintExt: Sized {
    fn parse(binder: Path, reporter: &Reporter) -> Result<Self, AttributeParsingError>;
}

impl LintExt for lowered_ast::attribute::Lint {
    fn parse(binder: Path, reporter: &Reporter) -> Result<Self, AttributeParsingError> {
        Err(AttributeParsingError::Erased(
            Diagnostic::error()
                .code(ErrorCode::E018)
                .message(format!("the lint ‘{binder}’ is not defined"))
                .unlabeled_span(binder.span())
                .report(reporter),
        ))
    }
}

fn invalid_unnamed_path_hanger_error(hanger: ast::Hanger) -> Diagnostic {
    Diagnostic::error()
        .code(ErrorCode::E025)
        .message(format!("path ‘{hanger}’ is not bound to an identifier"))
        .unlabeled_span(hanger)
        .note("a use-declaration has to introduce at least one new binder")
        .help("bind the path to a name with ‘as’")
}

fn incorrectly_positioned_path_hanger_error(hanger: ast::Hanger) -> Diagnostic {
    Diagnostic::error()
        .code(ErrorCode::E026)
        .message(format!("path ‘{hanger}’ not allowed in this position"))
        .unlabeled_span(hanger)
        .help("consider moving this path to a separate use-declaration")
}

// @Temporary signature
fn unexpected_named_attribute_argument_error(
    actual: ast::Identifier,
    expected: Atom,
) -> Diagnostic {
    Diagnostic::error()
        .code(ErrorCode::E028)
        .message(format!(
            "found named argument ‘{actual}’ but expected ‘{expected}’"
        ))
        .unlabeled_span(actual)
}

// @Temporary signature
fn invalid_attribute_argument_type_error(
    actual: Spanned<&'static str>,
    expected: &'static str,
) -> Diagnostic {
    Diagnostic::error()
        .code(ErrorCode::E027)
        .message(format!("found {actual} but expected {expected}"))
        .unlabeled_span(actual)
}

fn missing_mandatory_type_annotation_error(
    spanning: impl Spanning,
    target: TypeAnnotationTarget<'_>,
) -> Diagnostic {
    let substitution = match target {
        TypeAnnotationTarget::Parameter(parameter) => {
            let explicitness = parameter.bare.explicitness;
            let binder = &parameter.bare.binder;

            Substitution::from(format!("{explicitness}({binder}: "))
                .placeholder("Type")
                .str(")")
        }
        TypeAnnotationTarget::Declaration(_) => Substitution::from(": ").placeholder("Type"),
    };

    let span = spanning.span();

    Diagnostic::error()
        .code(ErrorCode::E015)
        .message(format!("the {target} does not have a type annotation"))
        .span(span, "missing mandatory type annotation")
        .suggest(
            span,
            format!("annotate the {} with a type", target.name()),
            substitution,
        )
}

/// A place in the AST which can have a syntactically optional type annotation.
///
/// Exclusively used for error reporting.
enum TypeAnnotationTarget<'a> {
    Parameter(&'a Parameter),
    Declaration(ast::Identifier),
}

impl TypeAnnotationTarget<'_> {
    const fn name(&self) -> &'static str {
        match self {
            Self::Parameter(_) => "parameter",
            Self::Declaration(_) => "declaration",
        }
    }
}

impl fmt::Display for TypeAnnotationTarget<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ", self.name())?;

        match self {
            Self::Parameter(parameter) => write!(f, "‘{}’", parameter.bare.binder),
            Self::Declaration(binder) => write!(f, "‘{binder}’"),
        }
    }
}
