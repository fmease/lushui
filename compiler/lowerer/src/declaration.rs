use crate::Lowerer;
use ast::{BareHanger, LocalBinder, Path};
use diagnostics::{Diagnostic, ErrorCode, Substitution, error::PossiblyErroneous};
use lo_ast::{
    AttributeName, Attributes, DeBruijnLevel,
    attribute::{ParentDeclarationKind, Target},
};
use span::{PossiblySpanning, Span, Spanned, Spanning};
use utility::{Atom, FILE_EXTENSION, FormatError, SmallVec, default, smallvec};

// @Bug trait lowering is incorrect but it can't easily be fixed (the synthesized field accessors can refer to each
// other order independently and the constructor can refer to them too by accident if it refers to fields that are
// out of order or ones that should be undefined)
// @Bug further, the lowering of records, traits (and maybe givens, too) is super useless for the type-checker
// it doesn't simplify things at all

impl Lowerer<'_> {
    pub(crate) fn lower_declaration(
        &mut self,
        declaration: ast::Declaration,
    ) -> SmallVec<lo_ast::Declaration, 1> {
        self.lower_declaration_with(declaration, &default())
    }

    fn lower_declaration_with(
        &mut self,
        declaration: ast::Declaration,
        inline_modules: &InlineModules<'_>,
    ) -> SmallVec<lo_ast::Declaration, 1> {
        use ast::BareDeclaration::*;

        let attributes = self.lower_attributes(
            &declaration.attributes,
            &declaration,
            ParentDeclarationKind::Module,
        );

        match declaration.bare {
            Function(function) => {
                let function = self.lower_function(*function, &attributes, declaration.span);

                smallvec![lo_ast::Declaration::new(attributes, declaration.span, function.into(),)]
            }
            Data(type_) => smallvec![self.lower_data(*type_, attributes, declaration.span)],
            Given(given) => smallvec![self.lower_given(*given, attributes, declaration.span)],
            Module(module) => {
                smallvec![self.lower_module(*module, attributes, declaration.span, inline_modules)]
            }
            // This is handled in the `Module` case.
            ModuleHeader => unreachable!(),
            Use(use_) => self.lower_use_declaration(*use_, attributes, declaration.span),
        }
    }

    fn lower_function(
        &mut self,
        function: ast::Function,
        attributes: &Attributes,
        span: Span,
    ) -> lo_ast::Function {
        self.check_function_body(&function, attributes, span);

        let type_ = match function.type_ {
            Some(type_) => self.lower_expression(type_),
            None => lo_ast::Expression::common(
                function.binder.span().fit_end(&function.parameters).end(),
                lo_ast::BareExpression::Type,
            ),
        };

        let body = function.body.map(|body| {
            let body = self.lower_expression(body);
            self.lower_parameters_to_lambda_with_default(
                function.parameters.clone(),
                Some(type_.clone()),
                body,
            )
        });

        let type_ = self.lower_parameters_to_pi_type(function.parameters, type_);

        lo_ast::Function { binder: function.binder, type_, body }
    }

    fn check_function_body(
        &mut self,
        function: &ast::Function,
        attributes: &Attributes,
        span: Span,
    ) {
        let before_body_span =
            function.binder.span().fit_end(&function.parameters).fit_end(&function.type_).end();

        match (&function.body, attributes.span(AttributeName::Intrinsic)) {
            (Some(_), Some(intrinsic)) => {
                // @Task use Span combinators here instead like `unexpected_body` does
                let equals = before_body_span.between(span.end()).trim_start_matches(
                    |character| character.is_ascii_whitespace(),
                    &self.session.shared_map(),
                );

                error::unexpected_body_for_intrinsic(
                    equals,
                    "the body conflicting with the attribute",
                    intrinsic,
                )
                .handle(self);
            }
            (None, None) => error::missing_body(
                function.binder,
                before_body_span,
                Substitution::from(" = ").placeholder("value"),
            )
            .handle(self),
            _ => {}
        };
    }

    fn lower_data(
        &mut self,
        data_type: ast::Data,
        mut attributes: Attributes,
        span: Span,
    ) -> lo_ast::Declaration {
        self.check_data_body(&data_type, &attributes, span);

        let type_ = match data_type.type_ {
            Some(type_) => self.lower_expression(type_),
            None => lo_ast::Expression::common(
                data_type.binder.span().fit_end(&data_type.parameters).end(),
                lo_ast::BareExpression::Type,
            ),
        };
        let type_ = self.lower_parameters_to_pi_type(data_type.parameters.clone(), type_);

        let declarations = data_type.declarations.map(|declarations| match data_type.kind {
            ast::DataKind::Data => {
                self.lower_constructors(declarations, data_type.binder, &data_type.parameters)
            }
            ast::DataKind::Record => {
                vec![self.lower_fields_to_constructor(
                    declarations,
                    RecordKind::Record,
                    data_type.binder,
                    data_type.parameters,
                )]
            }
            ast::DataKind::Trait => {
                self.lower_trait_body(declarations, data_type.binder, data_type.parameters)
            }
        });

        if let ast::DataKind::Record | ast::DataKind::Trait = data_type.kind {
            attributes.0.push(Spanned::new(span, lo_ast::BareAttribute::Record));
        }

        if let ast::DataKind::Trait = data_type.kind {
            attributes.0.push(Spanned::new(span, lo_ast::BareAttribute::Trait));
        }

        lo_ast::Declaration::new(
            attributes,
            span,
            lo_ast::Data { binder: data_type.binder, type_, declarations }.into(),
        )
    }

    fn check_data_body(&mut self, type_: &ast::Data, attributes: &Attributes, span: Span) {
        let before_body_span =
            type_.binder.span().fit_end(&type_.parameters).fit_end(&type_.type_).end();

        match (&type_.declarations, attributes.span(AttributeName::Intrinsic)) {
            (Some(constructors), Some(intrinsic)) => {
                // @Task use Span combinators here instead like `unexpected_body` does
                let keyword = before_body_span.between(span.end()).trim_start_matches(
                    |character| character.is_ascii_whitespace(),
                    &self.session.shared_map(),
                );

                let label = if constructors.is_empty() {
                    "\
the body specifying that the data type has no constructors and is therefore uninhabited
         conflicting with the attribute"
                } else {
                    "\
the body containing a set of constructors
         conflicting with the attribute"
                };

                error::unexpected_body_for_intrinsic(keyword.merge(constructors), label, intrinsic)
                    .handle(self);
            }
            (None, None) => error::missing_body(
                type_.binder,
                before_body_span,
                // @Bug the placeholder does not really make sense
                // @Task use a multi-line suggestion once we support that
                Substitution::from(" of ").placeholder("…"),
            )
            .handle(self),
            _ => {}
        };
    }

    fn lower_constructors(
        &mut self,
        declarations: Vec<ast::Declaration>,
        type_constructor: ast::Identifier,
        type_constructor_parameters: &ast::Parameters,
    ) -> Vec<lo_ast::Declaration> {
        declarations
            .into_iter()
            .map(|declaration| {
                let attributes = self.lower_attributes(
                    &declaration.attributes,
                    &declaration,
                    ParentDeclarationKind::Data,
                );

                match declaration.bare {
                    ast::BareDeclaration::Function(function) => lo_ast::Declaration::new(
                        attributes,
                        declaration.span,
                        self.lower_constructor(
                            *function,
                            type_constructor,
                            type_constructor_parameters.clone(),
                        )
                        .into(),
                    ),
                    // @Task support this
                    ast::BareDeclaration::Use(_) => Diagnostic::error()
                        .message(
                            "a use-declaration may not appear inside of a data declaration yet",
                        )
                        .unlabeled_span(declaration.span)
                        .embed(&mut *self),
                    _ => error::misplaced_declaration(
                        declaration.name(ParentDeclarationKind::Data),
                        declaration.span,
                        type_constructor.into_inner().remap(Atom::DATA),
                    )
                    .embed(&mut *self),
                }
            })
            .collect()
    }

    fn lower_constructor(
        &mut self,
        constructor: ast::Function,
        type_constructor: ast::Identifier,
        type_constructor_parameters: ast::Parameters,
    ) -> lo_ast::Constructor {
        if let Some(body) = &constructor.body {
            error::unexpected_body("constructors", &constructor, body.span).handle(&mut *self);
        }

        let type_ = match constructor.type_ {
            Some(type_) => self.lower_expression(type_),
            None => synthesize_constructee(
                type_constructor,
                &type_constructor_parameters,
                constructor.binder.span().fit_end(&constructor.parameters).end(),
            ),
        };

        let type_ = self.lower_parameters_to_pi_type(constructor.parameters, type_);
        let type_ = self.lower_parent_parameters_to_pi_type(type_constructor_parameters, type_);

        lo_ast::Constructor { binder: constructor.binder, type_ }
    }

    /// Lower record or trait fields to a constructor.
    fn lower_fields_to_constructor(
        &mut self,
        fields: Vec<ast::Declaration>,
        kind: RecordKind,
        type_constructor: ast::Identifier,
        type_constructor_parameters: ast::Parameters,
    ) -> lo_ast::Declaration {
        // @Task use the span `of><` if there are no fields, using `default()` is a @Bug
        let span = fields.possible_span().unwrap_or_default();

        let mut type_ =
            synthesize_constructee(type_constructor, &type_constructor_parameters, span);

        for field in fields.into_iter().rev() {
            // @Task transfer lowered attrs to pi-type (once that's supported)
            let _attributes = self.lower_attributes(&field.attributes, &field, kind.into());

            let field = match field.bare {
                ast::BareDeclaration::Function(field) => *field,
                _ => {
                    error::misplaced_declaration(
                        field.name(kind.into()),
                        field.span,
                        type_constructor.into_inner().remap(kind.name()),
                    )
                    .handle(&mut *self);
                    continue;
                }
            };

            if let RecordKind::Record = kind
                && let Some(span) = field.parameters.possible_span()
            {
                error::parametrized_record_field(
                    field.binder,
                    field.type_.as_ref(),
                    span,
                    &self.session.shared_map(),
                )
                .handle(&mut *self);
            }

            if let Some(body) = &field.body {
                error::unexpected_body(&format!("{} fields", kind.name()), &field, body.span)
                    .handle(&mut *self);
            }

            let domain = match field.type_ {
                Some(type_) => self.lower_expression(type_),
                None => lo_ast::Expression::common(
                    field.binder.span().fit_end(&field.parameters).end(),
                    lo_ast::BareExpression::Type,
                ),
            };

            let domain = self.lower_parameters_to_pi_type(field.parameters, domain);

            type_ = lo_ast::Expression::common(
                span,
                lo_ast::PiType {
                    kind: ast::ParameterKind::Explicit,
                    binder: Some(field.binder),
                    domain,
                    codomain: type_,
                }
                .into(),
            );
        }

        let type_ = self.lower_parent_parameters_to_pi_type(type_constructor_parameters, type_);

        lo_ast::Declaration::common(
            span,
            lo_ast::Constructor {
                binder: ast::Identifier::new_unchecked(span, kind.name()),
                type_,
            }
            .into(),
        )
    }

    fn lower_trait_body(
        &mut self,
        fields: Vec<ast::Declaration>,
        type_constructor: ast::Identifier,
        type_constructor_parameters: ast::Parameters,
    ) -> Vec<lo_ast::Declaration> {
        let mut declarations = Vec::with_capacity(1 /*constructor*/ + fields.len());

        fields
            .iter()
            .filter_map(|declaration| {
                // Attributes are lowered when we synthesize the constructor.

                // We reject non-functions later when lowering the fields into a constructor.
                let ast::BareDeclaration::Function(field) = &declaration.bare else {
                    return None;
                };

                let type_ = {
                    // @Beacon @Task don't lower the field type twice (here & in lower_fields)!
                    // (this is why we currently need to clone here)
                    let type_ = match &field.type_ {
                        Some(type_) => self.lower_expression(type_.clone()),
                        None => lo_ast::Expression::common(
                            field.binder.span().fit_end(&field.parameters),
                            lo_ast::BareExpression::Type,
                        ),
                    };

                    let type_ = self.lower_parameters_to_pi_type(field.parameters.clone(), type_);

                    let span = type_.span;
                    // @Task DRY
                    let binder = ast::Identifier::new_unchecked(span, Atom::TRAIT);
                    let type_ = lo_ast::Expression::common(
                        span,
                        lo_ast::PiType {
                            kind: ast::ParameterKind::Context,
                            binder: Some(binder),
                            // @Task don't synthesize the constructee multiple times
                            domain: synthesize_constructee(
                                type_constructor,
                                &type_constructor_parameters,
                                span,
                            ),
                            codomain: type_,
                        }
                        .into(),
                    );
                    // @Beacon @Task don't lower type-constructor params thrice (here, in lower_fields & in lower_data)
                    // @Task don't treat `_` special here (no harm, but no need here either)
                    self.lower_parent_parameters_to_pi_type(
                        type_constructor_parameters.clone(),
                        type_,
                    )
                };

                let body = {
                    let span = field.binder.span();
                    let binder = ast::Identifier::new_unchecked(span, Atom::TRAIT);

                    let projection = lo_ast::Expression::common(
                        span,
                        lo_ast::Projection {
                            basis: lo_ast::Expression::common(span, ast::Path::from(binder).into()),
                            field: field.binder.respan(span),
                        }
                        .into(),
                    );

                    let mut body = lo_ast::Expression::common(
                        span,
                        lo_ast::Lambda {
                            kind: ast::ParameterKind::Context,
                            binder: Some(binder),
                            // @Task don't synthesize the constructee multiple times
                            domain: Some(synthesize_constructee(
                                type_constructor,
                                &type_constructor_parameters,
                                span,
                            )),
                            codomain: None,
                            body: projection,
                        }
                        .into(),
                    );

                    // @Beacon @Task don't re-lower type_constructor params! (here, in lower_fields & in lower_data)
                    for parameter in type_constructor_parameters.clone().into_iter().rev() {
                        let binder = parameter.bare.binder.and_then(ast::LocalBinder::name);
                        let kind = parameter.bare.kind.adjust_for_child();
                        let domain = self.lower_parameter_type(parameter);

                        body = lo_ast::Expression::common(
                            span,
                            lo_ast::Lambda {
                                kind,
                                binder,
                                domain: Some(domain),
                                codomain: None,
                                body,
                            }
                            .into(),
                        );
                    }

                    body
                };

                Some(lo_ast::Declaration::common(
                    declaration.span,
                    lo_ast::Function { binder: field.binder, type_, body: Some(body) }.into(),
                ))
            })
            .collect_into(&mut declarations);

        declarations.push(self.lower_fields_to_constructor(
            fields,
            RecordKind::Trait,
            type_constructor,
            type_constructor_parameters,
        ));

        declarations
    }

    fn lower_given(
        &mut self,
        given: ast::Given,
        mut attributes: Attributes,
        span: Span,
    ) -> lo_ast::Declaration {
        attributes.0.push(Spanned::new(span, lo_ast::BareAttribute::Context));

        // @Task maybe reuse `lower_function`?

        let type_ = match given.type_ {
            Some(type_) => self.lower_expression(type_),
            None => lo_ast::Expression::common(
                given.binder.span().fit_end(&given.parameters).end(),
                lo_ast::BareExpression::Type,
            ),
        };

        // @Task if there's no body, check if `@intrinsic` or `@derive` (tba) is present,
        //       otherwise error out
        let body = given.body.map(|body| {
            let body = match body {
                ast::Body::Block { fields } => {
                    // @Task better span (ideally `of><`), just collect the info the parser
                    let body = fields.possible_span().unwrap_or(span);

                    let fields: Vec<_> = fields
                        .into_iter()
                        .filter_map(|declaration| {
                            // @Task transfer the lowered attributes to the pi-type parameters
                            let _attributes = self.lower_attributes(
                                &declaration.attributes,
                                &declaration,
                                ParentDeclarationKind::Given,
                            );

                            match declaration.bare {
                                // @Task (unrelated) create a lo_ast::Field { name, item } where item is not an Option
                                // but has to be defined (we need to actually *lower* stuff lol)
                                ast::BareDeclaration::Function(field) => {
                                    let body = match field.body {
                                        Some(body) => self.lower_expression(body),
                                        None => error::missing_field_body(&field).embed(&mut *self),
                                    };
                                    let type_ =
                                        field.type_.map(|type_| self.lower_expression(type_));
                                    let body = self.lower_parameters_to_lambda(
                                        field.parameters,
                                        type_,
                                        body,
                                    );

                                    Some(lo_ast::Field { binder: field.binder, body })
                                }
                                _ => {
                                    error::misplaced_declaration(
                                        declaration.name(ParentDeclarationKind::Given),
                                        declaration.span,
                                        Spanned::new(span, Atom::GIVEN),
                                    )
                                    .handle(&mut *self);
                                    None
                                }
                            }
                        })
                        .collect();

                    lo_ast::Expression::common(
                        body,
                        lo_ast::RecordLiteral {
                            fields: Spanned::new(body, fields),
                            path: None,
                            base: None,
                        }
                        .into(),
                    )
                }
                ast::Body::Expression { body } => self.lower_expression(body),
            };

            self.lower_parameters_to_lambda_with_default(
                given.parameters.clone(),
                Some(type_.clone()),
                body,
            )
        });

        let type_ = self.lower_parameters_to_pi_type(given.parameters, type_);

        lo_ast::Declaration::new(
            attributes,
            span,
            lo_ast::Function { binder: given.binder, type_, body }.into(),
        )
    }

    fn lower_module(
        &mut self,
        module: ast::Module,
        mut attributes: Attributes,
        span: Span,
        inline_modules: &InlineModules<'_>,
    ) -> lo_ast::Declaration {
        let is_inline_module = module.declarations.is_some();

        let declarations = match module.declarations {
            Some(declarations) => declarations,
            None => {
                let mut path = self.session.shared_map()[module.file]
                    .name()
                    .path()
                    .unwrap()
                    .as_inner()
                    .parent()
                    .unwrap()
                    .to_owned();

                match attributes.get::<{ AttributeName::Location }>() {
                    Some(location) => {
                        let mut inline_modules = inline_modules.as_slice();
                        _ = inline_modules.take_last();

                        path.extend(inline_modules);
                        path.push(location.bare.to_str());
                    }
                    None => {
                        path.extend(inline_modules);
                        path.push(module.binder.to_str());
                    }
                };

                path.set_extension(FILE_EXTENSION);

                // @Task create & use a different API that doesn't "recanonicalize" the path
                let file = self.session.map().load(&path, Some(self.session.component().index()));
                let file = match file {
                    Ok(file) => file,
                    Err(error) => {
                        return error::module_loading_failure(&module.binder, span, path, error)
                            .embed(&mut *self);
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

        let mut inline_modules_for_child = InlineModules::default();
        if is_inline_module {
            inline_modules_for_child.extend(inline_modules.clone());
        };
        inline_modules_for_child.push(module.binder.to_str());

        let mut lowered_declarations = Vec::new();
        let mut has_header = false;

        for (index, declaration) in declarations.into_iter().enumerate() {
            if declaration.bare == ast::BareDeclaration::ModuleHeader {
                if index == 0 {
                    // @Bug this sequence may lead to some unnecessary diagnostics being emitted
                    // since the "synergy check" (which filters duplicate attribute) is run too late

                    let module_header_attributes = self.lower_attributes(
                        &declaration.attributes,
                        &declaration,
                        ParentDeclarationKind::Module,
                    );
                    attributes.0.extend(module_header_attributes.0);
                    attributes = self.check_attribute_synergy(&attributes);
                } else {
                    error::misplaced_module_header(&declaration, has_header).handle(&mut *self);
                }

                has_header = true;
                continue;
            }

            lowered_declarations
                .extend(self.lower_declaration_with(declaration, &inline_modules_for_child));
        }

        lo_ast::Declaration::new(
            attributes,
            span,
            lo_ast::Module {
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
    ) -> SmallVec<lo_ast::Declaration, 1> {
        let mut declarations = SmallVec::new();

        'discriminate: {
            match use_.bindings.bare {
                ast::BareUsePathTree::Single { target, binder } => {
                    let binder = binder.or_else(|| target.segments.last().copied());
                    let Some(binder) = binder else {
                        error::unnamed_path_hanger(target.hanger.unwrap()).handle(&mut *self);
                        break 'discriminate;
                    };

                    declarations.push(lo_ast::Declaration::new(
                        attributes,
                        span,
                        lo_ast::Use { binder, target }.into(),
                    ));
                }
                ast::BareUsePathTree::Multiple { path, subpaths } => {
                    self.lower_use_path_tree(&path, subpaths, span, &attributes, &mut declarations);
                }
            }
        }

        declarations
    }

    fn lower_use_path_tree(
        &mut self,
        path: &Path,
        subpaths: Vec<ast::UsePathTree>,
        span: Span,
        attributes: &Attributes,
        declarations: &mut SmallVec<lo_ast::Declaration, 1>,
    ) {
        for subpath in subpaths {
            match subpath.bare {
                ast::BareUsePathTree::Single { target, binder } => {
                    let combined_target = match path.clone().join(target.clone()) {
                        Ok(target) => target,
                        Err(hanger) => {
                            error::misplaced_path_hanger(hanger).handle(&mut *self);
                            continue;
                        }
                    };

                    // if the binder is not explicitly set, look for the most-specific/last/right-most
                    // identifier of the target but if that one is `self`, look up the last identifier of
                    // the parent path
                    let Some(binder) = binder.or_else(|| {
                        if target.is_bare_hanger(BareHanger::Self_) { path } else { &target }
                            .segments
                            .last()
                            .copied()
                    }) else {
                        error::unnamed_path_hanger(target.hanger.unwrap()).handle(&mut *self);
                        continue;
                    };

                    declarations.push(lo_ast::Declaration::new(
                        attributes.clone(),
                        span,
                        lo_ast::Use { binder, target: combined_target }.into(),
                    ));
                }
                ast::BareUsePathTree::Multiple { path: subpath, subpaths } => {
                    let path = match path.clone().join(subpath) {
                        Ok(path) => path,
                        Err(hanger) => {
                            error::misplaced_path_hanger(hanger).handle(&mut *self);
                            continue;
                        }
                    };

                    self.lower_use_path_tree(&path, subpaths, span, attributes, declarations);
                }
            }
        }
    }

    fn lower_parent_parameters_to_pi_type(
        &mut self,
        parameters: ast::Parameters,
        mut type_: lo_ast::Expression,
    ) -> lo_ast::Expression {
        // @Task don't type_ctor_params twice (1st here, 2nd in lower_data)
        for parameter in parameters.into_iter().rev() {
            // @Task clean up this comment a bit more
            // Even though the parameter might be unnameable by the user due to the use of an
            // underscore or due to the omission of a binder as is possible with context parameters,
            // it *is* actually referenced in the synthesized type by a *hygienic local binding*.
            // If we mapped such parameters to `None`, we would incorrectly claim to the type
            // checker that it is not referenced inside of the type.
            // Thus we need to convert discards or absent binders to actual binders.
            let binder = match parameter.bare.binder {
                Some(ast::LocalBinder::Named(binder)) => binder.respan(type_.span),
                _ => ast::Identifier::new_unchecked(type_.span, Atom::UNDERSCORE),
            };
            let kind = parameter.bare.kind.adjust_for_child();
            let domain = self.lower_parameter_type(parameter);

            type_ = lo_ast::Expression::common(
                type_.span,
                lo_ast::PiType { kind, binder: Some(binder), domain, codomain: type_ }.into(),
            );
        }

        type_
    }

    fn lower_parameters_to_pi_type(
        &mut self,
        parameters: ast::Parameters,
        mut type_: lo_ast::Expression,
    ) -> lo_ast::Expression {
        for parameter in parameters.into_iter().rev() {
            let binder = parameter.bare.binder.and_then(ast::LocalBinder::name);
            let kind = parameter.bare.kind;
            let domain = self.lower_parameter_type(parameter);

            type_ = lo_ast::Expression::common(
                type_.span,
                lo_ast::PiType { kind, binder, domain, codomain: type_ }.into(),
            );
        }

        type_
    }

    fn lower_parameters_to_lambda_with_default(
        &mut self,
        parameters: ast::Parameters,
        type_: Option<lo_ast::Expression>,
        mut body: lo_ast::Expression,
    ) -> lo_ast::Expression {
        let mut type_ = type_.into_iter();

        for parameter in parameters.into_iter().rev() {
            let binder = parameter.bare.binder.and_then(LocalBinder::name);
            let kind = parameter.bare.kind;
            let span = parameter.span;
            let domain = self.lower_parameter_type(parameter);

            body = lo_ast::Expression::common(
                span,
                lo_ast::Lambda { kind, binder, domain: Some(domain), codomain: type_.next(), body }
                    .into(),
            );
        }

        body
    }

    fn lower_parameters_to_lambda(
        &mut self,
        parameters: ast::Parameters,
        type_: Option<lo_ast::Expression>,
        mut body: lo_ast::Expression,
    ) -> lo_ast::Expression {
        let mut type_ = type_.into_iter();

        for parameter in parameters.into_iter().rev() {
            let domain = parameter.bare.type_.map(|type_| self.lower_expression(type_));

            body = lo_ast::Expression::common(
                parameter.span,
                lo_ast::Lambda {
                    kind: parameter.bare.kind,
                    binder: parameter.bare.binder.and_then(LocalBinder::name),
                    domain,
                    codomain: type_.next(),
                    body,
                }
                .into(),
            );
        }

        body
    }
}

fn synthesize_constructee(
    binder: ast::Identifier,
    parameters: &ast::Parameters,
    span: Span,
) -> lo_ast::Expression {
    // Prefixing the type constructor with `self.` to prevent the (unlikely) case of
    // a parameter shadowing it.
    let mut type_ = lo_ast::Expression::common(
        span,
        ast::Path::hung(ast::Hanger::new(span, ast::BareHanger::Self_), smallvec![
            binder.respan(span)
        ])
        .into(),
    );

    for (level, parameter) in parameters.iter().enumerate() {
        type_ = lo_ast::Expression::common(
            type_.span,
            lo_ast::Application {
                kind: parameter.bare.kind,
                argument: lo_ast::Expression::common(type_.span, match parameter.bare.binder {
                    Some(ast::LocalBinder::Named(binder)) => {
                        Path::from(binder.respan(type_.span)).into()
                    }
                    _ => DeBruijnLevel(level).into(),
                }),
                callee: type_,
            }
            .into(),
        );
    }

    type_
}

/// The path of inline module declarations leading to a declaration.
///
/// The path starts either at the root module declaration or at the closest out-of-line module.
type InlineModules<'a> = Vec<&'a str>;

#[derive(Clone, Copy)]
enum RecordKind {
    Record,
    Trait,
}

impl RecordKind {
    const fn name(self) -> Atom {
        match self {
            Self::Record => Atom::RECORD,
            Self::Trait => Atom::TRAIT,
        }
    }
}

impl From<RecordKind> for ParentDeclarationKind {
    fn from(kind: RecordKind) -> Self {
        match kind {
            RecordKind::Record => Self::Record,
            RecordKind::Trait => Self::Trait,
        }
    }
}

mod error {
    #[allow(clippy::wildcard_imports)] // private inline module
    use super::*;
    use diagnostics::Substitution;
    use span::SourceMap;

    pub(super) fn misplaced_declaration(
        name: &str,
        span: Span,
        parent: Spanned<Atom>,
    ) -> Diagnostic {
        Diagnostic::error()
            .message(format!("{name} may not appear inside of a {parent} declaration",))
            .span(span, "misplaced declaration")
            .label(parent, format!("the enclosing {parent} declaration"))
    }

    pub(super) fn misplaced_module_header(
        header: &ast::Declaration,
        duplicate: bool,
    ) -> Diagnostic {
        let it = Diagnostic::error()
            .code(ErrorCode::E041)
            .message("the module header has to be the first declaration of the module")
            .unlabeled_span(header);

        if duplicate {
            // @Task make this a note with a span/highlight!
            // @Update @Task make this a secondary hl (& ch wording)
            it.note("however, the current module already has a module header")
        } else {
            it
        }
    }

    pub(super) fn misplaced_path_hanger(hanger: ast::Hanger) -> Diagnostic {
        Diagnostic::error()
            .code(ErrorCode::E026)
            .message(format!("path ‘{hanger}’ not allowed in this position"))
            .unlabeled_span(hanger)
            .help("consider moving this path to a separate use-declaration")
    }

    pub(super) fn missing_body(
        binder: ast::Identifier,
        span: Span,
        substitution: Substitution,
    ) -> Diagnostic {
        Diagnostic::error()
            .code(ErrorCode::E012)
            .message(format!("the declaration ‘{binder}’ does not have a body"))
            .span(span, "missing definition")
            .suggest(span, "provide a definition for the declaration", substitution)
    }

    // @Task dedup w/ `missing_body` (E012)
    pub(super) fn missing_field_body(field: &ast::Function) -> Diagnostic {
        let span = field.binder.span().fit_end(&field.parameters).fit_end(&field.type_).end();

        Diagnostic::error()
            .message(format!("the field `{}` does not have a body", field.binder))
            .span(span, "missing definition")
            .suggest(
                span,
                "provide a definition for the declaration",
                Substitution::from(" = ").placeholder("body"),
            )
    }

    pub(super) fn module_loading_failure(
        module: &ast::Identifier,
        span: Span,
        path: std::path::PathBuf,
        cause: std::io::Error,
    ) -> Diagnostic {
        // @Task instead of a note saying the error, print a help message
        // saying to create the missing file or change the access rights etc.
        Diagnostic::error()
            .code(ErrorCode::E016)
            .message(format!("could not load the module ‘{module}’"))
            .path(path)
            .unlabeled_span(span)
            .note(cause.format())
    }

    pub(super) fn parametrized_record_field(
        binder: ast::Identifier,
        type_: Option<&ast::Expression>,
        span: Span,
        map: &SourceMap,
    ) -> Diagnostic {
        // @Task use a multi-part suggestion here once they are available

        let substitution =
            Substitution::from(": For ").str(map.snippet(span).to_owned()).str(" -> ");

        Diagnostic::error()
            .message("record fields may not have parameters")
            .unlabeled_span(span)
            .suggest(
                binder.span().end().merge(&type_.possible_span().map_or(span, Span::start)),
                "move the parameters to a function type",
                if type_.is_some() { substitution } else { substitution.placeholder("Type") },
            )
    }

    pub(super) fn unexpected_body(kind: &str, function: &ast::Function, body: Span) -> Diagnostic {
        // The body span including the span of the preceeding `=`
        let span = body
            .merge_into(&function.binder.span().end())
            .merge_into(&function.parameters.possible_span().map(Span::end))
            .merge_into(&function.type_.possible_span().map(Span::end));

        Diagnostic::error()
            .message(format!("{kind} may not have a body"))
            .span(span, "unexpected body")
    }

    pub(super) fn unexpected_body_for_intrinsic(
        span: Span,
        label: &'static str,
        attribute: Span,
    ) -> Diagnostic {
        Diagnostic::error()
            .code(ErrorCode::E042)
            .message("intrinsic declarations may not have a body")
            .span(span, label)
            .label(attribute, "marks the declaration as being defined outside of the language")
    }

    pub(super) fn unnamed_path_hanger(hanger: ast::Hanger) -> Diagnostic {
        // @Task improve the message for `use topmost.(self)`: hint that `self`
        // is effectively unnamed because `topmost` is unnamed
        // @Task the message is even worse (it is misleading!) with `use extern.(self)`
        // currently leads to the suggestion to bind `self` to an identifier but
        // for `extern` that is invalid, too

        Diagnostic::error()
            .code(ErrorCode::E025)
            .message(format!("the path ‘{hanger}’ is not bound to a name"))
            .unlabeled_span(hanger)
            .suggest(
                hanger.span.end(),
                "bind the path to a name",
                Substitution::from(" as ").placeholder("name"),
            )
    }
}
