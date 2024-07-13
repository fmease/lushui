use crate::Lowerer;
use ast::{BareHanger, LocalBinder, Path};
use diagnostics::{error::PossiblyErroneous, Diag, ErrorCode, Substitution};
use lo_ast::{
    attr::{ParentDeclarationKind, Target},
    AttrName, Attrs, DeBruijnLevel,
};
use span::{PossiblySpanning, Span, Spanned, Spanning};
use utility::{default, smallvec, Atom, FormatError, SmallVec, FILE_EXTENSION};

// @Bug trait lowering is incorrect but it can't easily be fixed (the synthesized field accessors can refer to each
// other order independently and the constructor can refer to them too by accident if it refers to fields that are
// out of order or ones that should be undefined)
// @Bug further, the lowering of records, traits (and maybe givens, too) is super useless for the type-checker
// it doesn't simplify things at all

impl Lowerer<'_> {
    pub(crate) fn lower_decl(&mut self, decl: ast::Decl) -> SmallVec<lo_ast::Decl, 1> {
        self.lower_decl_with(decl, &default())
    }

    fn lower_decl_with(
        &mut self,
        decl: ast::Decl,
        inline_modules: &InlineModules<'_>,
    ) -> SmallVec<lo_ast::Decl, 1> {
        use ast::BareDecl::*;

        let attrs = self.lower_attrs(&decl.attrs, &decl, ParentDeclarationKind::Module);

        match decl.bare {
            Func(func) => {
                let func = self.lower_function(*func, &attrs, decl.span);

                smallvec![lo_ast::Decl::new(attrs, decl.span, func.into(),)]
            }
            DataTy(ty) => smallvec![self.lower_data(*ty, attrs, decl.span)],
            Given(given) => smallvec![self.lower_given(*given, attrs, decl.span)],
            Module(module) => {
                smallvec![self.lower_module(*module, attrs, decl.span, inline_modules)]
            }
            // This is handled in the `Module` case.
            ModuleHeader => unreachable!(),
            Use(use_) => self.lower_use_declaration(*use_, attrs, decl.span),
        }
    }

    fn lower_function(&mut self, func: ast::Func, attrs: &Attrs, span: Span) -> lo_ast::Func {
        self.check_func_body(&func, attrs, span);

        let ty = match func.ty {
            Some(ty) => self.lower_expr(ty),
            None => error::missing_type_annotation(
                "function",
                func.binder,
                func.binder.span().fit_end(&func.params).end(),
            )
            .embed(&mut *self),
        };

        let body = func.body.map(|body| {
            let body = self.lower_expr(body);
            self.lower_params_to_lambda_with_default(func.params.clone(), Some(ty.clone()), body)
        });

        let ty = self.lower_params_to_pi_ty(func.params, ty);

        lo_ast::Func {
            binder: func.binder,
            ty,
            body,
        }
    }

    fn check_func_body(&mut self, func: &ast::Func, attrs: &Attrs, span: Span) {
        let before_body_span = func
            .binder
            .span()
            .fit_end(&func.params)
            .fit_end(&func.ty)
            .end();

        match (&func.body, attrs.span(AttrName::Intrinsic)) {
            (Some(_), Some(intrinsic)) => {
                // @Task use Span combinators here instead like `unexpected_body` does
                let equals = before_body_span.between(span.end()).trim_start_matches(
                    |character| character.is_ascii_whitespace(),
                    &self.sess.shared_map(),
                );

                error::unexpected_body_for_intrinsic(
                    equals,
                    "the body conflicting with the attribute",
                    intrinsic,
                )
                .handle(self);
            }
            (None, None) => error::missing_body(
                func.binder,
                before_body_span,
                Substitution::from(" = ").placeholder("value"),
            )
            .handle(self),
            _ => {}
        };
    }

    fn lower_data(&mut self, data_ty: ast::DataTy, mut attrs: Attrs, span: Span) -> lo_ast::Decl {
        self.check_data_body(&data_ty, &attrs, span);

        let ty = match data_ty.ty {
            Some(ty) => self.lower_expr(ty),
            None => lo_ast::Expr::common(
                data_ty.binder.span().fit_end(&data_ty.params).end(),
                lo_ast::BareExpr::Ty,
            ),
        };
        let ty = self.lower_params_to_pi_ty(data_ty.params.clone(), ty);

        let decls = data_ty.decls.map(|decls| match data_ty.kind {
            ast::DataKind::Data => self.lower_ctors(decls, data_ty.binder, &data_ty.params),
            ast::DataKind::Record => {
                vec![self.lower_fields_to_ctor(
                    decls,
                    RecordKind::Record,
                    data_ty.binder,
                    data_ty.params,
                )]
            }
            ast::DataKind::Trait => self.lower_trait_body(decls, data_ty.binder, data_ty.params),
        });

        if let ast::DataKind::Record | ast::DataKind::Trait = data_ty.kind {
            attrs.0.push(Spanned::new(span, lo_ast::BareAttr::Record));
        }

        if let ast::DataKind::Trait = data_ty.kind {
            attrs.0.push(Spanned::new(span, lo_ast::BareAttr::Trait));
        }

        lo_ast::Decl::new(
            attrs,
            span,
            lo_ast::DataTy {
                binder: data_ty.binder,
                ty,
                decls,
            }
            .into(),
        )
    }

    fn check_data_body(&mut self, ty: &ast::DataTy, attrs: &Attrs, span: Span) {
        let before_body_span = ty.binder.span().fit_end(&ty.params).fit_end(&ty.ty).end();

        match (&ty.decls, attrs.span(AttrName::Intrinsic)) {
            (Some(ctors), Some(intrinsic)) => {
                // @Task use Span combinators here instead like `unexpected_body` does
                let keyword = before_body_span.between(span.end()).trim_start_matches(
                    |character| character.is_ascii_whitespace(),
                    &self.sess.shared_map(),
                );

                let label = if ctors.is_empty() {
                    "\
the body specifying that the data type has no constructors and is therefore uninhabited
         conflicting with the attribute"
                } else {
                    "\
the body containing a set of constructors
         conflicting with the attribute"
                };

                error::unexpected_body_for_intrinsic(keyword.merge(ctors), label, intrinsic)
                    .handle(self);
            }
            (None, None) => error::missing_body(
                ty.binder,
                before_body_span,
                // @Bug the placeholder does not really make sense
                // @Task use a multi-line suggestion once we support that
                Substitution::from(" of ").placeholder("…"),
            )
            .handle(self),
            _ => {}
        };
    }

    fn lower_ctors(
        &mut self,
        decls: Vec<ast::Decl>,
        ty_ctor: ast::Ident,
        ty_ctor_params: &ast::Params,
    ) -> Vec<lo_ast::Decl> {
        decls
            .into_iter()
            .map(|decl| {
                let attrs = self.lower_attrs(&decl.attrs, &decl, ParentDeclarationKind::Data);

                match decl.bare {
                    ast::BareDecl::Func(func) => lo_ast::Decl::new(
                        attrs,
                        decl.span,
                        self.lower_ctor(*func, ty_ctor, ty_ctor_params.clone())
                            .into(),
                    ),
                    // @Task support this
                    ast::BareDecl::Use(_) => Diag::error()
                        .message(
                            "a use-declaration may not appear inside of a data declaration yet",
                        )
                        .unlabeled_span(decl.span)
                        .embed(&mut *self),
                    _ => error::misplaced_declaration(
                        decl.name(ParentDeclarationKind::Data),
                        decl.span,
                        ty_ctor.into_inner().remap(Atom::DATA),
                    )
                    .embed(&mut *self),
                }
            })
            .collect()
    }

    fn lower_ctor(
        &mut self,
        ctor: ast::Func,
        ty_ctor: ast::Ident,
        ty_ctor_params: ast::Params,
    ) -> lo_ast::Ctor {
        if let Some(body) = &ctor.body {
            error::unexpected_body("constructors", &ctor, body.span).handle(&mut *self);
        }

        let ty = match ctor.ty {
            Some(ty) => self.lower_expr(ty),
            None => synthesize_constructee(
                ty_ctor,
                &ty_ctor_params,
                ctor.binder.span().fit_end(&ctor.params).end(),
            ),
        };

        let ty = self.lower_params_to_pi_ty(ctor.params, ty);
        let ty = self.lower_parent_params_to_pi_ty(ty_ctor_params, ty);

        lo_ast::Ctor {
            binder: ctor.binder,
            ty,
        }
    }

    /// Lower record or trait fields to a constructor.
    fn lower_fields_to_ctor(
        &mut self,
        fields: Vec<ast::Decl>,
        kind: RecordKind,
        ty_ctor: ast::Ident,
        ty_ctor_params: ast::Params,
    ) -> lo_ast::Decl {
        // @Task use the span `of><` if there are no fields, using `default()` is a @Bug
        let span = fields.possible_span().unwrap_or_default();

        let mut ty = synthesize_constructee(ty_ctor, &ty_ctor_params, span);

        for field in fields.into_iter().rev() {
            // @Task transfer lowered attrs to pi-type (once that's supported)
            let _attributes = self.lower_attrs(&field.attrs, &field, kind.into());

            let field = match field.bare {
                ast::BareDecl::Func(field) => *field,
                _ => {
                    error::misplaced_declaration(
                        field.name(kind.into()),
                        field.span,
                        ty_ctor.into_inner().remap(kind.name()),
                    )
                    .handle(&mut *self);
                    continue;
                }
            };

            if let RecordKind::Record = kind
                && let Some(span) = field.params.possible_span()
            {
                error::parametrized_record_field(
                    field.binder,
                    field.ty.as_ref(),
                    span,
                    &self.sess.shared_map(),
                )
                .handle(&mut *self);
            }

            if let Some(body) = &field.body {
                error::unexpected_body(&format!("{} fields", kind.name()), &field, body.span)
                    .handle(&mut *self);
            }

            let domain = match field.ty {
                Some(ty) => self.lower_expr(ty),
                None => error::missing_type_annotation(
                    &format!("{} field", kind.name()),
                    field.binder,
                    field.binder.span().fit_end(&field.params).end(),
                )
                .embed(&mut *self),
            };

            let domain = self.lower_params_to_pi_ty(field.params, domain);

            ty = lo_ast::Expr::common(
                span,
                lo_ast::PiTy {
                    kind: ast::ParamKind::Explicit,
                    binder: Some(field.binder),
                    domain,
                    codomain: ty,
                }
                .into(),
            );
        }

        let ty = self.lower_parent_params_to_pi_ty(ty_ctor_params, ty);

        lo_ast::Decl::common(
            span,
            lo_ast::Ctor {
                binder: ast::Ident::new_unchecked(span, kind.name()),
                ty,
            }
            .into(),
        )
    }

    fn lower_trait_body(
        &mut self,
        fields: Vec<ast::Decl>,
        ty_ctor: ast::Ident,
        ty_ctor_params: ast::Params,
    ) -> Vec<lo_ast::Decl> {
        let mut decls = Vec::with_capacity(1 /*constructor*/ + fields.len());

        fields
            .iter()
            .filter_map(|decl| {
                // Attrs are lowered when we synthesize the constructor.

                // We reject non-functions later when lowering the fields into a constructor.
                let ast::BareDecl::Func(field) = &decl.bare else {
                    return None;
                };

                let ty = {
                    let ty = field.ty.clone()?;

                    // @Beacon @Task don't lower the field type twice (here & in lower_fields)!
                    // (this is why we currently need to clone here)
                    let ty = self.lower_expr(ty);
                    let ty = self.lower_params_to_pi_ty(field.params.clone(), ty);

                    let span = ty.span;
                    // @Task DRY
                    let binder = ast::Ident::new_unchecked(span, Atom::TRAIT);
                    let ty = lo_ast::Expr::common(
                        span,
                        lo_ast::PiTy {
                            kind: ast::ParamKind::Context,
                            binder: Some(binder),
                            // @Task don't synthesize the constructee multiple times
                            domain: synthesize_constructee(ty_ctor, &ty_ctor_params, span),
                            codomain: ty,
                        }
                        .into(),
                    );
                    // @Beacon @Task don't lower type-constructor params thrice (here, in lower_fields & in lower_data)
                    // @Task don't treat `_` special here (no harm, but no need here either)
                    self.lower_parent_params_to_pi_ty(ty_ctor_params.clone(), ty)
                };

                let body = {
                    let span = field.binder.span();
                    let binder = ast::Ident::new_unchecked(span, Atom::TRAIT);

                    let projection = lo_ast::Expr::common(
                        span,
                        lo_ast::Proj {
                            basis: lo_ast::Expr::common(span, ast::Path::from(binder).into()),
                            field: field.binder.respan(span),
                        }
                        .into(),
                    );

                    let mut body = lo_ast::Expr::common(
                        span,
                        lo_ast::LamLit {
                            kind: ast::ParamKind::Context,
                            binder: Some(binder),
                            // @Task don't synthesize the constructee multiple times
                            domain: Some(synthesize_constructee(ty_ctor, &ty_ctor_params, span)),
                            codomain: None,
                            body: projection,
                        }
                        .into(),
                    );

                    // @Beacon @Task don't re-lower type_constructor params! (here, in lower_fields & in lower_data)
                    for param in ty_ctor_params.clone().into_iter().rev() {
                        let domain = self.lower_param_ty_with_default(
                            param.bare.ty,
                            param.bare.kind,
                            param.span,
                        );

                        body = lo_ast::Expr::common(
                            span,
                            lo_ast::LamLit {
                                kind: param.bare.kind.adjust_for_child(),
                                binder: param.bare.binder.and_then(ast::LocalBinder::name),
                                domain: Some(domain),
                                codomain: None,
                                body,
                            }
                            .into(),
                        );
                    }

                    body
                };

                Some(lo_ast::Decl::common(
                    decl.span,
                    lo_ast::Func {
                        binder: field.binder,
                        ty,
                        body: Some(body),
                    }
                    .into(),
                ))
            })
            .collect_into(&mut decls);

        decls.push(self.lower_fields_to_ctor(fields, RecordKind::Trait, ty_ctor, ty_ctor_params));

        decls
    }

    fn lower_given(&mut self, given: ast::Given, mut attrs: Attrs, span: Span) -> lo_ast::Decl {
        attrs.0.push(Spanned::new(span, lo_ast::BareAttr::Context));

        // @Task maybe reuse `lower_function`?

        let ty = match given.ty {
            Some(ty) => self.lower_expr(ty),
            None => error::missing_type_annotation(
                "given",
                given.binder,
                given.binder.span().fit_end(&given.params).end(),
            )
            .embed(&mut *self),
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
                            let _attributes = self.lower_attrs(
                                &declaration.attrs,
                                &declaration,
                                ParentDeclarationKind::Given,
                            );

                            match declaration.bare {
                                // @Task (unrelated) create a lo_ast::Field { name, item } where item is not an Option
                                // but has to be defined (we need to actually *lower* stuff lol)
                                ast::BareDecl::Func(field) => {
                                    let body = match field.body {
                                        Some(body) => self.lower_expr(body),
                                        None => error::missing_field_body(&field).embed(&mut *self),
                                    };
                                    let ty = field.ty.map(|ty| self.lower_expr(ty));
                                    let body = self.lower_params_to_lambda(field.params, ty, body);

                                    Some(lo_ast::Field {
                                        binder: field.binder,
                                        body,
                                    })
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

                    lo_ast::Expr::common(
                        body,
                        lo_ast::RecLit {
                            fields: Spanned::new(body, fields),
                            path: None,
                            base: None,
                        }
                        .into(),
                    )
                }
                ast::Body::Expr { body } => self.lower_expr(body),
            };

            self.lower_params_to_lambda_with_default(given.params.clone(), Some(ty.clone()), body)
        });

        let ty = self.lower_params_to_pi_ty(given.params, ty);

        lo_ast::Decl::new(
            attrs,
            span,
            lo_ast::Func {
                binder: given.binder,
                ty,
                body,
            }
            .into(),
        )
    }

    fn lower_module(
        &mut self,
        module: ast::Module,
        mut attrs: Attrs,
        span: Span,
        inline_modules: &InlineModules<'_>,
    ) -> lo_ast::Decl {
        let is_inline_module = module.decls.is_some();

        let decls = match module.decls {
            Some(decls) => decls,
            None => {
                let mut path = self.sess.shared_map()[module.file]
                    .name()
                    .path()
                    .unwrap()
                    .as_path()
                    .parent()
                    .unwrap()
                    .to_owned();

                match attrs.get::<{ AttrName::Location }>() {
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
                let file = self.sess.map().load(&path, Some(self.sess.comp().idx()));
                let file = match file {
                    Ok(file) => file,
                    Err(error) => {
                        return error::module_loading_failure(&module.binder, span, path, error)
                            .embed(&mut *self)
                    }
                };

                let decl = match syntax::parse_module_file(file, module.binder, self.sess) {
                    Ok(declaration) => declaration,
                    Err(error) => {
                        self.health.taint(error);
                        return PossiblyErroneous::error(error);
                    }
                };

                // at this point in time, they are still on the module header if at all
                assert!(decl.attrs.is_empty());

                let module: ast::Module = decl.bare.try_into().unwrap();
                module.decls.unwrap()
            }
        };

        let mut inline_modules_for_child = InlineModules::default();
        if is_inline_module {
            inline_modules_for_child.extend(inline_modules.clone());
        };
        inline_modules_for_child.push(module.binder.to_str());

        let mut lowered_decls = Vec::new();
        let mut has_header = false;

        for (index, decl) in decls.into_iter().enumerate() {
            if decl.bare == ast::BareDecl::ModuleHeader {
                if index == 0 {
                    // @Bug this sequence may lead to some unnecessary diagnostics being emitted
                    // since the "synergy check" (which filters duplicate attribute) is run too late

                    let module_header_attrs =
                        self.lower_attrs(&decl.attrs, &decl, ParentDeclarationKind::Module);
                    attrs.0.extend(module_header_attrs.0);
                    attrs = self.check_attr_synergy(&attrs);
                } else {
                    error::misplaced_module_header(&decl, has_header).handle(&mut *self);
                }

                has_header = true;
                continue;
            }

            lowered_decls.extend(self.lower_decl_with(decl, &inline_modules_for_child));
        }

        lo_ast::Decl::new(
            attrs,
            span,
            lo_ast::Module {
                binder: module.binder,
                file: module.file,
                decls: lowered_decls,
            }
            .into(),
        )
    }

    // @Task verify that the resulting spans are correct
    fn lower_use_declaration(
        &mut self,
        use_: ast::Use,
        attrs: Attrs,
        span: Span,
    ) -> SmallVec<lo_ast::Decl, 1> {
        let mut decls = SmallVec::new();

        'discriminate: {
            match use_.bindings.bare {
                ast::BareUsePathTree::Single { target, binder } => {
                    let binder = binder.or_else(|| target.segments.last().copied());
                    let Some(binder) = binder else {
                        error::unnamed_path_hanger(target.hanger.unwrap()).handle(&mut *self);
                        break 'discriminate;
                    };

                    decls.push(lo_ast::Decl::new(
                        attrs,
                        span,
                        lo_ast::Use { binder, target }.into(),
                    ));
                }
                ast::BareUsePathTree::Multiple { path, subpaths } => {
                    self.lower_use_path_tree(&path, subpaths, span, &attrs, &mut decls);
                }
            }
        }

        decls
    }

    fn lower_use_path_tree(
        &mut self,
        path: &Path,
        subpaths: Vec<ast::UsePathTree>,
        span: Span,
        attrs: &Attrs,
        decls: &mut SmallVec<lo_ast::Decl, 1>,
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
                        if target.is_bare_hanger(BareHanger::Self_) {
                            path
                        } else {
                            &target
                        }
                        .segments
                        .last()
                        .copied()
                    }) else {
                        error::unnamed_path_hanger(target.hanger.unwrap()).handle(&mut *self);
                        continue;
                    };

                    decls.push(lo_ast::Decl::new(
                        attrs.clone(),
                        span,
                        lo_ast::Use {
                            binder,
                            target: combined_target,
                        }
                        .into(),
                    ));
                }
                ast::BareUsePathTree::Multiple {
                    path: subpath,
                    subpaths,
                } => {
                    let path = match path.clone().join(subpath) {
                        Ok(path) => path,
                        Err(hanger) => {
                            error::misplaced_path_hanger(hanger).handle(&mut *self);
                            continue;
                        }
                    };

                    self.lower_use_path_tree(&path, subpaths, span, attrs, decls);
                }
            }
        }
    }

    fn lower_parent_params_to_pi_ty(
        &mut self,
        params: ast::Params,
        mut ty: lo_ast::Expr,
    ) -> lo_ast::Expr {
        // @Task don't type_ctor_params twice (1st here, 2nd in lower_data)
        for param in params.into_iter().rev() {
            let domain =
                self.lower_param_ty_with_default(param.bare.ty, param.bare.kind, param.span);

            // @Task clean up this comment a bit more
            // Even though the parameter might be unnameable by the user due to the use of an
            // underscore or due to the omission of a binder as is possible with context parameters,
            // it *is* actually referenced in the synthesized type by a *hygienic local binding*.
            // If we mapped such parameters to `None`, we would incorrectly claim to the type
            // checker that it is not referenced inside of the type.
            // Thus we need to convert discards or absent binders to actual binders.
            let binder = match param.bare.binder {
                Some(ast::LocalBinder::Named(binder)) => binder.respan(ty.span),
                _ => ast::Ident::new_unchecked(ty.span, Atom::UNDERSCORE),
            };

            ty = lo_ast::Expr::common(
                ty.span,
                lo_ast::PiTy {
                    kind: param.bare.kind.adjust_for_child(),
                    binder: Some(binder),
                    domain,
                    codomain: ty,
                }
                .into(),
            );
        }

        ty
    }

    fn lower_params_to_pi_ty(&mut self, params: ast::Params, mut ty: lo_ast::Expr) -> lo_ast::Expr {
        for param in params.into_iter().rev() {
            let domain =
                self.lower_param_ty_with_default(param.bare.ty, param.bare.kind, param.span);

            ty = lo_ast::Expr::common(
                ty.span,
                lo_ast::PiTy {
                    kind: param.bare.kind,
                    binder: param.bare.binder.and_then(ast::LocalBinder::name),
                    domain,
                    codomain: ty,
                }
                .into(),
            );
        }

        ty
    }

    fn lower_params_to_lambda_with_default(
        &mut self,
        params: ast::Params,
        ty: Option<lo_ast::Expr>,
        mut body: lo_ast::Expr,
    ) -> lo_ast::Expr {
        let mut ty = ty.into_iter();

        for param in params.into_iter().rev() {
            let domain =
                self.lower_param_ty_with_default(param.bare.ty, param.bare.kind, param.span);

            body = lo_ast::Expr::common(
                param.span,
                lo_ast::LamLit {
                    kind: param.bare.kind,
                    binder: param.bare.binder.and_then(LocalBinder::name),
                    domain: Some(domain),
                    codomain: ty.next(),
                    body,
                }
                .into(),
            );
        }

        body
    }

    fn lower_params_to_lambda(
        &mut self,
        params: ast::Params,
        ty: Option<lo_ast::Expr>,
        mut body: lo_ast::Expr,
    ) -> lo_ast::Expr {
        let mut ty = ty.into_iter();

        for param in params.into_iter().rev() {
            let domain = param.bare.ty.map(|ty| self.lower_expr(ty));

            body = lo_ast::Expr::common(
                param.span,
                lo_ast::LamLit {
                    kind: param.bare.kind,
                    binder: param.bare.binder.and_then(LocalBinder::name),
                    domain,
                    codomain: ty.next(),
                    body,
                }
                .into(),
            );
        }

        body
    }
}

fn synthesize_constructee(binder: ast::Ident, params: &ast::Params, span: Span) -> lo_ast::Expr {
    // Prefixing the type constructor with `self.` to prevent the (unlikely) case of
    // a parameter shadowing it.
    let mut ty = lo_ast::Expr::common(
        span,
        ast::Path::hung(
            ast::Hanger::new(span, ast::BareHanger::Self_),
            smallvec![binder.respan(span)],
        )
        .into(),
    );

    for (level, param) in params.iter().enumerate() {
        ty = lo_ast::Expr::common(
            ty.span,
            lo_ast::App {
                kind: param.bare.kind,
                arg: lo_ast::Expr::common(
                    ty.span,
                    match param.bare.binder {
                        Some(ast::LocalBinder::Named(binder)) => {
                            Path::from(binder.respan(ty.span)).into()
                        }
                        _ => DeBruijnLevel(level).into(),
                    },
                ),
                callee: ty,
            }
            .into(),
        );
    }

    ty
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

    pub(super) fn misplaced_declaration(name: &str, span: Span, parent: Spanned<Atom>) -> Diag {
        Diag::error()
            .message(format!(
                "{name} may not appear inside of a {parent} declaration",
            ))
            .span(span, "misplaced declaration")
            .label(parent, format!("the enclosing {parent} declaration"))
    }

    pub(super) fn misplaced_module_header(header: &ast::Decl, duplicate: bool) -> Diag {
        let it = Diag::error()
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

    pub(super) fn misplaced_path_hanger(hanger: ast::Hanger) -> Diag {
        Diag::error()
            .code(ErrorCode::E026)
            .message(format!("path ‘{hanger}’ not allowed in this position"))
            .unlabeled_span(hanger)
            .help("consider moving this path to a separate use-declaration")
    }

    pub(super) fn missing_body(binder: ast::Ident, span: Span, substitution: Substitution) -> Diag {
        Diag::error()
            .code(ErrorCode::E012)
            .message(format!("the declaration ‘{binder}’ does not have a body"))
            .span(span, "missing definition")
            .suggest(
                span,
                "provide a definition for the declaration",
                substitution,
            )
    }

    // @Task dedup w/ `missing_body` (E012)
    pub(super) fn missing_field_body(field: &ast::Func) -> Diag {
        let span = field
            .binder
            .span()
            .fit_end(&field.params)
            .fit_end(&field.ty)
            .end();

        Diag::error()
            .message(format!("the field `{}` does not have a body", field.binder))
            .span(span, "missing definition")
            .suggest(
                span,
                "provide a definition for the declaration",
                Substitution::from(" = ").placeholder("body"),
            )
    }

    pub(super) fn missing_type_annotation(kind: &str, binder: ast::Ident, span: Span) -> Diag {
        Diag::error()
            .code(ErrorCode::E015)
            .message(format!(
                "the {kind} ‘{binder}’ does not have a type annotation"
            ))
            .span(span, "missing required type annotation")
            .suggest(
                span,
                "annotate the declaration with a type",
                Substitution::from(": ").placeholder("Type"),
            )
    }

    pub(super) fn module_loading_failure(
        module: &ast::Ident,
        span: Span,
        path: std::path::PathBuf,
        cause: std::io::Error,
    ) -> Diag {
        // @Task instead of a note saying the error, print a help message
        // saying to create the missing file or change the access rights etc.
        Diag::error()
            .code(ErrorCode::E016)
            .message(format!("could not load the module ‘{module}’"))
            .path(path)
            .unlabeled_span(span)
            .note(cause.format())
    }

    pub(super) fn parametrized_record_field(
        binder: ast::Ident,
        ty: Option<&ast::Expr>,
        span: Span,
        map: &SourceMap,
    ) -> Diag {
        // @Task use a multi-part suggestion here once they are available

        let subst = Substitution::from(": For ")
            .str(map.snippet(span).to_owned())
            .str(" -> ");

        Diag::error()
            .message("record fields may not have parameters")
            .unlabeled_span(span)
            .suggest(
                binder
                    .span()
                    .end()
                    .merge(&ty.possible_span().map_or(span, Span::start)),
                "move the parameters to a function type",
                if ty.is_some() {
                    subst
                } else {
                    subst.placeholder("Type")
                },
            )
    }

    pub(super) fn unexpected_body(kind: &str, func: &ast::Func, body: Span) -> Diag {
        // The body span including the span of the preceeding `=`
        let span = body
            .merge_into(&func.binder.span().end())
            .merge_into(&func.params.possible_span().map(Span::end))
            .merge_into(&func.ty.possible_span().map(Span::end));

        Diag::error()
            .message(format!("{kind} may not have a body"))
            .span(span, "unexpected body")
    }

    pub(super) fn unexpected_body_for_intrinsic(
        span: Span,
        label: &'static str,
        attr: Span,
    ) -> Diag {
        Diag::error()
            .code(ErrorCode::E042)
            .message("intrinsic declarations may not have a body")
            .span(span, label)
            .label(
                attr,
                "marks the declaration as being defined outside of the language",
            )
    }

    pub(super) fn unnamed_path_hanger(hanger: ast::Hanger) -> Diag {
        // @Task improve the message for `use topmost.(self)`: hint that `self`
        // is effectively unnamed because `topmost` is unnamed
        // @Task the message is even worse (it is misleading!) with `use extern.(self)`
        // currently leads to the suggestion to bind `self` to an identifier but
        // for `extern` that is invalid, too

        Diag::error()
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
