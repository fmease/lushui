//! The name resolver.
//!
//! It traverses the [Lo-AST](lo_ast) and registers/defines bindings
//! defined both at module-level using declarations and at function
//! and pattern level as parameters. Furthermore, it resolves all paths inside
//! expressions and patterns to [(resolved) identifiers](Ident) which
//! contain a [declaration index](DeclIdx) or a [de Bruijn index](DeBruijnIdx)
//! respectively.
#![feature(let_chains, iter_array_chunks)]

// @Task improve docs above!
// @Task get rid of "register" terminology
// @Task transform all Result-returning functions into ()-returning ones modifying self.health
//       and use the new Handler API and get rid of the Stain API

use ast::ParamKind::Explicit;
use diagnostics::{
    error::{Handler, Health, Outcome, PossiblyErroneous, Result, Stain},
    reporter::ErasedReportedError,
    Diag, ErrorCode, LintCode,
};
use hir::{
    special::{self, NumTy, SeqTy, Ty},
    AttrName, Attrs, DeBruijnIdx, DeclIdx, Entity, EntityKind, Exposure, ExposureReach, Ident,
    Index, LocalDeclIdx, PartiallyResolvedPath,
};
use hir_format::{Display, SessionExt};
use lexer::word::Word;
use lo_ast::DeBruijnLevel;
use session::{
    component::{Comp, DeclIdxExt, IdentExt, LocalDeclIdxExt},
    Session,
};
use span::{Span, Spanned, Spanning};
use std::{cmp::Ordering, fmt, io, mem, sync::Mutex};
use unicode_width::UnicodeWidthStr;
use utility::{
    cycle::find_cycles,
    default, displayed, obtain,
    paint::{paint_to_string, ColorChoice, Effects, Painter},
    pluralize, smallvec, Atom, ChangesetExt, Conjunction, HashMap, ListingExt,
    OwnedOrBorrowed::*,
    QuoteExt, SmallVec, PROGRAM_ENTRY,
};

/// Resolve the names of a declaration.
///
/// It performs four passes to resolve all possible out of order declarations.
///
/// # Panics
///
/// If the declaration passed is not a module, this function will panic as it
/// requires a root module which is defined through the root module.
// @Task improve docs: mention that it not only looks things up but also defines bindings
// and that `resolve` should only be called once per Component (bc it would fail anyways the 2nd
// time (to re-define root (I think)))
pub fn resolve_decls(root_module: lo_ast::Decl, sess: &mut Session<'_>) -> Result<hir::Decl> {
    let mut resolver = ResolverMut::new(sess);

    if let Err(error) = resolver.start_resolve_decl(&root_module, None, default()) {
        for (binder, naming_conflicts) in mem::take(&mut resolver.naming_conflicts) {
            error::confliction_definitions(resolver.sess[binder].src, &naming_conflicts)
                .report(resolver.sess.rep());
        }

        return Err(error.into_inner());
    }

    // @Beacon @Note these two passes should probably not run after one another but
    // intertwined since use bindings depend on exposure
    // unless we the new "partially resolved exposure" logic fixes everything.
    // Would probably fix the ordering bug in test name-resolution/exposure/re-export-bindings
    resolver.resolve_use_bindings();
    resolver.resolve_exposure_reaches();

    let decl = resolver
        .fin_resolve_decl(root_module, None, default())
        .unwrap();

    Outcome::new(decl, resolver.health).into()
}

// @Task docs: mention that the current component should be pre-populated before calling this
// (using resolver::resolve)
pub fn resolve_path(path: &ast::Path, namespace: DeclIdx, sess: &Session<'_>) -> Result<DeclIdx> {
    Resolver::new(sess)
        .resolve_path::<target::Any>(path, PathResolutionContext::new(namespace))
        .map_err(|error| Resolver::new(sess).report_resolution_error(error))
}

// @Question can we merge Resolver and ResolverMut if we introduce a separate
// lifetime for Resolver.session.at?
struct ResolverMut<'sess, 'ctx> {
    sess: &'sess mut Session<'ctx>,
    /// For resolving out of order use-declarations.
    partially_resolved_use_bindings: HashMap<LocalDeclIdx, PartiallyResolvedUseBinding>,
    /// Naming conflicts used for error reporting.
    ///
    /// Allows us to group conflicts by binder and emit a *single* diagnostic *per group* (making
    /// use of multiple primary highlights).
    naming_conflicts: HashMap<LocalDeclIdx, SmallVec<Span, 2>>,
    health: Health,
}

impl<'sess, 'ctx> ResolverMut<'sess, 'ctx> {
    fn new(sess: &'sess mut Session<'ctx>) -> Self {
        Self {
            sess,
            partially_resolved_use_bindings: HashMap::default(),
            naming_conflicts: HashMap::default(),
            health: Health::Untainted,
        }
    }

    fn as_ref(&self) -> Resolver<'_> {
        Resolver { sess: self.sess }
    }

    /// Partially resolve a declaration merely registering declarations.
    ///
    /// This traverses all declarations and registers module-level bindings
    /// checking that they are only defined once per namespace.
    /// Use-bindings which refer to an unknown binding are marked as such
    /// to be resolved in the second, a minor pass.
    ///
    /// This also searches the program entry and stores it when it finds it.
    ///
    /// In contrast to [`Self::finish_resolve_declaration`], this does not actually return a
    /// new intermediate HIR because of too much mapping and type-system boilerplate
    /// and it's just not worth it memory-wise.
    /// For more on this, see [`Resolver::reobtain_resolved_ident`].
    fn start_resolve_decl(
        &mut self,
        decl: &lo_ast::Decl,
        module: Option<LocalDeclIdx>,
        cx: Context,
    ) -> Result<(), DefinitionError> {
        use lo_ast::BareDecl::*;

        let exp = match decl.attrs.get::<{ AttrName::Public }>() {
            Some(public) => match &public.bare.reach {
                Some(reach) => ExposureReach::PartiallyResolved(PartiallyResolvedPath {
                    // unwrap: root always has Exposure::Unrestricted, it won't reach this branch
                    namespace: module.unwrap(),
                    path: reach.clone(),
                })
                .into(),
                None => Exposure::Unrestricted,
            },
            None => match module {
                // a lack of `@public` means private i.e. restricted to `self` i.e. `@(public self)`
                Some(module) => ExposureReach::Resolved(module).into(),
                None => Exposure::Unrestricted,
            },
        };

        match &decl.bare {
            Func(func) => {
                let module = module.unwrap();

                let index = self.define(
                    func.binder,
                    exp,
                    decl.attrs.clone(),
                    EntityKind::FuncUntyped,
                    Some(module),
                )?;

                let binder = Ident::new(index.global(self.sess), func.binder);

                if let Some(intrinsic) = decl.attrs.get::<{ AttrName::Intrinsic }>()
                    && let Err(error) = self.sess.define_special(
                        special::Kind::Intrinsic,
                        binder,
                        match &intrinsic.bare.name {
                            Some(name) => special::DefinitionStyle::Explicit { name },
                            None => special::DefinitionStyle::Implicit {
                                namespace: Some(module),
                            },
                        },
                        intrinsic.span,
                    )
                {
                    error.handle(&mut *self);
                }
            }
            DataTy(ty) => {
                // there is always a root module
                let module = module.unwrap();

                // @Task don't return early, see analoguous code for modules
                let index = self.define(
                    ty.binder,
                    exp,
                    decl.attrs.clone(),
                    EntityKind::untyped_data_ty(),
                    Some(module),
                )?;

                let binder = Ident::new(index.global(self.sess), ty.binder);

                let known = decl.attrs.get::<{ AttrName::Known }>();
                if let Some(known) = known
                    && let Err(error) = self.sess.define_special(
                        special::Kind::Known,
                        binder,
                        match &known.bare.name {
                            Some(name) => special::DefinitionStyle::Explicit { name },
                            None => special::DefinitionStyle::Implicit { namespace: None },
                        },
                        known.span,
                    )
                {
                    error.handle(&mut *self);
                }

                if let Some(intrinsic) = decl.attrs.get::<{ AttrName::Intrinsic }>()
                    && let Err(error) = self.sess.define_special(
                        special::Kind::Intrinsic,
                        binder,
                        match &intrinsic.bare.name {
                            Some(name) => special::DefinitionStyle::Explicit { name },
                            None => special::DefinitionStyle::Implicit { namespace: None },
                        },
                        intrinsic.span,
                    )
                {
                    error.handle(&mut *self);
                }

                let health = &mut Health::Untainted;

                if let Some(constructors) = &ty.decls {
                    for constructor in constructors {
                        let transparency = match decl.attrs.has(AttrName::Abstract) {
                            true => Transparency::Abstract,
                            false => Transparency::Transparent,
                        };

                        self.start_resolve_decl(
                            constructor,
                            Some(module),
                            Context::DataDecl {
                                index,
                                transparency: Some(transparency),
                                known: known.map(|known| known.span),
                            },
                        )
                        .map_err(DefinitionError::into_inner)
                        .stain(health);
                    }
                }

                // @Beacon @Task get rid of unchecked call
                return Result::from(*health)
                    .map_err(|_| DefinitionError::Erased(ErasedReportedError::new_unchecked()));
            }
            Ctor(ctor) => {
                // there is always a root module
                let module = module.unwrap();
                let Context::DataDecl {
                    index: namespace,
                    transparency,
                    known,
                } = cx
                else {
                    unreachable!()
                };

                let exposure = match transparency.unwrap() {
                    Transparency::Transparent => self.sess[namespace].exp.clone(),
                    // as if a @(public super) was attached to the constructor
                    Transparency::Abstract => ExposureReach::Resolved(module).into(),
                };

                let index = self.define(
                    ctor.binder,
                    exposure,
                    decl.attrs.clone(),
                    EntityKind::CtorUntyped,
                    Some(namespace),
                )?;

                let binder = Ident::new(index.global(self.sess), ctor.binder);

                // @Task support `@(known name)` on constructors
                if let Some(known) = known
                    && let Err(error) = self.sess.define_special(
                        special::Kind::Known,
                        binder,
                        special::DefinitionStyle::Implicit {
                            namespace: Some(namespace),
                        },
                        known,
                    )
                {
                    error.handle(&mut *self);
                }
            }
            Module(submodule) => {
                // @Task @Beacon don't return early on error
                // @Note you need to create a fake index for this (an index which points to
                // a fake, nameless binding)
                let index = self.define(
                    submodule.binder,
                    exp,
                    // @Beacon @Bug this does not account for attributes found on the attribute header!
                    decl.attrs.clone(),
                    EntityKind::module(),
                    module,
                )?;

                let health = &mut Health::Untainted;

                for decl in &submodule.decls {
                    self.start_resolve_decl(decl, Some(index), Context::default())
                        .map_err(DefinitionError::into_inner)
                        .stain(health);
                }

                // @Beacon @Task get rid of unchecked call
                return Result::from(*health)
                    .map_err(|_| DefinitionError::Erased(ErasedReportedError::new_unchecked()));
            }
            Use(use_) => {
                // there is always a root module
                let module = module.unwrap();

                let index = self.define(
                    use_.binder,
                    exp,
                    decl.attrs.clone(),
                    EntityKind::UseUnres,
                    Some(module),
                )?;

                {
                    let previous = self.partially_resolved_use_bindings.insert(
                        index,
                        PartiallyResolvedUseBinding {
                            binder: index,
                            target: PartiallyResolvedPath {
                                namespace: module,
                                path: use_.target.clone(),
                            },
                        },
                    );

                    debug_assert!(previous.is_none());
                };
            }
            Error(_) => {}
        }

        Ok(())
    }

    /// Bind the given identifier to the given entity.
    fn define(
        &mut self,
        binder: ast::Ident,
        exp: Exposure,
        attrs: Attrs,
        binding: EntityKind,
        namespace: Option<LocalDeclIdx>,
    ) -> Result<LocalDeclIdx, DefinitionError> {
        if let Some(namespace) = namespace {
            if let Some(index) = self.sess[namespace]
                .namespace()
                .unwrap()
                .binders
                .iter()
                .map(|&index| index.local(self.sess).unwrap())
                .find(|&index| self.sess[index].src == binder)
            {
                let previous = &self.sess.comp().bindings[index].src;

                self.naming_conflicts
                    .entry(index)
                    .or_insert_with(|| smallvec![previous.span()])
                    .push(binder.span());

                // @Beacon @Bug that's not how new_unchecked
                // is supposed to be used! get rid of the ERE here! it's a lie!
                return Err(DefinitionError::ConflictingDefinition(
                    ErasedReportedError::new_unchecked(),
                ));
            }
        }

        let index = self.sess.define(Entity {
            src: binder,
            kind: binding,
            exp,
            attrs,
            parent: namespace,
        });

        if let Some(namespace) = namespace {
            let index = index.global(self.sess);
            self.sess[namespace]
                .namespace_mut()
                .unwrap()
                .binders
                .push(index);
        }

        Ok(index)
    }

    /// Completely resolve a lowered declaration to a declaration of the HIR.
    ///
    /// Tries to resolve all expressions and patterns contained within all declarations
    /// and actually builds the new HIR.
    ///
    /// This is the second major pass (but the third in total including the middle minor
    /// use-declaration resolving one).
    fn fin_resolve_decl(
        &mut self,
        decl: lo_ast::Decl,
        module: Option<LocalDeclIdx>,
        cx: Context,
    ) -> Option<hir::Decl> {
        use lo_ast::BareDecl::*;

        match decl.bare {
            Func(func) => {
                let module = module.unwrap();

                let binder = self
                    .as_ref()
                    .reobtain_resolved_ident::<target::Value>(func.binder, module);

                let ty_ann = self
                    .as_ref()
                    .resolve_expr(func.ty, &FunctionScope::Module(module))
                    .stain(&mut self.health);

                let expr = func.body.map(|expression| {
                    self.as_ref()
                        .resolve_expr(expression, &FunctionScope::Module(module))
                        .stain(&mut self.health)
                });

                Some(hir::Decl::new(
                    decl.attrs,
                    decl.span,
                    hir::Func {
                        binder,
                        ty: ty_ann,
                        body: expr,
                    }
                    .into(),
                ))
            }
            DataTy(ty) => {
                let module = module.unwrap();

                // @Beacon @Question wouldn't it be great if that method returned a
                // LocalDeclIdx instead of an Identifier?
                // or maybe even a *LocalIdentifier?
                let binder = self
                    .as_ref()
                    .reobtain_resolved_ident::<target::Value>(ty.binder, module);

                let ty_ann = self
                    .as_ref()
                    .resolve_expr(ty.ty, &FunctionScope::Module(module))
                    .stain(&mut self.health);

                let ctors = ty.decls.map(|ctors| {
                    ctors
                        .into_iter()
                        .filter_map(|ctor| {
                            self.fin_resolve_decl(
                                ctor,
                                Some(module),
                                Context::DataDecl {
                                    index: binder.local_decl_idx(self.sess).unwrap(),
                                    transparency: None,
                                    known: None,
                                },
                            )
                        })
                        .collect::<Vec<_>>()
                });

                Some(hir::Decl::new(
                    decl.attrs,
                    decl.span,
                    hir::DataTy {
                        binder,
                        ty: ty_ann,
                        ctors,
                    }
                    .into(),
                ))
            }
            Ctor(ctor) => {
                let module = module.unwrap();
                let Context::DataDecl {
                    index: namespace, ..
                } = cx
                else {
                    unreachable!()
                };

                let binder = self
                    .as_ref()
                    .reobtain_resolved_ident::<target::Value>(ctor.binder, namespace);

                let ty_ann = self
                    .as_ref()
                    .resolve_expr(ctor.ty, &FunctionScope::Module(module))
                    .stain(&mut self.health);

                Some(hir::Decl::new(
                    decl.attrs,
                    decl.span,
                    hir::Ctor { binder, ty: ty_ann }.into(),
                ))
            }
            Module(submodule) => {
                let index = match module {
                    // unwrap: could only ever be non-local if the binder was a use-binding
                    // but it is module binding
                    Some(module) => self
                        .as_ref()
                        .reobtain_resolved_ident::<target::Module>(submodule.binder, module)
                        .local(self.sess)
                        .unwrap(),
                    None => self.sess.comp().root_local(),
                };

                let decls = submodule
                    .decls
                    .into_iter()
                    .filter_map(|decl| self.fin_resolve_decl(decl, Some(index), Context::default()))
                    .collect();

                Some(hir::Decl::new(
                    decl.attrs,
                    decl.span,
                    hir::Module {
                        binder: Ident::new(index.global(self.sess), submodule.binder),
                        file: submodule.file,
                        decls,
                    }
                    .into(),
                ))
            }
            Use(_) => None,
            Error(error) => Some(PossiblyErroneous::error(error)),
        }
    }

    /// Resolve use-bindings.
    ///
    /// This is the second pass of three of the name resolver.
    ///
    /// This uses a queue to resolve use-bindings over and over until
    /// all out of order use-bindings are successfully resolved or until
    /// no progress can be made anymore in which case all remaining
    /// use-bindings are actually circular and are thus reported.
    // @Task update docs in regards to number of passes
    // @Task update docs regarding errors
    fn resolve_use_bindings(&mut self) {
        use ResolutionError::*;

        while !self.partially_resolved_use_bindings.is_empty() {
            let mut partially_resolved_use_bindings = HashMap::default();

            for (&index, item) in &self.partially_resolved_use_bindings {
                let namespace = item.target.namespace.global(self.sess);

                match self.as_ref().resolve_path::<target::Any>(
                    &item.target.path,
                    PathResolutionContext::new(namespace),
                ) {
                    Ok(target) => {
                        self.sess[index].kind = EntityKind::Use { target };
                    }
                    Err(error @ (UnresolvedBinding { .. } | Erased(_))) => {
                        self.sess[index].kind =
                            PossiblyErroneous::error(ErasedReportedError::new_unchecked());
                        let error = self.as_ref().report_resolution_error(error);
                        self.health.taint(error);
                    }
                    Err(UnresolvedUseBinding {
                        binder,
                        extra: _extra,
                    }) => {
                        partially_resolved_use_bindings.insert(
                            index,
                            PartiallyResolvedUseBinding {
                                // unwrap: The binder should always be local since external components
                                //         always contain resolved bindings.
                                binder: binder.local(self.sess).unwrap(),
                                target: item.target.clone(),
                            },
                        );
                    }
                }
            }

            // resolution stalled; therefore there are circular bindings
            if partially_resolved_use_bindings.len() == self.partially_resolved_use_bindings.len() {
                for &index in partially_resolved_use_bindings.keys() {
                    // @Beacon @Beacon @Beacon @Task don't use new_unchecked here!
                    self.sess[index].kind =
                        PossiblyErroneous::error(ErasedReportedError::new_unchecked());
                }

                for cycle in find_cycles(
                    &partially_resolved_use_bindings
                        .into_iter()
                        .map(|(index, binding)| (index, binding.binder))
                        .collect(),
                ) {
                    error::circular_declarations(cycle, self.sess).report(self.sess.rep());
                }

                // the loop above *has* to run at least once and report an error
                self.health.taint(ErasedReportedError::new_unchecked());
                break;
            }

            self.partially_resolved_use_bindings = partially_resolved_use_bindings;
        }

        self.partially_resolved_use_bindings.clear();
    }

    fn resolve_exposure_reaches(&mut self) {
        for (index, entity) in &self.sess.comp().bindings {
            if let Exposure::Restricted(exposure) = &entity.exposure {
                // unwrap: root always has Exposure::Unrestricted, it won't reach this branch
                let definition_site_namespace = entity.parent.unwrap().global(self.sess);

                if let Err(error) = self
                    .as_ref()
                    .resolve_restricted_exposure(exposure, definition_site_namespace)
                {
                    self.health.taint(error);
                    continue;
                };
            }

            if let EntityKind::Use {
                target: target_index,
            } = entity.kind
            {
                let target = &self.sess[target_index];

                if entity.exposure.compare(&target.exp, self.sess.comp()) == Some(Ordering::Greater)
                {
                    let error = Diag::error()
                        .code(ErrorCode::E009)
                        .message(format!(
                            "re-export of the more private binding ‘{}’",
                            self.sess.index_to_path(target_index)
                        ))
                        .span(entity.src, "re-exporting binding with greater exposure")
                        .label(target.src, "re-exported binding with lower exposure")
                        .note(format!(
                            "\
expected the exposure of ‘{}’
           to be at most {}
      but it actually is {}",
                            self.sess.local_index_to_path(index),
                            self.as_ref().display(&target.exp),
                            self.as_ref().display(&entity.exposure),
                        ))
                        .report(self.sess.rep());
                    self.health.taint(error);
                }
            }
        }
    }
}

impl Handler for &mut ResolverMut<'_, '_> {
    fn embed<T: PossiblyErroneous>(self, diag: Diag) -> T {
        let error = diag.report(self.sess.rep());
        self.health.taint(error);
        T::error(error)
    }
}

struct Resolver<'a> {
    sess: &'a Session<'a>,
}

impl<'a> Resolver<'a> {
    fn new(sess: &'a Session<'a>) -> Self {
        Self { sess }
    }

    fn resolve_expr(&self, expr: lo_ast::Expr, scope: &FunctionScope<'_>) -> Result<hir::Expr> {
        use lo_ast::BareExpr::*;

        Ok(match expr.bare {
            Ty => {
                // @Task don't use def-site span use use-site span
                // @Bug the span is not really a "user" (in the typer sense) but just a reference
                // @Task distinguish
                let ty = self
                    .sess
                    .require_special(special::Ty::Type, Some(expr.span))?;

                hir::Expr::new(expr.attrs, expr.span, hir::Binding(ty).into())
            }
            LocalBinding(level) => hir::Expr::new(
                expr.attrs,
                expr.span,
                hir::Binding(Ident::new(
                    scope.index_from_level(level),
                    ast::Ident::new_unchecked(expr.span, Atom::UNDERSCORE),
                ))
                .into(),
            ),
            Wildcard(_) => {
                return Err(Diag::error()
                    .message("wildcards are not supported yet")
                    .unlabeled_span(expr)
                    .report(self.sess.rep()))
            }
            Proj(proj) => hir::Expr::new(
                expr.attrs,
                expr.span,
                hir::Proj {
                    basis: self.resolve_expr(proj.basis, scope)?,
                    field: proj.field,
                }
                .into(),
            ),
            PiTy(pi_ty) => {
                let domain = self.resolve_expr(pi_ty.domain, scope);
                let codomain = match pi_ty.binder {
                    Some(binder) => {
                        self.resolve_expr(pi_ty.codomain, &scope.extend_with_param(binder))
                    }
                    None => self.resolve_expr(pi_ty.codomain, scope),
                };

                hir::Expr::new(
                    expr.attrs,
                    expr.span,
                    hir::PiTy {
                        domain: domain?,
                        codomain: codomain?,
                        kind: pi_ty.kind,
                        binder: pi_ty.binder.map(|binder| Ident::new(Index::Param, binder)),
                    }
                    .into(),
                )
            }
            App(app) => {
                let callee = self.resolve_expr(app.callee, scope);
                let arg = self.resolve_expr(app.arg, scope);

                hir::Expr::new(
                    expr.attrs,
                    expr.span,
                    hir::App {
                        callee: callee?,
                        arg: arg?,
                        kind: app.kind,
                    }
                    .into(),
                )
            }
            NumLit(num) => {
                self.resolve_num_lit(hir::Item::new(expr.attrs, expr.span, *num), scope)?
            }
            TextLit(text) => {
                self.resolve_text_lit(hir::Item::new(expr.attrs, expr.span, *text), scope)?
            }
            Path(path) => hir::Expr::new(
                expr.attrs,
                expr.span,
                hir::Binding(self.resolve_path_inside_func(&path, scope)?).into(),
            ),
            LamLit(lambda) => {
                let domain = lambda.domain.map(|ty| self.resolve_expr(ty, scope));
                let scope = match lambda.binder {
                    Some(binder) => Owned(scope.extend_with_param(binder)),
                    None => Borrowed(scope),
                };
                let scope = scope.as_ref();

                let codomain = lambda.codomain.map(|ty| self.resolve_expr(ty, scope));

                let body = self.resolve_expr(lambda.body, scope);

                hir::Expr::new(
                    expr.attrs,
                    expr.span,
                    hir::LamLit {
                        binder: lambda.binder.map(|binder| Ident::new(Index::Param, binder)),
                        domain: domain.transpose()?,
                        codomain: codomain.transpose()?,
                        body: body?,
                        kind: lambda.kind,
                    }
                    .into(),
                )
            }
            UseBinding => {
                return Err(Diag::error()
                    .message("use-bindings are not supported yet")
                    .unlabeled_span(&expr)
                    .report(self.sess.rep()));
            }
            CaseAnalysis(analysis) => {
                let scrutinee = self.resolve_expr(analysis.scrutinee, scope)?;
                let mut cases = Vec::new();

                for case in analysis.cases {
                    let (pattern, binders) = self.resolve_pat(case.pattern, scope)?;
                    let body =
                        self.resolve_expr(case.body, &scope.extend_with_pat_binders(binders))?;

                    cases.push(hir::Case { pat: pattern, body });
                }

                hir::Expr::new(
                    expr.attrs,
                    expr.span,
                    hir::CaseAnalysis { scrutinee, cases }.into(),
                )
            }
            SeqLit(seq) => {
                let mut lems = Vec::with_capacity(seq.elems.bare.len());
                let health = &mut Health::Untainted;

                for element in seq.elems.bare {
                    lems.push(self.resolve_expr(element, scope).stain(health));
                }

                Result::from(*health)?;

                self.resolve_seq_lit(
                    hir::Item::new(
                        expr.attrs,
                        expr.span,
                        ast::SeqLit {
                            path: seq.path,
                            elems: Spanned::new(seq.elems.span, lems),
                        },
                    ),
                    scope,
                )?
            }
            RecLit(rec) => {
                let path = rec.path.as_ref().ok_or_else(|| {
                    error::unqualified_literal("record", rec.fields.span).report(self.sess.rep())
                })?;

                // @Task maybe check if it points to a record!
                let ty = Spanned::new(
                    path.span(), // @Task use the span of the last segment for consistency
                    self.resolve_path_of_lit(path, rec.fields.span, scope)?,
                );

                if let Some(base) = rec.base {
                    let error = Diag::error()
                        .message("record literal bases are not supported yet")
                        .unlabeled_span(base.span)
                        .report(self.sess.rep());

                    let _ = self.resolve_expr(base, scope)?;

                    return Err(error);
                }

                let mut fields = Vec::with_capacity(rec.fields.bare.len());
                let health = &mut Health::Untainted;

                for field in rec.fields.bare {
                    fields.push(hir::Field {
                        binder: field.binder,
                        body: self.resolve_expr(field.body, scope).stain(health),
                    });
                }

                Result::from(*health)?;

                hir::Expr::new(expr.attrs, expr.span, hir::RecLit { ty, fields }.into())
            }
            Error(error) => PossiblyErroneous::error(error),
        })
    }

    // @Task use the Stain::stain instead of moving the try operator below resolve calls
    fn resolve_pat(
        &self,
        pat: lo_ast::Pat,
        scope: &FunctionScope<'_>,
    ) -> Result<(hir::Pat, Vec<ast::Ident>)> {
        use lo_ast::BarePattern::*;

        // @Task replace this hideous binders.append logic
        let mut binders: Vec<ast::Ident> = Vec::new();

        let pattern = match pat.bare {
            Wildcard(_) => {
                return Err(Diag::error()
                    .message("wildcards are not supported yet")
                    .unlabeled_span(pat)
                    .report(self.sess.rep()))
            }
            NumLit(num) => {
                self.resolve_num_lit(hir::Item::new(pat.attrs, pat.span, *num), scope)?
            }
            TextLit(text) => {
                self.resolve_text_lit(hir::Item::new(pat.attrs, pat.span, *text), scope)?
            }
            Path(path) => hir::Pat::new(
                pat.attrs,
                pat.span,
                hir::Binding(self.resolve_path_inside_func(&path, scope)?).into(),
            ),
            LetBinding(binder) => {
                if let ast::LocalBinder::Named(binder) = binder {
                    binders.push(binder);
                }

                hir::Pat::new(
                    pat.attrs,
                    pat.span,
                    binder.map(|binder| Ident::new(Index::Param, binder)).into(),
                )
            }
            App(app) => {
                let callee = self.resolve_pat(app.callee, scope);
                let arg = self.resolve_pat(app.arg, scope);

                let (callee, mut callee_binders) = callee?;
                let (arg, mut arg_binders) = arg?;

                binders.append(&mut callee_binders);
                binders.append(&mut arg_binders);

                hir::Pat::new(
                    pat.attrs,
                    pat.span,
                    hir::App {
                        callee,
                        kind: app.kind,
                        arg,
                    }
                    .into(),
                )
            }
            SeqLit(seq) => {
                let mut elems = Vec::new();

                for elem in seq.elems.bare {
                    // @Task use Stain::stain to not return early!
                    let (element, mut element_binders) = self.resolve_pat(elem, scope)?;
                    binders.append(&mut element_binders);
                    elems.push(element);
                }

                self.resolve_seq_lit(
                    hir::Item::new(
                        pat.attrs,
                        pat.span,
                        ast::SeqLit {
                            path: seq.path,
                            elems: Spanned::new(seq.elems.span, elems),
                        },
                    ),
                    scope,
                )?
            }
            Error(error) => PossiblyErroneous::error(error),
        };

        Ok((pattern, binders))
    }

    fn resolve_num_lit<T>(
        &self,
        number: hir::Item<ast::NumLit>,
        scope: &FunctionScope<'_>,
    ) -> Result<hir::Item<T>>
    where
        T: From<hir::NumLit>,
    {
        let ty = number
            .bare
            .path
            .as_ref()
            .map(|path| {
                Ok(Spanned::new(
                    path.span(), // @Task use the span of the last segment for consistency
                    self.resolve_path_of_lit(path, number.bare.lit.span, scope)?,
                ))
            })
            .transpose()?;

        let literal = &number.bare.lit;

        let ty = match ty {
            Some(ty) => self
                .sess
                .specials()
                .get(ty.bare)
                // @Task write more concisely
                .and_then(|intrinsic| obtain!(intrinsic, special::Binding::Ty(Ty::Num(ty)) => ty))
                .ok_or_else(|| {
                    self.literal_used_for_unsupported_ty(literal, "number", ty)
                        .report(self.sess.rep())
                })?,
            // for now, we default to `Nat` until we implement polymorphic number literals and their inference
            None => NumTy::Nat,
        };

        let Ok(resolved_number) = hir::NumLit::parse(literal.bare.to_str(), ty) else {
            return Err(Diag::error()
                .code(ErrorCode::E007)
                .message(format!(
                    "number literal ‘{literal}’ does not fit type ‘{ty}’",
                ))
                .unlabeled_span(literal)
                .note(format!(
                    "values of this type must fit integer interval {}",
                    ty.interval(),
                ))
                .report(self.sess.rep()));
        };

        Ok(number.remap(resolved_number.into()))
    }

    fn resolve_text_lit<T>(
        &self,
        text: hir::Item<ast::TextLit>,
        scope: &FunctionScope<'_>,
    ) -> Result<hir::Item<T>>
    where
        T: From<hir::TextLit>,
    {
        let ty = text
            .bare
            .path
            .as_ref()
            .map(|path| {
                Ok(Spanned::new(
                    path.span(), // @Task use the span of the last segment for consistency
                    self.resolve_path_of_lit(path, text.bare.lit.span, scope)?,
                ))
            })
            .transpose()?;

        let _ty = match ty {
            Some(ty) => self
                .sess
                .specials()
                .get(ty.bare)
                .filter(|&intrinsic| matches!(intrinsic, special::Binding::Ty(Ty::Text)))
                .ok_or_else(|| {
                    self.literal_used_for_unsupported_ty(text.bare.lit, "text", ty)
                        .report(self.sess.rep())
                })?,
            // for now, we default to `Text` until we implement polymorphic text literals and their inference
            None => Ty::Text.into(),
        };

        Ok(hir::Item::new(
            text.attrs,
            text.span,
            // @Beacon @Task avoid Atom::to_string
            hir::TextLit::Text(text.bare.lit.bare.to_string()).into(),
        ))
    }

    fn resolve_seq_lit<T>(
        &self,
        seq: hir::Item<ast::SeqLit<hir::Item<T>>>,
        scope: &FunctionScope<'_>,
    ) -> Result<hir::Item<T>>
    where
        T: Clone + From<hir::Binding> + From<hir::NumLit> + From<hir::App<hir::Item<T>>>,
    {
        // @Task test sequence literals inside of patterns!

        // @Task test this!
        let path = seq.bare.path.as_ref().ok_or_else(|| {
            error::unqualified_literal("sequence", seq.bare.elems.span).report(self.sess.rep())
        })?;

        let ty = Spanned::new(
            path.span(), // @Task use the span of the last segment for consistency
            self.resolve_path_of_lit(path, seq.bare.elems.span, scope)?,
        );

        // @Task test this!
        let ty = self
            .sess
            .specials()
            .get(ty.bare)
            .and_then(|known| obtain!(known, special::Binding::Ty(special::Ty::Seq(ty)) => ty))
            .ok_or_else(|| {
                self.literal_used_for_unsupported_ty(&seq.bare.elems, "sequence", ty)
                    .report(self.sess.rep())
            })?;

        match ty {
            SeqTy::List => self.resolve_list_lit(seq.bare.elems),
            SeqTy::Vector => self.resolve_vector_lit(seq.bare.elems),
            SeqTy::Tuple => self.resolve_tuple_lit(seq.bare.elems),
        }
    }

    fn resolve_list_lit<T>(&self, lems: Spanned<Vec<hir::Item<T>>>) -> Result<hir::Item<T>>
    where
        T: Clone + From<hir::Binding> + From<hir::App<hir::Item<T>>>,
    {
        let span = lems.span;
        let mut elems = lems.bare.into_iter();
        let Some(element_ty) = elems.next() else {
            return Err(Diag::error()
                .message("list literals cannot be empty")
                .note(
                    "due to limitations of the current type system, \
                     element types cannot be inferred and\n\
                     have to be manually supplied as the first “element”",
                )
                .unlabeled_span(span)
                .report(self.sess.rep()));
        };

        let empty = self
            .sess
            .require_special(special::Ctor::ListEmpty, Some(span))?;
        let prepend = self
            .sess
            .require_special(special::Ctor::ListPrepend, Some(span))?;

        // @Task check if all those attributes & spans make sense
        let mut result = hir::Item::common(
            span,
            hir::App {
                // @Task don't throw away attributes
                callee: hir::Item::common(span, hir::Binding(empty).into()),
                kind: Explicit,
                arg: element_ty.clone(),
            }
            .into(),
        );

        for elem in elems.rev() {
            // @Task check if all those attributes & spans make sense
            let prepend = hir::Item::common(
                elem.span,
                hir::App {
                    callee: hir::Item::common(elem.span, hir::Binding(prepend).into()),
                    kind: Explicit,
                    arg: element_ty.clone(),
                }
                .into(),
            );

            // @Task check if all those attributes & spans make sense
            result = hir::Item::common(
                elem.span,
                hir::App {
                    callee: hir::Item::common(
                        elem.span,
                        hir::App {
                            callee: prepend,
                            kind: Explicit,
                            arg: elem,
                        }
                        .into(),
                    ),
                    kind: Explicit,
                    arg: result,
                }
                .into(),
            );
        }

        Ok(result)
    }

    // @Task write UI tests
    fn resolve_vector_lit<T>(&self, elems: Spanned<Vec<hir::Item<T>>>) -> Result<hir::Item<T>>
    where
        T: Clone + From<hir::Binding> + From<hir::NumLit> + From<hir::App<hir::Item<T>>>,
    {
        let span = elems.span;
        let mut elems = elems.bare.into_iter();
        let Some(elem_ty) = elems.next() else {
            return Err(Diag::error()
                .message("vector literals cannot be empty")
                .note(
                    "due to limitations of the current type system, \
                     element types cannot be inferred and\n\
                     have to be manually supplied as the first “element”",
                )
                .unlabeled_span(span)
                .report(self.sess.rep()));
        };

        let empty = self
            .sess
            .require_special(special::Ctor::VectorEmpty, Some(span))?;
        let prepend = self
            .sess
            .require_special(special::Ctor::VectorPrepend, Some(span))?;

        // @Task check if all those attributes & spans make sense
        let mut result = hir::Item::common(
            span,
            hir::App {
                // @Task don't throw away attributes & span
                callee: hir::Item::common(span, hir::Binding(empty).into()),
                kind: Explicit,
                arg: elem_ty.clone(),
            }
            .into(),
        );

        for (length, elem) in elems.rev().enumerate() {
            // @Task check if all those attributes & spans make sense
            let prepend = hir::Item::common(
                elem.span,
                hir::App {
                    callee: hir::Item::common(
                        elem.span,
                        hir::App {
                            callee: hir::Item::common(elem.span, hir::Binding(prepend).into()),
                            kind: Explicit,
                            // @Beacon @Question What happens if the user does not define the intrinsic type `Nat`?
                            //                   Is that going to lead to crashes later on?
                            arg: hir::Item::common(
                                elem.span,
                                hir::NumLit::Nat(length.into()).into(),
                            ),
                        }
                        .into(),
                    ),
                    kind: Explicit,
                    arg: elem_ty.clone(),
                }
                .into(),
            );

            // @Task check if all those attributes & spans make sense
            result = hir::Item::common(
                elem.span,
                hir::App {
                    callee: hir::Item::common(
                        elem.span,
                        hir::App {
                            callee: prepend,
                            kind: Explicit,
                            arg: elem,
                        }
                        .into(),
                    ),
                    kind: Explicit,
                    arg: result,
                }
                .into(),
            );
        }

        Ok(result)
    }

    // @Task write UI tests
    fn resolve_tuple_lit<T>(&self, elems: Spanned<Vec<hir::Item<T>>>) -> Result<hir::Item<T>>
    where
        T: Clone + From<hir::Binding> + From<hir::App<hir::Item<T>>>,
    {
        if elems.bare.len() % 2 != 0 {
            return Err(Diag::error()
                .message("tuple literals cannot be empty")
                .note(
                    "due to limitations of the current type system, \
                     element types cannot be inferred and\n\
                     have to be manually supplied to the left of each element",
                )
                .unlabeled_span(elems.span)
                .report(self.sess.rep()));
        }

        let empty = self
            .sess
            .require_special(special::Ctor::TupleEmpty, Some(elems.span))?;
        let prepend = self
            .sess
            .require_special(special::Ctor::TuplePrepend, Some(elems.span))?;
        let ty = self
            .sess
            .require_special(special::Ty::Type, Some(elems.span))?;

        // @Task check if all those attributes & spans make sense
        let mut result = hir::Item::common(elems.span, hir::Binding(empty).into());
        let mut list = vec![hir::Item::common(elems.span, hir::Binding(ty).into())];

        for [elem_ty, element] in elems.bare.into_iter().array_chunks().rev() {
            // @Task check if all those attributes & spans make sense
            let prepend = hir::Item::common(
                element.span,
                hir::App {
                    callee: hir::Item::common(
                        element.span,
                        hir::App {
                            callee: hir::Item::common(element.span, hir::Binding(prepend).into()),
                            kind: Explicit,
                            arg: elem_ty.clone(),
                        }
                        .into(),
                    ),
                    kind: Explicit,
                    arg: self.resolve_list_lit(Spanned::new(element.span, list.clone()))?,
                }
                .into(),
            );

            // @Task find a cleaner approach
            list.insert(1, elem_ty);

            // @Task check if all those attributes & spans make sense
            result = hir::Item::common(
                element.span,
                hir::App {
                    callee: hir::Item::common(
                        element.span,
                        hir::App {
                            callee: prepend,
                            kind: Explicit,
                            arg: element,
                        }
                        .into(),
                    ),
                    kind: Explicit,
                    arg: result,
                }
                .into(),
            );
        }

        Ok(result)
    }

    fn literal_used_for_unsupported_ty(
        &self,
        literal: impl Spanning,
        name: &str,
        ty: Spanned<DeclIdx>,
    ) -> Diag {
        // @Task use correct terminology here: type vs. type constructor
        Diag::error()
            .code(ErrorCode::E043)
            .message(format!(
                "a {name} literal is not a valid constructor for type ‘{}’",
                self.sess[ty.bare].src
            ))
            .span(literal, "this literal may not construct the type")
            .label(ty, "the data type")
    }

    fn resolve_path_of_lit(
        &self,
        path: &ast::Path,
        literal: Span,
        scope: &FunctionScope<'_>,
    ) -> Result<DeclIdx> {
        let namespace = scope.module().global(self.sess);
        let binding = self
            .resolve_path::<target::Any>(path, PathResolutionContext::new(namespace))
            .map_err(|error| self.report_resolution_error(error))?;

        {
            let entity = &self.sess[binding];
            if !entity.is_data_ty() {
                // @Task code
                return Err(Diag::error()
                    .message(format!("binding ‘{path}’ is not a data type"))
                    // @Task future-proof a/an
                    .span(path, format!("a {}", entity.kind.name()))
                    .label(literal, "literal requires a data type as its namespace")
                    .report(self.sess.rep()));
            }
        }

        Ok(binding)
    }

    /// Resolve a syntactic path given a namespace.
    // @Task memoize by (path, namespace)
    fn resolve_path<Target: ResolutionTarget>(
        &self,
        path: &ast::Path,
        cx: PathResolutionContext,
    ) -> Result<Target::Output, ResolutionError> {
        if let Some(hanger) = &path.hanger {
            use ast::BareHanger::*;

            let namespace = match hanger.bare {
                Extern => {
                    let Some(comp) = path.segments.first() else {
                        // @Task improve the error message, code
                        return Err(Diag::error()
                            .message("path ‘extern’ is used in isolation")
                            .unlabeled_span(hanger)
                            .note("the path segment ‘extern’ is only to be used indirectly to refer to specific component")
                            .report(self.sess.rep()).into());
                    };

                    // @Beacon @Task add test for error case
                    let comp: Spanned<Word> = (*comp).try_into().map_err(|()| {
                        // @Task DRY @Question is the common code justified?
                        Diag::error()
                            .code(ErrorCode::E036)
                            .message(format!("the component name ‘{comp}’ is not a valid word"))
                            .unlabeled_span(comp)
                            .report(self.sess.rep())
                    })?;

                    let Some(&comp) = self.sess.comp().deps().get(&comp.bare) else {
                        // @Task If it's not a single source file, suggest adding to `dependencies` section in
                        // the package manifest
                        // @Task suggest similarly named dependencies!
                        // @Task check if it's a transitive dependency
                        // with the *same* name and add the note that they (trans deps) have to be
                        // explicitly added to the deps list to be referenceable in this component
                        // @Task check if it is a sublibrary that was not explicitly added and suggest doing so
                        // @Task better phrasing
                        return Err(Diag::error()
                            .message(format!("the component ‘{comp}’ is not defined"))
                            .unlabeled_span(comp)
                            .report(self.sess.rep())
                            .into());
                    };

                    let comp = &self.sess.look_up_comp(comp);
                    let root = comp.root();

                    return match &*path.segments {
                        &[ident] => Ok(Target::output(root, ident)),
                        [_, idet @ ..] => self.resolve_path::<Target>(
                            // @Task use PathView once available
                            &ast::Path::unhung(idet.to_owned().into()),
                            PathResolutionContext::new(root)
                                .origin_namespace(cx.origin_namespace)
                                .qualified_identifier(),
                        ),
                        [] => unreachable!(),
                    };
                }
                Topmost => self.sess.comp().root(),
                Super => self
                    .resolve_super(hanger, cx.namespace.local(self.sess).unwrap())?
                    .global(self.sess),
                Self_ => cx.namespace,
            };

            return if path.segments.is_empty() {
                Target::output_bare_path_hanger(hanger, namespace)
                    .map_err(|error| error.report(self.sess.rep()).into())
            } else {
                self.resolve_path::<Target>(
                    // @Task use PathView once available
                    &ast::Path::unhung(path.segments.clone()),
                    cx.namespace(namespace).qualified_identifier(),
                )
            };
        }

        let index = self.resolve_ident(path.segments[0], cx)?;
        let entity = &self.sess[index];

        match &*path.segments {
            &[ident] => {
                Target::validate_ident(ident, entity)
                    .map_err(|error| error.report(self.sess.rep()))?;
                Ok(Target::output(index, ident))
            }
            [ident, idents @ ..] => {
                if entity.is_namespace() {
                    self.resolve_path::<Target>(
                        // @Task use PathView once available
                        &ast::Path::unhung(idents.to_owned().into()),
                        cx.namespace(index).qualified_identifier(),
                    )
                } else if entity.is_error() {
                    // @Task add rationale why `last`
                    Ok(Target::output(index, *idents.last().unwrap()))
                } else {
                    let diagnostic = self.attempt_to_access_subbinder_of_non_namespace_error(
                        *ident,
                        &entity.kind,
                        cx.namespace,
                        *idents.first().unwrap(),
                    );
                    Err(diagnostic.report(self.sess.rep()).into())
                }
            }
            [] => unreachable!(),
        }
    }

    fn resolve_super(&self, hanger: &ast::Hanger, module: LocalDeclIdx) -> Result<LocalDeclIdx> {
        self.sess[module].parent.ok_or_else(|| {
            Diag::error()
                .code(ErrorCode::E021) // @Question use a dedicated code?
                .message("the root module does not have a parent module")
                .unlabeled_span(hanger)
                .report(self.sess.rep())
        })
    }

    fn resolve_ident(
        &self,
        ident: ast::Ident,
        cx: PathResolutionContext,
    ) -> Result<DeclIdx, ResolutionError> {
        let index = self.sess[cx.namespace]
            .namespace()
            .unwrap()
            .binders
            .iter()
            .copied()
            .find(|&index| self.sess[index].src == ident)
            .ok_or(ResolutionError::UnresolvedBinding {
                ident,
                namespace: cx.namespace,
                usage: cx.usage,
            })?;

        // @Temporary hack until we can manage cyclic exposure reaches!
        if cx.validate_exposure {
            self.handle_exposure(index, ident, cx.origin_namespace)?;
        }

        if !cx.allow_deprecated
            && let Some(deprecated) = self.sess[index].attrs.get::<{ AttrName::Deprecated }>()
        {
            let mut message = format!(
                "use of deprecated binding ‘{}’",
                self.sess.index_to_path(index),
            );

            if let Some(reason) = &deprecated.bare.reason {
                message += ": ";
                message += reason.to_str();
            }

            Diag::warning()
                .code(LintCode::Deprecated)
                .message(message)
                .unlabeled_span(ident)
                .report(self.sess.rep());
        }

        self.collapse_use_chain(index, ident.span())
    }

    // @Task verify that the exposure is checked even in the case of use-declarations
    // using use-bindings (use-chains).
    fn handle_exposure(
        &self,
        index: DeclIdx,
        ident: ast::Ident,
        origin: DeclIdx,
    ) -> Result<(), ResolutionError> {
        let entity = &self.sess[index];

        if let Exposure::Restricted(exposure) = &entity.exp {
            // unwrap: root always has Exposure::Unrestricted
            let definition_site_namespace = entity.parent.unwrap();
            let reach = self.resolve_restricted_exposure(
                exposure,
                definition_site_namespace.global(self.sess),
            )?;

            if !self.sess.comp().is_allowed_to_access(
                origin,
                definition_site_namespace.global(self.sess),
                reach,
            ) {
                return Err(Diag::error()
                    .code(ErrorCode::E029)
                    .message(format!(
                        "binding ‘{}’ is private",
                        self.sess.index_to_path(index)
                    ))
                    .unlabeled_span(ident)
                    .report(self.sess.rep())
                    .into());
            }
        }

        Ok(())
    }

    /// Collapse chain of use-bindings aka indirect uses.
    ///
    /// This is an invariant established to make things easier to reason about during resolution.
    fn collapse_use_chain(&self, index: DeclIdx, extra: Span) -> Result<DeclIdx, ResolutionError> {
        use EntityKind::*;

        match self.sess[index].kind {
            Use { target } => Ok(target),
            UseUnres => Err(ResolutionError::UnresolvedUseBinding {
                binder: index,
                extra,
            }),
            _ => Ok(index),
        }
    }

    fn resolve_restricted_exposure(
        &self,
        exp: &Mutex<ExposureReach>,
        def_site_namespace: DeclIdx,
    ) -> Result<DeclIdx> {
        let expr_ = exp.lock().unwrap();

        Ok(match &*expr_ {
            ExposureReach::PartiallyResolved(PartiallyResolvedPath { namespace, path }) => {
                // @Task Use `resolve_path` once we can detect cyclic exposure (would close #67 #68)
                //       and remove `CheckExposure`.
                // @Task Try to obtain and store a partially resolved path on resolution failure.
                //       I think this is the way to achieve the first task.
                // @Note This could maybe also allow us to report more details (more highlights)
                //       for cyclic exposure reaches. Improving output for test
                //       name-resolution/exposure/indirect-cycle
                let reach = self
                    .resolve_path::<target::Module>(
                        path,
                        PathResolutionContext::new(namespace.global(self.sess)).ignore_exposure(),
                    )
                    .map_err(|error| self.report_resolution_error(error))?;

                let reach_is_ancestor = self
                    .sess
                    .comp()
                    .some_ancestor_equals(def_site_namespace, reach);

                if !reach_is_ancestor {
                    return Err(Diag::error()
                        .code(ErrorCode::E037)
                        .message("exposure can only be restricted to ancestor modules")
                        .unlabeled_span(path)
                        .report(self.sess.rep()));
                }

                drop(expr_);
                *exp.lock().unwrap() =
                    // @Question unwrap() correct?
                    ExposureReach::Resolved(reach.local(self.sess).unwrap());

                reach
            }
            &ExposureReach::Resolved(reach) => reach.global(self.sess),
        })
    }

    /// Reobtain the resolved identifier.
    ///
    /// Used in [`ResolverMut::finish_resolve_declaration`], the last pass of the
    /// name resolver, to re-gain some information (the [`Identifier`]s) collected
    /// during the first pass.
    ///
    /// This way, [`ResolverMut::start_resolve_declaration`] does not need to return
    /// a new intermediate representation being a representation between the
    /// Lo-AST and the HIR where all the _binders_ of declarations are resolved
    /// (i.e. are [`Identifier`]s) but all _bindings_ (in type annotations, expressions, …)
    /// are still unresolved (i.e. are [`ast::Identifier`]s).
    ///
    /// Such an IR would imply writing a lot of boilerplate if we were to duplicate
    /// definitions & mappings or – if even possible – creating a totally complicated
    /// parameterized Lo-AST with complicated traits having many associated types
    /// (painfully learned through previous experiences).
    // @Task add to documentation that this panics on unresolved and does not check exposure,
    // also it does not check the resolution target etc
    // @Task add that it may only fail if circular use-bindings were found or a use
    // binding could not be resolved since `Self::resolve_use_binding` is treated non-fatally
    // in Resolver::resolve_declaration
    fn reobtain_resolved_ident<Target: ResolutionTarget>(
        &self,
        ident: ast::Ident,
        namespace: LocalDeclIdx,
    ) -> Target::Output {
        let index = self.sess[namespace]
            .namespace()
            .unwrap()
            .binders
            .iter()
            .map(|index| index.local(self.sess).unwrap())
            .find(|&index| self.sess[index].src == ident)
            .unwrap();
        let index = self
            .collapse_use_chain(index.global(self.sess), ident.span())
            .unwrap_or_else(|_| unreachable!());

        Target::output(index, ident)
    }

    /// Resolve a path inside of a function.
    fn resolve_path_inside_func(
        &self,
        query: &ast::Path,
        scope: &FunctionScope<'_>,
    ) -> Result<Ident> {
        self.resolve_path_inside_func_with_depth(query, scope, 0, scope)
    }

    /// Resolve a path inside of a function given a depth.
    ///
    /// The `depth` is necessary for the recursion to successfully create de Bruijn indices.
    ///
    /// The `origin` signifies the innermost function scope from where the resolution was first requested.
    /// This information is used for diagnostics, namely typo flagging where we once again start at the origin
    /// and walk back out.
    fn resolve_path_inside_func_with_depth(
        &self,
        query: &ast::Path,
        scope: &FunctionScope<'_>,
        depth: usize,
        origin: &FunctionScope<'_>,
    ) -> Result<Ident> {
        use FunctionScope::*;

        if let (false, Some(ident)) = (matches!(scope, Module(_)), query.ident_head()) {
            match scope {
                FuncParam { parent, binder } => {
                    if *binder == ident {
                        Ok(Ident::new(DeBruijnIdx(depth), ident))
                    } else {
                        self.resolve_path_inside_func_with_depth(query, parent, depth + 1, origin)
                    }
                }
                PatBinders { parent, binders } => {
                    match binders
                        .iter()
                        .rev()
                        .zip(depth..)
                        .find(|(&binder, _)| binder == ident)
                    {
                        Some((_, depth)) => Ok(Ident::new(DeBruijnIdx(depth), ident)),
                        None => self.resolve_path_inside_func_with_depth(
                            query,
                            parent,
                            depth + binders.len(),
                            origin,
                        ),
                    }
                }
                Module(_) => unreachable!(),
            }
        } else {
            self.resolve_path::<target::Value>(
                query,
                PathResolutionContext::new(scope.module().global(self.sess)),
            )
            .map_err(|error| {
                self.report_resolution_error_searching_lookalikes(error, |ident, _| {
                    self.find_similarly_named(origin, ident.to_str())
                })
            })
        }
    }

    /// Find a similarly named binding in the same namespace.
    ///
    /// Used for error reporting when an undefined binding was encountered.
    /// In the future, we might decide to find not one but several similar names
    /// but that would be computationally heavier.
    // @Beacon @Task don't suggest private bindings!
    fn find_similarly_named_decl(
        &self,
        ident: &str,
        pred: impl Fn(&Entity) -> bool,
        namespace: DeclIdx,
    ) -> Option<Atom> {
        self.sess[namespace]
            .namespace()
            .unwrap()
            .binders
            .iter()
            .map(|&index| &self.sess[index])
            .filter(|entity| !entity.is_error() && pred(entity))
            .map(|entity| entity.src.bare())
            .find(|some_identifier| is_similar(some_identifier.to_str(), ident))
    }

    /// Find a similarly named binding in the scope.
    ///
    /// With "scope", it is meant to include parent scopes, too.
    ///
    /// Used for error reporting when an undefined binding was encountered.
    /// In the future, we might decide to find not one but several similar names
    /// but that would be computationally heavier and we would need to be careful
    /// and consider the effects of shadowing.
    fn find_similarly_named<'s>(
        &'s self,
        scope: &'s FunctionScope<'_>,
        ident: &str,
    ) -> Option<Atom> {
        use FunctionScope::*;

        match scope {
            &Module(module) => {
                self.find_similarly_named_decl(ident, |_| true, module.global(self.sess))
            }
            FuncParam { parent, binder } => {
                if is_similar(ident, binder.to_str()) {
                    Some(binder.bare())
                } else {
                    self.find_similarly_named(parent, ident)
                }
            }
            PatBinders { parent, binders } => {
                if let Some(binder) = binders
                    .iter()
                    .rev()
                    .find(|binder| is_similar(ident, binder.to_str()))
                {
                    Some(binder.bare())
                } else {
                    self.find_similarly_named(parent, ident)
                }
            }
        }
    }

    fn report_resolution_error(&self, error: ResolutionError) -> ErasedReportedError {
        self.report_resolution_error_searching_lookalikes(error, |ident, namespace| {
            self.find_similarly_named_decl(ident.to_str(), |_| true, namespace)
        })
    }

    #[allow(clippy::needless_pass_by_value)] // by design
    fn report_resolution_error_searching_lookalikes(
        &self,
        error: ResolutionError,
        lookalike_finder: impl FnOnce(Atom, DeclIdx) -> Option<Atom>,
    ) -> ErasedReportedError {
        match error {
            ResolutionError::Erased(error) => error,
            ResolutionError::UnresolvedBinding {
                ident,
                namespace,
                usage,
            } => {
                let mut message = format!("the binding ‘{ident}’ is not defined in ");

                match usage {
                    IdentifierUsage::Unqualified => message += "this scope",
                    IdentifierUsage::Qualified => {
                        if namespace == self.sess.comp().root() {
                            message += "the root module";
                        } else {
                            message += match self.sess[namespace].is_module() {
                                true => "module",
                                false => "namespace",
                            };
                            message += " ‘";
                            message += &self.sess.index_to_path(namespace);
                            message += "’";
                        }
                    }
                }

                Diag::error()
                    .code(ErrorCode::E021)
                    .message(message)
                    .unlabeled_span(ident)
                    .with(|error| {
                        let ident = ident.bare();

                        match lookalike_finder(ident, namespace) {
                            Some(lookalike) => error.help(format!(
                                "a binding with a similar name exists in scope: {}",
                                Lookalike {
                                    actual: ident,
                                    lookalike,
                                },
                            )),
                            None => error,
                        }
                    })
                    .report(self.sess.rep())
            }
            // @Beacon @Bug we cannot just assume this is circular exposure reach,
            // this method is a general resolution error formatter!!!!
            ResolutionError::UnresolvedUseBinding { binder, extra } => {
                // @Beacon @Task
                Diag::error()
                    .message(format!(
                        "exposure reach ‘{}’ is circular",
                        self.sess.index_to_path(binder)
                    ))
                    .unlabeled_span(extra)
                    .report(self.sess.rep())
            }
        }
    }

    // @Question parent: *Local*DeclIdx?
    fn attempt_to_access_subbinder_of_non_namespace_error(
        &self,
        binder: ast::Ident,
        kind: &EntityKind,
        parent: DeclIdx,
        subbinder: ast::Ident,
    ) -> Diag {
        // @Question should we also include lookalike namespaces that don't contain the
        // subbinding (displaying them in a separate help message?)?
        let similarly_named_namespace = self.find_similarly_named_decl(
            binder.to_str(),
            |entity| {
                entity.namespace().map_or(false, |namespace| {
                    namespace
                        .binders
                        .iter()
                        .any(|&index| self.sess[index].src == subbinder)
                })
            },
            parent,
        );

        let show_very_general_help = similarly_named_namespace.is_none();

        Diag::error()
            .code(ErrorCode::E017)
            .message(format!("binding ‘{binder}’ is not a namespace"))
            .span(binder, format!("not a namespace but a {}", kind.name()))
            .label(
                // the subbinder together with the leading dot
                // @Task trim_start_matches ascii_whitespace
                binder.span().end().merge(&subbinder),
                "denotes a reference to a binding inside of a namespace",
            )
            .with(|it| match similarly_named_namespace {
                Some(lookalike) => it.help(format!(
                "a namespace with a similar name exists in scope containing the binding:\n    {}",
                Lookalike {
                    actual: binder.bare(),
                    lookalike
                },
            )),
                None => it,
            })
            .with(|it| {
                if show_very_general_help {
                    // no type information here yet to check if the non-namespace is indeed a record
                    it.help("use ‘::’ to reference a field of a record")
                } else {
                    it
                }
            })
    }

    fn display<'f>(&'f self, value: &'f impl Display) -> impl std::fmt::Display + 'f {
        displayed(|f| value.write(self.sess, f))
    }
}

pub trait ProgramEntryExt {
    fn look_up_program_entry(&self) -> Option<Ident>;
}

impl ProgramEntryExt for Session<'_> {
    fn look_up_program_entry(&self) -> Option<Ident> {
        let resolver = Resolver::new(self);

        let binder = ast::Ident::new_unchecked(default(), PROGRAM_ENTRY);
        let index = resolver
            .resolve_ident(
                binder,
                PathResolutionContext::new(self.comp().root())
                    .ignore_exposure()
                    .allow_deprecated(),
            )
            .ok()?;
        let entity = &self[index];

        if !entity.is_func() {
            return None;
        }

        Some(Ident::new(index, entity.src))
    }
}

#[derive(Default, Debug, Clone, Copy)]
enum Context {
    #[default]
    Decl,
    DataDecl {
        index: LocalDeclIdx,
        transparency: Option<Transparency>,
        known: Option<Span>,
    },
}

#[derive(Debug, Clone, Copy)]
enum Transparency {
    Transparent,
    Abstract,
}

#[derive(Clone, Copy)]
struct PathResolutionContext {
    namespace: DeclIdx,
    origin_namespace: DeclIdx,
    usage: IdentifierUsage,
    validate_exposure: bool,
    allow_deprecated: bool,
}

impl PathResolutionContext {
    fn new(namespace: DeclIdx) -> Self {
        Self {
            namespace,
            origin_namespace: namespace,
            usage: IdentifierUsage::Unqualified,
            validate_exposure: true,
            allow_deprecated: false,
        }
    }

    fn namespace(self, namespace: DeclIdx) -> Self {
        Self { namespace, ..self }
    }

    fn origin_namespace(self, origin_namespace: DeclIdx) -> Self {
        Self {
            origin_namespace,
            ..self
        }
    }

    fn ignore_exposure(self) -> Self {
        Self {
            validate_exposure: false,
            ..self
        }
    }

    fn allow_deprecated(self) -> Self {
        Self {
            allow_deprecated: true,
            ..self
        }
    }

    fn qualified_identifier(self) -> Self {
        Self {
            usage: IdentifierUsage::Qualified,
            ..self
        }
    }
}

/// A use-binding whose target is not fully resolved.
struct PartiallyResolvedUseBinding {
    binder: LocalDeclIdx,
    target: PartiallyResolvedPath,
}

impl fmt::Debug for PartiallyResolvedUseBinding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(use {:?} as {:?})", self.target, self.binder)
    }
}

/// If an identifier is used unqualified or qualified.
///
/// Exclusively used for error reporting.
#[derive(Clone, Copy)]
enum IdentifierUsage {
    Qualified,
    Unqualified,
}

trait ComponentExt {
    fn some_ancestor_equals(&self, index: DeclIdx, namespace: DeclIdx) -> bool;

    /// Indicate if the definition-site namespace can be accessed from the given namespace.
    fn is_allowed_to_access(
        &self,
        namespace: DeclIdx,
        definition_site_namespace: DeclIdx,
        reach: DeclIdx,
    ) -> bool {
        self.some_ancestor_equals(namespace, definition_site_namespace) // access from same namespace or below
                || self.some_ancestor_equals(namespace, reach) // access from above in reach
    }
}

impl ComponentExt for Comp {
    // @Beacon @Question shouldn't `index` be a LocalDeclIdx?
    fn some_ancestor_equals(&self, mut index: DeclIdx, namespace: DeclIdx) -> bool {
        loop {
            if index == namespace {
                break true;
            }

            // @Beacon @Question can this ever panic?
            index = match self[index.local(self).unwrap()].parent {
                Some(parent) => parent.global(self),
                None => break false,
            }
        }
    }
}

// @Task better name
trait ExposureCompare {
    fn compare(&self, other: &Self, component: &Comp) -> Option<Ordering>;
}

impl ExposureCompare for Exposure {
    fn compare(&self, other: &Self, component: &Comp) -> Option<Ordering> {
        use Exposure::*;

        match (self, other) {
            (Unrestricted, Unrestricted) => Some(Ordering::Equal),
            (Unrestricted, Restricted(_)) => Some(Ordering::Greater),
            (Restricted(_), Unrestricted) => Some(Ordering::Less),
            (Restricted(this), Restricted(other)) => {
                let this = this.lock().unwrap();
                let other = other.lock().unwrap();
                this.compare(&other, component)
            }
        }
    }
}

impl ExposureCompare for ExposureReach {
    fn compare(&self, other: &Self, component: &Comp) -> Option<Ordering> {
        Some(match (self, other) {
            (&ExposureReach::Resolved(this), &ExposureReach::Resolved(other)) => {
                let this = this.global(component);
                let other = other.global(component);

                if this == other {
                    Ordering::Equal
                } else if component.some_ancestor_equals(other, this) {
                    Ordering::Greater
                } else if component.some_ancestor_equals(this, other) {
                    Ordering::Less
                } else {
                    return None;
                }
            }
            // @Question can we be smarter here? do we need to be smarter here?
            _ => return None,
        })
    }
}

enum FunctionScope<'a> {
    Module(LocalDeclIdx),
    FuncParam {
        parent: &'a Self,
        binder: ast::Ident,
    },
    PatBinders {
        parent: &'a Self,
        binders: Vec<ast::Ident>,
    },
}

impl<'a> FunctionScope<'a> {
    fn extend_with_param(&'a self, binder: ast::Ident) -> Self {
        Self::FuncParam {
            parent: self,
            binder,
        }
    }

    fn extend_with_pat_binders(&'a self, binders: Vec<ast::Ident>) -> Self {
        Self::PatBinders {
            parent: self,
            binders,
        }
    }

    fn module(&self) -> LocalDeclIdx {
        match self {
            &Self::Module(module) => module,
            Self::FuncParam { parent, .. } | Self::PatBinders { parent, .. } => parent.module(),
        }
    }

    fn depth(&self) -> usize {
        match self {
            Self::Module(_) => 0,
            Self::FuncParam { parent, .. } => 1 + parent.depth(),
            Self::PatBinders { parent, binders } => binders.len() + parent.depth(),
        }
    }

    fn index_from_level(&self, DeBruijnLevel(level): DeBruijnLevel) -> DeBruijnIdx {
        DeBruijnIdx(self.depth() - 1 + level)
    }
}

fn is_similar(ident: &str, other_ident: &str) -> bool {
    strsim::levenshtein(other_ident, ident) <= std::cmp::max(ident.len(), 3) / 3
}

struct Lookalike {
    actual: Atom,
    lookalike: Atom,
}

impl Lookalike {
    fn render(&self, p: &mut Painter) -> io::Result<()> {
        use difference::{Changeset, Difference};
        use std::io::Write;

        let actual = self.actual.to_str();
        let changeset = Changeset::new(actual, self.lookalike.to_str(), "");
        let mut purely_additive = true;

        write!(p, "‘")?;

        for difference in &changeset.diffs {
            match difference {
                Difference::Same(segment) => write!(p, "{segment}")?,
                Difference::Add(segment) => {
                    p.set(Effects::BOLD)?;
                    write!(p, "{segment}")?;
                    p.unset()?;
                }
                Difference::Rem(_) => {
                    purely_additive = false;
                }
            }
        }

        write!(p, "’")?;

        if !(purely_additive || actual.width() == 1 && changeset.distance == 2) {
            write!(p, " (")?;
            changeset.render(p)?;
            write!(p, ")")?;
        }

        Ok(())
    }
}

// FIXME: Remove this impl since it creates a fresh `Painter` and thus violates the
//        layers of abstraction. It can lead to broken output due to nested styles
//        not being properly handled. Further, this painter has no way of respecting
//        `--color`.
//        Instead, the diagnostics API should expose a method that provides the
//        underlying painter and callers of this impl should use `render` directly
//        instead.
impl fmt::Display for Lookalike {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&paint_to_string(|painter| self.render(painter), ColorChoice::Auto).unwrap())
    }
}

fn module_used_as_a_value_error(module: Spanned<impl fmt::Display>) -> Diag {
    // @Task levenshtein-search for similar named bindings which are in fact values and suggest the first one
    // @Task print absolute path of the module in question and use highlight the entire path, not just the last
    // segment
    // @Task improve this diagnostic!
    Diag::error()
            .code(ErrorCode::E023)
            .message(format!("module ‘{module}’ is used as a value"))
            .unlabeled_span(module)
            .help("modules are not first-class citizens, consider utilizing records for such cases instead")
}

pub enum DefinitionError {
    /// Some opaque error that was already reported.
    Erased(ErasedReportedError),
    /// Definitions conflicting in name were found.
    ///
    /// Details about this error are **not stored here** but in the
    /// resolver state to allow grouping.
    ConflictingDefinition(ErasedReportedError),
}

impl DefinitionError {
    // cannot be a From impl since apparently rustc would be unable to infer it
    fn into_inner(self) -> ErasedReportedError {
        match self {
            Self::Erased(error) | Self::ConflictingDefinition(error) => error,
        }
    }
}

impl From<ErasedReportedError> for DefinitionError {
    fn from(error: ErasedReportedError) -> Self {
        Self::Erased(error)
    }
}

/// Specifies behavior for resolving different sorts of entities.
///
/// Right now, it's only about the difference between values and modules
/// since modules are not values (non-first-class). As such, this trait
/// allows to implementors to define what should happen with the resolved entity
/// if it appears in a specific location
// @Task get rid of this thing! smh! **esp. the assoc ty `Output`!**
trait ResolutionTarget {
    type Output;

    fn output(index: DeclIdx, ident: ast::Ident) -> Self::Output;

    fn output_bare_path_hanger(hanger: &ast::Hanger, index: DeclIdx) -> Result<Self::Output, Diag>;

    fn validate_ident(ident: ast::Ident, entity: &Entity) -> Result<(), Diag>;
}

mod target {
    #[allow(clippy::wildcard_imports)] // private inline module
    use super::*;

    pub(super) enum Any {}

    impl ResolutionTarget for Any {
        type Output = DeclIdx;

        fn output(index: DeclIdx, _: ast::Ident) -> Self::Output {
            index
        }

        fn output_bare_path_hanger(_: &ast::Hanger, index: DeclIdx) -> Result<Self::Output, Diag> {
            Ok(index)
        }

        fn validate_ident(_: ast::Ident, _: &Entity) -> Result<(), Diag> {
            Ok(())
        }
    }

    /// Marker to specify to only resolve to values.
    pub(super) enum Value {}

    impl ResolutionTarget for Value {
        type Output = Ident;

        fn output(index: DeclIdx, ident: ast::Ident) -> Self::Output {
            Ident::new(index, ident)
        }

        fn output_bare_path_hanger(hanger: &ast::Hanger, _: DeclIdx) -> Result<Self::Output, Diag> {
            Err(module_used_as_a_value_error(hanger.as_ref()))
        }

        fn validate_ident(ident: ast::Ident, entity: &Entity) -> Result<(), Diag> {
            if entity.is_module() {
                return Err(module_used_as_a_value_error(ident.into_inner()));
            }

            Ok(())
        }
    }

    pub(super) enum Module {}

    impl ResolutionTarget for Module {
        type Output = DeclIdx;

        fn output(index: DeclIdx, _: ast::Ident) -> Self::Output {
            index
        }

        fn output_bare_path_hanger(_: &ast::Hanger, index: DeclIdx) -> Result<Self::Output, Diag> {
            Ok(index)
        }

        fn validate_ident(ident: ast::Ident, entity: &Entity) -> Result<(), Diag> {
            // @Task print absolute path!
            if !(entity.is_module() || entity.is_error()) {
                return Err(Diag::error()
                    .code(ErrorCode::E022)
                    .message(format!("binding ‘{ident}’ is not a module"))
                    .unlabeled_span(ident));
            }

            Ok(())
        }
    }
}

/// A possibly recoverable error that cab emerge during resolution.
enum ResolutionError {
    /// Some opaque error that was already reported.
    Erased(ErasedReportedError),
    UnresolvedBinding {
        ident: ast::Ident,
        namespace: DeclIdx,
        usage: IdentifierUsage,
    },
    UnresolvedUseBinding {
        binder: DeclIdx,
        extra: Span,
    },
}

impl From<ErasedReportedError> for ResolutionError {
    fn from(error: ErasedReportedError) -> Self {
        Self::Erased(error)
    }
}

mod error {
    #[allow(clippy::wildcard_imports)] // private inline module
    use super::*;
    use utility::cycle::Cycle;

    pub(super) fn confliction_definitions(binder: ast::Ident, conflicts: &[Span]) -> Diag {
        Diag::error()
            .code(ErrorCode::E020)
            .message(format!(
                "‘{binder}’ is defined multiple times in this scope"
            ))
            .spans(conflicts, "conflicting definition")
    }

    // @Note hmm, taking a Session is not really in the spirit of those error fns
    // but the alternative wouldn't be performant
    pub(super) fn circular_declarations(
        cycle: Cycle<'_, LocalDeclIdx>,
        sess: &Session<'_>,
    ) -> Diag {
        let paths = cycle
            .iter()
            .map(|&&index| sess.local_index_to_path(index).quote())
            .list(Conjunction::And);

        Diag::error()
            .code(ErrorCode::E024)
            .message(format!(
                "the {} {paths} {} circular",
                pluralize!(cycle.len(), "declaration"),
                pluralize!(cycle.len(), "is", "are"),
            ))
            .unlabeled_spans(cycle.into_iter().map(|&index| sess[index].src.span()))
    }

    pub(super) fn unqualified_literal(kind: &'static str, span: Span) -> Diag {
        Diag::error()
            .message(format!(
                "{kind} literals without explicit type are not supported yet"
            ))
            .unlabeled_span(span)
            .help("consider prefixing the literal with a path to a type followed by a ‘.’")
    }
}
