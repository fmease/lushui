//! The name resolver.
//!
//! It traverses the [Lo-AST](lo_ast) and registers/defines bindings
//! defined both at module-level using declarations and at function
//! and pattern level as parameters. Furthermore, it resolves all paths inside
//! expressions and patterns to [(resolved) identifiers](Identifier) which
//! contain a [declaration index](DeclarationIndex) or a [de Bruijn index](DeBruijnIndex)
//! respectively.
#![feature(let_chains, iter_array_chunks)]

// @Task improve docs above!
// @Task get rid of "register" terminology
// @Task transform all Result-returning functions into ()-returning ones modifying self.health
//       and use the new Handler API and get rid of the Stain API

use ast::ParameterKind::Explicit;
use diagnostics::{
    error::{Handler, Health, Outcome, PossiblyErroneous, Result, Stain},
    reporter::ErasedReportedError,
    Diagnostic, ErrorCode, LintCode,
};
use hir::{
    special::{self, NumericType, SequentialType, Type},
    AttributeName, Attributes, DeBruijnIndex, DeclarationIndex, Entity, EntityKind, Exposure,
    ExposureReach, Identifier, Index, LocalDeclarationIndex, PartiallyResolvedPath,
};
use hir_format::{Display, SessionExt as _};
use lexer::word::Word;
use lo_ast::DeBruijnLevel;
use session::{
    component::{ComponentMetadata, DeclarationIndexExt, IdentifierExt, LocalDeclarationIndexExt},
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
pub fn resolve_declarations(
    root_module: lo_ast::Declaration,
    session: &mut Session<'_>,
) -> Result<hir::Declaration> {
    let mut resolver = ResolverMut::new(session);

    if let Err(error) = resolver.start_resolve_declaration(&root_module, None, default()) {
        for (binder, naming_conflicts) in mem::take(&mut resolver.naming_conflicts) {
            error::confliction_definitions(resolver.session[binder].source, &naming_conflicts)
                .report(resolver.session.reporter());
        }

        return Err(error.into_inner());
    }

    // @Beacon @Note these two passes should probably not run after one another but
    // intertwined since use bindings depend on exposure
    // unless we the new "partially resolved exposure" logic fixes everything.
    // Would probably fix the ordering bug in test name-resolution/exposure/re-export-bindings
    resolver.resolve_use_bindings();
    resolver.resolve_exposure_reaches();

    let declaration = resolver
        .finish_resolve_declaration(root_module, None, default())
        .unwrap();

    Outcome::new(declaration, resolver.health).into()
}

// @Task docs: mention that the current component should be pre-populated before calling this
// (using resolver::resolve)
pub fn resolve_path(
    path: &ast::Path,
    namespace: DeclarationIndex,
    session: &Session<'_>,
) -> Result<DeclarationIndex> {
    Resolver::new(session)
        .resolve_path::<target::Any>(path, PathResolutionContext::new(namespace))
        .map_err(|error| Resolver::new(session).report_resolution_error(error))
}

// @Question can we merge Resolver and ResolverMut if we introduce a separate
// lifetime for Resolver.session.at?
struct ResolverMut<'sess, 'ctx> {
    session: &'sess mut Session<'ctx>,
    /// For resolving out of order use-declarations.
    partially_resolved_use_bindings: HashMap<LocalDeclarationIndex, PartiallyResolvedUseBinding>,
    /// Naming conflicts used for error reporting.
    ///
    /// Allows us to group conflicts by binder and emit a *single* diagnostic *per group* (making
    /// use of multiple primary highlights).
    naming_conflicts: HashMap<LocalDeclarationIndex, SmallVec<Span, 2>>,
    health: Health,
}

impl<'sess, 'ctx> ResolverMut<'sess, 'ctx> {
    fn new(session: &'sess mut Session<'ctx>) -> Self {
        Self {
            session,
            partially_resolved_use_bindings: HashMap::default(),
            naming_conflicts: HashMap::default(),
            health: Health::Untainted,
        }
    }

    fn as_ref(&self) -> Resolver<'_> {
        Resolver {
            session: self.session,
        }
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
    /// For more on this, see [`Resolver::reobtain_resolved_identifier`].
    fn start_resolve_declaration(
        &mut self,
        declaration: &lo_ast::Declaration,
        module: Option<LocalDeclarationIndex>,
        context: Context,
    ) -> Result<(), DefinitionError> {
        use lo_ast::BareDeclaration::*;

        let exposure = match declaration.attributes.get::<{ AttributeName::Public }>() {
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

        match &declaration.bare {
            Function(function) => {
                let module = module.unwrap();

                let index = self.define(
                    function.binder,
                    exposure,
                    declaration.attributes.clone(),
                    EntityKind::UntypedFunction,
                    Some(module),
                )?;

                let binder = Identifier::new(index.global(&*self.session), function.binder);

                if let Some(intrinsic) =
                    declaration.attributes.get::<{ AttributeName::Intrinsic }>()
                    && let Err(error) = self.session.define_special(
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
            Data(type_) => {
                // there is always a root module
                let module = module.unwrap();

                // @Task don't return early, see analoguous code for modules
                let index = self.define(
                    type_.binder,
                    exposure,
                    declaration.attributes.clone(),
                    EntityKind::untyped_data_type(),
                    Some(module),
                )?;

                let binder = Identifier::new(index.global(&*self.session), type_.binder);

                let known = declaration.attributes.get::<{ AttributeName::Known }>();
                if let Some(known) = known
                    && let Err(error) = self.session.define_special(
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

                if let Some(intrinsic) =
                    declaration.attributes.get::<{ AttributeName::Intrinsic }>()
                    && let Err(error) = self.session.define_special(
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

                if let Some(constructors) = &type_.declarations {
                    for constructor in constructors {
                        let transparency = match declaration.attributes.has(AttributeName::Abstract)
                        {
                            true => Transparency::Abstract,
                            false => Transparency::Transparent,
                        };

                        self.start_resolve_declaration(
                            constructor,
                            Some(module),
                            Context::DataDeclaration {
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
            Constructor(constructor) => {
                // there is always a root module
                let module = module.unwrap();
                let Context::DataDeclaration {
                    index: namespace,
                    transparency,
                    known,
                } = context
                else {
                    unreachable!()
                };

                let exposure = match transparency.unwrap() {
                    Transparency::Transparent => self.session[namespace].exposure.clone(),
                    // as if a @(public super) was attached to the constructor
                    Transparency::Abstract => ExposureReach::Resolved(module).into(),
                };

                let index = self.define(
                    constructor.binder,
                    exposure,
                    declaration.attributes.clone(),
                    EntityKind::UntypedConstructor,
                    Some(namespace),
                )?;

                let binder = Identifier::new(index.global(&*self.session), constructor.binder);

                // @Task support `@(known name)` on constructors
                if let Some(known) = known
                    && let Err(error) = self.session.define_special(
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
                    exposure,
                    // @Beacon @Bug this does not account for attributes found on the attribute header!
                    declaration.attributes.clone(),
                    EntityKind::module(),
                    module,
                )?;

                let health = &mut Health::Untainted;

                for declaration in &submodule.declarations {
                    self.start_resolve_declaration(declaration, Some(index), Context::default())
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
                    exposure,
                    declaration.attributes.clone(),
                    EntityKind::UnresolvedUse,
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
        binder: ast::Identifier,
        exposure: Exposure,
        attributes: Attributes,
        binding: EntityKind,
        namespace: Option<LocalDeclarationIndex>,
    ) -> Result<LocalDeclarationIndex, DefinitionError> {
        if let Some(namespace) = namespace
            && let Some(index) = self.session[namespace]
                .namespace()
                .unwrap()
                .binders
                .iter()
                .map(|&index| index.local(&*self.session).unwrap())
                .find(|&index| self.session[index].source == binder)
        {
            let previous = &self.session[index].source;

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

        let index = self.session.define(Entity {
            source: binder,
            kind: binding,
            exposure,
            attributes,
            parent: namespace,
        });

        if let Some(namespace) = namespace {
            let index = index.global(&*self.session);
            self.session[namespace]
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
    fn finish_resolve_declaration(
        &mut self,
        declaration: lo_ast::Declaration,
        module: Option<LocalDeclarationIndex>,
        context: Context,
    ) -> Option<hir::Declaration> {
        use lo_ast::BareDeclaration::*;

        match declaration.bare {
            Function(function) => {
                let module = module.unwrap();

                let binder = self
                    .as_ref()
                    .reobtain_resolved_identifier::<target::Value>(function.binder, module);

                let type_annotation = self
                    .as_ref()
                    .resolve_expression(function.type_, &FunctionScope::Module(module))
                    .stain(&mut self.health);

                let expression = function.body.map(|expression| {
                    self.as_ref()
                        .resolve_expression(expression, &FunctionScope::Module(module))
                        .stain(&mut self.health)
                });

                Some(hir::Declaration::new(
                    declaration.attributes,
                    declaration.span,
                    hir::Function {
                        binder,
                        type_: type_annotation,
                        body: expression,
                    }
                    .into(),
                ))
            }
            Data(type_) => {
                let module = module.unwrap();

                // @Beacon @Question wouldn't it be great if that method returned a
                // LocalDeclarationIndex instead of an Identifier?
                // or maybe even a *LocalIdentifier?
                let binder = self
                    .as_ref()
                    .reobtain_resolved_identifier::<target::Value>(type_.binder, module);

                let type_annotation = self
                    .as_ref()
                    .resolve_expression(type_.type_, &FunctionScope::Module(module))
                    .stain(&mut self.health);

                let constructors = type_.declarations.map(|constructors| {
                    constructors
                        .into_iter()
                        .filter_map(|constructor| {
                            self.finish_resolve_declaration(
                                constructor,
                                Some(module),
                                Context::DataDeclaration {
                                    index: binder.local_declaration_index(self.session).unwrap(),
                                    transparency: None,
                                    known: None,
                                },
                            )
                        })
                        .collect::<Vec<_>>()
                });

                Some(hir::Declaration::new(
                    declaration.attributes,
                    declaration.span,
                    hir::Data {
                        binder,
                        type_: type_annotation,
                        constructors,
                    }
                    .into(),
                ))
            }
            Constructor(constructor) => {
                let module = module.unwrap();
                let Context::DataDeclaration {
                    index: namespace, ..
                } = context
                else {
                    unreachable!()
                };

                let binder = self
                    .as_ref()
                    .reobtain_resolved_identifier::<target::Value>(constructor.binder, namespace);

                let type_annotation = self
                    .as_ref()
                    .resolve_expression(constructor.type_, &FunctionScope::Module(module))
                    .stain(&mut self.health);

                Some(hir::Declaration::new(
                    declaration.attributes,
                    declaration.span,
                    hir::Constructor {
                        binder,
                        type_: type_annotation,
                    }
                    .into(),
                ))
            }
            Module(submodule) => {
                let index = match module {
                    // unwrap: could only ever be non-local if the binder was a use-binding
                    // but it is module binding
                    Some(module) => self
                        .as_ref()
                        .reobtain_resolved_identifier::<target::Module>(submodule.binder, module)
                        .local(&*self.session)
                        .unwrap(),
                    None => ComponentMetadata::ROOT,
                };

                let declarations = submodule
                    .declarations
                    .into_iter()
                    .filter_map(|declaration| {
                        self.finish_resolve_declaration(
                            declaration,
                            Some(index),
                            Context::default(),
                        )
                    })
                    .collect();

                Some(hir::Declaration::new(
                    declaration.attributes,
                    declaration.span,
                    hir::Module {
                        binder: Identifier::new(index.global(&*self.session), submodule.binder),
                        file: submodule.file,
                        declarations,
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
                let namespace = item.target.namespace.global(&*self.session);

                match self.as_ref().resolve_path::<target::Any>(
                    &item.target.path,
                    PathResolutionContext::new(namespace),
                ) {
                    Ok(target) => {
                        self.session[index].kind = EntityKind::Use { target };
                    }
                    Err(error @ (UnresolvedBinding { .. } | Erased(_))) => {
                        self.session[index].kind =
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
                                binder: binder.local(&*self.session).unwrap(),
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
                    self.session[index].kind =
                        PossiblyErroneous::error(ErasedReportedError::new_unchecked());
                }

                for cycle in find_cycles(
                    &partially_resolved_use_bindings
                        .into_iter()
                        .map(|(index, binding)| (index, binding.binder))
                        .collect(),
                ) {
                    error::circular_declarations(cycle, self.session)
                        .report(self.session.reporter());
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
        for (index, entity) in &self.session.context.bindings[self.session.component] {
            if let Exposure::Restricted(exposure) = &entity.exposure {
                // unwrap: root always has Exposure::Unrestricted, it won't reach this branch
                let definition_site_namespace = entity.parent.unwrap().global(&*self.session);

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
                let target = &self.session[target_index];

                if entity.exposure.compare(&target.exposure, self.session)
                    == Some(Ordering::Greater)
                {
                    let error = Diagnostic::error()
                        .code(ErrorCode::E009)
                        .message(format!(
                            "re-export of the more private binding ‘{}’",
                            self.session.index_to_path(target_index)
                        ))
                        .span(entity.source, "re-exporting binding with greater exposure")
                        .label(target.source, "re-exported binding with lower exposure")
                        .note(format!(
                            "\
expected the exposure of ‘{}’
           to be at most {}
      but it actually is {}",
                            self.session.local_index_to_path(index),
                            self.as_ref().display(&target.exposure),
                            self.as_ref().display(&entity.exposure),
                        ))
                        .report(self.session.reporter());
                    self.health.taint(error);
                }
            }
        }
    }
}

impl Handler for &mut ResolverMut<'_, '_> {
    fn embed<T: PossiblyErroneous>(self, diagnostic: Diagnostic) -> T {
        let error = diagnostic.report(self.session.reporter());
        self.health.taint(error);
        T::error(error)
    }
}

struct Resolver<'a> {
    session: &'a Session<'a>,
}

impl<'a> Resolver<'a> {
    fn new(session: &'a Session<'a>) -> Self {
        Self { session }
    }

    fn resolve_expression(
        &self,
        expression: lo_ast::Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<hir::Expression> {
        use lo_ast::BareExpression::*;

        Ok(match expression.bare {
            Type => {
                // @Task don't use def-site span use use-site span
                // @Bug the span is not really a "user" (in the typer sense) but just a reference
                // @Task distinguish
                let type_ = self
                    .session
                    .require_special(special::Type::Type, Some(expression.span))?;

                hir::Expression::new(
                    expression.attributes,
                    expression.span,
                    hir::Binding(type_).into(),
                )
            }
            LocalBinding(level) => hir::Expression::new(
                expression.attributes,
                expression.span,
                hir::Binding(Identifier::new(
                    scope.index_from_level(level),
                    ast::Identifier::new_unchecked(expression.span, Atom::UNDERSCORE),
                ))
                .into(),
            ),
            Wildcard(_) => {
                return Err(Diagnostic::error()
                    .message("wildcards are not supported yet")
                    .unlabeled_span(expression)
                    .report(self.session.reporter()))
            }
            Projection(projection) => hir::Expression::new(
                expression.attributes,
                expression.span,
                hir::Projection {
                    basis: self.resolve_expression(projection.basis, scope)?,
                    field: projection.field,
                }
                .into(),
            ),
            PiType(pi) => {
                let domain = self.resolve_expression(pi.domain, scope);
                let codomain = match pi.binder {
                    Some(parameter) => self
                        .resolve_expression(pi.codomain, &scope.extend_with_parameter(parameter)),
                    None => self.resolve_expression(pi.codomain, scope),
                };

                hir::Expression::new(
                    expression.attributes,
                    expression.span,
                    hir::PiType {
                        domain: domain?,
                        codomain: codomain?,
                        kind: pi.kind,
                        binder: pi
                            .binder
                            .map(|parameter| Identifier::new(Index::Parameter, parameter)),
                    }
                    .into(),
                )
            }
            Application(application) => {
                let callee = self.resolve_expression(application.callee, scope);
                let argument = self.resolve_expression(application.argument, scope);

                hir::Expression::new(
                    expression.attributes,
                    expression.span,
                    hir::Application {
                        callee: callee?,
                        argument: argument?,
                        kind: application.kind,
                    }
                    .into(),
                )
            }
            NumberLiteral(number) => self.resolve_number_literal(
                hir::Item::new(expression.attributes, expression.span, *number),
                scope,
            )?,
            TextLiteral(text) => self.resolve_text_literal(
                hir::Item::new(expression.attributes, expression.span, *text),
                scope,
            )?,
            Path(path) => hir::Expression::new(
                expression.attributes,
                expression.span,
                hir::Binding(self.resolve_path_inside_function(&path, scope)?).into(),
            ),
            Lambda(lambda) => {
                let domain = lambda
                    .domain
                    .map(|type_| self.resolve_expression(type_, scope));
                let scope = match lambda.binder {
                    Some(binder) => Owned(scope.extend_with_parameter(binder)),
                    None => Borrowed(scope),
                };
                let scope = scope.as_ref();

                let codomain = lambda
                    .codomain
                    .map(|type_| self.resolve_expression(type_, scope));

                let body = self.resolve_expression(lambda.body, scope);

                hir::Expression::new(
                    expression.attributes,
                    expression.span,
                    hir::Lambda {
                        binder: lambda
                            .binder
                            .map(|binder| Identifier::new(Index::Parameter, binder)),
                        domain: domain.transpose()?,
                        codomain: codomain.transpose()?,
                        body: body?,
                        kind: lambda.kind,
                    }
                    .into(),
                )
            }
            UseBinding => {
                return Err(Diagnostic::error()
                    .message("use-bindings are not supported yet")
                    .unlabeled_span(&expression)
                    .report(self.session.reporter()));
            }
            CaseAnalysis(analysis) => {
                let scrutinee = self.resolve_expression(analysis.scrutinee, scope)?;
                let mut cases = Vec::new();

                for case in analysis.cases {
                    let (pattern, binders) = self.resolve_pattern(case.pattern, scope)?;
                    let body = self.resolve_expression(
                        case.body,
                        &scope.extend_with_pattern_binders(binders),
                    )?;

                    cases.push(hir::Case { pattern, body });
                }

                hir::Expression::new(
                    expression.attributes,
                    expression.span,
                    hir::CaseAnalysis { scrutinee, cases }.into(),
                )
            }
            SequenceLiteral(sequence) => {
                let mut elements = Vec::with_capacity(sequence.elements.bare.len());
                let health = &mut Health::Untainted;

                for element in sequence.elements.bare {
                    elements.push(self.resolve_expression(element, scope).stain(health));
                }

                Result::from(*health)?;

                self.resolve_sequence_literal(
                    hir::Item::new(
                        expression.attributes,
                        expression.span,
                        ast::SequenceLiteral {
                            path: sequence.path,
                            elements: Spanned::new(sequence.elements.span, elements),
                        },
                    ),
                    scope,
                )?
            }
            RecordLiteral(record) => {
                let path = record.path.as_ref().ok_or_else(|| {
                    error::unqualified_literal("record", record.fields.span)
                        .report(self.session.reporter())
                })?;

                // @Task maybe check if it points to a record!
                let type_ = Spanned::new(
                    path.span(), // @Task use the span of the last segment for consistency
                    self.resolve_path_of_literal(path, record.fields.span, scope)?,
                );

                if let Some(base) = record.base {
                    let error = Diagnostic::error()
                        .message("record literal bases are not supported yet")
                        .unlabeled_span(base.span)
                        .report(self.session.reporter());

                    let _ = self.resolve_expression(base, scope)?;

                    return Err(error);
                }

                let mut fields = Vec::with_capacity(record.fields.bare.len());
                let health = &mut Health::Untainted;

                for field in record.fields.bare {
                    fields.push(hir::Field {
                        binder: field.binder,
                        body: self.resolve_expression(field.body, scope).stain(health),
                    });
                }

                Result::from(*health)?;

                hir::Expression::new(
                    expression.attributes,
                    expression.span,
                    hir::Record { type_, fields }.into(),
                )
            }
            Error(error) => PossiblyErroneous::error(error),
        })
    }

    // @Task use the Stain::stain instead of moving the try operator below resolve calls
    fn resolve_pattern(
        &self,
        pattern: lo_ast::Pattern,
        scope: &FunctionScope<'_>,
    ) -> Result<(hir::Pattern, Vec<ast::Identifier>)> {
        use lo_ast::BarePattern::*;

        // @Task replace this hideous binders.append logic
        let mut binders: Vec<ast::Identifier> = Vec::new();

        let pattern = match pattern.bare {
            Wildcard(_) => {
                return Err(Diagnostic::error()
                    .message("wildcards are not supported yet")
                    .unlabeled_span(pattern)
                    .report(self.session.reporter()))
            }
            NumberLiteral(number) => self.resolve_number_literal(
                hir::Item::new(pattern.attributes, pattern.span, *number),
                scope,
            )?,
            TextLiteral(text) => self.resolve_text_literal(
                hir::Item::new(pattern.attributes, pattern.span, *text),
                scope,
            )?,
            Path(path) => hir::Pattern::new(
                pattern.attributes,
                pattern.span,
                hir::Binding(self.resolve_path_inside_function(&path, scope)?).into(),
            ),
            LetBinding(binder) => {
                if let ast::LocalBinder::Named(binder) = binder {
                    binders.push(binder);
                }

                hir::Pattern::new(
                    pattern.attributes,
                    pattern.span,
                    binder
                        .map(|binder| Identifier::new(Index::Parameter, binder))
                        .into(),
                )
            }
            Application(application) => {
                let callee = self.resolve_pattern(application.callee, scope);
                let argument = self.resolve_pattern(application.argument, scope);

                let (callee, mut callee_binders) = callee?;
                let (argument, mut argument_binders) = argument?;

                binders.append(&mut callee_binders);
                binders.append(&mut argument_binders);

                hir::Pattern::new(
                    pattern.attributes,
                    pattern.span,
                    hir::Application {
                        callee,
                        kind: application.kind,
                        argument,
                    }
                    .into(),
                )
            }
            SequenceLiteral(sequence) => {
                let mut elements = Vec::new();

                for element in sequence.elements.bare {
                    // @Task use Stain::stain to not return early!
                    let (element, mut element_binders) = self.resolve_pattern(element, scope)?;
                    binders.append(&mut element_binders);
                    elements.push(element);
                }

                self.resolve_sequence_literal(
                    hir::Item::new(
                        pattern.attributes,
                        pattern.span,
                        ast::SequenceLiteral {
                            path: sequence.path,
                            elements: Spanned::new(sequence.elements.span, elements),
                        },
                    ),
                    scope,
                )?
            }
            Error(error) => PossiblyErroneous::error(error),
        };

        Ok((pattern, binders))
    }

    fn resolve_number_literal<T>(
        &self,
        number: hir::Item<ast::NumberLiteral>,
        scope: &FunctionScope<'_>,
    ) -> Result<hir::Item<T>>
    where
        T: From<hir::Number>,
    {
        let type_ = number
            .bare
            .path
            .as_ref()
            .map(|path| {
                Ok(Spanned::new(
                    path.span(), // @Task use the span of the last segment for consistency
                    self.resolve_path_of_literal(path, number.bare.literal.span, scope)?,
                ))
            })
            .transpose()?;

        let literal = &number.bare.literal;

        let type_ = match type_ {
            Some(type_) => self
                .session
                .specials().get(type_.bare)
                // @Task write more concisely
                .and_then(|intrinsic| obtain!(intrinsic, special::Binding::Type(Type::Numeric(type_)) => type_))
                .ok_or_else(|| {
                    self.literal_used_for_unsupported_type(literal, "number", type_)
                        .report(self.session.reporter())
                })?,
            // for now, we default to `Nat` until we implement polymorphic number literals and their inference
            None => NumericType::Nat,
        };

        let Ok(resolved_number) = hir::Number::parse(literal.bare.to_str(), type_) else {
            return Err(Diagnostic::error()
                .code(ErrorCode::E007)
                .message(format!(
                    "number literal ‘{literal}’ does not fit type ‘{type_}’",
                ))
                .unlabeled_span(literal)
                .note(format!(
                    "values of this type must fit integer interval {}",
                    type_.interval(),
                ))
                .report(self.session.reporter()));
        };

        Ok(number.remap(resolved_number.into()))
    }

    fn resolve_text_literal<T>(
        &self,
        text: hir::Item<ast::TextLiteral>,
        scope: &FunctionScope<'_>,
    ) -> Result<hir::Item<T>>
    where
        T: From<hir::Text>,
    {
        let type_ = text
            .bare
            .path
            .as_ref()
            .map(|path| {
                Ok(Spanned::new(
                    path.span(), // @Task use the span of the last segment for consistency
                    self.resolve_path_of_literal(path, text.bare.literal.span, scope)?,
                ))
            })
            .transpose()?;

        let _type = match type_ {
            Some(type_) => self
                .session
                .specials()
                .get(type_.bare)
                .filter(|&intrinsic| matches!(intrinsic, special::Binding::Type(Type::Text)))
                .ok_or_else(|| {
                    self.literal_used_for_unsupported_type(text.bare.literal, "text", type_)
                        .report(self.session.reporter())
                })?,
            // for now, we default to `Text` until we implement polymorphic text literals and their inference
            None => Type::Text.into(),
        };

        Ok(hir::Item::new(
            text.attributes,
            text.span,
            // @Beacon @Task avoid Atom::to_string
            hir::Text::Text(text.bare.literal.bare.to_string()).into(),
        ))
    }

    fn resolve_sequence_literal<T>(
        &self,
        sequence: hir::Item<ast::SequenceLiteral<hir::Item<T>>>,
        scope: &FunctionScope<'_>,
    ) -> Result<hir::Item<T>>
    where
        T: Clone + From<hir::Binding> + From<hir::Number> + From<hir::Application<hir::Item<T>>>,
    {
        // @Task test sequence literals inside of patterns!

        // @Task test this!
        let path = sequence.bare.path.as_ref().ok_or_else(|| {
            error::unqualified_literal("sequence", sequence.bare.elements.span)
                .report(self.session.reporter())
        })?;

        let type_ = Spanned::new(
            path.span(), // @Task use the span of the last segment for consistency
            self.resolve_path_of_literal(path, sequence.bare.elements.span, scope)?,
        );

        // @Task test this!
        let type_ = self
            .session
            .specials().get(type_.bare)
            .and_then(|known| obtain!(known, special::Binding::Type(special::Type::Sequential(type_)) => type_))
            .ok_or_else(|| {
                self.literal_used_for_unsupported_type(&sequence.bare.elements, "sequence", type_)
                    .report(self.session.reporter())
            })?;

        match type_ {
            SequentialType::List => self.resolve_list_literal(sequence.bare.elements),
            SequentialType::Vector => self.resolve_vector_literal(sequence.bare.elements),
            SequentialType::Tuple => self.resolve_tuple_literal(sequence.bare.elements),
        }
    }

    fn resolve_list_literal<T>(&self, elements: Spanned<Vec<hir::Item<T>>>) -> Result<hir::Item<T>>
    where
        T: Clone + From<hir::Binding> + From<hir::Application<hir::Item<T>>>,
    {
        let span = elements.span;
        let mut elements = elements.bare.into_iter();
        let Some(element_type) = elements.next() else {
            return Err(Diagnostic::error()
                .message("list literals cannot be empty")
                .note(
                    "due to limitations of the current type system, \
                     element types cannot be inferred and\n\
                     have to be manually supplied as the first “element”",
                )
                .unlabeled_span(span)
                .report(self.session.reporter()));
        };

        let empty = self
            .session
            .require_special(special::Constructor::ListEmpty, Some(span))?;
        let prepend = self
            .session
            .require_special(special::Constructor::ListPrepend, Some(span))?;

        // @Task check if all those attributes & spans make sense
        let mut result = hir::Item::common(
            span,
            hir::Application {
                // @Task don't throw away attributes
                callee: hir::Item::common(span, hir::Binding(empty).into()),
                kind: Explicit,
                argument: element_type.clone(),
            }
            .into(),
        );

        for element in elements.rev() {
            // @Task check if all those attributes & spans make sense
            let prepend = hir::Item::common(
                element.span,
                hir::Application {
                    callee: hir::Item::common(element.span, hir::Binding(prepend).into()),
                    kind: Explicit,
                    argument: element_type.clone(),
                }
                .into(),
            );

            // @Task check if all those attributes & spans make sense
            result = hir::Item::common(
                element.span,
                hir::Application {
                    callee: hir::Item::common(
                        element.span,
                        hir::Application {
                            callee: prepend,
                            kind: Explicit,
                            argument: element,
                        }
                        .into(),
                    ),
                    kind: Explicit,
                    argument: result,
                }
                .into(),
            );
        }

        Ok(result)
    }

    // @Task write UI tests
    fn resolve_vector_literal<T>(
        &self,
        elements: Spanned<Vec<hir::Item<T>>>,
    ) -> Result<hir::Item<T>>
    where
        T: Clone + From<hir::Binding> + From<hir::Number> + From<hir::Application<hir::Item<T>>>,
    {
        let span = elements.span;
        let mut elements = elements.bare.into_iter();
        let Some(element_type) = elements.next() else {
            return Err(Diagnostic::error()
                .message("vector literals cannot be empty")
                .note(
                    "due to limitations of the current type system, \
                     element types cannot be inferred and\n\
                     have to be manually supplied as the first “element”",
                )
                .unlabeled_span(span)
                .report(self.session.reporter()));
        };

        let empty = self
            .session
            .require_special(special::Constructor::VectorEmpty, Some(span))?;
        let prepend = self
            .session
            .require_special(special::Constructor::VectorPrepend, Some(span))?;

        // @Task check if all those attributes & spans make sense
        let mut result = hir::Item::common(
            span,
            hir::Application {
                // @Task don't throw away attributes & span
                callee: hir::Item::common(span, hir::Binding(empty).into()),
                kind: Explicit,
                argument: element_type.clone(),
            }
            .into(),
        );

        for (length, element) in elements.rev().enumerate() {
            // @Task check if all those attributes & spans make sense
            let prepend = hir::Item::common(
                element.span,
                hir::Application {
                    callee: hir::Item::common(
                        element.span,
                        hir::Application {
                            callee: hir::Item::common(element.span, hir::Binding(prepend).into()),
                            kind: Explicit,
                            // @Beacon @Question What happens if the user does not define the intrinsic type `Nat`?
                            //                   Is that going to lead to crashes later on?
                            argument: hir::Item::common(
                                element.span,
                                hir::Number::Nat(length.into()).into(),
                            ),
                        }
                        .into(),
                    ),
                    kind: Explicit,
                    argument: element_type.clone(),
                }
                .into(),
            );

            // @Task check if all those attributes & spans make sense
            result = hir::Item::common(
                element.span,
                hir::Application {
                    callee: hir::Item::common(
                        element.span,
                        hir::Application {
                            callee: prepend,
                            kind: Explicit,
                            argument: element,
                        }
                        .into(),
                    ),
                    kind: Explicit,
                    argument: result,
                }
                .into(),
            );
        }

        Ok(result)
    }

    // @Task write UI tests
    fn resolve_tuple_literal<T>(&self, elements: Spanned<Vec<hir::Item<T>>>) -> Result<hir::Item<T>>
    where
        T: Clone + From<hir::Binding> + From<hir::Application<hir::Item<T>>>,
    {
        if elements.bare.len() % 2 != 0 {
            return Err(Diagnostic::error()
                .message("tuple literals cannot be empty")
                .note(
                    "due to limitations of the current type system, \
                     element types cannot be inferred and\n\
                     have to be manually supplied to the left of each element",
                )
                .unlabeled_span(elements.span)
                .report(self.session.reporter()));
        }

        let empty = self
            .session
            .require_special(special::Constructor::TupleEmpty, Some(elements.span))?;
        let prepend = self
            .session
            .require_special(special::Constructor::TuplePrepend, Some(elements.span))?;
        let type_ = self
            .session
            .require_special(special::Type::Type, Some(elements.span))?;

        // @Task check if all those attributes & spans make sense
        let mut result = hir::Item::common(elements.span, hir::Binding(empty).into());
        let mut list = vec![hir::Item::common(elements.span, hir::Binding(type_).into())];

        for [element_type, element] in elements.bare.into_iter().array_chunks().rev() {
            // @Task check if all those attributes & spans make sense
            let prepend = hir::Item::common(
                element.span,
                hir::Application {
                    callee: hir::Item::common(
                        element.span,
                        hir::Application {
                            callee: hir::Item::common(element.span, hir::Binding(prepend).into()),
                            kind: Explicit,
                            argument: element_type.clone(),
                        }
                        .into(),
                    ),
                    kind: Explicit,
                    argument: self
                        .resolve_list_literal(Spanned::new(element.span, list.clone()))?,
                }
                .into(),
            );

            // @Task find a cleaner approach
            list.insert(1, element_type);

            // @Task check if all those attributes & spans make sense
            result = hir::Item::common(
                element.span,
                hir::Application {
                    callee: hir::Item::common(
                        element.span,
                        hir::Application {
                            callee: prepend,
                            kind: Explicit,
                            argument: element,
                        }
                        .into(),
                    ),
                    kind: Explicit,
                    argument: result,
                }
                .into(),
            );
        }

        Ok(result)
    }

    fn literal_used_for_unsupported_type(
        &self,
        literal: impl Spanning,
        name: &str,
        type_: Spanned<DeclarationIndex>,
    ) -> Diagnostic {
        // @Task use correct terminology here: type vs. type constructor
        Diagnostic::error()
            .code(ErrorCode::E043)
            .message(format!(
                "a {name} literal is not a valid constructor for type ‘{}’",
                self.session[type_.bare].source
            ))
            .span(literal, "this literal may not construct the type")
            .label(type_, "the data type")
    }

    fn resolve_path_of_literal(
        &self,
        path: &ast::Path,
        literal: Span,
        scope: &FunctionScope<'_>,
    ) -> Result<DeclarationIndex> {
        let namespace = scope.module().global(self.session);
        let binding = self
            .resolve_path::<target::Any>(path, PathResolutionContext::new(namespace))
            .map_err(|error| self.report_resolution_error(error))?;

        {
            let entity = &self.session[binding];
            if !entity.is_data_type() {
                // @Task code
                return Err(Diagnostic::error()
                    .message(format!("binding ‘{path}’ is not a data type"))
                    // @Task future-proof a/an
                    .span(path, format!("a {}", entity.kind.name()))
                    .label(literal, "literal requires a data type as its namespace")
                    .report(self.session.reporter()));
            }
        }

        Ok(binding)
    }

    /// Resolve a syntactic path given a namespace.
    // @Task memoize by (path, namespace)
    fn resolve_path<Target: ResolutionTarget>(
        &self,
        path: &ast::Path,
        context: PathResolutionContext,
    ) -> Result<Target::Output, ResolutionError> {
        if let Some(hanger) = &path.hanger {
            use ast::BareHanger::*;

            let namespace = match hanger.bare {
                Extern => {
                    let Some(component) = path.segments.first() else {
                        // @Task improve the error message, code
                        return Err(Diagnostic::error()
                            .message("path ‘extern’ is used in isolation")
                            .unlabeled_span(hanger)
                            .note("the path segment ‘extern’ is only to be used indirectly to refer to specific component")
                            .report(self.session.reporter()).into());
                    };

                    // @Beacon @Task add test for error case
                    let component: Spanned<Word> = (*component).try_into().map_err(|()| {
                        // @Task DRY @Question is the common code justified?
                        Diagnostic::error()
                            .code(ErrorCode::E036)
                            .message(format!(
                                "the component name ‘{component}’ is not a valid word"
                            ))
                            .unlabeled_span(component)
                            .report(self.session.reporter())
                    })?;

                    let Some(&component) =
                        self.session.component().dependencies().get(&component.bare)
                    else {
                        // @Task If it's not a single source file, suggest adding to `dependencies` section in
                        // the package manifest
                        // @Task suggest similarly named dependencies!
                        // @Task check if it's a transitive dependency
                        // with the *same* name and add the note that they (trans deps) have to be
                        // explicitly added to the deps list to be referenceable in this component
                        // @Task check if it is a sublibrary that was not explicitly added and suggest doing so
                        // @Task better phrasing
                        return Err(Diagnostic::error()
                            .message(format!("the component ‘{component}’ is not defined"))
                            .unlabeled_span(component)
                            .report(self.session.reporter())
                            .into());
                    };

                    let component = &self.session[component];
                    let root = component.root();

                    return match &*path.segments {
                        &[identifier] => Ok(Target::output(root, identifier)),
                        [_, identifiers @ ..] => self.resolve_path::<Target>(
                            // @Task use PathView once available
                            &ast::Path::unhung(identifiers.to_owned().into()),
                            PathResolutionContext::new(root)
                                .origin_namespace(context.origin_namespace)
                                .qualified_identifier(),
                        ),
                        [] => unreachable!(),
                    };
                }
                Topmost => self.session.component().root(),
                Super => self
                    .resolve_super(hanger, context.namespace.local(self.session).unwrap())?
                    .global(self.session),
                Self_ => context.namespace,
            };

            return if path.segments.is_empty() {
                Target::output_bare_path_hanger(hanger, namespace)
                    .map_err(|error| error.report(self.session.reporter()).into())
            } else {
                self.resolve_path::<Target>(
                    // @Task use PathView once available
                    &ast::Path::unhung(path.segments.clone()),
                    context.namespace(namespace).qualified_identifier(),
                )
            };
        }

        let index = self.resolve_identifier(path.segments[0], context)?;
        let entity = &self.session[index];

        match &*path.segments {
            &[identifier] => {
                Target::validate_identifier(identifier, entity)
                    .map_err(|error| error.report(self.session.reporter()))?;
                Ok(Target::output(index, identifier))
            }
            [identifier, identifiers @ ..] => {
                if entity.is_namespace() {
                    self.resolve_path::<Target>(
                        // @Task use PathView once available
                        &ast::Path::unhung(identifiers.to_owned().into()),
                        context.namespace(index).qualified_identifier(),
                    )
                } else if entity.is_error() {
                    // @Task add rationale why `last`
                    Ok(Target::output(index, *identifiers.last().unwrap()))
                } else {
                    let diagnostic = self.attempt_to_access_subbinder_of_non_namespace_error(
                        *identifier,
                        &entity.kind,
                        context.namespace,
                        *identifiers.first().unwrap(),
                    );
                    Err(diagnostic.report(self.session.reporter()).into())
                }
            }
            [] => unreachable!(),
        }
    }

    fn resolve_super(
        &self,
        hanger: &ast::Hanger,
        module: LocalDeclarationIndex,
    ) -> Result<LocalDeclarationIndex> {
        self.session[module].parent.ok_or_else(|| {
            Diagnostic::error()
                .code(ErrorCode::E021) // @Question use a dedicated code?
                .message("the root module does not have a parent module")
                .unlabeled_span(hanger)
                .report(self.session.reporter())
        })
    }

    fn resolve_identifier(
        &self,
        identifier: ast::Identifier,
        context: PathResolutionContext,
    ) -> Result<DeclarationIndex, ResolutionError> {
        let index = self.session[context.namespace]
            .namespace()
            .unwrap()
            .binders
            .iter()
            .copied()
            .find(|&index| self.session[index].source == identifier)
            .ok_or(ResolutionError::UnresolvedBinding {
                identifier,
                namespace: context.namespace,
                usage: context.usage,
            })?;

        // @Temporary hack until we can manage cyclic exposure reaches!
        if context.validate_exposure {
            self.handle_exposure(index, identifier, context.origin_namespace)?;
        }

        if !context.allow_deprecated
            && let Some(deprecated) = self.session[index]
                .attributes
                .get::<{ AttributeName::Deprecated }>()
        {
            let mut message = format!(
                "use of deprecated binding ‘{}’",
                self.session.index_to_path(index),
            );

            if let Some(reason) = &deprecated.bare.reason {
                message += ": ";
                message += reason.to_str();
            }

            Diagnostic::warning()
                .code(LintCode::Deprecated)
                .message(message)
                .unlabeled_span(identifier)
                .report(self.session.reporter());
        }

        self.collapse_use_chain(index, identifier.span())
    }

    // @Task verify that the exposure is checked even in the case of use-declarations
    // using use-bindings (use-chains).
    fn handle_exposure(
        &self,
        index: DeclarationIndex,
        identifier: ast::Identifier,
        origin_namespace: DeclarationIndex,
    ) -> Result<(), ResolutionError> {
        let entity = &self.session[index];

        if let Exposure::Restricted(exposure) = &entity.exposure {
            // unwrap: root always has Exposure::Unrestricted
            let definition_site_namespace = entity.parent.unwrap();
            let reach = self.resolve_restricted_exposure(
                exposure,
                definition_site_namespace.global(self.session),
            )?;

            if !self.session.is_allowed_to_access(
                origin_namespace,
                definition_site_namespace.global(self.session),
                reach,
            ) {
                return Err(Diagnostic::error()
                    .code(ErrorCode::E029)
                    .message(format!(
                        "binding ‘{}’ is private",
                        self.session.index_to_path(index)
                    ))
                    .unlabeled_span(identifier)
                    .report(self.session.reporter())
                    .into());
            }
        }

        Ok(())
    }

    /// Collapse chain of use-bindings aka indirect uses.
    ///
    /// This is an invariant established to make things easier to reason about during resolution.
    fn collapse_use_chain(
        &self,
        index: DeclarationIndex,
        extra: Span,
    ) -> Result<DeclarationIndex, ResolutionError> {
        use EntityKind::*;

        match self.session[index].kind {
            Use { target } => Ok(target),
            UnresolvedUse => Err(ResolutionError::UnresolvedUseBinding {
                binder: index,
                extra,
            }),
            _ => Ok(index),
        }
    }

    fn resolve_restricted_exposure(
        &self,
        exposure: &Mutex<ExposureReach>,
        definition_site_namespace: DeclarationIndex,
    ) -> Result<DeclarationIndex> {
        let exposure_ = exposure.lock().unwrap();

        Ok(match &*exposure_ {
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
                        PathResolutionContext::new(namespace.global(self.session))
                            .ignore_exposure(),
                    )
                    .map_err(|error| self.report_resolution_error(error))?;

                let reach_is_ancestor = self
                    .session
                    .some_ancestor_equals(definition_site_namespace, reach);

                if !reach_is_ancestor {
                    return Err(Diagnostic::error()
                        .code(ErrorCode::E037)
                        .message("exposure can only be restricted to ancestor modules")
                        .unlabeled_span(path)
                        .report(self.session.reporter()));
                }

                drop(exposure_);
                *exposure.lock().unwrap() =
                    // @Question unwrap() correct?
                    ExposureReach::Resolved(reach.local(self.session).unwrap());

                reach
            }
            &ExposureReach::Resolved(reach) => reach.global(self.session),
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
    fn reobtain_resolved_identifier<Target: ResolutionTarget>(
        &self,
        identifier: ast::Identifier,
        namespace: LocalDeclarationIndex,
    ) -> Target::Output {
        let index = self.session[namespace]
            .namespace()
            .unwrap()
            .binders
            .iter()
            .map(|index| index.local(self.session).unwrap())
            .find(|&index| self.session[index].source == identifier)
            .unwrap();
        let index = self
            .collapse_use_chain(index.global(self.session), identifier.span())
            .unwrap_or_else(|_| unreachable!());

        Target::output(index, identifier)
    }

    /// Resolve a path inside of a function.
    fn resolve_path_inside_function(
        &self,
        query: &ast::Path,
        scope: &FunctionScope<'_>,
    ) -> Result<Identifier> {
        self.resolve_path_inside_function_with_depth(query, scope, 0, scope)
    }

    /// Resolve a path inside of a function given a depth.
    ///
    /// The `depth` is necessary for the recursion to successfully create de Bruijn indices.
    ///
    /// The `origin` signifies the innermost function scope from where the resolution was first requested.
    /// This information is used for diagnostics, namely typo flagging where we once again start at the origin
    /// and walk back out.
    fn resolve_path_inside_function_with_depth(
        &self,
        query: &ast::Path,
        scope: &FunctionScope<'_>,
        depth: usize,
        origin: &FunctionScope<'_>,
    ) -> Result<Identifier> {
        use FunctionScope::*;

        if let (false, Some(identifier)) = (matches!(scope, Module(_)), query.identifier_head()) {
            match scope {
                FunctionParameter { parent, binder } => {
                    if *binder == identifier {
                        Ok(Identifier::new(DeBruijnIndex(depth), identifier))
                    } else {
                        self.resolve_path_inside_function_with_depth(
                            query,
                            parent,
                            depth + 1,
                            origin,
                        )
                    }
                }
                PatternBinders { parent, binders } => {
                    match binders
                        .iter()
                        .rev()
                        .zip(depth..)
                        .find(|(&binder, _)| binder == identifier)
                    {
                        Some((_, depth)) => Ok(Identifier::new(DeBruijnIndex(depth), identifier)),
                        None => self.resolve_path_inside_function_with_depth(
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
                PathResolutionContext::new(scope.module().global(self.session)),
            )
            .map_err(|error| {
                self.report_resolution_error_searching_lookalikes(error, |identifier, _| {
                    self.find_similarly_named(origin, identifier.to_str())
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
    fn find_similarly_named_declaration(
        &self,
        identifier: &str,
        predicate: impl Fn(&Entity) -> bool,
        namespace: DeclarationIndex,
    ) -> Option<Atom> {
        self.session[namespace]
            .namespace()
            .unwrap()
            .binders
            .iter()
            .map(|&index| &self.session[index])
            .filter(|entity| !entity.is_error() && predicate(entity))
            .map(|entity| entity.source.bare())
            .find(|some_identifier| is_similar(some_identifier.to_str(), identifier))
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
        identifier: &str,
    ) -> Option<Atom> {
        use FunctionScope::*;

        match scope {
            &Module(module) => self.find_similarly_named_declaration(
                identifier,
                |_| true,
                module.global(self.session),
            ),
            FunctionParameter { parent, binder } => {
                if is_similar(identifier, binder.to_str()) {
                    Some(binder.bare())
                } else {
                    self.find_similarly_named(parent, identifier)
                }
            }
            PatternBinders { parent, binders } => {
                if let Some(binder) = binders
                    .iter()
                    .rev()
                    .find(|binder| is_similar(identifier, binder.to_str()))
                {
                    Some(binder.bare())
                } else {
                    self.find_similarly_named(parent, identifier)
                }
            }
        }
    }

    fn report_resolution_error(&self, error: ResolutionError) -> ErasedReportedError {
        self.report_resolution_error_searching_lookalikes(error, |identifier, namespace| {
            self.find_similarly_named_declaration(identifier.to_str(), |_| true, namespace)
        })
    }

    #[allow(clippy::needless_pass_by_value)] // by design
    fn report_resolution_error_searching_lookalikes(
        &self,
        error: ResolutionError,
        lookalike_finder: impl FnOnce(Atom, DeclarationIndex) -> Option<Atom>,
    ) -> ErasedReportedError {
        match error {
            ResolutionError::Erased(error) => error,
            ResolutionError::UnresolvedBinding {
                identifier,
                namespace,
                usage,
            } => {
                let mut message = format!("the binding ‘{identifier}’ is not defined in ");

                match usage {
                    IdentifierUsage::Unqualified => message += "this scope",
                    IdentifierUsage::Qualified => {
                        if namespace == self.session.component().root() {
                            message += "the root module";
                        } else {
                            message += match self.session[namespace].is_module() {
                                true => "module",
                                false => "namespace",
                            };
                            message += " ‘";
                            message += &self.session.index_to_path(namespace);
                            message += "’";
                        }
                    }
                }

                Diagnostic::error()
                    .code(ErrorCode::E021)
                    .message(message)
                    .unlabeled_span(identifier)
                    .with(|error| {
                        let identifier = identifier.bare();

                        match lookalike_finder(identifier, namespace) {
                            Some(lookalike) => error.help(format!(
                                "a binding with a similar name exists in scope: {}",
                                Lookalike {
                                    actual: identifier,
                                    lookalike,
                                },
                            )),
                            None => error,
                        }
                    })
                    .report(self.session.reporter())
            }
            // @Beacon @Bug we cannot just assume this is circular exposure reach,
            // this method is a general resolution error formatter!!!!
            ResolutionError::UnresolvedUseBinding { binder, extra } => {
                // @Beacon @Task
                Diagnostic::error()
                    .message(format!(
                        "exposure reach ‘{}’ is circular",
                        self.session.index_to_path(binder)
                    ))
                    .unlabeled_span(extra)
                    .report(self.session.reporter())
            }
        }
    }

    // @Question parent: *Local*DeclarationIndex?
    fn attempt_to_access_subbinder_of_non_namespace_error(
        &self,
        binder: ast::Identifier,
        kind: &EntityKind,
        parent: DeclarationIndex,
        subbinder: ast::Identifier,
    ) -> Diagnostic {
        // @Question should we also include lookalike namespaces that don't contain the
        // subbinding (displaying them in a separate help message?)?
        let similarly_named_namespace = self.find_similarly_named_declaration(
            binder.to_str(),
            |entity| {
                entity.namespace().map_or(false, |namespace| {
                    namespace
                        .binders
                        .iter()
                        .any(|&index| self.session[index].source == subbinder)
                })
            },
            parent,
        );

        let show_very_general_help = similarly_named_namespace.is_none();

        Diagnostic::error()
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
        displayed(|f| value.write(self.session, f))
    }
}

pub trait ProgramEntryExt {
    fn look_up_program_entry(&self) -> Option<Identifier>;
}

impl ProgramEntryExt for Session<'_> {
    fn look_up_program_entry(&self) -> Option<Identifier> {
        let resolver = Resolver::new(self);

        let binder = ast::Identifier::new_unchecked(default(), PROGRAM_ENTRY);
        let index = resolver
            .resolve_identifier(
                binder,
                PathResolutionContext::new(self.component().root())
                    .ignore_exposure()
                    .allow_deprecated(),
            )
            .ok()?;
        let entity = &self[index];

        if !entity.is_function() {
            return None;
        }

        Some(Identifier::new(index, entity.source))
    }
}

#[derive(Default, Debug, Clone, Copy)]
enum Context {
    #[default]
    Declaration,
    DataDeclaration {
        index: LocalDeclarationIndex,
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
    namespace: DeclarationIndex,
    origin_namespace: DeclarationIndex,
    usage: IdentifierUsage,
    validate_exposure: bool,
    allow_deprecated: bool,
}

impl PathResolutionContext {
    fn new(namespace: DeclarationIndex) -> Self {
        Self {
            namespace,
            origin_namespace: namespace,
            usage: IdentifierUsage::Unqualified,
            validate_exposure: true,
            allow_deprecated: false,
        }
    }

    fn namespace(self, namespace: DeclarationIndex) -> Self {
        Self { namespace, ..self }
    }

    fn origin_namespace(self, origin_namespace: DeclarationIndex) -> Self {
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
    binder: LocalDeclarationIndex,
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

trait SessionExt {
    fn some_ancestor_equals(&self, index: DeclarationIndex, namespace: DeclarationIndex) -> bool;

    /// Indicate if the definition-site namespace can be accessed from the given namespace.
    fn is_allowed_to_access(
        &self,
        namespace: DeclarationIndex,
        definition_site_namespace: DeclarationIndex,
        reach: DeclarationIndex,
    ) -> bool {
        self.some_ancestor_equals(namespace, definition_site_namespace) // access from same namespace or below
                || self.some_ancestor_equals(namespace, reach) // access from above in reach
    }
}

impl SessionExt for Session<'_> {
    fn some_ancestor_equals(
        &self,
        mut index: DeclarationIndex,
        namespace: DeclarationIndex,
    ) -> bool {
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
    fn compare(&self, other: &Self, session: &Session<'_>) -> Option<Ordering>;
}

impl ExposureCompare for Exposure {
    fn compare(&self, other: &Self, session: &Session<'_>) -> Option<Ordering> {
        use Exposure::*;

        match (self, other) {
            (Unrestricted, Unrestricted) => Some(Ordering::Equal),
            (Unrestricted, Restricted(_)) => Some(Ordering::Greater),
            (Restricted(_), Unrestricted) => Some(Ordering::Less),
            (Restricted(this), Restricted(other)) => {
                let this = this.lock().unwrap();
                let other = other.lock().unwrap();
                this.compare(&other, session)
            }
        }
    }
}

impl ExposureCompare for ExposureReach {
    fn compare(&self, other: &Self, session: &Session<'_>) -> Option<Ordering> {
        Some(match (self, other) {
            (&ExposureReach::Resolved(this), &ExposureReach::Resolved(other)) => {
                let this = this.global(session);
                let other = other.global(session);

                if this == other {
                    Ordering::Equal
                } else if session.some_ancestor_equals(other, this) {
                    Ordering::Greater
                } else if session.some_ancestor_equals(this, other) {
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
    Module(LocalDeclarationIndex),
    FunctionParameter {
        parent: &'a Self,
        binder: ast::Identifier,
    },
    PatternBinders {
        parent: &'a Self,
        binders: Vec<ast::Identifier>,
    },
}

impl<'a> FunctionScope<'a> {
    fn extend_with_parameter(&'a self, binder: ast::Identifier) -> Self {
        Self::FunctionParameter {
            parent: self,
            binder,
        }
    }

    fn extend_with_pattern_binders(&'a self, binders: Vec<ast::Identifier>) -> Self {
        Self::PatternBinders {
            parent: self,
            binders,
        }
    }

    fn module(&self) -> LocalDeclarationIndex {
        match self {
            &Self::Module(module) => module,
            Self::FunctionParameter { parent, .. } | Self::PatternBinders { parent, .. } => {
                parent.module()
            }
        }
    }

    fn depth(&self) -> usize {
        match self {
            Self::Module(_) => 0,
            Self::FunctionParameter { parent, .. } => 1 + parent.depth(),
            Self::PatternBinders { parent, binders } => binders.len() + parent.depth(),
        }
    }

    fn index_from_level(&self, DeBruijnLevel(level): DeBruijnLevel) -> DeBruijnIndex {
        DeBruijnIndex(self.depth() - 1 + level)
    }
}

fn is_similar(identifier: &str, other_identifier: &str) -> bool {
    strsim::levenshtein(other_identifier, identifier) <= std::cmp::max(identifier.len(), 3) / 3
}

struct Lookalike {
    actual: Atom,
    lookalike: Atom,
}

impl Lookalike {
    fn render(&self, painter: &mut Painter) -> io::Result<()> {
        use difference::{Changeset, Difference};
        use std::io::Write;

        let actual = self.actual.to_str();
        let changeset = Changeset::new(actual, self.lookalike.to_str(), "");
        let mut purely_additive = true;

        write!(painter, "‘")?;

        for difference in &changeset.diffs {
            match difference {
                Difference::Same(segment) => write!(painter, "{segment}")?,
                Difference::Add(segment) => {
                    painter.set(Effects::BOLD)?;
                    write!(painter, "{segment}")?;
                    painter.unset()?;
                }
                Difference::Rem(_) => {
                    purely_additive = false;
                }
            }
        }

        write!(painter, "’")?;

        if !(purely_additive || actual.width() == 1 && changeset.distance == 2) {
            write!(painter, " (")?;
            changeset.render(painter)?;
            write!(painter, ")")?;
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

fn module_used_as_a_value_error(module: Spanned<impl fmt::Display>) -> Diagnostic {
    // @Task levenshtein-search for similar named bindings which are in fact values and suggest the first one
    // @Task print absolute path of the module in question and use highlight the entire path, not just the last
    // segment
    // @Task improve this diagnostic!
    Diagnostic::error()
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

    fn output(index: DeclarationIndex, identifier: ast::Identifier) -> Self::Output;

    fn output_bare_path_hanger(
        hanger: &ast::Hanger,
        index: DeclarationIndex,
    ) -> Result<Self::Output, Diagnostic>;

    fn validate_identifier(identifier: ast::Identifier, entity: &Entity) -> Result<(), Diagnostic>;
}

mod target {
    #[allow(clippy::wildcard_imports)] // private inline module
    use super::*;

    pub(super) enum Any {}

    impl ResolutionTarget for Any {
        type Output = DeclarationIndex;

        fn output(index: DeclarationIndex, _: ast::Identifier) -> Self::Output {
            index
        }

        fn output_bare_path_hanger(
            _: &ast::Hanger,
            index: DeclarationIndex,
        ) -> Result<Self::Output, Diagnostic> {
            Ok(index)
        }

        fn validate_identifier(_: ast::Identifier, _: &Entity) -> Result<(), Diagnostic> {
            Ok(())
        }
    }

    /// Marker to specify to only resolve to values.
    pub(super) enum Value {}

    impl ResolutionTarget for Value {
        type Output = Identifier;

        fn output(index: DeclarationIndex, identifier: ast::Identifier) -> Self::Output {
            Identifier::new(index, identifier)
        }

        fn output_bare_path_hanger(
            hanger: &ast::Hanger,
            _: DeclarationIndex,
        ) -> Result<Self::Output, Diagnostic> {
            Err(module_used_as_a_value_error(hanger.as_ref()))
        }

        fn validate_identifier(
            identifier: ast::Identifier,
            entity: &Entity,
        ) -> Result<(), Diagnostic> {
            if entity.is_module() {
                return Err(module_used_as_a_value_error(identifier.into_inner()));
            }

            Ok(())
        }
    }

    pub(super) enum Module {}

    impl ResolutionTarget for Module {
        type Output = DeclarationIndex;

        fn output(index: DeclarationIndex, _: ast::Identifier) -> Self::Output {
            index
        }

        fn output_bare_path_hanger(
            _: &ast::Hanger,
            index: DeclarationIndex,
        ) -> Result<Self::Output, Diagnostic> {
            Ok(index)
        }

        fn validate_identifier(
            identifier: ast::Identifier,
            entity: &Entity,
        ) -> Result<(), Diagnostic> {
            // @Task print absolute path!
            if !(entity.is_module() || entity.is_error()) {
                return Err(Diagnostic::error()
                    .code(ErrorCode::E022)
                    .message(format!("binding ‘{identifier}’ is not a module"))
                    .unlabeled_span(identifier));
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
        identifier: ast::Identifier,
        namespace: DeclarationIndex,
        usage: IdentifierUsage,
    },
    UnresolvedUseBinding {
        binder: DeclarationIndex,
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

    pub(super) fn confliction_definitions(
        binder: ast::Identifier,
        conflicts: &[Span],
    ) -> Diagnostic {
        Diagnostic::error()
            .code(ErrorCode::E020)
            .message(format!(
                "‘{binder}’ is defined multiple times in this scope"
            ))
            .spans(conflicts, "conflicting definition")
    }

    // @Note hmm, taking a Session is not really in the spirit of those error fns
    // but the alternative wouldn't be performant
    pub(super) fn circular_declarations(
        cycle: Cycle<'_, LocalDeclarationIndex>,
        session: &Session<'_>,
    ) -> Diagnostic {
        let paths = cycle
            .iter()
            .map(|&&index| session.local_index_to_path(index).quote())
            .list(Conjunction::And);

        Diagnostic::error()
            .code(ErrorCode::E024)
            .message(format!(
                "the {} {paths} {} circular",
                pluralize!(cycle.len(), "declaration"),
                pluralize!(cycle.len(), "is", "are"),
            ))
            .unlabeled_spans(cycle.into_iter().map(|&index| session[index].source.span()))
    }

    pub(super) fn unqualified_literal(kind: &'static str, span: Span) -> Diagnostic {
        Diagnostic::error()
            .message(format!(
                "{kind} literals without explicit type are not supported yet"
            ))
            .unlabeled_span(span)
            .help("consider prefixing the literal with a path to a type followed by a ‘.’")
    }
}
