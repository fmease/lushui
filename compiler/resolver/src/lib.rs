//! The name resolver.
//!
//! It traverses the [lowered AST](lowered_ast) and registers/defines bindings
//! defined both at module-level using declarations and at function
//! and pattern level as parameters. Furthermore, it resolves all paths inside
//! expressions and patterns to [(resolved) identifiers](Identifier) which
//! contain a [declaration index](DeclarationIndex) or a [de Bruijn index](DeBruijnIndex)
//! respectively.
#![feature(default_free_fn, let_chains, iter_array_chunks)]

// @Task improve docs above!
// @Task get rid of "register" terminology
// @Task transform all Result-returning functions into ()-returning ones modifying self.health
//       and use the new Handler API and get rid of the Stain API

use ast::Explicitness::Explicit;
use colored::Colorize;
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
use hir_format::{ComponentExt as _, DefaultContext, Display};
use session::{
    BuildSession, Component, DeclarationIndexExt, IdentifierExt, LocalDeclarationIndexExt,
};
use span::{Span, Spanned, Spanning};
use std::{cmp::Ordering, default::default, fmt, mem, sync::Mutex};
use token::Word;
use unicode_width::UnicodeWidthStr;
use utilities::{
    cycle::find_cycles, displayed, obtain, pluralize, smallvec, AsAutoColoredChangeset,
    Conjunction, HashMap, ListingExt, QuoteExt, SmallVec,
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
    root_module: lowered_ast::Declaration,
    component: &mut Component,
    session: &mut BuildSession,
) -> Result<hir::Declaration> {
    let mut resolver = ResolverMut::new(component, session);

    if let Err(error) = resolver.start_resolve_declaration(&root_module, None, default()) {
        for (binder, naming_conflicts) in mem::take(&mut resolver.naming_conflicts) {
            Diagnostic::error()
                .code(ErrorCode::E020)
                .message(format!(
                    "‘{}’ is defined multiple times in this scope",
                    resolver.component[binder].source,
                ))
                .labeled_primary_spans(naming_conflicts, "conflicting definition")
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

    let declaration = resolver.finish_resolve_declaration(root_module, None, default());

    Outcome::new(declaration, resolver.health).into()
}

// @Task docs: mention that the current component should be pre-populated before calling this
// (using resolver::resolve)
pub fn resolve_path(
    path: &ast::Path,
    namespace: DeclarationIndex,
    component: &Component,
    session: &BuildSession,
) -> Result<DeclarationIndex> {
    Resolver::new(component, session)
        .resolve_path::<target::Any>(path, PathResolutionContext::new(namespace))
        .map_err(|error| Resolver::new(component, session).report_resolution_error(error))
}

// @Question can we merge Resolver and ResolverMut if we introduce a separate
// lifetime for Resolver.component?
struct ResolverMut<'a> {
    component: &'a mut Component,
    session: &'a mut BuildSession,
    /// For resolving out of order use-declarations.
    partially_resolved_use_bindings: HashMap<LocalDeclarationIndex, PartiallyResolvedUseBinding>,
    /// Naming conflicts used for error reporting.
    ///
    /// Allows us to group conflicts by binder and emit a *single* diagnostic *per group* (making
    /// use of multiple primary highlights).
    naming_conflicts: HashMap<LocalDeclarationIndex, SmallVec<Span, 2>>,
    health: Health,
}

impl<'a> ResolverMut<'a> {
    fn new(component: &'a mut Component, session: &'a mut BuildSession) -> Self {
        Self {
            component,
            session,
            partially_resolved_use_bindings: HashMap::default(),
            naming_conflicts: HashMap::default(),
            health: Health::Untainted,
        }
    }

    fn as_ref(&self) -> Resolver<'_> {
        Resolver {
            component: self.component,
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
        declaration: &lowered_ast::Declaration,
        module: Option<LocalDeclarationIndex>,
        context: Context,
    ) -> Result<(), DefinitionError> {
        use lowered_ast::BareDeclaration::*;

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
                    function.binder.clone(),
                    exposure,
                    declaration.attributes.clone(),
                    EntityKind::UntypedFunction,
                    Some(module),
                )?;

                let binder = Identifier::new(index.global(self.component), function.binder.clone());

                if let Some(intrinsic) = declaration.attributes.get::<{ AttributeName::Intrinsic }>()
                && let Err(error) = self.session
                    .special
                    .define(
                        special::Kind::Intrinsic,
                        binder,
                        match &intrinsic.bare.name {
                            Some(name) => special::DefinitionStyle::Explicit { name },
                            None => special::DefinitionStyle::Implicit { namespace: Some(self.component[module].source.as_str()) },
                        },
                        intrinsic.span,
                    )
                {
                    let _: ErasedReportedError = error.handle(&mut *self);
                }
            }
            Data(type_) => {
                // there is always a root module
                let module = module.unwrap();

                // @Task don't return early, see analoguous code for modules
                let index = self.define(
                    type_.binder.clone(),
                    exposure,
                    declaration.attributes.clone(),
                    EntityKind::untyped_data_type(),
                    Some(module),
                )?;

                let binder = Identifier::new(index.global(self.component), type_.binder.clone());

                let known = declaration.attributes.get::<{ AttributeName::Known }>();
                if let Some(known) = known
                && let Err(error) = self.session.special.define(
                    special::Kind::Known,
                    binder.clone(),
                    match &known.bare.name {
                        Some(name) => special::DefinitionStyle::Explicit { name },
                        None => special::DefinitionStyle::Implicit { namespace: None },
                    },
                    known.span,
                ) {
                    let _: ErasedReportedError = error.handle(&mut *self);
                }

                if let Some(intrinsic) = declaration.attributes.get::<{ AttributeName::Intrinsic }>()
                && let Err(error) = self.session
                    .special
                    .define(
                        special::Kind::Intrinsic,
                        binder,
                        match &intrinsic.bare.name {
                            Some(name) => special::DefinitionStyle::Explicit { name },
                            None => special::DefinitionStyle::Implicit { namespace: None },
                        },
                        intrinsic.span,
                    )
                {
                    let _: ErasedReportedError = error.handle(&mut *self);
                }

                let health = &mut Health::Untainted;

                if let Some(constructors) = &type_.constructors {
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
                let Context::DataDeclaration { index: namespace, transparency, known } = context else { unreachable!() };

                let exposure = match transparency.unwrap() {
                    Transparency::Transparent => self.component[namespace].exposure.clone(),
                    // as if a @(public super) was attached to the constructor
                    Transparency::Abstract => ExposureReach::Resolved(module).into(),
                };

                let index = self.define(
                    constructor.binder.clone(),
                    exposure,
                    declaration.attributes.clone(),
                    EntityKind::UntypedConstructor,
                    Some(namespace),
                )?;

                let binder =
                    Identifier::new(index.global(self.component), constructor.binder.clone());

                // @Task support `@(known name)` on constructors
                if let Some(known) = known
                && let Err(error) = self.session
                    .special
                    .define(
                        special::Kind::Known,
                        binder,
                        special::DefinitionStyle::Implicit { namespace: Some(self.component[namespace].source.as_str()) },
                        known,
                    )
                {
                    let _: ErasedReportedError = error.handle(&mut *self);
                }
            }
            Module(submodule) => {
                // @Task @Beacon don't return early on error
                // @Note you need to create a fake index for this (an index which points to
                // a fake, nameless binding)
                let index = self.define(
                    submodule.binder.clone(),
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
                    use_.binder.clone(),
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
        if let Some(namespace) = namespace {
            if let Some(index) = self.component[namespace]
                .namespace()
                .unwrap()
                .binders
                .iter()
                .map(|&index| index.local(self.component).unwrap())
                .find(|&index| self.component[index].source == binder)
            {
                let previous = &self.component.bindings[index].source;

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

        let index = self.component.bindings.insert(Entity {
            source: binder,
            kind: binding,
            exposure,
            attributes,
            parent: namespace,
        });

        if let Some(namespace) = namespace {
            let index = index.global(self.component);
            self.component[namespace]
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
        declaration: lowered_ast::Declaration,
        module: Option<LocalDeclarationIndex>,
        context: Context,
    ) -> hir::Declaration {
        use lowered_ast::BareDeclaration::*;

        match declaration.bare {
            Function(function) => {
                let module = module.unwrap();

                let binder = self
                    .as_ref()
                    .reobtain_resolved_identifier::<target::Value>(&function.binder, module);

                let type_annotation = self
                    .as_ref()
                    .resolve_expression(function.type_annotation, &FunctionScope::Module(module))
                    .stain(&mut self.health);

                let expression = function.expression.map(|expression| {
                    self.as_ref()
                        .resolve_expression(expression, &FunctionScope::Module(module))
                        .stain(&mut self.health)
                });

                hir::Declaration::new(
                    declaration.attributes,
                    declaration.span,
                    hir::Function {
                        binder,
                        type_annotation,
                        expression,
                    }
                    .into(),
                )
            }
            Data(type_) => {
                let module = module.unwrap();

                // @Beacon @Question wouldn't it be great if that method returned a
                // LocalDeclarationIndex instead of an Identifier?
                // or maybe even a *LocalIdentifier?
                let binder = self
                    .as_ref()
                    .reobtain_resolved_identifier::<target::Value>(&type_.binder, module);

                let type_annotation = self
                    .as_ref()
                    .resolve_expression(type_.type_annotation, &FunctionScope::Module(module))
                    .stain(&mut self.health);

                let constructors = type_.constructors.map(|constructors| {
                    constructors
                        .into_iter()
                        .map(|constructor| {
                            self.finish_resolve_declaration(
                                constructor,
                                Some(module),
                                Context::DataDeclaration {
                                    index: binder.local_declaration_index(self.component).unwrap(),
                                    transparency: None,
                                    known: None,
                                },
                            )
                        })
                        .collect::<Vec<_>>()
                });

                hir::Declaration::new(
                    declaration.attributes,
                    declaration.span,
                    hir::Data {
                        binder,
                        type_annotation,
                        constructors,
                    }
                    .into(),
                )
            }
            Constructor(constructor) => {
                let module = module.unwrap();
                let Context::DataDeclaration { index: namespace, .. } = context else { unreachable!() };

                let binder = self
                    .as_ref()
                    .reobtain_resolved_identifier::<target::Value>(&constructor.binder, namespace);

                let type_annotation = self
                    .as_ref()
                    .resolve_expression(constructor.type_annotation, &FunctionScope::Module(module))
                    .stain(&mut self.health);

                hir::Declaration::new(
                    declaration.attributes,
                    declaration.span,
                    hir::Constructor {
                        binder,
                        type_annotation,
                    }
                    .into(),
                )
            }
            Module(submodule) => {
                let index = match module {
                    // unwrap: could only ever be non-local if the binder was a use-binding
                    // but it is module binding
                    Some(module) => self
                        .as_ref()
                        .reobtain_resolved_identifier::<target::Module>(&submodule.binder, module)
                        .local(self.component)
                        .unwrap(),
                    None => self.component.root_local(),
                };

                let declarations = submodule
                    .declarations
                    .into_iter()
                    .map(|declaration| {
                        self.finish_resolve_declaration(
                            declaration,
                            Some(index),
                            Context::default(),
                        )
                    })
                    .collect();

                hir::Declaration::new(
                    declaration.attributes,
                    declaration.span,
                    hir::Module {
                        binder: Identifier::new(index.global(self.component), submodule.binder),
                        file: submodule.file,
                        declarations,
                    }
                    .into(),
                )
            }
            Use(use_) => {
                let module = module.unwrap();

                let binder = Identifier::new(
                    self.as_ref()
                        .reobtain_resolved_identifier::<target::Any>(&use_.binder, module),
                    use_.binder,
                );

                hir::Declaration::new(
                    declaration.attributes,
                    declaration.span,
                    hir::Use {
                        binder: Some(binder.clone()),
                        target: binder,
                    }
                    .into(),
                )
            }
            Error(error) => PossiblyErroneous::error(error),
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
                let namespace = item.target.namespace.global(self.component);

                match self.as_ref().resolve_path::<target::Any>(
                    &item.target.path,
                    PathResolutionContext::new(namespace),
                ) {
                    Ok(target) => {
                        self.component.bindings[index].kind = EntityKind::Use { target };
                    }
                    Err(error @ (UnresolvedBinding { .. } | Erased(_))) => {
                        self.component.bindings[index].kind =
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
                                binder: binder.local(self.component).unwrap(),
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
                    self.component[index].kind =
                        PossiblyErroneous::error(ErasedReportedError::new_unchecked());
                }

                for cycle in find_cycles(
                    &partially_resolved_use_bindings
                        .into_iter()
                        .map(|(index, binding)| (index, binding.binder))
                        .collect(),
                ) {
                    let paths = cycle
                        .iter()
                        .map(|&index| {
                            self.component
                                .index_to_path(index.global(self.component), self.session)
                                .quote()
                        })
                        .list(Conjunction::And);
                    Diagnostic::error()
                        .code(ErrorCode::E024)
                        .message(format!(
                            "the {} {paths} {} circular",
                            pluralize!(cycle.len(), "declaration"),
                            pluralize!(cycle.len(), "is", "are"),
                        ))
                        .primary_spans(
                            cycle
                                .into_iter()
                                .map(|&index| self.component[index].source.span()),
                        )
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
        for (index, entity) in &self.component.bindings {
            if let Exposure::Restricted(exposure) = &entity.exposure {
                // unwrap: root always has Exposure::Unrestricted, it won't reach this branch
                let definition_site_namespace = entity.parent.unwrap().global(self.component);

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
                let target = self.as_ref().look_up(target_index);

                if entity.exposure.compare(&target.exposure, self.component)
                    == Some(Ordering::Greater)
                {
                    let error = Diagnostic::error()
                        .code(ErrorCode::E009)
                        .message(format!(
                            "re-export of the more private binding ‘{}’",
                            self.component.index_to_path(target_index, self.session)
                        ))
                        .labeled_primary_span(
                            &entity.source,
                            "re-exporting binding with greater exposure",
                        )
                        .labeled_secondary_span(
                            &target.source,
                            "re-exported binding with lower exposure",
                        )
                        .note(format!(
                            "\
expected the exposure of ‘{}’
           to be at most {}
      but it actually is {}",
                            self.component
                                .index_to_path(index.global(self.component), self.session),
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

impl Handler for &mut ResolverMut<'_> {
    fn handle<T: PossiblyErroneous>(self, diagnostic: Diagnostic) -> T {
        let error = diagnostic.report(self.session.reporter());
        self.health.taint(error);
        T::error(error)
    }
}

struct Resolver<'a> {
    component: &'a Component,
    session: &'a BuildSession,
}

impl<'a> Resolver<'a> {
    fn new(component: &'a Component, session: &'a BuildSession) -> Self {
        Self { component, session }
    }

    fn resolve_expression(
        &self,
        expression: lowered_ast::Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<hir::Expression> {
        use lowered_ast::BareExpression::*;

        let expression = match expression.bare {
            PiType(pi) => {
                let domain = self.resolve_expression(pi.domain.clone(), scope);
                let codomain = match pi.parameter.clone() {
                    Some(parameter) => self.resolve_expression(
                        pi.codomain.clone(),
                        &scope.extend_with_parameter(parameter),
                    ),
                    None => self.resolve_expression(pi.codomain.clone(), scope),
                };

                return Ok(hir::Expression::new(
                    expression.attributes,
                    expression.span,
                    hir::PiType {
                        domain: domain?,
                        codomain: codomain?,
                        explicitness: pi.explicitness,
                        laziness: pi.laziness,
                        parameter: pi
                            .parameter
                            .clone()
                            .map(|parameter| Identifier::new(Index::DeBruijnParameter, parameter)),
                    }
                    .into(),
                ));
            }
            Application(application) => {
                let callee = self.resolve_expression(application.callee.clone(), scope);
                let argument = self.resolve_expression(application.argument.clone(), scope);

                return Ok(hir::Expression::new(
                    expression.attributes,
                    expression.span,
                    hir::Application {
                        callee: callee?,
                        argument: argument?,
                        explicitness: application.explicitness,
                    }
                    .into(),
                ));
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
                let parameter_type_annotation = lambda
                    .parameter_type_annotation
                    .clone()
                    .map(|type_| self.resolve_expression(type_, scope));
                let body_type_annotation = lambda.body_type_annotation.clone().map(|type_| {
                    self.resolve_expression(
                        type_,
                        &scope.extend_with_parameter(lambda.parameter.clone()),
                    )
                });
                let body = self.resolve_expression(
                    lambda.body.clone(),
                    &scope.extend_with_parameter(lambda.parameter.clone()),
                );

                hir::Expression::new(
                    expression.attributes,
                    expression.span,
                    hir::Lambda {
                        parameter: Identifier::new(
                            Index::DeBruijnParameter,
                            lambda.parameter.clone(),
                        ),
                        parameter_type_annotation: parameter_type_annotation.transpose()?,
                        body_type_annotation: body_type_annotation.transpose()?,
                        body: body?,
                        explicitness: lambda.explicitness,
                        laziness: lambda.laziness,
                    }
                    .into(),
                )
            }
            UseIn => {
                return Err(Diagnostic::error()
                    .message("use/in-expressions are not supported yet")
                    .primary_span(&expression)
                    .report(self.session.reporter()));
            }
            CaseAnalysis(analysis) => {
                let scrutinee = self.resolve_expression(analysis.scrutinee.clone(), scope)?;
                let mut cases = Vec::new();

                for case in &analysis.cases {
                    let (pattern, binders) = self.resolve_pattern(case.pattern.clone(), scope)?;
                    let body = self.resolve_expression(
                        case.body.clone(),
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
                let mut elements = Vec::new();

                for element in sequence.elements.bare {
                    // @Task use Stain::stain to not return early!
                    elements.push(self.resolve_expression(element, scope)?);
                }

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
            Error(error) => PossiblyErroneous::error(error),
        };

        Ok(expression)
    }

    // @Task use the Stain::stain instead of moving the try operator below resolve calls
    fn resolve_pattern(
        &self,
        pattern: lowered_ast::Pattern,
        scope: &FunctionScope<'_>,
    ) -> Result<(hir::Pattern, Vec<ast::Identifier>)> {
        use lowered_ast::BarePattern::*;

        // @Task replace this hideous binders.append logic
        let mut binders: Vec<ast::Identifier> = Vec::new();

        let pattern = match pattern.bare.clone() {
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
            Binder(binder) => {
                binders.push((*binder).clone());

                hir::Pattern::new(
                    pattern.attributes,
                    pattern.span,
                    hir::Binder(Identifier::new(Index::DeBruijnParameter, *binder)).into(),
                )
            }
            Application(application) => {
                let callee = self.resolve_pattern(application.callee.clone(), scope);
                let argument = self.resolve_pattern(application.argument.clone(), scope);

                let (callee, mut callee_binders) = callee?;
                let (argument, mut argument_binders) = argument?;

                binders.append(&mut callee_binders);
                binders.append(&mut argument_binders);

                hir::Pattern::new(
                    pattern.attributes,
                    pattern.span,
                    hir::Application {
                        callee,
                        explicitness: application.explicitness,
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
                    path.span(),
                    self.resolve_path_of_literal(path, number.bare.literal.span, scope)?,
                ))
            })
            .transpose()?;

        let literal = &number.bare.literal;

        let type_ = match type_ {
            Some(type_) => self
                .session
                .special.get(type_.bare)
                // @Task write more concisely
                .and_then(|intrinsic| obtain!(intrinsic, special::Binding::Type(Type::Numeric(type_)) => type_))
                .ok_or_else(|| {
                    self.literal_used_for_unsupported_type(literal, "number", type_)
                        .report(self.session.reporter())
                })?,
            // for now, we default to `Nat` until we implement polymorphic number literals and their inference
            None => NumericType::Nat,
        };

        let Ok(resolved_number) = hir::Number::parse(&literal.bare, type_) else {
            return Err(Diagnostic::error()
                .code(ErrorCode::E007)
                .message(format!(
                    "number literal ‘{literal}’ does not fit type ‘{type_}’",
                ))
                .primary_span(literal)
                .note(format!(
                    "values of this type must fit integer interval {}",
                    type_.interval(),
                ))
                .report(self.session.reporter()));
        };

        Ok(number.map(|_| resolved_number.into()))
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
                    path.span(),
                    self.resolve_path_of_literal(path, text.bare.literal.span, scope)?,
                ))
            })
            .transpose()?;

        let _type = match type_ {
            Some(type_) => self
                .session
                .special
                .get(type_.bare)
                .filter(|&intrinsic| matches!(intrinsic, special::Binding::Type(Type::Text)))
                .ok_or_else(|| {
                    self.literal_used_for_unsupported_type(&text.bare.literal, "text", type_)
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
            Diagnostic::error()
                .message("sequence literals without explicit type are not supported yet")
                .primary_span(&sequence.bare.elements)
                .help("consider prefixing the literal with a path to a type followed by a ‘.’")
                .report(self.session.reporter())
        })?;

        let type_ = Spanned::new(
            path.span(),
            self.resolve_path_of_literal(path, sequence.bare.elements.span, scope)?,
        );

        // @Task test this!
        let type_ = self
            .session
            .special.get(type_.bare)
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
                .primary_span(span)
                .report(self.session.reporter()));
        };

        // @Task don't unwrap, @Bug this is gonna crash if sb. were to define
        // `List` as `@known data List: … of {}` (no constructors)
        // @Task use `require` instead once it returns an Identifier instead of an Item
        let empty = self
            .session
            .special
            .get(special::Constructor::ListEmpty)
            .unwrap();
        let prepend = self
            .session
            .special
            .get(special::Constructor::ListPrepend)
            .unwrap();

        // @Task check if all those attributes & spans make sense
        let mut result = hir::Item::new(
            default(),
            span,
            hir::Application {
                // @Task don't throw away attributes & span
                callee: hir::Item::new(default(), span, hir::Binding(empty.clone()).into()),
                explicitness: Explicit,
                argument: element_type.clone(),
            }
            .into(),
        );

        for element in elements.rev() {
            // @Task check if all those attributes & spans make sense
            let prepend = hir::Item::new(
                default(),
                element.span,
                hir::Application {
                    callee: hir::Item::new(
                        default(),
                        element.span,
                        hir::Binding(prepend.clone()).into(),
                    ),
                    explicitness: Explicit,
                    argument: element_type.clone(),
                }
                .into(),
            );

            // @Task check if all those attributes & spans make sense
            result = hir::Item::new(
                default(),
                element.span,
                hir::Application {
                    callee: hir::Item::new(
                        default(),
                        element.span,
                        hir::Application {
                            callee: prepend,
                            explicitness: Explicit,
                            argument: element,
                        }
                        .into(),
                    ),
                    explicitness: Explicit,
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
                .primary_span(span)
                .report(self.session.reporter()));
        };

        // @Task don't unwrap, @Bug this is gonna crash if sb. were to define
        // `Vector` as `@known data Vector: … of {}` (no constructors)
        // @Task use `require` instead once it returns an Identifier instead of an Item
        let empty = self
            .session
            .special
            .get(special::Constructor::VectorEmpty)
            .unwrap();
        let prepend = self
            .session
            .special
            .get(special::Constructor::VectorPrepend)
            .unwrap();

        // @Task check if all those attributes & spans make sense
        let mut result = hir::Item::new(
            default(),
            span,
            hir::Application {
                // @Task don't throw away attributes & span
                callee: hir::Item::new(default(), span, hir::Binding(empty.clone()).into()),
                explicitness: Explicit,
                argument: element_type.clone(),
            }
            .into(),
        );

        for (length, element) in elements.rev().enumerate() {
            // @Task check if all those attributes & spans make sense
            let prepend = hir::Item::new(
                default(),
                element.span,
                hir::Application {
                    callee: hir::Item::new(
                        default(),
                        element.span,
                        hir::Application {
                            callee: hir::Item::new(
                                default(),
                                element.span,
                                hir::Binding(prepend.clone()).into(),
                            ),
                            explicitness: Explicit,
                            // @Beacon @Question What happens if the user does not define the intrinsic type `Nat`?
                            //                   Is that going to lead to crashes later on?
                            argument: hir::Item::new(
                                default(),
                                element.span,
                                hir::Number::Nat(length.into()).into(),
                            ),
                        }
                        .into(),
                    ),
                    explicitness: Explicit,
                    argument: element_type.clone(),
                }
                .into(),
            );

            // @Task check if all those attributes & spans make sense
            result = hir::Item::new(
                default(),
                element.span,
                hir::Application {
                    callee: hir::Item::new(
                        default(),
                        element.span,
                        hir::Application {
                            callee: prepend,
                            explicitness: Explicit,
                            argument: element,
                        }
                        .into(),
                    ),
                    explicitness: Explicit,
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
                .primary_span(elements.span)
                .report(self.session.reporter()));
        }

        // @Task don't unwrap, @Bug this is gonna crash if sb. were to define
        // `Tuple` as `@known data Tuple: … of {}` (no constructors)
        // @Task use `require` instead once it returns an Identifier instead of an Item
        let empty = self
            .session
            .special
            .get(special::Constructor::TupleEmpty)
            .unwrap();
        let prepend = self
            .session
            .special
            .get(special::Constructor::TuplePrepend)
            .unwrap();
        let type_ = self.session.special.get(special::Type::Type).unwrap();

        // @Task check if all those attributes & spans make sense
        let mut result =
            hir::Item::new(default(), elements.span, hir::Binding(empty.clone()).into());
        let mut list = vec![hir::Item::new(
            default(),
            elements.span,
            hir::Binding(type_.clone()).into(),
        )];

        for [element_type, element] in elements.bare.into_iter().array_chunks().rev() {
            // @Task check if all those attributes & spans make sense
            let prepend = hir::Item::new(
                default(),
                element.span,
                hir::Application {
                    callee: hir::Item::new(
                        default(),
                        element.span,
                        hir::Application {
                            callee: hir::Item::new(
                                default(),
                                element.span,
                                hir::Binding(prepend.clone()).into(),
                            ),
                            explicitness: Explicit,
                            argument: element_type.clone(),
                        }
                        .into(),
                    ),
                    explicitness: Explicit,
                    argument: self
                        .resolve_list_literal(Spanned::new(element.span, list.clone()))?,
                }
                .into(),
            );

            // @Task find a cleaner approach
            list.insert(1, element_type);

            // @Task check if all those attributes & spans make sense
            result = hir::Item::new(
                default(),
                element.span,
                hir::Application {
                    callee: hir::Item::new(
                        default(),
                        element.span,
                        hir::Application {
                            callee: prepend,
                            explicitness: Explicit,
                            argument: element,
                        }
                        .into(),
                    ),
                    explicitness: Explicit,
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
                self.look_up(type_.bare).source
            ))
            .labeled_primary_span(literal, "this literal may not construct the type")
            .labeled_secondary_span(type_, "the data type")
    }

    fn resolve_path_of_literal(
        &self,
        path: &ast::Path,
        literal: Span,
        scope: &FunctionScope<'_>,
    ) -> Result<DeclarationIndex> {
        let namespace = scope.module().global(self.component);
        let binding = self
            .resolve_path::<target::Any>(path, PathResolutionContext::new(namespace))
            .map_err(|error| self.report_resolution_error(error))?;

        {
            let entity = self.look_up(binding);
            if !entity.is_data_type() {
                // @Task code
                return Err(Diagnostic::error()
                    .message(format!("binding ‘{path}’ is not a data type"))
                    // @Task future-proof a/an
                    .labeled_primary_span(path, format!("a {}", entity.kind.name()))
                    .labeled_secondary_span(
                        literal,
                        "literal requires a data type as its namespace",
                    )
                    .report(self.session.reporter()));
            }
        }

        Ok(binding)
    }

    // @Task dedup with documenter, code generator, interpreter
    fn look_up(&self, index: DeclarationIndex) -> &'a Entity {
        match index.local(self.component) {
            Some(index) => &self.component[index],
            None => &self.session[index],
        }
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
                            .primary_span(hanger)
                            .note("the path segment ‘extern’ is only to be used indirectly to refer to specific component")
                            .report(self.session.reporter()).into());
                    };

                    // @Beacon @Task add test for error case
                    let component: Spanned<Word> = component.clone().try_into().map_err(|_| {
                        // @Task DRY @Question is the common code justified?
                        Diagnostic::error()
                            .code(ErrorCode::E036)
                            .message(format!(
                                "the component name ‘{component}’ is not a valid word"
                            ))
                            .primary_span(component)
                            .report(self.session.reporter())
                    })?;

                    let Some(component) = self.component.dependencies.get(&component.bare).copied() else {
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
                            .primary_span(component)
                            .report(self.session.reporter()).into());
                    };

                    let component = &self.session[component];
                    let root = component.root();

                    return match &*path.segments {
                        [identifier] => Ok(Target::output(root, identifier)),
                        [_, identifiers @ ..] => self.resolve_path::<Target>(
                            // @Task use PathView once available
                            &ast::Path::with_segments(identifiers.to_owned().into()),
                            PathResolutionContext::new(root)
                                .origin_namespace(context.origin_namespace)
                                .qualified_identifier(),
                        ),
                        [] => unreachable!(),
                    };
                }
                Topmost => self.component.root(),
                Super => self
                    .resolve_super(hanger, context.namespace.local(self.component).unwrap())?
                    .global(self.component),
                Self_ => context.namespace,
            };

            return if path.segments.is_empty() {
                Target::output_bare_path_hanger(hanger, namespace)
                    .map_err(|error| error.report(self.session.reporter()).into())
            } else {
                self.resolve_path::<Target>(
                    // @Task use PathView once available
                    &ast::Path::with_segments(path.segments.clone()),
                    context.namespace(namespace).qualified_identifier(),
                )
            };
        }

        let index = self.resolve_identifier(&path.segments[0], context)?;
        let entity = self.look_up(index);

        match &*path.segments {
            [identifier] => {
                Target::validate_identifier(identifier, entity)
                    .map_err(|error| error.report(self.session.reporter()))?;
                Ok(Target::output(index, identifier))
            }
            [identifier, identifiers @ ..] => {
                if entity.is_namespace() {
                    self.resolve_path::<Target>(
                        // @Task use PathView once available
                        &ast::Path::with_segments(identifiers.to_owned().into()),
                        context.namespace(index).qualified_identifier(),
                    )
                } else if entity.is_error() {
                    // @Task add rationale why `last`
                    Ok(Target::output(index, identifiers.last().unwrap()))
                } else {
                    let diagnostic = self.attempt_to_access_subbinder_of_non_namespace(
                        identifier,
                        &entity.kind,
                        context.namespace,
                        identifiers.first().unwrap(),
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
        self.component[module].parent.ok_or_else(|| {
            Diagnostic::error()
                .code(ErrorCode::E021) // @Question use a dedicated code?
                .message("the root module does not have a parent module")
                .primary_span(hanger)
                .report(self.session.reporter())
        })
    }

    fn resolve_identifier(
        &self,
        identifier: &ast::Identifier,
        context: PathResolutionContext,
    ) -> Result<DeclarationIndex, ResolutionError> {
        let index = self
            .look_up(context.namespace)
            .namespace()
            .unwrap()
            .binders
            .iter()
            .copied()
            .find(|&index| &self.look_up(index).source == identifier)
            .ok_or_else(|| ResolutionError::UnresolvedBinding {
                identifier: identifier.clone(),
                namespace: context.namespace,
                usage: context.usage,
            })?;

        // @Temporary hack until we can manage cyclic exposure reaches!
        if context.validate_exposure {
            self.handle_exposure(index, identifier, context.origin_namespace)?;
        }

        if !context.allow_deprecated
        && let Some(deprecated) = self
            .look_up(index)
            .attributes
            .get::<{ AttributeName::Deprecated }>()
        {
            let mut message = format!(
                "use of deprecated binding ‘{}’",
                self.component.index_to_path(index, self.session),
            );

            if let Some(reason) = &deprecated.bare.reason {
                message += ": ";
                message += reason;
            }

            Diagnostic::warning()
                .code(LintCode::Deprecated)
                .message(message)
                .primary_span(identifier)
                .report(self.session.reporter());
        }

        self.collapse_use_chain(index, identifier.span())
    }

    // @Task verify that the exposure is checked even in the case of use-declarations
    // using use-bindings (use-chains).
    fn handle_exposure(
        &self,
        index: DeclarationIndex,
        identifier: &ast::Identifier,
        origin_namespace: DeclarationIndex,
    ) -> Result<(), ResolutionError> {
        let entity = self.look_up(index);

        if let Exposure::Restricted(exposure) = &entity.exposure {
            // unwrap: root always has Exposure::Unrestricted
            let definition_site_namespace = entity.parent.unwrap();
            let reach = self.resolve_restricted_exposure(
                exposure,
                definition_site_namespace.global(self.component),
            )?;

            if !self.component.is_allowed_to_access(
                origin_namespace,
                definition_site_namespace.global(self.component),
                reach,
            ) {
                return Err(Diagnostic::error()
                    .code(ErrorCode::E029)
                    .message(format!(
                        "binding ‘{}’ is private",
                        self.component.index_to_path(index, self.session)
                    ))
                    .primary_span(identifier)
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

        match self.look_up(index).kind {
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
                        PathResolutionContext::new(namespace.global(self.component))
                            .ignore_exposure(),
                    )
                    .map_err(|error| self.report_resolution_error(error))?;

                let reach_is_ancestor = self
                    .component
                    .some_ancestor_equals(definition_site_namespace, reach);

                if !reach_is_ancestor {
                    return Err(Diagnostic::error()
                        .code(ErrorCode::E037)
                        .message("exposure can only be restricted to ancestor modules")
                        .primary_span(path)
                        .report(self.session.reporter()));
                }

                drop(exposure_);
                *exposure.lock().unwrap() =
                    // @Question unwrap() correct?
                    ExposureReach::Resolved(reach.local(self.component).unwrap());

                reach
            }
            &ExposureReach::Resolved(reach) => reach.global(self.component),
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
    /// lowered AST and the HIR where all the _binders_ of declarations are resolved
    /// (i.e. are [`Identifier`]s) but all _bindings_ (in type annotations, expressions, …)
    /// are still unresolved (i.e. are [`ast::Identifier`]s).
    ///
    /// Such an IR would imply writing a lot of boilerplate if we were to duplicate
    /// definitions & mappings or – if even possible – creating a totally complicated
    /// parameterized lowered AST with complicated traits having many associated types
    /// (painfully learned through previous experiences).
    // @Task add to documentation that this panics on unresolved and does not check exposure,
    // also it does not check the resolution target etc
    // @Task add that it may only fail if circular use-bindings were found or a use
    // binding could not be resolved since `Self::resolve_use_binding` is treated non-fatally
    // in Resolver::resolve_declaration
    fn reobtain_resolved_identifier<Target: ResolutionTarget>(
        &self,
        identifier: &ast::Identifier,
        namespace: LocalDeclarationIndex,
    ) -> Target::Output {
        let index = self.component[namespace]
            .namespace()
            .unwrap()
            .binders
            .iter()
            .map(|index| index.local(self.component).unwrap())
            .find(|&index| &self.component[index].source == identifier)
            .unwrap();
        let index = self
            .collapse_use_chain(index.global(self.component), identifier.span())
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
                    if binder == identifier {
                        Ok(Identifier::new(DeBruijnIndex(depth), identifier.clone()))
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
                        .find(|(binder, _)| binder == &identifier)
                    {
                        Some((_, depth)) => {
                            Ok(Identifier::new(DeBruijnIndex(depth), identifier.clone()))
                        }
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
                PathResolutionContext::new(scope.module().global(self.component)),
            )
            .map_err(|error| {
                self.report_resolution_error_searching_lookalikes(error, |identifier, _| {
                    self.find_similarly_named(origin, identifier)
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
    fn find_similarly_named_declaration<'s>(
        &'s self,
        identifier: &str,
        predicate: impl Fn(&'s Entity) -> bool,
        namespace: DeclarationIndex,
    ) -> Option<&'s str> {
        self.look_up(namespace)
            .namespace()
            .unwrap()
            .binders
            .iter()
            .map(|&index| self.look_up(index))
            .filter(|entity| !entity.is_error() && predicate(entity))
            .map(|entity| entity.source.as_str())
            .find(|some_identifier| is_similar(some_identifier, identifier))
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
    ) -> Option<&'s str> {
        use FunctionScope::*;

        match scope {
            &Module(module) => self.find_similarly_named_declaration(
                identifier,
                |_| true,
                module.global(self.component),
            ),
            FunctionParameter { parent, binder } => {
                if is_similar(identifier, binder.as_str()) {
                    Some(binder.as_str())
                } else {
                    self.find_similarly_named(parent, identifier)
                }
            }
            PatternBinders { parent, binders } => {
                if let Some(binder) = binders
                    .iter()
                    .rev()
                    .find(|binder| is_similar(identifier, binder.as_str()))
                {
                    Some(binder.as_str())
                } else {
                    self.find_similarly_named(parent, identifier)
                }
            }
        }
    }

    fn report_resolution_error(&self, error: ResolutionError) -> ErasedReportedError {
        self.report_resolution_error_searching_lookalikes(error, |identifier, namespace| {
            self.find_similarly_named_declaration(identifier, |_| true, namespace)
        })
    }

    fn report_resolution_error_searching_lookalikes<'s>(
        &self,
        error: ResolutionError,
        lookalike_finder: impl FnOnce(&str, DeclarationIndex) -> Option<&'s str>,
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
                        if namespace == self.component.root() {
                            message += "the root module";
                        } else {
                            message += match self.look_up(namespace).is_module() {
                                true => "module",
                                false => "namespace",
                            };
                            message += " ‘";
                            message += &self.component.index_to_path(namespace, self.session);
                            message += "’";
                        }
                    }
                }

                Diagnostic::error()
                    .code(ErrorCode::E021)
                    .message(message)
                    .primary_span(&identifier)
                    .with(
                        |error| match lookalike_finder(identifier.as_str(), namespace) {
                            Some(lookalike) => error.help(format!(
                                "a binding with a similar name exists in scope: {}",
                                Lookalike {
                                    actual: identifier.as_str(),
                                    lookalike,
                                },
                            )),
                            None => error,
                        },
                    )
                    .report(self.session.reporter())
            }
            // @Beacon @Bug we cannot just assume this is circular exposure reach,
            // this method is a general resolution error formatter!!!!
            ResolutionError::UnresolvedUseBinding { binder, extra } => {
                // @Beacon @Task
                Diagnostic::error()
                    .message(format!(
                        "exposure reach ‘{}’ is circular",
                        self.component.index_to_path(binder, self.session)
                    ))
                    .primary_span(extra)
                    .report(self.session.reporter())
            }
        }
    }

    // @Question parent: *Local*DeclarationIndex?
    fn attempt_to_access_subbinder_of_non_namespace(
        &self,
        binder: &ast::Identifier,
        kind: &EntityKind,
        parent: DeclarationIndex,
        subbinder: &ast::Identifier,
    ) -> Diagnostic {
        // @Question should we also include lookalike namespaces that don't contain the
        // subbinding (displaying them in a separate help message?)?
        let similarly_named_namespace = self.find_similarly_named_declaration(
            binder.as_str(),
            |entity| {
                entity.namespace().map_or(false, |namespace| {
                    namespace
                        .binders
                        .iter()
                        .any(|&index| &self.look_up(index).source == subbinder)
                })
            },
            parent,
        );

        let show_very_general_help = similarly_named_namespace.is_none();

        Diagnostic::error()
            .code(ErrorCode::E017)
            .message(format!("binding ‘{binder}’ is not a namespace"))
            .labeled_primary_span(binder, format!("not a namespace but a {}", kind.name()))
            .labeled_secondary_span(
                // the subbinder together with the leading dot
                // @Task trim_start_matches ascii_whitespace
                binder.span().end().merge(subbinder),
                "denotes a reference to a binding inside of a namespace",
            )
            .with(|error| match similarly_named_namespace {
                Some(lookalike) => error.help(format!(
                "a namespace with a similar name exists in scope containing the binding:\n    {}",
                Lookalike {
                    actual: binder.as_str(),
                    lookalike
                },
            )),
                None => error,
            })
            .with(|error| {
                if show_very_general_help {
                    // no type information here yet to check if the non-namespace is indeed a record
                    error.help("use ‘::’ to reference a field of a record")
                } else {
                    error
                }
            })
    }

    fn display<'f>(
        &'f self,
        value: &'f impl Display<Context<'f> = DefaultContext<'f>>,
    ) -> impl std::fmt::Display + 'f {
        displayed(|f| value.write((self.component, self.session), f))
    }
}

pub trait ProgramEntryExt {
    const PROGRAM_ENTRY_IDENTIFIER: &'static str = "main";

    fn look_up_program_entry(&self, session: &BuildSession) -> Option<Identifier>;
}

impl ProgramEntryExt for Component {
    fn look_up_program_entry(&self, session: &BuildSession) -> Option<Identifier> {
        let resolver = Resolver::new(self, session);

        let identifier =
            ast::Identifier::new_unchecked(Self::PROGRAM_ENTRY_IDENTIFIER.into(), default());
        let index = resolver
            .resolve_identifier(
                &identifier,
                PathResolutionContext::new(self.root())
                    .ignore_exposure()
                    .allow_deprecated(),
            )
            .ok()?;
        let entity = &resolver.look_up(index);

        if !entity.is_function() {
            return None;
        }

        Some(Identifier::new(index, entity.source.clone()))
    }
}

#[derive(Default, Debug)]
enum Context {
    #[default]
    Boring,
    DataDeclaration {
        index: LocalDeclarationIndex,
        transparency: Option<Transparency>,
        known: Option<Span>,
    },
}

#[derive(Debug)]
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

trait ComponentExt {
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

impl ComponentExt for Component {
    // @Beacon @Question shouldn't `index` be a LocalDeclarationIndex?
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
    fn compare(&self, other: &Self, component: &Component) -> Option<Ordering>;
}

impl ExposureCompare for Exposure {
    fn compare(&self, other: &Self, component: &Component) -> Option<Ordering> {
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
    fn compare(&self, other: &Self, component: &Component) -> Option<Ordering> {
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
}

fn is_similar(identifier: &str, other_identifier: &str) -> bool {
    strsim::levenshtein(other_identifier, identifier) <= std::cmp::max(identifier.len(), 3) / 3
}

struct Lookalike<'a> {
    actual: &'a str,
    lookalike: &'a str,
}

impl fmt::Display for Lookalike<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use difference::{Changeset, Difference};

        let changeset = Changeset::new(self.actual, self.lookalike, "");
        let mut purely_additive = true;

        write!(f, "‘")?;

        for difference in &changeset.diffs {
            match difference {
                Difference::Same(segment) => write!(f, "{segment}")?,
                Difference::Add(segment) => write!(f, "{}", segment.bold())?,
                Difference::Rem(_) => {
                    purely_additive = false;
                }
            }
        }

        write!(f, "’")?;

        if !(purely_additive || self.actual.width() == 1 && changeset.distance == 2) {
            write!(f, " ({})", changeset.auto_colored())?;
        }

        Ok(())
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
            .primary_span(module)
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

    fn output(index: DeclarationIndex, identifier: &ast::Identifier) -> Self::Output;

    fn output_bare_path_hanger(
        hanger: &ast::Hanger,
        index: DeclarationIndex,
    ) -> Result<Self::Output, Diagnostic>;

    fn validate_identifier(identifier: &ast::Identifier, entity: &Entity)
        -> Result<(), Diagnostic>;
}

mod target {
    #[allow(clippy::wildcard_imports)]
    use super::*;

    pub(super) enum Any {}

    impl ResolutionTarget for Any {
        type Output = DeclarationIndex;

        fn output(index: DeclarationIndex, _: &ast::Identifier) -> Self::Output {
            index
        }

        fn output_bare_path_hanger(
            _: &ast::Hanger,
            index: DeclarationIndex,
        ) -> Result<Self::Output, Diagnostic> {
            Ok(index)
        }

        fn validate_identifier(_: &ast::Identifier, _: &Entity) -> Result<(), Diagnostic> {
            Ok(())
        }
    }

    /// Marker to specify to only resolve to values.
    pub(super) enum Value {}

    impl ResolutionTarget for Value {
        type Output = Identifier;

        fn output(index: DeclarationIndex, identifier: &ast::Identifier) -> Self::Output {
            Identifier::new(index, identifier.clone())
        }

        fn output_bare_path_hanger(
            hanger: &ast::Hanger,
            _: DeclarationIndex,
        ) -> Result<Self::Output, Diagnostic> {
            Err(module_used_as_a_value_error(hanger.as_ref()))
        }

        fn validate_identifier(
            identifier: &ast::Identifier,
            entity: &Entity,
        ) -> Result<(), Diagnostic> {
            if entity.is_module() {
                return Err(module_used_as_a_value_error(identifier.as_spanned_str()));
            }

            Ok(())
        }
    }

    pub(super) enum Module {}

    impl ResolutionTarget for Module {
        type Output = DeclarationIndex;

        fn output(index: DeclarationIndex, _: &ast::Identifier) -> Self::Output {
            index
        }

        fn output_bare_path_hanger(
            _: &ast::Hanger,
            index: DeclarationIndex,
        ) -> Result<Self::Output, Diagnostic> {
            Ok(index)
        }

        fn validate_identifier(
            identifier: &ast::Identifier,
            entity: &Entity,
        ) -> Result<(), Diagnostic> {
            // @Task print absolute path!
            if !(entity.is_module() || entity.is_error()) {
                return Err(Diagnostic::error()
                    .code(ErrorCode::E022)
                    .message(format!("binding ‘{identifier}’ is not a module"))
                    .primary_span(identifier));
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
