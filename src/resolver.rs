//! The name resolver.
//!
//! It traverses the [lowered AST](lowered_ast) and registers bindings
//! defined both at module-level using declarations and at function
//! and pattern level as parameters. Furthermore, it resolves all paths inside
//! expressions and patterns to [(resolved) identifiers](Identifier) which
//! contain a [declaration index](DeclarationIndex) or a [de Bruijn index](DeBruijnIndex)
//! respectively.

mod scope;

use crate::{
    diagnostics::{Code, Diagnostic, Reporter},
    entity::{Entity, EntityKind},
    error::{Health, PossiblyErroneous, ReportedExt, Result},
    format::{pluralize, unordered_listing, Conjunction, DisplayWith, QuoteExt},
    hir::{self, decl, expr, pat},
    package::BuildSession,
    span::Spanning,
    syntax::{
        ast,
        lowered_ast::{self, AttributeKeys, AttributeKind},
    },
    utility::{obtain, unrc, HashMap, HashSet},
};
pub use scope::{
    CrateScope, DeBruijnIndex, DeclarationIndex, Exposure, FunctionScope, Identifier, Index,
    LocalDeclarationIndex, Namespace,
};
use scope::{RegistrationError, RestrictedExposure};
use std::{cell::RefCell, cmp::Ordering, collections::hash_map::Entry};

const PROGRAM_ENTRY_IDENTIFIER: &str = "main";

/// The state of the resolver.
pub struct Resolver<'a> {
    scope: &'a mut CrateScope,
    session: &'a BuildSession,
    reporter: &'a Reporter,
}

impl<'a> Resolver<'a> {
    pub fn new(
        scope: &'a mut CrateScope,
        session: &'a BuildSession,
        reporter: &'a Reporter,
    ) -> Self {
        Self {
            scope,
            session,
            reporter,
        }
    }

    /// Resolve the names of a declaration.
    ///
    /// It performs four passes to resolve all possible out of order declarations.
    ///
    /// # Panics
    ///
    /// If the declaration passed is not a module, this function will panic as it
    /// requires a crate root which is defined through the root module.
    pub fn resolve_declaration(
        &mut self,
        declaration: lowered_ast::Declaration,
    ) -> Result<hir::Declaration> {
        self.start_resolve_declaration(&declaration, None, Context::default())
            .map_err(|_| {
                std::mem::take(&mut self.scope.duplicate_definitions)
                    .into_values()
                    .for_each(|error| Diagnostic::from(error).report(self.reporter));
            })?;

        self.resolve_use_bindings();

        // @Task @Beacon don't return early here
        self.resolve_exposure_reaches()?;

        let declaration = self.finish_resolve_declaration(declaration, None, Context::default());

        Result::from(self.scope.health).and(declaration)
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
    /// For more on this, see [`Self::reobtain_resolved_identifier`].
    fn start_resolve_declaration(
        &mut self,
        declaration: &lowered_ast::Declaration,
        module: Option<LocalDeclarationIndex>,
        context: Context,
    ) -> Result<(), RegistrationError> {
        use lowered_ast::DeclarationKind::*;

        let exposure = match declaration.attributes.has(AttributeKeys::PUBLIC) {
            true => match declaration
                .attributes
                .get(|kind| obtain!(kind, AttributeKind::Public { reach } => reach))
            {
                Some(reach) => RestrictedExposure::Unresolved {
                    reach: reach.clone(),
                }
                .into(),
                None => Exposure::Unrestricted,
            },
            false => match module {
                // a lack of `@public` means private i.e. restricted to `self` i.e. `@(public self)`
                Some(module) => RestrictedExposure::Resolved { reach: module }.into(),
                None => Exposure::Unrestricted,
            },
        };

        match &declaration.data {
            Value(value) => {
                let module = module.unwrap();

                let index = self.scope.register_binding(
                    value.binder.clone(),
                    exposure,
                    EntityKind::UntypedValue,
                    Some(module),
                )?;

                if self.scope.program_entry.is_none()
                    && module == self.scope.root()
                    && value.binder.as_str() == PROGRAM_ENTRY_IDENTIFIER
                {
                    self.scope.program_entry = Some(Identifier::new(
                        self.scope.global_index(index),
                        value.binder.clone(),
                    ));
                }
            }
            Data(data) => {
                // there is always a root module
                let module = module.unwrap();

                // @Task don't return early, see analoguous code for modules
                let namespace = self.scope.register_binding(
                    data.binder.clone(),
                    exposure,
                    EntityKind::untyped_data_type(),
                    Some(module),
                )?;

                if let Some(constructors) = &data.constructors {
                    // @Task awkward API: Result <-> Result<(), RegistrationError>
                    let health =
                        constructors
                            .iter()
                            .fold(Health::Untainted, |health, constructor| {
                                let opacity =
                                    match declaration.attributes.has(AttributeKeys::OPAQUE) {
                                        true => Opacity::Opaque,
                                        false => Opacity::Transparent,
                                    };

                                health
                                    & self
                                        .start_resolve_declaration(
                                            constructor,
                                            Some(module),
                                            Context {
                                                parent_data_binding: Some((
                                                    namespace,
                                                    Some(opacity),
                                                )),
                                            },
                                        )
                                        .map_err(|_| ())
                            });
                    return Result::from(health).map_err(|_| RegistrationError::Unrecoverable);
                }
            }
            Constructor(constructor) => {
                // there is always a root module
                let module = module.unwrap();
                let (namespace, module_opacity) = context.parent_data_binding.unwrap();

                let exposure = match module_opacity.unwrap() {
                    Opacity::Transparent => self.scope[namespace].exposure.clone(),
                    // as if a @(public super) was attached to the constructor
                    Opacity::Opaque => RestrictedExposure::Resolved { reach: module }.into(),
                };

                // @Task don't return early, see analoguous code for modules
                let namespace = self.scope.register_binding(
                    constructor.binder.clone(),
                    exposure,
                    EntityKind::untyped_constructor(),
                    Some(namespace),
                )?;

                let mut overall_result = Ok(());

                let mut type_annotation = &constructor.type_annotation;

                while let lowered_ast::ExpressionKind::PiType(pi) = &type_annotation.data {
                    if pi.aspect.is_field() {
                        let parameter = pi.parameter.as_ref().unwrap();

                        overall_result = self
                            .scope
                            .register_binding(
                                parameter.clone(),
                                Exposure::Unrestricted, // @Temporary
                                EntityKind::UntypedValue,
                                Some(namespace),
                            )
                            .map(|_| ());
                    }
                    type_annotation = &pi.codomain;
                }

                overall_result?;
            }
            Module(submodule) => {
                // @Task @Beacon don't return early on error
                // @Note you need to create a fake index for this (an index which points to
                // a fake, nameless binding)
                let index = self.scope.register_binding(
                    submodule.binder.clone(),
                    exposure,
                    EntityKind::module(),
                    module,
                )?;

                // @Note awkward API
                let health =
                    submodule
                        .declarations
                        .iter()
                        .fold(Health::Untainted, |health, declaration| {
                            health
                                & self
                                    .start_resolve_declaration(
                                        declaration,
                                        Some(index),
                                        Context::default(),
                                    )
                                    .map_err(|_| ())
                        });
                return Result::from(health).map_err(|_| RegistrationError::Unrecoverable);
            }
            Use(use_) => {
                // there is always a root module
                let module = module.unwrap();

                let index = self.scope.register_binding(
                    use_.binder.clone(),
                    exposure,
                    EntityKind::UnresolvedUse,
                    Some(module),
                )?;

                self.scope
                    .register_use_binding(index, use_.target.clone(), module);
            }
            Error => {}
        }

        Ok(())
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
    ) -> Result<hir::Declaration> {
        use lowered_ast::DeclarationKind::*;

        Ok(match declaration.data {
            Value(value) => {
                let module = module.unwrap();

                let type_annotation =
                    self.resolve_expression(value.type_annotation, &FunctionScope::Module(module));

                let expression = value
                    .expression
                    .map(|expression| {
                        self.resolve_expression(expression, &FunctionScope::Module(module))
                    })
                    .transpose();

                let binder = self.reobtain_resolved_identifier::<OnlyValue>(&value.binder, module);

                decl! {
                    Value {
                        declaration.attributes,
                        declaration.span;
                        type_annotation: type_annotation?,
                        expression: expression?,
                        binder,
                    }
                }
            }
            Data(data) => {
                let module = module.unwrap();

                let type_annotation =
                    self.resolve_expression(data.type_annotation, &FunctionScope::Module(module));

                // @Beacon @Question wouldn't it be great if that method returned a
                // LocalDeclarationIndex instead of an Identifier?
                // or maybe even a *LocalIdentifier?
                let binder = self.reobtain_resolved_identifier::<OnlyValue>(&data.binder, module);

                let constructors = data.constructors.map(|constructors| {
                    constructors
                        .into_iter()
                        .map(|constructor| {
                            self.finish_resolve_declaration(
                                constructor,
                                Some(module),
                                Context {
                                    parent_data_binding: Some((
                                        self.scope
                                            .local_index(binder.declaration_index().unwrap())
                                            .unwrap(),
                                        None,
                                    )),
                                },
                            )
                        })
                        .collect()
                });

                decl! {
                    Data {
                        declaration.attributes,
                        declaration.span;
                        constructors: constructors.transpose()?,
                        type_annotation: type_annotation?,
                        binder,
                    }
                }
            }
            Constructor(constructor) => {
                let module = module.unwrap();

                let type_annotation = self.resolve_expression(
                    constructor.type_annotation,
                    &FunctionScope::Module(module),
                )?;

                let binder = self.reobtain_resolved_identifier::<OnlyValue>(
                    &constructor.binder,
                    context.parent_data_binding.unwrap().0,
                );

                decl! {
                    Constructor {
                        declaration.attributes,
                        declaration.span;
                        binder,
                        type_annotation,
                    }
                }
            }
            Module(submodule) => {
                let index = match module {
                    // unwrap: could only ever be non-local if the binder was a use-binding
                    // but it is module binding
                    Some(module) => {
                        self.scope
                            .local_index(self.reobtain_resolved_identifier::<OnlyModule>(
                                &submodule.binder,
                                module,
                            ))
                            .unwrap()
                    }
                    None => self.scope.root(),
                };

                let mut health = Health::Untainted;

                // @Note awkward API
                let declarations = submodule
                    .declarations
                    .into_iter()
                    .flat_map(|declaration| {
                        let declaration = self.finish_resolve_declaration(
                            declaration,
                            Some(index),
                            Context::default(),
                        );
                        if declaration.is_err() {
                            health.taint();
                        }
                        declaration
                    })
                    .collect();

                Result::from(health)?;

                decl! {
                    Module {
                        declaration.attributes,
                        declaration.span;
                        binder: Identifier::new(self.scope.global_index(index), submodule.binder),
                        file: submodule.file,
                        declarations,
                    }
                }
            }
            Use(use_) => {
                let module = module.unwrap();

                let binder = Identifier::new(
                    self.reobtain_resolved_identifier::<ValueOrModule>(&use_.binder, module),
                    use_.binder,
                );

                decl! {
                    Use {
                        declaration.attributes,
                        declaration.span;
                        binder: Some(binder.clone()),
                        target: binder,
                    }
                }
            }
            Error => PossiblyErroneous::error(),
        })
    }

    // @Task @Beacon use Rc::try_unwrap more instead of clone
    fn resolve_expression(
        &mut self,
        expression: lowered_ast::Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<hir::Expression> {
        use lowered_ast::ExpressionKind::*;

        let expression = match expression.data {
            PiType(pi) => {
                let domain = self.resolve_expression(pi.domain.clone(), scope);
                let codomain = match pi.parameter.clone() {
                    Some(parameter) => self.resolve_expression(
                        pi.codomain.clone(),
                        &scope.extend_with_parameter(parameter),
                    ),
                    None => self.resolve_expression(pi.codomain.clone(), scope),
                };

                return Ok(expr! {
                    PiType {
                        expression.attributes,
                        expression.span;
                        domain: domain?,
                        codomain: codomain?,
                        explicitness: pi.explicitness,
                        aspect: pi.aspect,
                        parameter: pi.parameter.clone()
                            .map(|parameter| Identifier::new(Index::DeBruijnParameter, parameter)),
                    }
                });
            }
            Application(application) => {
                let callee = self.resolve_expression(application.callee.clone(), scope);
                let argument = self.resolve_expression(application.argument.clone(), scope);

                return Ok(expr! {
                    Application {
                        expression.attributes,
                        expression.span;
                        callee: callee?,
                        argument: argument?,
                        explicitness: application.explicitness,
                    }
                });
            }
            Type => expr! { Type { expression.attributes, expression.span } },
            Number(number) => expr! { Number(expression.attributes, expression.span; number) },
            Text(text) => expr! { Text(expression.attributes, expression.span; text) },
            Binding(binding) => expr! {
                Binding {
                    expression.attributes,
                    expression.span;
                    binder: self.resolve_binding(&binding.binder, scope)?,
                }
            },
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

                expr! {
                    Lambda {
                        expression.attributes,
                        expression.span;
                        parameter: Identifier::new(Index::DeBruijnParameter, lambda.parameter.clone()),
                        parameter_type_annotation: parameter_type_annotation.transpose()?,
                        body_type_annotation: body_type_annotation.transpose()?,
                        body: body?,
                        explicitness: lambda.explicitness,
                        laziness: lambda.laziness,
                    }
                }
            }
            UseIn => {
                Diagnostic::unimplemented("use/in expression")
                    .primary_span(&expression)
                    .report(self.reporter);
                return Err(());
            }
            CaseAnalysis(analysis) => {
                let subject = self.resolve_expression(analysis.subject.clone(), scope)?;
                let mut cases = Vec::new();

                for case in &analysis.cases {
                    let (pattern, binders) = self.resolve_pattern(case.pattern.clone(), scope)?;
                    let body = self.resolve_expression(
                        case.body.clone(),
                        &scope.extend_with_pattern_binders(binders),
                    )?;

                    cases.push(hir::Case { pattern, body });
                }

                expr! {
                    CaseAnalysis {
                        expression.attributes,
                        expression.span;
                        subject,
                        cases,
                    }
                }
            }
            Error => PossiblyErroneous::error(),
        };

        Ok(expression)
    }

    fn resolve_pattern(
        &mut self,
        pattern: lowered_ast::Pattern,
        scope: &FunctionScope<'_>,
    ) -> Result<(hir::Pattern, Vec<ast::Identifier>)> {
        use lowered_ast::PatternKind::*;

        let mut binders = Vec::new();

        let pattern = match pattern.data.clone() {
            Number(number) => pat! { Number(pattern.attributes, pattern.span; number) },
            Text(text) => pat! { Text(pattern.attributes, pattern.span; text) },
            Binding(binding) => pat! {
                Binding {
                    pattern.attributes,
                    pattern.span;
                    binder: self.resolve_binding(&binding.binder, scope)?,
                }
            },
            Binder(binder) => {
                binders.push(binder.binder.clone());
                pat! {
                    Binder {
                        pattern.attributes,
                        pattern.span;
                        binder: Identifier::new(Index::DeBruijnParameter, unrc!(binder.binder)),
                    }
                }
            }
            Deapplication(deapplication) => {
                let callee = self.resolve_pattern(deapplication.callee.clone(), scope);
                let argument = self.resolve_pattern(deapplication.argument.clone(), scope);

                let (callee, mut callee_binders) = callee?;
                let (argument, mut argument_binders) = argument?;

                binders.append(&mut callee_binders);
                binders.append(&mut argument_binders);

                pat! {
                    Deapplication {
                        pattern.attributes,
                        pattern.span;
                        callee,
                        argument,
                    }
                }
            }
            Error => PossiblyErroneous::error(),
        };

        Ok((pattern, binders))
    }

    fn look_up(&self, index: DeclarationIndex) -> &Entity {
        match self.scope.local_index(index) {
            Some(index) => &self.scope[index],
            None => &self.session[index],
        }
    }

    /// Resolve a syntactic path given a namespace.
    fn resolve_path<Target: ResolutionTarget>(
        &self,
        path: &ast::Path,
        namespace: DeclarationIndex,
    ) -> Result<Target::Output, ResolutionError> {
        self.resolve_path_with_origin::<Target>(
            path,
            namespace,
            PathResolutionContext {
                origin_namespace: namespace,
                usage: IdentifierUsage::Unqualified,
                check_exposure: CheckExposure::Yes,
            },
        )
    }

    // @Temporary
    fn resolve_path_unchecked_exposure<Target: ResolutionTarget>(
        &self,
        path: &ast::Path,
        namespace: DeclarationIndex,
    ) -> Result<Target::Output, ResolutionError> {
        self.resolve_path_with_origin::<Target>(
            path,
            namespace,
            PathResolutionContext {
                origin_namespace: namespace,
                usage: IdentifierUsage::Unqualified,
                check_exposure: CheckExposure::No,
            },
        )
    }

    /// Resolve a syntactic path given a namespace with an explicit origin.
    // @Task memoize by (path, namespace)
    fn resolve_path_with_origin<Target: ResolutionTarget>(
        &self,
        path: &ast::Path,
        namespace: DeclarationIndex,
        context: PathResolutionContext,
    ) -> Result<Target::Output, ResolutionError> {
        if let Some(hanger) = &path.hanger {
            use ast::HangerKind::*;

            let namespace = match hanger.data {
                Extern => {
                    let crate_ = match path.segments.first() {
                        Some(crate_) => crate_,
                        None => {
                            // @Task improve the error message, code
                            Diagnostic::error()
                                .message("path `extern` is used in isolation")
                                .primary_span(hanger)
                                .note("the path segment `extern` is only to be used indirectly to refer to specific crates")
                                .report(self.reporter);
                            return Err(ResolutionError::Unrecoverable);
                        }
                    };

                    let crate_ = match self.scope.dependency(crate_.as_str(), self.session) {
                        Some(crate_) => crate_,
                        None => {
                            // @Temporary message
                            // @Task add help:
                            // * if it's a single-file-crate (@Task smh propagate??)
                            //   then suggest `--link`ing
                            // * otherwise suggest adding to `dependencies` section in
                            //   the package manifest (@Note does not scale to dev-deps)
                            // @Task suggest similarly named dep(s)!
                            // @Task check if a dependency has a (transitive) dependency
                            // with the *same* name and add the note that they (trans deps) have to be
                            // explicitly added to the deps list to be referenceable in this crate
                            Diagnostic::error()
                                .message(format!("crate `{crate_}` does not exist"))
                                .primary_span(crate_)
                                .report(self.reporter);
                            return Err(ResolutionError::Unrecoverable);
                        }
                    };

                    let scope = &self.session[crate_];
                    let root = scope.global_index(scope.root());

                    return match &*path.segments {
                        [identifier] => Ok(Target::output(root, identifier)),
                        [_, identifiers @ ..] => self.resolve_path_with_origin::<Target>(
                            &ast::Path::with_segments(identifiers.to_owned().into()),
                            root,
                            PathResolutionContext {
                                usage: IdentifierUsage::Qualified,
                                check_exposure: CheckExposure::Yes,
                                ..context
                            },
                        ),
                        [] => unreachable!(),
                    };
                }
                Crate => self.scope.global_index(self.scope.root()),
                Super => self.scope.global_index(
                    self.resolve_super(hanger, self.scope.local_index(namespace).unwrap())?,
                ),
                Self_ => namespace,
            };

            return if path.segments.is_empty() {
                Target::output_bare_path_hanger(hanger, namespace).map_err(|error| {
                    error.report(self.reporter);
                    ResolutionError::Unrecoverable
                })
            } else {
                self.resolve_path_with_origin::<Target>(
                    &ast::Path::with_segments(path.segments.clone()),
                    namespace,
                    context.qualified_identifier(),
                )
            };
        }

        let index = self.resolve_path_segment(&path.segments[0], namespace, context)?;
        let entity = self.look_up(index);

        match &*path.segments {
            [identifier] => {
                Target::handle_simple_path(identifier, entity).reported(self.reporter)?;
                Ok(Target::output(index, identifier))
            }
            [identifier, identifiers @ ..] => {
                if entity.is_namespace() {
                    self.resolve_path_with_origin::<Target>(
                        &ast::Path::with_segments(identifiers.to_owned().into()),
                        index,
                        context.qualified_identifier(),
                    )
                } else if entity.is_error() {
                    // @Task add rationale
                    Ok(Target::output(index, identifiers.last().unwrap()))
                } else {
                    self.value_used_as_a_namespace(
                        identifier,
                        identifiers.first().unwrap(),
                        namespace,
                    )
                    .report(self.reporter);
                    Err(ResolutionError::Unrecoverable)
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
        self.scope[module].parent.ok_or_else(|| {
            Diagnostic::error()
                .code(Code::E021) // @Question use a dedicated code?
                .message("the crate root does not have a parent")
                .primary_span(hanger)
                .report(self.reporter);
        })
    }

    fn resolve_path_segment(
        &self,
        identifier: &ast::Identifier,
        namespace: DeclarationIndex,
        context: PathResolutionContext,
    ) -> Result<DeclarationIndex, ResolutionError> {
        let index = self
            .look_up(namespace)
            .namespace()
            .unwrap()
            .binders
            .iter()
            .copied()
            .find(|&index| &self.look_up(index).source == identifier)
            .ok_or_else(|| ResolutionError::UnresolvedBinding {
                identifier: identifier.clone(),
                namespace,
                usage: context.usage,
            })?;

        // @Temporary hack until we can manage cyclic exposure reaches!
        if matches!(context.check_exposure, CheckExposure::Yes) {
            self.handle_exposure(index, identifier, context.origin_namespace)?;
        }

        self.collapse_use_chain(index)
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
                self.scope.global_index(definition_site_namespace),
            )?;

            if !self.scope.is_allowed_to_access(
                origin_namespace,
                self.scope.global_index(definition_site_namespace),
                reach,
            ) {
                Diagnostic::error()
                    .code(Code::E029)
                    .message(format!(
                        "binding `{}` is private",
                        self.scope.absolute_path(index, self.session)
                    ))
                    .primary_span(identifier)
                    .report(self.reporter);
                return Err(ResolutionError::Unrecoverable);
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
    ) -> Result<DeclarationIndex, ResolutionError> {
        use EntityKind::*;

        match self.look_up(index).kind {
            Use { reference } => Ok(reference),
            UnresolvedUse => Err(ResolutionError::UnresolvedUseBinding { inquirer: index }),
            _ => Ok(index),
        }
    }

    fn resolve_restricted_exposure(
        &self,
        exposure: &RefCell<RestrictedExposure>,
        definition_site_namespace: DeclarationIndex,
    ) -> Result<DeclarationIndex> {
        let exposure_ = exposure.borrow();

        Ok(match &*exposure_ {
            RestrictedExposure::Unresolved {
                reach: unresolved_reach,
            } => {
                // Here we indeed resolve the exposure reach without validating *its*
                // exposure reach. This is not a problem however since in all cases where
                // it actually is private, it cannot be an ancestor module as those are
                // always accessible to their descendants, and therefore we are going to
                // throw an error.
                // It's not possible to use `resolve_path` as that can lead to infinite
                // loops with out of order use-bindings.
                let reach = self
                    .resolve_path_unchecked_exposure::<OnlyModule>(
                        unresolved_reach,
                        definition_site_namespace,
                    )
                    .map_err(|error| self.report_resolution_error(error))?;

                let reach_is_ancestor = self
                    .scope
                    .some_ancestor_equals(definition_site_namespace, reach);

                if !reach_is_ancestor {
                    Diagnostic::error()
                        .code(Code::E037)
                        .message("exposure can only be restricted to ancestor modules")
                        .primary_span(unresolved_reach)
                        .report(self.reporter);
                    return Err(());
                }

                drop(exposure_);
                *exposure.borrow_mut() = RestrictedExposure::Resolved {
                    // @Beacon @Bug unwrap not verified
                    reach: self.scope.local_index(reach).unwrap(),
                };

                reach
            }
            &RestrictedExposure::Resolved { reach } => self.scope.global_index(reach),
        })
    }

    /// Reobtain the resolved identifier.
    ///
    /// Used in [`Self::finish_resolve_declaration`], the last pass of the
    /// name resolver, to re-gain some information (the [`Identifier`]s) collected
    /// during the first pass.
    ///
    /// This way, [`Self::start_resolve_declaration`] does not need to return
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
        let index = self.scope[namespace]
            .namespace()
            .unwrap()
            .binders
            .iter()
            .map(|&index| self.scope.local_index(index).unwrap())
            .find(|&index| &self.scope[index].source == identifier)
            .unwrap();
        let index = self
            .collapse_use_chain(self.scope.global_index(index))
            .unwrap_or_else(|_| unreachable!());

        Target::output(index, identifier)
    }

    /// Resolve a binding in a function scope.
    fn resolve_binding(&self, query: &ast::Path, scope: &FunctionScope<'_>) -> Result<Identifier> {
        self.resolve_binding_with_depth(query, scope, 0, scope)
    }

    /// Resolve a binding in a function scope given a depth.
    ///
    /// The `depth` is necessary for the recursion to successfully create DeBruijn-indices.
    ///
    /// The `origin` signifies the innermost function scope from where the resolution was first requested.
    /// This information is used for diagnostics, namely typo flagging where we once again start at the origin
    /// and walk back out.
    fn resolve_binding_with_depth<'s: 'a>(
        &self,
        query: &ast::Path,
        scope: &'s FunctionScope<'_>,
        depth: usize,
        origin: &'s FunctionScope<'_>,
    ) -> Result<Identifier> {
        use FunctionScope::*;

        // @Note kinda awkward API with map_err
        match scope {
            &Module(module) => self
                .resolve_path::<OnlyValue>(query, self.scope.global_index(module))
                .map_err(|error| {
                    self.report_resolution_error_searching_lookalikes(error, |identifier, _| {
                        self.find_similarly_named(origin, identifier)
                    });
                }),
            // @Note this looks ugly/complicated, use helper functions
            FunctionParameter { parent, binder } => {
                if let Some(identifier) = query.identifier_head() {
                    if binder == identifier {
                        if query.segments.len() > 1 {
                            self.value_used_as_a_namespace(
                                identifier,
                                &query.segments[1],
                                self.scope.global_index(scope.module()),
                            )
                            .report(self.reporter);
                            return Err(());
                        }

                        Ok(Identifier::new(DeBruijnIndex(depth), identifier.clone()))
                    } else {
                        self.resolve_binding_with_depth(query, parent, depth + 1, origin)
                    }
                } else {
                    self.resolve_path::<OnlyValue>(query, self.scope.global_index(parent.module()))
                        .map_err(|error| self.report_resolution_error(error))
                }
            }
            // @Note this looks ugly/complicated, use helper functions
            PatternBinders { parent, binders } => {
                if let Some(identifier) = query.identifier_head() {
                    match binders
                        .iter()
                        .rev()
                        .zip(depth..)
                        .find(|(binder, _)| binder == &identifier)
                    {
                        Some((_, depth)) => {
                            if query.segments.len() > 1 {
                                self.value_used_as_a_namespace(
                                    identifier,
                                    &query.segments[1],
                                    self.scope.global_index(scope.module()),
                                )
                                .report(self.reporter);
                                return Err(());
                            }

                            Ok(Identifier::new(DeBruijnIndex(depth), identifier.clone()))
                        }
                        None => self.resolve_binding_with_depth(
                            query,
                            parent,
                            depth + binders.len(),
                            origin,
                        ),
                    }
                } else {
                    self.resolve_path::<OnlyValue>(query, self.scope.global_index(parent.module()))
                        .map_err(|error| self.report_resolution_error(error))
                }
            }
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
    // @Task update docs in regards to number of phases
    // @Task update docs regarding errors
    pub(super) fn resolve_use_bindings(&mut self) {
        use ResolutionError::*;

        while !self.scope.partially_resolved_use_bindings.is_empty() {
            let mut partially_resolved_use_bindings = HashMap::default();

            for (&index, item) in &self.scope.partially_resolved_use_bindings {
                match self.resolve_path::<ValueOrModule>(
                    &item.reference,
                    self.scope.global_index(item.module),
                ) {
                    Ok(reference) => {
                        self.scope.bindings[index].kind = EntityKind::Use { reference };
                    }
                    Err(error @ (UnresolvedBinding { .. } | Unrecoverable)) => {
                        self.scope.bindings[index].mark_as_error();
                        self.report_resolution_error(error);
                        self.scope.health.taint();
                    }
                    Err(UnresolvedUseBinding { inquirer }) => {
                        partially_resolved_use_bindings.insert(
                            index,
                            scope::PartiallyResolvedUseBinding {
                                reference: item.reference.clone(),
                                module: item.module,
                                // @Beacon @Bug unwrap not verified
                                inquirer: Some(self.scope.local_index(inquirer).unwrap()),
                            },
                        );
                    }
                }
            }

            // resolution stalled; therefore there are circular bindings
            if partially_resolved_use_bindings.len()
                == self.scope.partially_resolved_use_bindings.len()
            {
                for &index in partially_resolved_use_bindings.keys() {
                    self.scope[index].mark_as_error();
                }

                for cycle in find_cycles(partially_resolved_use_bindings) {
                    let paths = cycle.iter().map(|&index| {
                        self.scope
                            .absolute_path(self.scope.global_index(index), self.session)
                            .quote()
                    });
                    let paths = unordered_listing(paths, Conjunction::And);
                    let spans = cycle.iter().map(|&index| self.scope[index].source.span());

                    Diagnostic::error()
                        .code(Code::E024)
                        .message(pluralize!(
                            cycle.len(),
                            format!("declaration {paths} is circular"),
                            format!("declarations {paths} are circular"),
                        ))
                        .primary_spans(spans)
                        .report(self.reporter);
                }

                self.scope.health.taint();
                break;
            }

            self.scope.partially_resolved_use_bindings = partially_resolved_use_bindings;
        }

        self.scope.partially_resolved_use_bindings.clear();

        type UseBindings = HashMap<LocalDeclarationIndex, scope::PartiallyResolvedUseBinding>;
        type Cycle = HashSet<LocalDeclarationIndex>;

        fn find_cycles(bindings: UseBindings) -> Vec<Cycle> {
            let mut cycles = Vec::new();
            let mut visited = HashMap::default();

            enum Status {
                InProgress,
                Finished,
            }

            for &index in bindings.keys() {
                if let Entry::Vacant(entry) = visited.entry(index) {
                    let mut worklist = vec![index];
                    entry.insert(Status::InProgress);
                    cycles.extend(find_cycle(&bindings, &mut worklist, &mut visited));
                }
            }

            fn find_cycle(
                bindings: &UseBindings,
                worklist: &mut Vec<LocalDeclarationIndex>,
                visited: &mut HashMap<LocalDeclarationIndex, Status>,
            ) -> Option<Cycle> {
                let target = bindings[worklist.last().unwrap()].inquirer.unwrap();

                let cycle = match visited.get(&target) {
                    Some(Status::InProgress) => Some(
                        worklist
                            .iter()
                            .copied()
                            .skip_while(|&vertex| vertex != target)
                            .collect(),
                    ),
                    Some(Status::Finished) => None,
                    None => {
                        worklist.push(target);
                        visited.insert(target, Status::InProgress);
                        find_cycle(bindings, worklist, visited)
                    }
                };
                visited.insert(worklist.pop().unwrap(), Status::Finished);

                cycle
            }

            cycles
        }
    }

    fn resolve_exposure_reaches(&mut self) -> Result {
        let mut health = Health::Untainted;

        for (index, entity) in self.scope.bindings.iter() {
            if let Exposure::Restricted(exposure) = &entity.exposure {
                // unwrap: root always has Exposure::Unrestricted
                let definition_site_namespace = self.scope.global_index(entity.parent.unwrap());

                if self
                    .resolve_restricted_exposure(exposure, definition_site_namespace)
                    .is_err()
                {
                    health.taint();
                    continue;
                };
            }

            if let EntityKind::Use {
                reference: reference_index,
            } = entity.kind
            {
                let reference = self.look_up(reference_index);

                if entity.exposure.compare(&reference.exposure, self.scope)
                    == Some(Ordering::Greater)
                {
                    Diagnostic::error()
                        .code(Code::E009)
                        .message(format!(
                            "re-export of the more private binding `{}`",
                            self.scope.absolute_path(reference_index, self.session)
                        ))
                        .labeled_primary_span(
                            &entity.source,
                            "re-exporting binding with greater exposure",
                        )
                        .labeled_secondary_span(
                            &reference.source,
                            "re-exported binding with lower exposure",
                        )
                        .note(format!(
                            "\
expected the exposure of `{}`
           to be at most {}
      but it actually is {}",
                            self.scope
                                .absolute_path(self.scope.global_index(index), self.session),
                            reference.exposure.with((self.scope, self.session)),
                            entity.exposure.with((self.scope, self.session)),
                        ))
                        .report(self.reporter);
                    health.taint();
                }
            }
        }

        health.into()
    }

    /// Find a similarly named binding in the same namespace.
    ///
    /// Used for error reporting when an undefined binding was encountered.
    /// In the future, we might decide to find not one but several similar names
    /// but that would be computationally heavier.
    fn find_similarly_named_declaration(
        &self,
        queried_identifier: &str,
        predicate: impl Fn(&Entity) -> bool,
        namespace: DeclarationIndex,
    ) -> Option<&str> {
        self.look_up(namespace)
            .namespace()
            .unwrap()
            .binders
            .iter()
            .map(|&index| self.look_up(index))
            .filter(|entity| !entity.is_error() && predicate(entity))
            .map(|entity| entity.source.as_str())
            .find(|identifier| scope::is_similar(identifier, queried_identifier))
    }

    /// Find a similarly named binding in the scope.
    ///
    /// With "scope", it is meant to include parent scopes, too.
    ///
    /// Used for error reporting when an undefined binding was encountered.
    /// In the future, we might decide to find not one but several similar names
    /// but that would be computationally heavier and we would need to be careful
    /// and consider the effects of shadowing.
    // @Task make lifetimes less restrictive: &'s <'s> => &'s <'z>
    pub(super) fn find_similarly_named<'s>(
        &'s self,
        scope: &'s FunctionScope<'s>,
        identifier: &str,
    ) -> Option<&'s str> {
        use FunctionScope::*;

        match scope {
            &Module(module) => self.find_similarly_named_declaration(
                identifier,
                |_| true,
                self.scope.global_index(module),
            ),
            FunctionParameter { parent, binder } => {
                if scope::is_similar(identifier, binder.as_str()) {
                    Some(binder.as_str())
                } else {
                    self.find_similarly_named(parent, identifier)
                }
            }
            PatternBinders { parent, binders } => {
                if let Some(binder) = binders
                    .iter()
                    .rev()
                    .find(|binder| scope::is_similar(identifier, binder.as_str()))
                {
                    Some(binder.as_str())
                } else {
                    self.find_similarly_named(parent, identifier)
                }
            }
        }
    }

    fn report_resolution_error(&self, error: ResolutionError) {
        self.report_resolution_error_searching_lookalikes(error, |identifier, namespace| {
            self.find_similarly_named_declaration(identifier, |_| true, namespace)
        });
    }

    fn report_resolution_error_searching_lookalikes(
        &self,
        error: ResolutionError,
        lookalike_finder: impl FnOnce(&str, DeclarationIndex) -> Option<&'a str>,
    ) {
        match error {
            ResolutionError::Unrecoverable => {}
            ResolutionError::UnresolvedBinding {
                identifier,
                namespace,
                usage,
            } => {
                let mut message = format!("binding `{identifier}` is not defined in ");

                match usage {
                    IdentifierUsage::Unqualified => message += "this scope",
                    IdentifierUsage::Qualified => {
                        message += match self.look_up(namespace).is_module() {
                            true => "module",
                            false => "namespace",
                        };
                        message += " `";
                        message += &self.scope.absolute_path(namespace, self.session);
                        message += "`";
                    }
                }

                Diagnostic::error()
                    .code(Code::E021)
                    .message(message)
                    .primary_span(&identifier)
                    .if_present(
                        lookalike_finder(identifier.as_str(), namespace),
                        |diagnostic, binding| {
                            diagnostic.help(format!(
                                "a binding with a similar name exists in scope: {}",
                                scope::Lookalike {
                                    actual: identifier.as_str(),
                                    lookalike: binding
                                },
                            ))
                        },
                    )
                    .report(self.reporter);
            }
            ResolutionError::UnresolvedUseBinding { .. } => unreachable!(),
        }
    }

    // @Question parent: *Local*DeclarationIndex?
    fn value_used_as_a_namespace(
        &self,
        non_namespace: &ast::Identifier,
        subbinder: &ast::Identifier,
        parent: DeclarationIndex,
    ) -> Diagnostic {
        // @Question should we also include lookalike namespaces that don't contain the
        // subbinding (displaying them in a separate help message?)?
        // @Beacon @Bug this ignores shadowing @Task define this method for FunctionScope
        // @Update @Note actually in the future (lang spec) this kind of shadowing does not
        // occur, the subbinder access pierces through parameters
        let similarly_named_namespace = self.find_similarly_named_declaration(
            non_namespace.as_str(),
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
            .code(Code::E022)
            .message(format!("value `{non_namespace}` is not a namespace"))
            .labeled_primary_span(non_namespace, "not a namespace, just a value")
            .labeled_secondary_span(
                subbinder,
                "requires the preceeding path segment to refer to a namespace",
            )
            .if_present(
                similarly_named_namespace,
                |this, lookalike| {
                    this
                        .help(format!(
                            "a namespace with a similar name containing the binding exists in scope:\n    {}",
                            scope::Lookalike { actual: non_namespace.as_str(), lookalike },
                        ))
                }
            )
            .if_(show_very_general_help, |this| {
                this
                    .note("identifiers following a `.` refer to bindings defined in a namespace (i.e. a module or a data type)")
                    // no type information here yet to check if the non-namespace is indeed a record
                    .help("use `::` to reference a field of a record")
            })
    }
}

/// Additional context for name resolution.
// @Task split context for start|finish resolve second does not store info about opacity
#[derive(Default, Debug)]
struct Context {
    // @Note if we were to rewrite/refactor the lowered AST to use indices for nested
    // declarations, then we could simply loop up the AST node and wouldn't need
    // `Opacity`
    parent_data_binding: Option<(LocalDeclarationIndex, Option<Opacity>)>,
}

#[derive(Debug)]
enum Opacity {
    Transparent,
    Opaque,
}

// @Temporary location
#[derive(Clone, Copy)]
struct PathResolutionContext {
    origin_namespace: DeclarationIndex,
    usage: IdentifierUsage,
    check_exposure: CheckExposure,
}

impl PathResolutionContext {
    fn qualified_identifier(self) -> Self {
        Self {
            usage: IdentifierUsage::Qualified,
            ..self
        }
    }
}

/// If an identifier is used unqualified or qualified.
///
/// Exclusively used for error reporting.
#[derive(Debug)] // @Temporary
#[derive(Clone, Copy)]
enum IdentifierUsage {
    Qualified,
    Unqualified,
}

// @Temporary
#[derive(Clone, Copy)]
enum CheckExposure {
    Yes,
    No,
}

// @Task put all that stuff into a module `resolution_target`
/// Specifies behavior for resolving different sorts of entities.
///
/// Right now, it's only about the difference between values and modules
/// since modules are not values (non-first-class). As such, this trait
/// allows to implementors to define what should happen with the resolved entity
/// if it appears in a specific location
// @Task get rid of this thing! smh!
trait ResolutionTarget {
    type Output;

    fn output(index: DeclarationIndex, identifier: &ast::Identifier) -> Self::Output;

    fn output_bare_path_hanger(
        hanger: &ast::Hanger,
        index: DeclarationIndex,
    ) -> Result<Self::Output, Diagnostic>;

    fn handle_simple_path(identifier: &ast::Identifier, entity: &Entity) -> Result<(), Diagnostic>;
}

/// Marker to specify it's okay to resolve to either value or module.
enum ValueOrModule {}

impl ResolutionTarget for ValueOrModule {
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

    fn handle_simple_path(_: &ast::Identifier, _: &Entity) -> Result<(), Diagnostic> {
        Ok(())
    }
}

/// Marker to specify to only resolve to values.
enum OnlyValue {}

impl ResolutionTarget for OnlyValue {
    type Output = Identifier;

    fn output(index: DeclarationIndex, identifier: &ast::Identifier) -> Self::Output {
        Identifier::new(index, identifier.clone())
    }

    fn output_bare_path_hanger(
        hanger: &ast::Hanger,
        _: DeclarationIndex,
    ) -> Result<Self::Output, Diagnostic> {
        Err(scope::module_used_as_a_value(hanger.as_ref()))
    }

    fn handle_simple_path(identifier: &ast::Identifier, entity: &Entity) -> Result<(), Diagnostic> {
        if entity.is_module() {
            return Err(scope::module_used_as_a_value(identifier.as_spanned_str()));
        }

        Ok(())
    }
}

enum OnlyModule {}

impl ResolutionTarget for OnlyModule {
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

    fn handle_simple_path(identifier: &ast::Identifier, entity: &Entity) -> Result<(), Diagnostic> {
        // @Task print absolute path!
        if !entity.is_module() {
            return Err(Diagnostic::error()
                // @Task custom code
                .code(Code::E022)
                .message(format!("value `{}` is not a module", identifier))
                .primary_span(identifier));
        }

        Ok(())
    }
}

/// A possibly recoverable error that cab emerge during resolution.
enum ResolutionError {
    Unrecoverable,
    UnresolvedBinding {
        identifier: ast::Identifier,
        namespace: DeclarationIndex,
        usage: IdentifierUsage,
    },
    UnresolvedUseBinding {
        inquirer: DeclarationIndex,
    },
}

impl ResolutionError {}

impl From<()> for ResolutionError {
    fn from((): ()) -> Self {
        Self::Unrecoverable
    }
}
