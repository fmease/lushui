//! The name resolver.
//!
//! It traverses the [lowered AST](lowered_ast) and registers/defines bindings
//! defined both at module-level using declarations and at function
//! and pattern level as parameters. Furthermore, it resolves all paths inside
//! expressions and patterns to [(resolved) identifiers](Identifier) which
//! contain a [declaration index](DeclarationIndex) or a [de Bruijn index](DeBruijnIndex)
//! respectively.

use crate::{
    diagnostics::{Code, Diagnostic, Reporter},
    entity::{Entity, EntityKind},
    error::{Health, PossiblyErroneous, ReportedExt, Result},
    format::{pluralize, unordered_listing, Conjunction, DisplayWith, QuoteExt},
    hir::{
        self, decl, expr, pat, DeBruijnIndex, DeclarationIndex, Identifier, Index,
        LocalDeclarationIndex,
    },
    package::BuildSession,
    span::Spanning,
    syntax::{
        ast::{self, Path},
        lowered_ast::{self, AttributeName},
        CapsuleName,
    },
    utility::{HashMap, HashSet},
};
pub(crate) use scope::{Capsule, Exposure, FunctionScope, Namespace};
use scope::{RegistrationError, RestrictedExposure};
use std::{cmp::Ordering, collections::hash_map::Entry, fmt, sync::Mutex};

mod scope;

pub const PROGRAM_ENTRY_IDENTIFIER: &str = "main";

/// Resolve the names of a declaration.
///
/// It performs four passes to resolve all possible out of order declarations.
///
/// # Panics
///
/// If the declaration passed is not a module, this function will panic as it
/// requires a capsule root which is defined through the root module.
// @Task improve docs: mention that it not only looks things up but also defines bindings
// and that `resolve` should only be called once per Capsule (bc it would fail anyways the 2nd
// time (to re-define root (I think)))
pub fn resolve_declarations(
    capsule_root: lowered_ast::Declaration,
    capsule: &mut Capsule,
    session: &BuildSession,
    reporter: &Reporter,
) -> Result<hir::Declaration> {
    let mut resolver = ResolverMut::new(capsule, session, reporter);

    resolver
        .start_resolve_declaration(&capsule_root, None, Context::default())
        .map_err(|_| {
            std::mem::take(&mut resolver.capsule.duplicate_definitions)
                .into_values()
                .for_each(|error| Diagnostic::from(error).report(resolver.reporter));
        })?;

    resolver.resolve_use_bindings();

    // @Task @Beacon don't return early here
    resolver.resolve_exposure_reaches()?;

    let declaration = resolver.finish_resolve_declaration(capsule_root, None, Context::default());

    Result::from(resolver.health).and(declaration)
}

// @Task docs: mention that the current capsule should be pre-populated before calling this
// (using resolver::resolve)
pub(crate) fn resolve_path(
    path: &ast::Path,
    namespace: DeclarationIndex,
    capsule: &Capsule,
    session: &BuildSession,
    reporter: &Reporter,
) -> Result<DeclarationIndex> {
    let resolver = Resolver::new(capsule, session, reporter);

    resolver
        .resolve_path::<target::Any>(path, namespace)
        .map_err(|error| resolver.report_resolution_error(error))
}

// @Question can we merge Resolver and ResolverMut if we introduce a separate
// lifetime for Resolver.capsule?
struct ResolverMut<'a> {
    capsule: &'a mut Capsule,
    session: &'a BuildSession,
    reporter: &'a Reporter,
    /// For resolving out of order use-declarations.
    partially_resolved_use_bindings: HashMap<LocalDeclarationIndex, PartiallyResolvedUseBinding>,
    health: Health,
}

impl<'a> ResolverMut<'a> {
    fn new(capsule: &'a mut Capsule, session: &'a BuildSession, reporter: &'a Reporter) -> Self {
        Self {
            capsule,
            session,
            reporter,
            partially_resolved_use_bindings: HashMap::default(),
            health: Health::Untainted,
        }
    }

    fn as_ref(&self) -> Resolver<'_> {
        Resolver {
            capsule: self.capsule,
            session: self.session,
            reporter: self.reporter,
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
    ) -> Result<(), RegistrationError> {
        use lowered_ast::DeclarationKind::*;

        let exposure = match declaration.attributes.get::<{ AttributeName::Public }>() {
            Some(public) => match &public.reach {
                Some(reach) => RestrictedExposure::Unresolved {
                    reach: reach.clone(),
                }
                .into(),
                None => Exposure::Unrestricted,
            },
            None => match module {
                // a lack of `@public` means private i.e. restricted to `self` i.e. `@(public self)`
                Some(module) => RestrictedExposure::Resolved { reach: module }.into(),
                None => Exposure::Unrestricted,
            },
        };

        match &declaration.value {
            Function(function) => {
                let module = module.unwrap();

                self.capsule.register_binding(
                    function.binder.clone(),
                    exposure,
                    declaration.attributes.clone(),
                    EntityKind::UntypedFunction,
                    Some(module),
                )?;
            }
            Data(type_) => {
                // there is always a root module
                let module = module.unwrap();

                // @Task don't return early, see analoguous code for modules
                let namespace = self.capsule.register_binding(
                    type_.binder.clone(),
                    exposure,
                    declaration.attributes.clone(),
                    EntityKind::untyped_data_type(),
                    Some(module),
                )?;

                if let Some(constructors) = &type_.constructors {
                    // @Task awkward API: Result <-> Result<(), RegistrationError>
                    let health =
                        constructors
                            .iter()
                            .fold(Health::Untainted, |health, constructor| {
                                let transparency =
                                    if declaration.attributes.contains(AttributeName::Abstract) {
                                        Transparency::Abstract
                                    } else {
                                        Transparency::Transparent
                                    };

                                health
                                    & self
                                        .start_resolve_declaration(
                                            constructor,
                                            Some(module),
                                            Context {
                                                parent_data_binding: Some((
                                                    namespace,
                                                    Some(transparency),
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
                let (namespace, module_transparency) = context.parent_data_binding.unwrap();

                let exposure = match module_transparency.unwrap() {
                    Transparency::Transparent => self.capsule[namespace].exposure.clone(),
                    // as if a @(public super) was attached to the constructor
                    Transparency::Abstract => RestrictedExposure::Resolved { reach: module }.into(),
                };

                self.capsule.register_binding(
                    constructor.binder.clone(),
                    exposure,
                    declaration.attributes.clone(),
                    EntityKind::UntypedConstructor,
                    Some(namespace),
                )?;
            }
            Module(submodule) => {
                // @Task @Beacon don't return early on error
                // @Note you need to create a fake index for this (an index which points to
                // a fake, nameless binding)
                let index = self.capsule.register_binding(
                    submodule.binder.clone(),
                    exposure,
                    // @Beacon @Bug this does not account for attributes found on the attribute header!
                    declaration.attributes.clone(),
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

                let index = self.capsule.register_binding(
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
                            reference: use_.target.clone(),
                            module,
                            inquirer: None,
                        },
                    );

                    debug_assert!(previous.is_none());
                };
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

        Ok(match declaration.value {
            Function(function) => {
                let module = module.unwrap();

                let type_annotation = self
                    .as_ref()
                    .resolve_expression(function.type_annotation, &FunctionScope::Module(module));

                let expression = function
                    .expression
                    .map(|expression| {
                        self.as_ref()
                            .resolve_expression(expression, &FunctionScope::Module(module))
                    })
                    .transpose();

                let binder = self
                    .as_ref()
                    .reobtain_resolved_identifier::<target::Value>(&function.binder, module);

                if module == self.capsule.local_root()
                    && self.capsule.is_executable()
                    && function.binder.as_str() == PROGRAM_ENTRY_IDENTIFIER
                {
                    self.capsule.program_entry = Some(binder.clone());
                }

                decl! {
                    Function {
                        declaration.attributes,
                        declaration.span;
                        type_annotation: type_annotation?,
                        expression: expression?,
                        binder,
                    }
                }
            }
            Data(type_) => {
                let module = module.unwrap();

                let type_annotation = self
                    .as_ref()
                    .resolve_expression(type_.type_annotation, &FunctionScope::Module(module));

                // @Beacon @Question wouldn't it be great if that method returned a
                // LocalDeclarationIndex instead of an Identifier?
                // or maybe even a *LocalIdentifier?
                let binder = self
                    .as_ref()
                    .reobtain_resolved_identifier::<target::Value>(&type_.binder, module);

                let constructors = type_.constructors.map(|constructors| {
                    constructors
                        .into_iter()
                        .map(|constructor| {
                            self.finish_resolve_declaration(
                                constructor,
                                Some(module),
                                Context {
                                    parent_data_binding: Some((
                                        binder.local_declaration_index(self.capsule).unwrap(),
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

                let type_annotation = self.as_ref().resolve_expression(
                    constructor.type_annotation,
                    &FunctionScope::Module(module),
                )?;

                let binder = self.as_ref().reobtain_resolved_identifier::<target::Value>(
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
                    Some(module) => self
                        .as_ref()
                        .reobtain_resolved_identifier::<target::Module>(&submodule.binder, module)
                        .local(self.capsule)
                        .unwrap(),
                    None => self.capsule.local_root(),
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
                        binder: Identifier::new(index.global(self.capsule), submodule.binder),
                        file: submodule.file,
                        declarations,
                    }
                }
            }
            Use(use_) => {
                let module = module.unwrap();

                let binder = Identifier::new(
                    self.as_ref()
                        .reobtain_resolved_identifier::<target::Any>(&use_.binder, module),
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
    pub(super) fn resolve_use_bindings(&mut self) {
        use ResolutionError::*;

        while !self.partially_resolved_use_bindings.is_empty() {
            let mut partially_resolved_use_bindings = HashMap::default();

            for (&index, item) in &self.partially_resolved_use_bindings {
                match self
                    .as_ref()
                    .resolve_path::<target::Any>(&item.reference, item.module.global(self.capsule))
                {
                    Ok(reference) => {
                        self.capsule.bindings[index].kind = EntityKind::Use { reference };
                    }
                    Err(error @ (UnresolvedBinding { .. } | Unrecoverable)) => {
                        self.capsule.bindings[index].mark_as_error();
                        self.as_ref().report_resolution_error(error);
                        self.health.taint();
                    }
                    Err(UnresolvedUseBinding { inquirer }) => {
                        partially_resolved_use_bindings.insert(
                            index,
                            PartiallyResolvedUseBinding {
                                reference: item.reference.clone(),
                                module: item.module,
                                // @Beacon @Bug unwrap not verified
                                inquirer: Some(inquirer.local(self.capsule).unwrap()),
                            },
                        );
                    }
                }
            }

            // resolution stalled; therefore there are circular bindings
            if partially_resolved_use_bindings.len() == self.partially_resolved_use_bindings.len() {
                for &index in partially_resolved_use_bindings.keys() {
                    self.capsule[index].mark_as_error();
                }

                for cycle in find_cycles(partially_resolved_use_bindings) {
                    let paths = cycle.iter().map(|&index| {
                        self.capsule
                            .absolute_path_to_string(index.global(self.capsule), self.session)
                            .quote()
                    });
                    let paths = unordered_listing(paths, Conjunction::And);
                    let spans = cycle.iter().map(|&index| self.capsule[index].source.span());

                    Diagnostic::error()
                        .code(Code::E024)
                        .message(pluralize!(
                            cycle.len(),
                            format!("the declaration {paths} is circular"),
                            format!("the declarations {paths} are circular"),
                        ))
                        .primary_spans(spans)
                        .report(self.reporter);
                }

                self.health.taint();
                break;
            }

            self.partially_resolved_use_bindings = partially_resolved_use_bindings;
        }

        self.partially_resolved_use_bindings.clear();

        type UseBindings = HashMap<LocalDeclarationIndex, PartiallyResolvedUseBinding>;
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

        for (index, entity) in self.capsule.bindings.iter() {
            if let Exposure::Restricted(exposure) = &entity.exposure {
                // unwrap: root always has Exposure::Unrestricted, it won't reach this branch
                let definition_site_namespace = entity.parent.unwrap().global(self.capsule);

                if self
                    .as_ref()
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
                let reference = self.as_ref().look_up(reference_index);

                if entity.exposure.compare(&reference.exposure, self.capsule)
                    == Some(Ordering::Greater)
                {
                    Diagnostic::error()
                        .code(Code::E009)
                        .message(format!(
                            "re-export of the more private binding `{}`",
                            self.capsule
                                .absolute_path_to_string(reference_index, self.session)
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
                            self.capsule
                                .absolute_path_to_string(index.global(self.capsule), self.session),
                            reference.exposure.with((self.capsule, self.session)),
                            entity.exposure.with((self.capsule, self.session)),
                        ))
                        .report(self.reporter);
                    health.taint();
                }
            }
        }

        health.into()
    }
}

struct Resolver<'a> {
    capsule: &'a Capsule,
    session: &'a BuildSession,
    reporter: &'a Reporter,
}

impl<'a> Resolver<'a> {
    fn new(capsule: &'a Capsule, session: &'a BuildSession, reporter: &'a Reporter) -> Self {
        Self {
            capsule,
            session,
            reporter,
        }
    }

    // @Task @Beacon use Rc::try_unwrap more instead of clone
    fn resolve_expression(
        &self,
        expression: lowered_ast::Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<hir::Expression> {
        use lowered_ast::ExpressionKind::*;

        let expression = match expression.value {
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
                        laziness: pi.laziness,
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
            NumberLiteral(number) => {
                todo!()

                // expr! { Number(expression.attributes, expression.span; number) }
            }
            TextLiteral(text) => {
                todo!()

                // expr! { Text(expression.attributes, expression.span; text) }
            }
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
        &self,
        pattern: lowered_ast::Pattern,
        scope: &FunctionScope<'_>,
    ) -> Result<(hir::Pattern, Vec<ast::Identifier>)> {
        use lowered_ast::PatternKind::*;

        let mut binders = Vec::new();

        let pattern = match pattern.value.clone() {
            NumberLiteral(number) => {
                todo!()

                // pat! { Number(pattern.attributes, pattern.span; number) }
            }
            TextLiteral(text) => {
                todo!()

                //  pat! { Text(pattern.attributes, pattern.span; text) }
            }
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
                        binder: Identifier::new(Index::DeBruijnParameter, binder.binder),
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

    fn look_up(&self, index: DeclarationIndex) -> &'a Entity {
        match index.local(self.capsule) {
            Some(index) => &self.capsule[index],
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

            let namespace = match hanger.value {
                Extern => {
                    let Some(capsule) = path.segments.first() else {
                        // @Task improve the error message, code
                        Diagnostic::error()
                            .message("path `extern` is used in isolation")
                            .primary_span(hanger)
                            .note("the path segment `extern` is only to be used indirectly to refer to specific capsule")
                            .report(self.reporter);
                        return Err(ResolutionError::Unrecoverable);
                    };

                    // @Beacon @Task add test for error case
                    let capsule = CapsuleName::from_identifier(capsule.clone())
                        .map_err(|error| error.primary_span(capsule).report(self.reporter))?;

                    let Some(capsule) = self.capsule.dependency(&capsule.value, self.session) else {
                        // @Temporary message
                        // @Task add help:
                        // * if it's a single-file package (@Task smh propagate??)
                        //   then suggest `--link`ing
                        // * otherwise suggest adding to `dependencies` section in
                        //   the package manifest (@Note does not scale to dev-deps)
                        // @Task suggest similarly named dep(s)!
                        // @Task check if a dependency has a (transitive) dependency
                        // with the *same* name and add the note that they (trans deps) have to be
                        // explicitly added to the deps list to be referenceable in this capsule
                        Diagnostic::error()
                            .message(format!("capsule `{capsule}` does not exist"))
                            .primary_span(capsule)
                            .report(self.reporter);
                        return Err(ResolutionError::Unrecoverable);
                    };

                    let capsule = &self.session[capsule];
                    let root = capsule.root();

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
                Capsule => self.capsule.root(),
                Super => self
                    .resolve_super(hanger, namespace.local(self.capsule).unwrap())?
                    .global(self.capsule),
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
        self.capsule[module].parent.ok_or_else(|| {
            Diagnostic::error()
                .code(Code::E021) // @Question use a dedicated code?
                .message("the capsule root does not have a parent")
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

        if let Some(deprecated) = self
            .look_up(index)
            .attributes
            .get::<{ AttributeName::Deprecated }>()
        {
            let mut message = format!(
                "use of deprecated binding `{}`",
                self.capsule.absolute_path_to_string(index, self.session),
            );

            if let Some(reason) = &deprecated.reason {
                message += ": ";
                message += reason;
            }

            Diagnostic::warning()
                .message(message)
                .primary_span(identifier)
                .report(self.reporter);
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
                definition_site_namespace.global(self.capsule),
            )?;

            if !self.capsule.is_allowed_to_access(
                origin_namespace,
                definition_site_namespace.global(self.capsule),
                reach,
            ) {
                Diagnostic::error()
                    .code(Code::E029)
                    .message(format!(
                        "binding `{}` is private",
                        self.capsule.absolute_path_to_string(index, self.session)
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
        exposure: &Mutex<RestrictedExposure>,
        definition_site_namespace: DeclarationIndex,
    ) -> Result<DeclarationIndex> {
        let exposure_ = exposure.lock().unwrap();

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
                    .resolve_path_unchecked_exposure::<target::Module>(
                        unresolved_reach,
                        definition_site_namespace,
                    )
                    .map_err(|error| self.report_resolution_error(error))?;

                let reach_is_ancestor = self
                    .capsule
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
                *exposure.lock().unwrap() = RestrictedExposure::Resolved {
                    // @Beacon @Bug unwrap not verified
                    reach: reach.local(self.capsule).unwrap(),
                };

                reach
            }
            &RestrictedExposure::Resolved { reach } => reach.global(self.capsule),
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
        let index = self.capsule[namespace]
            .namespace()
            .unwrap()
            .binders
            .iter()
            .map(|index| index.local(self.capsule).unwrap())
            .find(|&index| &self.capsule[index].source == identifier)
            .unwrap();
        let index = self
            .collapse_use_chain(index.global(self.capsule))
            .unwrap_or_else(|_| unreachable!());

        Target::output(index, identifier)
    }

    /// Resolve a binding in a function scope.
    fn resolve_binding(&self, query: &ast::Path, scope: &FunctionScope<'_>) -> Result<Identifier> {
        self.resolve_binding_with_depth(query, scope, 0, scope)
    }

    /// Resolve a binding in a function scope given a depth.
    ///
    /// The `depth` is necessary for the recursion to successfully create de Bruijn indices.
    ///
    /// The `origin` signifies the innermost function scope from where the resolution was first requested.
    /// This information is used for diagnostics, namely typo flagging where we once again start at the origin
    /// and walk back out.
    fn resolve_binding_with_depth(
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
                        self.resolve_binding_with_depth(query, parent, depth + 1, origin)
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
                        None => self.resolve_binding_with_depth(
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
            self.resolve_path::<target::Value>(query, scope.module().global(self.capsule))
                .map_err(|error| {
                    self.report_resolution_error_searching_lookalikes(error, |identifier, _| {
                        self.find_similarly_named(origin, identifier)
                    });
                })
        }
    }

    /// Find a similarly named binding in the same namespace.
    ///
    /// Used for error reporting when an undefined binding was encountered.
    /// In the future, we might decide to find not one but several similar names
    /// but that would be computationally heavier.
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
            .find(|some_identifier| scope::is_similar(some_identifier, identifier))
    }

    /// Find a similarly named binding in the scope.
    ///
    /// With "scope", it is meant to include parent scopes, too.
    ///
    /// Used for error reporting when an undefined binding was encountered.
    /// In the future, we might decide to find not one but several similar names
    /// but that would be computationally heavier and we would need to be careful
    /// and consider the effects of shadowing.
    pub(super) fn find_similarly_named<'s>(
        &'s self,
        scope: &'s FunctionScope<'_>,
        identifier: &str,
    ) -> Option<&'s str> {
        use FunctionScope::*;

        match scope {
            &Module(module) => self.find_similarly_named_declaration(
                identifier,
                |_| true,
                module.global(self.capsule),
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

    fn report_resolution_error_searching_lookalikes<'s>(
        &self,
        error: ResolutionError,
        lookalike_finder: impl FnOnce(&str, DeclarationIndex) -> Option<&'s str>,
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
                        if namespace == self.capsule.root() {
                            message += "the capsule root";
                        } else {
                            message += match self.look_up(namespace).is_module() {
                                true => "module",
                                false => "namespace",
                            };
                            message += " `";
                            message += &self
                                .capsule
                                .absolute_path_to_string(namespace, self.session);
                            message += "`";
                        }
                    }
                }

                Diagnostic::error()
                    .code(Code::E021)
                    .message(message)
                    .primary_span(&identifier)
                    .if_present(
                        lookalike_finder(identifier.as_str(), namespace),
                        |this, lookalike| {
                            this.help(format!(
                                "a binding with a similar name exists in scope: {}",
                                scope::Lookalike {
                                    actual: identifier.as_str(),
                                    lookalike,
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
    parent_data_binding: Option<(LocalDeclarationIndex, Option<Transparency>)>,
}

#[derive(Debug)]
enum Transparency {
    Transparent,
    Abstract,
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

struct PartiallyResolvedUseBinding {
    reference: Path,
    module: LocalDeclarationIndex,
    // @Beacon @Question local or global??
    inquirer: Option<LocalDeclarationIndex>,
}

impl fmt::Debug for PartiallyResolvedUseBinding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}.{}", self.module, self.reference)?;

        if let Some(inquirer) = self.inquirer {
            write!(f, " (inquired by {inquirer:?})")?;
        }

        Ok(())
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

        fn handle_simple_path(_: &ast::Identifier, _: &Entity) -> Result<(), Diagnostic> {
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
            Err(Diagnostic::module_used_as_a_value(hanger.as_ref()))
        }

        fn handle_simple_path(
            identifier: &ast::Identifier,
            entity: &Entity,
        ) -> Result<(), Diagnostic> {
            if entity.is_module() {
                return Err(Diagnostic::module_used_as_a_value(
                    identifier.as_spanned_str(),
                ));
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

        fn handle_simple_path(
            identifier: &ast::Identifier,
            entity: &Entity,
        ) -> Result<(), Diagnostic> {
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
