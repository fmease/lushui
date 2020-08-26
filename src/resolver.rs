//! The name resolver.
//!
//! It traverses the desugared AST (aka desugar-HIR) and registers bindings
//! defined both at module-level using declarations and at function
//! and pattern level as parameters. Furthermore, it resolves all paths inside
//! expressions and patterns to resolver identifiers [Identifier] which
//! either links to a crate-local index [CrateIndex] or a Debruijn-index [DebruijnIndex]
//! respectively.
//!
//! It does two main passes and a (hopefully) small one for use declarations to support
//! out of order declarations.
//!
//! ## Future Features
//!
//! * handle module privacy (notably restricted exposure and good error messages)
//! * handle crate declarations

use crate::{
    ast::{self, Path},
    desugar::Desugared,
    diagnostic::{Code, Diagnostic, Diagnostics, Result, Results},
    entity::{Entity, EntityKind},
    hir::{self, decl, expr, Declaration, Expression, Pass},
    span::{Span, Spanning},
    support::{
        accumulate_errors::*, release, DebugIsDisplay, InvalidFallback, ManyErrExt, TransposeExt,
        TrySoftly,
    },
    typer::interpreter::{ffi, scope::Registration},
};
use indexed_vec::IndexVec;
use joinery::JoinableIterator;
use std::{collections::HashMap, default::default, rc::Rc};

const PROGRAM_ENTRY_IDENTIFIER: &str = "main";

type Bindings = IndexVec<CrateIndex, Entity>;

/// The crate scope for module-level bindings.
///
/// This structure is used not only by the name resolver but also the type checker.
#[derive(Default)]
pub struct CrateScope {
    pub(crate) program_entry: Option<Identifier>,
    /// All bindings inside of a crate.
    ///
    /// The first element will always be the root module.
    pub(crate) bindings: Bindings,
    /// For resolving out of order use declarations.
    unresolved_uses: HashMap<CrateIndex, UnresolvedUse>,
    // @Note ugly types!
    pub foreign_types: HashMap<&'static str, Option<Identifier>>,
    pub foreign_bindings: HashMap<&'static str, (usize, ffi::ForeignFunction)>,
    pub inherent_values: ffi::InherentValueMap,
    pub inherent_types: ffi::InherentTypeMap,
    // @Note this is very coarse-grained: as soon as we cannot resolve EITHER type annotation (for example)
    // OR actual value(s), we bail out and add this here. This might be too conversative (leading to more
    // "circular type" errors or whatever), we can just discriminate by creating sth like
    // UnresolvedThingy/WorlistItem { index: CrateIndex, expression: TypeAnnotation|Value|Both|... }
    pub out_of_order_bindings: Vec<Registration>,
}

impl CrateScope {
    /// The crate root.
    ///
    /// Unbeknownst to the subdeclarations of the crate, it takes the name
    /// given by external sources, the crate name. Inside the crate, the root
    /// can only ever be referenced through the keyword `crate` (unless renamed).
    fn root(&self) -> CrateIndex {
        CrateIndex(0)
    }

    fn unwrap_namespace_scope(&self, index: CrateIndex) -> &Namespace {
        match &self.bindings[index].kind {
            EntityKind::Module(scope) | EntityKind::UntypedDataType(scope) => scope,
            _ => unreachable!(),
        }
    }

    fn unwrap_namespace_scope_mut(&mut self, index: CrateIndex) -> &mut Namespace {
        match &mut self.bindings[index].kind {
            EntityKind::Module(scope) | EntityKind::UntypedDataType(scope) => scope,
            _ => unreachable!(),
        }
    }

    // @Task return Cow
    // @Task handle non-alphanums
    // @Note this prepends `crate`
    pub fn absolute_path(&self, index: CrateIndex) -> String {
        let entity = &self.bindings[index];
        if let Some(parent) = entity.parent {
            let mut parent = self.absolute_path(parent);
            parent.push('.');
            parent += entity.source.as_str();
            parent
        } else {
            String::from("crate")
        }
    }

    fn register_value_binding(
        &mut self,
        binder: &ast::Identifier,
        module: CrateIndex,
    ) -> Result<CrateIndex> {
        self.register_binding(
            binder,
            Entity {
                source: binder.clone(),
                parent: Some(module),
                kind: EntityKind::UntypedValue,
            },
            Some(module),
        )
    }

    fn register_module_binding(
        &mut self,
        binder: &ast::Identifier,
        module: Option<CrateIndex>,
    ) -> Result<CrateIndex> {
        self.register_binding(
            binder,
            Entity {
                source: binder.clone(),
                parent: module,
                kind: EntityKind::Module(Namespace {
                    // parent: module,
                    bindings: Vec::new(),
                }),
            },
            module,
        )
    }

    fn register_data_type_binding(
        &mut self,
        binder: &ast::Identifier,
        module: CrateIndex,
    ) -> Result<CrateIndex> {
        self.register_binding(
            binder,
            Entity {
                source: binder.clone(),
                parent: Some(module),
                kind: EntityKind::UntypedDataType(Namespace {
                    // parent: Some(module),
                    bindings: Vec::new(),
                }),
            },
            Some(module),
        )
    }

    fn register_use_binding(
        &mut self,
        binder: &ast::Identifier,
        reference: &Path,
        module: CrateIndex,
    ) -> Result<()> {
        let index = self.register_binding(
            binder,
            Entity {
                source: binder.clone(),
                parent: Some(module),
                kind: EntityKind::UnresolvedUse,
            },
            Some(module),
        )?;

        let old = self.unresolved_uses.insert(
            index,
            UnresolvedUse {
                reference: reference.clone(),
                module,
            },
        );

        debug_assert!(old.is_none());

        Ok(())
    }

    /// Register a binding to a given entity.
    ///
    /// Apart from actually registering, mainly checks for name duplication.
    fn register_binding(
        &mut self,
        binder: &ast::Identifier,
        binding: Entity,
        // or namespace in general
        module: Option<CrateIndex>,
    ) -> Result<CrateIndex> {
        if let Some(module) = module {
            if let Some(previous) = self
                .unwrap_namespace_scope(module)
                .bindings
                .iter()
                .map(|&index| &self.bindings[index])
                .find(|binding| &binding.source == binder)
            {
                return Err(Diagnostic::error()
                    .with_code(Code::E020)
                    .with_message(format!(
                        "`{}` is defined multiple times in this scope",
                        binder
                    ))
                    .with_labeled_span(&binder, "redefinition")
                    .with_labeled_span(&previous.source, "previous definition"));
            }
        }

        let index = self.bindings.push(binding);

        if let Some(module) = module {
            self.unwrap_namespace_scope_mut(module).bindings.push(index);
        }

        Ok(index)
    }

    /// Resolves a syntactic path given a module.
    fn resolve_path<Target: ResolutionTarget>(
        &self,
        path: &Path,
        module: CrateIndex,
        inquirer: Inquirer,
    ) -> Result<Target::Output, Error> {
        use ast::HeadKind::*;
        use EntityKind::*;

        if let Some(head) = &path.head {
            if path.segments.is_empty() {
                return Target::resolve_bare_super_and_crate(head.span, module).map_err(Into::into);
            }

            return self.resolve_path::<Target>(
                &path.tail(),
                match head.kind {
                    Crate => self.root(),
                    Super => self.resolve_super(head, module)?,
                    Self_ => module,
                },
                Inquirer::System,
            );
        }

        let index = self.resolve_first_segment(&path.segments[0], module, inquirer)?;
        let binding = &self.bindings[index].kind;

        match path.simple() {
            Some(identifier) => {
                Target::resolve_simple_path(identifier, binding, index).map_err(Into::into)
            }
            None => match binding {
                Module(_) | UntypedDataType(_) => {
                    self.resolve_path::<Target>(&path.tail(), index, Inquirer::System)
                }
                UntypedValue => {
                    Err(value_used_as_a_namespace(&path.segments[0], &path.segments[1]).into())
                }
                _ => unreachable!(),
            },
        }
    }

    fn resolve_super(&self, head: &ast::Head, module: CrateIndex) -> Result<CrateIndex> {
        self.bindings[module].parent.ok_or_else(|| {
            Diagnostic::error()
                .with_code(Code::E021)
                .with_message("the crate root does not have a parent")
                .with_span(head)
        })
    }

    fn resolve_first_segment(
        &self,
        identifier: &ast::Identifier,
        namespace: CrateIndex,
        inquirer: Inquirer,
    ) -> Result<CrateIndex, Error> {
        self.unwrap_namespace_scope(namespace)
            .bindings
            .iter()
            .copied()
            .find(|&index| &self.bindings[index].source == identifier)
            .ok_or_else(|| {
                let mut message = format!("binding `{}` is not defined in ", identifier);

                match inquirer {
                    Inquirer::User => message += "this scope",
                    Inquirer::System => {
                        match self.bindings[namespace].kind {
                            EntityKind::Module(_) => message += "module",
                            EntityKind::UntypedDataType(_) => message += "namespace",
                            _ => unreachable!(),
                        };
                        message += " `";
                        message += &self.absolute_path(namespace);
                        message += "`";
                    }
                }

                Diagnostic::error()
                    .with_code(Code::E021)
                    .with_message(message)
                    .with_span(&identifier)
                    .into()
            })
            .and_then(|index| self.resolve_use(index))
    }

    /// Resolve indirect uses aka chains of use bindings.
    ///
    /// Makes a lot of sense to be honest and it's just an arbitrary invariant
    /// I created to make things easir to reason about during resolution.
    fn resolve_use(&self, index: CrateIndex) -> Result<CrateIndex, Error> {
        use EntityKind::*;

        match self.bindings[index].kind {
            UntypedValue | Module(_) | UntypedDataType(_) => Ok(index),
            Use(reference) => Ok(reference),
            UnresolvedUse => Err(Error::FoundUnresolvedUse),
            _ => unreachable!(),
        }
    }

    /// Resolve a single identifier (in contrast to a whole path).
    fn resolve_identifier<Target: ResolutionTarget>(
        &self,
        identifier: &ast::Identifier,
        module: CrateIndex,
    ) -> Result<Target::Output> {
        let index = self
            .resolve_first_segment(identifier, module, Inquirer::System)
            .map_err(Error::unwrap)?;
        Target::resolve_simple_path(identifier, &self.bindings[index].kind, index)
    }

    fn resolve_unresolved_uses(&mut self) -> Results<()> {
        while !self.unresolved_uses.is_empty() {
            let mut unresolved_uses = HashMap::new();

            for (&index, item) in self.unresolved_uses.iter() {
                match self.resolve_path::<ValueOrModule>(
                    &item.reference,
                    item.module,
                    Inquirer::User,
                ) {
                    Ok(reference) => {
                        self.bindings[index].kind = EntityKind::Use(reference);
                    }
                    Err(Error::Unrecoverable(error)) => return Err(error).many_err(),
                    Err(Error::FoundUnresolvedUse) => {
                        unresolved_uses.insert(
                            index,
                            UnresolvedUse {
                                reference: item.reference.clone(),
                                module: item.module,
                            },
                        );
                    }
                }
            }

            if unresolved_uses.len() == self.unresolved_uses.len() {
                return Err(unresolved_uses
                    .keys()
                    .map(|&index| {
                        Diagnostic::error()
                            .with_code(Code::E024)
                            .with_message("this declaration is circular")
                            .with_span(&self.bindings[index].source)
                    })
                    .collect());
            }

            self.unresolved_uses = unresolved_uses;
        }

        Ok(())
    }
}

use crate::support::DisplayWith;

impl fmt::Debug for CrateScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use crate::support::DisplayIsDebug;

        f.debug_struct("resolver::CrateScope")
            .field(
                "bindings",
                &DisplayIsDebug(
                    &self
                        .bindings
                        .iter_enumerated()
                        .map(|(index, binding)| {
                            format!("\n    {:?}: {}", index, binding.with(self))
                        })
                        .collect::<String>(),
                ),
            )
            .field("unresolved_uses", &self.unresolved_uses)
            // .field("out_of_order_bindings", &self.out_of_order_bindings)
            .finish()
    }
}
/// Partially resolved use binding.
#[derive(Debug)]
struct UnresolvedUse {
    reference: Path,
    module: CrateIndex,
}

/// Marker to specify it's okay to resolve to either value or module.
enum ValueOrModule {}

impl ResolutionTarget for ValueOrModule {
    type Output = CrateIndex;

    fn resolve_bare_super_and_crate(_: Span, module: CrateIndex) -> Result<Self::Output> {
        Ok(module)
    }

    fn resolve_simple_path(
        _: &ast::Identifier,
        _: &EntityKind,
        index: CrateIndex,
    ) -> Result<Self::Output> {
        Ok(index)
    }
}

/// Marker to specify to only resolve to values.
enum OnlyValue {}

impl ResolutionTarget for OnlyValue {
    type Output = Identifier;

    fn resolve_bare_super_and_crate(span: Span, _: CrateIndex) -> Result<Self::Output> {
        Err(module_used_as_a_value(span))
    }

    fn resolve_simple_path(
        identifier: &ast::Identifier,
        binding: &EntityKind,
        index: CrateIndex,
    ) -> Result<Self::Output> {
        use EntityKind::*;

        match binding {
            UntypedValue | UntypedDataType(_) => Ok(Identifier::new(index, identifier.clone())),
            Module(_) => Err(module_used_as_a_value(identifier.span)),
            _ => unreachable!(),
        }
    }
}

/// Specifies behavior for resolving different sorts of entities.
///
/// Right now, it's only about the difference between values and modules
/// since modules are not values (non-first-class). As such, this trait
/// allows to implementors to define what should happen with the resolved entity
/// if it appears in a specific location
trait ResolutionTarget {
    type Output;

    fn resolve_bare_super_and_crate(span: Span, module: CrateIndex) -> Result<Self::Output>;

    fn resolve_simple_path(
        identifier: &ast::Identifier,
        binding: &EntityKind,
        index: CrateIndex,
    ) -> Result<Self::Output>;
}

/// The agent who inquired after a path.
///
/// Used for error reporting.
#[derive(PartialEq, Eq, Clone, Copy)]
enum Inquirer {
    User,
    System,
}

/// Some more additional information for resolving a declaration.
///
/// Grown out of the situation that constructors are represented as
/// any other declaration thus lacking knowledge about its parent
/// data declaration which is necessary for resolving.
#[derive(Default)]
struct Environment {
    data: Option<CrateIndex>,
}

impl Declaration<Desugared> {
    /// Resolve a desugar-HIR declaration to a resolver-HIR declaration.
    ///
    /// It performs three passes to resolve all possible out of order declarations.
    /// If the declaration passed is not a module, this function will panic as it
    /// requires a crate root which is defined through the root module.
    pub fn resolve(self, scope: &mut CrateScope) -> Results<Declaration<Resolved>> {
        self.resolve_first_pass(None, scope, default())?;
        // @Bug creates fatal errors for use stuff (see tests/multiple-undefined1)
        scope.resolve_unresolved_uses()?;
        self.resolve_second_pass(None, scope, default())
    }

    /// Partially resolve a declaration merely registering declarations.
    ///
    /// This traverses all declarations and registers module-level bindings
    /// checking that they are only defined once per namespace.
    /// Use bindings which refer to an unknown binding are marked as such
    /// to be resolved in the second, a minor pass.
    ///
    /// This also searches the program entry and stores it when it finds it.
    ///
    /// In contrast to [Self::resolve_second_pass], this does not actually return a
    /// new intermediate HIR because of too much mapping and type-system boilerplate
    /// and it's just not worth it memory-wise.
    fn resolve_first_pass(
        &self,
        module: Option<CrateIndex>,
        scope: &mut CrateScope,
        environment: Environment,
    ) -> Results<()> {
        use hir::DeclarationKind::*;

        match &self.kind {
            Value(declaration) => {
                let module = module.unwrap();

                let index = scope
                    .register_value_binding(&declaration.binder, module)
                    .many_err()?;

                if scope.program_entry.is_none() && module == scope.root() {
                    if declaration.binder.as_str() == PROGRAM_ENTRY_IDENTIFIER {
                        scope.program_entry =
                            Some(Identifier::new(index, declaration.binder.clone()));
                    }
                }
            }
            Data(declaration) => {
                let module = module.unwrap();

                let index = scope
                    .register_data_type_binding(&declaration.binder, module)
                    .many_err()?;

                if let Some(constructors) = &declaration.constructors {
                    constructors
                        .iter()
                        .map(|constructor| {
                            constructor.resolve_first_pass(
                                Some(module),
                                scope,
                                Environment { data: Some(index) },
                            )
                        })
                        .collect::<Vec<_>>()
                        .transpose()?;
                }
            }
            Constructor(declaration) => {
                scope
                    .register_value_binding(&declaration.binder, environment.data.unwrap())
                    .many_err()?;
            }
            Module(declaration) => {
                // @Task @Beacon don't return early on error
                // @Note you need to create a fake index for this (an index which points to
                // a fake, nameless binding)
                let index = scope
                    .register_module_binding(&declaration.binder, module)
                    .many_err()?;

                declaration
                    .declarations
                    .iter()
                    .map(|declaration| {
                        declaration.resolve_first_pass(Some(index), scope, default())
                    })
                    .collect::<Vec<_>>()
                    .transpose()?;
            }
            Use(declaration) => {
                let module = module.unwrap();

                let binder = declaration
                    .binder
                    .as_ref()
                    .ok_or_else(|| {
                        // @Task add note why
                        Diagnostic::error()
                            .with_message("`use` of bare `super` and `crate` disallowed")
                            .with_span(self)
                    })
                    .many_err()?;

                scope
                    .register_use_binding(binder, &declaration.target, module)
                    .many_err()?;
            }
            Invalid => {}
        }

        Ok(())
    }

    /// Completely resolve a desugar-HIR declaration to a resolver-HIR declaration.
    ///
    /// Tries to resolve all expressions and patterns contained within all declarations
    /// and actually builds the new HIR.
    ///
    /// This is the second major pass (but the third in total including the middle minor
    /// use-declaration resolving one).
    fn resolve_second_pass(
        self,
        module: Option<CrateIndex>,
        scope: &mut CrateScope,
        environment: Environment,
    ) -> Results<Declaration<Resolved>> {
        use hir::DeclarationKind::*;

        Ok(match self.kind {
            Value(value) => {
                let module = module.unwrap();

                let type_annotation = value
                    .type_annotation
                    .resolve(&FunctionScope::Module(module), scope);

                let expression = value
                    .expression
                    .map(|expression| expression.resolve(&FunctionScope::Module(module), scope))
                    .transpose();

                let binder = scope
                    .resolve_identifier::<OnlyValue>(&value.binder, module)
                    .many_err()?;

                let span = self.span;
                let attributes = self.attributes;

                let (type_annotation, expression) =
                    (type_annotation, expression).accumulate_err()?;

                decl! {
                    Value[span][attributes] {
                        binder,
                        type_annotation,
                        expression,
                    }
                }
            }
            Data(data) => {
                let module = module.unwrap();

                let type_annotation = data
                    .type_annotation
                    .resolve(&FunctionScope::Module(module), scope);

                let binder = scope
                    .resolve_identifier::<OnlyValue>(&data.binder, module)
                    .many_err()?;

                let constructors = data.constructors.map(|constructors| {
                    constructors
                        .into_iter()
                        .map(|constructor| {
                            constructor.resolve_second_pass(
                                Some(module),
                                scope,
                                Environment {
                                    data: Some(binder.crate_index().unwrap()),
                                },
                            )
                        })
                        .collect()
                });

                let (type_annotation, constructors) =
                    (type_annotation, constructors.transpose()).accumulate_err()?;

                decl! {
                    Data[self.span][self.attributes] {
                        binder,
                        constructors,
                        type_annotation,
                    }
                }
            }
            Constructor(constructor) => {
                let module = module.unwrap();

                let type_annotation = constructor
                    .type_annotation
                    .resolve(&FunctionScope::Module(module), scope)?;

                let binder = scope
                    .resolve_identifier::<OnlyValue>(&constructor.binder, environment.data.unwrap())
                    .many_err()?;

                decl! {
                    Constructor[self.span][self.attributes] {
                        binder,
                        type_annotation,
                    }
                }
            }
            Module(submodule) => {
                // @Note ValueOrModule too general @Bug this might lead to
                // values used as modules!! we should create OnlyModule
                let index = match module {
                    Some(module) => scope
                        .resolve_identifier::<ValueOrModule>(&submodule.binder, module)
                        .many_err()?,
                    None => scope.root(),
                };

                let declarations = submodule
                    .declarations
                    .into_iter()
                    .map(|declaration| {
                        declaration.resolve_second_pass(Some(index), scope, default())
                    })
                    .collect::<Vec<_>>()
                    .transpose()?;

                decl! {
                    Module[self.span][self.attributes] {
                        binder: Identifier::new(index, submodule.binder),
                        file: submodule.file,
                        declarations,
                    }
                }
            }
            Use(use_) => {
                let module = module.unwrap();
                let binder = use_.binder.unwrap();

                let index = scope
                    .resolve_identifier::<ValueOrModule>(&binder, module)
                    .many_err()?;

                decl! {
                    Use[self.span][self.attributes] {
                        binder: Some(Identifier::new(index, binder.clone())),
                        // @Temporary
                        target: Identifier::new(index, binder),
                    }
                }
            }
            Invalid => InvalidFallback::invalid(),
        })
    }
}

// @Task @Beacon use Rc::try_unwrap more instead of clone
impl Expression<Desugared> {
    fn resolve(
        self,
        scope: &FunctionScope<'_>,
        crate_scope: &CrateScope,
    ) -> Results<Expression<Resolved>> {
        use hir::ExpressionKind::*;

        let mut errors = Diagnostics::new();

        let expression = match self.kind {
            PiType(pi) => {
                let (domain, codomain) = (
                    pi.domain.clone().resolve(scope, crate_scope),
                    match pi.parameter.clone() {
                        Some(parameter) => pi
                            .codomain
                            .clone()
                            .resolve(&scope.extend_with_parameter(parameter), crate_scope),
                        None => pi.codomain.clone().resolve(scope, crate_scope),
                    },
                )
                    .accumulate_err()?;

                return Ok(expr! {
                    PiType[self.span] {
                        parameter: pi.parameter.clone()
                            .map(|parameter| Identifier::new(Index::DebruijnParameter, parameter.clone())),
                        domain,
                        codomain,
                        explicitness: pi.explicitness,
                    }
                });
            }
            Application(application) => {
                let (callee, argument) = (
                    application.callee.clone().resolve(scope, crate_scope),
                    application.argument.clone().resolve(scope, crate_scope),
                )
                    .accumulate_err()?;

                return Ok(expr! {
                    Application[self.span] {
                        callee,
                        argument,
                        explicitness: application.explicitness,
                    }
                });
            }
            Type => expr! { Type[self.span] },
            Number(number) => expr! {
                Number[self.span](number)
            },
            Text(text) => expr! {
                Text[self.span](text)
            },
            Binding(binding) => expr! {
                Binding[self.span] {
                    binder: scope.resolve_binding(&binding.binder, crate_scope).many_err()?,
                }
            },
            // @Task @Beacon @Beacon don't use try_softly here: you don't need to: use
            // accumulate_err, the stuff here is independent! right??
            Lambda(lambda) => expr! {
                Lambda[self.span] {
                    parameter: Identifier::new(Index::DebruijnParameter, lambda.parameter.clone()),
                    parameter_type_annotation: lambda.parameter_type_annotation.clone()
                        .map(|type_| type_
                            .resolve(scope, crate_scope)
                            .try_softly(&mut errors)),
                    body_type_annotation: lambda.body_type_annotation.clone()
                        .map(|type_| type_
                            .resolve(&scope.extend_with_parameter(lambda.parameter.clone()), crate_scope)
                            .try_softly(&mut errors)),
                    body: lambda.body.clone()
                        .resolve(&scope.extend_with_parameter(lambda.parameter.clone()), crate_scope)
                        .try_softly(&mut errors),
                    explicitness: lambda.explicitness,
                }
            },
            UseIn => todo!("resolving use/in"),
            CaseAnalysis(analysis) => {
                let subject = analysis.subject.clone().resolve(scope, crate_scope)?;
                let mut cases = Vec::new();

                for case in &analysis.cases {
                    let (pattern, binders) = case.pattern.clone().resolve(scope, crate_scope)?;
                    let body = case
                        .body
                        .clone()
                        .resolve(&scope.extend_with_pattern_binders(binders), crate_scope)?;

                    cases.push(hir::Case { pattern, body });
                }

                expr! {
                    CaseAnalysis[self.span] {
                        subject,
                        cases,
                    }
                }
            }
            Substitution(_) | ForeignApplication(_) => unreachable!(),
            Invalid => InvalidFallback::invalid(),
        };

        release!(errors);

        Ok(expression)
    }
}

use hir::Pattern;

impl Pattern<Desugared> {
    fn resolve(
        self,
        scope: &FunctionScope<'_>,
        crate_scope: &CrateScope,
    ) -> Results<(Pattern<Resolved>, Vec<ast::Identifier>)> {
        use hir::{pat, PatternKind::*};

        let mut binders = Vec::new();

        let pattern = match self.kind.clone() {
            Number(nat) => pat! {
                Number[self.span](nat)
            },
            Text(text) => pat! {
                Text[self.span](text)
            },
            Binding(binding) => pat! {
                Binding[self.span] {
                    binder: scope.resolve_binding(&binding.binder, crate_scope).many_err()?,
                }
            },
            Binder(binder) => {
                binders.push(binder.binder.clone());
                pat! {
                    Binder[self.span] {
                        binder: Identifier::new(Index::DebruijnParameter, unrc!(binder.binder)),
                    }
                }
            }
            Deapplication(deapplication) => {
                let ((callee, mut callee_binders), (argument, mut argument_binders)) = (
                    deapplication.callee.clone().resolve(scope, crate_scope),
                    deapplication.argument.clone().resolve(scope, crate_scope),
                )
                    .accumulate_err()?;

                binders.append(&mut callee_binders);
                binders.append(&mut argument_binders);

                pat! {
                    Deapplication[self.span] {
                        callee,
                        argument,
                    }
                }
            }
        };

        Ok((pattern, binders))
    }
}

/// A namespace can either be a module or a data type.
/// A module contains any declarations (except constructors) and
/// a data type their constructors.
#[derive(Clone)]
pub struct Namespace {
    bindings: Vec<CrateIndex>,
}

impl fmt::Debug for Namespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.bindings.iter().map(DebugIsDisplay).join_with(" ")
        )
    }
}

#[derive(Clone, PartialEq)]
pub struct Identifier {
    /// Source at the use-site/call-site or def-site if definition.
    pub source: ast::Identifier,
    pub index: Index,
}

impl Identifier {
    fn new(index: impl Into<Index>, source: ast::Identifier) -> Self {
        Self {
            index: index.into(),
            source,
        }
    }

    pub fn as_str(&self) -> &str {
        self.source.as_str()
    }

    pub fn to_expression(self) -> Expression<Resolved> {
        expr! { Binding[self.span()] { binder: self } }
    }

    // @Note bad name
    pub fn as_innermost(&self) -> Self {
        Self::new(DebruijnIndex(0), self.source.clone())
    }

    pub fn stripped(self) -> Self {
        Self {
            source: self.source.stripped(),
            ..self
        }
    }

    pub fn is_innermost(&self) -> bool {
        self.index == DebruijnIndex(0).into()
    }

    pub fn shift(self, amount: usize) -> Self {
        Self {
            index: self.index.shift(amount),
            ..self
        }
    }

    pub fn unshift(self) -> Self {
        Self {
            index: self.index.unshift(),
            ..self
        }
    }

    pub fn crate_index(&self) -> Option<CrateIndex> {
        match self.index {
            Index::Crate(index) => Some(index),
            _ => None,
        }
    }

    pub fn debruijn_index(&self) -> Option<DebruijnIndex> {
        match self.index {
            Index::Debruijn(index) => Some(index),
            _ => None,
        }
    }
}

impl Spanning for Identifier {
    fn span(&self) -> Span {
        self.source.span
    }
}

use std::fmt;

// @Task print whole path
impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.source)?;
        if crate::OPTIONS.get().unwrap().display_crate_indices {
            write!(f, "#")?;
            match self.index {
                Index::Crate(index) => write!(f, "{:?}", index)?,
                Index::Debruijn(index) => write!(f, "{}D", index.0)?,
                Index::DebruijnParameter => write!(f, "P")?,
            }
        }
        Ok(())
    }
}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Clone)]
pub enum Resolved {}

impl Pass for Resolved {
    type Binder = Identifier;
    type ReferencedBinder = Self::Binder;
    type ForeignApplicationBinder = Self::Binder;
    type Substitution = hir::Substitution<Self>;
    type ShowLinchpin = CrateScope;

    fn format_binding(
        binder: &Self::ReferencedBinder,
        scope: &CrateScope,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write!(f, "{}", FunctionScope::absolute_path(binder, scope))
    }

    fn format_substitution(
        substitution: &Self::Substitution,
        linchpin: &Self::ShowLinchpin,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write!(
            f,
            "<substitution {} {}>",
            substitution.substitution.with(linchpin),
            substitution.expression.with(linchpin)
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Index {
    Crate(CrateIndex),
    Debruijn(DebruijnIndex),
    DebruijnParameter,
}

impl Index {
    fn shift(self, amount: usize) -> Self {
        match self {
            Self::Crate(_) => self,
            Self::Debruijn(index) => DebruijnIndex(index.0 + amount).into(),
            Self::DebruijnParameter => unreachable!(),
        }
    }

    fn unshift(self) -> Self {
        match self {
            Self::Crate(_) => self,
            Self::Debruijn(index) => DebruijnIndex(index.0.saturating_sub(1)).into(),
            Self::DebruijnParameter => unreachable!(),
        }
    }
}

/// Crate-local identifier for bindings defined through declarations.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct CrateIndex(usize);

impl fmt::Debug for CrateIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}C", self.0)
    }
}

impl From<CrateIndex> for Index {
    fn from(index: CrateIndex) -> Self {
        Self::Crate(index)
    }
}

impl indexed_vec::Idx for CrateIndex {
    fn new(index: usize) -> Self {
        Self(index)
    }
    fn index(self) -> usize {
        self.0
    }
}

/// Identifier for bindings defined through function parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DebruijnIndex(pub usize);

impl From<DebruijnIndex> for Index {
    fn from(index: DebruijnIndex) -> Self {
        Self::Debruijn(index)
    }
}

pub enum FunctionScope<'a> {
    Module(CrateIndex),
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

    pub fn absolute_path(binder: &Identifier, scope: &CrateScope) -> String {
        match binder.index {
            Index::Crate(index) => scope.absolute_path(index),
            Index::Debruijn(_) | Index::DebruijnParameter => binder.as_str().into(),
        }
    }

    fn resolve_binding(&self, query: &Path, scope: &CrateScope) -> Result<Identifier> {
        self.resolve_binding_with_depth(query, scope, 0)
    }

    fn resolve_binding_with_depth(
        &self,
        query: &Path,
        scope: &CrateScope,
        depth: usize,
    ) -> Result<Identifier> {
        use FunctionScope::*;

        match self {
            Module(module) => scope
                .resolve_path::<OnlyValue>(query, *module, Inquirer::User)
                .map_err(Error::unwrap),
            // @Note this looks ugly/complicated, use helper functions
            FunctionParameter { parent, binder } => {
                if let Some(identifier) = query.first_identifier() {
                    if binder == identifier {
                        if query.segments.len() > 1 {
                            return Err(value_used_as_a_namespace(identifier, &query.segments[1]));
                        }

                        Ok(Identifier::new(DebruijnIndex(depth), identifier.clone()))
                    } else {
                        parent.resolve_binding_with_depth(query, scope, depth + 1)
                    }
                } else {
                    scope
                        .resolve_path::<OnlyValue>(query, parent.module(), Inquirer::User)
                        .map_err(Error::unwrap)
                }
            }
            // @Note this looks ugly/complicated, use helper functions
            PatternBinders { parent, binders } => {
                if let Some(identifier) = query.first_identifier() {
                    match binders
                        .iter()
                        .rev()
                        .zip(depth..)
                        .find(|(binder, _)| binder == &identifier)
                    {
                        Some((_, depth)) => {
                            if query.segments.len() > 1 {
                                return Err(value_used_as_a_namespace(
                                    identifier,
                                    &query.segments[1],
                                ));
                            }

                            Ok(Identifier::new(DebruijnIndex(depth), identifier.clone()))
                        }
                        None => {
                            parent.resolve_binding_with_depth(query, scope, depth + binders.len())
                        }
                    }
                } else {
                    scope
                        .resolve_path::<OnlyValue>(query, parent.module(), Inquirer::User)
                        .map_err(Error::unwrap)
                }
            }
        }
    }

    fn module(&self) -> CrateIndex {
        use FunctionScope::*;

        match self {
            Module(index) => *index,
            FunctionParameter { parent, .. } => parent.module(),
            PatternBinders { parent, .. } => parent.module(),
        }
    }
}

enum Error {
    Unrecoverable(Diagnostic),
    FoundUnresolvedUse,
}

impl Error {
    fn unwrap(self) -> Diagnostic {
        match self {
            Self::Unrecoverable(error) => error,
            _ => unreachable!(),
        }
    }
}

impl From<Diagnostic> for Error {
    fn from(error: Diagnostic) -> Self {
        Self::Unrecoverable(error)
    }
}

// @Task make those functions error variants

// @Question fatal?? and if non-fatal, it's probably ignored
fn value_used_as_a_namespace(
    non_namespace: &ast::Identifier,
    subbinder: &ast::Identifier,
) -> Diagnostic {
    Diagnostic::error()
        .with_code(Code::E022)
        .with_message(format!("value `{}` is not a namespace", non_namespace))
        .with_labeled_span(subbinder, "reference to a subdeclaration")
        .with_labeled_span(non_namespace, "a value, not a namespace")
}

fn module_used_as_a_value(span: Span) -> Diagnostic {
    Diagnostic::error()
        .with_code(Code::E023)
        .with_message("module used as if it was a value")
        .with_span(&span)
}

macro unrc($compound:ident.$projection:ident) {
    Rc::try_unwrap($compound)
        .map(|compound| compound.$projection)
        .unwrap_or_else(|compound| compound.$projection.clone())
}
