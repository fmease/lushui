//! Scope data structures used for name resolution.
//!
//! There are two types of scopes:
//!
//! * [CrateScope] for module-level bindings (may be out of order and recursive)
//!   i.e. bindings defined by declarations in modules, data types and constructors
//! * [FunctionScope] for bindings defined inside of expressions or functions (order matters)
//!   like parameters, let/in-binders and case-analysis binders
//!
//! The most important functions are [FunctionScope::resolve_binding] and [CrateScope::resolve_path]
//! (used by `resolve_binding`).
//!
//! The equivalent in the type checker is [crate::typer::interpreter::scope].

// @Beacon @Beacon @Task don't report similarily named *private* bindings!
// @Task recognize leaks of private types!

use crate::{
    ast::{self, Path},
    diagnostics::{Code, Diagnostic, Handler},
    entity::{Entity, EntityKind},
    error::{Health, Result},
    format::{pluralize, unordered_listing, AsDebug, Conjunction, DisplayWith, QuoteExt},
    smallvec,
    span::{Span, Spanned, Spanning},
    typer::interpreter::{ffi, scope::Registration},
    HashMap, HashSet, SmallVec,
};
use colored::Colorize;
use indexed_vec::IndexVec;
use joinery::JoinableIterator;
use std::{cell::RefCell, cmp::Ordering, fmt, usize};

type Bindings = IndexVec<CrateIndex, Entity>;

/// The crate scope for module-level bindings.
///
/// This structure is used not only by the name resolver but also the type checker.
#[derive(Default)]
pub struct CrateScope {
    pub(crate) program_entry: Option<Identifier>,
    /// All bindings inside of a crate.
    ///
    /// The first element must always be the root module.
    pub(crate) bindings: Bindings,
    /// For resolving out of order use-declarations.
    partially_resolved_use_bindings: HashMap<CrateIndex, PartiallyResolvedUseBinding>,
    /// For error reporting.
    pub(super) duplicate_definitions: HashMap<CrateIndex, DuplicateDefinition>,
    pub(super) health: Health,
    pub(crate) ffi: ffi::Scope,
    // @Note this is very coarse-grained: as soon as we cannot resolve EITHER type annotation (for example)
    // OR actual value(s), we bail out and add this here. This might be too conversative (leading to more
    // "circular type" errors or whatever), we can just discriminate by creating sth like
    // UnresolvedThingy/WorlistItem { index: CrateIndex, expression: TypeAnnotation|Value|Both|... }
    // for the typer only!
    pub(crate) out_of_order_bindings: Vec<Registration>,
}

impl CrateScope {
    /// The crate root.
    ///
    /// Unbeknownst to the subdeclarations of the crate, it takes the name
    /// given by external sources, the crate name. Inside the crate, the root
    /// can only ever be referenced through the keyword `crate` (unless renamed).
    pub(super) fn root(&self) -> CrateIndex {
        CrateIndex(0)
    }

    /// Build a textual representation of the absolute path of a binding.
    ///
    /// This procedure always prepends `crate.` to every path.
    pub fn absolute_path(&self, index: CrateIndex) -> String {
        use crate::lexer::token::is_punctuation;

        let entity = &self.bindings[index];

        if let Some(parent) = entity.parent {
            let mut parent = self.absolute_path(parent);

            let parent_is_punctuation = is_punctuation(parent.chars().next_back().unwrap());

            if parent_is_punctuation {
                parent.push(' ');
            }

            parent.push('.');

            if entity.source.is_punctuation() && parent_is_punctuation {
                parent.push(' ');
            }

            parent += entity.source.as_str();
            parent
        } else {
            "crate".into()
        }
    }

    /// Register a binding to a given entity.
    ///
    /// Apart from actually registering, it mainly checks for name duplication.
    // @Note the way we handle errors here is a mess but it takes time and understanding
    // to design a better API meeting all/most of our requirements!
    pub(super) fn register_binding(
        &mut self,
        binder: ast::Identifier,
        exposure: Exposure,
        binding: EntityKind,
        namespace: Option<CrateIndex>,
    ) -> Result<CrateIndex, RegistrationError> {
        if let Some(namespace) = namespace {
            if let Some(&index) = self.bindings[namespace]
                .namespace()
                .unwrap()
                .bindings
                .iter()
                .find(|&&index| self.bindings[index].source == binder)
            {
                let previous = &self.bindings[index].source;

                self.duplicate_definitions
                    .entry(index)
                    .or_insert(DuplicateDefinition {
                        binder: previous.to_string(),
                        occurrences: smallvec![previous.span],
                    })
                    .occurrences
                    .push(binder.span);

                return Err(RegistrationError::DuplicateDefinition);
            }
        }

        let index = self.bindings.push(Entity {
            source: binder,
            kind: binding,
            exposure,
            parent: namespace,
        });

        if let Some(namespace) = namespace {
            self.bindings[namespace]
                .namespace_mut()
                .unwrap()
                .bindings
                .push(index);
        }

        Ok(index)
    }

    pub(super) fn register_use_binding(
        &mut self,
        index: CrateIndex,
        reference: Path,
        module: CrateIndex,
    ) {
        let previous = self.partially_resolved_use_bindings.insert(
            index,
            PartiallyResolvedUseBinding {
                reference,
                module,
                inquirer: None,
            },
        );

        debug_assert!(previous.is_none());
    }

    /// Resolve a syntactic path given a namespace.
    fn resolve_path<Target: ResolutionTarget>(
        &self,
        path: &Path,
        namespace: CrateIndex,
        handler: &Handler,
    ) -> Result<Target::Output, ResolutionError> {
        self.resolve_path_with_origin::<Target>(
            path,
            namespace,
            namespace,
            IdentifierUsage::Unqualified,
            CheckExposure::Yes,
            handler,
        )
    }

    fn resolve_path_unchecked_exposure<Target: ResolutionTarget>(
        &self,
        path: &Path,
        namespace: CrateIndex,
        handler: &Handler,
    ) -> Result<Target::Output, ResolutionError> {
        self.resolve_path_with_origin::<Target>(
            path,
            namespace,
            namespace,
            IdentifierUsage::Unqualified,
            CheckExposure::No,
            handler,
        )
    }

    /// Resolve a syntactic path given a namespace with an explicit origin.
    // @Task memoize by (path, namespace)
    fn resolve_path_with_origin<Target: ResolutionTarget>(
        &self,
        path: &Path,
        namespace: CrateIndex,
        origin_namespace: CrateIndex,
        usage: IdentifierUsage,
        check_exposure: CheckExposure,
        handler: &Handler,
    ) -> Result<Target::Output, ResolutionError> {
        use ast::HangerKind::*;

        if let Some(hanger) = &path.hanger {
            let namespace = match hanger.kind {
                Crate => self.root(),
                Super => self.resolve_super(hanger, namespace, handler)?,
                Self_ => namespace,
            };

            return if path.segments.is_empty() {
                Target::handle_bare_super_and_crate(hanger, namespace, handler).map_err(Into::into)
            } else {
                self.resolve_path_with_origin::<Target>(
                    &path.tail(),
                    namespace,
                    origin_namespace,
                    IdentifierUsage::Qualified,
                    check_exposure,
                    handler,
                )
            };
        }

        let index = self.resolve_first_path_segment(
            &path.segments[0],
            namespace,
            origin_namespace,
            usage,
            check_exposure,
            handler,
        )?;

        match path.to_single_identifier() {
            Some(identifier) => {
                Target::resolve_simple_path(identifier, &self.bindings[index].kind, index, handler)
                    .map_err(Into::into)
            }
            None => match self.bindings[index].namespace() {
                Some(_) => self.resolve_path_with_origin::<Target>(
                    &path.tail(),
                    index,
                    origin_namespace,
                    IdentifierUsage::Qualified,
                    check_exposure,
                    handler,
                ),
                None if self.bindings[index].is_error() => {
                    // @Task add rationale
                    Ok(Target::output(index, path.last_identifier().unwrap()))
                }
                None => {
                    value_used_as_a_namespace(
                        &path.segments[0],
                        &path.segments[1],
                        namespace,
                        self,
                    )
                    .emit(handler);
                    Err(ResolutionError::Unrecoverable)
                }
            },
        }
    }

    fn resolve_super(
        &self,
        hanger: &ast::Hanger,
        module: CrateIndex,
        handler: &Handler,
    ) -> Result<CrateIndex> {
        module.parent(self).ok_or_else(|| {
            Diagnostic::error()
                .code(Code::E021) // @Question use a dedicated code?
                .message("the crate root does not have a parent")
                .primary_span(hanger)
                .emit(handler)
        })
    }

    /// Resolve the first identifier segment of a path.
    fn resolve_first_path_segment(
        &self,
        identifier: &ast::Identifier,
        namespace: CrateIndex,
        origin_namespace: CrateIndex,
        usage: IdentifierUsage,
        check_exposure: CheckExposure,
        handler: &Handler,
    ) -> Result<CrateIndex, ResolutionError> {
        let index = self.bindings[namespace]
            .namespace()
            .unwrap()
            .bindings
            .iter()
            .copied()
            .find(|&index| &self.bindings[index].source == identifier)
            .ok_or_else(|| ResolutionError::UnresolvedBinding {
                identifier: identifier.clone(),
                namespace,
                usage,
            })?;

        // @Temporary hack until we can manage cyclic exposure reaches!
        if matches!(check_exposure, CheckExposure::Yes) {
            self.handle_exposure(index, identifier, origin_namespace, handler)?;
        }
        self.collapse_use_chain(index)
    }

    // @Task verify that the exposure is checked even in the case of use-declarations
    // using use-bindings (use-chains).
    fn handle_exposure(
        &self,
        index: CrateIndex,
        identifier: &ast::Identifier,
        origin_namespace: CrateIndex,
        handler: &Handler,
    ) -> Result<(), ResolutionError> {
        let binding = &self.bindings[index];

        if let Exposure::Restricted(exposure) = &binding.exposure {
            // unwrap: root always has Exposure::Unrestricted
            let definition_site_namespace = binding.parent.unwrap();
            let reach =
                RestrictedExposure::resolve(exposure, definition_site_namespace, self, handler)?;

            if !self.is_allowed_to_access(origin_namespace, definition_site_namespace, reach) {
                Diagnostic::error()
                    .code(Code::E029)
                    .message(format!(
                        "binding `{}` is private",
                        self.absolute_path(index)
                    ))
                    .primary_span(identifier)
                    .emit(handler);
                return Err(ResolutionError::Unrecoverable);
            }
        }

        Ok(())
    }

    /// Indicate if the definition-site namespace can be accessed from the given namespace.
    fn is_allowed_to_access(
        &self,
        namespace: CrateIndex,
        definition_site_namespace: CrateIndex,
        reach: CrateIndex,
    ) -> bool {
        let access_from_same_namespace_or_below =
            namespace.some_ancestor_equals(definition_site_namespace, self);

        let access_from_above_in_reach = || namespace.some_ancestor_equals(reach, self);

        access_from_same_namespace_or_below || access_from_above_in_reach()
    }

    pub(super) fn resolve_exposure_reaches(&mut self, handler: &Handler) -> Result {
        let mut health = Health::Untainted;

        for binding in &self.bindings {
            if let Exposure::Restricted(exposure) = &binding.exposure {
                // unwrap: root always has Exposure::Unrestricted
                let definition_site_namespace = binding.parent.unwrap();

                if RestrictedExposure::resolve(exposure, definition_site_namespace, self, handler)
                    .is_err()
                {
                    health.taint();
                    continue;
                };
            }

            if let EntityKind::Use { reference } = binding.kind {
                let reference = &self.bindings[reference];

                if binding.exposure.compare(&reference.exposure, self) == Some(Ordering::Greater) {
                    Diagnostic::error()
                        .code(Code::E009)
                        // @Question use absolute path?
                        .message(format!(
                            "re-export of the more private binding `{}`",
                            reference.source
                        ))
                        .labeled_primary_span(
                            &binding.source,
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
                            binding.source, // @Task absolute path
                            reference.exposure.with(self),
                            binding.exposure.with(self),
                        ))
                        .emit(handler);
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
    fn find_similarly_named(
        &self,
        queried_identifier: &str,
        namespace: &Namespace,
    ) -> Option<&str> {
        namespace
            .bindings
            .iter()
            .map(|&index| &self.bindings[index])
            .filter(|binding| !binding.is_error())
            .map(|binding| binding.source.as_str())
            .find(|&identifier| is_similar(queried_identifier, identifier))
    }

    // @Question better API?
    fn find_similarly_named_filtering(
        &self,
        identifier: &str,
        filter: impl Fn(&Entity) -> bool,
        namespace: &Namespace,
    ) -> Option<&str> {
        namespace
            .bindings
            .iter()
            .map(|&index| &self.bindings[index])
            .filter(|entity| filter(entity))
            .map(|entity| entity.source.as_str())
            .find(|&other_identifier| is_similar(identifier, other_identifier))
    }

    /// Collapse chain of use-bindings aka indirect uses.
    ///
    /// This is an invariant established to make things easier to reason about during resolution.
    fn collapse_use_chain(&self, index: CrateIndex) -> Result<CrateIndex, ResolutionError> {
        use EntityKind::*;

        match self.bindings[index].kind {
            UntypedValue | Module(_) | UntypedDataType(_) | UntypedConstructor(_) => Ok(index),
            Use { reference } => Ok(reference),
            UnresolvedUse => Err(ResolutionError::UnresolvedUseBinding { inquirer: index }),
            Error => Ok(index),
            _ => unreachable!(),
        }
    }

    /// Reobtain the resolved identifier.
    ///
    /// Used in [super::Resolver::finish_resolve_declaration], the last pass of the
    /// name resolver, to re-gain some information (the [Identifier]s) collected during the first pass.
    ///
    /// This way, [super::Resolver::start_resolve_declaration] does not need to return
    /// a new intermediate representation being a representation between the
    /// lowered AST and the HIR where all the _binders_ of declarations are resolved
    /// (i.e. are [Identifier]s) but all _bindings_ (in type annotations, expressions, …)
    /// are still unresolved (i.e. are [crate::ast::Identifier]s).
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
    pub(super) fn reobtain_resolved_identifier<Target: ResolutionTarget>(
        &self,
        identifier: &ast::Identifier,
        namespace: CrateIndex,
    ) -> Target::Output {
        let index = self.bindings[namespace]
            .namespace()
            .unwrap()
            .bindings
            .iter()
            .copied()
            .find(|&index| &self.bindings[index].source == identifier)
            .unwrap();

        let index = self
            .collapse_use_chain(index)
            .unwrap_or_else(|_| unreachable!());

        Target::output(index, identifier)
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
    pub(super) fn resolve_use_bindings(&mut self, handler: &Handler) {
        use ResolutionError::*;

        while !self.partially_resolved_use_bindings.is_empty() {
            let mut partially_resolved_use_bindings = HashMap::default();

            for (&index, item) in self.partially_resolved_use_bindings.iter() {
                match self.resolve_path::<ValueOrModule>(&item.reference, item.module, handler) {
                    Ok(reference) => {
                        self.bindings[index].kind = EntityKind::Use { reference };
                    }
                    Err(error @ (UnresolvedBinding { .. } | Unrecoverable)) => {
                        self.bindings[index].mark_as_error();
                        error.emit(self, handler);
                        self.health.taint();
                    }
                    Err(UnresolvedUseBinding { inquirer }) => {
                        partially_resolved_use_bindings.insert(
                            index,
                            PartiallyResolvedUseBinding {
                                reference: item.reference.clone(),
                                module: item.module,
                                inquirer: Some(inquirer),
                            },
                        );
                    }
                }
            }

            // resolution stalled; therefore there are circular bindings
            if partially_resolved_use_bindings.len() == self.partially_resolved_use_bindings.len() {
                for &index in partially_resolved_use_bindings.keys() {
                    self.bindings[index].mark_as_error();
                }

                for cycle in find_cycles(partially_resolved_use_bindings) {
                    let paths = cycle.iter().map(|&index| self.absolute_path(index).quote());
                    let paths = unordered_listing(paths, Conjunction::And);
                    let spans = cycle.iter().map(|&index| self.bindings[index].source.span);

                    Diagnostic::error()
                        .code(Code::E024)
                        .message(pluralize!(
                            cycle.len(),
                            format!("declaration {paths} is circular"),
                            format!("declarations {paths} are circular"),
                        ))
                        .primary_spans(spans)
                        .emit(handler);
                }

                self.health.taint();
                break;
            }

            self.partially_resolved_use_bindings = partially_resolved_use_bindings;
        }

        self.partially_resolved_use_bindings.clear();

        type UseBindings = HashMap<CrateIndex, PartiallyResolvedUseBinding>;
        type Cycle = HashSet<CrateIndex>;

        fn find_cycles(bindings: UseBindings) -> Vec<Cycle> {
            let mut cycles = Vec::new();
            let mut visited = HashMap::default();

            enum Status {
                InProgress,
                Finished,
            }

            for &index in bindings.keys() {
                if !visited.contains_key(&index) {
                    let mut worklist = vec![index];
                    visited.insert(index, Status::InProgress);
                    cycles.extend(find_cycle(&bindings, &mut worklist, &mut visited));
                }
            }

            fn find_cycle(
                bindings: &UseBindings,
                worklist: &mut Vec<CrateIndex>,
                visited: &mut HashMap<CrateIndex, Status>,
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
}

impl fmt::Debug for CrateScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CrateScope")
            .field(
                "bindings",
                &self
                    .bindings
                    .iter_enumerated()
                    .map(|(index, binding)| format!("\n    {:?}: {}", index, binding.with(self)))
                    .collect::<String>()
                    .as_debug(),
            )
            // @Bug debug out broken: missing newline: we need to do it manually without debug_struct
            .field(
                "partially_resolved_use_bindings",
                &self.partially_resolved_use_bindings,
            )
            // .field("out_of_order_bindings", &self.out_of_order_bindings)
            .finish()
    }
}

#[derive(Clone, Copy)]
enum CheckExposure {
    Yes,
    No,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Exposure {
    Unrestricted,
    Restricted(RefCell<RestrictedExposure>),
}

impl Exposure {
    fn compare(&self, other: &Self, scope: &CrateScope) -> Option<Ordering> {
        use Exposure::*;

        match (self, other) {
            (Unrestricted, Unrestricted) => Some(Ordering::Equal),
            (Unrestricted, Restricted(_)) => Some(Ordering::Greater),
            (Restricted(_), Unrestricted) => Some(Ordering::Less),
            (Restricted(this), Restricted(other)) => this.borrow().compare(&other.borrow(), scope),
        }
    }
}

impl fmt::Debug for Exposure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unrestricted => write!(f, "{}", "unrestricted"),
            Self::Restricted(reach) => {
                write!(f, "restricted(")?;
                match &*reach.borrow() {
                    RestrictedExposure::Unresolved { reach } => write!(f, "{reach}"),
                    RestrictedExposure::Resolved { reach } => write!(f, "{reach}"),
                }?;
                write!(f, ")")
            }
        }
    }
}

impl DisplayWith for Exposure {
    type Linchpin = CrateScope;

    fn format(&self, scope: &CrateScope, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unrestricted => write!(f, "unrestricted"),
            Self::Restricted(reach) => write!(f, "`{}`", reach.borrow().with(scope)),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum RestrictedExposure {
    Unresolved { reach: Path },
    Resolved { reach: CrateIndex },
}

impl RestrictedExposure {
    fn resolve(
        exposure: &RefCell<Self>,
        definition_site_namespace: CrateIndex,
        scope: &CrateScope,
        handler: &Handler,
    ) -> Result<CrateIndex> {
        let exposure_ = exposure.borrow();

        Ok(match &*exposure_ {
            Self::Unresolved {
                reach: unresolved_reach,
            } => {
                // Here we indeed resolve the exposure reach without validating *its*
                // exposure reach. This is not a problem however since in all cases where
                // it actually is private, it cannot be an ancestor module as those are
                // always accessible to their descendants, and therefore we are going to
                // throw an error.
                // It's not possible to use `resolve_path` as that can lead to infinite
                // loops with out of order use-bindings.
                let reach = scope
                    .resolve_path_unchecked_exposure::<OnlyModule>(
                        unresolved_reach,
                        definition_site_namespace,
                        handler,
                    )
                    .map_err(|error| error.emit(scope, handler))?;

                let reach_is_ancestor =
                    definition_site_namespace.some_ancestor_equals(reach, scope);

                if !reach_is_ancestor {
                    return Err(Diagnostic::error()
                        .code(Code::E000)
                        .message("exposure can only be restricted to ancestor modules")
                        .primary_span(unresolved_reach)
                        .emit(handler));
                }

                drop(exposure_);
                *exposure.borrow_mut() = RestrictedExposure::Resolved { reach };

                reach
            }
            &Self::Resolved { reach } => reach,
        })
    }

    fn compare(&self, other: &Self, scope: &CrateScope) -> Option<Ordering> {
        use RestrictedExposure::*;

        Some(match (self, other) {
            (&Resolved { reach: this }, &Resolved { reach: other }) => {
                if this == other {
                    Ordering::Equal
                } else if other.some_ancestor_equals(this, scope) {
                    Ordering::Greater
                } else if this.some_ancestor_equals(other, scope) {
                    Ordering::Less
                } else {
                    return None;
                }
            }
            _ => return None,
        })
    }
}

impl DisplayWith for RestrictedExposure {
    type Linchpin = CrateScope;

    fn format(&self, scope: &CrateScope, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unresolved { reach } => write!(f, "{}", reach),
            &Self::Resolved { reach } => write!(f, "{}", scope.absolute_path(reach)),
        }
    }
}

impl From<RestrictedExposure> for Exposure {
    fn from(exposure: RestrictedExposure) -> Self {
        Self::Restricted(RefCell::new(exposure))
    }
}

/// Duplicate definition diagnostic.
pub(super) struct DuplicateDefinition {
    binder: String,
    occurrences: SmallVec<Span, 2>,
}

impl From<DuplicateDefinition> for Diagnostic {
    fn from(definition: DuplicateDefinition) -> Self {
        Diagnostic::error()
            .code(Code::E020)
            .message(format!(
                "`{}` is defined multiple times in this scope",
                definition.binder
            ))
            .labeled_primary_spans(definition.occurrences.into_iter(), "conflicting definition")
    }
}

/// Specifies behavior for resolving different sorts of entities.
///
/// Right now, it's only about the difference between values and modules
/// since modules are not values (non-first-class). As such, this trait
/// allows to implementors to define what should happen with the resolved entity
/// if it appears in a specific location
pub(super) trait ResolutionTarget {
    type Output;

    fn output(index: CrateIndex, identifier: &ast::Identifier) -> Self::Output;

    fn handle_bare_super_and_crate(
        hanger: &ast::Hanger,
        namespace: CrateIndex,
        handler: &Handler,
    ) -> Result<Self::Output>;

    fn resolve_simple_path(
        identifier: &ast::Identifier,
        binding: &EntityKind,
        index: CrateIndex,
        handler: &Handler,
    ) -> Result<Self::Output>;
}

/// Marker to specify it's okay to resolve to either value or module.
pub(super) enum ValueOrModule {}

impl ResolutionTarget for ValueOrModule {
    type Output = CrateIndex;

    fn output(index: CrateIndex, _: &ast::Identifier) -> Self::Output {
        index
    }

    fn handle_bare_super_and_crate(
        _: &ast::Hanger,
        namespace: CrateIndex,
        _: &Handler,
    ) -> Result<Self::Output> {
        Ok(namespace)
    }

    fn resolve_simple_path(
        _: &ast::Identifier,
        _: &EntityKind,
        index: CrateIndex,
        _: &Handler,
    ) -> Result<Self::Output> {
        Ok(index)
    }
}

/// Marker to specify to only resolve to values.
pub(super) enum OnlyValue {}

impl ResolutionTarget for OnlyValue {
    type Output = Identifier;

    fn output(index: CrateIndex, identifier: &ast::Identifier) -> Self::Output {
        Identifier::new(index, identifier.clone())
    }

    fn handle_bare_super_and_crate(
        hanger: &ast::Hanger,
        _: CrateIndex,
        handler: &Handler,
    ) -> Result<Self::Output> {
        Err(module_used_as_a_value(hanger.as_ref()).emit(handler))
    }

    fn resolve_simple_path(
        identifier: &ast::Identifier,
        binding: &EntityKind,
        index: CrateIndex,
        handler: &Handler,
    ) -> Result<Self::Output> {
        use EntityKind::*;

        match binding {
            UntypedValue | UntypedDataType(_) | UntypedConstructor(_) | Error => {
                Ok(Self::output(index, identifier))
            }
            Module(_) => Err(module_used_as_a_value(Spanned::new(
                identifier.span,
                identifier.as_str(),
            ))
            .emit(handler)),
            _ => unreachable!(),
        }
    }
}

pub(super) enum OnlyModule {}

impl ResolutionTarget for OnlyModule {
    type Output = CrateIndex;

    fn output(index: CrateIndex, _: &ast::Identifier) -> Self::Output {
        index
    }

    fn handle_bare_super_and_crate(
        _: &ast::Hanger,
        namespace: CrateIndex,
        _: &Handler,
    ) -> Result<Self::Output> {
        Ok(namespace)
    }

    fn resolve_simple_path(
        identifier: &ast::Identifier,
        binding: &EntityKind,
        index: CrateIndex,
        handler: &Handler,
    ) -> Result<Self::Output> {
        use EntityKind::*;

        match binding {
            // @Beacon @Task use a distinct code!
            UntypedValue | UntypedDataType(_) | UntypedConstructor(_) => Err(Diagnostic::error()
                .code(Code::E022)
                .message(format!("value `{}` is not a module", identifier))
                .primary_span(identifier)
                .emit(handler)),
            Module(_) => Ok(index),
            _ => unreachable!(),
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

#[derive(Debug)]
struct PartiallyResolvedUseBinding {
    reference: Path,
    module: CrateIndex,
    inquirer: Option<CrateIndex>,
}

/// A namespace can either be a module or a data type.
/// A module contains any declarations (except constructors) and
/// a data type their constructors.
// @Task update documentation
#[derive(Clone, Default)]
pub struct Namespace {
    bindings: Vec<CrateIndex>,
}

impl fmt::Debug for Namespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.bindings.iter().join_with(" "))
    }
}

// @Question move to resolver/hir.rs?
/// A name-resolved identifier.
#[derive(Clone, PartialEq)]
pub struct Identifier {
    /// Source at the use-site/call-site or def-site if definition.
    pub(crate) source: ast::Identifier,
    pub(crate) index: Index,
}

impl Identifier {
    pub(super) fn new(index: impl Into<Index>, source: ast::Identifier) -> Self {
        Self {
            index: index.into(),
            source,
        }
    }

    pub(crate) fn parameter(name: &str) -> Self {
        Identifier::new(
            Index::DeBruijnParameter,
            ast::Identifier::new(name.into(), Span::SHAM),
        )
    }

    pub fn as_str(&self) -> &str {
        self.source.as_str()
    }

    pub fn to_expression(self) -> crate::hir::Expression {
        crate::hir::expr! {
            Binding {
                crate::lowered_ast::Attributes::default(), self.span();
                binder: self
            }
        }
    }

    // @Note bad name
    pub fn as_innermost(&self) -> Self {
        Self::new(DeBruijnIndex(0), self.source.clone())
    }

    pub fn stripped(self) -> Self {
        Self {
            source: self.source.stripped(),
            ..self
        }
    }

    pub fn is_innermost(&self) -> bool {
        self.index == DeBruijnIndex(0).into()
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
        self.index.crate_()
    }

    pub fn de_bruijn_index(&self) -> Option<DeBruijnIndex> {
        self.index.de_bruijn()
    }
}

impl Spanning for Identifier {
    fn span(&self) -> Span {
        self.source.span
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.source)?;
        if crate::OPTIONS
            .get()
            .map_or(false, |options| options.show_binding_indices)
        {
            // @Note does not work well with punctuation..
            write!(f, "#{:?}", self.index)?;
        }
        Ok(())
    }
}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum Index {
    Crate(CrateIndex),
    DeBruijn(DeBruijnIndex),
    DeBruijnParameter,
}

impl Index {
    fn shift(self, amount: usize) -> Self {
        match self {
            Self::DeBruijn(index) => DeBruijnIndex(index.0 + amount).into(),
            Self::Crate(_) | Self::DeBruijnParameter => self,
        }
    }

    fn unshift(self) -> Self {
        match self {
            Self::DeBruijn(index) => DeBruijnIndex(index.0.saturating_sub(1)).into(),
            Self::Crate(_) | Self::DeBruijnParameter => self,
        }
    }

    pub fn crate_(self) -> Option<CrateIndex> {
        match self {
            Self::Crate(index) => Some(index),
            _ => None,
        }
    }

    pub fn de_bruijn(self) -> Option<DeBruijnIndex> {
        match self {
            Self::DeBruijn(index) => Some(index),
            _ => None,
        }
    }
}

impl fmt::Debug for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Crate(index) => write!(f, "{:?}", index),
            Self::DeBruijn(index) => write!(f, "{:?}", index),
            Self::DeBruijnParameter => write!(f, "P"),
        }
    }
}

/// Crate-local identifier for bindings defined through declarations.
// @Note name is kind of misleading, we should think about renaming it
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct CrateIndex(pub(crate) usize);

impl CrateIndex {
    fn parent(self, scope: &CrateScope) -> Option<Self> {
        scope.bindings[self].parent
    }

    fn some_ancestor_equals(mut self, namespace: CrateIndex, scope: &CrateScope) -> bool {
        loop {
            if self == namespace {
                break true;
            }

            self = match self.parent(scope) {
                Some(parent) => parent,
                None => break false,
            }
        }
    }
}

impl fmt::Debug for CrateIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}C", self.0)
    }
}

impl fmt::Display for CrateIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
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

/// De Bruijn index — index for bindings defined by function parameters.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DeBruijnIndex(pub usize);

impl fmt::Debug for DeBruijnIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}D", self.0)
    }
}

impl From<DeBruijnIndex> for Index {
    fn from(index: DeBruijnIndex) -> Self {
        Self::DeBruijn(index)
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
    pub(super) fn extend_with_parameter(&'a self, binder: ast::Identifier) -> Self {
        Self::FunctionParameter {
            parent: self,
            binder,
        }
    }

    pub(super) fn extend_with_pattern_binders(&'a self, binders: Vec<ast::Identifier>) -> Self {
        Self::PatternBinders {
            parent: self,
            binders,
        }
    }

    fn module(&self) -> CrateIndex {
        match self {
            &Self::Module(module) => module,
            Self::FunctionParameter { parent, .. } | Self::PatternBinders { parent, .. } => {
                parent.module()
            }
        }
    }

    pub fn absolute_path(binder: &Identifier, scope: &CrateScope) -> String {
        match binder.index {
            Index::Crate(index) => scope.absolute_path(index),
            Index::DeBruijn(_) | Index::DeBruijnParameter => binder.as_str().into(),
        }
    }

    /// Resolve a binding in a function scope given a depth.
    pub(super) fn resolve_binding(
        &self,
        query: &Path,
        scope: &CrateScope,
        handler: &Handler,
    ) -> Result<Identifier> {
        self.resolve_binding_with_depth(query, scope, 0, self, handler)
    }

    /// Resolve a binding in a function scope given a depth.
    ///
    /// The `depth` is necessary for the recursion to successfully create DeBruijn-indices.
    ///
    /// The `origin` signifies the innermost function scope from where the resolution was first requested.
    /// This information is used for diagnostics, namely typo flagging where we once again start at the origin
    /// and walk back out.
    fn resolve_binding_with_depth(
        &self,
        query: &Path,
        scope: &CrateScope,
        depth: usize,
        origin: &Self,
        handler: &Handler,
    ) -> Result<Identifier> {
        use FunctionScope::*;

        // @Note kinda awkward API with map_err
        match self {
            &Module(module) => scope
                .resolve_path::<OnlyValue>(query, module, handler)
                .map_err(|error| {
                    error.emit_finding_lookalike(
                        scope,
                        |identifier, _| origin.find_similarly_named(identifier, scope),
                        handler,
                    )
                }),
            // @Note this looks ugly/complicated, use helper functions
            FunctionParameter { parent, binder } => {
                if let Some(identifier) = query.identifier_head() {
                    if binder == identifier {
                        if query.segments.len() > 1 {
                            return Err(value_used_as_a_namespace(
                                identifier,
                                &query.segments[1],
                                self.module(),
                                scope,
                            )
                            .emit(handler));
                        }

                        Ok(Identifier::new(DeBruijnIndex(depth), identifier.clone()))
                    } else {
                        parent.resolve_binding_with_depth(query, scope, depth + 1, origin, handler)
                    }
                } else {
                    scope
                        .resolve_path::<OnlyValue>(query, parent.module(), handler)
                        .map_err(|error| error.emit(scope, handler))
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
                                return Err(value_used_as_a_namespace(
                                    identifier,
                                    &query.segments[1],
                                    self.module(),
                                    scope,
                                )
                                .emit(handler));
                            }

                            Ok(Identifier::new(DeBruijnIndex(depth), identifier.clone()))
                        }
                        None => parent.resolve_binding_with_depth(
                            query,
                            scope,
                            depth + binders.len(),
                            origin,
                            handler,
                        ),
                    }
                } else {
                    scope
                        .resolve_path::<OnlyValue>(query, parent.module(), handler)
                        .map_err(|error| error.emit(scope, handler))
                }
            }
        }
    }

    /// Find a similarly named binding in the scope.
    ///
    /// With "scope", it is meant to include parent scopes, too.
    ///
    /// Used for error reporting when an undefined binding was encountered.
    /// In the future, we might decide to find not one but several similar names
    /// but that would be computationally heavier and we would need to be careful
    /// and consider the effects of shadowing.
    fn find_similarly_named(&self, identifier: &str, scope: &'a CrateScope) -> Option<&str> {
        use FunctionScope::*;

        match self {
            &Module(module) => {
                scope.find_similarly_named(identifier, scope.bindings[module].namespace().unwrap())
            }
            FunctionParameter { parent, binder } => {
                if is_similar(identifier, binder.as_str()) {
                    Some(binder.as_str())
                } else {
                    parent.find_similarly_named(identifier, scope)
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
                    parent.find_similarly_named(identifier, scope)
                }
            }
        }
    }
}

fn is_similar(queried_identifier: &str, other_identifier: &str) -> bool {
    strsim::levenshtein(other_identifier, queried_identifier)
        <= std::cmp::max(queried_identifier.len(), 3) / 3
}

struct Lookalike<'a> {
    actual: &'a str,
    lookalike: &'a str,
}

impl fmt::Display for Lookalike<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use difference::Difference::*;

        let changeset = difference::Changeset::new(self.actual, self.lookalike, "");
        let mut purely_additive = true;

        write!(f, "`")?;

        for difference in &changeset.diffs {
            match difference {
                Same(segment) => write!(f, "{}", segment)?,
                Add(segment) => write!(f, "{}", segment.bold())?,
                Rem(_) => {
                    purely_additive = false;
                }
            }
        }

        write!(f, "`")?;

        if !purely_additive {
            // @Beacon @Beacon @Bug the provided Display impl disrespects NO_COLOR!!
            // write a custom implementation!
            write!(f, " ({changeset})")?;
        }

        Ok(())
    }
}

fn value_used_as_a_namespace(
    non_namespace: &ast::Identifier,
    subbinder: &ast::Identifier,
    parent: CrateIndex,
    scope: &CrateScope,
) -> Diagnostic {
    // @Question should we also include lookalike namespaces that don't contain the
    // subbinding (displaying them in a separate help message?)?
    // @Beacon @Bug this ignores shadowing @Task define this method for FunctionScope
    // @Update @Note actually in the future (lang spec) this kind of shadowing does not
    // occur, the subbinder access pierces through parameters
    let similarly_named_namespace = scope.find_similarly_named_filtering(
        non_namespace.as_str(),
        |entity| {
            entity.namespace().map_or(false, |namespace| {
                namespace
                    .bindings
                    .iter()
                    .find(|&&index| &scope.bindings[index].source == subbinder)
                    .is_some()
            })
        },
        scope.bindings[parent].namespace().unwrap(),
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
        .when_present(
            similarly_named_namespace,
            |this, lookalike| {
                this
                    .help(format!(
                        "a namespace with a similar name containing the binding exists in scope:\n    {}",
                        Lookalike { actual: non_namespace.as_str(), lookalike },
                    ))
            }
        )
        .when(show_very_general_help, |this| {
            this
                .note("identifiers following a `.` refer to bindings defined in a namespace (i.e. a module or a data type)")
                // no type information here yet to check if the non-namespace is indeed a record
                .help("use `::` to reference a field of a record")
        })
}

fn module_used_as_a_value(module: Spanned<impl fmt::Display>) -> Diagnostic {
    // @Task levenshtein-search for similar named bindings which are in fact values and suggest the first one
    // @Task print absolute path of the module in question and use highlight the entire path, not just the last
    // segment
    Diagnostic::error()
        .code(Code::E023)
        .message(format!("module `{module}` is used as a value"))
        .primary_span(module)
        .help("modules are not first-class citizens, consider utilizing records for such cases instead")
}

/// A possibly recoverable error that cab emerge during resolution.
#[derive(Debug)] // @Temporary
enum ResolutionError {
    Unrecoverable,
    UnresolvedBinding {
        identifier: ast::Identifier,
        namespace: CrateIndex,
        usage: IdentifierUsage,
    },
    UnresolvedUseBinding {
        inquirer: CrateIndex,
    },
}

impl ResolutionError {
    fn emit(self, scope: &CrateScope, handler: &Handler) {
        self.emit_finding_lookalike(
            scope,
            |identifier, namespace| {
                scope.find_similarly_named(
                    identifier,
                    scope.bindings[namespace].namespace().unwrap(),
                )
            },
            handler,
        );
    }

    fn emit_finding_lookalike<'s>(
        self,
        scope: &CrateScope,
        find_lookalike: impl FnOnce(&str, CrateIndex) -> Option<&'s str>,
        handler: &Handler,
    ) {
        match self {
            Self::Unrecoverable => {}
            Self::UnresolvedBinding {
                identifier,
                namespace,
                usage,
            } => {
                // @Question should we use the terminology "field" when the namespace is a record?
                let mut message = format!("binding `{}` is not defined in ", identifier);

                match usage {
                    IdentifierUsage::Unqualified => message += "this scope",
                    IdentifierUsage::Qualified => {
                        message += match scope.bindings[namespace].kind {
                            EntityKind::Module(_) => "module",
                            EntityKind::UntypedDataType(_) | EntityKind::UntypedConstructor(_) => {
                                "namespace"
                            }
                            _ => unreachable!(),
                        };
                        message += " `";
                        message += &scope.absolute_path(namespace);
                        message += "`";
                    }
                }

                Diagnostic::error()
                    .code(Code::E021)
                    .message(message)
                    .primary_span(&identifier)
                    .when_present(
                        find_lookalike(identifier.as_str(), namespace),
                        |diagnostic, binding| {
                            diagnostic.help(format!(
                                "a binding with a similar name exists in scope: {}",
                                Lookalike {
                                    actual: identifier.as_str(),
                                    lookalike: binding
                                },
                            ))
                        },
                    )
                    .emit(handler);
            }
            _ => unreachable!(),
        }
    }
}

impl From<()> for ResolutionError {
    fn from((): ()) -> Self {
        Self::Unrecoverable
    }
}

pub(super) enum RegistrationError {
    Unrecoverable,
    /// Duplicate definitions found.
    ///
    /// Details about this error are **not stored here** but as
    /// [DuplicateDefinition]s in the [super::Resolver] so they can
    /// be easily grouped.
    DuplicateDefinition,
}

impl From<()> for RegistrationError {
    fn from((): ()) -> Self {
        RegistrationError::Unrecoverable
    }
}
