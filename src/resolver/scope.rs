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
    diagnostics::{Code, Diagnostic, Reporter},
    entity::{Entity, EntityKind},
    error::{Health, Result},
    format::{
        pluralize, unordered_listing, AsAutoColoredChangeset, Conjunction, DisplayWith, QuoteExt,
    },
    package::{CrateIndex, CrateStore, CrateType, PackageIndex},
    parser::ast::HangerKind,
    span::{Span, Spanned, Spanning},
    typer::interpreter::{ffi, scope::Registration},
    util::{HashMap, HashSet, SmallVec},
};
use colored::Colorize;
pub use index::{DeBruijnIndex, DeclarationIndex, Index, LocalDeclarationIndex};
use index_map::IndexMap;
use joinery::JoinableIterator;
use smallvec::smallvec;
use std::{cell::RefCell, cmp::Ordering, default::default, fmt, path::PathBuf, usize};
use unicode_width::UnicodeWidthStr;

mod index;

// @Task better docs
/// The crate scope for module-level bindings.
///
/// This structure is used not only by the name resolver but also the type checker.
// @Question rename to just "Crate"?
// @Task add optional field `name` (None means inherited from package)
pub struct CrateScope {
    pub index: CrateIndex,
    pub package: PackageIndex,
    pub path: PathBuf,
    pub type_: CrateType,
    // @Question move??
    pub(crate) program_entry: Option<Identifier>,
    /// All bindings inside of a crate.
    ///
    /// The first element must always be the root module.
    // @Temporary
    pub(crate) bindings: IndexMap<LocalDeclarationIndex, Entity>,
    /// For resolving out of order use-declarations.
    partially_resolved_use_bindings: HashMap<LocalDeclarationIndex, PartiallyResolvedUseBinding>,
    /// For error reporting.
    pub(super) duplicate_definitions: HashMap<LocalDeclarationIndex, DuplicateDefinition>,
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
    pub fn new(index: CrateIndex, package: PackageIndex, path: PathBuf, type_: CrateType) -> Self {
        Self {
            index,
            package,
            path,
            type_,
            program_entry: default(),
            bindings: default(),
            partially_resolved_use_bindings: default(),
            duplicate_definitions: default(),
            health: default(),
            ffi: default(),
            out_of_order_bindings: default(),
        }
    }

    fn dependency(&self, name: &str, crates: &CrateStore) -> Option<CrateIndex> {
        let package = &crates[self.package];
        let dependency = package.dependencies.get(name).copied();

        // @Question should we forbid direct dependencies with the same name as the current package
        // (in a prior step)?
        match self.type_ {
            CrateType::Library => dependency,
            CrateType::Binary => {
                dependency.or_else(|| (name == package.name).then(|| package.library).flatten())
            }
        }
    }

    pub fn global_index(&self, index: LocalDeclarationIndex) -> DeclarationIndex {
        DeclarationIndex::new(self.index, index)
    }

    const DEBUG_INDEX_TRANSLATIONS: bool = false;

    // @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Temporary
    #[track_caller]
    pub fn __temporary_global_index(&self, index: LocalDeclarationIndex) -> DeclarationIndex {
        if Self::DEBUG_INDEX_TRANSLATIONS {
            eprintln!("{:?} ~> {:?}", index, self.global_index(index));
        }
        self.global_index(index)
    }

    pub fn is_local(&self, index: DeclarationIndex) -> bool {
        index.crate_index() == self.index
    }

    pub fn local_index(&self, index: DeclarationIndex) -> Option<LocalDeclarationIndex> {
        self.is_local(index).then(|| index.local_index())
    }

    // @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Temporary
    #[track_caller]
    pub fn __temporary_local_index(&self, index: DeclarationIndex) -> LocalDeclarationIndex {
        if Self::DEBUG_INDEX_TRANSLATIONS {
            eprintln!(
                "{:?} ~> {}",
                index,
                match self.local_index(index) {
                    Some(index) => format!("{index:?}"),
                    None => format!("none (local={:?})", self.index),
                }
            );
        }

        self.local_index(index).unwrap()
    }

    #[track_caller]
    pub fn get(&self, index: LocalDeclarationIndex) -> &Entity {
        if self.bindings.get(index).is_none() {
            eprintln!("{self:#?}");

            eprintln!(
                "get() idx={index:?} cr={:?} #entities={:?}",
                self.index,
                self.bindings.len()
            );
        }

        &self.bindings[index]
    }

    #[track_caller]
    pub fn get_mut(&mut self, index: LocalDeclarationIndex) -> &mut Entity {
        &mut self.bindings[index]
    }

    // @Note bad naming, better naming scheme: get <-> entity
    pub fn entity<'a>(&'a self, index: DeclarationIndex, crates: &'a CrateStore) -> &'a Entity {
        match self.local_index(index) {
            Some(index) => self.get(index),
            None => crates.entity(index),
        }
    }

    fn some_ancestor_equals(
        &self,
        mut index: DeclarationIndex,
        namespace: DeclarationIndex,
    ) -> bool {
        loop {
            if index == namespace {
                break true;
            }

            index = match self.get(self.__temporary_local_index(index)).parent {
                Some(parent) => self.__temporary_global_index(parent),
                None => break false,
            }
        }
    }

    // @Task update/refine the docs
    /// The crate root.
    ///
    /// Unbeknownst to the subdeclarations of the crate, it takes the name
    /// given by external sources, the crate name. Inside the crate, the root
    /// can only ever be referenced through the keyword `crate` (unless renamed).
    pub(super) fn root(&self) -> LocalDeclarationIndex {
        LocalDeclarationIndex::new(0)
    }

    /// Build a textual representation of the absolute path of a binding.
    pub fn absolute_path(&self, index: DeclarationIndex, crates: &CrateStore) -> String {
        self.absolute_path_with_root(index, HangerKind::Crate.name().to_owned(), crates)
    }

    pub fn absolute_path_with_root(
        &self,
        index: DeclarationIndex,
        root: String,
        crates: &CrateStore,
    ) -> String {
        use crate::lexer::token::is_punctuation;

        let index = match self.local_index(index) {
            Some(index) => index,
            None => {
                let crate_ = &crates[index.crate_index()];
                let root = format!(
                    "{}.{}",
                    HangerKind::Crates.name(),
                    crates[crate_.package].name
                );

                return crate_.absolute_path_with_root(index, root, crates);
            }
        };

        let entity = self.get(index);

        if let Some(parent) = entity.parent {
            let mut parent_path =
                self.absolute_path_with_root(self.global_index(parent), root, crates);

            let parent_is_punctuation = is_punctuation(parent_path.chars().next_back().unwrap());

            if parent_is_punctuation {
                parent_path.push(' ');
            }

            parent_path.push('.');

            if entity.source.is_punctuation() && parent_is_punctuation {
                parent_path.push(' ');
            }

            parent_path += entity.source.as_str();
            parent_path
        } else {
            root
        }
    }

    /// Register a binding to a given entity.
    ///
    /// Apart from actually registering, it mainly checks for name duplication.
    pub(super) fn register_binding(
        &mut self,
        binder: ast::Identifier,
        exposure: Exposure,
        binding: EntityKind,
        namespace: Option<LocalDeclarationIndex>,
    ) -> Result<LocalDeclarationIndex, RegistrationError> {
        if let Some(namespace) = namespace {
            // @Temporary let-binding (borrowck bug?)
            let index = self
                .get(namespace)
                .namespace()
                .unwrap()
                .binders
                .iter()
                .map(|&index| self.local_index(index).unwrap())
                .find(|&index| self.get(index).source == binder);

            if let Some(index) = index {
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

        let index = self.bindings.insert(Entity {
            source: binder,
            kind: binding,
            exposure,
            parent: namespace,
        });

        if let Some(namespace) = namespace {
            let index = self.global_index(index);

            self.get_mut(namespace)
                .namespace_mut()
                .unwrap()
                .binders
                .push(index);
        }

        Ok(index)
    }

    pub(super) fn register_use_binding(
        &mut self,
        index: LocalDeclarationIndex,
        reference: Path,
        module: LocalDeclarationIndex,
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
        namespace: DeclarationIndex,
        crates: &CrateStore,
        reporter: &Reporter,
    ) -> Result<Target::Output, ResolutionError> {
        self.resolve_path_with_origin::<Target>(
            path,
            namespace,
            namespace,
            IdentifierUsage::Unqualified,
            CheckExposure::Yes,
            crates,
            reporter,
        )
    }

    // @Task get rid of this concept
    fn resolve_path_unchecked_exposure<Target: ResolutionTarget>(
        &self,
        path: &Path,
        namespace: DeclarationIndex,
        crates: &CrateStore,
        reporter: &Reporter,
    ) -> Result<Target::Output, ResolutionError> {
        self.resolve_path_with_origin::<Target>(
            path,
            namespace,
            namespace,
            IdentifierUsage::Unqualified,
            CheckExposure::No,
            crates,
            reporter,
        )
    }

    /// Resolve a syntactic path given a namespace with an explicit origin.
    // @Task memoize by (path, namespace)
    fn resolve_path_with_origin<Target: ResolutionTarget>(
        &self,
        path: &Path,
        namespace: DeclarationIndex,
        origin_namespace: DeclarationIndex,
        usage: IdentifierUsage,
        check_exposure: CheckExposure,
        crates: &CrateStore,
        reporter: &Reporter,
    ) -> Result<Target::Output, ResolutionError> {
        if let Some(hanger) = &path.hanger {
            use ast::HangerKind::*;

            if path.segments.is_empty() {
                // `crates` already handled in the lowerer (for now)
                Target::handle_bare_path_hanger_except_crates(hanger)
                    .map_err(|error| error.report(reporter))?;
            }

            let namespace = match hanger.kind {
                Crates => {
                    // @Note ugly!
                    let crate_ = &path.segments[0];

                    let crate_ = match self.dependency(crate_.as_str(), crates) {
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
                                .report(reporter);
                            return Err(ResolutionError::Unrecoverable);
                        }
                    };

                    let scope = &crates[crate_];
                    let root = scope.global_index(scope.root());

                    return match &*path.segments {
                        [identifier] => Ok(Target::output(root, identifier)),
                        [_, identifiers @ ..] => scope.resolve_path_with_origin::<Target>(
                            &Path::with_segments(identifiers.to_owned().into()),
                            root,
                            origin_namespace,
                            IdentifierUsage::Qualified,
                            CheckExposure::Yes,
                            crates,
                            reporter,
                        ),
                        [] => unreachable!(),
                    };
                }
                Crate => self.global_index(self.root()),
                Super => self.global_index(self.resolve_super(
                    hanger,
                    self.local_index(namespace).unwrap(),
                    reporter,
                )?),
                Self_ => namespace,
            };

            return self.resolve_path_with_origin::<Target>(
                &path.tail().unwrap(),
                namespace,
                origin_namespace,
                IdentifierUsage::Qualified,
                check_exposure,
                crates,
                reporter,
            );
        }

        let index = self.resolve_path_segment(
            &path.segments[0],
            namespace,
            origin_namespace,
            usage,
            check_exposure,
            crates,
            reporter,
        )?;

        let entity = self.entity(index, crates);

        match &*path.segments {
            [identifier] => {
                Target::handle_simple_path(identifier, entity)
                    .map_err(|error| error.report(reporter))?;
                Ok(Target::output(index, identifier))
            }
            [identifier, identifiers @ ..] => {
                if entity.is_namespace() {
                    self.resolve_path_with_origin::<Target>(
                        &Path::with_segments(identifiers.to_owned().into()),
                        index,
                        origin_namespace,
                        IdentifierUsage::Qualified,
                        check_exposure,
                        crates,
                        reporter,
                    )
                } else if entity.is_error() {
                    // @Task add rationale
                    Ok(Target::output(index, identifiers.last().unwrap()))
                } else {
                    value_used_as_a_namespace(
                        identifier,
                        identifiers.first().unwrap(),
                        namespace,
                        self,
                        crates,
                    )
                    .report(reporter);
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
        reporter: &Reporter,
    ) -> Result<LocalDeclarationIndex> {
        self.get(module).parent.ok_or_else(|| {
            Diagnostic::error()
                .code(Code::E021) // @Question use a dedicated code?
                .message("the crate root does not have a parent")
                .primary_span(hanger)
                .report(reporter)
        })
    }

    fn resolve_path_segment(
        &self,
        identifier: &ast::Identifier,
        namespace: DeclarationIndex,
        origin_namespace: DeclarationIndex,
        usage: IdentifierUsage,
        check_exposure: CheckExposure,
        crates: &CrateStore,
        reporter: &Reporter,
    ) -> Result<DeclarationIndex, ResolutionError> {
        let entity = self.entity(namespace, crates);

        let index = entity
            .namespace()
            .unwrap()
            .binders
            .iter()
            .copied()
            .find(|&index| &self.entity(index, crates).source == identifier)
            .ok_or_else(|| ResolutionError::UnresolvedBinding {
                identifier: identifier.clone(),
                namespace,
                usage,
            })?;

        // @Temporary hack until we can manage cyclic exposure reaches!
        if matches!(check_exposure, CheckExposure::Yes) {
            self.handle_exposure(index, identifier, origin_namespace, crates, reporter)?;
        }
        self.collapse_use_chain(index, crates)
    }

    // @Task verify that the exposure is checked even in the case of use-declarations
    // using use-bindings (use-chains).
    fn handle_exposure(
        &self,
        index: DeclarationIndex,
        identifier: &ast::Identifier,
        origin_namespace: DeclarationIndex,
        crates: &CrateStore,
        reporter: &Reporter,
    ) -> Result<(), ResolutionError> {
        let entity = self.entity(index, crates);

        if let Exposure::Restricted(exposure) = &entity.exposure {
            // unwrap: root always has Exposure::Unrestricted
            let definition_site_namespace = entity.parent.unwrap();
            let reach = RestrictedExposure::resolve(
                exposure,
                self.global_index(definition_site_namespace),
                self,
                crates,
                reporter,
            )?;

            if !self.is_allowed_to_access(
                origin_namespace,
                self.global_index(definition_site_namespace),
                reach,
            ) {
                Diagnostic::error()
                    .code(Code::E029)
                    .message(format!(
                        "binding `{}` is private",
                        self.absolute_path(index, crates)
                    ))
                    .primary_span(identifier)
                    .report(reporter);
                return Err(ResolutionError::Unrecoverable);
            }
        }

        Ok(())
    }

    /// Indicate if the definition-site namespace can be accessed from the given namespace.
    fn is_allowed_to_access(
        &self,
        namespace: DeclarationIndex,
        definition_site_namespace: DeclarationIndex,
        reach: DeclarationIndex,
    ) -> bool {
        let access_from_same_namespace_or_below =
            self.some_ancestor_equals(namespace, definition_site_namespace);

        let access_from_above_in_reach = || self.some_ancestor_equals(namespace, reach);

        access_from_same_namespace_or_below || access_from_above_in_reach()
    }

    pub(super) fn resolve_exposure_reaches(
        &mut self,
        crates: &CrateStore,
        reporter: &Reporter,
    ) -> Result {
        let mut health = Health::Untainted;

        for binding in self.bindings.values() {
            if let Exposure::Restricted(exposure) = &binding.exposure {
                // unwrap: root always has Exposure::Unrestricted
                let definition_site_namespace = binding.parent.unwrap();

                if RestrictedExposure::resolve(
                    exposure,
                    self.__temporary_global_index(definition_site_namespace),
                    self,
                    crates,
                    reporter,
                )
                .is_err()
                {
                    health.taint();
                    continue;
                };
            }

            if let EntityKind::Use { reference } = binding.kind {
                let reference = self.entity(reference, crates);

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
                            reference.exposure.with((self, crates)),
                            binding.exposure.with((self, crates)),
                        ))
                        .report(reporter);
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
    fn find_similarly_named<'a>(
        &'a self,
        queried_identifier: &str,
        predicate: impl for<'f> Fn(&'f Entity) -> bool,
        namespace: DeclarationIndex,
        crates: &'a CrateStore,
    ) -> Option<&'a str> {
        self.entity(namespace, crates)
            .namespace()
            .unwrap()
            .binders
            .iter()
            .map(|&index| self.entity(index, crates))
            .filter(|entity| !entity.is_error() && predicate(entity))
            .map(|entity| entity.source.as_str())
            .find(|identifier| is_similar(identifier, queried_identifier))
    }

    /// Collapse chain of use-bindings aka indirect uses.
    ///
    /// This is an invariant established to make things easier to reason about during resolution.
    fn collapse_use_chain(
        &self,
        index: DeclarationIndex,
        crates: &CrateStore,
    ) -> Result<DeclarationIndex, ResolutionError> {
        use EntityKind::*;

        match self.entity(index, crates).kind {
            Use { reference } => Ok(reference),
            UnresolvedUse => Err(ResolutionError::UnresolvedUseBinding { inquirer: index }),
            _ => Ok(index),
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
        namespace: LocalDeclarationIndex,
        crates: &CrateStore,
    ) -> Target::Output {
        let index = self
            .get(namespace)
            .namespace()
            .unwrap()
            .binders
            .iter()
            .map(|&index| self.local_index(index).unwrap())
            .find(|&index| &self.get(index).source == identifier)
            .unwrap();
        let index = self
            .collapse_use_chain(self.global_index(index), crates)
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
    pub(super) fn resolve_use_bindings(&mut self, crates: &CrateStore, reporter: &Reporter) {
        use ResolutionError::*;

        while !self.partially_resolved_use_bindings.is_empty() {
            let mut partially_resolved_use_bindings = HashMap::default();

            for (&index, item) in self.partially_resolved_use_bindings.iter() {
                match self.resolve_path::<ValueOrModule>(
                    &item.reference,
                    self.global_index(item.module),
                    crates,
                    reporter,
                ) {
                    Ok(reference) => {
                        self.bindings[index].kind = EntityKind::Use { reference };
                    }
                    Err(error @ (UnresolvedBinding { .. } | Unrecoverable)) => {
                        self.bindings[index].mark_as_error();
                        error.report(self, crates, reporter);
                        self.health.taint();
                    }
                    Err(UnresolvedUseBinding { inquirer }) => {
                        partially_resolved_use_bindings.insert(
                            index,
                            PartiallyResolvedUseBinding {
                                reference: item.reference.clone(),
                                module: item.module,
                                inquirer: Some(self.__temporary_local_index(inquirer)),
                            },
                        );
                    }
                }
            }

            // resolution stalled; therefore there are circular bindings
            if partially_resolved_use_bindings.len() == self.partially_resolved_use_bindings.len() {
                for &index in partially_resolved_use_bindings.keys() {
                    self.get_mut(index).mark_as_error();
                }

                for cycle in find_cycles(partially_resolved_use_bindings) {
                    let paths = cycle.iter().map(|&index| {
                        self.absolute_path(self.__temporary_global_index(index), crates)
                            .quote()
                    });
                    let paths = unordered_listing(paths, Conjunction::And);
                    let spans = cycle.iter().map(|&index| self.get(index).source.span);

                    Diagnostic::error()
                        .code(Code::E024)
                        .message(pluralize!(
                            cycle.len(),
                            format!("declaration {paths} is circular"),
                            format!("declarations {paths} are circular"),
                        ))
                        .primary_spans(spans)
                        .report(reporter);
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
                if !visited.contains_key(&index) {
                    let mut worklist = vec![index];
                    visited.insert(index, Status::InProgress);
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
}

// @Temporary
impl fmt::Debug for CrateScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CrateScope")
            .field("index", &self.index)
            .field("package", &self.package)
            .field("path", &self.path)
            .field("type_", &self.type_)
            .field("program_entry", &self.program_entry)
            .field("bindings", &self.bindings.indices().collect::<Vec<_>>())
            .finish()
    }
}

// @Note it would be better if we had `DebugWith`
impl DisplayWith for CrateScope {
    type Context<'a> = &'a CrateStore;

    fn format(&self, crates: &CrateStore, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "{} {} ({:?}) {:?}",
            self.type_, crates[self.package].name, self.index, self.package
        )?;

        writeln!(f, "  bindings:")?;

        for (index, entity) in self.bindings.iter() {
            writeln!(f, "    {index:?}: {}", entity.with((self, crates)))?;
        }

        writeln!(f, "  partially unresolved use-bindings:")?;

        for (index, binding) in &self.partially_resolved_use_bindings {
            writeln!(f, "    {index:?}: {binding:?}")?;
        }

        Ok(())
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
                    RestrictedExposure::Resolved { reach } => write!(f, "{reach:?}"),
                }?;
                write!(f, ")")
            }
        }
    }
}

impl DisplayWith for Exposure {
    type Context<'a> = (&'a CrateScope, &'a CrateStore);

    fn format(
        &self,
        (scope, crates): Self::Context<'_>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        match self {
            Self::Unrestricted => write!(f, "unrestricted"),
            Self::Restricted(reach) => write!(f, "`{}`", reach.borrow().with((scope, crates))),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum RestrictedExposure {
    // @Beacon @Question does it yield an advantage if we make this more fine-grained
    // with the concept of partially resolved restricted exposure reaches
    // i.e. RestrictedExposureReach::PartiallyResolved {
    //     namespace: LocalDeclarationIndex, path: Path }
    // it might help us detect and handle circular expsure reaches better
    Unresolved { reach: Path },
    Resolved { reach: LocalDeclarationIndex },
}

impl RestrictedExposure {
    fn resolve(
        exposure: &RefCell<Self>,
        definition_site_namespace: DeclarationIndex,
        scope: &CrateScope,
        crates: &CrateStore,
        reporter: &Reporter,
    ) -> Result<DeclarationIndex> {
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
                        crates,
                        reporter,
                    )
                    .map_err(|error| error.report(scope, crates, reporter))?;

                let reach_is_ancestor =
                    scope.some_ancestor_equals(definition_site_namespace, reach);

                if !reach_is_ancestor {
                    return Err(Diagnostic::error()
                        .code(Code::E000)
                        .message("exposure can only be restricted to ancestor modules")
                        .primary_span(unresolved_reach)
                        .report(reporter));
                }

                drop(exposure_);
                *exposure.borrow_mut() = RestrictedExposure::Resolved {
                    reach: scope.__temporary_local_index(reach),
                };

                reach
            }
            &Self::Resolved { reach } => scope.__temporary_global_index(reach),
        })
    }

    fn compare(&self, other: &Self, scope: &CrateScope) -> Option<Ordering> {
        use RestrictedExposure::*;

        Some(match (self, other) {
            (&Resolved { reach: this }, &Resolved { reach: other }) => {
                let this = scope.__temporary_global_index(this);
                let other = scope.__temporary_global_index(other);

                if this == other {
                    Ordering::Equal
                } else if scope.some_ancestor_equals(other, this) {
                    Ordering::Greater
                } else if scope.some_ancestor_equals(this, other) {
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
    type Context<'a> = (&'a CrateScope, &'a CrateStore);

    fn format(
        &self,
        (scope, crates): Self::Context<'_>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        match self {
            Self::Unresolved { reach } => write!(f, "{}", reach),
            &Self::Resolved { reach } => write!(
                f,
                "{}",
                scope.absolute_path(scope.__temporary_global_index(reach), crates)
            ),
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

// @Task put all that stuff into a module `resolution_target`
/// Specifies behavior for resolving different sorts of entities.
///
/// Right now, it's only about the difference between values and modules
/// since modules are not values (non-first-class). As such, this trait
/// allows to implementors to define what should happen with the resolved entity
/// if it appears in a specific location
pub(super) trait ResolutionTarget {
    type Output;

    fn output(index: DeclarationIndex, identifier: &ast::Identifier) -> Self::Output;

    fn handle_bare_path_hanger_except_crates(hanger: &ast::Hanger) -> Result<(), Diagnostic>;

    fn handle_simple_path(identifier: &ast::Identifier, entity: &Entity) -> Result<(), Diagnostic>;
}

/// Marker to specify it's okay to resolve to either value or module.
pub(super) enum ValueOrModule {}

impl ResolutionTarget for ValueOrModule {
    type Output = DeclarationIndex;

    fn output(index: DeclarationIndex, _: &ast::Identifier) -> Self::Output {
        index
    }

    fn handle_bare_path_hanger_except_crates(_: &ast::Hanger) -> Result<(), Diagnostic> {
        Ok(())
    }

    fn handle_simple_path(_: &ast::Identifier, _: &Entity) -> Result<(), Diagnostic> {
        Ok(())
    }
}

/// Marker to specify to only resolve to values.
pub(super) enum OnlyValue {}

impl ResolutionTarget for OnlyValue {
    type Output = Identifier;

    fn output(index: DeclarationIndex, identifier: &ast::Identifier) -> Self::Output {
        Identifier::new(index, identifier.clone())
    }

    fn handle_bare_path_hanger_except_crates(hanger: &ast::Hanger) -> Result<(), Diagnostic> {
        Err(module_used_as_a_value(hanger.as_ref()))
    }

    fn handle_simple_path(identifier: &ast::Identifier, entity: &Entity) -> Result<(), Diagnostic> {
        if entity.is_module() {
            return Err(module_used_as_a_value(Spanned::new(
                identifier.span,
                identifier.as_str(),
            )));
        }

        Ok(())
    }
}

pub(super) enum OnlyModule {}

impl ResolutionTarget for OnlyModule {
    type Output = DeclarationIndex;

    fn output(index: DeclarationIndex, _: &ast::Identifier) -> Self::Output {
        index
    }

    fn handle_bare_path_hanger_except_crates(_: &ast::Hanger) -> Result<(), Diagnostic> {
        Ok(())
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

/// If an identifier is used unqualified or qualified.
///
/// Exclusively used for error reporting.
#[derive(Debug)] // @Temporary
#[derive(Clone, Copy)]
enum IdentifierUsage {
    Qualified,
    Unqualified,
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

#[derive(Clone, Default)]
pub struct Namespace {
    pub binders: Vec<DeclarationIndex>,
}

impl fmt::Debug for Namespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.binders
                .iter()
                .map(|binding| format!("{binding:?}"))
                .join_with(' ')
        )
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

    pub fn declaration_index(&self) -> Option<DeclarationIndex> {
        self.index.declaration_index()
    }

    pub fn de_bruijn_index(&self) -> Option<DeBruijnIndex> {
        self.index.de_bruijn_index()
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

pub enum FunctionScope<'a> {
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

    fn module(&self) -> LocalDeclarationIndex {
        match self {
            &Self::Module(module) => module,
            Self::FunctionParameter { parent, .. } | Self::PatternBinders { parent, .. } => {
                parent.module()
            }
        }
    }

    pub fn absolute_path(binder: &Identifier, scope: &CrateScope, crates: &CrateStore) -> String {
        match binder.index {
            Index::Declaration(index) => scope.absolute_path(index, crates),
            Index::DeBruijn(_) | Index::DeBruijnParameter => binder.as_str().into(),
        }
    }

    /// Resolve a binding in a function scope given a depth.
    pub(super) fn resolve_binding(
        &self,
        query: &Path,
        scope: &CrateScope,
        crates: &CrateStore,
        reporter: &Reporter,
    ) -> Result<Identifier> {
        self.resolve_binding_with_depth(query, scope, 0, self, crates, reporter)
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
        crates: &CrateStore,
        reporter: &Reporter,
    ) -> Result<Identifier> {
        use FunctionScope::*;

        // @Note kinda awkward API with map_err
        match self {
            &Module(module) => scope
                .resolve_path::<OnlyValue>(query, scope.global_index(module), crates, reporter)
                .map_err(|error| {
                    error.emit_finding_lookalike(
                        |identifier, _| origin.find_similarly_named(identifier, scope, crates),
                        scope,
                        crates,
                        reporter,
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
                                scope.global_index(self.module()),
                                scope,
                                crates,
                            )
                            .report(reporter));
                        }

                        Ok(Identifier::new(DeBruijnIndex(depth), identifier.clone()))
                    } else {
                        parent.resolve_binding_with_depth(
                            query,
                            scope,
                            depth + 1,
                            origin,
                            crates,
                            reporter,
                        )
                    }
                } else {
                    scope
                        .resolve_path::<OnlyValue>(
                            query,
                            scope.global_index(parent.module()),
                            crates,
                            reporter,
                        )
                        .map_err(|error| error.report(scope, crates, reporter))
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
                                    scope.global_index(self.module()),
                                    scope,
                                    crates,
                                )
                                .report(reporter));
                            }

                            Ok(Identifier::new(DeBruijnIndex(depth), identifier.clone()))
                        }
                        None => parent.resolve_binding_with_depth(
                            query,
                            scope,
                            depth + binders.len(),
                            origin,
                            crates,
                            reporter,
                        ),
                    }
                } else {
                    scope
                        .resolve_path::<OnlyValue>(
                            query,
                            scope.global_index(parent.module()),
                            crates,
                            reporter,
                        )
                        .map_err(|error| error.report(scope, crates, reporter))
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
    fn find_similarly_named<'b>(
        &'b self,
        identifier: &str,
        scope: &'b CrateScope,
        crates: &'b CrateStore,
    ) -> Option<&'b str>
    where
        'a: 'b,
    {
        use FunctionScope::*;

        match self {
            &Module(module) => {
                scope.find_similarly_named(identifier, |_| true, scope.global_index(module), crates)
            }
            FunctionParameter { parent, binder } => {
                if is_similar(identifier, binder.as_str()) {
                    Some(binder.as_str())
                } else {
                    parent.find_similarly_named(identifier, scope, crates)
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
                    parent.find_similarly_named(identifier, scope, crates)
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
        use difference::{Changeset, Difference};

        let changeset = Changeset::new(self.actual, self.lookalike, "");
        let mut purely_additive = true;

        write!(f, "`")?;

        for difference in &changeset.diffs {
            match difference {
                Difference::Same(segment) => write!(f, "{}", segment)?,
                Difference::Add(segment) => write!(f, "{}", segment.bold())?,
                Difference::Rem(_) => {
                    purely_additive = false;
                }
            }
        }

        write!(f, "`")?;

        if !purely_additive && !(self.actual.width() == 1 && changeset.distance == 2) {
            write!(f, " ({})", changeset.auto_colored())?;
        }

        Ok(())
    }
}

// @Question parent: *Local*DeclarationIndex?
fn value_used_as_a_namespace(
    non_namespace: &ast::Identifier,
    subbinder: &ast::Identifier,
    parent: DeclarationIndex,
    scope: &CrateScope,
    crates: &CrateStore,
) -> Diagnostic {
    // @Question should we also include lookalike namespaces that don't contain the
    // subbinding (displaying them in a separate help message?)?
    // @Beacon @Bug this ignores shadowing @Task define this method for FunctionScope
    // @Update @Note actually in the future (lang spec) this kind of shadowing does not
    // occur, the subbinder access pierces through parameters
    let similarly_named_namespace = scope.find_similarly_named(
        non_namespace.as_str(),
        |entity| {
            entity.namespace().map_or(false, |namespace| {
                namespace
                    .binders
                    .iter()
                    .find(|&&index| &scope.entity(index, crates).source == subbinder)
                    .is_some()
            })
        },
        parent,
        crates,
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
        namespace: DeclarationIndex,
        usage: IdentifierUsage,
    },
    UnresolvedUseBinding {
        inquirer: DeclarationIndex,
    },
}

impl ResolutionError {
    fn report(self, scope: &CrateScope, crates: &CrateStore, reporter: &Reporter) {
        self.emit_finding_lookalike(
            |identifier, namespace| {
                scope.find_similarly_named(identifier, |_| true, namespace, crates)
            },
            scope,
            crates,
            reporter,
        );
    }

    fn emit_finding_lookalike<'s>(
        self,
        find_lookalike: impl FnOnce(&str, DeclarationIndex) -> Option<&'s str>,
        scope: &CrateScope,
        crates: &CrateStore,
        reporter: &Reporter,
    ) {
        match self {
            Self::Unrecoverable => {}
            Self::UnresolvedBinding {
                identifier,
                namespace,
                usage,
            } => {
                let mut message = format!("binding `{identifier}` is not defined in ");

                match usage {
                    IdentifierUsage::Unqualified => message += "this scope",
                    IdentifierUsage::Qualified => {
                        message += match scope.entity(namespace, crates).is_module() {
                            true => "module",
                            false => "namespace",
                        };
                        message += " `";
                        message += &scope.absolute_path(namespace, crates);
                        message += "`";
                        // @Beacon @Temporary
                        message += &format!(
                            " ({namespace:?}) [{:?}]",
                            scope.entity(namespace, crates).namespace()
                        );
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
                    .report(reporter);
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
