//! Scope data structures used for name resolution.
//!
//! The equivalent in the type checker is [`crate::typer::interpreter::scope`].

// @Beacon @Beacon @Task don't report similarily named *private* bindings!

use crate::{
    diagnostics::{Code, Diagnostic},
    entity::{Entity, EntityKind},
    error::Result,
    format::{AsAutoColoredChangeset, DisplayWith},
    hir::{DeclarationIndex, Identifier, Index, LocalDeclarationIndex},
    package::{BuildSession, CapsuleIndex, CapsuleMetadata, CapsuleType, Package},
    span::{Span, Spanned, Spanning},
    syntax::{
        ast::{self, HangerKind, Path},
        lexer::is_punctuation,
        lowered_ast::Attributes,
        CapsuleName,
    },
    utility::{HashMap, SmallVec},
};
use colored::Colorize;
use index_map::IndexMap;
use joinery::JoinableIterator;
use smallvec::smallvec;
use std::{
    cmp::Ordering,
    collections::VecDeque,
    default::default,
    fmt,
    sync::{Arc, Mutex},
};
use unicode_width::UnicodeWidthStr;

/// A sealed container of modules regarded as one unit embodying libraries and executables[^1].
///
/// # Naming
///
/// A _capsule_ means (among other things) _a compact often sealed and detachable container
/// or compartment_
/// [according to Merriam-Webster (meaning 6a, 2022-01-10)](https://www.merriam-webster.com/dictionary/capsule).
///
/// [^1]: And integration and system tests, benchmarks and other things in the future.
pub struct Capsule {
    pub metadata: CapsuleMetadata,
    pub program_entry: Option<Identifier>,
    /// All bindings inside of the capsule.
    // The first element has to be the root module.
    pub(crate) bindings: IndexMap<LocalDeclarationIndex, Entity>,
    /// For error reporting.
    pub(super) duplicate_definitions: HashMap<LocalDeclarationIndex, DuplicateDefinition>,
}

impl Capsule {
    pub(crate) fn new(meta: CapsuleMetadata) -> Self {
        Self {
            metadata: meta,
            program_entry: default(),
            bindings: default(),
            duplicate_definitions: default(),
        }
    }

    pub fn name(&self) -> &CapsuleName {
        &self.metadata.name
    }

    pub(crate) fn index(&self) -> CapsuleIndex {
        self.metadata.index
    }

    pub fn path(&self) -> &std::path::Path {
        &self.metadata.path
    }

    pub fn type_(&self) -> CapsuleType {
        self.metadata.type_
    }

    pub fn is_library(&self) -> bool {
        self.type_() == CapsuleType::Library
    }

    pub fn is_executable(&self) -> bool {
        self.type_() == CapsuleType::Executable
    }

    pub fn package<'s>(&self, session: &'s BuildSession) -> &'s Package {
        &session[self.metadata.package]
    }

    /// Test if this capsule is the standard library `core`.
    pub fn is_core_library(&self, session: &BuildSession) -> bool {
        self.package(session).is_core() && self.is_library()
    }

    pub fn is_goal(&self, session: &BuildSession) -> bool {
        self.index() == session.goal_capsule()
    }

    pub fn in_goal_package(&self, session: &BuildSession) -> bool {
        self.metadata.package == session.goal_package()
    }

    pub(super) fn dependency(
        &self,
        name: &CapsuleName,
        session: &BuildSession,
    ) -> Option<CapsuleIndex> {
        let package = self.package(session);
        let dependency = package.dependencies.get(name).copied();

        // @Question should we forbid direct dependencies with the same name as the current package
        // (in a prior step)?
        match self.type_() {
            CapsuleType::Library => dependency,
            CapsuleType::Executable => {
                dependency.or_else(|| (name == &package.name).then(|| package.library).flatten())
            }
        }
    }

    // @Beacon @Question shouldn't `index` be a LocalDeclarationIndex?
    pub(super) fn some_ancestor_equals(
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

    /// Get the capsule root as a local index.
    #[allow(clippy::unused_self)] // nicer API
    pub(crate) fn local_root(&self) -> LocalDeclarationIndex {
        LocalDeclarationIndex::new(0)
    }

    /// Get the capsule root.
    pub(crate) fn root(&self) -> DeclarationIndex {
        self.local_root().global(self)
    }

    /// Build a textual representation of the absolute path of a binding.
    // @Task extend docs
    pub(crate) fn absolute_path_to_string(
        &self,
        index: DeclarationIndex,
        session: &BuildSession,
    ) -> String {
        self.absolute_path_with_root_to_string(
            index,
            HangerKind::Capsule.name().to_owned(),
            session,
        )
    }

    fn absolute_path_with_root_to_string(
        &self,
        index: DeclarationIndex,
        root: String,
        session: &BuildSession,
    ) -> String {
        match index.local(self) {
            Some(index) => self.local_path_with_root_to_string(index, root),
            None => {
                let capsule = &session[index.capsule()];
                let root = format!("{}.{}", HangerKind::Extern.name(), capsule.name());

                capsule.absolute_path_with_root_to_string(index, root, session)
            }
        }
    }

    // @Note bad name
    // @Task add docs
    pub(crate) fn local_path_to_string(&self, index: LocalDeclarationIndex) -> String {
        self.local_path_with_root_to_string(index, self.name().to_string())
    }

    // @Note bad name
    fn local_path_with_root_to_string(&self, index: LocalDeclarationIndex, root: String) -> String {
        let entity = &self[index];

        // @Task rewrite this recursive approach to an iterative one!
        if let Some(parent) = entity.parent {
            let mut parent_path = self.local_path_with_root_to_string(parent, root);

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

    // @Note bad name
    pub(crate) fn local_path_segments(&self, mut index: LocalDeclarationIndex) -> VecDeque<&str> {
        let mut segments = VecDeque::new();

        loop {
            let entity = &self[index];
            segments.push_front(entity.source.as_str());

            if let Some(parent) = entity.parent {
                index = parent;
            } else {
                break;
            }
        }

        segments
    }

    /// Register a binding to a given entity.
    ///
    /// Apart from actually registering, it mainly checks for name duplication.
    pub(super) fn register_binding(
        &mut self,
        binder: ast::Identifier,
        exposure: Exposure,
        attributes: Attributes,
        binding: EntityKind,
        namespace: Option<LocalDeclarationIndex>,
    ) -> Result<LocalDeclarationIndex, RegistrationError> {
        if let Some(namespace) = namespace {
            // @Temporary let-binding (borrowck bug?)
            let index = self[namespace]
                .namespace()
                .unwrap()
                .binders
                .iter()
                .map(|&index| index.local(self).unwrap())
                .find(|&index| self[index].source == binder);

            if let Some(index) = index {
                let previous = &self.bindings[index].source;

                self.duplicate_definitions
                    .entry(index)
                    .or_insert(DuplicateDefinition {
                        binder: previous.to_string(),
                        occurrences: smallvec![previous.span()],
                    })
                    .occurrences
                    .push(binder.span());

                return Err(RegistrationError::DuplicateDefinition);
            }
        }

        let index = self.bindings.insert(Entity {
            source: binder,
            kind: binding,
            exposure,
            attributes,
            parent: namespace,
        });

        if let Some(namespace) = namespace {
            let index = index.global(self);
            self[namespace].namespace_mut().unwrap().binders.push(index);
        }

        Ok(index)
    }

    /// Indicate if the definition-site namespace can be accessed from the given namespace.
    pub(super) fn is_allowed_to_access(
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
}

impl std::ops::Index<LocalDeclarationIndex> for Capsule {
    type Output = Entity;

    #[track_caller]
    fn index(&self, index: LocalDeclarationIndex) -> &Self::Output {
        &self.bindings[index]
    }
}

impl std::ops::IndexMut<LocalDeclarationIndex> for Capsule {
    #[track_caller]
    fn index_mut(&mut self, index: LocalDeclarationIndex) -> &mut Self::Output {
        &mut self.bindings[index]
    }
}

// @Note it would be better if we had `DebugWith`
impl DisplayWith for Capsule {
    type Context<'a> = &'a BuildSession;

    fn format(&self, session: &BuildSession, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "{} {} ({:?}) {:?}",
            self.type_(),
            self.name(),
            self.index(),
            self.metadata.package
        )?;

        writeln!(f, "  bindings:")?;

        for (index, entity) in self.bindings.iter() {
            writeln!(f, "    {index:?}: {}", entity.with((self, session)))?;
        }

        Ok(())
    }
}

#[derive(Clone)]
pub(crate) enum Exposure {
    Unrestricted,
    // @Task find a way to get rid of interior mutability here
    Restricted(Arc<Mutex<RestrictedExposure>>),
}

impl Exposure {
    pub(super) fn compare(&self, other: &Self, capsule: &Capsule) -> Option<Ordering> {
        use Exposure::*;

        match (self, other) {
            (Unrestricted, Unrestricted) => Some(Ordering::Equal),
            (Unrestricted, Restricted(_)) => Some(Ordering::Greater),
            (Restricted(_), Unrestricted) => Some(Ordering::Less),
            (Restricted(this), Restricted(other)) => this
                .lock()
                .unwrap()
                .compare(&other.lock().unwrap(), capsule),
        }
    }
}

impl fmt::Debug for Exposure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unrestricted => write!(f, "unrestricted"),
            Self::Restricted(reach) => {
                write!(f, "restricted(")?;
                match &*reach.lock().unwrap() {
                    RestrictedExposure::Unresolved { reach } => write!(f, "{reach}"),
                    RestrictedExposure::Resolved { reach } => write!(f, "{reach:?}"),
                }?;
                write!(f, ")")
            }
        }
    }
}

impl DisplayWith for Exposure {
    type Context<'a> = (&'a Capsule, &'a BuildSession);

    fn format(&self, context: Self::Context<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unrestricted => write!(f, "unrestricted"),
            Self::Restricted(reach) => write!(f, "`{}`", reach.lock().unwrap().with(context)),
        }
    }
}

impl PartialEq for Exposure {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Restricted(l0), Self::Restricted(r0)) => {
                *l0.lock().unwrap() == *r0.lock().unwrap()
            }
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Eq for Exposure {}

#[derive(Clone, PartialEq, Eq)]
pub(crate) enum RestrictedExposure {
    // @Beacon @Question does it yield an advantage if we make this more fine-grained
    // with the concept of partially resolved restricted exposure reaches
    // i.e. RestrictedExposureReach::PartiallyResolved {
    //     namespace: LocalDeclarationIndex, path: Path }
    // it might help us detect and handle circular expsure reaches better
    Unresolved { reach: Path },
    Resolved { reach: LocalDeclarationIndex },
}

impl RestrictedExposure {
    fn compare(&self, other: &Self, capsule: &Capsule) -> Option<Ordering> {
        use RestrictedExposure::*;

        Some(match (self, other) {
            (&Resolved { reach: this }, &Resolved { reach: other }) => {
                let this = this.global(capsule);
                let other = other.global(capsule);

                if this == other {
                    Ordering::Equal
                } else if capsule.some_ancestor_equals(other, this) {
                    Ordering::Greater
                } else if capsule.some_ancestor_equals(this, other) {
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
    type Context<'a> = (&'a Capsule, &'a BuildSession);

    fn format(
        &self,
        (scope, session): Self::Context<'_>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        match self {
            Self::Unresolved { reach } => write!(f, "{}", reach),
            &Self::Resolved { reach } => write!(
                f,
                "{}",
                scope.absolute_path_to_string(reach.global(scope), session)
            ),
        }
    }
}

impl From<RestrictedExposure> for Exposure {
    fn from(exposure: RestrictedExposure) -> Self {
        Self::Restricted(Arc::new(Mutex::new(exposure)))
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

#[derive(Clone, Default)]
pub(crate) struct Namespace {
    pub(crate) binders: Vec<DeclarationIndex>,
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

pub(crate) enum FunctionScope<'a> {
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

    pub(super) fn module(&self) -> LocalDeclarationIndex {
        match self {
            &Self::Module(module) => module,
            Self::FunctionParameter { parent, .. } | Self::PatternBinders { parent, .. } => {
                parent.module()
            }
        }
    }

    pub(crate) fn absolute_path_to_string(
        binder: &Identifier,
        capsule: &Capsule,
        session: &BuildSession,
    ) -> String {
        match binder.index {
            Index::Declaration(index) => capsule.absolute_path_to_string(index, session),
            Index::DeBruijn(_) | Index::DeBruijnParameter => binder.to_string(),
        }
    }
}

pub(super) fn is_similar(identifier: &str, other_identifier: &str) -> bool {
    strsim::levenshtein(other_identifier, identifier) <= std::cmp::max(identifier.len(), 3) / 3
}

pub(super) struct Lookalike<'a> {
    pub(super) actual: &'a str,
    pub(super) lookalike: &'a str,
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

        if !(purely_additive || self.actual.width() == 1 && changeset.distance == 2) {
            write!(f, " ({})", changeset.auto_colored())?;
        }

        Ok(())
    }
}

impl Diagnostic {
    pub(super) fn module_used_as_a_value(module: Spanned<impl fmt::Display>) -> Self {
        // @Task levenshtein-search for similar named bindings which are in fact values and suggest the first one
        // @Task print absolute path of the module in question and use highlight the entire path, not just the last
        // segment
        Self::error()
            .code(Code::E023)
            .message(format!("module `{module}` is used as a value"))
            .primary_span(module)
            .help("modules are not first-class citizens, consider utilizing records for such cases instead")
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
