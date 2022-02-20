//! Scope data structures used for name resolution.
//!
//! The equivalent in the type checker is [`crate::typer::interpreter::scope`].

use crate::{
    diagnostics::{reporter::ErrorReported, Code, Diagnostic},
    entity::{Entity, EntityKind},
    error::Result,
    format::{AsAutoColoredChangeset, DisplayWith},
    hir::{DeclarationIndex, Identifier, Index, LocalDeclarationIndex},
    package::{BuildSession, ComponentIndex, ComponentMetadata, ComponentType, Package},
    span::{Span, Spanned, Spanning},
    syntax::{
        ast::{self, HangerKind, Path},
        lexer::is_punctuation,
        lowered_ast::Attributes,
        Word,
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
/// [^1]: And integration and system tests, benchmarks and other things in the future.
pub struct Component {
    pub metadata: ComponentMetadata,
    pub program_entry: Option<Identifier>,
    /// All bindings inside of the component.
    // The first element has to be the root module.
    pub(crate) bindings: IndexMap<LocalDeclarationIndex, Entity>,
    /// For error reporting.
    pub(super) duplicate_definitions: HashMap<LocalDeclarationIndex, DuplicateDefinition>,
}

impl Component {
    pub(crate) fn new(meta: ComponentMetadata) -> Self {
        Self {
            metadata: meta,
            program_entry: default(),
            bindings: default(),
            duplicate_definitions: default(),
        }
    }

    pub fn name(&self) -> &Word {
        &self.metadata.name
    }

    pub(crate) fn index(&self) -> ComponentIndex {
        self.metadata.index
    }

    pub fn path(&self) -> &std::path::Path {
        &self.metadata.path
    }

    pub fn type_(&self) -> ComponentType {
        self.metadata.type_
    }

    pub fn is_library(&self) -> bool {
        self.type_() == ComponentType::Library
    }

    pub fn is_executable(&self) -> bool {
        self.type_() == ComponentType::Executable
    }

    pub fn package<'s>(&self, session: &'s BuildSession) -> &'s Package {
        &session[self.metadata.package]
    }

    /// Test if this component is the standard library `core`.
    pub fn is_core_library(&self, session: &BuildSession) -> bool {
        self.package(session).is_core() && self.is_library()
    }

    pub fn is_goal(&self, session: &BuildSession) -> bool {
        self.index() == session.goal_component()
    }

    pub fn in_goal_package(&self, session: &BuildSession) -> bool {
        self.metadata.package == session.goal_package()
    }

    pub(super) fn dependency(&self, name: &Word, session: &BuildSession) -> Option<ComponentIndex> {
        let package = self.package(session);
        let dependency = package.dependencies.get(name).copied();

        // @Question should we forbid direct dependencies with the same name as the current package
        // (in a prior step)?
        match self.type_() {
            ComponentType::Library => dependency,
            ComponentType::Executable => {
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

    /// The root module / the component root as a local index.
    #[allow(clippy::unused_self)] // yields a nicer API
    pub(crate) fn local_root(&self) -> LocalDeclarationIndex {
        LocalDeclarationIndex::new(0)
    }

    /// The root module / the component root.
    pub(crate) fn root(&self) -> DeclarationIndex {
        self.local_root().global(self)
    }

    /// The textual representation of the [path][1] of the given binding relative to the root of the current component.
    ///
    /// Rephrased it returns a path that could be used in any expression in any module of the current component
    /// to unambiguously (including shadowing) refer to the given binding ignoring exposure.
    /// If the binding is defined in this component, it will always start with the path hanger `topmost`,
    /// otherwise it will start with `extern` followed by the respective name of the external component.
    ///
    /// # Examples
    ///
    /// `topmost`, `topmost.alpha`, `topmost.gamma.<?//`, `extern.core`, `extern.core.nat.Nat`.
    ///
    /// [1]: crate::syntax::ast::Path
    pub(crate) fn path_to_string(&self, index: DeclarationIndex, session: &BuildSession) -> String {
        self.path_with_root_to_string(index, HangerKind::Topmost.name().to_owned(), session)
    }

    fn path_with_root_to_string(
        &self,
        index: DeclarationIndex,
        root: String,
        session: &BuildSession,
    ) -> String {
        match index.local(self) {
            Some(index) => self.extern_path_with_root_to_string(index, root),
            None => {
                let component = &session[index.component()];
                let root = format!("{}.{}", HangerKind::Extern.name(), component.name());

                component.path_with_root_to_string(index, root, session)
            }
        }
    }

    /// The textual representation of the [path][1] to the given binding relative to a component root
    /// prefixed with name of the corresponding component.
    ///
    /// Rephrased, it returns a path that could be used in any dependent components (reverse dependencies)
    /// to refer to the binding ignoring exposure as long as one would prepend the path hanger `extern`.
    ///
    /// # Examples
    ///
    /// `core.nat.Nat`, `json.parse`.
    ///
    /// [1]: crate::syntax::ast::Path
    pub(crate) fn extern_path_to_string(&self, index: LocalDeclarationIndex) -> String {
        self.extern_path_with_root_to_string(index, self.name().to_string())
    }

    fn extern_path_with_root_to_string(
        &self,
        index: LocalDeclarationIndex,
        root: String,
    ) -> String {
        let entity = &self[index];

        // @Task rewrite this recursive approach to an iterative one!
        if let Some(parent) = entity.parent {
            let mut parent_path = self.extern_path_with_root_to_string(parent, root);

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

    /// The segments of the [extern path](Self::extern_path_to_string) to the given binding.
    pub(crate) fn extern_path_segments(&self, mut index: LocalDeclarationIndex) -> VecDeque<&str> {
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

                // @Task add docs that the unchecked call is "unavoidable"
                return Err(RegistrationError::DuplicateDefinition(
                    ErrorReported::error_will_be_reported_unchecked(),
                ));
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

impl std::ops::Index<LocalDeclarationIndex> for Component {
    type Output = Entity;

    #[track_caller]
    fn index(&self, index: LocalDeclarationIndex) -> &Self::Output {
        &self.bindings[index]
    }
}

impl std::ops::IndexMut<LocalDeclarationIndex> for Component {
    #[track_caller]
    fn index_mut(&mut self, index: LocalDeclarationIndex) -> &mut Self::Output {
        &mut self.bindings[index]
    }
}

// @Note it would be better if we had `DebugWith`
impl DisplayWith for Component {
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
            writeln!(
                f,
                "    {}: {}",
                format!("{index:?}").red(),
                entity.with((self, session))
            )?;
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
    pub(super) fn compare(&self, other: &Self, component: &Component) -> Option<Ordering> {
        use Exposure::*;

        match (self, other) {
            (Unrestricted, Unrestricted) => Some(Ordering::Equal),
            (Unrestricted, Restricted(_)) => Some(Ordering::Greater),
            (Restricted(_), Unrestricted) => Some(Ordering::Less),
            (Restricted(this), Restricted(other)) => this
                .lock()
                .unwrap()
                .compare(&other.lock().unwrap(), component),
        }
    }
}

impl fmt::Debug for Exposure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unrestricted => write!(f, "*"),
            Self::Restricted(reach) => match &*reach.lock().unwrap() {
                RestrictedExposure::Unresolved { reach } => write!(f, "{reach}"),
                RestrictedExposure::Resolved { reach } => write!(f, "{reach:?}"),
            },
        }
    }
}

impl DisplayWith for Exposure {
    type Context<'a> = (&'a Component, &'a BuildSession);

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
    fn compare(&self, other: &Self, component: &Component) -> Option<Ordering> {
        use RestrictedExposure::*;

        Some(match (self, other) {
            (&Resolved { reach: this }, &Resolved { reach: other }) => {
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
            _ => return None,
        })
    }
}

impl DisplayWith for RestrictedExposure {
    type Context<'a> = (&'a Component, &'a BuildSession);

    fn format(
        &self,
        (scope, session): Self::Context<'_>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        match self {
            Self::Unresolved { reach } => write!(f, "{}", reach),
            &Self::Resolved { reach } => {
                write!(f, "{}", scope.path_to_string(reach.global(scope), session))
            }
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
            "{}",
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

    /// The textual representation of the [path][1] of the given binding.
    ///
    /// If the binding is local meaning it represents a function parameter or a binder in a pattern,
    /// this method will return a single identifier that could be used in the corresponding function or
    /// case analysis case to refer to it until the point it gets shadowed (if any) but not outside of
    /// that environment.
    ///
    /// If it's not local, this method will return
    /// [the path relative to the root of the current component][2].
    ///
    /// # Examples
    ///
    /// `x`, `alpha`, `topmost`, `topmost.alpha`, `topmost.gamma.<?//`, `extern.core`,
    /// `extern.core.nat.Nat`.
    ///
    /// [1]: crate::syntax::ast::Path
    /// [2]: Component::path_to_string
    pub(crate) fn path_to_string(
        binder: &Identifier,
        component: &Component,
        session: &BuildSession,
    ) -> String {
        match binder.index {
            Index::Declaration(index) => component.path_to_string(index, session),
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
        // @Task improve this diagnostic!
        Self::error()
            .code(Code::E023)
            .message(format!("module `{module}` is used as a value"))
            .primary_span(module)
            .help("modules are not first-class citizens, consider utilizing records for such cases instead")
    }
}

pub(super) enum RegistrationError {
    Unrecoverable(ErrorReported),
    /// Duplicate definitions found.
    ///
    /// Details about this error are **not stored here** but as
    /// [DuplicateDefinition]s in the [super::Resolver] so they can
    /// be easily grouped.
    DuplicateDefinition(ErrorReported),
}

impl RegistrationError {
    pub(super) fn token(self) -> ErrorReported {
        match self {
            Self::Unrecoverable(token) | Self::DuplicateDefinition(token) => token,
        }
    }
}

impl From<ErrorReported> for RegistrationError {
    fn from(token: ErrorReported) -> Self {
        Self::Unrecoverable(token)
    }
}
