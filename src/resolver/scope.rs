//! Scope data structures used for name resolution.
//!
//! There are two types of scopes:
//!
//! * [`Crate`] for module-level bindings (may be out of order and recursive)
//!   i.e. bindings defined by declarations in modules, data types and constructors
//! * [`FunctionScope`] for bindings defined inside of expressions or functions (order matters)
//!   like parameters, let/in-binders and case-analysis binders
//!
//! The equivalent in the type checker is [`crate::typer::interpreter::scope`].

// @Beacon @Beacon @Task don't report similarily named *private* bindings!
// @Task recognize leaks of private types!

use crate::{
    diagnostics::{Code, Diagnostic},
    entity::{Entity, EntityKind},
    error::{Health, Result},
    format::{AsAutoColoredChangeset, DisplayWith},
    package::{BuildSession, CrateIndex, CrateType, PackageIndex},
    span::{Span, Spanned, Spanning},
    syntax::{
        ast::{self, HangerKind, Path},
        lexer::is_punctuation,
        CrateName,
    },
    typer::interpreter::scope::BindingRegistration,
    utility::{HashMap, SmallVec},
};
use colored::Colorize;
pub use index::{DeBruijnIndex, DeclarationIndex, Index, LocalDeclarationIndex};
use index_map::IndexMap;
use joinery::JoinableIterator;
use smallvec::smallvec;
use std::{
    cell::RefCell, cmp::Ordering, collections::VecDeque, default::default, fmt, path::PathBuf,
    usize,
};
use unicode_width::UnicodeWidthStr;

mod index;

// @Task better docs
/// The crate scope for module-level bindings.
///
/// This structure is used not only by the name resolver but also the type checker.
// @Task move out of this module
pub struct Crate {
    pub index: CrateIndex,
    pub package: PackageIndex,
    pub path: PathBuf,
    pub type_: CrateType,
    pub program_entry: Option<Identifier>,
    /// All bindings inside of the crate.
    // The first element has to be the root module.
    pub(crate) bindings: IndexMap<LocalDeclarationIndex, Entity>,
    /// For resolving out of order use-declarations.
    pub(super) partially_resolved_use_bindings:
        HashMap<LocalDeclarationIndex, PartiallyResolvedUseBinding>,
    /// For error reporting.
    pub(super) duplicate_definitions: HashMap<LocalDeclarationIndex, DuplicateDefinition>,
    // @Note this is very coarse-grained: as soon as we cannot resolve EITHER type annotation (for example)
    // OR actual value(s), we bail out and add this here. This might be too conversative (leading to more
    // "circular type" errors or whatever), we can just discriminate by creating sth like
    // UnresolvedThingy/WorlistItem { index: CrateIndex, expression: TypeAnnotation|Value|Both|... }
    // for the typer only!
    pub(crate) out_of_order_bindings: Vec<BindingRegistration>,
    pub(super) health: Health,
}

impl Crate {
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
            out_of_order_bindings: default(),
        }
    }

    /// Test if this crate is the standard library `core`.
    pub fn is_core_library(&self, session: &BuildSession) -> bool {
        session[self.package].is_core() && self.type_ == CrateType::Library
    }

    pub(super) fn dependency(
        &self,
        name: &CrateName,
        session: &BuildSession,
    ) -> Option<CrateIndex> {
        let package = &session[self.package];
        let dependency = package.dependencies.get(name).copied();

        // @Question should we forbid direct dependencies with the same name as the current package
        // (in a prior step)?
        match self.type_ {
            CrateType::Library => dependency,
            CrateType::Binary => {
                dependency.or_else(|| (name == &package.name).then(|| package.library).flatten())
            }
        }
    }

    pub fn global_index(&self, index: LocalDeclarationIndex) -> DeclarationIndex {
        DeclarationIndex::new(self.index, index)
    }

    pub fn is_local(&self, index: DeclarationIndex) -> bool {
        index.crate_index() == self.index
    }

    pub fn local_index(&self, index: DeclarationIndex) -> Option<LocalDeclarationIndex> {
        self.is_local(index).then(|| index.local_index())
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
            index = match self[self.local_index(index).unwrap()].parent {
                Some(parent) => self.global_index(parent),
                None => break false,
            }
        }
    }

    /// The crate root aka `crate`.
    #[allow(clippy::unused_self)] // nicer API
    pub fn root(&self) -> LocalDeclarationIndex {
        LocalDeclarationIndex::new(0)
    }

    pub fn unhanged_local_path(&self, mut index: LocalDeclarationIndex) -> Path {
        let mut segments = VecDeque::default();

        loop {
            let entity = &self[index];

            if let Some(parent) = entity.parent {
                segments.push_front(entity.source.clone());
                index = parent;
            } else {
                break;
            }
        }

        Path::with_segments(Vec::from(segments).into())
    }

    /// Build a textual representation of the absolute path of a binding.
    pub fn display_absolute_path(&self, index: DeclarationIndex, session: &BuildSession) -> String {
        self.display_absolute_path_with_root(index, HangerKind::Crate.name().to_owned(), session)
    }

    pub fn display_absolute_path_with_root(
        &self,
        index: DeclarationIndex,
        root: String,
        session: &BuildSession,
    ) -> String {
        let Some(index) = self.local_index(index) else {
            let crate_ = &session[index.crate_index()];
            let root = format!(
                "{}.{}",
                HangerKind::Extern.name(),
                session[crate_.package].name,
            );

            return crate_.display_absolute_path_with_root(index, root, session);
        };

        let entity = &self[index];

        // @Task transform into a while loop w/o recursion
        if let Some(parent) = entity.parent {
            let mut parent_path =
                self.display_absolute_path_with_root(self.global_index(parent), root, session);

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
            let index = self[namespace]
                .namespace()
                .unwrap()
                .binders
                .iter()
                .map(|&index| self.local_index(index).unwrap())
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
            parent: namespace,
        });

        if let Some(namespace) = namespace {
            let index = self.global_index(index);

            self[namespace].namespace_mut().unwrap().binders.push(index);
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

impl std::ops::Index<LocalDeclarationIndex> for Crate {
    type Output = Entity;

    #[track_caller]
    fn index(&self, index: LocalDeclarationIndex) -> &Self::Output {
        &self.bindings[index]
    }
}

impl std::ops::IndexMut<LocalDeclarationIndex> for Crate {
    #[track_caller]
    fn index_mut(&mut self, index: LocalDeclarationIndex) -> &mut Self::Output {
        &mut self.bindings[index]
    }
}

// @Note it would be better if we had `DebugWith`
impl DisplayWith for Crate {
    type Context<'a> = &'a BuildSession;

    fn format(&self, session: &BuildSession, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "{} {} ({:?}) {:?}",
            self.type_, session[self.package].name, self.index, self.package
        )?;

        writeln!(f, "  bindings:")?;

        for (index, entity) in self.bindings.iter() {
            writeln!(f, "    {index:?}: {}", entity.with((self, session)))?;
        }

        writeln!(f, "  partially unresolved use-bindings:")?;

        for (index, binding) in &self.partially_resolved_use_bindings {
            writeln!(f, "    {index:?}: {binding:?}")?;
        }

        Ok(())
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Exposure {
    Unrestricted,
    Restricted(RefCell<RestrictedExposure>),
}

impl Exposure {
    pub(super) fn compare(&self, other: &Self, crate_: &Crate) -> Option<Ordering> {
        use Exposure::*;

        match (self, other) {
            (Unrestricted, Unrestricted) => Some(Ordering::Equal),
            (Unrestricted, Restricted(_)) => Some(Ordering::Greater),
            (Restricted(_), Unrestricted) => Some(Ordering::Less),
            (Restricted(this), Restricted(other)) => this.borrow().compare(&other.borrow(), crate_),
        }
    }
}

impl fmt::Debug for Exposure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unrestricted => write!(f, "unrestricted"),
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
    type Context<'a> = (&'a Crate, &'a BuildSession);

    fn format(&self, context: Self::Context<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unrestricted => write!(f, "unrestricted"),
            Self::Restricted(reach) => write!(f, "`{}`", reach.borrow().with(context)),
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
    fn compare(&self, other: &Self, crate_: &Crate) -> Option<Ordering> {
        use RestrictedExposure::*;

        Some(match (self, other) {
            (&Resolved { reach: this }, &Resolved { reach: other }) => {
                let this = crate_.global_index(this);
                let other = crate_.global_index(other);

                if this == other {
                    Ordering::Equal
                } else if crate_.some_ancestor_equals(other, this) {
                    Ordering::Greater
                } else if crate_.some_ancestor_equals(this, other) {
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
    type Context<'a> = (&'a Crate, &'a BuildSession);

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
                scope.display_absolute_path(scope.global_index(reach), session)
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

pub(super) struct PartiallyResolvedUseBinding {
    pub(super) reference: Path,
    pub(super) module: LocalDeclarationIndex,
    // @Beacon @Question local or global??
    pub(super) inquirer: Option<LocalDeclarationIndex>,
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
    pub fn new(index: impl Into<Index>, source: ast::Identifier) -> Self {
        Self {
            index: index.into(),
            source,
        }
    }

    pub(crate) fn parameter(name: &str) -> Self {
        Identifier::new(
            Index::DeBruijnParameter,
            ast::Identifier::new_unchecked(name.into(), default()),
        )
    }

    pub fn as_str(&self) -> &str {
        self.source.as_str()
    }

    pub fn to_expression(self) -> crate::hir::Expression {
        crate::hir::expr! {
            Binding {
                default(), self.span();
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
        self.source.span()
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.source)?;
        if crate::OPTIONS
            .get()
            .map_or(false, |options| options.show_indices)
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

    pub(super) fn module(&self) -> LocalDeclarationIndex {
        match self {
            &Self::Module(module) => module,
            Self::FunctionParameter { parent, .. } | Self::PatternBinders { parent, .. } => {
                parent.module()
            }
        }
    }

    pub fn display_absolute_path(
        binder: &Identifier,
        crate_: &Crate,
        session: &BuildSession,
    ) -> String {
        match binder.index {
            Index::Declaration(index) => crate_.display_absolute_path(index, session),
            Index::DeBruijn(_) | Index::DeBruijnParameter => binder.as_str().into(),
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
