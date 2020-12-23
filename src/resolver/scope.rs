//! Scope data structures used for name resolution.
//!
//! There are two types of scopes:
//!
//! * [CrateScope] for module-level bindings (may be out of order and recursive)
//!   i.e. bindings defined by declarations in modules, data types and constructors
//! * [FunctionScope] for bindings defined inside of expressions or functions (order matters)
//!   like parameters, let/in binders and case analysis binders
//!
//! The most important functions are [FunctionScope::resolve_binding] and [CrateScope::resolve_path]
//! (used by `resolve_binding`).
//!
//! The equivalent in the type checker is [crate::typer::interpreter::scope].

use crate::{
    ast::{self, Path},
    diagnostics::{Code, Diagnostic, Diagnostics, Result, Results},
    entity::{Entity, EntityKind},
    smallvec,
    span::{Span, Spanning},
    support::{
        unordered_listing, AsDebug, AsDisplay, Conjunction, DisplayWith, ManyErrExt, QuoteExt,
    },
    typer::interpreter::{ffi, scope::Registration},
    HashMap, HashSet, SmallVec,
};
use indexed_vec::IndexVec;
use joinery::JoinableIterator;
use std::{fmt, iter::once};

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
    /// For resolving out of order use declarations.
    unresolved_uses: HashMap<CrateIndex, UnresolvedUse>,
    /// Used for grouping circular bindings in diagnostics
    // @Question can we smh unify this with `unresolved_uses`?
    unresolved_uses_grouped: Vec<HashSet<CrateIndex>>,
    /// For error reporting.
    pub(super) duplicate_definitions: HashMap<CrateIndex, DuplicateDefinition>,
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
    // @Task handle punctuation segments correctly!
    pub fn absolute_path(&self, index: CrateIndex) -> String {
        let entity = &self.bindings[index];
        if let Some(parent) = entity.parent {
            let mut parent = self.absolute_path(parent);
            parent.push('.');
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

    pub(super) fn register_unresolved_use(
        &mut self,
        index: CrateIndex,
        reference: Path,
        module: CrateIndex,
    ) {
        let previous = self
            .unresolved_uses
            .insert(index, UnresolvedUse { reference, module });

        debug_assert!(previous.is_none());
    }

    /// Resolve a syntactic path given a namespace.
    fn resolve_path<Target: ResolutionTarget>(
        &self,
        path: &Path,
        namespace: CrateIndex,
        inquirer: Inquirer,
    ) -> Result<Target::Output, ResolutionError> {
        use ast::HangerKind::*;

        if let Some(hanger) = &path.hanger {
            if path.segments.is_empty() {
                return Target::resolve_bare_super_and_crate(hanger.span, namespace)
                    .map_err(Into::into);
            }

            return self.resolve_path::<Target>(
                &path.tail(),
                match hanger.kind {
                    Crate => self.root(),
                    Super => self.resolve_super(hanger, namespace)?,
                    Self_ => namespace,
                },
                Inquirer::System,
            );
        }

        let index = self.resolve_first_segment(&path.segments[0], namespace, inquirer)?;

        match path.to_simple() {
            Some(identifier) => {
                Target::resolve_simple_path(identifier, &self.bindings[index].kind, index)
                    .map_err(Into::into)
            }
            None => match self.bindings[index].namespace() {
                Some(_) => self.resolve_path::<Target>(&path.tail(), index, Inquirer::System),
                None => Err(value_used_as_a_namespace(&path.segments[0], &path.segments[1]).into()),
            },
        }
    }

    fn resolve_super(&self, hanger: &ast::Hanger, module: CrateIndex) -> Result<CrateIndex> {
        self.bindings[module].parent.ok_or_else(|| {
            Diagnostic::error()
                .with_code(Code::E021)
                .with_message("the crate root does not have a parent")
                .with_primary_span(hanger)
        })
    }

    /// Resolve the first identifier segment of a path.
    fn resolve_first_segment(
        &self,
        identifier: &ast::Identifier,
        namespace: CrateIndex,
        inquirer: Inquirer,
    ) -> Result<CrateIndex, ResolutionError> {
        self.bindings[namespace]
            .namespace()
            .unwrap()
            .bindings
            .iter()
            .find(|&&index| &self.bindings[index].source == identifier)
            .ok_or_else(|| ResolutionError::UnresolvedBinding {
                identifier: identifier.clone(),
                namespace,
                inquirer,
            })
            .and_then(|&index| self.collapse_use_chains(index))
    }

    fn find_similarly_named(&self, identifier: &str, namespace: &Namespace) -> Option<&str> {
        namespace
            .bindings
            .iter()
            .map(|&index| self.bindings[index].source.as_str())
            .find(|&other_identifier| is_similar(identifier, other_identifier))
    }

    /// Collapse chains of use bindings aka indirect uses.
    ///
    /// This is an invariant established to to make things easier to reason about during resolution.
    fn collapse_use_chains(&self, index: CrateIndex) -> Result<CrateIndex, ResolutionError> {
        use EntityKind::*;

        match self.bindings[index].kind {
            UntypedValue | Module(_) | UntypedDataType(_) | UntypedConstructor(_) => Ok(index),
            Use(reference) => Ok(reference),
            UnresolvedUse => Err(ResolutionError::UnresolvedUseBinding {
                inquirer_index: index,
            }),
            _ => unreachable!(),
        }
    }

    /// Resolve a single identifier (in contrast to a whole path).
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
    pub(super) fn resolve_identifier<Target: ResolutionTarget>(
        &self,
        identifier: &ast::Identifier,
        module: CrateIndex,
    ) -> Result<Target::Output> {
        let index = self
            .resolve_first_segment(identifier, module, Inquirer::System)
            .map_err(|error| error.diagnostic(self).unwrap())?;
        Target::resolve_simple_path(identifier, &self.bindings[index].kind, index)
    }

    /// Resolve unresolved uses.
    ///
    /// This is the second pass of three of the name resolver.
    ///
    /// This uses a queue to resolve use bindings over and over until
    /// all out of order use bindings are successfully resolved or until
    /// no progress can be made anymore in which case all remaining
    /// use bindings are actually circular and are thus reported.
    pub(super) fn resolve_unresolved_uses(&mut self) -> Results<()> {
        while !self.unresolved_uses.is_empty() {
            let mut unresolved_uses = HashMap::default();

            for (&index, item) in self.unresolved_uses.iter() {
                match self.resolve_path::<ValueOrModule>(
                    &item.reference,
                    item.module,
                    Inquirer::User,
                ) {
                    Ok(reference) => {
                        self.bindings[index].kind = EntityKind::Use(reference);
                    }
                    Err(
                        error
                        @
                        (ResolutionError::UnresolvedBinding { .. }
                        | ResolutionError::Unrecoverable(_)),
                    ) => return Err(error.diagnostic(self).unwrap()).many_err(),
                    Err(ResolutionError::UnresolvedUseBinding {
                        inquirer_index: _og_index,
                    }) => {
                        unresolved_uses.insert(
                            index,
                            UnresolvedUse {
                                reference: item.reference.clone(),
                                module: item.module,
                            },
                        );
                        // @Beacon @Beacon @Question would it change if we were
                        // to add it to a intially empty local definition `unresolved_uses_grouped`
                        // just like `unresolved_uses` (c.f. `self.unresolved_uses`)
                        // and adding to it and updating it after the "stalling" check
                        // does it remove non-circular bindings over time? or is it just like
                        // today's version?? check this with playground/somecirc.lushui
                        if let Some(indices) = self
                            .unresolved_uses_grouped
                            .iter_mut()
                            .filter(|indices| indices.contains(&_og_index))
                            .next()
                        {
                            indices.insert(index);
                        } else {
                            let mut indices = HashSet::default();
                            indices.insert(index);
                            self.unresolved_uses_grouped.push(indices);
                        }
                    }
                }
            }

            // resolution stalled; therefore there are circular bindings
            if unresolved_uses.len() == self.unresolved_uses.len() {
                return Err(self
                    .unresolved_uses_grouped
                    .iter()
                    .filter(|indices| {
                        indices
                            .iter()
                            .any(|index| unresolved_uses.contains_key(index))
                    })
                    .map(|indices| -> Diagnostic {
                        Diagnostic::error()
                            .with_code(Code::E024)
                            .with_message(format!(
                                "declarations {} are circular",
                                unordered_listing(
                                    indices
                                        .iter()
                                        .map(|&index| self.absolute_path(index).quote()),
                                    Conjunction::And
                                ),
                            ))
                            .with_primary_spans(
                                indices
                                    .iter()
                                    .map(|&index| self.bindings[index].source.span),
                            )
                    })
                    .collect());
            }

            self.unresolved_uses = unresolved_uses;
        }

        Ok(())
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
            .field("unresolved_uses", &self.unresolved_uses)
            // .field("out_of_order_bindings", &self.out_of_order_bindings)
            .finish()
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
            .with_code(Code::E020)
            .with_message(format!(
                "`{}` is defined multiple times in this scope",
                definition.binder
            ))
            .with_labeled_primary_spans(
                definition.occurrences.into_iter(),
                "conflicting definition",
            )
    }
}

impl From<HashMap<CrateIndex, DuplicateDefinition>> for Diagnostics {
    fn from(definitions: HashMap<CrateIndex, DuplicateDefinition>) -> Self {
        definitions.into_values().map(Into::into).collect()
    }
}

/// Marker to specify it's okay to resolve to either value or module.
pub(super) enum ValueOrModule {}

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
pub(super) enum OnlyValue {}

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
            UntypedValue | UntypedDataType(_) | UntypedConstructor(_) => {
                Ok(Identifier::new(index, identifier.clone()))
            }
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
pub(super) trait ResolutionTarget {
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

/// Partially resolved use binding.
struct UnresolvedUse {
    reference: Path,
    module: CrateIndex,
}

impl fmt::Debug for UnresolvedUse {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("UnresolvedUse")
            .field("reference", &self.reference.as_debug())
            .field("module", &self.module)
            .finish()
    }
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
        write!(
            f,
            "{}",
            self.bindings
                .iter()
                .map(|binding| binding.as_display())
                .join_with(" ")
        )
    }
}

// @Question move to resolver/hir.rs?
/// A name-resolved identifier.
#[derive(Clone, PartialEq)]
pub struct Identifier {
    /// Source at the use-site/call-site or def-site if definition.
    pub source: ast::Identifier,
    pub index: Index,
}

impl Identifier {
    pub(super) fn new(index: impl Into<Index>, source: ast::Identifier) -> Self {
        Self {
            index: index.into(),
            source,
        }
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
        match self.index {
            Index::Crate(index) => Some(index),
            _ => None,
        }
    }

    pub fn de_bruijn_index(&self) -> Option<DeBruijnIndex> {
        match self.index {
            Index::DeBruijn(index) => Some(index),
            _ => None,
        }
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
            .map_or(false, |options| options.display_crate_indices)
        {
            write!(f, "#")?;
            match self.index {
                Index::Crate(index) => write!(f, "{:?}", index)?,
                Index::DeBruijn(index) => write!(f, "{}D", index.0)?,
                Index::DeBruijnParameter => write!(f, "P")?,
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Index {
    Crate(CrateIndex),
    DeBruijn(DeBruijnIndex),
    DeBruijnParameter,
}

impl Index {
    fn shift(self, amount: usize) -> Self {
        match self {
            Self::Crate(_) => self,
            Self::DeBruijn(index) => DeBruijnIndex(index.0 + amount).into(),
            Self::DeBruijnParameter => unreachable!(),
        }
    }

    fn unshift(self) -> Self {
        match self {
            Self::Crate(_) => self,
            Self::DeBruijn(index) => DeBruijnIndex(index.0.saturating_sub(1)).into(),
            Self::DeBruijnParameter => unreachable!(),
        }
    }
}

/// Crate-local identifier for bindings defined through declarations.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct CrateIndex(pub(crate) usize);

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

/// De Bruijn index — index for bindings defined by function parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DeBruijnIndex(pub usize);

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

    pub fn absolute_path(binder: &Identifier, scope: &CrateScope) -> String {
        match binder.index {
            Index::Crate(index) => scope.absolute_path(index),
            Index::DeBruijn(_) | Index::DeBruijnParameter => binder.as_str().into(),
        }
    }

    /// Resolve a binding in a function scope given a depth.
    pub(super) fn resolve_binding(&self, query: &Path, scope: &CrateScope) -> Result<Identifier> {
        self.resolve_binding_with_depth(query, scope, 0, self)
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
    ) -> Result<Identifier> {
        use FunctionScope::*;

        match self {
            &Module(module) => scope
                .resolve_path::<OnlyValue>(query, module, Inquirer::User)
                .map_err(|error| {
                    error
                        .diagnostic_with(scope, |identifier, _| {
                            origin.find_similarly_named(identifier, scope)
                        })
                        .unwrap()
                }),
            // @Note this looks ugly/complicated, use helper functions
            FunctionParameter { parent, binder } => {
                if let Some(identifier) = query.identifier_head() {
                    if binder == identifier {
                        if query.segments.len() > 1 {
                            return Err(value_used_as_a_namespace(identifier, &query.segments[1]));
                        }

                        Ok(Identifier::new(DeBruijnIndex(depth), identifier.clone()))
                    } else {
                        parent.resolve_binding_with_depth(query, scope, depth + 1, origin)
                    }
                } else {
                    scope
                        .resolve_path::<OnlyValue>(query, parent.module(), Inquirer::User)
                        .map_err(|error| error.diagnostic(scope).unwrap())
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
                                ));
                            }

                            Ok(Identifier::new(DeBruijnIndex(depth), identifier.clone()))
                        }
                        None => parent.resolve_binding_with_depth(
                            query,
                            scope,
                            depth + binders.len(),
                            origin,
                        ),
                    }
                } else {
                    scope
                        .resolve_path::<OnlyValue>(query, parent.module(), Inquirer::User)
                        .map_err(|error| error.diagnostic(scope).unwrap())
                }
            }
        }
    }

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

    fn module(&self) -> CrateIndex {
        use FunctionScope::*;

        match self {
            Module(index) => *index,
            FunctionParameter { parent, .. } => parent.module(),
            PatternBinders { parent, .. } => parent.module(),
        }
    }
}

fn is_similar(queried_identifier: &str, other_identifier: &str) -> bool {
    strsim::levenshtein(other_identifier, queried_identifier)
        <= std::cmp::max(queried_identifier.len(), 3) / 3
}

// @Task levenshtein-search for similar named bindings which are in fact a namespace and suggest the first one
fn value_used_as_a_namespace(
    non_namespace: &ast::Identifier,
    subbinder: &ast::Identifier,
) -> Diagnostic {
    Diagnostic::error()
        .with_code(Code::E022)
        .with_message(format!("value `{}` is not a namespace", non_namespace))
        .with_labeled_primary_span(subbinder, "reference to a subdeclaration")
        .with_labeled_secondary_span(non_namespace, "a value, not a namespace")
}

// @Task levenshtein-search for similar named bindings which are in fact values and suggest the first one
// @Task print absolute path of the module in question and use highlight the entire path, not just the last
// segment
fn module_used_as_a_value(span: Span) -> Diagnostic {
    Diagnostic::error()
        .with_code(Code::E023)
        .with_message("module used as if it was a value")
        .with_primary_span(&span)
        .with_help("modules are not first-class citizens, consider utilizing records for such cases instead")
}

/// A possibly recoverable error that cab emerge during resolution.
enum ResolutionError {
    Unrecoverable(Diagnostic),
    UnresolvedBinding {
        identifier: ast::Identifier,
        namespace: CrateIndex,
        inquirer: Inquirer,
    },
    UnresolvedUseBinding {
        // @Note I don't know if this name makes sense at all
        inquirer_index: CrateIndex,
    },
}

impl ResolutionError {
    fn diagnostic(self, scope: &CrateScope) -> Option<Diagnostic> {
        self.diagnostic_with(scope, |identifier, namespace| {
            scope.find_similarly_named(identifier, scope.bindings[namespace].namespace().unwrap())
        })
    }

    fn diagnostic_with<'s>(
        self,
        scope: &CrateScope,
        find_lookalike: impl FnOnce(&str, CrateIndex) -> Option<&'s str>,
    ) -> Option<Diagnostic> {
        match self {
            Self::Unrecoverable(error) => Some(error),
            Self::UnresolvedBinding {
                identifier,
                namespace,
                inquirer,
            } => {
                // @Question should we use the terminology "field" when the namespace is a record?
                let mut message = format!("binding `{}` is not defined in ", identifier);

                match inquirer {
                    Inquirer::User => message += "this scope",
                    Inquirer::System => {
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

                Some(
                    Diagnostic::error()
                        .with_code(Code::E021)
                        .with_message(message)
                        .with_primary_span(&identifier)
                        .when_some(
                            find_lookalike(identifier.as_str(), namespace),
                            |diagnostic, binding| {
                                diagnostic.with_help(format!(
                                    "a binding with a similar name exists in scope: `{}`",
                                    binding
                                ))
                            },
                        ),
                )
            }
            _ => None,
        }
    }
}

impl From<Diagnostic> for ResolutionError {
    fn from(error: Diagnostic) -> Self {
        Self::Unrecoverable(error)
    }
}

pub(super) enum RegistrationError {
    Unrecoverable(Diagnostics),
    /// Duplicate definitions found.
    ///
    /// Details about this error are **not stored here** but as
    /// [DuplicateDefinition]s in the [super::Resolver] so they can
    /// be easily grouped.
    DuplicateDefinition,
}

impl RegistrationError {
    pub(super) fn diagnostics(
        self,
        duplicate_definitions: HashMap<CrateIndex, DuplicateDefinition>,
    ) -> Diagnostics {
        duplicate_definitions
            .into_values()
            .map(Into::into)
            .chain(self)
            .collect()
    }
}

impl From<Diagnostic> for RegistrationError {
    fn from(diagnostic: Diagnostic) -> Self {
        Self::Unrecoverable(once(diagnostic).collect())
    }
}

// @Note not entirely happy that we need to implement this
// this undermines the safety of this type: We only want you
// to be able to access the unrecoverable diagnostics if you
// give us duplicate_definitions: HashMap<CrateIndex, DuplicateDefinition>,
// but now, this is not true anymore
impl IntoIterator for RegistrationError {
    type Item = Diagnostic;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Self::Unrecoverable(diagnostics) => diagnostics.into_iter(),
            Self::DuplicateDefinition => Diagnostics::default().into_iter(),
        }
    }
}

// @Note uggllyyy!!!
impl Extend<Diagnostic> for RegistrationError {
    fn extend<T: IntoIterator<Item = Diagnostic>>(&mut self, iter: T) {
        if let Self::DuplicateDefinition = self {
            *self = Self::Unrecoverable(Diagnostics::default());
        };

        match self {
            Self::Unrecoverable(diagnostics) => diagnostics.extend(iter),
            Self::DuplicateDefinition => unreachable!(),
        };
    }
}
