// @Task module-level documentation

use crate::{
    ast::{self, Path},
    diagnostic::{Code, Diagnostic, Result, Results},
    entity::{Entity, EntityKind},
    span::{Span, Spanning},
    support::{DebugIsDisplay, DisplayWith, ManyErrExt},
    typer::interpreter::{ffi, scope::Registration},
    HashMap,
};
use indexed_vec::IndexVec;
use joinery::JoinableIterator;
use std::fmt;

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
    pub runners: IndexVec<ffi::IOIndex, ffi::IORunner>,
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
    pub(super) fn root(&self) -> CrateIndex {
        CrateIndex(0)
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

    /// Register a binding to a given entity.
    ///
    /// Apart from actually registering, mainly checks for name duplication.
    pub(super) fn register_binding(
        &mut self,
        binder: ast::Identifier,
        binding: EntityKind,
        namespace: Option<CrateIndex>,
    ) -> Result<CrateIndex> {
        // @Beacon @Task emit N error messages if there are N binders with the same name (multiple declarations, same name)
        // and not N-1 as we currently do. this is the new design where for conflicting things, we emit a diagnostic each
        // where one time, the span is primary and all other times, it is secondary (this has already been implemented with
        // mututally exclusive attributes)
        // @Update @Task also print other duplicate/conflicting bindings with secondary role (not just the very first
        // binding)

        if let Some(namespace) = namespace {
            if let Some(previous) = self.bindings[namespace]
                .namespace()
                .unwrap()
                .bindings
                .iter()
                .map(|&index| &self.bindings[index])
                .find(|binding| binding.source == binder)
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

    /// Resolves a syntactic path given a namespace.
    fn resolve_path<Target: ResolutionTarget>(
        &self,
        path: &Path,
        namespace: CrateIndex,
        inquirer: Inquirer,
    ) -> Result<Target::Output, Error> {
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
                .with_span(hanger)
        })
    }

    fn resolve_first_segment(
        &self,
        identifier: &ast::Identifier,
        namespace: CrateIndex,
        inquirer: Inquirer,
    ) -> Result<CrateIndex, Error> {
        self.bindings[namespace]
            .namespace()
            .unwrap()
            .bindings
            .iter()
            .find(|&&index| &self.bindings[index].source == identifier)
            .ok_or_else(|| Error::UnresolvedBinding {
                identifier: identifier.clone(),
                namespace,
                inquirer,
            })
            .and_then(|&index| self.resolve_use(index))
    }

    fn find_similarly_named(&self, identifier: &str, namespace: &Namespace) -> Option<&str> {
        namespace
            .bindings
            .iter()
            .map(|&index| self.bindings[index].source.as_str())
            .find(|&other_identifier| is_similar(identifier, other_identifier))
    }

    /// Resolve indirect uses aka chains of use bindings.
    ///
    /// Makes a lot of sense to be honest and it's just an arbitrary invariant
    /// created to make things easier to reason about during resolution.
    fn resolve_use(&self, index: CrateIndex) -> Result<CrateIndex, Error> {
        use EntityKind::*;

        match self.bindings[index].kind {
            UntypedValue | Module(_) | UntypedDataType(_) | UntypedConstructor(_) => Ok(index),
            Use(reference) => Ok(reference),
            UnresolvedUse => Err(Error::UnresolvedUseBinding),
            _ => unreachable!(),
        }
    }

    // @Question please document what this is useful/used for, I always forget
    /// Resolve a single identifier (in contrast to a whole path).
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
                    Err(error @ (Error::UnresolvedBinding { .. } | Error::Unrecoverable(_))) => {
                        return Err(error.diagnostic(self).unwrap()).many_err()
                    }
                    Err(Error::UnresolvedUseBinding) => {
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
            // @Bug debug out broken: missing newline: we need to do it manually without debug_struct
            .field("unresolved_uses", &self.unresolved_uses)
            // .field("out_of_order_bindings", &self.out_of_order_bindings)
            .finish()
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
#[derive(Debug)]
struct UnresolvedUse {
    reference: Path,
    module: CrateIndex,
}

/// A namespace can either be a module or a data type.
/// A module contains any declarations (except constructors) and
/// a data type their constructors.
#[derive(Clone, Default)]
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
            Index::Debruijn(_) | Index::DebruijnParameter => binder.as_str().into(),
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
                        .diagnostic_with(scope, |identifier: &str, _| {
                            origin.find_similarly_named(identifier, scope)
                        })
                        .unwrap()
                }),
            // @Note this looks ugly/complicated, use helper functions
            FunctionParameter { parent, binder } => {
                if let Some(identifier) = query.first_identifier() {
                    if binder == identifier {
                        if query.segments.len() > 1 {
                            return Err(value_used_as_a_namespace(identifier, &query.segments[1]));
                        }

                        Ok(Identifier::new(DebruijnIndex(depth), identifier.clone()))
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
        .with_labeled_span(subbinder, "reference to a subdeclaration")
        .with_labeled_span(non_namespace, "a value, not a namespace")
}

// @Task levenshtein-search for similar named bindings which are in fact values and suggest the first one
fn module_used_as_a_value(span: Span) -> Diagnostic {
    Diagnostic::error()
        .with_code(Code::E023)
        .with_message("module used as if it was a value")
        .with_span(&span)
}

enum Error {
    Unrecoverable(Diagnostic),
    UnresolvedBinding {
        identifier: ast::Identifier,
        namespace: CrateIndex,
        inquirer: Inquirer,
    },
    UnresolvedUseBinding,
}

impl Error {
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
                        .with_span(&identifier)
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

impl From<Diagnostic> for Error {
    fn from(error: Diagnostic) -> Self {
        Self::Unrecoverable(error)
    }
}
