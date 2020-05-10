//! Name resolver.
//!
//! This module lowers the desugared AST to the HIR (high-level intermediate representation).
//!
//! ## Future Features
//!
//! * resolve out-of-order declarations
//! * resolve recursive (value) declarations and prepare the resulting bindings
//!   for the type checker which should be able to type-check recursive functions
//!   and (!) equi-recursive types)
//! * gracefully handle cyclic dependencies
//! * handle module privacy (notably restricted exposure and good error messages)
//! * handle crate declarations

// @Note the `handle` methods ignore whether an error is fatal or not, this should be changed @Task

// @Beacon @Task `X: Type = Duple Non-Existent-One Non-Existent-Two` should
// report both errors (I don't think that's the case right now)
// @Note for this create a DeclarationKind::Invalid (and maybe also ::Fatal) and have a#
// Diagnostic buffer in CrateScope. Then, bubble invalids up

use indexed_vec::IndexVec;
use std::{collections::HashMap, rc::Rc};

use crate::{
    desugar::Desugared,
    diagnostic::*,
    hir::{decl, expr, Declaration, DeclarationKind, Expression, ExpressionKind, Pass},
    parser::{self, Path},
    span::Span,
    support::{handle::*, ManyErrExt, TransposeExt},
};

const PROGRAM_ENTRY_IDENTIFIER: &str = "main";

#[derive(Default)]
pub struct CrateScope {
    pub program_entry: Option<Identifier>,
    /// All bindings inside of a crate.
    ///
    /// The first element will always be the root module.
    bindings: IndexVec<CrateIndex, Entity>,
    unresolved_uses: HashMap<CrateIndex, WorklistItem>,
}

struct WorklistItem {
    reference: Path,
    module: CrateIndex,
}

impl CrateScope {
    fn root(&self) -> CrateIndex {
        CrateIndex(0)
    }

    fn unwrap_module_scope(&self, index: CrateIndex) -> &ModuleScope {
        match &self.bindings[index].kind {
            EntityKind::Module(scope) => scope,
            _ => unreachable!(),
        }
    }

    fn unwrap_module_scope_mut(&mut self, index: CrateIndex) -> &mut ModuleScope {
        match &mut self.bindings[index].kind {
            EntityKind::Module(scope) => scope,
            _ => unreachable!(),
        }
    }

    fn register_value_binding(
        &mut self,
        binder: parser::Identifier,
        module: CrateIndex,
    ) -> Result<Identifier> {
        self.register_binding(
            &binder,
            Entity {
                source: binder.clone(),
                kind: EntityKind::Value,
            },
            Some(module),
        )
        .map(|index| Identifier::new(index, binder))
    }

    fn register_module_binding(
        &mut self,
        binder: parser::Identifier,
        module: Option<CrateIndex>,
    ) -> Result<CrateIndex> {
        self.register_binding(
            &binder,
            Entity {
                source: binder.clone(),
                kind: EntityKind::Module(ModuleScope {
                    parent: module,
                    bindings: Vec::new(),
                }),
            },
            module,
        )
    }

    fn register_use_binding(
        &mut self,
        binder: parser::Identifier,
        reference: &Path,
        module: CrateIndex,
    ) -> Result<()> {
        let index = self.register_binding(
            &binder,
            Entity {
                source: binder.clone(),
                kind: EntityKind::UnresolvedUse,
            },
            Some(module),
        )?;

        let old = self.unresolved_uses.insert(
            index,
            WorklistItem {
                reference: reference.clone(),
                module,
            },
        );

        debug_assert!(old.is_none());

        Ok(())
    }

    fn register_binding(
        &mut self,
        binder: &parser::Identifier,
        binding: Entity,
        module: Option<CrateIndex>,
    ) -> Result<CrateIndex> {
        if let Some(module) = module {
            if let Some(previous) = self
                .unwrap_module_scope(module)
                .bindings
                .iter()
                .map(|&index| &self.bindings[index])
                .find(|binding| &binding.source == binder)
            {
                return Err(Diagnostic::new(
                    Level::Error,
                    Code::E020,
                    format!("`{}` is defined multiple times in this scope", binder),
                )
                .with_labeled_span(binder.span, "redefinition")
                .with_labeled_span(previous.source.span, "previous definition"));
            }
        }

        let index = self.bindings.push(binding);

        if let Some(module) = module {
            self.unwrap_module_scope_mut(module).bindings.push(index);
        }

        Ok(index)
    }

    fn resolve_path<Target: ResolutionTarget>(
        &self,
        path: &Path,
        module: CrateIndex,
    ) -> Result<Target::Output, Error> {
        use parser::PathHeadKind::*;
        use EntityKind::*;

        if let Some(head) = &path.head {
            if path.segments.is_empty() {
                return Target::resolve_bare_super_and_crate(head.span, module).map_err(Into::into);
            }

            return self.resolve_path::<Target>(
                &path.tail(),
                match head.kind {
                    Crate => self.root(),
                    Super => self.resolve_super(head.span, module)?,
                },
            );
        }

        let index = self.resolve_first_segment(&path.segments[0], module)?;
        let binding = &self.bindings[index].kind;

        match path.simple() {
            Some(identifier) => {
                Target::resolve_simple_path(identifier, binding, index).map_err(Into::into)
            }
            None => match binding {
                Module(_) => self.resolve_path::<Target>(&path.tail(), index),
                Value => {
                    Err(value_used_as_a_module(path.segments[0].span, path.segments[1].span).into())
                }
                UnresolvedUse | Use(_) => unreachable!(),
            },
        }
    }

    fn resolve_super(&self, span: Span, module: CrateIndex) -> Result<CrateIndex> {
        self.unwrap_module_scope(module).parent.ok_or_else(|| {
            Diagnostic::new(
                Level::Error,
                Code::E021,
                "the crate root does not have a parent",
            )
            .with_span(span)
        })
    }

    fn resolve_first_segment(
        &self,
        identifier: &parser::Identifier,
        module: CrateIndex,
    ) -> Result<CrateIndex, Error> {
        self.unwrap_module_scope(module)
            .bindings
            .iter()
            .copied()
            .find(|&index| &self.bindings[index].source == identifier)
            .ok_or_else(|| Error::UndefinedBinding(identifier.clone()))
            .and_then(|index| self.resolve_use(index))
    }

    fn resolve_use(&self, index: CrateIndex) -> Result<CrateIndex, Error> {
        use EntityKind::*;

        match self.bindings[index].kind {
            Value | Module(_) => Ok(index),
            Use(reference) => Ok(reference),
            UnresolvedUse => Err(Error::FoundUnresolvedUse),
        }
    }

    fn resolve_identifier<Target: ResolutionTarget>(
        &self,
        identifier: &parser::Identifier,
        module: CrateIndex,
    ) -> Result<Target::Output> {
        let index = self.resolve_first_segment(identifier, module)?;
        Target::resolve_simple_path(identifier, &self.bindings[index].kind, index)
    }

    fn resolve_unresolved_uses(&mut self) -> Result<(), Diagnostics> {
        while !self.unresolved_uses.is_empty() {
            let mut unresolved_uses = HashMap::new();

            for (&index, item) in self.unresolved_uses.iter() {
                match self.resolve_path::<ValueOrModule>(&item.reference, item.module) {
                    Ok(reference) => {
                        self.bindings[index].kind = EntityKind::Use(reference);
                    }
                    Err(FoundUnresolvedUse) => {
                        unresolved_uses.insert(
                            index,
                            WorklistItem {
                                reference: item.reference.clone(),
                                module: item.module,
                            },
                        );
                    }
                    Err(other) => return Err(other.into()).many_err(),
                }
            }

            if unresolved_uses.len() == self.unresolved_uses.len() {
                return Err(unresolved_uses
                    .keys()
                    .map(|&index| {
                        Diagnostic::new(Level::Error, Code::E024, "this declaration is circular")
                            .with_span(self.bindings[index].source.span)
                    })
                    .collect());
            }

            self.unresolved_uses = unresolved_uses;
        }

        Ok(())
    }
}

enum ValueOrModule {}

impl ResolutionTarget for ValueOrModule {
    type Output = CrateIndex;

    fn resolve_bare_super_and_crate(_: Span, module: CrateIndex) -> Result<Self::Output> {
        Ok(module)
    }

    fn resolve_simple_path(
        _: &parser::Identifier,
        _: &EntityKind,
        index: CrateIndex,
    ) -> Result<Self::Output> {
        Ok(index)
    }
}

enum OnlyValue {}

impl ResolutionTarget for OnlyValue {
    type Output = Identifier;

    fn resolve_bare_super_and_crate(span: Span, _: CrateIndex) -> Result<Self::Output> {
        Err(module_used_as_a_value(span))
    }

    fn resolve_simple_path(
        identifier: &parser::Identifier,
        binding: &EntityKind,
        index: CrateIndex,
    ) -> Result<Self::Output> {
        use EntityKind::*;

        match binding {
            Value => Ok(Identifier::new(index, identifier.clone())),
            Module(_) => Err(module_used_as_a_value(identifier.span)),
            UnresolvedUse | Use(_) => unreachable!(),
        }
    }
}

trait ResolutionTarget {
    type Output;

    fn resolve_bare_super_and_crate(span: Span, module: CrateIndex) -> Result<Self::Output>;

    fn resolve_simple_path(
        identifier: &parser::Identifier,
        binding: &EntityKind,
        index: CrateIndex,
    ) -> Result<Self::Output>;
}

impl Declaration<Desugared> {
    pub fn resolve(self, scope: &mut CrateScope) -> Result<Declaration<Resolved>, Diagnostics> {
        self.resolve_first_pass(None, scope)?;
        scope.resolve_unresolved_uses()?;
        self.resolve_second_pass(None, scope)
    }

    fn resolve_first_pass(
        &self,
        module: Option<CrateIndex>,
        scope: &mut CrateScope,
    ) -> Result<(), Diagnostics> {
        use DeclarationKind::*;

        match &self.kind {
            Value(declaration) => {
                let module = module.unwrap();

                let binder = scope
                    .register_value_binding(declaration.binder.clone(), module)
                    .many_err()?;

                if scope.program_entry.is_none() && module == scope.root() {
                    if &binder.source.atom == PROGRAM_ENTRY_IDENTIFIER {
                        scope.program_entry = Some(binder.clone());
                    }
                }
            }
            Data(declaration) => {
                let module = module.unwrap();

                // @Task @Beacon don't return early on error
                scope
                    .register_value_binding(declaration.binder.clone(), module)
                    .many_err()?;

                if let Some(constructors) = &declaration.constructors {
                    for constructor in constructors {
                        // @Task @Beacon don't return early on error
                        constructor.resolve_first_pass(Some(module), scope)?;
                    }
                }
            }
            Constructor(declaration) => {
                let module = module.unwrap();

                scope
                    .register_value_binding(declaration.binder.clone(), module)
                    .many_err()?;
            }
            Module(declaration) => {
                // @Task @Beacon don't return early on error
                let index = scope
                    .register_module_binding(declaration.binder.clone(), module)
                    .many_err()?;

                for declaration in &declaration.declarations {
                    // @Task @Beacon don't return early on error
                    declaration.resolve_first_pass(Some(index), scope)?;
                }
            }
            Use(declaration) => {
                let module = module.unwrap();

                let binder = declaration
                    .binder
                    .as_ref()
                    .ok_or_else(|| {
                        Diagnostic::new(
                            Level::Fatal,
                            None,
                            "`use` of bare `super` and `crate` disallowed",
                        )
                        .with_span(self.span)
                    })
                    .many_err()?;

                scope
                    .register_use_binding(binder.clone(), &declaration.reference, module)
                    .many_err()?;
            }
        }

        Ok(())
    }

    fn resolve_second_pass(
        self,
        module: Option<CrateIndex>,
        scope: &mut CrateScope,
    ) -> Result<Declaration<Resolved>, Diagnostics> {
        use DeclarationKind::*;

        match self.kind {
            Value(declaration) => {
                let module = module.unwrap();

                let type_annotation = declaration
                    .type_annotation
                    .resolve(&FunctionScope::Module(module), scope)
                    .many_err();

                let expression = declaration
                    .expression
                    .map(|expression| {
                        expression
                            .resolve(&FunctionScope::Module(module), scope)
                            .many_err()
                    })
                    .transpose();

                let binder = scope
                    .resolve_identifier::<OnlyValue>(&declaration.binder, module)
                    .many_err()?;

                let span = self.span;
                let attributes = self.attributes;

                (type_annotation, expression).handle(|type_annotation, expression| {
                    decl! {
                        Value[span][attributes] {
                            binder,
                            type_annotation,
                            expression,
                        }
                    }
                })
            }
            Data(declaration) => {
                let module = module.unwrap();

                let type_annotation = declaration
                    .type_annotation
                    .resolve(&FunctionScope::Module(module), scope)
                    .many_err();

                let constructors = declaration.constructors.map(|constructors| {
                    constructors
                        .into_iter()
                        .map(|constructor| constructor.resolve_second_pass(Some(module), scope))
                        .collect()
                });

                let binder = scope
                    .resolve_identifier::<OnlyValue>(&declaration.binder, module)
                    .many_err()?;
                let span = self.span;
                let attributes = self.attributes;

                (type_annotation, constructors.transpose()).handle(
                    |type_annotation, constructors| {
                        decl! {
                            Data[span][attributes] {
                                binder,
                                constructors,
                                type_annotation,
                            }
                        }
                    },
                )
            }
            Constructor(declaration) => {
                let module = module.unwrap();

                let type_annotation = declaration
                    .type_annotation
                    .resolve(&FunctionScope::Module(module), scope)
                    .many_err()?;

                let binder = scope
                    .resolve_identifier::<OnlyValue>(&declaration.binder, module)
                    .many_err()?;
                let span = self.span;
                let attributes = self.attributes;

                Ok(decl! {
                    Constructor[span][attributes] {
                        binder,
                        type_annotation,
                    }
                })
            }
            Module(declaration) => {
                // @Note ValueOrModule too general @Bug this might lead to
                // values used as modules!! we should create OnlyModule
                let index = match module {
                    Some(module) => scope
                        .resolve_identifier::<ValueOrModule>(&declaration.binder, module)
                        .many_err()?,
                    None => scope.root(),
                };

                let declarations = declaration
                    .declarations
                    .into_iter()
                    .map(|declaration| declaration.resolve_second_pass(Some(index), scope))
                    .collect::<Vec<_>>()
                    .transpose()?;

                Ok(decl! {
                    Module[self.span][self.attributes] {
                        binder: Identifier::new(index, declaration.binder),
                        file: declaration.file,
                        declarations,
                    }
                })
            }
            Use(declaration) => {
                let module = module.unwrap();
                let binder = declaration.binder.unwrap();

                let index = scope
                    .resolve_identifier::<ValueOrModule>(&binder, module)
                    .many_err()?;

                Ok(decl! {
                    Use[self.span][self.attributes] {
                        binder: Some(Identifier::new(index, binder.clone())),
                        // @Temporary
                        reference: Identifier::new(index, binder.clone()),
                    }
                })
            }
        }
    }
}

// @Task @Beacon use Rc::try_unwrap more instead of clone
impl Expression<Desugared> {
    fn resolve(
        self,
        scope: &FunctionScope<'_>,
        crate_scope: &CrateScope,
    ) -> Result<Expression<Resolved>> {
        use ExpressionKind::*;

        Ok(match self.kind {
            PiType(pi) => {
                expr! {
                    PiType[self.span] {
                        parameter: pi.parameter.clone()
                            .map(|parameter| Identifier::new(Index::DebruijnParameter, parameter.clone())),
                        domain: pi.domain.clone().resolve(scope, crate_scope)?,
                        codomain: match pi.parameter.clone() {
                            Some(parameter) => pi.codomain.clone().resolve(&scope.extend(parameter), crate_scope)?,
                            None => pi.codomain.clone().resolve(scope, crate_scope)?,
                        },
                        explicitness: pi.explicitness,
                    }
                }
            }
            Application(application) => expr! {
                Application[self.span] {
                    callee: application.callee.clone().resolve(scope, crate_scope)?,
                    argument: application.argument.clone().resolve(scope, crate_scope)?,
                    explicitness: application.explicitness,
                }
            },
            Type => expr! { Type[self.span] },
            Nat(nat) => expr! {
                Nat[self.span] {
                    value: Rc::try_unwrap(nat)
                        .map(|nat| nat.value)
                        .unwrap_or_else(|nat| nat.value.clone()),
                }
            },
            Text(text) => expr! {
                Text[self.span] {
                    value: Rc::try_unwrap(text)
                        .map(|text| text.value)
                        .unwrap_or_else(|text| text.value.clone()),
                }
            },
            Binding(binding) => expr! {
                Binding[self.span] {
                    binder: scope.resolve_binding(&binding.binder, crate_scope)?,
                }
            },
            Lambda(lambda) => expr! {
                Lambda[self.span] {
                    parameter: Identifier::new(Index::DebruijnParameter, lambda.parameter.clone()),
                    parameter_type_annotation: lambda.parameter_type_annotation.clone()
                        .map(|r#type| r#type.resolve(scope, crate_scope))
                        .transpose()?,
                    body_type_annotation: lambda.body_type_annotation.clone()
                        .map(|r#type| r#type.resolve(&scope.extend(lambda.parameter.clone()), crate_scope))
                        .transpose()?,
                    body: lambda.body.clone()
                        .resolve(&scope.extend(lambda.parameter.clone()), crate_scope)?,
                    explicitness: lambda.explicitness,
                }
            },
            UseIn => todo!("resolving use/in"),
            CaseAnalysis(_expression) => todo!("resolving case analysis"),
            Substitution(_) | ForeignApplication(_) => unreachable!(),
        })
    }
}

pub struct ModuleScope {
    parent: Option<CrateIndex>,
    bindings: Vec<CrateIndex>,
}

impl fmt::Debug for ModuleScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "super: ")?;
        match &self.parent {
            Some(parent) => write!(f, "{:?}", parent)?,
            None => f.write_str("_|_")?,
        };
        write!(f, ", {:?}", self.bindings)
    }
}

#[derive(Clone, PartialEq)]
pub struct Identifier {
    /// Source at the use-site/call-site or def-site if definition.
    pub source: parser::Identifier,
    pub index: Index,
}

impl Identifier {
    fn new(index: impl Into<Index>, source: parser::Identifier) -> Self {
        Self {
            index: index.into(),
            source,
        }
    }

    // @Note bad name
    pub fn localized(&self) -> Self {
        Self::new(DebruijnIndex(0), self.source.clone())
    }

    // @Task find better name which suggests Span
    pub fn dummified(self) -> Self {
        Self {
            source: self.source.dummified(),
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

    pub fn krate(&self) -> Option<CrateIndex> {
        match self.index {
            Index::Crate(index) => Some(index),
            _ => None,
        }
    }

    pub fn debruijn(&self) -> Option<DebruijnIndex> {
        match self.index {
            Index::Debruijn(index) => Some(index),
            _ => None,
        }
    }
}

use std::fmt;

// @Task print whole path
impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.source)?;
        if crate::OPTIONS.get().unwrap().display_crate_indices {
            f.write_str("#")?;
            match self.index {
                Index::Crate(index) => write!(f, "{:?}", index)?,
                Index::Debruijn(index) => write!(f, "{}D", index.0)?,
                Index::DebruijnParameter => f.write_str("P")?,
            }
        }
        Ok(())
    }
}

#[derive(Clone)]
pub enum Resolved {}

impl Pass for Resolved {
    type Binder = Identifier;
    type ReferencedBinder = Self::Binder;
    type PatternBinder = Self::Binder;
    type ForeignApplicationBinder = Self::Binder;
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Index {
    Crate(CrateIndex),
    Debruijn(DebruijnIndex),
    DebruijnParameter,
}

impl Index {
    // @Bug @Beacon @Beacon if the index is a module index, it should not be "shifted"
    // but replaced by a new module index @Update idk what you are talking about, past self!
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
    Binding {
        parent: &'a Self,
        binder: parser::Identifier,
    },
}

impl<'a> FunctionScope<'a> {
    fn extend(&'a self, binder: parser::Identifier) -> Self {
        Self::Binding {
            parent: self,
            binder,
        }
    }

    fn resolve_binding(&self, query: &Path, crate_scope: &CrateScope) -> Result<Identifier> {
        self.resolve_binding_with_depth(query, crate_scope, 0)
    }

    fn resolve_binding_with_depth(
        &self,
        query: &Path,
        crate_scope: &CrateScope,
        depth: usize,
    ) -> Result<Identifier> {
        match self {
            FunctionScope::Module(module) => crate_scope
                .resolve_path::<OnlyValue>(query, *module)
                .map_err(Into::into),
            FunctionScope::Binding { parent, binder } => {
                if let Some(identifier) = query.identifier_head() {
                    if binder == identifier {
                        if query.segments.len() > 1 {
                            return Err(value_used_as_a_module(
                                identifier.span,
                                query.segments[1].span,
                            ));
                        }

                        Ok(Identifier::new(DebruijnIndex(depth), identifier.clone()))
                    } else {
                        parent.resolve_binding_with_depth(query, crate_scope, depth + 1)
                    }
                } else {
                    crate_scope
                        .resolve_path::<OnlyValue>(query, parent.module())
                        .map_err(Into::into)
                }
            }
        }
    }

    fn module(&self) -> CrateIndex {
        match self {
            Self::Module(index) => *index,
            Self::Binding { parent, .. } => parent.module(),
        }
    }
}

pub struct Entity {
    /// Source information of the definition site.
    source: parser::Identifier,
    kind: EntityKind,
}

impl fmt::Debug for Entity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:20} |-> {:?}", self.source, self.kind)
    }
}

// @Task later: Undefined, Untyped
// @Note corresponds to [crate::interpreter::scope::Entity].
enum EntityKind {
    Value,
    Module(ModuleScope),
    /// A use bindings means extra indirection. We don't just "clone" the value it gets
    /// "assigned" to. We merely reference it. This way we don't need to reference-count
    /// module scopes (to avoid deep copies). Also, once we merge this data structure with
    /// the one from the interpreter, we can successfully alias constructors and still
    /// pattern match on them!
    /// Invariant: The "target" is never a Use itself. There are no nested aliases
    Use(CrateIndex),
    UnresolvedUse,
}

impl fmt::Debug for EntityKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value => f.write_str("value"),
            Self::Module(scope) => write!(f, "module, {:?}", scope),
            Self::Use(index) => write!(f, "use {:?}", index),
            Self::UnresolvedUse => write!(f, "use ???"),
        }
    }
}

use Error::*;

enum Error {
    UndefinedBinding(parser::Identifier),
    FoundUnresolvedUse,
    Unrecoverable(Diagnostic),
}

impl From<Error> for Diagnostic {
    fn from(error: Error) -> Self {
        match error {
            UndefinedBinding(identifier) => Diagnostic::new(
                Level::Error,
                Code::E021,
                // @Task modify error message: if it's a simple path: "in this scope/module"
                // and if it's a complex one: "in module `module`"
                format!("binding `{}` is not defined in this scope", identifier),
            )
            .with_span(identifier.span),
            FoundUnresolvedUse => {
                Diagnostic::new(Level::Error, None, "some use declarations form a cycle")
            }
            Unrecoverable(diagnostic) => diagnostic,
        }
    }
}

impl From<Diagnostic> for Error {
    fn from(diagnostic: Diagnostic) -> Self {
        Unrecoverable(diagnostic)
    }
}

// @Question fatal?? and if non-fatal, it's probably ignored
fn value_used_as_a_module(non_module_span: Span, subbinder_span: Span) -> Diagnostic {
    Diagnostic::new(
        Level::Fatal,
        Code::E022,
        "values do not have subdeclarations",
    )
    .with_labeled_span(subbinder_span, "reference to a subdeclaration")
    .with_labeled_span(non_module_span, "a value, not a module")
}

fn module_used_as_a_value(span: Span) -> Diagnostic {
    Diagnostic::new(Level::Fatal, Code::E023, "module used as if it was a value").with_span(span)
}
