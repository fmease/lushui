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

use indexed_vec::IndexVec;
use std::rc::Rc;

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
    /// All bindings inside of a crate.
    ///
    /// The first element will always be the root module.
    bindings: IndexVec<CrateIndex, Binding>,
    pub program_entry: Option<Identifier>,
}

impl CrateScope {
    fn root(&self) -> CrateIndex {
        CrateIndex(0)
    }

    fn unwrap_module_scope(&self, index: CrateIndex) -> &ModuleScope {
        match &self.bindings[index].kind {
            BindingKind::Module(scope) => scope,
            _ => unreachable!(),
        }
    }

    fn unwrap_module_scope_mut(&mut self, index: CrateIndex) -> &mut ModuleScope {
        match &mut self.bindings[index].kind {
            BindingKind::Module(scope) => scope,
            _ => unreachable!(),
        }
    }

    fn register_value_binding(
        &mut self,
        binder: parser::Identifier,
        module: CrateIndex,
    ) -> Result<Identifier> {
        self.register_binding(
            binder.clone(),
            Binding {
                source: binder,
                kind: BindingKind::Value,
            },
            Some(module),
        )
    }

    fn register_module_binding(
        &mut self,
        binder: parser::Identifier,
        module: Option<CrateIndex>,
    ) -> Result<Identifier> {
        self.register_binding(
            binder.clone(),
            Binding {
                source: binder,
                kind: BindingKind::Module(ModuleScope {
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
        target: CrateIndex,
        module: CrateIndex,
    ) -> Result<Identifier> {
        let target = self.resolve_use(target);

        self.register_binding(
            binder.clone(),
            Binding {
                source: binder,
                kind: BindingKind::Use(target),
            },
            Some(module),
        )
    }

    fn register_binding(
        &mut self,
        binder: parser::Identifier,
        binding: Binding,
        module: Option<CrateIndex>,
    ) -> Result<Identifier> {
        if let Some(module) = module {
            if let Some(previous) = self
                .unwrap_module_scope(module)
                .bindings
                .iter()
                .map(|&index| &self.bindings[index])
                .find(|binding| binding.source == binder)
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

        Ok(Identifier::new(index, binder))
    }

    fn resolve_path<Target: ResolutionTarget>(
        &self,
        path: &Path,
        module: CrateIndex,
    ) -> Result<Target::Output> {
        use parser::PathHeadKind::*;
        use BindingKind::*;

        if let Some(head) = &path.head {
            if path.segments.is_empty() {
                return Target::resolve_bare_super_and_crate(head.span, module);
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
            Some(identifier) => Target::resolve_simple_path(identifier, binding, index),
            None => match binding {
                Module(_) => self.resolve_path::<Target>(&path.tail(), index),
                Value => Err(value_used_as_a_module(
                    path.segments[0].span,
                    path.segments[1].span,
                )),
                Use(_) => unreachable!(),
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
    ) -> Result<CrateIndex> {
        self.unwrap_module_scope(module)
            .bindings
            .iter()
            .copied()
            .find(|&index| &self.bindings[index].source == identifier)
            .ok_or_else(|| {
                Diagnostic::new(
                    Level::Error,
                    Code::E021,
                    // @Task modify error message: if it's a simple path: "in this scope/module"
                    // and if it's a complex one: "in module `module`"
                    format!("binding `{}` is not defined in this scope", identifier),
                )
                .with_span(identifier.span)
            })
            .map(|index| self.resolve_use(index))
    }

    fn resolve_use(&self, index: CrateIndex) -> CrateIndex {
        use BindingKind::*;

        match self.bindings[index].kind {
            Value | Module(_) => index,
            Use(target) => target,
        }
    }

    // @Beacon @Note I *think* we should return a Result because
    // if we resolve an identifier (textually), it could mean that it has been
    // defined multiple times and that the one we are getting (textually!) is
    // actually of a different kind (Value vs Module)
    fn resolve_identifier<Target: ResolutionTarget>(
        &self,
        identifier: &parser::Identifier,
        module: CrateIndex,
    ) -> Result<Target::Output> {
        let index = self.resolve_first_segment(identifier, module)?;
        // .unwrap_or_else(|_| unreachable!());
        Target::resolve_simple_path(identifier, &self.bindings[index].kind, index)
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
        _: &BindingKind,
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
        binding: &BindingKind,
        index: CrateIndex,
    ) -> Result<Self::Output> {
        use BindingKind::*;

        match binding {
            Value => Ok(Identifier::new(index, identifier.clone())),
            Module(_) => Err(module_used_as_a_value(identifier.span)),
            Use(_) => unreachable!(),
        }
    }
}

trait ResolutionTarget {
    type Output;

    fn resolve_bare_super_and_crate(span: Span, module: CrateIndex) -> Result<Self::Output>;

    fn resolve_simple_path(
        identifier: &parser::Identifier,
        binding: &BindingKind,
        index: CrateIndex,
    ) -> Result<Self::Output>;
}

impl Declaration<Desugared> {
    pub fn resolve(self, scope: &mut CrateScope) -> Result<Declaration<Resolved>, Diagnostics> {
        self.resolve_first_pass(None, scope)?;
        // eprintln!("{:?}", scope.bindings);
        self.resolve_second_pass(None, scope)
    }

    // @Task rename we are not doing any resolving...only registering
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
                let _binder = scope
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

                let _binder = scope
                    .register_value_binding(declaration.binder.clone(), module)
                    .many_err()?;
            }
            Module(declaration) => {
                // @Task @Beacon don't return early on error
                let binder = scope
                    .register_module_binding(declaration.binder.clone(), module)
                    .many_err()?;

                for declaration in &declaration.declarations {
                    // @Task @Beacon don't return early on error
                    declaration.resolve_first_pass(Some(binder.krate().unwrap()), scope)?;
                }
            }
            Use(_declaration) => {
                return Err(Diagnostic::new(
                    Level::Bug,
                    None,
                    "use declarations not supported right now",
                ))
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

                let binder = match declaration.binder.segments.last() {
                    Some(binder) => binder,
                    None => {
                        return Err(Diagnostic::new(
                            Level::Fatal,
                            None,
                            "`use` of bare `super` and `crate` disallowed",
                        )
                        .with_span(self.span))
                        .many_err()
                    }
                };

                // @Task even if this does not resolve, register the binder (last segment) into
                // this module to prevent unnecessary consequential errors
                let target = scope
                    .resolve_path::<ValueOrModule>(&declaration.binder, module)
                    .many_err()?;

                let binder = scope
                    .register_use_binding(binder.clone(), target, module)
                    .many_err()?;

                Ok(decl! { Use[self.span][self.attributes] { binder } })
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
                        parameter: pi.parameter.clone().map(|parameter| Identifier::new(Index::DebruijnParameter, parameter.clone())),
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
                    body: lambda.body.clone().resolve(&scope.extend(lambda.parameter.clone()), crate_scope)?,
                    explicitness: lambda.explicitness,
                }
            },
            UseIn => todo!("resolving use/in"),
            CaseAnalysis(_expression) => todo!("resolving case analysis"),
            Substitution(_) | ForeignApplication(_) => unreachable!(),
        })
    }
}

#[derive(Debug)]
pub struct ModuleScope {
    parent: Option<CrateIndex>,
    bindings: Vec<CrateIndex>,
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
                Index::Crate(index) => write!(f, "{}C", index.0)?,
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CrateIndex(usize);

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
            FunctionScope::Module(module) => crate_scope.resolve_path::<OnlyValue>(query, *module),
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
                    crate_scope.resolve_path::<OnlyValue>(query, parent.module())
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

pub struct Binding {
    /// Source information of the definition site.
    source: parser::Identifier,
    kind: BindingKind,
}

impl fmt::Debug for Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Binding {{ source: {}, kind: {:?} }}",
            self.source, self.kind
        )
    }
}

// @Task later: Undefined, Untyped
// @Note corresponds to [crate::interpreter::scope::Entity].
#[derive(Debug)]
enum BindingKind {
    Value,
    Module(ModuleScope),
    /// A use bindings means extra indirection. We don't just "clone" the value it gets
    /// "assigned" to. We merely reference it. This way we don't need to reference-count
    /// module scopes (to avoid deep copies). Also, once we merge this data structure with
    /// the one from the interpreter, we can successfully alias constructors and still
    /// pattern match on them!
    /// Invariant: The "target" is never a Use itself. There are no nested aliases
    Use(CrateIndex),
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
