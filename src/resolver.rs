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
    diagnostic::*,
    hir::{decl, expr, Binder, Declaration, DeclarationKind, Expression, ExpressionKind},
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
        let target = self.resolve_alias(target);

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

    // @Beacon @Task abstract over resolve_binding_{first,second}_pass with a trait and assoc types!
    fn resolve_binding_first_pass<Target: ResolutionTarget>(
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

            return self.resolve_binding_first_pass::<Target>(
                &path.tail(),
                match head.kind {
                    Crate => self.root(),
                    Super => self.resolve_super_first_pass(head.span, module)?,
                },
            );
        }

        let index = self.resolve_identifier_first_pass(module, &path.segments[0]);

        // @Temporary @Beacon
        let index = match index {
            Some(index) => index,
            None => {
                return match path.simple() {
                    Some(identifier) => Target::resolve_index_none(identifier),
                    None => Err(Diagnostic::new(
                        Level::Fatal,
                        None,
                        "order-independent modules not supported yet; only values right now",
                    )),
                }
            }
        };

        let index = self.resolve_alias(index);

        let binding = &self.bindings[index].kind;

        match path.simple() {
            Some(identifier) => Target::resolve_simple_path(identifier, binding, index),
            None => match binding {
                Module(_) => self.resolve_binding_first_pass::<Target>(&path.tail(), index),
                Value => Err(value_used_as_a_module(
                    path.segments[0].span,
                    path.segments[1].span,
                )),
                Use(_) => unreachable!(),
            },
        }
    }

    fn resolve_super_first_pass(&self, span: Span, module: CrateIndex) -> Result<CrateIndex> {
        self.unwrap_module_scope(module).parent.ok_or_else(|| {
            Diagnostic::new(
                Level::Error,
                Code::E021,
                "the crate root does not have a parent",
            )
            .with_span(span)
        })
    }

    fn resolve_identifier_first_pass(
        &self,
        module: CrateIndex,
        identifier: &parser::Identifier,
    ) -> Option<CrateIndex> {
        self.unwrap_module_scope(module)
            .bindings
            .iter()
            .copied()
            .find(|&index| &self.bindings[index].source == identifier)
    }

    fn resolve_alias(&self, index: CrateIndex) -> CrateIndex {
        use BindingKind::*;

        match self.bindings[index].kind {
            Value | Module(_) => index,
            Use(target) => target,
        }
    }

    // @Task
    // @Note if it's a Debruijn-Indexed id, return early with it
    // @Note prob it's binder &mut Identifier
    // @Task adopt ResolutionTarget for second pass also
    // @Task we probably need to check again for the target: OnlyValue
    // vs ValueOrModule
    fn resolve_binding_second_pass<T: ResolutionTarget>(
        &self,
        binder: Identifier,
        module: CrateIndex,
    ) -> Result<Identifier> {
        if binder.index != Index::None {
            return Ok(binder);
        }

        let index = self
            .unwrap_module_scope(module)
            .bindings
            .iter()
            .copied()
            .find(|&index| self.bindings[index].source == binder.source)
            .ok_or_else(|| {
                Diagnostic::new(
                    Level::Error,
                    Code::E021,
                    // @Task modify error message: if it's a simple path: "in this scope/module"
                    // and if it's a complex one: "in module `module`"
                    format!("binding `{}` is not defined in this scope", binder.source),
                )
                .with_span(binder.source.span)
            })?;

        Ok(Identifier::new(index, binder.source))
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

    fn resolve_index_none(_: &parser::Identifier) -> Result<Self::Output> {
        Err(Diagnostic::new(
            Level::Fatal,
            None,
            "cannot fully resolve order-independent things right now",
        ))
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

    fn resolve_index_none(identifier: &parser::Identifier) -> Result<Self::Output> {
        Ok(Identifier::new(Index::None, identifier.clone()))
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

    // @Temporary @Beacon
    fn resolve_index_none(identifier: &parser::Identifier) -> Result<Self::Output>;
}

impl Declaration<Path> {
    pub fn resolve(
        self,
        module: Option<CrateIndex>,
        scope: &mut CrateScope,
    ) -> Result<Declaration<Identifier>, Diagnostics> {
        let declaration = self.resolve_first_pass(module, scope)?;
        declaration.resolve_second_pass(module.unwrap_or(scope.root()), scope)
    }

    fn resolve_first_pass(
        self,
        module: Option<CrateIndex>,
        scope: &mut CrateScope,
    ) -> Result<Declaration<Identifier>, Diagnostics> {
        use DeclarationKind::*;

        match self.kind {
            Value(declaration) => {
                let module = module.unwrap();

                let binder = scope
                    .register_value_binding(declaration.binder, module)
                    .many_err();

                if scope.program_entry.is_none() && module == scope.root() {
                    if let Ok(binder) = &binder {
                        if &binder.source.atom == PROGRAM_ENTRY_IDENTIFIER {
                            scope.program_entry = Some(binder.clone());
                        }
                    }
                }

                let type_annotation = declaration
                    .type_annotation
                    .resolve_first_pass(&FunctionScope::Module(module), scope)
                    .many_err();

                let expression = declaration
                    .expression
                    .map(|expression| {
                        expression
                            .resolve_first_pass(&FunctionScope::Module(module), scope)
                            .many_err()
                    })
                    .transpose();

                let span = self.span;
                let attributes = self.attributes;

                (binder, type_annotation, expression).handle(
                    |binder, type_annotation, expression| {
                        decl! {
                            Value[span][attributes] {
                                binder,
                                type_annotation,
                                expression,
                            }
                        }
                    },
                )
            }
            Data(declaration) => {
                let module = module.unwrap();

                let binder = scope
                    .register_value_binding(declaration.binder, module)
                    .many_err();

                let type_annotation = declaration
                    .type_annotation
                    .resolve_first_pass(&FunctionScope::Module(module), scope)
                    .many_err();

                let constructors = declaration.constructors.map(|constructors| {
                    constructors
                        .into_iter()
                        .map(|constructor| constructor.resolve_first_pass(Some(module), scope))
                        .collect()
                });

                let span = self.span;
                let attributes = self.attributes;

                (binder, type_annotation, constructors.transpose()).handle(
                    |data_binder, type_annotation, constructors| {
                        decl! {
                            Data[span][attributes] {
                                binder: data_binder,
                                constructors,
                                type_annotation,
                            }
                        }
                    },
                )
            }
            Constructor(declaration) => {
                let module = module.unwrap();

                let binder = scope
                    .register_value_binding(declaration.binder, module)
                    .many_err();

                let type_annotation = declaration
                    .type_annotation
                    .resolve_first_pass(&FunctionScope::Module(module), scope)
                    .many_err();

                let span = self.span;
                let attributes = self.attributes;

                (binder, type_annotation).handle(|binder, type_annotation| {
                    decl! {
                        Constructor[span][attributes] {
                            binder,
                            type_annotation,
                        }
                    }
                })
            }
            Module(declaration) => {
                let binder = scope
                    .register_module_binding(declaration.binder, module)
                    .many_err()?;

                let declarations = declaration
                    .declarations
                    .into_iter()
                    .map(|declaration| {
                        declaration.resolve_first_pass(Some(binder.krate().unwrap()), scope)
                    })
                    .collect::<Vec<_>>()
                    .transpose()?;

                Ok(decl! {
                    Module[self.span][self.attributes] {
                        binder,
                        file: declaration.file,
                        declarations,
                    }
                })
            }
            // @Beacon @Beacon @Task don't actually register_use_binding since
            // there is no way to refer to it later: all the use binding are
            // directly resolved with CrateScope::resolve_alias
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
                    .resolve_binding_first_pass::<ValueOrModule>(&declaration.binder, module)
                    .many_err()?;

                let binder = scope
                    .register_use_binding(binder.clone(), target, module)
                    .many_err()?;

                Ok(decl! { Use[self.span][self.attributes] { binder } })
            }
        }
    }
}

impl Declaration<Identifier> {
    fn resolve_second_pass(
        self,
        module: CrateIndex,
        scope: &CrateScope,
    ) -> Result<Self, Diagnostics> {
        use DeclarationKind::*;

        match self.kind {
            Value(declaration) => {
                let type_annotation = declaration
                    .type_annotation
                    .resolve_second_pass(module, scope)
                    .many_err();

                let expression = declaration
                    .expression
                    .map(|expression| expression.resolve_second_pass(module, scope).many_err())
                    .transpose();

                let binder = declaration.binder;
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
                let type_annotation = declaration
                    .type_annotation
                    .resolve_second_pass(module, scope)
                    .many_err();

                let constructors = declaration.constructors.map(|constructors| {
                    constructors
                        .into_iter()
                        .map(|constructor| constructor.resolve_second_pass(module, scope))
                        .collect()
                });

                let binder = declaration.binder;
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
                let type_annotation = declaration
                    .type_annotation
                    .resolve_second_pass(module, scope)
                    .many_err()?;

                Ok(decl! {
                    Constructor[self.span][self.attributes] {
                        binder: declaration.binder,
                        type_annotation,
                    }
                })
            }
            Module(declaration) => {
                let binder = declaration.binder;

                let declarations = declaration
                    .declarations
                    .into_iter()
                    .map(|declaration| {
                        declaration.resolve_second_pass(binder.krate().unwrap(), scope)
                    })
                    .collect::<Vec<_>>()
                    .transpose()?;

                Ok(decl! {
                    Module[self.span][self.attributes] {
                        binder,
                        file: declaration.file,
                        declarations,
                    }
                })
            }
            Use(declaration) => {
                // @Task even if this does not resolve, register the binder (last segment) into
                // this module to prevent unnecessary consequential errors
                let _target = scope
                    .resolve_binding_second_pass::<ValueOrModule>(
                        declaration.binder.clone(),
                        module,
                    )
                    .many_err()?;

                // @Note we do something different and use `target` from above

                Ok(decl! {
                    Use[self.span][self.attributes] { binder: declaration.binder }
                })
            }
        }
    }
}

// @Task @Beacon use Rc::try_unwrap more instead of clone
impl Expression<Path> {
    fn resolve_first_pass(
        self,
        scope: &FunctionScope<'_>,
        crate_scope: &CrateScope,
    ) -> Result<Expression<Identifier>> {
        use ExpressionKind::*;

        Ok(match self.kind {
            PiType(pi) => {
                expr! {
                    PiType[self.span] {
                        parameter: pi.parameter.clone().map(|parameter| Identifier::new(Index::DebruijnParameter, parameter.clone())),
                        domain: pi.domain.clone().resolve_first_pass(scope, crate_scope)?,
                        codomain: match pi.parameter.clone() {
                            Some(parameter) => pi.codomain.clone().resolve_first_pass(&scope.extend(parameter), crate_scope)?,
                            None => pi.codomain.clone().resolve_first_pass(scope, crate_scope)?,
                        },
                        explicitness: pi.explicitness,
                    }
                }
            }
            Application(application) => expr! {
                Application[self.span] {
                    callee: application.callee.clone().resolve_first_pass(scope, crate_scope)?,
                    argument: application.argument.clone().resolve_first_pass(scope, crate_scope)?,
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
                    binder: scope.resolve_binding_first_pass(&binding.binder, crate_scope)?,
                }
            },
            Lambda(lambda) => expr! {
                Lambda[self.span] {
                    parameter: Identifier::new(Index::DebruijnParameter, lambda.parameter.clone()),
                    parameter_type_annotation: lambda.parameter_type_annotation.clone()
                        .map(|r#type| r#type.resolve_first_pass(scope, crate_scope))
                        .transpose()?,
                    body_type_annotation: lambda.body_type_annotation.clone()
                        .map(|r#type| r#type.resolve_first_pass(&scope.extend(lambda.parameter.clone()), crate_scope))
                        .transpose()?,
                    body: lambda.body.clone().resolve_first_pass(&scope.extend(lambda.parameter.clone()), crate_scope)?,
                    explicitness: lambda.explicitness,
                }
            },
            UseIn => todo!("resolving use/in"),
            CaseAnalysis(_expression) => todo!("resolving case analysis"),
            Substitution(_) | ForeignApplication(_) => unreachable!(),
        })
    }
}

// @Note cannot abstracto over Pass…rust's type system is too weak
// @Task @Beacon use Rc::try_unwrap more instead of clone
impl Expression<Identifier> {
    fn resolve_second_pass(
        self,
        module: CrateIndex,
        crate_scope: &CrateScope,
    ) -> Result<Expression<Identifier>> {
        use ExpressionKind::*;

        Ok(match self.kind {
            PiType(pi) => {
                expr! {
                    PiType[self.span] {
                        parameter: pi.parameter.clone(),
                        domain: pi.domain.clone().resolve_second_pass(module, crate_scope)?,
                        codomain: pi.codomain.clone().resolve_second_pass(module, crate_scope)?,
                        explicitness: pi.explicitness,
                    }
                }
            }
            Application(application) => expr! {
                Application[self.span] {
                    callee: application.callee.clone().resolve_second_pass(module, crate_scope)?,
                    argument: application.argument.clone().resolve_second_pass(module, crate_scope)?,
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
                    binder: crate_scope.resolve_binding_second_pass::<OnlyValue>(binding.binder.clone(), module)?,
                }
            },
            Lambda(lambda) => expr! {
                Lambda[self.span] {
                    parameter: lambda.parameter.clone(),
                    parameter_type_annotation: lambda.parameter_type_annotation.clone()
                        .map(|r#type| r#type.resolve_second_pass(module, crate_scope))
                        .transpose()?,
                    body_type_annotation: lambda.body_type_annotation.clone()
                        .map(|r#type| r#type.resolve_second_pass(module, crate_scope))
                        .transpose()?,
                    body: lambda.body.clone().resolve_second_pass(module, crate_scope)?,
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

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.source)?;
        // @Temporary
        // #[cfg(FALSE)]
        {
            f.write_str("#")?;
            match self.index {
                Index::Crate(index) => write!(f, "{}M", index.0)?,
                Index::Debruijn(index) => write!(f, "{}F", index.0)?,
                Index::DebruijnParameter => f.write_str("P")?,
                Index::None => f.write_str("N")?,
            }
        }
        Ok(())
    }
}

impl Binder for Identifier {
    type Simple = Self;
    type Pattern = Self;
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Index {
    Crate(CrateIndex),
    None,
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
            Self::DebruijnParameter | Self::None => unreachable!(),
        }
    }

    fn unshift(self) -> Self {
        match self {
            Self::Crate(_) => self,
            Self::Debruijn(index) => DebruijnIndex(index.0.saturating_sub(1)).into(),
            Self::DebruijnParameter | Self::None => unreachable!(),
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

    fn resolve_binding_first_pass(
        &self,
        query: &Path,
        crate_scope: &CrateScope,
    ) -> Result<Identifier> {
        self.resolve_binding_first_pass_with_depth(query, crate_scope, 0)
    }

    fn resolve_binding_first_pass_with_depth(
        &self,
        query: &Path,
        crate_scope: &CrateScope,
        depth: usize,
    ) -> Result<Identifier> {
        match self {
            FunctionScope::Module(module) => {
                crate_scope.resolve_binding_first_pass::<OnlyValue>(query, *module)
            }
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
                        parent.resolve_binding_first_pass_with_depth(query, crate_scope, depth + 1)
                    }
                } else {
                    crate_scope.resolve_binding_first_pass::<OnlyValue>(query, parent.module())
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

// @Task later: Undefined, Untyped
// @Note corresponds to [crate::interpreter::scope::Entity].
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
