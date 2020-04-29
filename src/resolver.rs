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

use indexed_vec::IndexVec;
use std::rc::Rc;

use crate::{
    diagnostic::*,
    hir::{decl, expr, Binder, Declaration, DeclarationKind, Expression, ExpressionKind},
    parser,
    span::SourceMap,
    support::{handle::*, ManyErrExt, TransposeExt},
};

pub struct Crate {
    _name: parser::Identifier,
    /// All bindings inside of a crate.
    ///
    /// The first element is always the root module.
    bindings: IndexVec<CrateIndex, Binding>,
}

impl Crate {
    pub fn new(name: parser::Identifier) -> Self {
        let mut bindings = IndexVec::new();

        bindings.push(Binding {
            source: name.clone(),
            kind: BindingKind::Module(ModuleScope {
                bindings: Vec::new(),
            }),
        });

        Self {
            _name: name,
            bindings,
        }
    }

    pub fn root(&self) -> CrateIndex {
        CrateIndex(0)
    }

    // @Temporary needs better API
    fn lookup_module_bindings(&self, index: CrateIndex) -> &[CrateIndex] {
        match &self.bindings[index].kind {
            BindingKind::Module(scope) => &scope.bindings,
            _ => unreachable!(),
        }
    }

    fn lookup_module_bindings_mut(&mut self, index: CrateIndex) -> &mut Vec<CrateIndex> {
        match &mut self.bindings[index].kind {
            BindingKind::Module(scope) => &mut scope.bindings,
            _ => unreachable!(),
        }
    }

    // @Task error if module used as value (maybe)
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
            module,
        )
    }

    pub fn register_module_binding(
        &mut self,
        binder: parser::Identifier,
        module: CrateIndex,
    ) -> Result<Identifier> {
        self.register_binding(
            binder.clone(),
            Binding {
                source: binder,
                kind: BindingKind::Module(ModuleScope::default()),
            },
            module,
        )
    }

    fn register_binding(
        &mut self,
        binder: parser::Identifier,
        binding: Binding,
        module: CrateIndex,
    ) -> Result<Identifier> {
        {
            if let Some(previous) = self
                .lookup_module_bindings(module)
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
        self.lookup_module_bindings_mut(module).push(index);

        Ok(Identifier::new(index, binder))
    }

    // @Note @Beacon @Beacon missing: resolving parser::Path to self::Identifier

    // @Task error if module used as value (maybe)
    // @Task @Beacon @Beacon @Beacon
    // @Question rename to resolve_value_binding?
    // @Question should we only handle value bindings here and disallow module bindings?
    // @Note dependends on where we want to check that we don't use a module as value
    /// Try to resolve a textual identifier to a resolved one.
    fn resolve_binding(
        &self,
        identifier: &parser::Identifier,
        module: CrateIndex,
    ) -> Result<Identifier> {
        self.lookup_module_bindings(module)
            .iter()
            .map(|&index| &self.bindings[index])
            .position(|binding| &binding.source == identifier)
            .map(|index| Identifier::new(CrateIndex(index), identifier.clone()))
            .ok_or_else(|| {
                Diagnostic::new(
                    Level::Error,
                    Code::E021,
                    format!("binding `{}` is not defined in this scope", identifier),
                )
                .with_span(identifier.span)
            })
    }
}

impl Declaration<parser::Path> {
    pub fn resolve(
        self,
        parent: CrateIndex,
        krate: &mut Crate,
        map: &mut SourceMap,
    ) -> Result<Declaration<Identifier>, Diagnostics> {
        use DeclarationKind::*;

        match self.kind {
            Value(value) => {
                let binder = krate
                    .register_value_binding(value.binder, parent)
                    .many_err();

                let type_annotation = value
                    .type_annotation
                    .resolve(&FunctionScope::Module(parent), krate)
                    .many_err();

                let expression = value
                    .expression
                    .map(|expression| {
                        expression
                            .resolve(&FunctionScope::Module(parent), krate)
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
            Data(data) => {
                let data_binder = krate.register_value_binding(data.binder, parent).many_err();

                let type_annotation = data
                    .type_annotation
                    .resolve(&FunctionScope::Module(parent), krate)
                    .many_err();

                let constructors = data.constructors.map(|constructors| {
                    constructors
                        .into_iter()
                        .map(|constructor| constructor.resolve(parent, krate, map))
                        .collect()
                });

                let span = self.span;
                let attributes = self.attributes;

                (data_binder, type_annotation, constructors.transpose()).handle(
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
            Constructor(constructor) => {
                let binder = krate
                    .register_value_binding(constructor.binder, parent)
                    .many_err();

                let type_annotation = constructor
                    .type_annotation
                    .resolve(&FunctionScope::Module(parent), krate)
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
            Module(module) => {
                // @Bug file modules (created by parse_file_module_no_header) are the file name,
                // maybe buggy behavior?
                // @Task don't use the try operator here, make it non-fatal @Temporary try
                let binder = krate
                    .register_module_binding(module.binder, parent)
                    .many_err()?;

                let declarations = match module.declarations {
                    Some(declarations) => declarations,
                    None => {
                        let path = std::path::Path::new(&module.file.name)
                            .ancestors()
                            .nth(1)
                            .unwrap()
                            .join(&format!("{}.lushui", binder.source.atom));
                        let file = map.load(path.to_str().unwrap()).many_err()?;
                        let tokens = crate::lexer::Lexer::new(&file).lex()?;
                        let node = parser::Parser::new(file, &tokens)
                            .parse_top_level()
                            .many_err()?;
                        let node = node.desugar()?;
                        let module = match node.kind {
                            Module(module) => module,
                            _ => unreachable!(),
                        };
                        if !node.attributes.is_empty() {
                            Diagnostic::new(
                                Level::Warning,
                                None,
                                "attributes on module headers are ignored right now",
                            )
                            .emit(None);
                        }
                        module.declarations.unwrap()
                    }
                };

                // @Temporary: if let Some
                let declarations = declarations
                    .into_iter()
                    .map(|declaration| declaration.resolve(binder.krate().unwrap(), krate, map))
                    .collect::<Vec<_>>()
                    .transpose()?;

                Ok(decl! {
                    Module[self.span][self.attributes] {
                        binder,
                        file: module.file,
                        declarations: Some(declarations),
                    }
                })
            }
            Use => todo!("resolving use declaration"),
        }
    }
}

// @Task @Beacon use Rc::try_unwrap more instead of clone
impl Expression<parser::Path> {
    pub fn resolve(
        self,
        scope: &FunctionScope<'_>,
        krate: &Crate,
    ) -> Result<Expression<Identifier>> {
        use ExpressionKind::*;

        Ok(match self.kind {
            PiType(pi) => {
                expr! {
                    PiType[self.span] {
                        parameter: pi.parameter.clone().map(|parameter| Identifier::new(Index::None, parameter.clone())),
                        domain: pi.domain.clone().resolve(scope, krate)?,
                        codomain: match pi.parameter.clone() {
                            Some(parameter) => pi.codomain.clone().resolve(&scope.extend(parameter), krate)?,
                            None => pi.codomain.clone().resolve(scope, krate)?,
                        },
                        explicitness: pi.explicitness,
                    }
                }
            }
            Application(application) => expr! {
                Application[self.span] {
                    callee: application.callee.clone().resolve(scope, krate)?,
                    argument: application.argument.clone().resolve(scope, krate)?,
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
            Binding(binding) => {
                // @Beacon @Beacon @Temporary handling of complex paths: resolve em!!
                // @Task implement it
                if binding.binder.head.is_some() || binding.binder.segments.len() != 1 {
                    panic!("complex paths cannot be handled by the resolver yet");
                }

                expr! {
                    Binding[self.span] {
                        binder: scope.resolve_binding(&binding.binder.segments[0], krate)?,
                    }
                }
            }
            Lambda(lambda) => expr! {
                Lambda[self.span] {
                    parameter: Identifier::new(Index::None, lambda.parameter.clone()),
                    parameter_type_annotation: lambda.parameter_type_annotation.clone()
                        .map(|r#type| r#type.resolve(scope, krate))
                        .transpose()?,
                    body_type_annotation: lambda.body_type_annotation.clone()
                        .map(|r#type| r#type.resolve(&scope.extend(lambda.parameter.clone()), krate))
                        .transpose()?,
                    body: lambda.body.clone().resolve(&scope.extend(lambda.parameter.clone()), krate)?,
                    explicitness: lambda.explicitness,
                }
            },
            UseIn => todo!("resolving use/in"),
            CaseAnalysis(_expression) => todo!("resolving case analysis"),
            Substitution(_) | ForeignApplication(_) => unreachable!(),
        })
    }
}

#[derive(Default)]
pub struct ModuleScope {
    // binder: ModuleIndex,
    // parent: Option<ModuleIndex>,
    bindings: Vec<CrateIndex>,
}

#[derive(Debug, Clone, PartialEq)]
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

    // @Note I dunno about this interface
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
        #[cfg(FALSE)]
        {
            match self.index {
                Index::Crate(index) => write!(f, "#{}M", index.0)?,
                Index::Debruijn(index) => write!(f, "#{}F", index.0)?,
                Index::None => (),
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
    Debruijn(DebruijnIndex),
    /// Dummy identifier for function parameters.
    ///
    /// Those "l-values" don't need to be referenceable by index in the world
    /// of Debruijn-indexing.
    None,
}

impl Index {
    // @Bug @Beacon @Beacon if the index is a module index, it should not be "shifted"
    // but replaced by a new module index @Update idk what you are talking about, past self!
    fn shift(self, amount: usize) -> Self {
        match self {
            Self::Crate(_) => self,
            Self::None => unreachable!(),
            Self::Debruijn(index) => DebruijnIndex(index.0 + amount).into(),
        }
    }

    fn unshift(self) -> Self {
        match self {
            Self::Crate(_) => self,
            Self::None => unreachable!(),
            Self::Debruijn(index) => DebruijnIndex(index.0.saturating_sub(1)).into(),
        }
    }
}

// @Question should the indices *really* be public?

/// Crate-local identifier for bindings defined through declarations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CrateIndex(pub usize);

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

    // @Task @Beacon @Beacon allow complex paths (parser::Path)

    fn resolve_binding(&self, query: &parser::Identifier, krate: &Crate) -> Result<Identifier> {
        fn resolve_binding(
            scope: &FunctionScope<'_>,
            query: &parser::Identifier,
            krate: &Crate,
            depth: usize,
        ) -> Result<Identifier> {
            match scope {
                FunctionScope::Module(module) => krate.resolve_binding(query, *module),
                FunctionScope::Binding { parent, binder } => {
                    if binder == query {
                        Ok(Identifier::new(DebruijnIndex(depth), query.clone()))
                    } else {
                        resolve_binding(parent, query, krate, depth + 1)
                    }
                }
            }
        }

        resolve_binding(self, query, krate, 0)
    }
}

// @Task redesign this stufff!!!

pub struct Binding {
    /// Source information of the definition site.
    source: parser::Identifier,
    kind: BindingKind,
}

// @Task later: Undefined, Untyped
enum BindingKind {
    Value,
    Module(ModuleScope),
}
