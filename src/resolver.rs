//! Name resolver.
//!
//! This module lowers the desugared AST to the HIR (high-level intermediate representation).
//!
//! ## Future Features
//!
//! * resolve package imports (package declarations)
//! * resolve module imports (file system module declarations)
//! * resolve paths to an identifier consisting of a package-unique definiton identifier
//!   (which is mappable to the definition-site span) and a usage-site span
//! * resolve out-of-order declarations
//! * resolve recursive (value) declarations and prepare the resulting bindings
//!   for the type checker which should be able to type-check recursive functions
//!   and (!) equi-recursive types)
//! * gracefully handle cyclic dependencies
//! * handle module privacy (notably restricted exposure and good error messages)

use std::rc::Rc;

use crate::{
    diagnostic::*,
    hir::{decl, expr, Binder, Declaration, DeclarationKind, Expression, ExpressionKind},
    parser,
    support::{handle::*, TransposeExt},
};

// @Note the `handle` methods ignore whether an error is fatal or not, this should be changed @Task

// @Beacon @Beacon @Beacon @Task
// * order-indepedence

impl Declaration<parser::Identifier> {
    pub fn resolve(
        self,
        scope: &mut ModuleScope,
    ) -> Result<Declaration<Identifier>, Vec<Diagnostic>> {
        use DeclarationKind::*;

        match self.kind {
            Value(value) => {
                let binder = scope
                    .insert_binding(value.binder)
                    .map_err(|error| vec![error]);

                let type_annotation = value
                    .type_annotation
                    .resolve(&FunctionScope::Module(scope))
                    .map_err(|error| vec![error]);

                let expression = value
                    .expression
                    .map(|expression| {
                        expression
                            .resolve(&FunctionScope::Module(scope))
                            .map_err(|error| vec![error])
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
                let data_binder = scope
                    .insert_binding(data.binder)
                    .map_err(|error| vec![error]);

                let type_annotation = data
                    .type_annotation
                    .resolve(&FunctionScope::Module(scope))
                    .map_err(|error| vec![error]);

                let constructors = data.constructors.map(|constructors| {
                    constructors
                        .into_iter()
                        .map(|constructor| constructor.resolve(scope))
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
                let binder = scope
                    .insert_binding(constructor.binder)
                    .map_err(|error| vec![error]);

                let type_annotation = constructor
                    .type_annotation
                    .resolve(&FunctionScope::Module(scope))
                    .map_err(|error| vec![error]);

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
                let declarations = module
                    .declarations
                    .into_iter()
                    .map(|declaration| declaration.resolve(scope))
                    .collect::<Vec<_>>()
                    .transpose()?;

                Ok(decl! {
                    Module[self.span][self.attributes] {
                        declarations,
                    }
                })
            }
            Use => todo!("resolving use declaration"),
        }
    }
}

// @Task @Beacon use Rc::try_unwrap more instead of clone
impl Expression<parser::Identifier> {
    pub fn resolve(self, scope: &FunctionScope<'_>) -> Result<Expression<Identifier>> {
        use ExpressionKind::*;

        Ok(match self.kind {
            PiType(pi) => {
                expr! {
                    PiType[self.span] {
                        parameter: pi.parameter.clone().map(|parameter| Identifier {
                            source: parameter.clone(),
                            index: Index::None,
                        }),
                        domain: pi.domain.clone().resolve(scope)?,
                        codomain: match pi.parameter.clone() {
                            Some(parameter) => pi.codomain.clone().resolve(&scope.extend(parameter))?,
                            None => pi.codomain.clone().resolve(scope)?,
                        },
                        explicitness: pi.explicitness,
                    }
                }
            }
            Application(application) => expr! {
                Application[self.span] {
                    callee: application.callee.clone().resolve(scope)?,
                    argument: application.argument.clone().resolve(scope)?,
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
                    binder: scope.lookup_binding(&binding.binder)?,
                }
            },
            Lambda(lambda) => expr! {
                Lambda[self.span] {
                    parameter: Identifier {
                        index: Index::None,
                        source: lambda.parameter.clone(),
                    },
                    parameter_type_annotation: lambda.parameter_type_annotation.clone()
                        .map(|r#type| r#type.resolve(scope))
                        .transpose()?,
                    body_type_annotation: lambda.body_type_annotation.clone()
                        .map(|r#type| r#type.resolve(&scope.extend(lambda.parameter.clone())))
                        .transpose()?,
                    body: lambda.body.clone().resolve(&scope.extend(lambda.parameter.clone()))?,
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
    bindings: Vec<Binding>,
}

impl ModuleScope {
    fn insert_binding(&mut self, identifier: parser::Identifier) -> Result<Identifier> {
        let index = self.bindings.len();
        let binding = Binding {
            source: identifier.clone(),
        };
        if let Some(old) = self
            .bindings
            .iter()
            .find(|binding| binding.source == identifier)
        {
            return Err(Diagnostic::new(
                Level::Error,
                Code::E020,
                format!("`{}` is defined multiple times in this scope", identifier),
            )
            .with_labeled_span(identifier.span, "redefinition")
            .with_labeled_span(old.source.span, "previous definition"));
        }
        self.bindings.push(binding);
        Ok(Identifier {
            index: Index::Module(ModuleIndex { value: index }),
            source: identifier,
        })
    }

    fn lookup_binding(&self, identifier: &parser::Identifier) -> Result<Identifier> {
        self.bindings
            .iter()
            .position(|binding| &binding.source == identifier)
            .map(|index| Identifier {
                source: identifier.clone(),
                index: Index::Module(ModuleIndex { value: index }),
            })
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

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    /// Source at the use-site/call-site or def-site if definition.
    pub source: parser::Identifier,
    pub index: Index,
}

impl Identifier {
    // @Note I dunno about this interface
    pub fn local(identifier: &Self) -> Self {
        Self {
            source: identifier.source.clone(),
            index: Index::Debruijn(DebruijnIndex { value: 0 }),
        }
    }

    // @Task find better name which suggests Span
    pub fn dummified(self) -> Self {
        Self {
            source: self.source.dummified(),
            ..self
        }
    }

    pub fn is_innermost(&self) -> bool {
        matches!(self.index, Index::Debruijn(DebruijnIndex { value: 0 }))
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

    pub fn module(&self) -> Option<ModuleIndex> {
        match self.index {
            Index::Module(index) => Some(index),
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
                Index::Module(index) => write!(f, "#{}M", index.value)?,
                Index::Debruijn(index) => write!(f, "#{}F", index.value)?,
                Index::None => (),
            }
        }
        Ok(())
    }
}

impl Binder for Identifier {}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Index {
    /// For bindings defined at module-scope.
    Module(ModuleIndex),
    /// For bindings bound by a function parameter.
    Debruijn(DebruijnIndex),
    /// For function parameters.
    None,
}

impl Index {
    // @Bug @Beacon @Beacon if the index is a module index, it should not be "shifted"
    // but replaced by a new module index
    fn shift(self, amount: usize) -> Self {
        match self {
            // Self::Module { .. } | Self::None => self,
            Self::Module { .. } => self,
            // @Question is that the case?
            Self::None => unreachable!(),
            Self::Debruijn(index) => Self::Debruijn(DebruijnIndex {
                value: index.value + amount,
            }),
        }
    }

    fn unshift(self) -> Self {
        match self {
            // Self::Module { .. } | Self::None => self,
            Self::Module(_) => self,
            // @Question is that the case?
            Self::None => unreachable!(),
            Self::Debruijn(index) => Self::Debruijn(DebruijnIndex {
                value: index.value.saturating_sub(1),
            }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleIndex {
    pub value: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DebruijnIndex {
    pub value: usize,
}

pub enum FunctionScope<'a> {
    Module(&'a ModuleScope),
    Binding {
        parent: &'a FunctionScope<'a>,
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

    fn lookup_binding(&self, query: &parser::Identifier) -> Result<Identifier> {
        fn lookup_binding(
            scope: &FunctionScope<'_>,
            query: &parser::Identifier,
            depth: usize,
        ) -> Result<Identifier> {
            match scope {
                FunctionScope::Module(module) => module.lookup_binding(query),
                FunctionScope::Binding { parent, binder } => {
                    if binder == query {
                        Ok(Identifier {
                            source: query.clone(),
                            index: Index::Debruijn(DebruijnIndex { value: depth }),
                        })
                    } else {
                        lookup_binding(parent, query, depth + 1)
                    }
                }
            }
        }

        lookup_binding(self, query, 0)
    }
}

#[derive(Clone)]
pub struct Binding {
    /// Source at the def-site
    source: parser::Identifier,
}
