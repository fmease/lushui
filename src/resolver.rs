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
//! * create Debruijn-indices for local names

use std::rc::Rc;

use crate::{
    desugar::{
        self, expr, Binder, Constructor, Declaration, DeclarationKind, Expression, ExpressionKind,
    },
    diagnostic::{Diagnostic, Level},
    parser,
    support::{Handle2Ext, Handle3Ext, TransposeExt},
};

// @Beacon @Beacon @Beacon @Task
// * order-indepedence

// @Task use SmallVec<[Diagnostic; 1]> instead of Vec
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
                    .resolve(&Scope::new(scope, &FunctionScope::Empty))
                    .map_err(|error| vec![error]);

                let expression = value
                    .expression
                    .resolve(&Scope::new(scope, &FunctionScope::Empty))
                    .map_err(|error| vec![error]);

                let span = self.span;

                (binder, type_annotation, expression).handle(
                    |binder, type_annotation, expression| Declaration {
                        kind: DeclarationKind::Value(Box::new(desugar::declaration::Value {
                            binder,
                            type_annotation,
                            expression,
                        })),
                        span,
                    },
                )
            }
            Data(data) => {
                let data_binder = scope
                    .insert_binding(data.binder)
                    .map_err(|error| vec![error]);

                let type_annotation = data
                    .type_annotation
                    .resolve(&Scope::new(scope, &FunctionScope::Empty))
                    .map_err(|error| vec![error]);

                let mut constructors = Vec::new();

                for constructor in data.constructors {
                    let constructor_binder = scope
                        .insert_binding(constructor.binder)
                        .map_err(|error| vec![error]);

                    let type_annotation = constructor
                        .type_annotation
                        .resolve(&Scope::new(scope, &FunctionScope::Empty))
                        .map_err(|error| vec![error]);

                    let span = constructor.span;

                    constructors.push((constructor_binder, type_annotation).handle(
                        |constructor_binder, type_annotation| Constructor {
                            binder: constructor_binder,
                            span,
                            type_annotation,
                        },
                    ));
                }

                let span = self.span;

                (data_binder, type_annotation, constructors.transpose()).handle(
                    |data_binder, type_annotation, constructors| Declaration {
                        kind: DeclarationKind::Data(Box::new(desugar::declaration::Data {
                            binder: data_binder,
                            constructors,
                            type_annotation,
                        })),
                        span,
                    },
                )
            }
            Module(module) => {
                let declarations = module
                    .declarations
                    .into_iter()
                    .map(|declaration| declaration.resolve(scope))
                    .collect::<Vec<_>>()
                    .transpose()?;

                Ok(Declaration {
                    kind: DeclarationKind::Module(Box::new(desugar::declaration::Module {
                        declarations,
                    })),
                    span: self.span,
                })
            }
            Use => todo!(),
            Foreign(foreign) => {
                let binder = scope
                    .insert_binding(foreign.binder)
                    .map_err(|error| vec![error]);

                let type_annotation = foreign
                    .type_annotation
                    .resolve(&Scope::new(scope, &FunctionScope::Empty))
                    .map_err(|error| vec![error]);

                let span = self.span;

                (binder, type_annotation).handle(|binder, type_annotation| Declaration {
                    kind: DeclarationKind::Foreign(Box::new(desugar::declaration::Foreign {
                        binder,
                        type_annotation,
                    })),
                    span,
                })
            }
        }
    }
}

// @Task @Beacon use Rc::try_unwrap more instead of clone
impl Expression<parser::Identifier> {
    pub fn resolve(self, scope: &Scope<'_>) -> Result<Expression<Identifier>, Diagnostic> {
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
                            Some(parameter) => pi.codomain.clone().resolve(&Scope { function: &scope.function.extend(parameter), module: scope.module })?,
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
            NatType => expr! { NatType[self.span] },
            TextType => expr! { TextType[self.span] },
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
                    binder: match scope.function.index(&binding.binder) {
                        Some(index) => Identifier { source: binding.binder.clone(), index: Index::Debruijn(index) },
                        None => scope.module.lookup_binding(&binding.binder)?,
                    }
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
                        .map(|r#type| r#type.resolve(&Scope { function: &scope.function.extend(lambda.parameter.clone()), module: scope.module }))
                        .transpose()?,
                    body: lambda.body.clone().resolve(&Scope { function: &scope.function.extend(lambda.parameter.clone()), module: scope.module })?,
                    explicitness: lambda.explicitness,
                }
            },
            UseIn => todo!(),
            CaseAnalysis(_expression) => todo!(),
            Substitution(_) | UnsaturatedForeignApplication(_) => unreachable!(),
        })
    }
}

pub struct Scope<'a> {
    module: &'a ModuleScope,
    function: &'a FunctionScope<'a>,
}

impl<'a> Scope<'a> {
    fn new(module: &'a ModuleScope, function: &'a FunctionScope<'a>) -> Self {
        Self { module, function }
    }
}

#[derive(Default)]
pub struct ModuleScope {
    bindings: Vec<Binding>,
}

impl ModuleScope {
    fn insert_binding(&mut self, identifier: parser::Identifier) -> Result<Identifier, Diagnostic> {
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

    fn lookup_binding(&self, identifier: &parser::Identifier) -> Result<Identifier, Diagnostic> {
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
    pub fn local(identifier: &Identifier) -> Self {
        Self {
            source: identifier.source.clone(),
            index: Index::Debruijn(DebruijnIndex { value: 0 }),
        }
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
    value: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DebruijnIndex {
    value: usize,
}

use std::fmt;

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.source)?;
        match self.index {
            Index::Module(index) => write!(f, "#{}M", index.value),
            Index::Debruijn(index) => write!(f, "#{}F", index.value),
            Index::None => Ok(()),
        }
    }
}

pub enum FunctionScope<'parent> {
    Empty,
    Binding {
        parent: &'parent FunctionScope<'parent>,
        binder: parser::Identifier,
    },
}

impl<'parent> FunctionScope<'parent> {
    pub fn extend(&'parent self, binder: parser::Identifier) -> Self {
        Self::Binding {
            parent: self,
            binder,
        }
    }

    fn index(&self, query: &parser::Identifier) -> Option<DebruijnIndex> {
        fn index(
            scope: &FunctionScope<'_>,
            query: &parser::Identifier,
            depth: usize,
        ) -> Option<usize> {
            match scope {
                FunctionScope::Empty => None,
                FunctionScope::Binding { parent, binder } => {
                    if binder == query {
                        Some(depth)
                    } else {
                        index(parent, query, depth + 1)
                    }
                }
            }
        }

        index(self, query, 0).map(|index| DebruijnIndex { value: index })
    }
}

#[derive(Clone)]
pub struct Binding {
    /// Source at the def-site
    source: parser::Identifier,
}
