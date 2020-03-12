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
    support::{Handle2ResultsExt as _, Handle3ResultsExt as _, TransposeVecResultVecExt as _},
};

// @Beacon @Beacon @Beacon @Task
// * order-indepedence

impl Declaration<parser::Identifier> {
    pub fn resolve(
        self,
        module_scope: &mut ModuleScope,
    ) -> Result<Declaration<Identifier>, Vec<Diagnostic>> {
        match self.kind {
            DeclarationKind::Value(value) => {
                let binder = module_scope
                    .insert_binding(value.binder)
                    .map_err(|error| vec![error]);

                let type_annotation = value
                    .type_annotation
                    .resolve(module_scope, &FunctionScope::Empty)
                    .map_err(|error| vec![error]);

                let expression = value
                    .expression
                    .resolve(module_scope, &FunctionScope::Empty)
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
            DeclarationKind::Data(data) => {
                let data_binder = module_scope
                    .insert_binding(data.binder)
                    .map_err(|error| vec![error]);

                let type_annotation = data
                    .type_annotation
                    .resolve(module_scope, &FunctionScope::Empty)
                    .map_err(|error| vec![error]);

                let mut constructors = Vec::new();

                for constructor in data.constructors {
                    let constructor_binder = module_scope
                        .insert_binding(constructor.binder)
                        .map_err(|error| vec![error]);

                    let type_annotation = constructor
                        .type_annotation
                        .resolve(module_scope, &FunctionScope::Empty)
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
            DeclarationKind::Module(module) => {
                let declarations = module
                    .declarations
                    .into_iter()
                    .map(|declaration| declaration.resolve(module_scope))
                    .collect::<Vec<_>>()
                    .transpose()?;

                Ok(Declaration {
                    kind: DeclarationKind::Module(Box::new(desugar::declaration::Module {
                        declarations,
                    })),
                    span: self.span,
                })
            }
            DeclarationKind::Use => todo!(),
            DeclarationKind::Foreign(foreign) => {
                let binder = module_scope
                    .insert_binding(foreign.binder)
                    .map_err(|error| vec![error]);

                let type_annotation = foreign
                    .type_annotation
                    .resolve(module_scope, &FunctionScope::Empty)
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
    // @Note we probably need to add Expression::LetIn and replace desugaring code
    pub fn resolve(
        self,
        module_scope: &mut ModuleScope,
        function_scope: &FunctionScope<'_>,
    ) -> Result<Expression<Identifier>, Diagnostic> {
        Ok(match self.kind {
            ExpressionKind::PiType(pi) => expr! {
                PiType[self.span] {
                    parameter: pi.parameter.clone().map(|parameter| Identifier {
                        source: parameter.clone(),
                        index: Index::None,
                    }),
                    domain: pi.domain.clone().resolve(module_scope, function_scope)?,
                    codomain: match pi.parameter.clone() {
                        Some(parameter) => pi.codomain.clone().resolve(module_scope, &function_scope.extend(parameter))?,
                        None => pi.codomain.clone().resolve(module_scope, function_scope)?,
                    },
                    explicitness: pi.explicitness,
                }
            },
            ExpressionKind::Application(application) => expr! {
                Application[self.span] {
                    callee: application.callee.clone().resolve(module_scope, function_scope)?,
                    argument: application.argument.clone().resolve(module_scope, function_scope)?,
                    explicitness: application.explicitness,
                }
            },
            ExpressionKind::Type => expr! { Type[self.span] },
            ExpressionKind::NatType => expr! { NatType[self.span] },
            ExpressionKind::TextType => expr! { TextType[self.span] },
            ExpressionKind::Nat(nat) => expr! {
                Nat[self.span] {
                    value: Rc::try_unwrap(nat)
                        .map(|nat| nat.value)
                        .unwrap_or_else(|nat| nat.value.clone()),
                    _marker: std::marker::PhantomData,
                }
            },
            ExpressionKind::Text(text) => expr! {
                Text[self.span] {
                    value: Rc::try_unwrap(text)
                        .map(|text| text.value)
                        .unwrap_or_else(|text| text.value.clone()),
                    _marker: std::marker::PhantomData,
                }
            },
            ExpressionKind::Binding(binding) => expr! {
                Binding[self.span] {
                    binder: match function_scope.index(&binding.binder) {
                        Some(index) => Identifier { source: binding.binder.clone(), index },
                        None => module_scope.lookup_binding(&binding.binder)?,
                    }
                }
            },
            ExpressionKind::Lambda(lambda) => expr! {
                Lambda[self.span] {
                    parameter: Identifier {
                        index: Index::None,
                        source: lambda.parameter.clone(),
                    },
                    parameter_type_annotation: lambda.parameter_type_annotation.clone()
                        .map(|r#type| r#type.resolve(module_scope, function_scope))
                        .transpose()?,
                    body_type_annotation: lambda.body_type_annotation.clone()
                        .map(|r#type| r#type.resolve(module_scope, &function_scope.extend(lambda.parameter.clone())))
                        .transpose()?,
                    body: lambda.body.clone().resolve(module_scope, &function_scope.extend(lambda.parameter.clone()))?,
                    explicitness: lambda.explicitness,
                }
            },
            ExpressionKind::UseIn => todo!(),
            ExpressionKind::CaseAnalysis(_expression) => todo!(),
            ExpressionKind::UnsaturatedForeignApplication(_) => unreachable!(),
        })
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
                format!("`{}` is defined multiple times", identifier),
            )
            .with_labeled_span(identifier.span, "redefinition")
            .with_labeled_span(old.source.span, "previous definition"));
        }
        self.bindings.push(binding);
        Ok(Identifier {
            index: Index::Module { value: index },
            source: identifier,
        })
    }

    fn lookup_binding(&self, identifier: &parser::Identifier) -> Result<Identifier, Diagnostic> {
        self.bindings
            .iter()
            .position(|binding| &binding.source == identifier)
            .map(|index| Identifier {
                source: identifier.clone(),
                index: Index::Module { value: index },
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

#[derive(Debug, Clone)]
pub struct Identifier {
    /// Source at the use-site/call-site or def-site if definition.
    source: parser::Identifier,
    index: Index,
}

// @Note it's kind of stupid to also store the index for defining binders because we'd need to
// keep them updated as well without any benefit
#[derive(Debug, Clone)]
enum Index {
    /// For bindings defined at module-scope.
    Module { value: usize },
    /// For bindings bound by a function parameter.
    Debruijn { value: usize },
    /// For function parameters.
    None,
}

impl Binder for Identifier {}

use std::fmt;

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.source)?;
        match self.index {
            Index::Module { value } => write!(f, "#{}M", value),
            Index::Debruijn { value } => write!(f, "#{}F", value),
            Index::None => Ok(()),
        }
    }
}

#[derive(Clone)]
pub struct Binding {
    /// Source at the def-site
    source: parser::Identifier,
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

    fn index(&self, query: &parser::Identifier) -> Option<Index> {
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

        index(self, query, 0).map(|index| Index::Debruijn { value: index })
    }
}
