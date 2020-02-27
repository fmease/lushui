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

// @Temporary
#![allow(dead_code, unused_imports, unreachable_code, unused_variables)]

use std::rc::Rc;

use crate::{
    desugar::{self, expr, Binder, Declaration, DeclarationKind, Expression, ExpressionKind},
    diagnostic::{self, Diagnostic},
    parser,
};

// enum Error {
//     MultipleDefinitions,
//     UseOfUndefined,
// }

// @Temporary signature and code
pub fn resolve_declaration(
    map: &mut Map,
    declaration: Declaration<parser::Identifier>,
) -> Result<Declaration<Identifier>, Vec<Diagnostic>> {
    Ok(match declaration.kind {
        DeclarationKind::Value(value) => {
            let binder = map
                .register_binding(value.binder)
                .map_err(|error| vec![error])?;

            Declaration {
                kind: DeclarationKind::Value(Box::new(desugar::declaration::Value {
                    binder,
                    // @Bug @Temporary yikes just for prototyping
                    type_annotation: unsafe { std::mem::transmute(value.type_annotation) },
                    expression: unsafe { std::mem::transmute(value.expression) },
                })),
                span: declaration.span,
            }
        }
        DeclarationKind::Data(declaration) => todo!(),
        DeclarationKind::Module(module) => {
            let mut results = Ok(Vec::new());

            for result in module
                .declarations
                .into_iter()
                .map(|declaration| resolve_declaration(map, declaration))
            {
                match &mut results {
                    Ok(declarations) => match result {
                        Ok(declaration) => declarations.push(declaration),
                        Err(errors) => results = Err(errors),
                    },
                    Err(previous_errors) => match result {
                        Ok(_) => {}
                        Err(errors) => previous_errors.extend(errors),
                    },
                }
            }

            Declaration {
                kind: DeclarationKind::Module(Box::new(desugar::declaration::Module {
                    declarations: results?,
                })),
                span: declaration.span,
            }
        }
        DeclarationKind::Use => todo!(),
        DeclarationKind::Foreign(declaration) => todo!(),
    })
}

// @Note @Bug we might be on the road of mixing up local bindings and module items
pub fn resolve_expression(
    map: &mut Map,
    expression: Expression<parser::Identifier>,
) -> Result<Expression<Identifier>, ()> {
    Ok(match expression.kind {
        ExpressionKind::PiTypeLiteral(literal) => todo!(),
        ExpressionKind::Application(application) => todo!(),
        ExpressionKind::TypeLiteral => expr! { TypeLiteral[expression.span] },
        ExpressionKind::NatTypeLiteral => expr! { NatTypeLiteral[expression.span] },
        ExpressionKind::TextTypeLiteral => expr! { TextTypeLiteral[expression.span] },
        ExpressionKind::NatLiteral(literal) => {
            expr! {
                NatLiteral[expression.span] {
                    value: Rc::try_unwrap(literal)
                        .map(|literal| literal.value)
                        .unwrap_or_else(|literal| literal.value.clone()),
                    _marker: std::marker::PhantomData,
                }
            }
        }
        ExpressionKind::TextLiteral(literal) => {
            expr! {
                TextLiteral[expression.span] {
                    value: Rc::try_unwrap(literal)
                        .map(|literal| literal.value)
                        .unwrap_or_else(|literal| literal.value.clone()),
                    _marker: std::marker::PhantomData,
                }
            }
        }
        ExpressionKind::Binding(binding) => todo!(),
        ExpressionKind::LambdaLiteral(literal) => todo!(),
        ExpressionKind::UseIn => todo!(),
        ExpressionKind::CaseAnalysis(expression) => todo!(),
        ExpressionKind::UnsaturatedForeignApplication(application) => unreachable!(),
    })
}

use std::collections::HashMap;

// @Note @Temporary design until we know what we are doing
#[derive(Default)]
pub struct Map {
    local_bindings: HashMap<parser::Identifier, Binding>,
    bindings: Vec<Binding>,
}

impl Map {
    // @Temporary signature
    fn register_binding(
        &mut self,
        identifier: parser::Identifier,
    ) -> Result<Identifier, Diagnostic> {
        let index = self.bindings.len();
        let binding = Binding {
            kind: BindingKind::Global,
            source: identifier.clone(),
        };
        if let Some(old) = self
            .local_bindings
            .insert(identifier.clone(), binding.clone())
        {
            // @Task add subdiagnostic/secondary span which points to first definition
            return Err(Diagnostic::error(
                format!("`{}` is defined multiple times", identifier),
                identifier.span,
            ));
        }
        self.bindings.push(binding);
        Ok(Identifier { index })
    }

    // @Question leaking implementation details?
    fn resolve_binding(&self, identifier: Identifier) -> &Binding {
        &self.bindings[identifier.index]
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct Identifier {
    index: usize,
}

impl Binder for Identifier {}

use std::fmt;

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "~{}", self.index)
    }
}

#[derive(Clone)]
pub struct Binding {
    kind: BindingKind,
    source: parser::Identifier,
}

// @Note bad naming, @Temporary design
#[derive(Clone)]
pub enum BindingKind {
    Global,
    Local(LocalBinding),
}

// @Note in the future with `index: DebruijnIndex` for locally "nameless"
#[derive(Clone)]
pub enum LocalBinding {
    Sourced,
    Synthetic { generation: u64 },
}
