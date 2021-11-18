use super::{Expression, Substitution::Shift};
use crate::{
    diagnostics::Reporter,
    entity::EntityKind,
    error::Result,
    format::{AsDebug, DisplayWith},
    hir::expr,
    package::BuildSession,
    resolver::{Crate, DeBruijnIndex, Identifier},
    span::Span,
    syntax::lowered_ast::{AttributeKeys, Attributes},
};
use std::fmt;

impl Crate {
    // @Bug does not understand non-local binders
    // @Beacon @Beacon @Beacon @Task make this a method of Typer instead
    pub fn carry_out(
        &mut self,
        registration: BindingRegistration,
        session: &mut BuildSession,
        reporter: &Reporter,
    ) -> Result {
        use BindingRegistrationKind::*;

        match registration.kind {
            Value {
                binder,
                type_,
                value,
            } => {
                let index = binder.declaration_index().unwrap();
                // @Bug unwrap None reachable
                let index = self.local_index(index).unwrap();
                let entity = &mut self[index];
                debug_assert!(entity.is_untyped_value() || entity.is_value_without_value());

                entity.kind = EntityKind::Value {
                    type_,
                    expression: value,
                };
            }
            Data { binder, type_ } => {
                let index = binder.declaration_index().unwrap();
                // @Bug unwrap None reachable
                let index = self.local_index(index).unwrap();
                let entity = &mut self[index];
                debug_assert!(entity.is_untyped_value());

                entity.kind = EntityKind::DataType {
                    namespace: std::mem::take(entity.namespace_mut().unwrap()),
                    type_,
                    constructors: Vec::new(),
                };
            }
            Constructor {
                binder,
                type_,
                data,
            } => {
                let index = binder.declaration_index().unwrap();
                // @Bug unwrap None reachable
                let index = self.local_index(index).unwrap();
                let entity = &mut self[index];
                debug_assert!(entity.is_untyped_value());

                entity.kind = EntityKind::Constructor {
                    namespace: std::mem::take(entity.namespace_mut().unwrap()),
                    type_,
                };

                // @Bug unwrap None reachable
                let data_index = self.local_index(data.declaration_index().unwrap()).unwrap();

                match self[data_index].kind {
                    EntityKind::DataType {
                        ref mut constructors,
                        ..
                    } => constructors.push(binder),
                    _ => unreachable!(),
                }
            }
            IntrinsicFunction { binder, type_ } => {
                let index = binder.declaration_index().unwrap();
                // @Bug unwrap None reachable
                let index = self.local_index(index).unwrap();
                debug_assert!(self[index].is_untyped_value());

                self[index].kind = session.register_intrinsic_function(
                    binder,
                    type_,
                    registration
                        .attributes
                        .filter(AttributeKeys::INTRINSIC)
                        .next()
                        .unwrap(),
                    reporter,
                )?
            }
            IntrinsicType { binder } => session.register_intrinsic_type(
                binder,
                registration
                    .attributes
                    .filter(AttributeKeys::INTRINSIC)
                    .next()
                    .unwrap(),
                reporter,
            )?,
        }
        Ok(())
    }
}

#[derive(Clone)] // @Question expensive attributes clone?
pub struct BindingRegistration {
    pub attributes: Attributes,
    pub kind: BindingRegistrationKind,
}

#[derive(Clone)]
pub enum BindingRegistrationKind {
    Value {
        binder: Identifier,
        type_: Expression,
        value: Option<Expression>,
    },
    Data {
        binder: Identifier,
        type_: Expression,
    },
    Constructor {
        binder: Identifier,
        type_: Expression,
        data: Identifier,
    },
    IntrinsicFunction {
        binder: Identifier,
        type_: Expression,
    },
    IntrinsicType {
        binder: Identifier,
    },
}

impl DisplayWith for BindingRegistration {
    type Context<'a> = (&'a Crate, &'a BuildSession);

    fn format(&self, context: Self::Context<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BindingRegistrationKind::*;

        match &self.kind {
            Value {
                binder,
                type_,
                value,
            } => {
                let mut compound = f.debug_struct("Value");
                compound
                    .field("binder", binder)
                    .field("type", &type_.with(context).as_debug());
                match value {
                    Some(value) => compound.field("value", &value.with(context).as_debug()),
                    None => compound.field("value", &"?(none)"),
                }
                .finish()
            }
            Data { binder, type_ } => f
                .debug_struct("Data")
                .field("binder", binder)
                .field("type", &type_.with(context).as_debug())
                .finish(),
            Constructor {
                binder,
                type_,
                data,
            } => f
                .debug_struct("Constructor")
                .field("binder", binder)
                .field("type", &type_.with(context).as_debug())
                .field("data", data)
                .finish(),
            IntrinsicFunction { binder, type_ } => f
                .debug_struct("IntrinsicFunction")
                .field("binder", binder)
                .field("type", &type_.with(context).as_debug())
                .finish(),
            IntrinsicType { binder } => f
                .debug_struct("IntrinsicType")
                .field("binder", binder)
                .finish(),
        }
    }
}

// @Task find out if we can get rid of this type by letting `ModuleScope::lookup_value` resolve to the Binder
// if it's neutral
pub enum ValueView {
    Reducible(Expression),
    Neutral,
}

/// The scope of bindings inside of a function.
pub enum FunctionScope<'a> {
    Crate,
    FunctionParameter {
        parent: &'a Self,
        type_: Expression,
    },
    PatternBinders {
        parent: &'a Self,
        // @Note idk
        types: Vec<Expression>,
    },
}

impl<'a> FunctionScope<'a> {
    pub fn extend_with_parameter(&'a self, type_: Expression) -> Self {
        Self::FunctionParameter {
            parent: self,
            type_,
        }
    }

    pub fn extend_with_pattern_binders(&'a self, types: Vec<Expression>) -> Self {
        Self::PatternBinders {
            parent: self,
            types,
        }
    }

    pub(super) fn look_up_type(&self, index: DeBruijnIndex) -> Expression {
        self.look_up_type_with_depth(index, 0)
    }

    fn look_up_type_with_depth(&self, index: DeBruijnIndex, depth: usize) -> Expression {
        match self {
            Self::FunctionParameter { parent, type_ } => {
                if depth == index.0 {
                    expr! {
                        Substitution {
                            Attributes::default(), Span::default();
                            substitution: Shift(depth + 1),
                            expression: type_.clone(),
                        }
                    }
                } else {
                    parent.look_up_type_with_depth(index, depth + 1)
                }
            }
            Self::PatternBinders { parent, types } => {
                match types
                    .iter()
                    .rev()
                    .zip(depth..)
                    .find(|(_, depth)| *depth == index.0)
                {
                    Some((type_, depth)) => expr! {
                        Substitution {
                            Attributes::default(), Span::default();
                            // @Task verify this shift
                            substitution: Shift(depth + 1),
                            expression: type_.clone(),
                        }
                    },
                    None => parent.look_up_type_with_depth(index, depth + types.len()),
                }
            }
            Self::Crate => unreachable!(),
        }
    }
}
