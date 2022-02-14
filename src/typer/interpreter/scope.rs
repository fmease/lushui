use super::{Expression, Substitution::Shift};
use crate::{
    format::{AsDebug, DisplayWith},
    hir::{self, DeBruijnIndex, Identifier},
    package::BuildSession,
    resolver::Component,
    syntax::lowered_ast::Attributes,
};
use std::{default::default, fmt};

#[derive(Clone)] // @Question expensive attributes clone?
pub(crate) struct BindingRegistration {
    pub(crate) attributes: Attributes,
    pub(crate) kind: BindingRegistrationKind,
}

#[derive(Clone)]
pub(crate) enum BindingRegistrationKind {
    Function {
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
        owner_data_type: Identifier,
    },
    IntrinsicFunction {
        binder: Identifier,
        type_: Expression,
    },
}

// only used to report "cyclic" types (currently treated as a bug)
impl DisplayWith for BindingRegistration {
    type Context<'a> = (&'a Component, &'a BuildSession);

    fn format(&self, context: Self::Context<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BindingRegistrationKind::*;

        match &self.kind {
            Function {
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
                owner_data_type: data,
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
        }
    }
}

// @Task find out if we can get rid of this type by letting `ModuleScope::lookup_value` resolve to the Binder
// if it's neutral
pub(crate) enum ValueView {
    Reducible(Expression),
    Neutral,
}

impl ValueView {
    pub(crate) fn is_neutral(&self) -> bool {
        matches!(self, Self::Neutral)
    }
}

/// The scope of bindings inside of a function.
pub(crate) enum FunctionScope<'a> {
    Module,
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
    pub(crate) fn extend_with_parameter(&'a self, type_: Expression) -> Self {
        Self::FunctionParameter {
            parent: self,
            type_,
        }
    }

    pub(crate) fn extend_with_pattern_binders(&'a self, types: Vec<Expression>) -> Self {
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
                    Expression::new(
                        default(),
                        default(),
                        hir::Substitution {
                            substitution: Shift(depth + 1),
                            expression: type_.clone(),
                        }
                        .into(),
                    )
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
                    Some((type_, depth)) => Expression::new(
                        default(),
                        default(),
                        hir::Substitution {
                            // @Task verify this shift
                            substitution: Shift(depth + 1),
                            expression: type_.clone(),
                        }
                        .into(),
                    ),
                    None => parent.look_up_type_with_depth(index, depth + types.len()),
                }
            }
            Self::Module => unreachable!(),
        }
    }
}
