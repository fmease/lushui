//! Binding and scope handler.

use super::{ffi, Expression, Substitution::Shift};
use crate::{
    diagnostic::{Code, Diagnostic, Result},
    entity::EntityKind,
    hir::expr,
    lexer::Number,
    lowered_ast::Attributes,
    resolver::{CrateIndex, CrateScope, DebruijnIndex, Identifier, Index},
    span::Span,
    support::DisplayIsDebug,
};

/// Many methods of module scope panic instead of returning a `Result` because
/// the invariants are expected to be checked beforehand by using the predicate
/// methods also found here. This design is most ergonomic for the caller.
impl CrateScope {
    pub fn register_foreign_bindings(&mut self) {
        ffi::register_foreign_bindings(self);
    }

    pub fn lookup_type(&self, index: CrateIndex) -> Option<Expression> {
        self.bindings[index].type_()
    }

    /// Look up the value of a binding.
    pub fn lookup_value(&self, index: CrateIndex) -> ValueView {
        self.bindings[index].value()
    }

    pub fn is_foreign(&self, index: CrateIndex) -> bool {
        matches!(self.bindings[index].kind, EntityKind::Foreign { .. })
    }

    /// Try applying foreign binding.
    ///
    /// ## Panics
    ///
    /// Panics if `binder` is either not bound or not foreign.
    // @Task correctly handle
    // * pure vs impure
    // * polymorphism
    // * illegal neutrals
    // * types (arguments of type `Type`): skip them
    // @Note: we need to convert to be able to convert to ffi::Value
    pub fn apply_foreign_binding(
        &self,
        binder: Identifier,
        arguments: Vec<Expression>,
    ) -> Result<Option<Expression>> {
        match self.bindings[binder.crate_index().unwrap()].kind {
            EntityKind::Foreign {
                arity, function, ..
            } => Ok(if arguments.len() == arity {
                let mut value_arguments = Vec::new();

                // @Task tidy up with iterator combinators
                for argument in arguments {
                    if let Some(argument) = ffi::Value::from_expression(&argument, self) {
                        value_arguments.push(argument);
                    } else {
                        return Ok(None);
                    }
                }

                Some(function(value_arguments).into_expression(self)?)
            } else {
                None
            }),
            _ => unreachable!(),
        }
    }

    pub fn carry_out(&mut self, registration: Registration) -> Result<()> {
        use Registration::*;

        Ok(match registration {
            ValueBinding {
                binder,
                type_,
                value,
            } => {
                let index = binder.crate_index().unwrap();
                debug_assert!(
                    self.bindings[index].is_untyped()
                        || self.bindings[index].is_value_without_value()
                );
                self.bindings[index].kind = EntityKind::Value {
                    type_,
                    expression: value,
                };
            }
            DataBinding { binder, type_ } => {
                let index = binder.crate_index().unwrap();
                debug_assert!(self.bindings[index].is_untyped());
                self.bindings[index].kind = EntityKind::DataType {
                    type_,
                    constructors: Vec::new(),
                };
            }
            ConstructorBinding {
                binder,
                type_,
                data,
            } => {
                let index = binder.crate_index().unwrap();
                debug_assert!(self.bindings[index].is_untyped());
                self.bindings[index].kind = EntityKind::Constructor { type_ };

                match self
                    .bindings
                    .get_mut(data.crate_index().unwrap())
                    .unwrap()
                    .kind
                {
                    EntityKind::DataType {
                        ref mut constructors,
                        ..
                    } => constructors.push(binder),
                    _ => unreachable!(),
                }
            }
            ForeignValueBinding { binder, type_ } => {
                let index = binder.crate_index().unwrap();
                debug_assert!(self.bindings[index].is_untyped());

                self.bindings[index].kind = match &self.foreign_bindings.remove(binder.as_str()) {
                    Some((arity, function)) => EntityKind::Foreign {
                        type_,
                        arity: *arity,
                        function: *function,
                    },
                    None => {
                        // @Task better message
                        return Err(Diagnostic::error()
                            .with_code(Code::E060)
                            .with_message(format!("foreign binding `{}` is not registered", binder))
                            .with_span(&binder));
                    }
                };
            }
            ForeignDataBinding { binder } => match self.foreign_types.get_mut(binder.as_str()) {
                Some(index @ None) => {
                    *index = Some(binder.clone());
                }
                Some(Some(_)) => unreachable!(),
                None => {
                    return Err(Diagnostic::error()
                        .with_code(Code::E060)
                        .with_message(format!("foreign data type `{}` is not registered", binder))
                        .with_span(&binder))
                }
            },
        })
    }

    /// Partially register a foreign binding letting it untyped.
    ///
    /// ## Panics
    ///
    /// Panics under `cfg(debug_assertions)` if the `binder` is already bound.
    pub fn register_pure_foreign_binding(
        &mut self,
        binder: &'static str,
        arity: usize,
        function: ffi::ForeignFunction,
    ) {
        let old = self.foreign_bindings.insert(binder, (arity, function));

        debug_assert!(old.is_none());
    }

    // @Task
    pub fn register_impure_foreign_binding<V: Into<ffi::Value>>(&mut self) {
        todo!("register impure foreign binding")
    }

    pub fn register_foreign_type(&mut self, binder: &'static str) {
        let old = self.foreign_types.insert(binder, None);
        debug_assert!(old.is_none());
    }

    // @Note does not scale to modules
    // @Task don't take expression as an argument to get access to span information.
    // rather, return a custom error type, so that the caller can append the label
    // @Temporary signature
    pub fn lookup_foreign_type(
        &self,
        binder: &'static str,
        expression_span: Option<Span>,
    ) -> Result<Expression> {
        match self.foreign_types.get(binder) {
            Some(Some(binder)) => Ok(expr! {
                Binding {
                    Attributes::default(),
                    Span::SHAM;
                    binder: binder.clone(),
                }
            }),
            // @Task better message
            Some(None) => {
                let diagnostic = Diagnostic::error()
                    .with_code(Code::E061)
                    .with_message(format!(
                        "the foreign type `{}` has not been declared",
                        binder
                    ));
                Err(match expression_span {
                    Some(span) => {
                        diagnostic.with_labeled_span(&span, "the type of this expression")
                    }
                    None => diagnostic,
                })
            }
            None => unreachable!(),
        }
    }

    pub fn lookup_foreign_number_type(
        &self,
        number: &Number,
        expression_span: Option<Span>,
    ) -> Result<Expression> {
        self.lookup_foreign_type(
            match number {
                Number::Nat(_) => ffi::Type::NAT,
                Number::Nat32(_) => ffi::Type::NAT32,
                Number::Nat64(_) => ffi::Type::NAT64,
                Number::Int(_) => ffi::Type::INT,
                Number::Int32(_) => ffi::Type::INT32,
                Number::Int64(_) => ffi::Type::INT64,
            },
            expression_span,
        )
    }
}

// @Question too big?
#[derive(Clone)]
pub enum Registration {
    ValueBinding {
        binder: Identifier,
        type_: Expression,
        value: Option<Expression>,
    },
    DataBinding {
        binder: Identifier,
        type_: Expression,
    },
    ConstructorBinding {
        binder: Identifier,
        type_: Expression,
        data: Identifier,
    },
    ForeignValueBinding {
        binder: Identifier,
        type_: Expression,
    },
    ForeignDataBinding {
        binder: Identifier,
    },
}

use std::fmt;

impl crate::support::DisplayWith for Registration {
    type Linchpin = CrateScope;

    fn format(&self, scope: &CrateScope, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Registration::*;

        match self {
            ValueBinding {
                binder,
                type_,
                value,
            } => {
                let mut compound = f.debug_struct("ValueBinding");
                compound
                    .field("binder", binder)
                    .field("type", &DisplayIsDebug(&type_.with(scope)));
                match value {
                    Some(value) => compound.field("value", &DisplayIsDebug(&value.with(scope))),
                    None => compound.field("value", &"<none>"),
                }
                .finish()
            }
            DataBinding { binder, type_ } => f
                .debug_struct("DataBinding")
                .field("binder", binder)
                .field("type", &DisplayIsDebug(&type_.with(scope)))
                .finish(),
            ConstructorBinding {
                binder,
                type_,
                data,
            } => f
                .debug_struct("ConstructorBinding")
                .field("binder", binder)
                .field("type", &DisplayIsDebug(&type_.with(scope)))
                .field("data", data)
                .finish(),
            ForeignValueBinding { binder, type_ } => f
                .debug_struct("ForeignValueBinding")
                .field("binder", binder)
                .field("type", &DisplayIsDebug(&type_.with(scope)))
                .finish(),
            ForeignDataBinding { binder } => f
                .debug_struct("ForeignDataBinding")
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
    CrateScope,
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

    pub fn lookup_type(&self, binder: &Identifier, scope: &CrateScope) -> Option<Expression> {
        use Index::*;

        match binder.index {
            Crate(index) => scope.lookup_type(index),
            Debruijn(index) => Some(self.lookup_type_with_depth(index, 0)),
            DebruijnParameter => unreachable!(),
        }
    }

    fn lookup_type_with_depth(&self, index: DebruijnIndex, depth: usize) -> Expression {
        match self {
            Self::FunctionParameter { parent, type_ } => {
                if depth == index.0 {
                    expr! {
                        Substitution {
                            Attributes::default(),
                            Span::SHAM;
                            substitution: Shift(depth + 1),
                            expression: type_.clone(),
                        }
                    }
                } else {
                    parent.lookup_type_with_depth(index, depth + 1)
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
                            Attributes::default(),
                            Span::SHAM;
                            // @Task verify this shift
                            substitution: Shift(depth + 1),
                            expression: type_.clone(),
                        }
                    },
                    None => parent.lookup_type_with_depth(index, depth + types.len()),
                }
            }
            Self::CrateScope => unreachable!(),
        }
    }

    pub fn lookup_value(&self, binder: &Identifier, scope: &CrateScope) -> ValueView {
        use Index::*;

        match binder.index {
            Crate(index) => scope.lookup_value(index),
            Debruijn(_) => ValueView::Neutral,
            DebruijnParameter => unreachable!(),
        }
    }

    pub fn is_foreign(&self, binder: &Identifier, scope: &CrateScope) -> bool {
        use Index::*;

        match binder.index {
            Crate(index) => scope.is_foreign(index),
            Debruijn(_) => false,
            DebruijnParameter => unreachable!(),
        }
    }
}
