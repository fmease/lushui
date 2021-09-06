use super::{ffi, Expression, Substitution::Shift};
use crate::{
    diagnostics::{Code, Diagnostic, Reporter},
    entity::EntityKind,
    error::Result,
    format::{AsDebug, DisplayWith},
    hir::expr,
    lowered_ast::{Attributes, Number},
    package::BuildSession,
    resolver::{CrateScope, DeBruijnIndex, Identifier},
    span::Span,
};
use std::fmt;

/// Many methods of module scope panic instead of returning a `Result` because
/// the invariants are expected to be checked beforehand by using the predicate
/// methods also found here. This design is most ergonomic for the caller.
impl CrateScope {
    pub fn register_foreign_bindings(&mut self) {
        ffi::register_foreign_bindings(self);
    }

    // @Bug does not understand non-local binders
    pub fn carry_out(&mut self, registration: Registration, reporter: &Reporter) -> Result {
        use Registration::*;

        Ok(match registration {
            ValueBinding {
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
            DataBinding { binder, type_ } => {
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
            ConstructorBinding {
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
            ForeignValueBinding { binder, type_ } => {
                let index = binder.declaration_index().unwrap();
                // @Bug unwrap None reachable
                let index = self.local_index(index).unwrap();
                debug_assert!(self[index].is_untyped_value());

                self[index].kind = match &self.ffi.foreign_bindings.remove(binder.as_str()) {
                    Some(ffi::ForeignFunction { arity, function }) => EntityKind::Foreign {
                        type_,
                        arity: *arity,
                        function: *function,
                    },
                    None => {
                        // @Task better message
                        Diagnostic::error()
                            .code(Code::E060)
                            .message(format!("foreign binding `{}` is not registered", binder))
                            .primary_span(&binder)
                            .report(reporter);
                        return Err(());
                    }
                };
            }
            // @Beacon @Beacon @Task throw an error ("redefinition")
            // if an earlier crates has already defined this type
            // (and also if it's defined several times in the same crate!!)
            ForeignDataBinding { binder } => {
                match self.ffi.foreign_types.get_mut(binder.as_str()) {
                    Some(index @ None) => {
                        *index = Some(binder.clone());
                    }
                    Some(Some(_)) => unreachable!(),
                    None => {
                        Diagnostic::error()
                            .code(Code::E060)
                            .message(format!("foreign data type `{}` is not registered", binder))
                            .primary_span(&binder)
                            .report(reporter);
                        return Err(());
                    }
                }
            }
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
        function: ffi::NakedForeignFunction,
    ) {
        let old = self
            .ffi
            .foreign_bindings
            .insert(binder, ffi::ForeignFunction { arity, function });

        debug_assert!(old.is_none());
    }

    // @Task
    pub fn register_impure_foreign_binding<V: Into<ffi::Value>>(&mut self) {
        todo!("register impure foreign binding")
    }

    pub fn register_foreign_type(&mut self, binder: &'static str) {
        let old = self.ffi.foreign_types.insert(binder, None);
        debug_assert!(old.is_none());
    }

    // @Note does not scale to modules
    // @Task don't take expression as an argument to get access to span information.
    // rather, return a custom error type, so that the caller can append the label
    // @Temporary signature
    pub fn look_up_foreign_type(
        &self,
        binder: &'static str,
        expression_span: Option<Span>,
        session: &BuildSession,
        reporter: &Reporter,
    ) -> Result<Expression> {
        if let Some(binder) = session.foreign_type(binder) {
            return Ok(binder.clone().to_expression());
        }

        match self.ffi.foreign_types.get(binder) {
            Some(Some(binder)) => Ok(binder.clone().to_expression()),
            Some(None) => {
                Diagnostic::error()
                    .code(Code::E061)
                    // @Beacon @Task write a waaay better message!!
                    .message(format!("foreign type `{}` is not defined", binder))
                    .when_present(expression_span, |diagnostic, span| {
                        diagnostic.labeled_primary_span(span, "the type of this expression")
                    })
                    .report(reporter);
                Err(())
            }
            None => unreachable!(),
        }
    }

    pub fn look_up_foreign_number_type(
        &self,
        number: &Number,
        expression_span: Option<Span>,
        session: &BuildSession,
        reporter: &Reporter,
    ) -> Result<Expression> {
        self.look_up_foreign_type(
            match number {
                Number::Nat(_) => ffi::Type::NAT,
                Number::Nat32(_) => ffi::Type::NAT32,
                Number::Nat64(_) => ffi::Type::NAT64,
                Number::Int(_) => ffi::Type::INT,
                Number::Int32(_) => ffi::Type::INT32,
                Number::Int64(_) => ffi::Type::INT64,
            },
            expression_span,
            session,
            reporter,
        )
    }

    pub fn look_up_unit_type(
        &self,
        expression_span: Option<Span>,
        reporter: &Reporter,
    ) -> Result<Expression> {
        Ok(self
            .ffi
            .inherent_types
            .unit
            .clone()
            .ok_or_else(|| undefined_inherent_type("Unit", expression_span).report(reporter))?
            .to_expression())
    }

    pub fn look_up_bool_type(
        &self,
        expression_span: Option<Span>,
        reporter: &Reporter,
    ) -> Result<Expression> {
        Ok(self
            .ffi
            .inherent_types
            .bool
            .clone()
            .ok_or_else(|| undefined_inherent_type("Bool", expression_span).report(reporter))?
            .to_expression())
    }

    pub fn look_up_option_type(
        &self,
        expression_span: Option<Span>,
        reporter: &Reporter,
    ) -> Result<Expression> {
        Ok(self
            .ffi
            .inherent_types
            .option
            .clone()
            .ok_or_else(|| undefined_inherent_type("Option", expression_span).report(reporter))?
            .to_expression())
    }
}

fn undefined_inherent_type(name: &'static str, expression_span: Option<Span>) -> Diagnostic {
    Diagnostic::error()
        .code(Code::E063)
        .message(format!("inherent type `{}` is not defined", name))
        .when_present(expression_span, |diagnostic, span| {
            diagnostic.labeled_primary_span(span, "the type of this expression")
        })
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

impl DisplayWith for Registration {
    type Context<'a> = (&'a CrateScope, &'a BuildSession);

    fn format(&self, context: Self::Context<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
                    .field("type", &type_.with(context).as_debug());
                match value {
                    Some(value) => compound.field("value", &value.with(context).as_debug()),
                    None => compound.field("value", &"?(none)"),
                }
                .finish()
            }
            DataBinding { binder, type_ } => f
                .debug_struct("DataBinding")
                .field("binder", binder)
                .field("type", &type_.with(context).as_debug())
                .finish(),
            ConstructorBinding {
                binder,
                type_,
                data,
            } => f
                .debug_struct("ConstructorBinding")
                .field("binder", binder)
                .field("type", &type_.with(context).as_debug())
                .field("data", data)
                .finish(),
            ForeignValueBinding { binder, type_ } => f
                .debug_struct("ForeignValueBinding")
                .field("binder", binder)
                .field("type", &type_.with(context).as_debug())
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

    pub(super) fn look_up_type(&self, index: DeBruijnIndex) -> Expression {
        self.look_up_type_with_depth(index, 0)
    }

    fn look_up_type_with_depth(&self, index: DeBruijnIndex, depth: usize) -> Expression {
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
                            Attributes::default(),
                            Span::SHAM;
                            // @Task verify this shift
                            substitution: Shift(depth + 1),
                            expression: type_.clone(),
                        }
                    },
                    None => parent.look_up_type_with_depth(index, depth + types.len()),
                }
            }
            Self::CrateScope => unreachable!(),
        }
    }
}
