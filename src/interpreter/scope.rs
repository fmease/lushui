//! Binding and scope handler.

use std::{collections::HashMap, fmt};

use super::{ffi, Expression, Substitution::Shift};
use crate::{
    diagnostic::*,
    entity::{Entity, EntityKind},
    hir::expr,
    resolver::{self, Bindings, CrateIndex, DebruijnIndex, Identifier, Index},
    span::Span,
};

#[derive(Default)]
pub struct CrateScope {
    bindings: Bindings,
    pub(crate) program_entry: Option<Identifier>,
    // for printing for now
    // names: HashMap<ModuleIndex, crate::parser::Identifier>,
    // @Note ugly types!
    pub foreign_types: HashMap<&'static str, Option<Identifier>>,
    foreign_bindings: HashMap<&'static str, (usize, ffi::ForeignFunction)>,
    pub inherent_values: ffi::InherentValueMap,
    pub inherent_types: ffi::InherentTypeMap,
    // @Note this is very coarse-grained: as soon as we cannot resolve EITHER type annotation (for example)
    // OR actual value(s), we bail out and add this here. This might be too conversative (leading to more
    // "circular type" errors or whatever), we can just discriminate by creating sth like
    // UnresolvedThingy/WorlistItem { index: CrateIndex, expression: TypeAnnotation|Value|Both|... }
    pub out_of_order_bindings: Vec<Registration>,
}

/// Many methods of module scope panic instead of returning a `Result` because
/// the invariants are expected to be checked beforehand by using the predicate
/// methods also found here. This design is most ergonomic for the caller.
impl CrateScope {
    /// Create a new scope with foreign bindings partially registered.
    pub fn new(scope: resolver::CrateScope) -> Self {
        let mut scope = Self::from(scope);
        ffi::register_foreign_bindings(&mut scope);
        scope
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
        match self.bindings[binder.krate().unwrap()].kind {
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
        Ok(match registration {
            Registration::ValueBinding {
                binder,
                type_,
                value,
            } => {
                let index = binder.krate().unwrap();
                debug_assert!(
                    self.bindings[index].is_untyped_value()
                        || matches!(self.bindings[index], Entity { kind: EntityKind::Value { expression: None, .. }, .. })
                );
                self.bindings[index].kind = EntityKind::Value {
                    type_,
                    expression: value,
                };
            }
            Registration::DataBinding { binder, type_ } => {
                let index = binder.krate().unwrap();
                debug_assert!(self.bindings[index].is_untyped_value());
                self.bindings[index].kind = EntityKind::DataType {
                    type_,
                    constructors: Vec::new(),
                };
            }
            Registration::ConstructorBinding {
                binder,
                type_,
                data,
            } => {
                let index = binder.krate().unwrap();
                debug_assert!(self.bindings[index].is_untyped_value());
                self.bindings[index].kind = EntityKind::Constructor { type_ };

                match self.bindings.get_mut(data.krate().unwrap()).unwrap().kind {
                    EntityKind::DataType {
                        ref mut constructors,
                        ..
                    } => constructors.push(binder),
                    _ => unreachable!(),
                }
            }
            Registration::ForeignValueBinding { binder, type_ } => {
                let index = binder.krate().unwrap();
                debug_assert!(self.bindings[index].is_untyped_value());

                self.bindings[index].kind = match &self.foreign_bindings.remove(binder.as_str()) {
                    Some((arity, function)) => EntityKind::Foreign {
                        type_,
                        arity: *arity,
                        function: *function,
                    },
                    None => {
                        // @Task better message
                        return Err(Diagnostic::new(
                            Level::Fatal,
                            Code::E060,
                            format!("foreign binding `{}` is not registered", binder),
                        )
                        .with_span(&binder));
                    }
                };
            }
            Registration::ForeignDataBinding { binder } => {
                match self.foreign_types.get_mut(binder.as_str()) {
                    Some(index @ None) => {
                        *index = Some(binder.clone());
                    }
                    Some(Some(_)) => unreachable!(),
                    None => {
                        return Err(Diagnostic::new(
                            Level::Fatal,
                            Code::E060,
                            format!("foreign data type `{}` is not registered", binder),
                        )
                        .with_span(&binder))
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
        function: ffi::ForeignFunction,
    ) {
        let old = self.foreign_bindings.insert(binder, (arity, function));

        debug_assert!(old.is_none());
    }

    // @Task
    pub fn register_impure_foreign_binding<V: Into<ffi::Value>>(&mut self) {
        std::todo!("register impure foreign binding")
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
                Binding[] {
                    binder: binder.clone(),
                }
            }),
            // @Task better message
            Some(None) => {
                let diagnostic = Diagnostic::new(
                    Level::Fatal,
                    Code::E061,
                    format!("the foreign type `{}` has not been declared", binder),
                );
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
}

impl fmt::Debug for CrateScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use crate::support::DisplayIsDebug;

        f.debug_struct("interpreter::CrateScope")
            .field(
                "bindings",
                &DisplayIsDebug(&resolver::display_bindings(&self.bindings)),
            )
            .field("out_of_order_bindings", &self.out_of_order_bindings)
            .finish()
    }
}

// @Temporary, for interop. ideally, those 2 will be merged
impl From<resolver::CrateScope> for CrateScope {
    fn from(scope: resolver::CrateScope) -> Self {
        Self {
            bindings: scope.bindings,
            program_entry: scope.program_entry,
            ..Default::default()
        }
    }
}

// @Question too big?
#[derive(Debug, Clone)]
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

// @Task find out if we can get rid of this type by letting `ModuleScope::lookup_value` resolve to the Binder
// if it's neutral
pub enum ValueView {
    Reducible(Expression),
    Neutral,
}

// @Temporary
impl fmt::Debug for ValueView {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Reducible(expression) => write!(f, "(REDUCIBLE {})", expression),
            Self::Neutral => write!(f, "NEUTRAL"),
        }
    }
}

/// The scope of bindings inside of a function.
///
/// Can only store function parameters at the moment.
///
/// Comparison to [ModuleScope]: In function scopes, declarations shadow other ones with the same name.
/// And since lambdas and let/ins are nested, they are ordered and
/// most importantly, recursion only works explicitly via the fix-point-combinator.
pub enum FunctionScope<'a> {
    CrateScope(&'a CrateScope),
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

    pub fn crate_scope(&self) -> &CrateScope {
        use FunctionScope::*;

        match self {
            CrateScope(scope) => scope,
            FunctionParameter { parent, .. } | PatternBinders { parent, .. } => {
                parent.crate_scope()
            }
        }
    }

    pub fn lookup_type(&self, binder: &Identifier) -> Option<Expression> {
        use Index::*;

        match binder.index {
            Crate(index) => self.crate_scope().lookup_type(index),
            Debruijn(index) => Some(self.lookup_type_with_depth(index, 0)),
            DebruijnParameter => unreachable!(),
        }
    }

    fn lookup_type_with_depth(&self, index: DebruijnIndex, depth: usize) -> Expression {
        match self {
            Self::FunctionParameter { parent, type_ } => {
                if depth == index.0 {
                    expr! {
                        Substitution[] {
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
                        Substitution[] {
                            // @Task verify this shift
                            substitution: Shift(depth + 1),
                            expression: type_.clone(),
                        }
                    },
                    None => parent.lookup_type_with_depth(index, depth + types.len()),
                }
            }
            Self::CrateScope(_) => unreachable!(),
        }
    }

    pub fn lookup_value(&self, binder: &Identifier) -> ValueView {
        use Index::*;

        match binder.index {
            Crate(index) => self.crate_scope().lookup_value(index),
            Debruijn(_) => ValueView::Neutral,
            DebruijnParameter => unreachable!(),
        }
    }

    pub fn is_foreign(&self, binder: &Identifier) -> bool {
        use Index::*;

        match binder.index {
            Crate(index) => self.crate_scope().is_foreign(index),
            Debruijn(_) => false,
            DebruijnParameter => unreachable!(),
        }
    }
}

impl<'a> From<&'a CrateScope> for FunctionScope<'a> {
    fn from(scope: &'a CrateScope) -> Self {
        Self::CrateScope(scope)
    }
}
