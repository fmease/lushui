//! Binding and scope handler.

use std::{collections::HashMap, fmt};

use super::{ffi, Expression, Substitution::Shift};
use crate::{
    diagnostic::*,
    entity::{Entity, EntityKind},
    hir::expr,
    resolver::{self, Bindings, CrateIndex, DebruijnIndex, Identifier, Index},
};

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

    fn lookup_type(&self, index: CrateIndex) -> Expression {
        self.bindings[index].r#type()
    }

    /// Look up the value of a binding.
    fn lookup_value(&self, index: CrateIndex) -> ValueView {
        self.bindings[index].value()
    }

    fn is(&self, binder: &Identifier, predicate: fn(&Entity) -> bool) -> bool {
        self.bindings
            .get(binder.krate().unwrap())
            .map(predicate)
            // @Question shouldn't it just be unwrap() b.c. resolver already ran?
            .unwrap_or(false)
    }

    pub fn is_constructor(&self, binder: &Identifier) -> bool {
        self.is(
            binder,
            |entity| matches!(entity.kind, EntityKind::Constructor { .. }),
        )
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

    /// Insert a value binding into the scope.
    ///
    /// ## Panics
    ///
    /// Panics under `cfg(debug_assertions)` if `binder` is already bound.
    // @Task better name
    pub fn insert_value_binding(
        &mut self,
        binder: Identifier,
        r#type: Expression,
        value: Expression,
    ) {
        let index = binder.krate().unwrap();
        debug_assert!(self.bindings[index].is_untyped_value());
        self.bindings[index].kind = EntityKind::Value {
            r#type,
            expression: value,
        };
    }

    /// Insert a data type binding into the scope.
    ///
    /// ## Panics
    ///
    /// Panics under `cfg(debug_assertions)` if the `binder` is already bound.
    // @Task change name
    pub fn insert_data_binding(&mut self, binder: Identifier, r#type: Expression) {
        let index = binder.krate().unwrap();
        debug_assert!(self.bindings[index].is_untyped_value());
        self.bindings[index].kind = EntityKind::DataType {
            r#type,
            constructors: Vec::new(),
        };
    }

    /// Insert constructor binding into the scope.
    ///
    /// # Panics
    ///
    /// Panics under `cfg(debug_assertions)` if `binder` is already bound and does so
    /// in every case if given data type does not exist or is not a data type.
    // @Task change name
    pub fn insert_constructor_binding(
        &mut self,
        binder: Identifier,
        r#type: Expression,
        data_type: &Identifier,
    ) {
        let index = binder.krate().unwrap();
        debug_assert!(self.bindings[index].is_untyped_value());
        self.bindings[index].kind = EntityKind::Constructor { r#type };

        match self
            .bindings
            .get_mut(data_type.krate().unwrap())
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

    /// Complete the registration of a foreign binding by typing it.
    ///
    /// ## Panics
    ///
    /// Panics if the `binder` is not bound or the binding is not a partially
    /// registered one.
    pub fn complete_foreign_binding(
        &mut self,
        binder: Identifier,
        r#type: Expression,
    ) -> Result<()> {
        let index = binder.krate().unwrap();
        debug_assert!(self.bindings[index].is_untyped_value());

        self.bindings[index].kind = match &self.foreign_bindings.remove(binder.as_str()) {
            Some((arity, function)) => EntityKind::Foreign {
                r#type,
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
                .with_span(binder.source.span));
            }
        };
        Ok(())
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

    pub fn insert_foreign_data(&mut self, binder: &Identifier) -> Result<()> {
        match self.foreign_types.get_mut(binder.as_str()) {
            Some(index @ None) => {
                *index = Some(binder.clone());
                Ok(())
            }
            Some(Some(_)) => unreachable!(),
            None => Err(Diagnostic::new(
                Level::Fatal,
                Code::E060,
                format!("foreign data type `{}` is not registered", binder),
            )
            .with_span(binder.source.span)),
        }
    }

    // @Note does not scale to modules
    // @Task don't take expression as an argument to get access to span information.
    // rather, return a custom error type, so that the caller can append the label
    // @Temporary signature
    pub fn lookup_foreign_type(
        &self,
        binder: &'static str,
        expression: Option<Expression>,
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
                Err(match expression {
                    Some(expression) => {
                        diagnostic.with_labeled_span(expression.span, "the type of this expression")
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
        write!(f, "{:#?}", self.bindings)
    }
}

// @Temporary, for interop. ideally, those 2 will be merged
impl From<resolver::CrateScope> for CrateScope {
    fn from(scope: resolver::CrateScope) -> Self {
        Self {
            bindings: scope.bindings,
            program_entry: scope.program_entry,
            foreign_bindings: Default::default(),
            foreign_types: Default::default(),
            inherent_values: Default::default(),
            inherent_types: Default::default(),
        }
    }
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
            Self::Neutral => f.write_str("NEUTRAL"),
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
    Module(&'a CrateScope),
    // @Note obviously, we don't store an Identifier but a DebruijnIndex here hmm
    Function {
        parent: &'a FunctionScope<'a>,
        // index: DebruijnIndex,
        r#type: Expression,
    },
}

impl<'a> FunctionScope<'a> {
    pub fn extend_with_parameter(&'a self, r#type: Expression) -> Self {
        Self::Function {
            parent: self,
            r#type,
        }
    }

    pub fn module(&self) -> &CrateScope {
        match self {
            Self::Module(module) => module,
            Self::Function { parent, .. } => parent.module(),
        }
    }

    pub fn lookup_type(&self, binder: &Identifier) -> Expression {
        match binder.index {
            Index::Crate(index) => self.module().lookup_type(index),
            Index::Debruijn(index) => self.lookup_type_with_depth(index, 0),
            Index::DebruijnParameter => unreachable!(),
        }
    }

    fn lookup_type_with_depth(&self, index: DebruijnIndex, depth: usize) -> Expression {
        match self {
            FunctionScope::Function { parent, r#type } => {
                if depth == index.0 {
                    expr! {
                        Substitution[] {
                            substitution: Shift(depth + 1),
                            expression: r#type.clone(),
                        }
                    }
                } else {
                    parent.lookup_type_with_depth(index, depth + 1)
                }
            }
            FunctionScope::Module(_) => unreachable!(),
        }
    }

    pub fn lookup_value(&self, binder: &Identifier) -> ValueView {
        match binder.index {
            Index::Crate(index) => self.module().lookup_value(index),
            Index::Debruijn(_) => ValueView::Neutral,
            Index::DebruijnParameter => unreachable!(),
        }
    }

    pub fn is_foreign(&self, binder: &Identifier) -> bool {
        match binder.index {
            Index::Crate(index) => self.module().is_foreign(index),
            Index::Debruijn(_) => false,
            Index::DebruijnParameter => unreachable!(),
        }
    }
}

impl fmt::Debug for FunctionScope<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Module(_) => f.write_str("module"),
            Self::Function { parent, r#type } => write!(f, "(': {}) --> {:?}", r#type, parent),
        }
    }
}
