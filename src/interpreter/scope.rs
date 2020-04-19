//! Binding and scope handler.
//!
//! Exposes two types of scopes:
//!
//! * [ModuleScope]
//! * [FunctionScope]

use std::{collections::HashMap, fmt};

use super::{ffi, Expression, Substitution::Shift};
use crate::{
    diagnostic::*,
    hir::expr,
    resolver::{DebruijnIndex, Identifier, Index, ModuleIndex},
};

/// The scope of bindings inside of a module.
///
/// Can store all kinds of bindings:
///
/// * typed values
/// * data types with its constructors
/// * constructors
/// * partially and fully registered (untyped and typed respectively)
///   foreign bindings
///
/// Difference to [FunctionScope]: The module scopes is designed for declarations which may appear out of order and
/// cross-reference each other (as long as there is no cyclic dependency) and for recursive bindings. It's flat.
/// The API offers mutating functions.
#[derive(Default)]
// @Question naming? `native_bindings`, `foreign_bindings`
pub struct ModuleScope {
    bindings: HashMap<ModuleIndex, Entity>,
    // for printing for now
    // names: HashMap<ModuleIndex, crate::parser::Identifier>,
    // @Note very ad-hoc solution, does not scale to modules
    // @Question merge the two?
    // `ForeignEntity`
    // @Beacon @Beacon @Update move the things below into CrateScope!!
    pub foreign_types: HashMap<&'static str, Option<Identifier>>,
    // @Temporary types (above and below, … they are not descriptive)
    foreign_bindings: HashMap<&'static str, (usize, ffi::ForeignFunction)>,
    pub inherent_values: ffi::InherentValueMap,
    pub inherent_types: ffi::InherentTypeMap,
}

/// Many methods of module scope panic instead of returning a `Result` because
/// the invariants are expected to be checked beforehand by using the predicate
/// methods also found here. This design is most flexible for the caller I think.
impl ModuleScope {
    /// Create a new scope with foreign bindings partially registered.
    pub fn new() -> Self {
        let mut scope = Self::default();
        ffi::register_foreign_bindings(&mut scope);
        scope
    }

    fn lookup_type(&self, index: ModuleIndex) -> Expression {
        self.bindings[&index].r#type()
    }

    /// Look up the value of a binding.
    fn lookup_value(&self, index: ModuleIndex) -> Value {
        self.bindings[&index].value()
    }

    fn is(&self, binder: &Identifier, predicate: fn(&Entity) -> bool) -> bool {
        self.bindings
            .get(&binder.module().unwrap())
            .map(predicate)
            // @Question shouldn't it just be unwrap() b.c. resolver already ran?
            .unwrap_or(false)
    }

    pub fn is_constructor(&self, binder: &Identifier) -> bool {
        self.is(
            binder,
            |entity| matches!(entity, Entity::Constructor { .. }),
        )
    }

    pub fn is_foreign(&self, index: ModuleIndex) -> bool {
        matches!(self.bindings[&index], Entity::Foreign { .. })
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
        match self.bindings[&binder.module().unwrap()] {
            Entity::Foreign {
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
    pub fn insert_value_binding(
        &mut self,
        binder: Identifier,
        r#type: Expression,
        value: Expression,
    ) {
        let old = self.bindings.insert(
            binder.module().unwrap(),
            Entity::Expression {
                r#type,
                expression: value,
            },
        );

        debug_assert!(old.is_none());
    }

    /// Insert a data type binding into the scope.
    ///
    /// ## Panics
    ///
    /// Panics under `cfg(debug_assertions)` if the `binder` is already bound.
    pub fn insert_data_binding(&mut self, binder: Identifier, r#type: Expression) {
        let old = self.bindings.insert(
            binder.module().unwrap(),
            Entity::DataType {
                r#type,
                constructors: Vec::new(),
            },
        );

        debug_assert!(old.is_none());
    }

    /// Insert constructor binding into the scope.
    ///
    /// # Panics
    ///
    /// Panics under `cfg(debug_assertions)` if `binder` is already bound and does so
    /// in every case if given data type does not exist or is not a data type.
    pub fn insert_constructor_binding(
        &mut self,
        binder: Identifier,
        r#type: Expression,
        data_type: &Identifier,
    ) {
        let old = self
            .bindings
            .insert(binder.module().unwrap(), Entity::Constructor { r#type });

        debug_assert!(old.is_none());

        match self.bindings.get_mut(&data_type.module().unwrap()).unwrap() {
            Entity::DataType {
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
        let index = binder.module().unwrap();
        self.bindings.insert(
            index,
            match &self.foreign_bindings.remove(&*binder.source.atom) {
                Some((arity, function)) => Entity::Foreign {
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
            },
        );
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
        match self.foreign_types.get_mut(&*binder.source.atom) {
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

impl fmt::Debug for ModuleScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (binder, entity) in &self.bindings {
            // writeln!(f, "{} |-> {}", self.names[binder], entity)?;
            writeln!(f, "{} |-> {}", binder.value, entity)?;
        }
        Ok(())
    }
}

// @Task find out if we can get rid of this type by letting `ModuleScope::lookup_value` resolve to the Binder
// if it's neutral
pub enum Value {
    Reducible(Expression),
    Neutral,
}

// @Temporary
impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Reducible(expression) => write!(f, "(REDUCIBLE {})", expression),
            Self::Neutral => f.write_str("NEUTRAL"),
        }
    }
}

/// An entity found inside a module scope.
#[derive(Clone)]
enum Entity {
    Expression {
        r#type: Expression,
        expression: Expression,
    },
    // @Question should we store the constructors?
    DataType {
        r#type: Expression,
        constructors: Vec<Identifier>,
    },
    Constructor {
        r#type: Expression,
    },
    Foreign {
        r#type: Expression,
        arity: usize,
        function: ffi::ForeignFunction,
    },
}

impl Entity {
    /// Retrieve the type of an entity.
    ///
    /// ## Panics
    ///
    /// Panics if called on an [Entity::UntypedForeign].
    fn r#type(&self) -> Expression {
        match self {
            Self::Expression { r#type, .. } => r#type,
            Self::DataType { r#type, .. } => r#type,
            Self::Constructor { r#type, .. } => r#type,
            Self::Foreign { r#type, .. } => r#type,
        }
        .clone()
    }

    /// Retrieve the value of an entity
    fn value(&self) -> Value {
        match self {
            Self::Expression { expression, .. } => Value::Reducible(expression.clone()),
            _ => Value::Neutral,
        }
    }
}

impl fmt::Display for Entity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expression { r#type, expression } => write!(f, "{}: {}", expression, r#type),
            Self::DataType {
                r#type,
                constructors,
            } => write!(
                f,
                "_data_: {} = {}",
                r#type,
                constructors
                    .iter()
                    .map(|constructor| format!("{} ", constructor))
                    .collect::<String>()
            ),
            Self::Constructor { r#type } => write!(f, "_constructor_: {}", r#type),
            Self::Foreign { r#type, .. } => write!(f, "_foreign_: {}", r#type),
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
    Module(&'a ModuleScope),
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

    pub fn module(&self) -> &ModuleScope {
        match self {
            Self::Module(module) => module,
            Self::Function { parent, .. } => parent.module(),
        }
    }

    pub fn lookup_type(&self, binder: &Identifier) -> Expression {
        fn lookup_type(
            scope: &FunctionScope<'_>,
            index: DebruijnIndex,
            depth: usize,
        ) -> Expression {
            match scope {
                FunctionScope::Function { parent, r#type } => {
                    if depth == index.value {
                        expr! {
                            Substitution[] {
                                substitution: Shift(depth + 1),
                                expression: r#type.clone(),
                            }
                        }
                    } else {
                        lookup_type(parent, index, depth + 1)
                    }
                }
                FunctionScope::Module(_) => unreachable!(),
            }
        }

        match binder.index {
            Index::Module(index) => self.module().lookup_type(index),
            Index::Debruijn(index) => lookup_type(self, index, 0),
            Index::None => unreachable!(),
        }
    }

    pub fn lookup_value(&self, binder: &Identifier) -> Value {
        match binder.index {
            Index::Module(index) => self.module().lookup_value(index),
            Index::Debruijn(_) => Value::Neutral,
            Index::None => unreachable!(),
        }
    }

    pub fn is_foreign(&self, binder: &Identifier) -> bool {
        match binder.index {
            Index::Module(index) => self.module().is_foreign(index),
            Index::Debruijn(_) => false,
            Index::None => unreachable!(),
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
