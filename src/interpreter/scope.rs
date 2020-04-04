//! Binding and scope handler.
//!
//! Exposes two types of scopes:
//!
//! * [ModuleScope]
//! * [FunctionScope]

mod ffi;

use std::{collections::HashMap, fmt};

use crate::{
    hir::{expr, Expression},
    resolver::{Identifier, ModuleIndex},
};

// use super::{Error, Result};

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
#[derive(Default, Clone)]
pub struct ModuleScope {
    bindings: HashMap<ModuleIndex, Entity>,
    // for printing for now
    // names: HashMap<ModuleIndex, crate::parser::Identifier>,
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

    fn lookup_type(&self, index: ModuleIndex) -> Expression<Identifier> {
        // @Temporary @Note indexing should never panic after name resolution
        // @Task verify and remove this notice
        self.bindings[&index].r#type()
    }

    /// Look up the value of a binding.
    fn lookup_value(&self, index: ModuleIndex) -> Value {
        // @Temporary @Note indexing should never panic after name resolution
        // @Task verify and remove this notice
        self.bindings[&index].value()
    }

    fn is(&self, binder: &Identifier, predicate: fn(&Entity) -> bool) -> bool {
        self.bindings
            .get(&binder.module().unwrap())
            .map(predicate)
            .unwrap_or(false)
    }

    pub fn is_constructor(&self, binder: &Identifier) -> bool {
        self.is(
            binder,
            |entity| matches!(entity, Entity::Constructor { .. }),
        )
    }

    pub fn is_foreign(&self, binder: &Identifier) -> bool {
        self.is(binder, |entity| matches!(entity, Entity::Foreign { .. }))
    }

    // /// Try applying foreign binding.
    // ///
    // /// ## Panics
    // ///
    // /// Panics if `binder` is either not bound or not foreign.
    // pub fn try_apply_foreign_binding(
    //     self,
    //     binder: &Identifier,
    //     arguments: VecDeque<Expression<Identifier>>,
    // ) -> Result<Expression<Identifier>> {
    //     match self.clone().bindings.borrow()[binder] {
    //         Entity::Foreign {
    //             arity, function, ..
    //         } => {
    //             if arguments.len() == arity {
    //                 // We normalize the result of the foreign binding to prevent the injection of some kinds of garbage values.
    //                 // @Question should we run `infer_type` over it as well? I think so
    //                 // @Task match_with_annotated_type
    //                 let function_scope = FunctionScope::new(self);
    //                 function(arguments).evaluate(&function_scope)
    //             } else {
    //                 Ok(expr! {
    //                     UnsaturatedForeignApplication {
    //                         callee: binder.clone(),
    //                         arguments,
    //                     }
    //                 })
    //             }
    //         }
    //         _ => unreachable!(),
    //     }
    // }

    /// Insert a value binding into the scope.
    ///
    /// ## Panics
    ///
    /// Panics under `cfg(debug_assertions)` if `binder` is already bound.
    pub fn insert_value_binding(
        &mut self,
        binder: Identifier,
        r#type: Expression<Identifier>,
        value: Expression<Identifier>,
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
    pub fn insert_data_binding(&mut self, binder: Identifier, r#type: Expression<Identifier>) {
        // eprintln!("ModuleScope::insert_data_binding({})", binder);

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
        r#type: Expression<Identifier>,
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
    pub fn insert_type_for_foreign_binding(
        &mut self,
        binder: Identifier,
        r#type: Expression<Identifier>,
    ) {
        let index = binder.module().unwrap();
        self.bindings.insert(
            index,
            match &self.bindings[&index] {
                Entity::_UntypedForeign { arity, function } => Entity::Foreign {
                    r#type,
                    arity: *arity,
                    function: *function,
                },
                _ => unreachable!(),
            },
        );
    }

    /// Partially register a foreign binding letting it untyped.
    ///
    /// ## Panics
    ///
    /// Panics under `cfg(debug_assertions)` if the `binder` is already bound.
    pub fn insert_untyped_foreign_binding(
        &mut self,
        _binder: &str,
        _arity: usize,
        _function: ffi::ForeignFunction,
    ) {
        todo!() // @Task @Beacon

        // let old = self.bindings.insert(
        //     Identifier::from(binder),
        //     Entity::UntypedForeign { arity, function },
        // );

        // debug_assert!(old.is_none());
    }
}

impl fmt::Debug for ModuleScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (binder, entity) in &self.bindings {
            if let Entity::_UntypedForeign { .. } = &entity {
                continue;
            }

            // writeln!(f, "{} |-> {}", self.names[binder], entity)?;
            writeln!(f, "{} |-> {}", binder.value, entity)?;
        }
        Ok(())
    }
}

// @Task find out if we can get rid of this type by letting `ModuleScope::lookup_value` resolve to the Binder
// if it's neutral
pub enum Value {
    Reducible(Expression<Identifier>),
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
        r#type: Expression<Identifier>,
        expression: Expression<Identifier>,
    },
    // @Question should we store the constructors?
    DataType {
        r#type: Expression<Identifier>,
        constructors: Vec<Identifier>,
    },
    Constructor {
        r#type: Expression<Identifier>,
    },
    _UntypedForeign {
        arity: usize,
        function: ffi::ForeignFunction,
    },
    Foreign {
        r#type: Expression<Identifier>,
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
    fn r#type(&self) -> Expression<Identifier> {
        match self {
            Self::Expression { r#type, .. } => r#type.clone(),
            Self::DataType { r#type, .. } => r#type.clone(),
            Self::Constructor { r#type, .. } => r#type.clone(),
            Self::_UntypedForeign { .. } => unreachable!(),
            Self::Foreign { r#type, .. } => r#type.clone(),
        }
    }

    /// Retrieve the value of an entity
    fn value(&self) -> Value {
        match self {
            Entity::Expression { expression, .. } => Value::Reducible(expression.clone()),
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
            Self::_UntypedForeign { .. } => f.write_str("_untyped foreign_"),
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
// @Note probably bad cache behavior as this is just a linked list. but I honestly cannot think
// of a better data structure right now that does not involve deep-cloning an association list or the like
// @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Bug
// rewrite this to correctly handle Identifier::None i.e. count depth just like in crate::resolver::Scope::resolve_binder
pub enum FunctionScope<'a> {
    Module(&'a ModuleScope),
    // @Note obviously, we don't store an Identifier but a DebruijnIndex here hmm
    Function {
        parent: &'a FunctionScope<'a>,
        // index: DebruijnIndex,
        r#type: Expression<Identifier>,
    },
}

use crate::interpreter::Substitution::Shift;
use crate::resolver::{DebruijnIndex, Index};

impl<'a> FunctionScope<'a> {
    pub fn extend_with_parameter(&'a self, r#type: Expression<Identifier>) -> Self {
        Self::Function {
            parent: self,
            r#type,
        }
    }

    fn module(&self) -> &ModuleScope {
        match self {
            Self::Module(module) => module,
            Self::Function { parent, .. } => parent.module(),
        }
    }

    pub fn lookup_type(&self, binder: &Identifier) -> Expression<Identifier> {
        // eprintln!("FunctionScope::lookup_type(binder={}):\n{:?}", binder, self);

        fn lookup_type(
            scope: &FunctionScope<'_>,
            index: DebruijnIndex,
            depth: usize,
        ) -> Expression<Identifier> {
            match scope {
                FunctionScope::Function { parent, r#type } => {
                    if depth == index.value {
                        expr! {
                            Substitution[crate::span::Span::dummy()] {
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
        // eprintln!("FunctionScope::lookup_value() binder={}", binder);

        match binder.index {
            Index::Module(index) => self.module().lookup_value(index),
            Index::Debruijn(_) => Value::Neutral,
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
