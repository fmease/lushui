//! Binding and scope handler.
//!
//! Exposes two types of scopes:
//!
//! * [ModuleScope]
//! * [FunctionScope]

mod ffi;

use super::{Error, Result};

pub use module::ModuleScope;

mod module {
    use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

    use super::{ffi, Error, FunctionScope, Result};
    use crate::hir::{expr, Expression, Identifier};
    use crate::interpreter::evaluate;

    /// An entity found inside a module scope.
    // @Question Expression<Normalized>?
    #[derive(Clone)]
    enum Entity {
        // @Question is this a value or an expression?
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
        UntypedForeign {
            arity: usize,
            function: ffi::ForeignFunction,
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
                Self::Expression { r#type, .. } => r#type.clone(),
                Self::DataType { r#type, .. } => r#type.clone(),
                Self::Constructor { r#type, .. } => r#type.clone(),
                Self::UntypedForeign { .. } => unreachable!(),
                Self::Foreign { r#type, .. } => r#type.clone(),
            }
        }

        /// Retrieve the value of an entity
        ///
        /// Returns `None` if the entity is **neutral** (non-reducible)
        fn value(&self) -> Option<Expression> {
            match self {
                Entity::Expression { expression, .. } => Some(expression.clone()),
                _ => None,
            }
        }
    }

    impl fmt::Debug for Entity {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Expression { r#type, expression } => write!(f, "{}: {}", expression, r#type),
                Self::DataType {
                    r#type,
                    constructors,
                } => write!(
                    f,
                    "data: {} = {}",
                    r#type,
                    constructors
                        .iter()
                        .map(|constructor| format!("{} ", constructor))
                        .collect::<String>()
                ),
                Self::Constructor { r#type } => write!(f, "constructor: {}", r#type),
                Self::UntypedForeign { .. } => f.write_str("untyped foreign"),
                Self::Foreign { r#type, .. } => write!(f, "foreign: {}", r#type),
            }
        }
    }

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
        bindings: Rc<RefCell<HashMap<Identifier, Entity>>>,
    }

    /// Many methods of module scope panic instead of returning a `Result` because
    /// the invariants are expected to be checked beforehand by using the predicate
    /// methods also found here. This design is most flexible for the caller I think.
    impl ModuleScope {
        /// Create a new scope with foreign bindings partially registered.
        pub fn new() -> Self {
            let scope = Self::default();
            ffi::register_foreign_bindings(scope.clone());
            scope
        }

        pub fn lookup_type(self, binder: &Identifier) -> Option<Expression> {
            self.bindings.borrow().get(binder).map(Entity::r#type)
        }

        /// Look up the value of a binding.
        ///
        /// * returns `None` if the `binder` is not bound.
        /// * returns `Some(None)` if the binding is **neutral** (non-reducible)
        pub fn lookup_value(self, binder: &Identifier) -> Option<Option<Expression>> {
            self.bindings
                .borrow()
                .get(binder)
                .map(|entity| entity.value())
        }

        fn is(self, binder: &Identifier, predicate: fn(&Entity) -> bool) -> bool {
            self.bindings
                .borrow()
                .get(binder)
                .map(predicate)
                .unwrap_or(false)
        }

        pub fn is_constructor(self, binder: &Identifier) -> bool {
            self.is(
                binder,
                |entity| matches!(entity, Entity::Constructor { .. }),
            )
        }

        pub fn is_foreign(self, binder: &Identifier) -> bool {
            self.is(binder, |entity| matches!(entity, Entity::Foreign { .. }))
        }

        /// Try applying foreign binding.
        ///
        /// ## Panics
        ///
        /// Panics if `binder` is either not bound or not foreign.
        pub fn try_applying_foreign_binding(
            self,
            binder: &Identifier,
            arguments: Vec<Expression>,
        ) -> Result<Expression> {
            match self.clone().bindings.borrow()[binder] {
                Entity::Foreign {
                    arity, function, ..
                } => {
                    if arguments.len() == arity {
                        // We normalize the result of the foreign binding to prevent the injection of some kinds of garbage values.
                        // @Question should we run `infer_type` over it as well? I think so
                        // @Task match_with_annotated_type
                        let function_scope = FunctionScope::new(self);
                        evaluate(function(&arguments), &function_scope)
                    } else {
                        Ok(expr! {
                            UnsaturatedForeignApplication {
                                callee: binder.clone(),
                                arguments,
                            }
                        })
                    }
                }
                _ => unreachable!(),
            }
        }

        pub fn assert_is_not_yet_defined(self, binder: Identifier) -> Result<()> {
            if let Some(entity) = self.bindings.borrow().get(&binder) {
                if !matches!(entity, Entity::UntypedForeign { .. }) {
                    return Err(Error::AlreadyDefined(binder));
                }
            }
            Ok(())
        }

        // @Temporary
        pub fn values_of_type_can_be_case_differenciated(self, _binder: &Identifier) -> bool {
            todo!()
        }

        // @Task better API, ah ugly: we need to clone the whole vector because Scope is Rc'd
        pub fn constructors(self, binder: &Identifier) -> Result<Vec<Identifier>, ()> {
            match self.bindings.borrow().get(binder).ok_or(())? {
                Entity::DataType { constructors, .. } => Ok(constructors.clone()),
                _ => Err(()),
            }
        }

        /// Insert a value binding into the scope.
        ///
        /// ## Panics
        ///
        /// Panics under `cfg(debug_assertions)` if `binder` is already bound.
        pub fn insert_value_binding(
            self,
            binder: Identifier,
            r#type: Expression,
            value: Expression,
        ) {
            debug_assert!(self
                .bindings
                .borrow_mut()
                .insert(
                    binder,
                    Entity::Expression {
                        r#type,
                        expression: value,
                    },
                )
                .is_none());
        }

        /// Insert a data type binding into the scope.
        ///
        /// ## Panics
        ///
        /// Panics under `cfg(debug_assertions)` if the `binder` is already bound.
        pub fn insert_data_binding(self, binder: Identifier, r#type: Expression) {
            debug_assert!(self
                .bindings
                .borrow_mut()
                .insert(
                    binder,
                    Entity::DataType {
                        r#type,
                        constructors: Vec::new(),
                    },
                )
                .is_none());
        }

        /// Insert constructor binding into the scope.
        ///
        /// # Panics
        ///
        /// Panics under `cfg(debug_assertions)` if `binder` is already bound and does so
        /// in every case if given data type does not exist or is not a data type.
        pub fn insert_constructor_binding(
            self,
            binder: Identifier,
            r#type: Expression,
            data_type: &Identifier,
        ) {
            let mut bindings = self.bindings.borrow_mut();

            debug_assert!(bindings
                .insert(binder.clone(), Entity::Constructor { r#type })
                .is_none());
            match bindings.get_mut(data_type).unwrap() {
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
        pub fn insert_type_for_foreign_binding(self, binder: Identifier, r#type: Expression) {
            let entity = match self.bindings.borrow()[&binder] {
                Entity::UntypedForeign { arity, function } => Entity::Foreign {
                    r#type,
                    arity,
                    function,
                },
                _ => unreachable!(),
            };
            self.bindings.borrow_mut().insert(binder, entity);
        }

        /// Partially register a foreign binding letting it untyped.
        ///
        /// ## Panics
        ///
        /// Panics under `cfg(debug_assertions)` if the `binder` is already bound.
        pub fn insert_untyped_foreign_binding(
            self,
            binder: &str,
            arity: usize,
            function: ffi::ForeignFunction,
        ) {
            debug_assert!(self
                .bindings
                .borrow_mut()
                .insert(
                    Identifier::from(binder),
                    Entity::UntypedForeign { arity, function }
                )
                .is_none());
        }
    }

    impl fmt::Debug for ModuleScope {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            for (binder, entity) in self.bindings.borrow().iter() {
                writeln!(f, "{} ===> {:?}", binder, entity)?;
            }
            Ok(())
        }
    }
}

pub use function::FunctionScope;

mod function {
    use super::{ModuleScope, Result};
    use crate::hir::{Expression, Identifier};

    /// And entity found in a function scope.
    ///
    /// Right now, that's solely parameters but this might be extended to have the two variants
    /// `Parameter` and `Expression` once we feature `LetIn` in the HIR because of locally nameless.
    enum Entity {
        Parameter { r#type: Expression },
    }

    impl Entity {
        fn r#type(&self) -> Expression {
            match self {
                Self::Parameter { r#type } => r#type.clone(),
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
    pub struct FunctionScope<'parent> {
        inner: Inner<'parent>,
    }

    // @Note probably bad cache behavior as this is just a linked list. but I honestly cannot think
    // of a better data structure right now that does not involve deep-cloning an association list or the like
    enum Inner<'parent> {
        Module(ModuleScope),
        Function {
            parent: &'parent FunctionScope<'parent>,
            binder: Identifier,
            entity: Entity,
        },
    }

    impl FunctionScope<'static> {
        pub fn new(module: ModuleScope) -> Self {
            Self {
                inner: Inner::Module(module),
            }
        }
    }

    impl<'parent> FunctionScope<'parent> {
        pub fn extend_with_parameter(
            &'parent self,
            binder: Identifier,
            r#type: Expression,
        ) -> Self {
            Self {
                inner: Inner::Function {
                    parent: self,
                    binder,
                    entity: Entity::Parameter { r#type },
                },
            }
        }

        fn proxy<T>(
            &self,
            queried_binder: &Identifier,
            handler: fn(&Entity) -> T,
            actual: fn(ModuleScope, &Identifier) -> T,
        ) -> T {
            match &self.inner {
                Inner::Module(module) => actual(module.clone(), queried_binder),
                Inner::Function {
                    parent,
                    binder,
                    entity,
                } => {
                    if queried_binder == binder {
                        handler(entity)
                    } else {
                        parent.proxy(queried_binder, handler, actual)
                    }
                }
            }
        }

        pub fn lookup_type(&self, queried_binder: &Identifier) -> Option<Expression> {
            self.proxy(
                queried_binder,
                |entity| Some(entity.r#type()),
                ModuleScope::lookup_type,
            )
        }

        pub fn lookup_value(&self, queried_binder: &Identifier) -> Option<Option<Expression>> {
            self.proxy(queried_binder, |_| Some(None), ModuleScope::lookup_value)
        }

        pub fn is_constructor(&self, queried_binder: &Identifier) -> bool {
            self.proxy(queried_binder, |_| false, ModuleScope::is_constructor)
        }

        pub fn is_foreign(&self, queried_binder: &Identifier) -> bool {
            self.proxy(queried_binder, |_| false, ModuleScope::is_foreign)
        }

        pub fn constructors(&self, queried_binder: &Identifier) -> Result<Vec<Identifier>, ()> {
            self.proxy(queried_binder, |_| Err(()), ModuleScope::constructors)
        }

        /// Try applying foreign binding.
        ///
        /// ## Panics
        ///
        /// Panics if `binder` is either not bound or not foreign.
        pub fn try_applying_foreign_binding(
            &self,
            binder: &Identifier,
            arguments: Vec<Expression>,
        ) -> Result<Expression> {
            match &self.inner {
                Inner::Module(module) => module
                    .clone()
                    .try_applying_foreign_binding(binder, arguments),
                Inner::Function { .. } => unreachable!(),
            }
        }
    }
}

pub use substitutions::Substitutions;

mod substitutions {
    use crate::hir::{Expression, Identifier};

    /// List of substitions.
    ///
    /// Probably bad cache behavior but it will be obsolete anyways once we are locally nameless.
    pub struct Substitutions<'parent> {
        inner: Inner<'parent>,
    }

    enum Inner<'parent> {
        Empty,
        Substitution {
            parent: &'parent Substitutions<'parent>,
            binder: Identifier,
            expression: Expression,
        },
    }

    impl Substitutions<'static> {
        /// Create a new empty substitution environment.
        pub fn new() -> Self {
            Substitutions {
                inner: Inner::Empty,
            }
        }
    }

    impl<'parent> Substitutions<'parent> {
        /// Extend the list of substitutions.
        pub fn extend_with(&'parent self, binder: Identifier, expression: Expression) -> Self {
            Substitutions {
                inner: Inner::Substitution {
                    parent: self,
                    binder,
                    expression,
                },
            }
        }

        pub fn retrieve(&self, queried_binder: &Identifier) -> Option<Expression> {
            match &self.inner {
                Inner::Empty => None,
                Inner::Substitution {
                    parent,
                    binder,
                    expression,
                } => {
                    if queried_binder == binder {
                        Some(expression.clone())
                    } else {
                        parent.retrieve(queried_binder)
                    }
                }
            }
        }
    }
}
