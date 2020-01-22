// @Task update docs
//! This module exposes an [`Environment`] and a [`ModuleScope`].
//!
//! What's the difference between those two? Well, apart from them not being
//! perfectly named, the first one represents function scoping and the second
//! one module scoping.
//!
//! As an aside, the [`ModuleScope`] contains a lot more information.
//!
//! The scoping rules are vastly different between those two kinds.
//! Environments (lambdas and let/ins) can store neitherdata nor foreign nor module declarations,
//! only let and use declarations.
//! In function scopes, declarations shadow other ones with the same name.
//! In module scope, declarations may appear out of order and cross-reference each other
//! as long as there is no cyclic dependency. Recursion is allowed.
//! Not so in function scopes! Since lambdas and let/ins are nested, they are ordered and
//! most importantly, recursion only works explicitly via the fix-point-combinator.
//!
//! **Note**: This module needs a lot of love. Currently, it's a big hack with a bad API.
//! Performance is bad I guess and a lot, a lot of memory is wasted! We need to refactor it
//! several times until I will be happy.
//!
//! **UPDATE**: I think what I said above does not hold! We use the ModuleContext for
//! function scoping as well! Of course! Like adding typed parameters to the context
//! with `extend_with_neutral_binding`! I think environment is solely for substitution.

mod ffi;

use super::{Error, Result};

pub use module::ModuleScope;

mod module {
    use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

    use super::{ffi, Error, FunctionScope, Result};
    use crate::effluvium::normalize;
    use crate::hir::{expr, Expression, Identifier};

    // @Question Expression<Normalized>?
    #[derive(Clone)]
    enum ModuleEntity {
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

    impl ModuleEntity {
        /// Retrieve the type of an entity.
        ///
        /// # Panics
        ///
        /// Panics if called on an [ModuleEntity::UntypedForeign].
        fn r#type(&self) -> Expression {
            match self {
                Self::Expression { r#type, .. } => r#type.clone(),
                Self::DataType { r#type, .. } => r#type.clone(),
                Self::Constructor { r#type, .. } => r#type.clone(),
                Self::UntypedForeign { .. } => unreachable!(),
                Self::Foreign { r#type, .. } => r#type.clone(),
            }
        }

        // None means the entity was neutral/non-reducible
        fn value(&self) -> Option<Expression> {
            match self {
                ModuleEntity::Expression { expression, .. } => Some(expression.clone()),
                _ => None,
            }
        }
    }

    impl fmt::Debug for ModuleEntity {
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

    #[derive(Default, Clone)]
    pub struct ModuleScope {
        bindings: Rc<RefCell<HashMap<Identifier, ModuleEntity>>>,
    }

    impl ModuleScope {
        pub fn new() -> Self {
            let scope = Self::default();
            ffi::register_foreign_bindings(scope.clone());
            scope
        }

        pub fn lookup_type(self, binder: &Identifier) -> Option<Expression> {
            self.bindings.borrow().get(binder).map(ModuleEntity::r#type)
        }

        // @Task document
        pub fn lookup_value(self, binder: &Identifier) -> Option<Option<Expression>> {
            self.bindings
                .borrow()
                .get(binder)
                .map(|entity| entity.value())
        }

        fn is(self, binder: &Identifier, predicate: fn(&ModuleEntity) -> bool) -> bool {
            self.bindings
                .borrow()
                .get(binder)
                .map(predicate)
                .unwrap_or(false)
        }

        pub fn is_constructor(self, binder: &Identifier) -> bool {
            self.is(
                binder,
                |entity| matches!(entity, ModuleEntity::Constructor { .. }),
            )
        }

        pub fn is_foreign(self, binder: &Identifier) -> bool {
            self.is(
                binder,
                |entity| matches!(entity, ModuleEntity::Foreign { .. }),
            )
        }

        /// Try applying foreign binding.
        ///
        /// # Panics
        ///
        /// Panics if `binder` is unmapped or not foreign.
        pub fn try_applying_foreign_binding(
            self,
            binder: &Identifier,
            arguments: Vec<Expression>,
        ) -> Result<Expression> {
            match self.clone().bindings.borrow()[binder] {
                ModuleEntity::Foreign {
                    arity, function, ..
                } => {
                    if arguments.len() == arity {
                        // We normalize the result of the foreign binding to prevent the injection of some kinds of garbage values.
                        // @Question should we run `infer_type` over it as well? I think so
                        // @Task match_with_annotated_type
                        let function_scope = FunctionScope::new(self);
                        normalize(function(&arguments), &function_scope)
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
                if !matches!(entity, ModuleEntity::UntypedForeign { .. }) {
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
                ModuleEntity::DataType { constructors, .. } => Ok(constructors.clone()),
                _ => Err(()),
            }
        }

        pub fn insert_value_binding(
            self,
            binder: Identifier,
            r#type: Expression,
            value: Expression,
        ) {
            self.bindings.borrow_mut().insert(
                binder,
                ModuleEntity::Expression {
                    r#type,
                    expression: value,
                },
            );
        }

        pub fn insert_data_binding(self, binder: Identifier, r#type: Expression) {
            self.bindings.borrow_mut().insert(
                binder,
                ModuleEntity::DataType {
                    r#type,
                    constructors: Vec::new(),
                },
            );
        }

        /// Insert constructor binding into the scope.
        ///
        /// # Panics
        ///
        /// Panics if given data type does not exist or is not a data type
        pub fn insert_constructor_binding(
            self,
            binder: Identifier,
            r#type: Expression,
            data_type: &Identifier,
        ) {
            let mut bindings = self.bindings.borrow_mut();

            bindings.insert(binder.clone(), ModuleEntity::Constructor { r#type });
            match bindings.get_mut(data_type).unwrap() {
                ModuleEntity::DataType {
                    ref mut constructors,
                    ..
                } => constructors.push(binder),
                _ => unreachable!(),
            }
        }

        // @Task docs, panics
        pub fn insert_type_for_foreign_binding(self, binder: Identifier, r#type: Expression) {
            let entity = match self.bindings.borrow()[&binder] {
                ModuleEntity::UntypedForeign { arity, function } => ModuleEntity::Foreign {
                    r#type,
                    arity,
                    function,
                },
                _ => unreachable!(),
            };
            self.bindings.borrow_mut().insert(binder, entity);
        }

        // @Task docs, panics
        pub fn insert_untyped_foreign_binding(
            self,
            binder: &str,
            arity: usize,
            function: ffi::ForeignFunction,
        ) {
            assert!(self
                .bindings
                .borrow_mut()
                .insert(
                    Identifier::from(binder),
                    ModuleEntity::UntypedForeign { arity, function }
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

    // @Note this might be extended to have two variants `Parameter` and `Expression`
    // once we feature `LetIn` in the HIR because of locally nameless/debruijn-indexing
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

        // @Task document, may panic
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

// @Beacon @Task make this its own type with helpful methods so we don't
// need to write that much boilerplate in crate::effluvium anymore!
// @Question why do we need to own Expression? that results in a lot of cloning!!
// what about a Cow<'a, Expression>?? or are gonna get lifetime issues?
/// Environment used for substitions.
///
/// Will be obsolete once we use locally nameless/Debruijn-indexing
pub type Environment =
    std::rc::Rc<std::collections::HashMap<crate::hir::Identifier, crate::hir::Expression>>;
