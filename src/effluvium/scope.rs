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

use super::{Error, Expression, Identifier, Result};
use std::fmt;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

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
    // @Temporary @Note move this kind of entity out of ModuleContext, it belongs to a not-yet-existing FunctionContext
    // @Note will be replaced by Debruijn-indeces
    Parameter {
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
    // @Note this method can be made obsolete if we decomposed Entity into
    // a struct Entity { r#type: Expression, kind: EntityKind } with enum EntityKind
    // might not be forward-compatible though (adding entities which are not typed)
    /// Retrieve the type of an entity.
    ///
    /// # Panics
    ///
    /// Panics if called on an [Entity::UntypedForeign].
    fn r#type(&self) -> Expression {
        match self {
            Self::Expression { r#type, .. } => r#type.clone(),
            Self::DataType { r#type, .. } => r#type.clone(),
            Self::Constructor { r#type, .. } => r#type.clone(),
            Self::Parameter { r#type, .. } => r#type.clone(),
            Self::UntypedForeign { .. } => unreachable!(),
            Self::Foreign { r#type, .. } => r#type.clone(),
        }
    }

    fn retrieve_value(&self) -> Option<Expression> {
        match self {
            Entity::Expression { expression, .. } => Some(expression.clone()),
            _ => None,
        }
    }
}

// @Temporary
impl fmt::Display for Entity {
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
            Self::Parameter { r#type } => write!(f, "parameter: {}", r#type),
            Self::UntypedForeign { .. } => f.write_str("untyped foreign"),
            Self::Foreign { r#type, .. } => write!(f, "foreign: {}", r#type),
        }
    }
}

// @Note needs refactoring
// @Note ModuleContext has a really really really bad API, geezz!!!
// @Note currently, ADTs and its constructors cannot be shadowed, but this will change.
// Once this happens, the mutating methods on Context will be removed and the methods
// immutably extending the Context also need to update ADTContext i.e. returning
// (Context, ADTContext) or even better storing the latter inside the former.
// But for now, variables refering to ADTs and constructors are global/globally unique.
// @Update @Beacon @Note once we separate ModuleScope and FunctionScope, this
// does not need to be reference counted anymore, right? we can just use plain old references
#[derive(Default, Clone)]
pub struct ModuleScope {
    bindings: Rc<RefCell<HashMap<Identifier, Entity>>>,
    // @Note only used for substitution, should be moved to future FunctionContext/FunctionScope
    last_generated_numeric_identifier: Rc<RefCell<u64>>,
}

impl ModuleScope {
    pub fn new() -> Self {
        let scope = Self::default();
        ffi::register_foreign_bindings(scope.clone());
        scope
    }

    fn contains(self, binder: &Identifier) -> bool {
        self.bindings.borrow().contains_key(binder)
    }

    pub fn lookup_type(self, binder: &Identifier) -> Option<Expression> {
        self.bindings.borrow().get(binder).map(Entity::r#type)
    }

    // @Task document
    pub fn lookup_value(self, binder: &Identifier) -> Option<Option<Expression>> {
        self.bindings
            .borrow()
            .get(binder)
            .map(|entity| entity.retrieve_value())
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
    /// # Panics
    ///
    /// Panics if `binder` is unmapped or not foreign.
    pub fn try_applying_foreign_binding(
        self,
        binder: &Identifier,
        arguments: Vec<Expression>,
    ) -> Expression {
        match self.bindings.borrow()[binder] {
            Entity::Foreign {
                arity, function, ..
            } => {
                if arguments.len() == arity {
                    function(&arguments)
                } else {
                    super::expr! {
                        UnsaturatedForeignApplication {
                            callee: binder.clone(),
                            arguments,
                        }
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn assert_is_not_yet_defined(self, binder: Identifier) -> Result<()> {
        if self.contains(&binder) {
            Err(Error::AlreadyDefined(binder))
        } else {
            Ok(())
        }
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

    // @Beacon @Beacon @Beacon @Bug @Bug this deeply clones the *whole* HashMap (set of declarations!!)
    // every time we add a binding into the scope!!!! HORRIBLE!!!
    pub fn extend_with_binding(
        self,
        binder: Identifier,
        r#type: Expression,
        value: Expression,
    ) -> Self {
        let mut map = self.bindings.as_ref().borrow().clone();
        map.insert(
            binder,
            Entity::Expression {
                r#type,
                expression: value,
            },
        );
        Self {
            bindings: Rc::new(RefCell::new(map)),
            ..self
        }
    }

    // @Beacon @Beacon @Beacon @Bug @Bug this deeply clones the *whole* HashMap (set of declarations!!)
    // every time we add a parameter into the scope!!!! HORRIBLE!!!!
    pub fn extend_with_parameter(self, binder: Identifier, r#type: Expression) -> Self {
        let mut map = self.bindings.as_ref().borrow().clone();
        map.insert(binder, Entity::Parameter { r#type });
        Self {
            bindings: Rc::new(RefCell::new(map)),
            ..self
        }
    }

    pub fn insert_value_binding(self, binder: Identifier, r#type: Expression, value: Expression) {
        self.bindings.borrow_mut().insert(
            binder,
            Entity::Expression {
                r#type,
                expression: value,
            },
        );
    }

    pub fn insert_parameter_binding(self, binder: Identifier, r#type: Expression) {
        self.bindings
            .borrow_mut()
            .insert(binder, Entity::Parameter { r#type });
    }

    pub fn insert_data_binding(self, binder: Identifier, r#type: Expression) {
        self.bindings.borrow_mut().insert(
            binder,
            Entity::DataType {
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

        bindings.insert(binder.clone(), Entity::Constructor { r#type });
        match bindings.get_mut(data_type).unwrap() {
            Entity::DataType {
                ref mut constructors,
                ..
            } => constructors.push(binder),
            _ => unreachable!(),
        }
    }

    // @Task docs, panics
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
                Entity::UntypedForeign { arity, function }
            )
            .is_none());
    }

    pub fn generate_numeric_identifier(self) -> u64 {
        *self.last_generated_numeric_identifier.borrow_mut() += 1;
        *self.last_generated_numeric_identifier.borrow()
    }
}

// @Temporary
impl fmt::Display for ModuleScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (binder, entity) in self.bindings.borrow().iter() {
            writeln!(f, "{} ===> {}", binder, entity)?;
        }
        Ok(())
    }
}

// @Task does it store a reference to it?
pub struct _FunctionScope {
    module: ModuleScope,
}

// @Beacon @Task make this its own type with helpful methods so we don't
// need to write that much boilerplate in crate::effluvium anymore!
// @Question why do we need to own Expression? that results in a lot of cloning!!
// what about a Cow<'a, Expression>?? or are gonna get lifetime issues?
pub type Environment = Rc<HashMap<Identifier, Expression>>;
