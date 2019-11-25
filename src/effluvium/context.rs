// @Note this module is badly named.

//! This module exposes an [`Environment`] and a [`ModuleContext`].
//!
//! What's the difference between those two? Well, apart from them not being
//! perfectly named, the first one represents function scoping and the second
//! one module scoping.
//!
//! As an aside, the [`ModuleContext`] contains a lot more information.
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

use super::{Expression, Identifier};
use std::fmt;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

// @Question Rc<Expression>?
// @Question Expression<Normalized>?
#[derive(Clone, Debug)]
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
}

impl Entity {
    fn r#type(&self) -> &Expression {
        match self {
            Self::Expression { r#type, .. } => r#type,
            Self::DataType { r#type, .. } => r#type,
            Self::Constructor { r#type, .. } => r#type,
            Self::Parameter { r#type, .. } => r#type,
        }
    }

    fn retrieve_value(&self) -> Option<&Expression> {
        match self {
            Entity::Expression { expression, .. } => Some(expression),
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
                "'data: {} = {}",
                r#type,
                constructors
                    .iter()
                    .map(|constructor| format!("{} ", constructor))
                    .collect::<String>()
            ),
            Self::Constructor { r#type } => write!(f, "'constructor: {}", r#type),
            Self::Parameter { r#type } => write!(f, "'parameter: {}", r#type),
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
// @Task rename to ModuleScope
// @Update @Beacon @Note once we separate ModuleScope and FunctionScope, this
// does not need to be reference counted anymore, right? we can just use plain old references
#[derive(Default, Clone, Debug)]
pub struct ModuleScope {
    bindings: Rc<RefCell<HashMap<Identifier, Entity>>>,
    // @Note only used for substitution, should be moved to future FunctionContext/FunctionScope
    last_generated_numeric_identifier: Rc<RefCell<u64>>,
}

impl ModuleScope {
    pub fn contains(self, binding: &Identifier) -> bool {
        self.bindings.borrow().contains_key(binding)
    }

    pub fn lookup_type(self, binding: &Identifier) -> Option<Expression> {
        self.bindings
            .borrow()
            .get(binding)
            .map(Entity::r#type)
            .cloned()
    }

    pub fn lookup_value(self, binding: &Identifier) -> Option<Option<Expression>> {
        self.bindings
            .borrow()
            .get(binding)
            .map(|entity| entity.retrieve_value().cloned())
    }

    // @Beacon @Beacon @Beacon @Bug @Bug this deeply clones the *whole* HashMap (set of declarations!!)
    // every time we add a binding into the scope!!!! HORRIBLE!!!
    pub fn extend_with_binding(
        self,
        binding: Identifier,
        r#type: Expression,
        value: Expression,
    ) -> Self {
        let mut map = self.bindings.as_ref().borrow().clone();
        map.insert(
            binding,
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
    pub fn extend_with_parameter(self, binding: Identifier, r#type: Expression) -> Self {
        let mut map = self.bindings.as_ref().borrow().clone();
        map.insert(binding, Entity::Parameter { r#type });
        Self {
            bindings: Rc::new(RefCell::new(map)),
            ..self
        }
    }

    pub fn insert_binding(self, binding: Identifier, r#type: Expression, value: Expression) {
        self.bindings.borrow_mut().insert(
            binding,
            Entity::Expression {
                r#type,
                expression: value,
            },
        );
    }

    pub fn insert_parameter(self, binding: Identifier, r#type: Expression) {
        self.bindings
            .borrow_mut()
            .insert(binding, Entity::Parameter { r#type });
    }

    pub fn insert_data_type(self, binding: Identifier, r#type: Expression) {
        self.bindings.borrow_mut().insert(
            binding,
            Entity::DataType {
                r#type,
                constructors: Vec::new(),
            },
        );
    }

    /// Panics if given data type does not exist or is not a data type
    pub fn insert_constructor(
        self,
        binding: Identifier,
        r#type: Expression,
        data_type: &Identifier,
    ) {
        let mut bindings = self.bindings.borrow_mut();

        bindings.insert(binding.clone(), Entity::Constructor { r#type });
        match bindings.get_mut(data_type).unwrap() {
            Entity::DataType {
                ref mut constructors,
                ..
            } => constructors.push(binding),
            _ => unreachable!(),
        }
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
