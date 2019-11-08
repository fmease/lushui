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

use super::{Expression, Identifier};
use std::fmt;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

// @Note expressions aren't the only entities: modules are too, gonna be added later
#[derive(Clone, Debug)]
enum Entity {
    // used for recursion
    _UnresolvedExpression { type_: Expression },
    // @Note technically, an ADT is a neutral expression, too @Question better naming?
    // @Note formal parameters
    NeutralExpression { type_: Expression },
    ResolvedExpression { type_: Expression, expr: Expression },
    // @Question additional payload? constructors?
    _AlgebraicDataType { type_: Expression },
}

impl Entity {
    // @Note returns Option because UnresolvedExpression does not have a known type_
    // in our current model
    fn retrieve_type(&self) -> &Expression {
        match self {
            Self::_UnresolvedExpression { type_ } => type_,
            Self::NeutralExpression { type_ } => type_,
            Self::ResolvedExpression { type_, .. } => type_,
            Self::_AlgebraicDataType { type_, .. } => type_,
        }
    }

    fn retrieve_value(&self) -> Option<&Expression> {
        if let Entity::ResolvedExpression { expr, .. } = self {
            Some(expr)
        } else {
            None
        }
    }
}

// @Temporary
impl fmt::Display for Entity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::_UnresolvedExpression { type_ } => write!(f, "UNRESOLVED {{ {} }}", type_),
            Self::NeutralExpression { type_ } => write!(f, "NEUTRAL {{ {} }}", type_),
            Self::ResolvedExpression { type_, expr } => {
                write!(f, "RESOLVED {{ {} }} {{ {} }}", type_, expr)
            }
            Self::_AlgebraicDataType { type_ } => write!(f, "ADT {{ {} }}", type_),
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
#[derive(Default, Clone, Debug)]
pub struct ModuleContext {
    // @Task @Question add binders: Vec<Variable>?
    bindings: Rc<RefCell<HashMap<Identifier, Entity>>>,
    // @Question do we really need this??
    adts: Rc<RefCell<HashMap<Identifier, Vec<Identifier>>>>,
    last_generated_numeric_identifier: Rc<RefCell<u64>>,
}

impl ModuleContext {
    pub fn contains(self, binding: &Identifier) -> bool {
        self.bindings.borrow().contains_key(binding)
    }

    pub fn adt(self, binding: &Identifier) -> bool {
        self.adts.borrow().contains_key(binding)
    }

    // @Note assumes binding is indeed an ADT @Note panics
    // @Note ugly: clones ... I just want to introspect the value
    // ... CPS would work here
    pub fn constructors(self, binding: &Identifier) -> Vec<Identifier> {
        self.adts.borrow()[binding].clone()
    }

    pub fn lookup_type(self, binding: &Identifier) -> Option<Expression> {
        self.bindings
            .borrow()
            .get(binding)
            .map(Entity::retrieve_type)
            .cloned()
    }

    pub fn lookup_value(self, binding: &Identifier) -> Option<Option<Expression>> {
        self.bindings
            .borrow()
            .get(binding)
            .map(|entity| entity.retrieve_value().cloned())
    }

    #[must_use]
    pub fn extend_with_binding(
        self,
        binding: Identifier,
        type_: Expression,
        value: Expression,
    ) -> Self {
        let mut map = self.bindings.as_ref().borrow().clone();
        map.insert(binding, Entity::ResolvedExpression { type_, expr: value });
        ModuleContext {
            bindings: Rc::new(RefCell::new(map)),
            ..self
        }
    }

    #[must_use]
    pub fn extend_with_neutral_binding(self, binding: Identifier, type_: Expression) -> Self {
        let mut map = self.bindings.as_ref().borrow().clone();
        map.insert(binding, Entity::NeutralExpression { type_ });
        ModuleContext {
            bindings: Rc::new(RefCell::new(map)),
            ..self
        }
    }

    /*
    // @Note important: see note above Context before impl'ing this
    pub fn extend_with_adt(self, binder: Variable: type_: Expr, xxx) -> Self {

    }*/

    // @Task remove: We should only use extend_with_bindings even in the REPL
    pub fn insert_binding(self, binding: Identifier, type_: Expression, value: Expression) {
        self.bindings
            .borrow_mut()
            .insert(binding, Entity::ResolvedExpression { type_, expr: value });
    }

    // @Task remove
    pub fn insert_neutral_binding(self, binding: Identifier, type_: Expression) {
        self.bindings
            .borrow_mut()
            .insert(binding, Entity::NeutralExpression { type_ });
    }

    // @Task remove
    pub fn insert_adt(self, binding: Identifier, type_: Expression) {
        self.clone().insert_neutral_binding(binding.clone(), type_);
        self.adts.borrow_mut().insert(binding, Vec::new());
    }

    // @Task remove
    // @Note panics if adt does not exist
    pub fn insert_constructor(self, binding: Identifier, type_: Expression, adt: &Identifier) {
        self.clone().insert_neutral_binding(binding.clone(), type_);
        self.adts.borrow_mut().get_mut(adt).unwrap().push(binding);
    }

    pub fn generate_numeric_identifier(self) -> u64 {
        *self.last_generated_numeric_identifier.borrow_mut() += 1;
        *self.last_generated_numeric_identifier.borrow()
    }
}

// @Temporary
impl fmt::Display for ModuleContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "[[BINDINGS]]")?;
        for (binding, entity) in self.bindings.borrow().iter() {
            writeln!(f, "{} ===> {}", binding, entity)?;
        }
        writeln!(f, "[[ADTS]]")?;
        for (binding, constructor_bindings) in self.adts.borrow().iter() {
            writeln!(
                f,
                "{} ===> {}",
                binding,
                constructor_bindings
                    .into_iter()
                    .map(|binding| format!("| {} ", binding))
                    .collect::<String>()
            )?;
        }
        Ok(())
    }
}

pub type Environment = Rc<HashMap<Identifier, Expression>>;
