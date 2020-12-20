//! The entity system: Information about bindings for the name resolver _and_ the type checker.
//!
//! Just like [CrateScope], [Entity] is a resource shared by those two passes.

use std::fmt;

use crate::{
    hir::Expression,
    resolver::{CrateIndex, CrateScope, Identifier, Namespace},
    support::DisplayWith,
    typer::interpreter::{ffi::NakedForeignFunction, scope::ValueView},
};

/// Something that can be bound to an identifier.
///
/// The second component of a binding where a _binding_ is a pair of a binder
/// and an entity. Where _binder_ is synonym for identifier.
///
/// This generalizes the notion of a "value" or an "expression" by including
/// standard _values_ or _expressions_ defined as (in case of values: a normalized
/// form of) something that can have _type_ (in the semantic sense) **but also**
/// second-class things like modules, use "links", atomic data types, constructors and foreign functions.
///
/// Most of them are just as well expressions, as can be seen by [Entity::value] which
/// panics on those that are not.
#[derive(Clone)]
pub struct Entity {
    /// Source information of the definition site.
    pub source: crate::ast::Identifier,
    /// The namespace this entity is a member of.
    pub parent: Option<CrateIndex>,
    pub kind: EntityKind,
}

impl Entity {
    pub const fn is_untyped(&self) -> bool {
        use EntityKind::*;
        matches!(self.kind, UntypedValue | UntypedDataType(_) | UntypedConstructor(_))
    }

    pub fn namespace(&self) -> Option<&Namespace> {
        use EntityKind::*;
        match &self.kind {
            Module(namespace) | UntypedDataType(namespace) | UntypedConstructor(namespace) => {
                Some(namespace)
            }
            _ => None,
        }
    }

    pub fn namespace_mut(&mut self) -> Option<&mut Namespace> {
        use EntityKind::*;
        match &mut self.kind {
            Module(namespace) | UntypedDataType(namespace) | UntypedConstructor(namespace) => {
                Some(namespace)
            }
            _ => None,
        }
    }

    pub const fn is_value_without_value(&self) -> bool {
        matches!(self.kind, EntityKind::Value { expression: None, .. })
    }

    pub fn type_(&self) -> Option<Expression> {
        use EntityKind::*;

        Some(
            match &self.kind {
                Value { type_, .. } => type_,
                DataType { type_, .. } => type_,
                Constructor { type_, .. } => type_,
                Foreign { type_, .. } => type_,
                UntypedValue | UntypedDataType(_) | UntypedConstructor(_) => return None,
                _ => unreachable!(),
            }
            .clone(),
        )
    }

    /// Retrieve the value of an entity
    ///
    /// ## Panics
    ///
    /// Panics if the entity can not be represented as an expression/value like modules
    /// (because they are second-class by specification) or untyped entities which are
    /// not ready yet.
    pub fn value(&self) -> ValueView {
        use EntityKind::*;

        match &self.kind {
            Value {
                expression: Some(expression),
                ..
            } => ValueView::Reducible(expression.clone()),
            Value {
                expression: None, ..
            } => ValueView::Neutral,
            DataType { .. } | Constructor { .. } | Foreign { .. } => ValueView::Neutral,
            UntypedValue
            | UntypedDataType(_)
            | UntypedConstructor(_)
            | Module(_)
            | Use(_)
            | UnresolvedUse => unreachable!(),
        }
    }
}

/// Textual representation of what lies beyond the crate root.
pub const BEYOND_CRATE_ROOT: &str = "crate.super";

impl DisplayWith for Entity {
    type Linchpin = <EntityKind as DisplayWith>::Linchpin;

    fn format(&self, scope: &CrateScope, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use std::borrow::Cow;

        write!(
            f,
            "{:>11}.{:20} |-> {}",
            self.parent
                .map_or(Cow::Borrowed(BEYOND_CRATE_ROOT), |parent| format!(
                    "{:?}",
                    parent
                )
                .into()),
            self.source,
            self.kind.with(scope)
        )
    }
}

#[derive(Clone)]
pub enum EntityKind {
    UntypedValue,
    Module(Namespace),
    /// Data types are not [Self::UntypedValue] as they are also namespaces containing constructors.
    UntypedDataType(Namespace),
    /// Constructors are not [Self::UntypedValue] as they are also namespaces containing fields.
    UntypedConstructor(Namespace),
    /// Invariant: The "target" is never a Use itself. There are no nested aliases
    Use(CrateIndex),
    UnresolvedUse,

    Value {
        type_: Expression,
        expression: Option<Expression>,
    },
    // @Question should we store the constructors?
    DataType {
        type_: Expression,
        constructors: Vec<Identifier>,
    },
    Constructor {
        type_: Expression,
    },
    Foreign {
        type_: Expression,
        arity: usize,
        function: NakedForeignFunction,
    },
}

impl EntityKind {
    pub fn module() -> Self {
        Self::Module(Namespace::default())
    }

    pub fn untyped_constructor() -> Self {
        Self::UntypedConstructor(Namespace::default())
    }

    pub fn untyped_data_type() -> Self {
        Self::UntypedDataType(Namespace::default())
    }
}

impl DisplayWith for EntityKind {
    type Linchpin = CrateScope;

    fn format(&self, scope: &CrateScope, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use EntityKind::*;

        match self {
            UntypedValue => write!(f, "untyped value"),
            Module(namespace) => write!(f, "module: {:?}", namespace),
            UntypedDataType(namespace) => write!(f, "untyped data type: {:?}", namespace),
            UntypedConstructor(namespace) => write!(f, "untyped constructor: {:?}", namespace),
            Use(index) => write!(f, "use {:?}", index),
            UnresolvedUse => write!(f, "unresolved use"),
            Value { type_, expression } => match expression {
                Some(expression) => write!(f, "{}: {}", expression.with(scope), type_.with(scope)),
                None => write!(f, ": {}", type_.with(scope)),
            },
            DataType {
                type_,
                constructors,
            } => write!(
                f,
                "data: {} = {}",
                type_.with(scope),
                constructors
                    .iter()
                    .map(|constructor| format!("{} ", constructor))
                    .collect::<String>()
            ),
            Constructor { type_ } => write!(f, "constructor: {}", type_.with(scope)),
            Foreign { type_, .. } => write!(f, "foreign: {}", type_.with(scope)),
        }
    }
}
