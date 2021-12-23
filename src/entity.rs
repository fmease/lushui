//! The entity system: Information about bindings for the name resolver _and_ the type checker.
//!
//! Just like [`Crate`], [`Entity`] is a resource shared by those two passes.

use crate::{
    error::PossiblyErroneous,
    format::DisplayWith,
    hir::{DeclarationIndex, Expression, Identifier, LocalDeclarationIndex},
    package::{session::BareIntrinsicFunctionValue, BuildSession},
    resolver::{Crate, Exposure, Namespace},
    typer::interpreter::scope::ValueView,
    utility::obtain,
};
use std::{default::default, fmt};
use EntityKind::*;

/// Something that can be bound to an identifier.
///
/// The second component of a binding where a _binding_ is a pair of a binder
/// and an entity. Where _binder_ is synonym for identifier.
///
/// This generalizes the notion of a "value" or an "expression" by including
/// standard _values_ or _expressions_ defined as (in case of values: a normalized
/// form of) something that can have _type_ (in the semantic sense) **but also**
/// second-class things like modules, use "links", atomic data types,
/// constructors and intrinsic functions.
///
/// Most of them are just as well expressions, as can be seen by [`Entity::value`] which
/// panics on those that are not.
#[derive(Clone)]
pub(crate) struct Entity {
    /// Source information of the definition site.
    pub(crate) source: crate::syntax::ast::Identifier,
    /// The namespace this entity is a member of.
    // @Question should we make this a DeclarationIndex?
    pub(crate) parent: Option<LocalDeclarationIndex>,
    pub(crate) exposure: Exposure,
    pub(crate) kind: EntityKind,
}

impl Entity {
    pub(crate) const fn is_untyped_value(&self) -> bool {
        matches!(
            self.kind,
            UntypedFunction | UntypedDataType { .. } | UntypedConstructor { .. }
        )
    }

    /// Returns `true` if the entity is a typed or untyped function.
    #[allow(dead_code)]
    pub(crate) const fn is_function(&self) -> bool {
        matches!(self.kind, UntypedFunction | Function { .. })
    }

    /// Returns `true` if the entity is a typed or untyped data type.
    pub(crate) const fn is_data_type(&self) -> bool {
        matches!(self.kind, UntypedDataType { .. } | DataType { .. })
    }

    /// Returns `true` if the entity is a typed or untyped constructor.
    #[allow(dead_code)]
    pub(crate) const fn is_constructor(&self) -> bool {
        matches!(self.kind, UntypedConstructor { .. } | Constructor { .. })
    }

    pub(crate) const fn is_module(&self) -> bool {
        matches!(self.kind, Module { .. })
    }

    pub(crate) const fn is_intrinsic(&self) -> bool {
        matches!(self.kind, Intrinsic { .. })
    }

    pub(crate) const fn is_error(&self) -> bool {
        matches!(self.kind, EntityKind::Error)
    }

    pub(crate) const fn is_namespace(&self) -> bool {
        self.is_module() || self.is_data_type()
    }

    pub(crate) const fn namespace(&self) -> Option<&Namespace> {
        obtain!(
            &self.kind,
            Module { namespace }
            | UntypedDataType { namespace }
            | DataType { namespace, .. } => namespace,
        )
    }

    pub(crate) fn namespace_mut(&mut self) -> Option<&mut Namespace> {
        obtain!(
            &mut self.kind,
            Module { namespace }
            | UntypedDataType { namespace }
            | DataType { namespace, .. } => namespace,
        )
    }

    pub(crate) const fn is_value_without_value(&self) -> bool {
        matches!(
            self.kind,
            Function {
                expression: None,
                ..
            }
        )
    }

    pub(crate) fn type_(&self) -> Option<Expression> {
        obtain!(
            &self.kind,
            Function { type_, .. } |
            DataType { type_, .. } |
            Constructor { type_, .. } |
            Intrinsic { type_, .. } => type_.clone(),
        )
    }

    /// Retrieve the value of an entity
    ///
    /// # Panics
    ///
    /// Panics if the entity can not be represented as an expression/value like modules
    /// (because they are second-class by specification) or untyped entities which are
    /// not ready yet.
    pub(crate) fn value(&self) -> ValueView {
        match &self.kind {
            Function {
                expression: Some(expression),
                ..
            } => ValueView::Reducible(expression.clone()),
            Function {
                expression: None, ..
            }
            | Error
            | DataType { .. }
            | Constructor { .. }
            | Intrinsic { .. } => ValueView::Neutral,
            UntypedFunction
            | UntypedDataType { .. }
            | UntypedConstructor { .. }
            | Module { .. }
            | Use { .. }
            | UnresolvedUse => unreachable!(),
        }
    }

    pub(crate) fn mark_as_error(&mut self) {
        self.kind = Error;
    }
}

impl DisplayWith for Entity {
    type Context<'a> = <EntityKind as DisplayWith>::Context<'a>;

    fn format(&self, context: Self::Context<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let parent = self
            .parent
            .map(|parent| format!("{parent:?}."))
            .unwrap_or_default();
        let source = &self.source;
        let exposure = &self.exposure;
        let kind = self.kind.with(context);

        write!(f, "{parent:>5}{source:20} {exposure:?} |-> {kind}")
    }
}

#[derive(Clone)]
pub(crate) enum EntityKind {
    UntypedFunction,
    Module {
        namespace: Namespace,
    },
    /// Data types are not [`Self::UntypedFunction`] as they are also namespaces containing constructors.
    UntypedDataType {
        namespace: Namespace,
    },
    UntypedConstructor,
    /// The `reference is never a `Use` itself.
    /// Nested aliases were already collapsed by [`crate::resolver::Resolver::collapse_use_chain`].
    Use {
        reference: DeclarationIndex,
    },
    UnresolvedUse,

    Function {
        type_: Expression,
        expression: Option<Expression>,
    },
    // @Question should we store the constructors?
    DataType {
        namespace: Namespace,
        type_: Expression,
        constructors: Vec<Identifier>,
    },
    Constructor {
        type_: Expression,
    },
    Intrinsic {
        type_: Expression,
        // @Beacon @Beacon @Beacon @Task wrap this in an IntrinsicFnVal
        arity: usize,
        function: BareIntrinsicFunctionValue,
    },
    // @Task explain why we want entities to be possibly erroneous
    Error,
}

impl PossiblyErroneous for EntityKind {
    fn error() -> Self {
        Self::Error
    }
}

impl EntityKind {
    pub(crate) fn module() -> Self {
        Module {
            namespace: default(),
        }
    }

    pub(crate) fn untyped_data_type() -> Self {
        UntypedDataType {
            namespace: default(),
        }
    }
}

impl DisplayWith for EntityKind {
    type Context<'a> = (&'a Crate, &'a BuildSession);

    fn format(&self, context: Self::Context<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UntypedFunction => write!(f, "untyped value"),
            Module { namespace } => write!(f, "module of {:?}", namespace),
            UntypedDataType { namespace } => write!(f, "untyped data type of {:?}", namespace),
            UntypedConstructor => write!(f, "untyped constructor"),
            Use { reference } => write!(f, "use {:?}", reference),
            UnresolvedUse => write!(f, "unresolved use"),
            Function { type_, expression } => match expression {
                Some(expression) => {
                    write!(f, "{}: {}", expression.with(context), type_.with(context))
                }
                None => write!(f, ": {}", type_.with(context)),
            },
            DataType {
                type_,
                constructors,
                ..
            } => write!(
                f,
                "data: {} = {}",
                type_.with(context),
                constructors
                    .iter()
                    .map(|constructor| format!("{} ", constructor))
                    .collect::<String>()
            ),
            Constructor { type_, .. } => write!(f, "constructor: {}", type_.with(context)),
            Intrinsic { type_, .. } => write!(f, "intrinsic: {}", type_.with(context)),
            Error => write!(f, "error"),
        }
    }
}
