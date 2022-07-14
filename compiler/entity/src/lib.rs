//! The entity system: Information about bindings for the name resolver _and_ the type checker.
//!
//! Just like [`Component`], [`Entity`] is a resource shared by those two passes.
#![feature(default_free_fn, generic_associated_types, type_alias_impl_trait)]

use colored::Colorize;
use joinery::JoinableIterator;
use lushui_error::PossiblyErroneous;
use lushui_hir::{
    self as hir, DeclarationIndex, Exposure, Expression, Identifier, LocalDeclarationIndex,
    Namespace, ValueView,
};
use lushui_lowered_ast::Attributes;
use lushui_utilities::{formatted, obtain};
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
pub struct Entity {
    /// Source information of the definition site.
    pub source: lushui_ast::Identifier,
    /// The namespace this entity is a member of.
    // @Question should we make this a DeclarationIndex?
    pub parent: Option<LocalDeclarationIndex>,
    pub exposure: Exposure,
    pub attributes: Attributes,
    pub kind: EntityKind,
}

impl Entity {
    pub const fn is_untyped(&self) -> bool {
        matches!(
            self.kind,
            UntypedFunction | UntypedDataType { .. } | UntypedConstructor { .. }
        )
    }

    /// Test if the entity is a (typed or untyped) function.
    pub const fn is_function(&self) -> bool {
        matches!(self.kind, UntypedFunction | Function { .. })
    }

    /// Test if the entity is a (typed or untyped) data type.
    pub const fn is_data_type(&self) -> bool {
        matches!(self.kind, UntypedDataType { .. } | DataType { .. })
    }

    /// Test if the entity is a (typed or untyped) constructor.
    pub const fn is_constructor(&self) -> bool {
        matches!(self.kind, UntypedConstructor { .. } | Constructor { .. })
    }

    pub const fn is_module(&self) -> bool {
        matches!(self.kind, Module { .. })
    }

    pub const fn is_intrinsic_function(&self) -> bool {
        matches!(self.kind, IntrinsicFunction { .. })
    }

    pub const fn is_error(&self) -> bool {
        matches!(self.kind, Error)
    }

    pub const fn is_namespace(&self) -> bool {
        self.is_module() || self.is_data_type()
    }

    pub const fn namespace(&self) -> Option<&Namespace> {
        obtain!(
            &self.kind,
            Module { namespace }
            | UntypedDataType { namespace }
            | DataType { namespace, .. } => namespace,
        )
    }

    pub fn namespace_mut(&mut self) -> Option<&mut Namespace> {
        obtain!(
            &mut self.kind,
            Module { namespace }
            | UntypedDataType { namespace }
            | DataType { namespace, .. } => namespace,
        )
    }

    pub const fn is_bodiless_function(&self) -> bool {
        matches!(
            self.kind,
            Function {
                expression: None,
                ..
            }
        )
    }

    pub fn type_(&self) -> Option<Expression> {
        obtain!(
            &self.kind,
            Function { type_, .. } |
            DataType { type_, .. } |
            Constructor { type_, .. } |
            IntrinsicFunction { type_, .. } => type_.clone(),
        )
    }

    /// The value of an entity.
    ///
    /// # Panics
    ///
    /// Panics if the entity can not be represented as an expression/value like modules
    /// (because they are second-class by specification) or untyped entities which are
    /// not ready yet.
    pub fn value(&self) -> ValueView {
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
            | IntrinsicFunction { .. } => ValueView::Neutral,
            UntypedFunction
            | UntypedDataType { .. }
            | UntypedConstructor { .. }
            | Module { .. }
            | Use { .. }
            | UnresolvedUse => unreachable!(),
        }
    }

    pub fn mark_as_error(&mut self) {
        self.kind = Error;
    }
}

impl<C: hir::DisplayContext> hir::Display<C> for Entity {
    type Output<'a> = impl fmt::Display where C: 'a;

    fn display(&self, context: C) -> Self::Output<'_> {
        formatted(move |f| {
            // @Task improve output for overly long lines (when the identifier is too long or more
            // importantly when the entity kind (esp. expressions within it) are big)

            let parent = self
                .parent
                .map(|parent| format!("{parent:?}."))
                .unwrap_or_default()
                .bright_black();
            let source = self.source.to_string().bright_red().bold();
            let path = format!("{parent}{source}");
            let exposure = format!("{:?}<", self.exposure).bright_black();
            let kind = self.kind.display(context);

            write!(f, "{exposure:>5}   {path:<40} ↦ {kind}")
        })
    }
}

#[derive(Clone)]
pub enum EntityKind {
    UntypedFunction,
    Module {
        namespace: Namespace,
    },
    /// Data types are not [`Self::UntypedFunction`] as they are also namespaces containing constructors.
    UntypedDataType {
        namespace: Namespace,
    },
    UntypedConstructor,
    /// The `target` is never a `Use` itself.
    ///
    /// Nested aliases were already collapsed by `Resolver::collapse_use_chain`.
    Use {
        target: DeclarationIndex,
    },
    UnresolvedUse,

    Function {
        type_: Expression,
        expression: Option<Expression>,
    },
    DataType {
        namespace: Namespace,
        type_: Expression,
        // @Question should we store them at all?
        constructors: Vec<Identifier>,
    },
    Constructor {
        type_: Expression,
    },
    IntrinsicFunction {
        type_: Expression,
        // @Beacon @Beacon @Beacon @Task wrap this in an IntrinsicFnVal
        arity: usize,
        function: hir::intrinsic::BareFunctionValue,
    },
    // @Task explain why we want entities to be possibly erroneous
    Error,
}

impl EntityKind {
    pub fn module() -> Self {
        Module {
            namespace: default(),
        }
    }

    pub fn untyped_data_type() -> Self {
        UntypedDataType {
            namespace: default(),
        }
    }

    /// The user-facing name of the entity kind.
    pub const fn name(&self) -> &'static str {
        match self {
            UntypedFunction | Function { .. } | IntrinsicFunction { .. } => "function",
            Module { .. } => "module",
            UntypedDataType { .. } | DataType { .. } => "data type",
            UntypedConstructor | Constructor { .. } => "constructor",
            Use { .. } | UnresolvedUse => "use-binding",
            // ideally, should not be reachable
            Error => "error",
        }
    }

    /// The developer-facing name of the entity kind.
    // @Task derive this with `#[derive(DiscriminantStr)] #[format(space_case)]` (sth like that)
    const fn precise_name(&self) -> &'static str {
        match self {
            UntypedFunction => "untyped function",
            Module { .. } => "module",
            UntypedDataType { .. } => "untyped data type",
            UntypedConstructor => "untyped constructor",
            Use { .. } => "use",
            UnresolvedUse => "unresolved use",
            Function { .. } => "function",
            DataType { .. } => "data type",
            Constructor { .. } => "constructor",
            IntrinsicFunction { .. } => "intrinsic function",
            Error => "error",
        }
    }
}

impl PossiblyErroneous for EntityKind {
    fn error() -> Self {
        Self::Error
    }
}

impl<C: hir::DisplayContext> hir::Display<C> for EntityKind {
    type Output<'a> = impl fmt::Display where C: 'a;

    fn display(&self, context: C) -> Self::Output<'_> {
        formatted(move |f| {
            write!(f, "{:>19}   ", self.precise_name().bright_blue())?;

            match self {
                Module { namespace } | UntypedDataType { namespace } => write!(f, "{namespace:?}"),
                Use { target } => write!(f, "{target:?}"),
                Function { type_, expression } => {
                    match expression {
                        Some(expression) => write!(f, "{}", expression.display(context)),
                        None => write!(f, "?(none)"),
                    }?;

                    write!(f, " : {}", type_.display(context))
                }
                DataType {
                    type_,
                    constructors,
                    ..
                } => {
                    write!(f, "{}", type_.display(context))?;

                    if !constructors.is_empty() {
                        write!(f, "; {}", constructors.iter().join_with(' '))?;
                    }

                    Ok(())
                }
                Constructor { type_ } | IntrinsicFunction { type_, .. } => {
                    write!(f, "{}", type_.display(context))
                }
                _ => Ok(()),
            }
        })
    }
}
