use crate::{special, Attrs, DeclIdx, Exposure, Expr, Ident, LocalDeclIdx, Namespace, ValueView};
use diagnostics::{error::PossiblyErroneous, reporter::ErasedReportedError};
use utility::default;
use utility::obtain;

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
    pub src: ast::Ident,
    /// The namespace this entity is a member of.
    // @Question should we make this a DeclIdx?
    pub parent: Option<LocalDeclIdx>,
    pub exp: Exposure,
    pub attrs: Attrs,
    pub kind: EntityKind,
}

impl Entity {
    pub const fn is_untyped(&self) -> bool {
        matches!(
            self.kind,
            FuncUntyped | DataTyUntyped { .. } | CtorUntyped { .. }
        )
    }

    /// Test if the entity is a (typed or untyped) function.
    pub const fn is_func(&self) -> bool {
        matches!(self.kind, FuncUntyped | Func { .. })
    }

    /// Test if the entity is a (typed or untyped) data type.
    pub const fn is_data_ty(&self) -> bool {
        matches!(self.kind, DataTyUntyped { .. } | DataTy { .. })
    }

    /// Test if the entity is a (typed or untyped) constructor.
    pub const fn is_ctor(&self) -> bool {
        matches!(self.kind, CtorUntyped { .. } | Ctor { .. })
    }

    pub const fn is_module(&self) -> bool {
        matches!(self.kind, Module { .. })
    }

    pub const fn is_intr_func(&self) -> bool {
        matches!(self.kind, FuncIntr { .. })
    }

    pub const fn is_error(&self) -> bool {
        matches!(self.kind, Error(_))
    }

    pub const fn is_namespace(&self) -> bool {
        self.is_module() || self.is_data_ty()
    }

    pub const fn namespace(&self) -> Option<&Namespace> {
        obtain!(
            &self.kind,
            Module { namespace }
            | DataTyUntyped { namespace }
            | DataTy { namespace, .. } => namespace,
        )
    }

    pub fn namespace_mut(&mut self) -> Option<&mut Namespace> {
        obtain!(
            &mut self.kind,
            Module { namespace }
            | DataTyUntyped { namespace }
            | DataTy { namespace, .. } => namespace,
        )
    }

    pub const fn is_bodiless_fn(&self) -> bool {
        matches!(
            self.kind,
            Func {
                expression: None,
                ..
            }
        )
    }

    pub fn ty(&self) -> Option<Expr> {
        obtain!(
            &self.kind,
            Func { ty, .. } |
            DataTy { ty, .. } |
            Ctor { ty, .. } |
            FuncIntr { ty, .. } => ty.clone(),
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
            Func {
                expression: Some(expression),
                ..
            } => ValueView::Reducible(expression.clone()),
            Func {
                expression: None, ..
            }
            | Error(_)
            | DataTy { .. }
            | Ctor { .. }
            | FuncIntr { .. } => ValueView::Neutral,
            FuncUntyped
            | DataTyUntyped { .. }
            | CtorUntyped { .. }
            | Module { .. }
            | Use { .. }
            | UseUnres => unreachable!(),
        }
    }
}

use EntityKind::*;

#[derive(Clone)]
pub enum EntityKind {
    Func {
        ty: Expr,
        expression: Option<Expr>,
    },
    FuncUntyped,
    FuncIntr {
        func: special::Func,
        ty: Expr,
    },

    DataTy {
        namespace: Namespace,
        ty: Expr,
        // @Question should we store them at all?
        ctors: Vec<Ident>,
    },
    /// Data types are not [`Self::FuncUntyped`] as they are also namespaces containing constructors.
    DataTyUntyped {
        namespace: Namespace,
    },

    Module {
        namespace: Namespace,
    },

    Ctor {
        ty: Expr,
    },
    CtorUntyped,

    /// The `target` is never a `Use` itself.
    ///
    /// Nested aliases were already collapsed by `Resolver::collapse_use_chain`.
    Use {
        target: DeclIdx,
    },
    UseUnres,

    Error(ErasedReportedError),
}

impl EntityKind {
    pub fn module() -> Self {
        Module {
            namespace: default(),
        }
    }

    pub fn untyped_data_ty() -> Self {
        DataTyUntyped {
            namespace: default(),
        }
    }

    /// The user-facing name of the entity kind.
    pub const fn name(&self) -> &'static str {
        match self {
            FuncUntyped | Func { .. } | FuncIntr { .. } => "function",
            Module { .. } => "module",
            DataTyUntyped { .. } | DataTy { .. } => "data type",
            CtorUntyped | Ctor { .. } => "constructor",
            Use { .. } | UseUnres => "use-binding",
            Error(_) => "error",
        }
    }

    // /// The developer-facing name of the entity kind.
    // // @Task derive this with `#[derive(DiscriminantStr)] #[format(space_case)]` (sth like that)
    pub const fn precise_name(&self) -> &'static str {
        match self {
            FuncUntyped => "func untyped",
            Module { .. } => "module",
            DataTyUntyped { .. } => "data ty untyped",
            CtorUntyped => "ctor untyped",
            Use { .. } => "use",
            UseUnres => "use unres",
            Func { .. } => "func",
            DataTy { .. } => "data ty",
            Ctor { .. } => "ctor",
            FuncIntr { .. } => "func intr",
            Error(_) => "error",
        }
    }
}

impl PossiblyErroneous for EntityKind {
    fn error(error: ErasedReportedError) -> Self {
        Self::Error(error)
    }
}
