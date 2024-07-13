//! The abstract syntax tree (AST).
//!
//! The most important definitions are [`Decl`], [`Expr`] and [`Pat`].
#![feature(let_chains)]

use span::{Span, Spanned};
use utility::SmallVec;

pub use attr::{Attr, AttrArg, Attrs, BareAttr, BareAttrArg, NamedAttrArg};
pub use decl::*;
pub use expr::*;
pub use ident::Ident;
pub use item::*;
pub use pat::*;
pub use path::{BareHanger, Hanger, Path};
pub use render::Render;

mod attr;
mod decl;
mod expr;
mod ident;
mod item;
mod pat;
mod path;
mod render;

/// A list of parameters.
pub type Params = SmallVec<Param, 1>;
/// A parameter.
// @Beacon @Task make this an Item<_> for attribute support on params
pub type Param = Spanned<BareParam>;

#[derive(Clone, PartialEq, Eq)]
pub struct BareParam {
    pub kind: ParamKind,
    pub binder: Option<LocalBinder>,
    pub ty: Option<Expr>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum ParamKind {
    #[default]
    Explicit,
    Implicit,
    Context,
}

impl ParamKind {
    // @Question shouldn't this live in the parser?
    pub fn from_apostrophe(apo: Option<Span>) -> Self {
        match apo {
            Some(_) => Self::Implicit,
            None => Self::Explicit,
        }
    }

    pub fn adjust_for_child(self) -> ParamKind {
        match self {
            kind @ Self::Context => kind,
            _ => Self::Implicit,
        }
    }
}

pub type LocalBinder = span::binder::LocalBinder<Ident>;
