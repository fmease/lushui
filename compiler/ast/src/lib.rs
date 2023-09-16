//! The abstract syntax tree (AST).
//!
//! The most important definitions are [`Declaration`], [`Expression`] and [`Pattern`].
#![feature(let_chains)]

use span::{Span, Spanned};
use utility::SmallVec;

pub use attribute::{
    Attribute, AttributeArgument, Attributes, BareAttribute, BareAttributeArgument,
    NamedAttributeArgument,
};
pub use declaration::*;
pub use expression::*;
pub use identifier::Identifier;
pub use item::*;
pub use path::{BareHanger, Hanger, Path};
pub use pattern::*;
pub use render::Render;

mod attribute;
mod declaration;
mod expression;
mod identifier;
mod item;
mod path;
mod pattern;
mod render;

pub type Parameters = SmallVec<Parameter, 1>;
// @Beacon @Task make this an Item<_> for attribute support on params
pub type Parameter = Spanned<BareParameter>;

#[derive(Clone, PartialEq, Eq)]
pub struct BareParameter {
    pub kind: ParameterKind,
    pub binder: Option<LocalBinder>,
    pub type_: Option<Expression>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum ParameterKind {
    #[default]
    Explicit,
    Implicit,
    Context,
}

impl ParameterKind {
    // @Question shouldn't this live in the parser?
    pub fn from_apostrophe(apostrophe: Option<Span>) -> Self {
        match apostrophe {
            Some(_) => Self::Implicit,
            None => Self::Explicit,
        }
    }

    pub fn adjust_for_child(self) -> ParameterKind {
        match self {
            kind @ Self::Context => kind,
            _ => Self::Implicit,
        }
    }
}

pub type LocalBinder = span::binder::LocalBinder<Identifier>;
