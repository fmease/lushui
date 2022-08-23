//! Abstractions for working with things that have a source location and can have attributes.
#![feature(default_free_fn, type_changing_struct_update)]

use error::PossiblyErroneous;
use span::{Span, Spanning};
use std::default::default;

/// Something with a source location and attributes.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Item<Bare, Attributes> {
    pub bare: Bare,
    pub span: Span,
    pub attributes: Attributes,
}

impl<Bare, Attributes> Item<Bare, Attributes> {
    pub const fn new(attributes: Attributes, span: Span, bare: Bare) -> Self {
        Self {
            bare,
            span,
            attributes,
        }
    }

    pub fn map<U>(self, mapper: impl FnOnce(Bare) -> U) -> Item<U, Attributes> {
        Item {
            bare: mapper(self.bare),
            ..self
        }
    }
}

impl<Bare, Attribute> Spanning for Item<Bare, Attribute> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<Bare: PossiblyErroneous, Attributes: Default> PossiblyErroneous for Item<Bare, Attributes> {
    fn error() -> Self {
        Self {
            bare: Bare::error(),
            span: default(),
            attributes: default(),
        }
    }
}
