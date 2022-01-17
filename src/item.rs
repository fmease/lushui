//! Abstractions for working with things that have a source location and can have attributes.

use std::default::default;

use crate::{
    error::PossiblyErroneous,
    span::{Span, Spanning},
};

/// Something with a source location and attributes.
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Item<T, Attributes> {
    pub value: T,
    pub span: Span,
    pub attributes: Attributes,
}

impl<T, Attributes> Item<T, Attributes> {
    pub(crate) const fn new(attributes: Attributes, span: Span, value: T) -> Self {
        Self {
            value,
            span,
            attributes,
        }
    }
}

impl<T, Attribute> Spanning for Item<T, Attribute> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<T: PossiblyErroneous, Attributes: Default> PossiblyErroneous for Item<T, Attributes> {
    fn error() -> Self {
        Self {
            value: T::error(),
            span: default(),
            attributes: default(),
        }
    }
}
