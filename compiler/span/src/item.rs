use crate::{Span, Spanning};
use utility::default;

/// Something with a source location and attributes.
#[derive(Clone, PartialEq, Eq)]
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

    pub fn common(span: Span, bare: Bare) -> Self
    where
        Attributes: Default,
    {
        Self::new(default(), span, bare)
    }

    pub fn bare(bare: Bare) -> Self
    where
        Attributes: Default,
    {
        Self::new(default(), default(), bare)
    }

    pub fn map<U>(self, mapper: impl FnOnce(Bare) -> U) -> Item<U, Attributes> {
        Item {
            bare: mapper(self.bare),
            ..self
        }
    }

    pub fn remap<U>(self, bare: U) -> Item<U, Attributes> {
        Item { bare, ..self }
    }
}

impl<Bare, Attribute> Spanning for Item<Bare, Attribute> {
    fn span(&self) -> Span {
        self.span
    }
}
