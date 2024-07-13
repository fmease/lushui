use crate::{Span, Spanning};
use utility::default;

/// Something with a source location and attributes.
#[derive(Clone, PartialEq, Eq)]
pub struct Item<Bare, Attrs> {
    pub bare: Bare,
    pub span: Span,
    pub attrs: Attrs,
}

impl<Bare, Attrs> Item<Bare, Attrs> {
    pub const fn new(attrs: Attrs, span: Span, bare: Bare) -> Self {
        Self { bare, span, attrs }
    }

    pub fn common(span: Span, bare: Bare) -> Self
    where
        Attrs: Default,
    {
        Self::new(default(), span, bare)
    }

    pub fn bare(bare: Bare) -> Self
    where
        Attrs: Default,
    {
        Self::new(default(), default(), bare)
    }

    pub fn map<U>(self, mapper: impl FnOnce(Bare) -> U) -> Item<U, Attrs> {
        Item {
            bare: mapper(self.bare),
            ..self
        }
    }

    pub fn remap<U>(self, bare: U) -> Item<U, Attrs> {
        Item { bare, ..self }
    }
}

impl<Bare, Attr> Spanning for Item<Bare, Attr> {
    fn span(&self) -> Span {
        self.span
    }
}
