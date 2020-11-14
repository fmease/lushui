use crate::{
    span::{Span, Spanning},
    support::InvalidFallback,
};

/// Something with a source location and attributes.
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Item<Kind, Attributes> {
    pub kind: Kind,
    pub span: Span,
    pub attributes: Attributes,
}

impl<Kind, Attributes> Item<Kind, Attributes> {
    pub const fn new(attributes: Attributes, span: Span, kind: Kind) -> Self {
        Self {
            kind,
            span,
            attributes,
        }
    }
}

impl<Kind, Attribute> Spanning for Item<Kind, Attribute> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<Kind: InvalidFallback, Attributes: Default> InvalidFallback for Item<Kind, Attributes> {
    fn invalid() -> Self {
        Self {
            kind: Kind::invalid(),
            span: Span::SHAM,
            attributes: Attributes::default(),
        }
    }
}

// @Note several hacks going on because apparently, one cannot use a $loc:path directly and concatenate it
// to the rest of another path. $($seg)::+ does not work either
pub macro item {
    ($loc:path, $item_kind:ident, $indirection:ident; $kind:ident { $attrs:expr, $span:expr $(; $( $body:tt )+ )? }) => {{
        #[allow(unused_imports)]
        use $loc as loc;
        Item::new(
            $attrs,
            $span,
            $item_kind::$kind $( ($indirection::new(loc::$kind { $( $body )+ })) )?,
        )
    }},
    ($loc:path, $item_kind:ident, $indirection:ident; $kind:ident($attrs:expr, $span:expr; $value:expr $(,)?)) => {
        Item::new(
            $attrs,
            $span,
            $item_kind::$kind($indirection::from($value)),
        )
    }
}
