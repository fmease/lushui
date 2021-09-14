//! Abstractions for working with things that have a source location and can have attributes.

use crate::{
    error::PossiblyErroneous,
    span::{Span, Spanning},
};

/// Something with a source location and attributes.
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Item<T, Attributes> {
    pub data: T,
    pub span: Span,
    pub attributes: Attributes,
}

impl<T, Attributes> Item<T, Attributes> {
    pub const fn new(attributes: Attributes, span: Span, data: T) -> Self {
        Self {
            data,
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
            data: T::error(),
            span: Span::SHAM,
            attributes: Attributes::default(),
        }
    }
}

/// Construct an [Item].
// @Note several hacks going on because apparently, one cannot use a $loc:path directly and concatenate it
// to the rest of another path. $($seg)::+ does not work either
pub macro item {
    ($loc:path, $item:ident, $indirection:ident; $data:ident { $attrs:expr, $span:expr $(; $( $body:tt )+ )? }) => {{
        #[allow(unused_imports)]
        use $loc as loc;
        Item::new(
            $attrs,
            $span,
            $item::$data $( ($indirection::new(loc::$data { $( $body )+ })) )?,
        )
    }},
    ($loc:path, $item:ident, $indirection:ident; $data:ident($attrs:expr, $span:expr; $value:expr $(,)?)) => {
        Item::new(
            $attrs,
            $span,
            $item::$data($indirection::from($value)),
        )
    }
}
