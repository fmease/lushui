use crate::{Ident, Path};
use span::Spanned;
use utility::{Atom, SmallVec};

/// A list of attributes.
pub type Attrs = Vec<Attr>;
/// An attribute.
pub type Attr = Spanned<BareAttr>;

/// A location-less attribute.
#[derive(Clone, PartialEq, Eq)]
pub enum BareAttr {
    /// A regular attribute.
    Reg {
        binder: Ident,
        args: SmallVec<AttrArg, 1>,
    },
    /// A documentation comment.
    Doc,
}

/// An attribute argument.
pub type AttrArg = Spanned<BareAttrArg>;

/// A location-less attribute argument.
#[derive(Clone, PartialEq, Eq)]
pub enum BareAttrArg {
    /// A number literal.
    NumLit(Atom),
    /// A text literal.
    TextLit(Atom),
    Path(Box<Path>),
    Named(Box<NamedAttrArg>),
}

impl BareAttrArg {
    pub const fn name(&self) -> &'static str {
        match self {
            Self::NumLit(_) => "number literal",
            Self::TextLit(_) => "text literal",
            Self::Path(_) => "path",
            Self::Named(_) => "named argument",
        }
    }
}

/// A named attribute argument.
#[derive(Clone, PartialEq, Eq)]
pub struct NamedAttrArg {
    pub binder: Ident,
    pub value: AttrArg,
}
