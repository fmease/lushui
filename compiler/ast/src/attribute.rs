use crate::{Identifier, Path};
use span::Spanned;
use utility::{Atom, SmallVec};

pub type Attributes = Vec<Attribute>;
pub type Attribute = Spanned<BareAttribute>;

#[derive(Clone, PartialEq, Eq)]
pub enum BareAttribute {
    Regular { binder: Identifier, arguments: SmallVec<AttributeArgument, 1> },
    Documentation,
}

pub type AttributeArgument = Spanned<BareAttributeArgument>;

#[derive(Clone, PartialEq, Eq)]
pub enum BareAttributeArgument {
    NumberLiteral(Atom),
    TextLiteral(Atom),
    Path(Box<Path>),
    Named(Box<NamedAttributeArgument>),
}

impl BareAttributeArgument {
    pub const fn name(&self) -> &'static str {
        match self {
            Self::NumberLiteral(_) => "number literal",
            Self::TextLiteral(_) => "text literal",
            Self::Path(_) => "path",
            Self::Named(_) => "named argument",
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct NamedAttributeArgument {
    pub binder: Identifier,
    pub value: AttributeArgument,
}
