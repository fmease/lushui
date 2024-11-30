use super::{Binding, Item};
use span::{Span, Spanning};
use std::fmt;
use utility::{Atom, ComponentIndex, obtain};

/// A name-resolved identifier.
#[derive(Clone, Copy, Eq)]
pub struct Identifier {
    /// Source at the use-site/call-site or def-site if definition.
    pub source: ast::Identifier,
    pub index: Index,
}

impl Identifier {
    pub fn new(index: impl Into<Index>, source: ast::Identifier) -> Self {
        Self { index: index.into(), source }
    }

    pub fn to_str(self) -> &'static str {
        self.source.to_str()
    }

    pub fn bare(self) -> Atom {
        self.source.bare()
    }

    pub fn to_item<T: From<Binding>>(self) -> Item<T> {
        Item::common(self.span(), Binding(self).into())
    }

    pub fn to_innermost(self) -> Self {
        Self::new(DeBruijnIndex::INNERMOST, self.source)
    }

    pub fn is_innermost(self) -> bool {
        self.index == DeBruijnIndex::INNERMOST.into()
    }

    pub fn shift(self, amount: usize) -> Self {
        Self { index: self.index.shift(amount), ..self }
    }

    pub fn unshift(self) -> Self {
        Self { index: self.index.unshift(), ..self }
    }

    pub fn declaration_index(self) -> Option<DeclarationIndex> {
        self.index.declaration()
    }

    pub fn de_bruijn_index(self) -> Option<DeBruijnIndex> {
        self.index.de_bruijn()
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl Spanning for Identifier {
    fn span(&self) -> Span {
        self.source.span()
    }
}

impl span::binder::Binder for Identifier {
    fn to_str(self) -> &'static str {
        self.to_str()
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.source)
    }
}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Clone, Copy, Eq)]
pub enum Index {
    Declaration(DeclarationIndex),
    DeBruijn(DeBruijnIndex),
    Parameter,
}

impl Index {
    pub fn shift(self, amount: usize) -> Self {
        match self {
            Self::DeBruijn(index) => index.shift(amount).into(),
            index => index,
        }
    }

    pub fn unshift(self) -> Self {
        match self {
            Self::DeBruijn(index) => index.unshift().into(),
            index => index,
        }
    }

    pub fn declaration(self) -> Option<DeclarationIndex> {
        obtain!(self, Self::Declaration(index) => index)
    }

    pub fn de_bruijn(self) -> Option<DeBruijnIndex> {
        obtain!(self, Self::DeBruijn(index) => index)
    }
}

impl PartialEq for Index {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Declaration(index0), Self::Declaration(index1)) => index0 == index1,
            (Self::DeBruijn(index0), Self::DeBruijn(index1)) => index0 == index1,
            _ => false,
        }
    }
}

impl fmt::Debug for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Declaration(index) => write!(f, "{index:?}"),
            Self::DeBruijn(index) => write!(f, "{index:?}"),
            Self::Parameter => write!(f, "P"),
        }
    }
}

/// Component-global index identifying bindings defined by declarations.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DeclarationIndex(u64);

impl DeclarationIndex {
    pub fn new(component_index: ComponentIndex, local_index: LocalDeclarationIndex) -> Self {
        let index = (u64::from(component_index.into_inner()) << LocalDeclarationIndex::BIT_WIDTH)
            | local_index.0;
        Self(index)
    }

    pub fn component(self) -> ComponentIndex {
        #[allow(clippy::cast_possible_truncation)]
        ComponentIndex::new_unchecked((self.0 >> LocalDeclarationIndex::BIT_WIDTH) as _)
    }

    pub fn local_unchecked(self) -> LocalDeclarationIndex {
        LocalDeclarationIndex(self.0 & LocalDeclarationIndex::MAX)
    }
}

impl fmt::Debug for DeclarationIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}{:?}", self.component(), self.local_unchecked())
    }
}

impl From<DeclarationIndex> for Index {
    fn from(index: DeclarationIndex) -> Self {
        Self::Declaration(index)
    }
}

/// Component-local index identifying bindings defined by declarations.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct LocalDeclarationIndex(u64);

impl LocalDeclarationIndex {
    const BIT_WIDTH: u32 = 48;
    const MAX: u64 = 2_u64.pow(Self::BIT_WIDTH) - 1;

    pub fn new(index: u64) -> Self {
        assert!(index < Self::MAX);

        Self(index)
    }
}

impl fmt::Debug for LocalDeclarationIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}D", self.0)
    }
}

impl index_map::Index for LocalDeclarationIndex {
    type Representation = u64;

    fn new(index: Self::Representation, _: index_map::Guard) -> Self {
        Self::new(index)
    }

    fn into_inner(self, _: index_map::Guard) -> Self::Representation {
        self.0
    }
}

/// De Bruijn index — index for bindings defined by function parameters.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DeBruijnIndex(pub usize);

impl DeBruijnIndex {
    pub const INNERMOST: Self = Self(0);

    pub fn shift(self, amount: usize) -> Self {
        Self(self.0 + amount)
    }

    pub fn unshift(self) -> Self {
        // FIXME: Should this really be saturating? Shouldn't we just panic?
        Self(self.0.saturating_sub(1))
    }
}

impl fmt::Debug for DeBruijnIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}B", self.0)
    }
}

impl From<DeBruijnIndex> for Index {
    fn from(index: DeBruijnIndex) -> Self {
        Self::DeBruijn(index)
    }
}
