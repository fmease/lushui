use super::{Binding, Item};
use span::{Span, Spanning};
use std::{default::default, fmt};
use utilities::ComponentIndex;

/// A name-resolved identifier.
#[derive(Clone, Eq)]
pub struct Identifier {
    /// Source at the use-site/call-site or def-site if definition.
    pub source: ast::Identifier,
    pub index: Index,
}

impl Identifier {
    pub fn new(index: impl Into<Index>, source: ast::Identifier) -> Self {
        Self {
            index: index.into(),
            source,
        }
    }

    pub fn parameter(name: &str) -> Self {
        Identifier::new(
            Index::DeBruijnParameter,
            ast::Identifier::new_unchecked(name.into(), default()),
        )
    }

    pub fn as_str(&self) -> &str {
        self.source.as_str()
    }

    pub fn into_item<T: From<Binding>>(self) -> Item<T> {
        Item::new(default(), self.span(), Binding(self).into())
    }

    // @Note bad name
    pub fn as_innermost(&self) -> Self {
        Self::new(DeBruijnIndex(0), self.source.clone())
    }

    pub fn is_innermost(&self) -> bool {
        self.index == DeBruijnIndex(0).into()
    }

    pub fn shift(self, amount: usize) -> Self {
        Self {
            index: self.index.shift(amount),
            ..self
        }
    }

    pub fn unshift(self) -> Self {
        Self {
            index: self.index.unshift(),
            ..self
        }
    }

    pub fn declaration_index(&self) -> Option<DeclarationIndex> {
        self.index.declaration()
    }

    pub fn de_bruijn_index(&self) -> Option<DeBruijnIndex> {
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
    DeBruijnParameter,
}

impl Index {
    pub fn shift(self, amount: usize) -> Self {
        match self {
            Self::DeBruijn(index) => DeBruijnIndex(index.0 + amount).into(),
            Self::Declaration(_) | Self::DeBruijnParameter => self,
        }
    }

    pub fn unshift(self) -> Self {
        match self {
            Self::DeBruijn(index) => DeBruijnIndex(index.0.saturating_sub(1)).into(),
            Self::Declaration(_) | Self::DeBruijnParameter => self,
        }
    }

    pub fn declaration(self) -> Option<DeclarationIndex> {
        match self {
            Self::Declaration(index) => Some(index),
            _ => None,
        }
    }

    pub fn de_bruijn(self) -> Option<DeBruijnIndex> {
        match self {
            Self::DeBruijn(index) => Some(index),
            _ => None,
        }
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
            Self::DeBruijnParameter => write!(f, "P"),
        }
    }
}

/// Component-global index identifying bindings defined by declarations.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DeclarationIndex(u64);

impl DeclarationIndex {
    pub fn new(component_index: ComponentIndex, local_index: LocalDeclarationIndex) -> Self {
        Self((u64::from(component_index.0) << LocalDeclarationIndex::BIT_WIDTH) | local_index.0)
    }

    pub fn component(self) -> ComponentIndex {
        #[allow(clippy::cast_possible_truncation)]
        ComponentIndex((self.0 >> LocalDeclarationIndex::BIT_WIDTH) as _)
    }

    pub fn local_unchecked(self) -> LocalDeclarationIndex {
        LocalDeclarationIndex(self.0 & LocalDeclarationIndex::MAX)
    }
}

impl fmt::Debug for DeclarationIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}{:?}", self.component(), self.local_unchecked(),)
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
    fn new(index: usize) -> Self {
        Self::new(index as u64)
    }

    fn value(self) -> usize {
        self.0.try_into().unwrap()
    }
}

/// De Bruijn index — index for bindings defined by function parameters.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DeBruijnIndex(pub usize);

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
