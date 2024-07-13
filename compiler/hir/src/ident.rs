use super::{Binding, Item};
use span::{Span, Spanning};
use std::fmt;
use utility::{obtain, Atom, CompIdx};

/// A name-resolved identifier.
#[derive(Clone, Copy, Eq)]
pub struct Ident {
    /// Source at the use-site/call-site or def-site if definition.
    pub src: ast::Ident,
    pub idx: Index,
}

impl Ident {
    pub fn new(idx: impl Into<Index>, src: ast::Ident) -> Self {
        Self {
            idx: idx.into(),
            src,
        }
    }

    pub fn to_str(self) -> &'static str {
        self.src.to_str()
    }

    pub fn bare(self) -> Atom {
        self.src.bare()
    }

    pub fn to_item<T: From<Binding>>(self) -> Item<T> {
        Item::common(self.span(), Binding(self).into())
    }

    // @Note bad name
    pub fn to_innermost(self) -> Self {
        Self::new(DeBruijnIdx(0), self.src)
    }

    pub fn is_innermost(self) -> bool {
        self.idx == DeBruijnIdx(0).into()
    }

    pub fn shift(self, amount: usize) -> Self {
        Self {
            idx: self.idx.shift(amount),
            ..self
        }
    }

    pub fn unshift(self) -> Self {
        Self {
            idx: self.idx.unshift(),
            ..self
        }
    }

    pub fn decl_idx(self) -> Option<DeclIdx> {
        self.idx.decl_idx()
    }

    pub fn de_bruijn_idx(self) -> Option<DeBruijnIdx> {
        self.idx.de_bruijn()
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}

impl Spanning for Ident {
    fn span(&self) -> Span {
        self.src.span()
    }
}

impl span::binder::Binder for Ident {
    fn to_str(self) -> &'static str {
        self.to_str()
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.src)
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Clone, Copy, Eq)]
pub enum Index {
    Decl(DeclIdx),
    DeBruijn(DeBruijnIdx),
    Param,
}

impl Index {
    pub fn shift(self, amount: usize) -> Self {
        match self {
            Self::DeBruijn(index) => DeBruijnIdx(index.0 + amount).into(),
            index => index,
        }
    }

    pub fn unshift(self) -> Self {
        match self {
            Self::DeBruijn(index) => DeBruijnIdx(index.0.saturating_sub(1)).into(),
            index => index,
        }
    }

    pub fn decl_idx(self) -> Option<DeclIdx> {
        obtain!(self, Self::Decl(index) => index)
    }

    pub fn de_bruijn(self) -> Option<DeBruijnIdx> {
        obtain!(self, Self::DeBruijn(index) => index)
    }
}

impl PartialEq for Index {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Decl(index0), Self::Decl(index1)) => index0 == index1,
            (Self::DeBruijn(index0), Self::DeBruijn(index1)) => index0 == index1,
            _ => false,
        }
    }
}

impl fmt::Debug for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Decl(index) => write!(f, "{index:?}"),
            Self::DeBruijn(index) => write!(f, "{index:?}"),
            Self::Param => write!(f, "P"),
        }
    }
}

/// Component-global index identifying bindings defined by declarations.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DeclIdx(u64);

impl DeclIdx {
    pub fn new(comp_idx: CompIdx, local_decl_idx: LocalDeclIdx) -> Self {
        Self((u64::from(comp_idx.0) << LocalDeclIdx::BIT_WIDTH) | local_decl_idx.0)
    }

    pub fn comp(self) -> CompIdx {
        #[allow(clippy::cast_possible_truncation)]
        CompIdx((self.0 >> LocalDeclIdx::BIT_WIDTH) as _)
    }

    pub fn local_unchecked(self) -> LocalDeclIdx {
        LocalDeclIdx(self.0 & LocalDeclIdx::MAX)
    }
}

impl fmt::Debug for DeclIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}{:?}", self.comp(), self.local_unchecked())
    }
}

impl From<DeclIdx> for Index {
    fn from(index: DeclIdx) -> Self {
        Self::Decl(index)
    }
}

/// Component-local index identifying bindings defined by declarations.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct LocalDeclIdx(u64);

impl LocalDeclIdx {
    const BIT_WIDTH: u32 = 48;
    const MAX: u64 = 2_u64.pow(Self::BIT_WIDTH) - 1;

    pub fn new(index: u64) -> Self {
        assert!(index < Self::MAX);

        Self(index)
    }
}

impl fmt::Debug for LocalDeclIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}D", self.0)
    }
}

impl index_map::Index for LocalDeclIdx {
    fn new(index: usize) -> Self {
        Self::new(index as u64)
    }

    fn value(self) -> usize {
        self.0.try_into().unwrap()
    }
}

/// De Bruijn index — index for bindings defined by function parameters.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DeBruijnIdx(pub usize);

impl fmt::Debug for DeBruijnIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}B", self.0)
    }
}

impl From<DeBruijnIdx> for Index {
    fn from(index: DeBruijnIdx) -> Self {
        Self::DeBruijn(index)
    }
}
