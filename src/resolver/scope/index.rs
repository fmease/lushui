// @Task docs

use std::{convert::TryInto, fmt};

use crate::package::CrateIndex;

#[derive(Clone, Copy, PartialEq)]
pub enum Index {
    Declaration(DeclarationIndex),
    DeBruijn(DeBruijnIndex),
    DeBruijnParameter,
}

impl Index {
    pub(super) fn shift(self, amount: usize) -> Self {
        match self {
            Self::DeBruijn(index) => DeBruijnIndex(index.0 + amount).into(),
            Self::Declaration(_) | Self::DeBruijnParameter => self,
        }
    }

    pub(super) fn unshift(self) -> Self {
        match self {
            Self::DeBruijn(index) => DeBruijnIndex(index.0.saturating_sub(1)).into(),
            Self::Declaration(_) | Self::DeBruijnParameter => self,
        }
    }

    pub fn declaration_index(self) -> Option<DeclarationIndex> {
        match self {
            Self::Declaration(index) => Some(index),
            _ => None,
        }
    }

    pub fn de_bruijn_index(self) -> Option<DeBruijnIndex> {
        match self {
            Self::DeBruijn(index) => Some(index),
            _ => None,
        }
    }
}

impl fmt::Debug for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Declaration(index) => write!(f, "{:?}", index),
            Self::DeBruijn(index) => write!(f, "{:?}", index),
            Self::DeBruijnParameter => write!(f, "P"),
        }
    }
}

/// Crate-global index identifying bindings defined by declarations.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DeclarationIndex(u64);

impl DeclarationIndex {
    pub fn new(crate_index: CrateIndex, local_index: LocalDeclarationIndex) -> Self {
        let shifted_crate_index = u64::from(crate_index.0) << LocalDeclarationIndex::BIT_WIDTH;

        Self(shifted_crate_index | local_index.0)
    }

    pub fn crate_index(self) -> CrateIndex {
        #[allow(clippy::cast_possible_truncation)]
        CrateIndex((self.0 >> LocalDeclarationIndex::BIT_WIDTH) as _)
    }

    pub fn local_index(self) -> LocalDeclarationIndex {
        LocalDeclarationIndex(self.0 & LocalDeclarationIndex::MAX)
    }
}

impl fmt::Debug for DeclarationIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}{:?}", self.crate_index(), self.local_index())
    }
}

impl From<DeclarationIndex> for Index {
    fn from(index: DeclarationIndex) -> Self {
        Self::Declaration(index)
    }
}

/// Crate-local index identifying bindings defined by declarations.
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
