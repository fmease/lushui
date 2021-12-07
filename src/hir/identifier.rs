// @Task docs

use super::Crate;
use crate::{
    package::CrateIndex,
    span::{Span, Spanning},
    syntax::ast,
};
use std::{default::default, fmt};

/// A name-resolved identifier.
#[derive(Clone, PartialEq)]
pub struct Identifier {
    /// Source at the use-site/call-site or def-site if definition.
    pub(crate) source: ast::Identifier,
    pub(crate) index: Index,
}

impl Identifier {
    pub fn new(index: impl Into<Index>, source: ast::Identifier) -> Self {
        Self {
            index: index.into(),
            source,
        }
    }

    pub(crate) fn parameter(name: &str) -> Self {
        Identifier::new(
            Index::DeBruijnParameter,
            ast::Identifier::new_unchecked(name.into(), default()),
        )
    }

    pub fn as_str(&self) -> &str {
        self.source.as_str()
    }

    pub fn to_expression(self) -> crate::hir::Expression {
        crate::hir::expr! {
            Binding {
                default(), self.span();
                binder: self
            }
        }
    }

    // @Note bad name
    pub fn as_innermost(&self) -> Self {
        Self::new(DeBruijnIndex(0), self.source.clone())
    }

    pub fn stripped(self) -> Self {
        Self {
            source: self.source.stripped(),
            ..self
        }
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
        self.index.declaration_index()
    }

    pub fn local_declaration_index(&self, crate_: &Crate) -> Option<LocalDeclarationIndex> {
        self.declaration_index()?.local_index(crate_)
    }

    pub fn de_bruijn_index(&self) -> Option<DeBruijnIndex> {
        self.index.de_bruijn_index()
    }
}

impl Spanning for Identifier {
    fn span(&self) -> Span {
        self.source.span()
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.source)?;
        if crate::OPTIONS
            .get()
            .map_or(false, |options| options.show_indices)
        {
            // @Note does not work well with punctuation..
            write!(f, "#{:?}", self.index)?;
        }
        Ok(())
    }
}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

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

    // @Task better name
    pub fn local_index_unchecked(self) -> LocalDeclarationIndex {
        LocalDeclarationIndex(self.0 & LocalDeclarationIndex::MAX)
    }

    pub fn is_local(self, crate_: &Crate) -> bool {
        self.crate_index() == crate_.index
    }

    pub fn local_index(self, crate_: &Crate) -> Option<LocalDeclarationIndex> {
        self.is_local(crate_).then(|| self.local_index_unchecked())
    }
}

impl fmt::Debug for DeclarationIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?}{:?}",
            self.crate_index(),
            self.local_index_unchecked(),
        )
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
