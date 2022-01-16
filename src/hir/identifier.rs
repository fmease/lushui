// @Task docs

use super::Capsule;
use crate::{
    package::CapsuleIndex,
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
    pub(crate) fn new(index: impl Into<Index>, source: ast::Identifier) -> Self {
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

    pub(crate) fn as_str(&self) -> &str {
        self.source.as_str()
    }

    pub(crate) fn into_expression(self) -> super::Expression {
        super::Expression::new(default(), self.span(), super::Binding(self).into())
    }

    // @Note bad name
    pub(crate) fn as_innermost(&self) -> Self {
        Self::new(DeBruijnIndex(0), self.source.clone())
    }

    pub(crate) fn is_innermost(&self) -> bool {
        self.index == DeBruijnIndex(0).into()
    }

    pub(crate) fn shift(self, amount: usize) -> Self {
        Self {
            index: self.index.shift(amount),
            ..self
        }
    }

    pub(crate) fn unshift(self) -> Self {
        Self {
            index: self.index.unshift(),
            ..self
        }
    }

    pub(crate) fn declaration_index(&self) -> Option<DeclarationIndex> {
        self.index.declaration_index()
    }

    pub(crate) fn local_declaration_index(
        &self,
        capsule: &Capsule,
    ) -> Option<LocalDeclarationIndex> {
        self.declaration_index()?.local(capsule)
    }

    #[allow(dead_code)]
    pub(crate) fn de_bruijn_index(&self) -> Option<DeBruijnIndex> {
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
        // @Task somehow thead this through w/o too much verbosity!
        // if crate::OPTIONS
        //     .get()
        //     .map_or(false, |options| options.show_indices)
        // {
        //     // @Note does not work well with punctuation..
        //     write!(f, "#{:?}", self.index)?;
        // }
        Ok(())
    }
}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Clone, Copy, PartialEq)]
#[allow(clippy::enum_variant_names)] // @Temporary false positive, see rust-clippy #8090, #8127
pub(crate) enum Index {
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

    pub(crate) fn declaration_index(self) -> Option<DeclarationIndex> {
        match self {
            Self::Declaration(index) => Some(index),
            _ => None,
        }
    }

    pub(crate) fn de_bruijn_index(self) -> Option<DeBruijnIndex> {
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

/// Capsule-global index identifying bindings defined by declarations.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct DeclarationIndex(u64);

impl DeclarationIndex {
    pub(crate) fn new(capsule_index: CapsuleIndex, local_index: LocalDeclarationIndex) -> Self {
        let shifted_capsule_index = u64::from(capsule_index.0) << LocalDeclarationIndex::BIT_WIDTH;

        Self(shifted_capsule_index | local_index.0)
    }

    pub(crate) fn capsule(self) -> CapsuleIndex {
        #[allow(clippy::cast_possible_truncation)]
        CapsuleIndex((self.0 >> LocalDeclarationIndex::BIT_WIDTH) as _)
    }

    pub(crate) fn local_unchecked(self) -> LocalDeclarationIndex {
        LocalDeclarationIndex(self.0 & LocalDeclarationIndex::MAX)
    }

    pub(crate) fn is_local(self, capsule: &Capsule) -> bool {
        self.capsule() == capsule.index()
    }

    pub(crate) fn local(self, capsule: &Capsule) -> Option<LocalDeclarationIndex> {
        self.is_local(capsule).then(|| self.local_unchecked())
    }
}

impl fmt::Debug for DeclarationIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}{:?}", self.capsule(), self.local_unchecked(),)
    }
}

impl From<DeclarationIndex> for Index {
    fn from(index: DeclarationIndex) -> Self {
        Self::Declaration(index)
    }
}

/// Capsule-local index identifying bindings defined by declarations.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) struct LocalDeclarationIndex(u64);

impl LocalDeclarationIndex {
    const BIT_WIDTH: u32 = 48;
    const MAX: u64 = 2_u64.pow(Self::BIT_WIDTH) - 1;

    pub(crate) fn new(index: u64) -> Self {
        assert!(index < Self::MAX);

        Self(index)
    }

    pub(crate) fn global(self, capsule: &Capsule) -> DeclarationIndex {
        DeclarationIndex::new(capsule.index(), self)
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
pub(crate) struct DeBruijnIndex(pub(crate) usize);

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
