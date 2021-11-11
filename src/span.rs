//! Data structures and procedures for handling source locations.

// @Task handle overflows showing errors like "file too big" to the user

use generic::Locality::*;
pub use source_map::{SharedSourceMap, SourceFile, SourceFileIndex, SourceMap};
pub use spanned::Spanned;
pub use spanning::{PossiblySpanning, Spanning};
use std::ops::{Add, Range, Sub};

mod source_map;

mod generic {
    use std::{
        fmt,
        num::TryFromIntError,
        ops::{Add, Sub},
    };

    /// An locality-abstract byte index.
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Default)]
    pub struct ByteIndex<const L: Locality>(pub(super) u32);

    impl<const L: Locality> ByteIndex<L> {
        pub const fn new(index: u32) -> Self {
            Self(index)
        }

        pub fn saturating_sub(self, offset: u32) -> Self {
            Self::new(self.0.saturating_sub(offset))
        }
    }

    impl<const L: Locality> Add<u32> for ByteIndex<L> {
        type Output = Self;

        fn add(self, offset: u32) -> Self::Output {
            Self(self.0 + offset)
        }
    }

    impl<const L: Locality> Sub for ByteIndex<L> {
        type Output = Self;

        fn sub(self, other: Self) -> Self::Output {
            Self(self.0 - other.0)
        }
    }

    impl<const L: Locality> Sub<u32> for ByteIndex<L> {
        type Output = Self;

        fn sub(self, offset: u32) -> Self::Output {
            Self(self.0 - offset)
        }
    }

    impl<const L: Locality> TryFrom<usize> for ByteIndex<L> {
        type Error = TryFromIntError;

        fn try_from(index: usize) -> Result<Self, Self::Error> {
            Ok(Self(index.try_into()?))
        }
    }

    // @Task if feasible, make the fields private
    #[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
    pub struct Span<const L: Locality> {
        /// The start of the span, inclusive.
        pub(super) start: ByteIndex<L>,
        /// The end of the span, exclusive.
        pub(super) end: ByteIndex<L>,
    }

    impl<const L: Locality> Span<L> {
        #[cfg_attr(debug_assertions, track_caller)]
        pub fn new(start: ByteIndex<L>, end: ByteIndex<L>) -> Self {
            debug_assert!(
                start <= end,
                "span start ({}) > span end ({})",
                start.0,
                end.0
            );

            Self { start, end }
        }

        /// Create an empty span at the given index.
        pub fn empty(index: ByteIndex<L>) -> Self {
            Self::new(index, index)
        }

        pub fn with_length(start: ByteIndex<L>, length: u32) -> Self {
            Self::new(start, ByteIndex(start.0 + length))
        }

        pub fn length(self) -> u32 {
            self.end.0 - self.start.0
        }

        pub fn is_empty(self) -> bool {
            self.start == self.end
        }

        pub fn contains(self, index: ByteIndex<L>) -> bool {
            self.start <= index && index <= self.end
        }

        pub fn start(self) -> Self {
            Self::empty(self.start)
        }

        pub fn end(self) -> Self {
            Self::empty(self.end)
        }

        pub fn set_end(&mut self, index: ByteIndex<L>) {
            self.end = index;
        }

        #[must_use]
        pub fn trim(self, amount: u32) -> Self {
            self.trim_start(amount).trim_end(amount)
        }

        #[must_use]
        pub fn trim_start(self, amount: u32) -> Self {
            Self::new(self.start + amount, self.end)
        }

        #[must_use]
        pub fn trim_end(self, amount: u32) -> Self {
            Self::new(self.start, self.end - amount)
        }
    }

    impl<const L: Locality> fmt::Debug for Span<L> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}..{}", self.start.0, self.end.0)
        }
    }

    #[derive(PartialEq, Eq)]
    pub enum Locality {
        Local,
        Global,
    }
}

/// A global byte index.
///
/// Here, "global" means local relative to a [source map](SourceMap).
pub type ByteIndex = generic::ByteIndex<{ Global }>;

/// A file-local byte index.
pub type LocalByteIndex = generic::ByteIndex<{ Local }>;

// @Task replace
impl From<LocalByteIndex> for usize {
    fn from(index: LocalByteIndex) -> Self {
        index.0 as _ // @Bug may truncate (u16)
    }
}

impl Add<char> for LocalByteIndex {
    type Output = Self;

    fn add(self, character: char) -> Self::Output {
        #![allow(clippy::cast_possible_truncation)] // always within 1..=4
        self + character.len_utf8() as u32
    }
}

// @Task replace
impl Sub<usize> for LocalByteIndex {
    type Output = Self;

    fn sub(self, offset: usize) -> Self::Output {
        self - LocalByteIndex::try_from(offset).unwrap()
    }
}

impl ByteIndex {
    /// Map a global byte index to a local one.
    pub fn local(self, file: &SourceFile) -> LocalByteIndex {
        LocalByteIndex::new(self.0 - file.span().start.0)
    }
}

impl LocalByteIndex {
    /// Map a local byte index to a global one.
    pub fn global(self, file: &SourceFile) -> ByteIndex {
        ByteIndex::new(file.span().start.0 + self.0)
    }
}

/// A global byte span of source code.
///
/// _Global_ means relative to a [`SourceMap`].
pub type Span = generic::Span<{ Global }>;

/// A span inside a single source file.
pub type LocalSpan = generic::Span<{ Local }>;

impl Span {
    pub fn local(self, file: &SourceFile) -> LocalSpan {
        LocalSpan::new(self.start.local(file), self.end.local(file))
    }

    // @Task find better names for those merging functions and/or add
    // documentation

    #[must_use]
    pub fn merge(self, other: impl PossiblySpanning) -> Self {
        match other.possible_span() {
            Some(other) => {
                // self.assert_disjoint_and_consecutive(other);
                Self::new(self.start, other.end)
            }
            None => self,
        }
    }

    #[must_use]
    pub fn merge_into(self, other: impl PossiblySpanning) -> Self {
        if let Some(other) = other.possible_span() {
            // other.assert_disjoint_and_consecutive(self);
            Self::new(other.start, self.end)
        } else {
            self
        }
    }

    pub fn merging<S: PossiblySpanning>(&mut self, other: S) -> S {
        if let Some(other) = other.possible_span() {
            // self.assert_disjoint_and_consecutive(other);
            self.end = other.end;
        }
        other
    }

    // @Note naming is not great
    pub fn merging_from(&mut self, other: impl PossiblySpanning) {
        if let Some(other) = other.possible_span() {
            // self.assert_disjoint_and_consecutive(other);
            debug_assert!(other.start <= self.start && other.end <= self.end);
            self.start = other.start;
        }
    }

    /// Similar to [`Self::merge`] except that the spans do not need to be disjoint.
    #[must_use]
    pub fn fit_end(self, other: impl PossiblySpanning) -> Self {
        match other.possible_span() {
            Some(other) => {
                // debug_assert!(self.start <= other.start && self.start <= other.end);

                Span::new(self.start, other.end)
            }
            None => self,
        }
    }
}

impl Spanning for Span {
    fn span(&self) -> Self {
        *self
    }
}

impl LocalSpan {
    pub fn global(self, file: &SourceFile) -> Span {
        Span::new(self.start.global(file), self.end.global(file))
    }
}

impl From<LocalSpan> for Range<usize> {
    fn from(span: LocalSpan) -> Self {
        span.start.into()..span.end.into()
    }
}

impl Sub<LocalByteIndex> for LocalSpan {
    type Output = Self;

    fn sub(self, offset: LocalByteIndex) -> Self::Output {
        Self::new(self.start - offset, self.end - offset)
    }
}

/// Convenience function for for constructing a global span for test code.
pub fn span(start: u32, end: u32) -> Span {
    Span::new(ByteIndex::new(start), ByteIndex::new(end))
}

mod spanning {
    use super::Span;
    use crate::utility::SmallVec;

    pub trait Spanning: PossiblySpanning {
        fn span(&self) -> Span;
    }

    impl<S: Spanning> Spanning for &'_ S {
        fn span(&self) -> Span {
            (**self).span()
        }
    }

    impl<S: Spanning, Z: Spanning> Spanning for (S, Z) {
        fn span(&self) -> Span {
            self.0.span().merge(self.1.span())
        }
    }

    pub trait PossiblySpanning {
        fn possible_span(&self) -> Option<Span>;
    }

    impl<S: Spanning> PossiblySpanning for S {
        fn possible_span(&self) -> Option<Span> {
            Some(self.span())
        }
    }

    // @Task generalize these two impls using DoubleEndedIterator

    impl<S: Spanning> PossiblySpanning for Vec<S> {
        fn possible_span(&self) -> Option<Span> {
            self.first().map(|item| {
                let mut span = item.span();
                span.merging(self.last());
                span
            })
        }
    }

    impl<S: Spanning, const N: usize> PossiblySpanning for SmallVec<S, N> {
        fn possible_span(&self) -> Option<Span> {
            self.first().map(|item| {
                let mut span = item.span();
                span.merging(self.last());
                span
            })
        }
    }

    impl<S> PossiblySpanning for Option<S>
    where
        S: Spanning,
    {
        fn possible_span(&self) -> Option<Span> {
            self.as_ref().map(<_>::span)
        }
    }

    // @Task smh (specialization?) abstract over those two impls
    // with impl<S: PossiblySpanning> PossiblySpanning for &'_ S
    // this currently (obviously) conflicts with impl<S: Spanning> Spanning for &'_ S

    // impl<S: PossiblySpanning> PossiblySpanning for &'_ S {
    //     fn possible_span(&self) -> Option<Span> {
    //         (**self).possible_span()
    //     }
    // }

    impl<S: Spanning> PossiblySpanning for &'_ Option<S> {
        fn possible_span(&self) -> Option<Span> {
            (**self).possible_span()
        }
    }

    impl<S: Spanning> PossiblySpanning for &'_ Vec<S> {
        fn possible_span(&self) -> Option<Span> {
            (**self).possible_span()
        }
    }
}

mod spanned {
    use super::{Span, Spanning};
    use std::{fmt, hash::Hash};

    #[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub struct Spanned<T> {
        pub value: T,
        pub span: Span,
    }

    impl<T> Spanned<T> {
        pub const fn new(span: Span, value: T) -> Self {
            Self { value, span }
        }

        pub fn map<U>(self, mapper: impl FnOnce(T) -> U) -> Spanned<U> {
            Spanned::new(self.span, mapper(self.value))
        }

        pub fn map_span(mut self, mapper: impl FnOnce(Span) -> Span) -> Self {
            self.span = mapper(self.span);
            self
        }

        pub const fn as_ref(&self) -> Spanned<&T> {
            Spanned::new(self.span, &self.value)
        }

        pub fn as_deref(&self) -> Spanned<&T::Target>
        where
            T: std::ops::Deref,
        {
            Spanned::new(self.span, &self.value)
        }
    }

    // @Temporary
    impl<T: Clone> Spanned<&T> {
        pub fn cloned(&self) -> Spanned<T> {
            self.map(Clone::clone)
        }
    }

    impl<T> Spanning for Spanned<T> {
        fn span(&self) -> Span {
            self.span
        }
    }

    impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?} {:?}", self.value, self.span)
        }
    }

    impl<T: fmt::Display> fmt::Display for Spanned<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.value.fmt(f)
        }
    }
}
