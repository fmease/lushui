//! Data structures and procedures for handling source locations.
#![feature(
    associated_type_bounds,
    adt_const_params,
    decl_macro,
    default_free_fn,
    min_specialization,
    type_changing_struct_update
)]
#![allow(incomplete_features)] // adt_const_params

// @Task handle overflows showing errors like "file too big" to the user

use generic::Locality::*;
pub use source_map::{FileName, SourceFile, SourceFileIndex, SourceMap};
pub use spanned::Spanned;
pub use spanning::{PossiblySpanning, Spanning};
use std::ops::{Add, Range, Sub};
pub use weakly_spanned::WeaklySpanned;

pub mod item;
pub mod source_map;

mod generic {
    use std::{
        cmp::Ordering,
        fmt,
        num::TryFromIntError,
        ops::{Add, AddAssign, Sub},
    };

    /// An locality-abstract byte index.
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Default)]
    pub struct ByteIndex<const L: Locality>(pub(super) u32);

    impl<const L: Locality> ByteIndex<L> {
        /// Relates the index to the given span.
        ///
        /// If the index is to the left of the span (i.e. smaller), it is considered [less].
        /// If it is to the right (i.e. greater), it is considered [greater].
        /// Otherwise, it has to be contained within the span and [equal] is returned.
        ///
        /// [less]: Ordering::Less
        /// [greater]: Ordering::Greater
        /// [equal]: Ordering::Equal
        pub fn relate(self, span: Span<L>) -> Ordering {
            if self < span.start {
                Ordering::Less
            } else if span.end <= self {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        }
    }

    impl<const L: Locality> ByteIndex<L> {
        pub const fn new(index: u32) -> Self {
            Self(index)
        }
    }

    impl<const L: Locality> Add<u32> for ByteIndex<L> {
        type Output = Self;

        fn add(self, offset: u32) -> Self::Output {
            Self(self.0 + offset)
        }
    }

    impl<const L: Locality> AddAssign<u32> for ByteIndex<L> {
        fn add_assign(&mut self, offset: u32) {
            self.0 += offset;
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

        pub fn between(self, other: Span<L>) -> Span<L> {
            Self::new(self.end, other.start)
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
        if let Some(other) = other.possible_span() {
            // self.assert_disjoint_and_consecutive(other);
            Self::new(self.start, other.end)
        } else {
            self
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

    pub fn trim_start_matches(self, predicate: impl Fn(char) -> bool, map: &SourceMap) -> Self {
        let source = map.snippet(self);
        let trimmed = source.trim_start_matches(predicate);
        let difference = source.len() - trimmed.len();

        Span::new(self.start + difference.try_into().unwrap(), self.end)
    }

    pub fn trim_end_matches(self, predicate: impl Fn(char) -> bool, map: &SourceMap) -> Self {
        let source = map.snippet(self);
        let trimmed = source.trim_end_matches(predicate);
        let difference = source.len() - trimmed.len();

        Span::new(
            self.start,
            self.end - ByteIndex::try_from(difference).unwrap(),
        )
    }

    // @Task implement expand_{start,end}_matches to expand outwards
    // we want to use it to expand to leading `{  `s and trailing `  }`s.

    #[allow(unused_variables, clippy::unused_self)]
    pub fn expand_start_matches(self, predicate: impl Fn(char) -> bool, map: &SourceMap) -> Self {
        // @Beacon @Task
        todo!()
    }

    #[allow(unused_variables, clippy::unused_self)]
    pub fn expand_end_matches(self, predicate: impl Fn(char) -> bool, map: &SourceMap) -> Self {
        // @Beacon @Task
        todo!()
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
    use utilities::SmallVec;

    pub trait Spanning: PossiblySpanning {
        fn span(&self) -> Span;
    }

    impl<S: Spanning> Spanning for &S {
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
        S: PossiblySpanning,
    {
        fn possible_span(&self) -> Option<Span> {
            self.as_ref().and_then(<_>::possible_span)
        }
    }

    // @Task smh (specialization?) abstract over those two impls
    // with impl<S: PossiblySpanning> PossiblySpanning for &S
    // this currently (obviously) conflicts with impl<S: Spanning> Spanning for &S

    // impl<S: PossiblySpanning> PossiblySpanning for &S {
    //     fn possible_span(&self) -> Option<Span> {
    //         (**self).possible_span()
    //     }
    // }

    impl<S: PossiblySpanning> PossiblySpanning for &Option<S> {
        fn possible_span(&self) -> Option<Span> {
            (**self).possible_span()
        }
    }

    impl<S: Spanning> PossiblySpanning for &Vec<S> {
        fn possible_span(&self) -> Option<Span> {
            (**self).possible_span()
        }
    }
}

// @Task combine Spanned, WeaklySpanned via parameter <const I: Influence = {Weak, Strong}>

mod spanned {
    use super::{Span, Spanning, WeaklySpanned};
    use std::{default::default, fmt, hash::Hash, ops::Deref};

    #[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub struct Spanned<Bare> {
        pub bare: Bare,
        pub span: Span,
    }

    impl<Bare> Spanned<Bare> {
        pub const fn new(span: Span, bare: Bare) -> Self {
            Self { bare, span }
        }

        pub fn bare(bare: Bare) -> Self {
            Self::new(default(), bare)
        }

        pub fn map<U>(self, mapper: impl FnOnce(Bare) -> U) -> Spanned<U> {
            Spanned::new(self.span, mapper(self.bare))
        }

        #[must_use]
        pub fn map_span(mut self, mapper: impl FnOnce(Span) -> Span) -> Self {
            self.span = mapper(self.span);
            self
        }

        pub const fn as_ref(&self) -> Spanned<&Bare> {
            Spanned::new(self.span, &self.bare)
        }

        pub fn as_mut(&mut self) -> Spanned<&mut Bare> {
            Spanned::new(self.span, &mut self.bare)
        }

        pub fn as_deref(&self) -> Spanned<&Bare::Target>
        where
            Bare: Deref,
        {
            Spanned::new(self.span, &self.bare)
        }

        pub fn weak(self) -> WeaklySpanned<Bare> {
            WeaklySpanned {
                bare: self.bare,
                span: self.span,
            }
        }
    }

    impl<Bare> Spanned<&Bare> {
        pub fn copied(self) -> Spanned<Bare>
        where
            Bare: Copy,
        {
            self.map(|value| *value)
        }

        pub fn cloned(self) -> Spanned<Bare>
        where
            Bare: Clone,
        {
            self.map(Clone::clone)
        }
    }

    impl<Bare> Spanning for Spanned<Bare> {
        fn span(&self) -> Span {
            self.span
        }
    }

    impl<Bare: fmt::Debug> fmt::Debug for Spanned<Bare> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?} {:?}", self.bare, self.span)
        }
    }

    impl<Bare: fmt::Display> fmt::Display for Spanned<Bare> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.bare.fmt(f)
        }
    }

    pub macro Spanned($span:pat, $bare:pat $(,)?) {
        Spanned {
            span: $span,
            bare: $bare,
        }
    }
}

mod weakly_spanned {
    use super::{Span, Spanned, Spanning};
    use std::{
        borrow::Borrow,
        fmt,
        hash::{Hash, Hasher},
        ops::Deref,
    };

    #[derive(Clone, Copy)]
    pub struct WeaklySpanned<Bare> {
        pub bare: Bare,
        pub span: Span,
    }

    impl<Bare> WeaklySpanned<Bare> {
        pub fn new(span: Span, bare: Bare) -> Self {
            Self { bare, span }
        }

        pub fn map_span(self, mapper: impl FnOnce(Span) -> Span) -> Self {
            Self {
                bare: self.bare,
                span: mapper(self.span),
            }
        }

        pub fn as_ref(&self) -> WeaklySpanned<&Bare> {
            WeaklySpanned::new(self.span, &self.bare)
        }

        pub fn as_deref(&self) -> WeaklySpanned<&Bare::Target>
        where
            Bare: Deref,
        {
            WeaklySpanned::new(self.span, &self.bare)
        }

        pub fn strong(self) -> Spanned<Bare> {
            Spanned {
                bare: self.bare,
                span: self.span,
            }
        }
    }

    impl<Bare> Spanning for WeaklySpanned<Bare> {
        fn span(&self) -> Span {
            self.span
        }
    }

    impl<Bare: PartialEq> PartialEq for WeaklySpanned<Bare> {
        fn eq(&self, other: &Self) -> bool {
            self.bare == other.bare
        }
    }

    impl<Bare: Eq> Eq for WeaklySpanned<Bare> {}

    impl<Bare: PartialOrd> PartialOrd for WeaklySpanned<Bare> {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            self.bare.partial_cmp(&other.bare)
        }
    }

    impl<Bare: Ord> Ord for WeaklySpanned<Bare> {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.bare.cmp(&other.bare)
        }
    }

    impl<Bare: Hash> Hash for WeaklySpanned<Bare> {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.bare.hash(state);
        }
    }

    impl<Bare> Borrow<Bare> for WeaklySpanned<Bare> {
        default fn borrow(&self) -> &Bare {
            &self.bare
        }
    }

    impl Borrow<str> for WeaklySpanned<String> {
        fn borrow(&self) -> &str {
            &self.bare
        }
    }

    impl<Bare: fmt::Debug> fmt::Debug for WeaklySpanned<Bare> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?} {:?}", self.bare, self.span)
        }
    }

    impl<Bare: fmt::Display> fmt::Display for WeaklySpanned<Bare> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.bare.fmt(f)
        }
    }
}
