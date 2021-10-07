//! Data structures and procedures for handling source locations.

// @Beacon @Task The API of [Span], [LocalSpan], [ByteIndex], [LocalByteIndex] is
// utter trash!! ugly, inconvenient, confusing, unsafe (trying to prevent overflow
// panics but they still gonna happen!)

// @Beacon @Beacon @Beacon @Bug we currently totally gloss over empty spans...
// we always assume a span of length at least 1
// this is because the upper end is inclusive, i.e. x..x is of length one
// but we need to be able to handle empty files:
// SourceFiles with span S..<S where S is the the "span of the source file"
// we have to refactor LocalSpan and Span and everything that depends on it
// (esp. Diagnostic!!!)
// we'd like to have fn is_empty(self) { self.start == self.end } in the end

use crate::format::DisplayWith;
pub use index::{ByteIndex, LocalByteIndex};
pub use source_file::SourceFile;
pub use source_map::{SharedSourceMap, SourceFileIndex, SourceMap};
pub use span::{LocalSpan, Span};
pub use spanned::Spanned;
pub use spanning::{PossiblySpanning, Spanning};
use std::{io, path::Path};

#[derive(PartialEq, Eq)]
pub enum Locality {
    Local,
    Global,
}

mod index {
    use super::{Error, Locality, SourceFile};
    use std::{
        convert::TryInto,
        ops::{Add, Sub},
    };

    /// A global byte index.
    ///
    /// Here, "global" means local relative to a [source map](super::SourceMap).
    pub type ByteIndex = AbstractByteIndex<{ Locality::Global }>;

    /// A file-local byte index.
    pub type LocalByteIndex = AbstractByteIndex<{ Locality::Local }>;

    pub type Representation = u32;

    /// An locality-abstract byte index.
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
    pub struct AbstractByteIndex<const L: Locality>(pub(super) Representation);

    impl<const L: Locality> AbstractByteIndex<L> {
        pub const fn new(index: Representation) -> Self {
            Self(index)
        }

        pub(super) fn map(self, mapper: impl FnOnce(Representation) -> Representation) -> Self {
            Self(mapper(self.0))
        }

        pub(super) fn offset(self, offset: u32) -> Result<Self, Error> {
            let sum = self.0.checked_add(offset).ok_or(Error::OffsetOverflow)?;

            Ok(Self::new(sum))
        }

        pub fn saturating_sub(self, offset: u32) -> Self {
            Self::new(self.0.saturating_sub(offset))
        }

        // @Task get rid of this method
        pub fn from_usize(index: usize) -> Self {
            Self::new(index.try_into().unwrap())
        }
    }

    impl ByteIndex {
        /// Map a local byte index to global global one.
        ///
        /// ## Panics
        ///
        /// Panics on addition overflow.
        pub fn from_local(source: &SourceFile, index: LocalByteIndex) -> Self {
            source.span.start.map(|start| start + index.0)
        }
    }

    impl LocalByteIndex {
        pub fn from_global(source: &SourceFile, index: ByteIndex) -> Self {
            Self::new(index.0 - source.span.start.0)
        }
    }

    impl From<LocalByteIndex> for usize {
        fn from(index: LocalByteIndex) -> Self {
            index.0 as _
        }
    }

    impl Add<usize> for LocalByteIndex {
        type Output = Self;

        fn add(self, offset: usize) -> Self::Output {
            Self::new(self.0 + LocalByteIndex::from_usize(offset).0)
        }
    }

    impl Add<char> for LocalByteIndex {
        type Output = Self;

        fn add(self, character: char) -> Self::Output {
            self + character.len_utf8() - 1
        }
    }

    impl Sub for LocalByteIndex {
        type Output = Self;

        fn sub(self, other: Self) -> Self::Output {
            Self::new(self.0 - other.0)
        }
    }

    impl Sub<usize> for LocalByteIndex {
        type Output = Self;

        fn sub(self, offset: usize) -> Self::Output {
            self - LocalByteIndex::from_usize(offset)
        }
    }
}

mod span {
    use super::{
        index::{AbstractByteIndex, Representation},
        ByteIndex, LocalByteIndex, Locality, PossiblySpanning, SourceFile, Spanning,
    };
    use std::{
        fmt,
        ops::{RangeInclusive, Sub},
    };

    /// A global byte span of source code.
    pub type Span = AbstractSpan<{ Locality::Global }>;

    /// A span inside a single source file.
    pub type LocalSpan = AbstractSpan<{ Locality::Local }>;

    #[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub struct AbstractSpan<const L: Locality> {
        pub start: AbstractByteIndex<L>,
        pub end: AbstractByteIndex<L>,
    }

    impl<const L: Locality> AbstractSpan<L> {
        pub fn new(start: AbstractByteIndex<L>, end: AbstractByteIndex<L>) -> Self {
            debug_assert!(start <= end);

            Self { start, end }
        }

        pub fn length(self) -> Representation {
            self.end.0 + 1 - self.start.0
        }

        pub fn contains_index(self, index: AbstractByteIndex<L>) -> bool {
            self.start <= index && index <= self.end
        }

        #[must_use]
        pub fn trim_start(self, amount: Representation) -> Self {
            // debug_assert!(amount < self.length());

            Self {
                start: self.start.map(|start| start + amount),
                end: self.end,
            }
        }

        #[must_use]
        pub fn trim_end(self, amount: Representation) -> Self {
            // debug_assert!(amount < self.length());

            Self {
                start: self.start,
                end: self.end.map(|end| end - amount),
            }
        }
    }

    impl<const L: Locality> fmt::Debug for AbstractSpan<L> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}..{}", self.start.0, self.end.0)
        }
    }

    // @Beacon @Task rename and create operations according to common set operations and adjust the
    // debugging asserts (which in many cases are overly restricted as they originally had a much more
    // narrow scope in mind)
    impl Span {
        /// Invalid span used for things without a source location.
        ///
        /// … but where the API requires it.
        pub const SHAM: Self = Self {
            start: ByteIndex::new(0),
            end: ByteIndex::new(0),
        };

        pub fn from_local(source: &SourceFile, span: LocalSpan) -> Self {
            Self::new(
                ByteIndex::from_local(source, span.start),
                ByteIndex::from_local(source, span.end),
            )
        }

        #[must_use]
        pub fn merge(self, other: impl PossiblySpanning) -> Self {
            match other.possible_span() {
                Some(other) => {
                    self.assert_disjoint_and_consecutive(other);
                    Self::new(self.start, other.end)
                }
                None => self,
            }
        }

        #[must_use]
        pub fn merge_into(self, other: impl PossiblySpanning) -> Self {
            if let Some(other) = other.possible_span() {
                other.assert_disjoint_and_consecutive(self);
                Self::new(other.start, self.end)
            } else {
                self
            }
        }

        pub fn merging<S: PossiblySpanning>(&mut self, other: S) -> S {
            if let Some(other) = other.possible_span() {
                self.assert_disjoint_and_consecutive(other);
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

        #[inline(always)]
        fn assert_disjoint_and_consecutive(self, other: Span) {
            debug_assert!(
                self.start <= other.start && self.end <= other.end,
                "assertion failed: {:?} <= {:?} && {:?} <= {:?} , start <= start' && end <= end'",
                self.start,
                other.start,
                self.end,
                other.end
            );
        }

        /// Similar to [Self::merge] except that the spans do not need to be disjoint.
        #[must_use]
        pub fn fit_end(self, other: impl PossiblySpanning) -> Self {
            match other.possible_span() {
                Some(other) => {
                    debug_assert!(self.start <= other.start && self.start <= other.end);

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
        // @Beacon @Note this can be removed once we support empty spans!
        pub fn zero() -> Self {
            Self::from(LocalByteIndex::new(0))
        }

        pub fn from_global(source: &SourceFile, span: Span) -> Self {
            Self::new(
                LocalByteIndex::from_global(source, span.start),
                LocalByteIndex::from_global(source, span.end),
            )
        }
    }

    impl From<LocalByteIndex> for LocalSpan {
        fn from(index: LocalByteIndex) -> Self {
            Self::new(index, index)
        }
    }

    impl From<LocalSpan> for RangeInclusive<usize> {
        fn from(span: LocalSpan) -> Self {
            span.start.into()..=span.end.into()
        }
    }

    impl Sub<LocalByteIndex> for LocalSpan {
        type Output = Self;

        fn sub(self, offset: LocalByteIndex) -> Self::Output {
            Self::new(self.start - offset, self.end - offset)
        }
    }
}

#[cfg(test)]
pub(crate) fn span(start: u32, end: u32) -> Span {
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
        pub data: T,
        pub span: Span,
    }

    impl<T> Spanned<T> {
        pub const fn new(span: Span, data: T) -> Self {
            Self { data, span }
        }

        pub fn map<Mapped>(self, mapper: impl FnOnce(T) -> Mapped) -> Spanned<Mapped> {
            Spanned {
                data: mapper(self.data),
                span: self.span,
            }
        }

        pub const fn as_ref(&self) -> Spanned<&T> {
            Spanned {
                data: &self.data,
                span: self.span,
            }
        }
    }

    impl<T> Spanning for Spanned<T> {
        fn span(&self) -> Span {
            self.span
        }
    }

    impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?} {:?}", self.data, self.span)
        }
    }

    impl<T: fmt::Display> fmt::Display for Spanned<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.data.fmt(f)
        }
    }
}

const START_OF_FIRST_SOURCE_FILE: ByteIndex = ByteIndex::new(1);

mod source_map {
    use super::{ByteIndex, Error, LocalByteIndex, LocalSpan, SourceFile, Span};
    use index_map::IndexMap;
    use std::{
        borrow::Borrow,
        cell::RefCell,
        path::{Path, PathBuf},
        rc::Rc,
    };
    use unicode_width::UnicodeWidthStr;

    pub type SharedSourceMap = Rc<RefCell<SourceMap>>;

    /// A mapping from an index or offset to source files.
    ///
    /// Most prominently, the offset is used to define [Span]s.
    #[derive(Default)]
    pub struct SourceMap {
        files: IndexMap<SourceFileIndex, SourceFile>,
    }

    impl SourceMap {
        pub fn shared() -> SharedSourceMap {
            Rc::new(RefCell::new(Self::default()))
        }

        fn next_offset(&self) -> Result<ByteIndex, Error> {
            self.files
                .last()
                .map(|file| file.span.end.offset(1))
                .unwrap_or(Ok(super::START_OF_FIRST_SOURCE_FILE))
        }

        pub fn load(&mut self, path: PathBuf) -> Result<SourceFileIndex, Error> {
            let source = std::fs::read_to_string(&path).map_err(Error::LoadFailure)?;
            self.add(Some(path), source)
        }

        /// Add a [SourceFile] to the source map.
        ///
        /// The first source file always has index `0`.
        pub fn add(
            &mut self,
            path: Option<PathBuf>,
            source: String,
        ) -> Result<SourceFileIndex, Error> {
            Ok(self
                .files
                .insert(SourceFile::new(path, source, self.next_offset()?)?))
        }

        /// Panics if span is a sham.
        // @Task do binary search (by span)
        fn file_from_span(&self, span: Span) -> &SourceFile {
            debug_assert!(span != Span::SHAM);

            self.files
                .values()
                .find(|file| file.span.contains_index(span.start))
                .unwrap()
        }

        /// Resolve a span to the string content it points to.
        ///
        /// This treats line breaks verbatim.
        pub fn snippet_from_span(&self, span: Span) -> &str {
            let file = self.file_from_span(span);
            let span = LocalSpan::from_global(&file, span);
            &file[span]
        }

        /// Resolve a span to various information useful for highlighting.
        ///
        /// This procedure is line-break-aware.
        // @Task update docs
        pub fn lines_from_span(&self, span: Span) -> Lines<'_> {
            let file = self.file_from_span(span);
            let span = LocalSpan::from_global(&file, span);

            let mut first_line = None;
            let mut final_line = None;

            let mut line = InterimLine {
                number: 1,
                start: None,
                end: None,
                highlight: None,
            };

            struct Highlight {
                start: LocalByteIndex,
                end: Option<LocalByteIndex>,
            }

            for (index, character) in file
                .content()
                .char_indices()
                .map(|(index, character)| (LocalByteIndex::from_usize(index), character))
            {
                if line.start.is_none() {
                    line.start = Some(index);
                    if first_line.is_some() {
                        line.highlight = Some(Highlight {
                            start: index,
                            end: None,
                        });
                    }
                }

                if index == span.start && line.highlight.is_none() {
                    line.highlight = Some(Highlight {
                        start: index,
                        end: None,
                    });
                }

                if index + character == span.end {
                    if let Some(highlight) = &mut line.highlight {
                        highlight.end = Some(index + character);
                    }
                }

                if character == '\n' {
                    line.end = Some(index);

                    if first_line.is_none() {
                        if let Some(highlight) = &mut line.highlight {
                            let number = line.number;

                            if highlight.end.is_none() {
                                highlight.end = line.end;
                            }

                            first_line = Some(line);

                            line = InterimLine {
                                number,
                                start: None,
                                end: None,
                                highlight: None,
                            };
                        }
                    }

                    if let Some(highlight) = &mut line.highlight {
                        if highlight.end.is_some() {
                            final_line = Some(line);
                            break;
                        }
                    }

                    line.start = None;
                    line.number += 1;
                }
            }

            struct InterimLine {
                number: u32,
                start: Option<LocalByteIndex>,
                end: Option<LocalByteIndex>,
                highlight: Option<Highlight>,
            }

            impl InterimLine {
                fn resolve(self, file: &SourceFile) -> Option<Line<'_>> {
                    let start = self.start?;
                    let highlight = self.highlight?;

                    let highlight_width =
                        match &file[LocalSpan::new(highlight.start, highlight.end?)] {
                            "\n" => 1,
                            snippet => snippet.width(),
                        };

                    let end = self.end?;

                    Some(Line {
                        number: self.number,
                        content: &file[LocalSpan::new(start, end)],
                        highlight_width,
                        highlight_prefix_width: if start < highlight.start {
                            file[LocalSpan::new(start, highlight.start - 1)].width()
                        } else {
                            0
                        },
                        highlight_start_column: (highlight.start + 1 - start).into(),
                    })
                }
            }

            Lines {
                path: file.path.as_deref().unwrap(),
                // @Beacon @Beacon @Beacon @Bug crashes on empty files
                // because our spans are always at least size 1
                first_line: first_line.unwrap().resolve(file).unwrap(),
                final_line: final_line.map(|line| line.resolve(file).unwrap()),
            }
        }
    }

    impl std::ops::Index<SourceFileIndex> for SourceMap {
        type Output = SourceFile;

        fn index(&self, index: SourceFileIndex) -> &Self::Output {
            &self.files.borrow()[index]
        }
    }

    #[derive(Debug, PartialEq, Eq, Clone, Copy, index_map::Index)]
    pub struct SourceFileIndex(usize);

    #[derive(Debug)]
    pub struct Lines<'a> {
        pub path: &'a Path,
        pub first_line: Line<'a>,
        /// This is `None` if the last is the first line.
        pub final_line: Option<Line<'a>>,
    }

    #[derive(Debug)]
    pub struct Line<'a> {
        pub number: u32,
        /// The content of the entire line that contains the to-be-highlighted snippet.
        ///
        /// It may contain the whole snippet or only the starting or the ending part of it
        /// if the snippet spans multiple lines.
        pub content: &'a str,
        /// In most cases the Unicode width of the to-be-highlighted snippet.
        ///
        /// The exception is the line break character (U+000A) which following the Unicode
        /// recommendations has a width of 0 but in our case has a width of 1. This allows
        /// us to actually point at the "invisible" character (by e.g. placing a caret below it).
        pub highlight_width: usize,
        pub highlight_prefix_width: usize,
        pub highlight_start_column: usize,
    }
}

mod source_file {
    use super::{ByteIndex, Error, LocalSpan, Span};
    use std::{convert::TryFrom, ops::RangeInclusive, path::PathBuf};

    /// A file, its contents and [its span](super::SourceMap).
    #[cfg_attr(test, derive(PartialEq, Eq))]
    pub struct SourceFile {
        pub path: Option<PathBuf>,
        content: String,
        pub span: Span,
    }

    impl SourceFile {
        pub fn new(
            path: Option<PathBuf>,
            content: String,
            start: ByteIndex,
        ) -> Result<Self, Error> {
            let offset = u32::try_from(content.len())
                .map_err(|_| Error::OffsetOverflow)?
                .saturating_sub(1);

            Ok(Self {
                path,
                content,
                span: Span::new(start, start.offset(offset)?),
            })
        }

        pub fn content(&self) -> &str {
            &self.content
        }
    }

    impl std::ops::Index<LocalSpan> for SourceFile {
        type Output = str;

        fn index(&self, index: LocalSpan) -> &Self::Output {
            &self.content[RangeInclusive::from(index)]
        }
    }
}

pub enum Error {
    OffsetOverflow,
    LoadFailure(io::Error),
}

impl DisplayWith for Error {
    type Context<'a> = &'a Path;

    fn format(&self, path: &Path, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use io::ErrorKind::*;

        write!(f, "the file `{}` ", path.to_string_lossy())?;

        f.write_str(match self {
            Self::OffsetOverflow => "is too large",
            // @Beacon @Beacon @Beacon @Beacon @Task expand this
            Self::LoadFailure(error) => match error.kind() {
                NotFound => "does not exist",
                PermissionDenied => "does not have the required permissions",
                // @Task reword
                InvalidData => "contains invalid UTF-8",
                // @Task use the provided message
                _ => "triggered some file system error",
            },
        })
    }
}
