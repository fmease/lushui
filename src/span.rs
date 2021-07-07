//! Data structures and procedures for handling source locations.

use crate::diagnostics::Diagnostic;
use std::{io, path::Path};

// @Beacon @Task The API of [Span], [LocalSpan], [ByteIndex], [LocalByteIndex] is
// utter trash!! ugly, inconvenient, confusing, unsafe (trying to prevent overflow
// panics but they still gonna happen!)

pub use index::{ByteIndex, LocalByteIndex};
pub use source_file::SourceFile;
pub use source_map::{SourceFileIndex, SourceMap};
pub use span::{LocalSpan, Span};
pub use spanned::Spanned;
pub use spanning::{PossiblySpanning, Spanning};

mod index {
    use super::{Error, SourceFile};
    use std::{
        convert::TryInto,
        ops::{Add, Sub},
    };

    /// A global byte index.
    ///
    /// Here, "global" means local relative to a [source map](super::SourceMap).
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
    pub struct ByteIndex(pub(super) u32);

    impl ByteIndex {
        pub const fn new(index: u32) -> Self {
            Self(index)
        }

        /// Map a local byte index to global global one.
        ///
        /// ## Panics
        ///
        /// Panics on addition overflow.
        pub fn local(source: &SourceFile, index: LocalByteIndex) -> Self {
            Self::new(source.span.start.0 + index.0)
        }

        pub(super) fn offset(self, offset: u32) -> Result<Self, Error> {
            let sum = self.0.checked_add(offset).ok_or(Error::OffsetOverflow)?;

            Ok(Self::new(sum))
        }
    }

    /// A file-local byte index.
    ///
    /// It _does not_ store information about the file. Hence, it is not
    /// [global](ByteIndex).
    #[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord)]
    pub struct LocalByteIndex(pub(super) u32);

    impl LocalByteIndex {
        pub const fn new(index: u32) -> Self {
            Self(index)
        }

        /// Create a new file-local byte index.
        ///
        /// ## Panics
        ///
        /// Panics if `index` does not fit into `u32`.
        // @Task get rid of this method
        pub fn from_usize(index: usize) -> Self {
            Self::new(index.try_into().unwrap())
        }

        pub fn global(source: &SourceFile, index: ByteIndex) -> Self {
            Self::new(index.0 - source.span.start.0)
        }

        pub fn saturating_sub(self, offset: u32) -> Self {
            Self::new(self.0.saturating_sub(offset))
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
    use super::{ByteIndex, LocalByteIndex, PossiblySpanning, SourceFile};
    use std::{
        fmt,
        mem::size_of,
        ops::{RangeInclusive, Sub},
    };

    /// A global byte span of source code.
    // @Task re-model with Option and NonZeroU32
    #[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub struct Span {
        pub(super) start: ByteIndex,
        pub(super) end: ByteIndex,
    }

    const _: () = assert!(size_of::<Span>() == 8);
    // const _: () = assert!(size_of::<Option<Span>>() == 8); // @Task

    // @Beacon @Task rename and create operations according to common set operations and adjust the
    // debugging asserts (which in many cases are overly restricted as they originally had a much more
    // narrow scope in mind)
    impl Span {
        /// Invalid span used for things without a source location.
        ///
        /// … but where the API requires it.
        // @Task remove and replace with None once we change the whole code base
        pub const SHAM: Self = Self {
            start: ByteIndex::new(0),
            end: ByteIndex::new(0),
        };

        pub fn new(start: ByteIndex, end: ByteIndex) -> Self {
            debug_assert!(start <= end);

            Self { start, end }
        }

        pub fn length(self) -> u32 {
            self.end.0 + 1 - self.start.0
        }

        #[must_use]
        pub fn trim_start(self, amount: u32) -> Self {
            debug_assert!(amount < self.length());

            Self {
                start: ByteIndex::new(self.start.0 + amount),
                end: self.end,
            }
        }

        pub fn local(source: &SourceFile, span: LocalSpan) -> Self {
            Self::new(
                ByteIndex::local(source, span.start),
                ByteIndex::local(source, span.end),
            )
        }

        pub fn contains_index(self, index: ByteIndex) -> bool {
            self.start <= index && index <= self.end
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

        pub fn merging<S: PossiblySpanning>(&mut self, spanning: S) -> S {
            if let Some(other) = spanning.possible_span() {
                self.assert_disjoint_and_consecutive(other);
                self.end = other.end;
            }
            spanning
        }

        // @Note naming is not great
        pub fn merging_from(&mut self, spanning: impl PossiblySpanning) {
            if let Some(other) = spanning.possible_span() {
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

    impl fmt::Debug for Span {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}..{}", self.start.0, self.end.0)
        }
    }

    /// A span inside a single source file.
    ///
    /// This _does not_ include information about the file. Thus, it is not
    /// [global](Span).
    #[derive(Clone, Copy, PartialEq, Eq, Debug)]
    pub struct LocalSpan {
        pub(crate) start: LocalByteIndex,
        pub(crate) end: LocalByteIndex,
    }

    impl LocalSpan {
        pub fn new(start: LocalByteIndex, end: LocalByteIndex) -> Self {
            Self { start, end }
        }

        pub fn zero() -> Self {
            Self::from(LocalByteIndex::new(0))
        }

        pub fn global(source: &SourceFile, span: Span) -> Self {
            Self::new(
                LocalByteIndex::global(source, span.start),
                LocalByteIndex::global(source, span.end),
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
    use super::{Span, Spanned};
    use crate::SmallVec;

    pub trait Spanning: PossiblySpanning {
        fn span(&self) -> Span;
    }

    impl Spanning for Span {
        fn span(&self) -> Self {
            *self
        }
    }

    impl<S: Spanning> Spanning for &'_ S {
        fn span(&self) -> Span {
            (**self).span()
        }
    }

    impl<S> Spanning for Spanned<S> {
        fn span(&self) -> Span {
            self.span
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
    use super::Span;
    use std::fmt;

    #[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub struct Spanned<Kind> {
        pub kind: Kind,
        pub span: Span,
    }

    impl<Kind> Spanned<Kind> {
        pub const fn new(span: Span, kind: Kind) -> Self {
            Self { kind, span }
        }

        pub fn map<MappedKind>(
            self,
            mapper: impl FnOnce(Kind) -> MappedKind,
        ) -> Spanned<MappedKind> {
            Spanned {
                kind: mapper(self.kind),
                span: self.span,
            }
        }

        pub const fn as_ref(&self) -> Spanned<&Kind> {
            Spanned {
                kind: &self.kind,
                span: self.span,
            }
        }
    }

    impl<Kind: fmt::Debug> fmt::Debug for Spanned<Kind> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?} {:?}", self.kind, self.span)
        }
    }

    impl<Kind: fmt::Display> fmt::Display for Spanned<Kind> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.kind.fmt(f)
        }
    }
}

const START_OF_FIRST_SOURCE_FILE: ByteIndex = ByteIndex::new(1);

mod source_map {
    use super::{ByteIndex, Error, LocalByteIndex, LocalSpan, SourceFile, Span};
    use indexed_vec::IndexVec;
    use std::{
        borrow::Borrow,
        path::{Path, PathBuf},
    };
    use unicode_width::UnicodeWidthStr;

    /// A mapping from an index or offset to source files.
    ///
    /// Most prominently, the offset is used to define [Span]s.
    // @Task use indices to enable unloading source file when they are not needed
    #[derive(Default)]
    pub struct SourceMap {
        // @Task do Rc<RefCell<_>>
        files: IndexVec<SourceFileIndex, SourceFile>,
    }

    impl SourceMap {
        fn next_offset(&self) -> Result<ByteIndex, Error> {
            self.files
                .last()
                .map(|file| file.span.end.offset(1))
                .unwrap_or(Ok(super::START_OF_FIRST_SOURCE_FILE))
        }

        // @Note this could instead return an index, and index into an IndexVec of
        // SourceFiles
        pub fn load(&mut self, path: PathBuf) -> Result<SourceFileIndex, Error> {
            let source = std::fs::read_to_string(&path).map_err(Error::LoadFailure)?;
            self.add(Some(path), source)
        }

        /// Add a [SourceFile] to the source map.
        ///
        /// The first source file always has index `0`.
        ///
        /// ```
        /// # fn main() {
        /// # use lushui::span::{SourceFileIndex, SourceMap};
        /// # use indexed_vec::Idx as _;
        /// let index = SourceMap::default()
        ///     .add(None, String::new())
        ///     .unwrap_or_else(|_| unreachable!());
        ///
        /// assert_eq!(index, SourceFileIndex::new(0));
        /// # }
        /// ```
        pub fn add(
            &mut self,
            path: Option<PathBuf>,
            source: String,
        ) -> Result<SourceFileIndex, Error> {
            Ok(self
                .files
                .push(SourceFile::new(path, source, self.next_offset()?)?))
        }

        pub fn get(&self, index: SourceFileIndex) -> &SourceFile {
            // Ref::map(self.files.borrow(), |files| &files[index])
            &self.files.borrow()[index]
        }

        /// Panics if span is a sham.
        // @Task do binary search (by span)
        fn file_from_span(&self, span: Span) -> &SourceFile {
            debug_assert!(span != Span::SHAM);

            self.files
                .iter()
                .find(|file| file.span.contains_index(span.start))
                .unwrap()
        }

        /// Resolve a span to the string content it points to.
        ///
        /// This treats line breaks verbatim.
        pub fn snippet_from_span(&self, span: Span) -> &str {
            let file = self.file_from_span(span);
            let span = LocalSpan::global(&file, span);
            &file[span]
        }

        /// Resolve a span to various information useful for highlighting.
        ///
        /// This procedure is line-break-aware.
        // @Task update docs
        pub fn lines_from_span(&self, span: Span) -> Lines<'_> {
            let file = self.file_from_span(span);
            let span = LocalSpan::global(&file, span);

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
                first_line: first_line.unwrap().resolve(file).unwrap(),
                final_line: final_line.map(|line| line.resolve(file).unwrap()),
            }
        }
    }

    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub struct SourceFileIndex(usize);

    impl indexed_vec::Idx for SourceFileIndex {
        fn new(index: usize) -> Self {
            Self(index)
        }

        fn index(self) -> usize {
            self.0
        }
    }

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

impl Error {
    pub fn message(&self, path: Option<&Path>) -> String {
        use io::ErrorKind::*;

        let mut message = path.map_or("referenced file ".into(), |path| {
            // @Question should we canonicalize the path? this might be less confusing for users
            format!("file `{}` ", path.to_string_lossy())
        });

        message += match self {
            Self::OffsetOverflow => "is too large",
            Self::LoadFailure(error) => match error.kind() {
                NotFound => "does not exist",
                PermissionDenied => "does not have required permissions",
                InvalidData => "contains invalid UTF-8",
                _ => "triggered some file system error",
            },
        };

        message
    }
}

impl From<Error> for Diagnostic {
    fn from(error: Error) -> Self {
        Diagnostic::error().message(error.message(None))
    }
}
