use crate::diagnostic::{Diagnostic, Result};
use std::{convert::TryInto, fmt, ops::RangeInclusive, path::PathBuf, rc::Rc};
use unicode_width::UnicodeWidthStr;

// @Beacon @Task The API of [Span], [LocalSpan], [ByteIndex], [LocalByteIndex] is
// utter trash!! ugly, inconvenient, confusing, unsafe (trying to prevent overflow
// panics but they still gonna happen!)

/// Global byte index.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct ByteIndex(u32);

impl ByteIndex {
    pub const fn new(index: u32) -> Self {
        Self(index)
    }

    /// Map local byte index to global global one.
    ///
    /// ## Panics
    ///
    /// Panics on addition overflow.
    pub fn local(source: &SourceFile, index: LocalByteIndex) -> Self {
        Self::new(source.span.start.0 + index.0)
    }

    fn offset(self, offset: u32) -> Result<Self, Error> {
        let sum = self.0.checked_add(offset).ok_or(Error::OffsetOverflow)?;

        Ok(Self::new(sum))
    }
}

/// File-local byte index.
#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct LocalByteIndex(u32);

impl LocalByteIndex {
    pub const fn new(index: u32) -> Self {
        Self(index)
    }

    /// Create new file-local byte index.
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

use std::ops::{Add, Sub};

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

#[derive(Clone, PartialEq, Eq)]
pub struct Spanned<K> {
    pub kind: K,
    pub span: Span,
}

impl<K> Spanned<K> {
    pub const fn new(span: Span, kind: K) -> Self {
        Self { kind, span }
    }
}

impl<K: fmt::Debug> fmt::Debug for Spanned<K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {:?}", self.kind, self.span)
    }
}

/// Global byte span of source code.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Span {
    pub start: ByteIndex,
    pub end: ByteIndex,
}

impl Span {
    /// Invalid span used for expressions etc. without a source location.
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

    pub fn merge(self, other: &impl PossiblySpanning) -> Self {
        if let Some(other) = other.possible_span() {
            self.assert_consecutive(other);
            Self::new(self.start, other.end)
        } else {
            self
        }
    }

    pub fn merge_into(self, other: &impl PossiblySpanning) -> Self {
        if let Some(other) = other.possible_span() {
            other.assert_consecutive(self);
            Self::new(other.start, self.end)
        } else {
            self
        }
    }

    pub fn merging(&mut self, other: &impl PossiblySpanning) {
        if let Some(other) = other.possible_span() {
            self.assert_consecutive(other);
            self.end = other.end;
        }
    }

    #[inline(always)]
    fn assert_consecutive(self, other: Span) {
        debug_assert!(
            self.start <= other.start && self.end <= other.end,
            "assertion failed: {:?} <= {:?} && {:?} <= {:?} , start <= start' && end <= end'",
            self.start,
            other.start,
            self.end,
            other.end
        );
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}..{}]", self.start.0, self.end.0)
    }
}

pub trait Spanning {
    fn span(&self) -> Span;
}

impl Spanning for Span {
    fn span(&self) -> Span {
        *self
    }
}

impl<S> Spanning for Spanned<S> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<S: Spanning> Spanning for &S {
    fn span(&self) -> Span {
        (*self).span()
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

impl<S> PossiblySpanning for Option<S>
where
    S: Spanning,
{
    fn possible_span(&self) -> Option<Span> {
        self.as_ref().map(<_>::span)
    }
}

/// Span inside a single source file.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct LocalSpan {
    pub start: LocalByteIndex,
    pub end: LocalByteIndex,
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

pub const START_OF_FIRST_SOURCE_FILE: ByteIndex = ByteIndex::new(1);

#[derive(Default)]
pub struct SourceMap {
    files: Vec<Rc<SourceFile>>,
}

impl SourceMap {
    fn next_offset(&self) -> Result<ByteIndex, Error> {
        self.files
            .last()
            .map(|file| file.span.end.offset(1))
            .unwrap_or(Ok(START_OF_FIRST_SOURCE_FILE))
    }

    // @Note this could instead return an index, and index into an IndexVec of
    // SourceFiles
    pub fn load(&mut self, path: &Path) -> Result<Rc<SourceFile>, Error> {
        let source = std::fs::read_to_string(path).map_err(Error::LoadFailure)?;
        self.add(path.to_owned(), source)
    }

    fn add(&mut self, path: PathBuf, source: String) -> Result<Rc<SourceFile>, Error> {
        let file = Rc::new(SourceFile::new(path, source, self.next_offset()?)?);
        self.files.push(file.clone());

        Ok(file)
    }

    /// Panics if span is a sham.
    fn resolve_span_to_file(&self, span: Span) -> &SourceFile {
        debug_assert!(span != Span::SHAM);

        self.files
            .iter()
            .find(|file| file.span.contains_index(span.start))
            .unwrap()

        // @Bug panics @Beacon @Task find out why and adjust
        // let index = self
        //     .files
        //     .binary_search_by(|file| file.span.cmp(&span))
        //     .unwrap();

        // self.files[index].clone()
    }

    /// Resolve a span to the string content it points to.
    ///
    /// This treats line breaks verbatim.
    pub fn resolve_span_to_snippet(&self, span: Span) -> &str {
        let file = self.resolve_span_to_file(span);
        let span = LocalSpan::global(&file, span);
        &file[span]
    }

    // @Task improve documentation, @Note bad name of the method
    /// Resolve a span to various information useful for highlighting.
    ///
    /// This procedure is line-break-aware.
    pub fn resolve_span(&self, span: Span) -> ResolvedSpan<'_> {
        let file = self.resolve_span_to_file(span);
        let span = LocalSpan::global(&file, span);

        let mut first_line = None;
        let mut final_line = None;

        let mut line = Line {
            number: 1,
            start: None,
            end: None,
            highlight: None,
        };

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

                        line = Line {
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

        struct Line {
            number: u32,
            start: Option<LocalByteIndex>,
            end: Option<LocalByteIndex>,
            highlight: Option<Highlight>,
        }

        impl Line {
            fn extract_information(self, file: &SourceFile) -> Option<LineInformation<'_>> {
                let start = self.start?;
                let highlight = self.highlight?;

                // @Question should we instead map all 0s to 1s?
                let highlight_width = match &file[LocalSpan::new(highlight.start, highlight.end?)] {
                    "\n" => 1,
                    snippet => snippet.width(),
                };

                Some(LineInformation {
                    number: self.number,
                    content: &file[LocalSpan::new(start, self.end?)],
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

        struct Highlight {
            start: LocalByteIndex,
            end: Option<LocalByteIndex>,
        }

        ResolvedSpan {
            path: &file.path,
            first_line: first_line.unwrap().extract_information(file).unwrap(),
            final_line: final_line.map(|line| line.extract_information(file).unwrap()),
        }
    }
}

#[derive(Debug)]
pub struct ResolvedSpan<'a> {
    pub path: &'a Path,
    pub first_line: LineInformation<'a>,
    /// This is `None` if the last is the first line.
    pub final_line: Option<LineInformation<'a>>,
}

#[derive(Debug)]
pub struct LineInformation<'a> {
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

pub struct SourceFile {
    pub path: PathBuf,
    content: String,
    pub span: Span,
}

impl SourceFile {
    pub fn new(path: PathBuf, content: String, start: ByteIndex) -> Result<Self, Error> {
        use std::convert::TryFrom;

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

impl fmt::Debug for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SourceFile {:?} {:?}", self.path, self.span)
    }
}

use std::{io, path::Path};

pub enum Error {
    OffsetOverflow,
    LoadFailure(io::Error),
}

impl Error {
    pub fn message(&self, path: Option<&Path>) -> String {
        use io::ErrorKind::*;

        let mut message = path.map_or("referenced file ".into(), |path| {
            // @Question should we canonicalize the path? this might be less confusing
            // for users
            format!("file `{}` ", path.to_string_lossy())
        });

        message += match self {
            Error::OffsetOverflow => "is too large",
            Error::LoadFailure(error) => match error.kind() {
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
        Diagnostic::error().with_message(error.message(None))
    }
}
