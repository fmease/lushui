use crate::diagnostic::*;
use std::{convert::TryInto, fmt, rc::Rc};

/// Global byte index.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct ByteIndex {
    value: u32,
}

impl ByteIndex {
    pub const fn new(index: u32) -> Self {
        ByteIndex { value: index }
    }

    /// Map local byte index to global global one.
    ///
    /// ## Panics
    ///
    /// Panics on addition overflow.
    pub fn from_local(source: &SourceFile, index: LocalByteIndex) -> Self {
        Self {
            value: source.span.start.value + index.value,
        }
    }

    fn try_add_offset(self, offset: u32) -> Result<Self> {
        let sum = self.value.checked_add(offset).ok_or(offset_overflow())?;

        Ok(Self::new(sum))
    }
}

/// File-local byte index.
#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct LocalByteIndex {
    value: u32,
}

impl LocalByteIndex {
    pub fn new(index: u32) -> Self {
        Self { value: index }
    }

    /// Create new file-local byte index.
    ///
    /// ## Panics
    ///
    /// Panics if `index` does not fit into `u32`.
    pub fn from_usize(index: usize) -> Self {
        Self::new(index.try_into().unwrap())
    }

    pub fn from_global(source: &SourceFile, index: ByteIndex) -> Self {
        Self::new(index.value - source.span.start.value)
    }
}

impl From<LocalByteIndex> for usize {
    fn from(index: LocalByteIndex) -> Self {
        index.value as _
    }
}

use std::ops::{Add, Sub};

impl Add<usize> for LocalByteIndex {
    type Output = Self;

    fn add(self, offset: usize) -> Self::Output {
        Self::new(self.value + LocalByteIndex::from_usize(offset).value)
    }
}

impl Sub for LocalByteIndex {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        Self::new(self.value - other.value)
    }
}

impl Sub<usize> for LocalByteIndex {
    type Output = Self;

    fn sub(self, offset: usize) -> Self::Output {
        self - LocalByteIndex::from_usize(offset)
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Spanned<K> {
    pub kind: K,
    pub span: Span,
}

impl<K> Spanned<K> {
    pub const fn new(kind: K, span: Span) -> Self {
        Self { kind, span }
    }
}

/// Global byte span of source code.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Span {
    pub start: ByteIndex,
    pub end: ByteIndex,
}

impl Span {
    pub const SHAM: Self = Self {
        start: ByteIndex::new(0),
        end: ByteIndex::new(0),
    };

    pub fn new(start: ByteIndex, end: ByteIndex) -> Self {
        debug_assert!(start <= end);

        Self { start, end }
    }

    pub fn from_local(source: &SourceFile, span: LocalSpan) -> Self {
        Self::new(
            ByteIndex::from_local(source, span.start),
            ByteIndex::from_local(source, span.end),
        )
    }

    pub fn contains_index(self, index: ByteIndex) -> bool {
        self.start <= index && index <= self.end
    }

    pub fn merge(self, other: Self) -> Self {
        debug_assert!(self.start <= other.start && self.end <= other.end);

        Self::new(self.start, other.end)
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Span({}, {})", self.start.value, self.end.value)
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

    // @Bug this is actually a valid span
    pub fn dummy() -> Self {
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

const START_OF_FIRST_SOURCE_FILE: ByteIndex = ByteIndex::new(1);

#[derive(Default)]
pub struct SourceMap {
    files: Vec<Rc<SourceFile>>,
}

impl SourceMap {
    fn next_offset(&self) -> Result<ByteIndex> {
        self.files
            .last()
            .map(|file| file.span.end.try_add_offset(1))
            .unwrap_or(Ok(START_OF_FIRST_SOURCE_FILE))
    }

    pub fn load(&mut self, path: &str) -> Result<Rc<SourceFile>> {
        let source = std::fs::read_to_string(path).map_err(io_error)?;
        self.add(path.to_owned(), source)
    }

    fn add(&mut self, name: String, source: String) -> Result<Rc<SourceFile>> {
        let file = Rc::new(SourceFile::new(name, source, self.next_offset()?)?);
        self.files.push(file.clone());

        Ok(file)
    }

    // @Note panics on invalid span
    fn file_from_span(&self, span: Span) -> &SourceFile {
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

    // @Beacon @Task handle multiline spans
    pub fn resolve_span(&self, span: Span) -> Lines {
        let file = self.file_from_span(span);
        let span = LocalSpan::from_global(&file, span);
        let mut line_number = 1;
        let mut highlight_start = None;
        let mut highlight = None;
        let mut line_start = None;
        let mut line = None;

        for (index, character) in file.content().char_indices() {
            let index = LocalByteIndex::from_usize(index);

            if line_start.is_none() {
                line_start = Some(index);
            }

            if index == span.start {
                highlight_start = Some(index);
            }
            if index + character.len_utf8() - 1 == span.end {
                let span = LocalSpan::new(highlight_start.unwrap(), index);
                let offset = line_start.unwrap();

                highlight = Some(Highlight {
                    line_number,
                    // @Note panics on multiline string
                    range: (span - offset).into(),
                });
            }

            if character == '\n' {
                // @Bug does not work with multiline spans
                if highlight.is_some() && line.is_some() {
                    break;
                }

                if highlight.is_some() {
                    line = Some(LocalSpan::new(line_start.unwrap(), index));
                }

                line_number += 1;
                line_start = None;
            } else {
            }
        }

        struct Highlight {
            line_number: u32,
            range: RangeInclusive<usize>,
        }

        let highlight = highlight.unwrap();
        let line = line.unwrap();

        Lines {
            filename: file.name.to_string(),
            first: Line {
                content: file[line].to_owned(),
                number: highlight.line_number,
                highlight: highlight.range,
            },
            last: None,
        }
    }
}

pub struct Lines {
    pub filename: String,
    pub first: Line,
    /// This is `None` if the last is the first line.
    pub last: Option<Line>,
}

use std::ops::RangeInclusive;

// @Task find better field names
pub struct Line {
    pub content: String,
    pub number: u32,
    pub highlight: RangeInclusive<usize>,
}

pub struct SourceFile {
    pub name: String,
    content: String,
    pub span: Span,
}

impl SourceFile {
    pub fn new(name: String, content: String, start: ByteIndex) -> Result<Self> {
        use std::convert::TryFrom;

        let offset = u32::try_from(content.len())
            .map_err(|_| offset_overflow())?
            .saturating_sub(1);

        Ok(Self {
            name,
            content,
            span: Span::new(start, start.try_add_offset(offset)?),
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
        write!(f, "SourceFile {} {:?}", self.name, self.span)
    }
}

// @Task add file name once we support it #SpanOfWholeFileInDiagnostic
fn offset_overflow() -> Diagnostic {
    Diagnostic::new(Level::Fatal, None, "file too large")
}

fn io_error(error: std::io::Error) -> Diagnostic {
    use std::io::ErrorKind::*;

    let message = match error.kind() {
        NotFound => "referenced file does not exist",
        PermissionDenied => "file does not have required permissions",
        InvalidData => "file contains invalid UTF-8",
        _ => "an I/O error occurred",
    };

    Diagnostic::new(Level::Fatal, None, message)
}
