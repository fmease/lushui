use crate::diagnostic::*;
use std::{convert::TryInto, fmt, ops::RangeInclusive, rc::Rc};
use unicode_width::UnicodeWidthStr;

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
    pub fn from_local(source: &SourceFile, index: LocalByteIndex) -> Self {
        Self::new(source.span.start.0 + index.0)
    }

    fn try_add_offset(self, offset: u32) -> Result<Self> {
        let sum = self.0.checked_add(offset).ok_or(offset_overflow())?;

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

    pub fn from_global(source: &SourceFile, index: ByteIndex) -> Self {
        Self::new(index.0 - source.span.start.0)
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
        write!(f, "Span({}, {})", self.start.0, self.end.0)
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

pub const START_OF_FIRST_SOURCE_FILE: ByteIndex = ByteIndex::new(1);

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

    pub fn resolve_span(&self, span: Span) -> ResolvedSpan<'_> {
        let file = self.file_from_span(span);
        let span = LocalSpan::from_global(&file, span);

        let mut first_line = None::<Line>;
        let mut final_line = None::<Line>;

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

                let highlight_width = match &file[LocalSpan::new(highlight.start, highlight.end?)] {
                    "\n" => 1,
                    snippet => snippet.width(),
                };

                Some(LineInformation {
                    number: self.number,
                    content: &file[LocalSpan::new(start, self.end?)],
                    highlight_width,
                    highlight_prefix_width: file[LocalSpan::new(start, highlight.start - 1)]
                        .width(),
                    highlight_start_column: (highlight.start + 1 - start).into(),
                })
            }
        }

        struct Highlight {
            start: LocalByteIndex,
            end: Option<LocalByteIndex>,
        }

        ResolvedSpan {
            filename: &file.name,
            first_line: first_line.unwrap().extract_information(file).unwrap(),
            final_line: final_line.map(|line| line.extract_information(file).unwrap()),
        }
    }
}

#[derive(Debug)]
pub struct ResolvedSpan<'a> {
    pub filename: &'a str,
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
