use std::{convert::TryInto, path::PathBuf, rc::Rc};

/// Global byte index.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct ByteIndex {
    value: u32,
}

impl ByteIndex {
    fn new(index: u32) -> Self {
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
        let sum = self
            .value
            .checked_add(offset)
            .ok_or(Error::OffsetOverflow)?;

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

/// Global byte span of source code.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    start: ByteIndex,
    end: ByteIndex,
}

impl Span {
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

    pub fn dummy() -> Self {
        Self::new(ByteIndex::new(0), ByteIndex::new(0))
    }

    pub fn contains_index(self, index: ByteIndex) -> bool {
        self.start <= index && index <= self.end
    }

    pub fn merge(self, other: Self) -> Self {
        debug_assert!(self.start <= other.start && self.end <= other.end);

        Self::new(self.start, other.end)
    }
}

/// Span inside a single source file.
#[derive(Clone, Copy)]
pub struct LocalSpan {
    pub start: LocalByteIndex,
    pub end: LocalByteIndex,
}

impl LocalSpan {
    pub fn new(start: LocalByteIndex, end: LocalByteIndex) -> Self {
        Self { start, end }
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

#[derive(Default)]
pub struct SourceMap {
    files: Vec<Rc<SourceFile>>,
}

impl SourceMap {
    fn next_offset(&self) -> Result<ByteIndex> {
        match self.files.last() {
            Some(file) => file.span.end.try_add_offset(1),
            None => Ok(ByteIndex::new(0)),
        }
    }

    pub fn load(&mut self, path: std::path::PathBuf) -> Result<Rc<SourceFile>> {
        let source = std::fs::read_to_string(&path).map_err(Error::IO)?;
        self.add(FileName::Real(path), source)
    }

    fn add(&mut self, name: FileName, source: String) -> Result<Rc<SourceFile>> {
        let file = Rc::new(SourceFile::new(name, source, self.next_offset()?)?);
        self.files.push(file.clone());

        Ok(file)
    }

    // @Task do a binary search instead of a linear one
    // @Note panics on invalid index
    fn file_from_index(&self, index: ByteIndex) -> Rc<SourceFile> {
        self.files
            .iter()
            .find(|file| file.span.contains_index(index))
            .unwrap()
            .clone()
    }

    // @Beacon @Task handle multiline spans
    pub fn resolve_span(&self, span: Span) -> Lines {
        let file = self.file_from_index(span.start);
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
            if index == span.end {
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

pub struct Line {
    pub content: String,
    pub number: u32,
    pub highlight: RangeInclusive<usize>,
}

pub struct SourceFile {
    name: FileName,
    content: String,
    span: Span,
}

impl SourceFile {
    pub fn new(name: FileName, content: String, start: ByteIndex) -> Result<Self> {
        use std::convert::TryFrom;

        let offset = u32::try_from(content.len()).map_err(|_| Error::OffsetOverflow)? - 1;

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

#[derive(Clone)]
pub enum FileName {
    Real(PathBuf),
    Anonymous,
}

impl FileName {
    // @Note we do not impl Display because I want to handle the inner PathBuf
    // differently, not using to_lossy. Maybe returning OsString?
    pub fn to_string(&self) -> String {
        match self {
            FileName::Real(path) => path.to_string_lossy().to_string(),
            FileName::Anonymous => "<anonymous>".into(),
        }
    }
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
pub enum Error {
    OffsetOverflow,
    IO(std::io::Error),
}

use std::fmt;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use std::io::ErrorKind::*;

        f.write_str(match self {
            Self::OffsetOverflow => "input file too large",
            Self::IO(error) => match error.kind() {
                NotFound => "referenced file does not exist",
                PermissionDenied => "file does not have required permissions",
                _ => "an I/O error occurred",
            },
        })
    }
}
