use std::{
    convert::TryInto,
    path::{Path, PathBuf},
    rc::Rc,
};

/// Global byte index.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct ByteIndex {
    pub value: u32,
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
            value: source.span.start().value + index.value,
        }
    }

    pub fn try_add_offset(self, offset: u32) -> Result<Self> {
        let sum = self
            .value
            .checked_add(offset)
            .ok_or(Error::OffsetOverflow)?;

        Ok(Self::new(sum))
    }

    // @Temporary
    pub fn try_add_negative_offset(self, offset: u32) -> Result<Self> {
        let sum = self
            .value
            .checked_sub(offset)
            .ok_or(Error::OffsetOverflow)?;

        Ok(Self::new(sum))
    }
}

/// File-local byte index.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct LocalByteIndex {
    value: u32,
}

impl LocalByteIndex {
    /// Create new file-local byte index.
    ///
    /// ## Panics
    ///
    /// Panics if `index` does not fit into `u32`.
    pub fn new(index: usize) -> Self {
        Self {
            value: index.try_into().unwrap(),
        }
    }
}

use std::ops::{Add, Sub};

impl Add<usize> for LocalByteIndex {
    type Output = Self;

    fn add(self, offset: usize) -> Self {
        Self {
            value: self.value + LocalByteIndex::new(offset).value,
        }
    }
}

impl Sub<usize> for LocalByteIndex {
    type Output = Self;

    fn sub(self, offset: usize) -> Self {
        Self {
            value: self.value - LocalByteIndex::new(offset).value,
        }
    }
}

/// Absolute byte span of source code.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    start: ByteIndex,
    end: ByteIndex,
}

impl Span {
    pub fn new(start: ByteIndex, end: ByteIndex) -> Span {
        debug_assert!(start <= end);

        Self { start, end }
    }

    pub fn from_local(source: &SourceFile, start: LocalByteIndex, end: LocalByteIndex) -> Self {
        Self::new(
            ByteIndex::from_local(source, start),
            ByteIndex::from_local(source, end),
        )
    }

    // @Bug @Temporary actually links to a possibly valid line
    pub fn dummy() -> Self {
        Self {
            start: ByteIndex::new(0),
            end: ByteIndex::new(0),
        }
    }

    pub fn start(self) -> ByteIndex {
        self.start
    }

    pub fn end(self) -> ByteIndex {
        self.end
    }

    // pub fn _contains(self, span: Span) -> bool {
    //     // @Task debug_assert that span does not span multiple files

    //     self.contains_index(span.start)
    // }

    pub fn contains_index(self, index: ByteIndex) -> bool {
        self.start <= index && index <= self.end
    }

    pub fn merge(self, other: Self) -> Self {
        debug_assert!(self.start <= other.start && self.end <= other.end);

        Span {
            start: self.start,
            end: other.end,
        }
    }
}

#[derive(Default)]
pub struct SourceMap {
    files: Vec<Rc<SourceFile>>,
}

impl SourceMap {
    fn next_offset(&self) -> Result<ByteIndex> {
        match self.files.last() {
            Some(file) => file.span.end().try_add_offset(1),
            None => Ok(ByteIndex::new(0)),
        }
    }

    pub fn load(&mut self, path: &Path) -> Result<Rc<SourceFile>> {
        let source = std::fs::read_to_string(path).map_err(Error::IO)?;
        self.add(FileName::Real(path.to_owned()), source)
    }

    fn add(&mut self, name: FileName, source: String) -> Result<Rc<SourceFile>> {
        let file = Rc::new(SourceFile::new(name, source, self.next_offset()?)?);
        self.files.push(file.clone());

        Ok(file)
    }

    // @Beacon @Beacon @Task
    // @Task do a binary search instead of a linear one
    // returns position as `<filepath>:<line>:<column>`
    pub fn index_to_file_name_line_column(&self, index: ByteIndex) -> Result<String> {
        let name = self
            .files
            .iter()
            .find(|file| file.span.contains_index(index))
            .map(|file| file.name.to_string())
            .ok_or(Error::UnmappedSpan)?;

        // @Task not done yet!!
        Ok(name)
    }

    // @Beacon @Beacon @Task
    // @Question should it panic or result?
    pub fn span_to_source(&self, _span: Span) -> Result<String> {
        if self.files.is_empty() {
            return Err(Error::UnmappedSpan);
        }

        todo!()
    }
}

pub struct SourceFile {
    name: FileName,
    content: String,
    pub span: Span,
}

impl SourceFile {
    pub fn new(name: FileName, content: String, start: ByteIndex) -> Result<Self> {
        let offset = content
            .len()
            .try_into()
            .map_err(|_| Error::OffsetOverflow)?;

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

use std::ops::{Index, RangeInclusive};

impl Index<RangeInclusive<LocalByteIndex>> for SourceFile {
    type Output = str;

    fn index(&self, index: RangeInclusive<LocalByteIndex>) -> &Self::Output {
        let start = index.start().value as usize;
        let end = index.end().value as usize;
        &self.content[start..=end]
    }
}

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
    UnmappedSpan,
    IO(std::io::Error),
}
