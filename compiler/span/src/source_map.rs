use super::{ByteIndex, LocalByteIndex, LocalSpan, Span, Spanning};
use index_map::IndexMap;
use std::{io, ops::Range, path::Path, sync::Arc};
use unicode_width::UnicodeWidthStr;
use utility::{
    default, obtain,
    path::{CanonicalPath, CanonicalPathBuf},
    ComponentIndex,
};

#[cfg(test)]
mod test;

// @Beacon @Task if possible, get rid of dependency on `ComponentIndex`
//               (it's was recently added for the LSP server)

/// A mapping from [index](SourceFileIndex) to [source file](SourceFile).
///
/// Most prominently, the index – an offset and obtained by adding a source file to this map –
/// is the key component to define [`Span`] (via [`ByteIndex`]).
///
/// The source files are laid out next to each other and padded on their left (at their start)
/// by one byte (in the sense of `Span::length(_) == 1`) to reserve space for _end of input_
/// pseudo tokens (e.g. `BareToken::EndOfInput`) and the _end of input_ virtual location.  
/// Additionally, this frees up the byte index `0` and allows `Span::default()`
/// – starting at this unmapped index and empty – to be interpreted as an _unknown location_.
///
/// # Visualization
///
/// ```text
/// | |  f0  | |  f1  | ...
///  ^ ^      ^ ^      ^
///  | |      | |      |
///  | |      | |      padding   f1.span().end()
///  | |      | source file f1   f1.span()
///  | |      padding            f0.span().end()
///  | source file f0            f0.span()
///  padding, unknown location   Span::default()
/// ```
#[derive(Default)]
pub struct SourceMap {
    files: IndexMap<SourceFileIndex, SourceFile>,
}

impl SourceMap {
    fn next_offset(&self) -> ByteIndex {
        const PADDING: u32 = 1;

        self.files
            .last()
            .map(|file| file.span().end)
            .unwrap_or_default()
            + PADDING
    }

    /// Open a file given its path and add it as a [`SourceFile`] to the map.
    pub fn load(
        &mut self,
        path: &Path,
        component: Option<ComponentIndex>,
    ) -> io::Result<SourceFileIndex> {
        let path = CanonicalPathBuf::new(path)?;
        let source = std::fs::read_to_string(&path)?;
        Ok(self.add(path, Arc::new(source), component))
    }

    // @Task better name
    pub fn read(
        &mut self,
        path: CanonicalPathBuf,
        component: Option<ComponentIndex>,
    ) -> io::Result<SourceFileIndex> {
        let source = std::fs::read_to_string(&path)?;
        Ok(self.add(path, Arc::new(source), component))
    }

    /// Add text to the map creating a [`SourceFile`] in the process.
    pub fn add(
        &mut self,
        name: impl Into<FileName>,
        source: Arc<String>,
        component: Option<ComponentIndex>,
    ) -> SourceFileIndex {
        self.files
            .insert(SourceFile::new(name, source, self.next_offset(), component))
    }

    pub fn add_str(&mut self, name: impl Into<FileName>, source: &str) -> SourceFileIndex {
        self.add(name, Arc::new(source.to_owned()), None)
    }

    pub fn file(&self, span: Span) -> &SourceFile {
        debug_assert!(span != default());

        // @Task do binary search (by span)
        self.files
            .values()
            .find(|file| file.span().contains(span.start))
            .unwrap()
    }

    // @Beacon @Temporary
    pub fn file_by_path(&self, path: &CanonicalPath) -> Option<&SourceFile> {
        self.files
            .values()
            .find(|file| file.name.path() == Some(path))
    }

    /// Resolve a span to the string content it points to.
    ///
    /// This treats line breaks verbatim.
    pub fn snippet(&self, span: Span) -> &str {
        let file = self.file(span);
        let span = span.local(file);
        &file[span]
    }

    pub fn lines_with_highlight(&self, span: Span) -> LinesWithHighlight<'_> {
        let file = self.file(span);
        let span = span.local(file);

        let mut current_line = InterimLine::new(1);
        let mut first_line = None; // the first line of the highlight
        let mut last_line = None; // the last line of the highlight

        for (index, character) in file
            .content()
            .char_indices()
            .map(|(index, character)| (index.try_into().unwrap(), character))
        {
            if current_line.start.is_none() {
                current_line.start = Some(index);

                if first_line.is_some() {
                    // the first line of the highlight has been found
                    // prepare for finding the final line (which might coincide with the first)
                    current_line.highlight = Some(InterimHighlight {
                        start: index,
                        end: None,
                    });
                }
            }

            if index == span.start {
                current_line.highlight = Some(InterimHighlight {
                    start: index,
                    end: None,
                });
            }

            if index == span.end {
                if let Some(highlight) = &mut current_line.highlight {
                    highlight.end = Some(index);
                }
            }

            if character == '\n' {
                current_line.end = Some(index);
                let line_number = current_line.number;

                if let Some(highlight) = &mut current_line.highlight {
                    if first_line.is_none() {
                        if highlight.end.is_none() {
                            highlight.end = current_line.end;
                        }
                        first_line = Some(current_line.reset(line_number));
                    } else if highlight.end.is_some() {
                        last_line = Some(current_line.reset(line_number));
                        break;
                    }
                }

                if index + character != span.end {
                    // unless this line break is the last character in the file

                    current_line.start = None;
                    current_line.end = None;
                    current_line.number += 1;
                }
            }
        }

        {
            let index = file.local_span().end;

            if current_line.start.is_none() {
                current_line.start = Some(index);
            }

            if index == span.start {
                current_line.highlight = Some(InterimHighlight {
                    start: index,
                    end: None,
                });
            }

            if index == span.end {
                if let Some(highlight) = &mut current_line.highlight {
                    highlight.end = Some(index);
                }
            }

            if current_line.end.is_none() {
                // no trailing line break

                current_line.end = Some(index);
            }

            if let Some(highlight) = &mut current_line.highlight {
                if first_line.is_none() {
                    if span.end <= index {
                        if highlight.end.is_none() {
                            highlight.end = current_line.end;
                        }
                        first_line = Some(current_line);
                    }
                }
                // @Question does it need a condition like span.end <= index??
                else if highlight.end.is_some() {
                    last_line = Some(current_line);
                }
            }
        }

        struct InterimLine {
            /// One-indexed line number.
            number: u32,
            start: Option<LocalByteIndex>,
            end: Option<LocalByteIndex>,
            highlight: Option<InterimHighlight>,
        }

        impl InterimLine {
            fn new(line_number: u32) -> Self {
                Self {
                    number: line_number,
                    start: None,
                    end: None,
                    highlight: None,
                }
            }

            fn reset(&mut self, line_number: u32) -> Self {
                std::mem::replace(self, Self::new(line_number))
            }

            fn resolve(self, file: &SourceFile) -> Option<LineWithHighlight<'_>> {
                let line_start = self.start?;
                let line_end = self.end?;
                let highlight = self.highlight?;
                let highlight_start = highlight.start;
                let highlight_end = highlight.end?;

                let highlight_prefix = &file[LocalSpan::new(line_start, highlight_start)];
                let highlight = &file[LocalSpan::new(highlight_start, highlight_end)];

                // @Beacon @Task avoid calling `.chars().count()` if possible for
                // performance reasons and try to derive this information from
                // `InterimLine` (or enrich that data structure if it doesn't contain it)
                let start = highlight_prefix.chars().count() + 1;
                let end = start + highlight.chars().count();

                Some(LineWithHighlight {
                    number: self.number,
                    content: &file[LocalSpan::new(line_start, line_end)],
                    highlight: Highlight {
                        start: start.try_into().unwrap(),
                        end: end.try_into().unwrap(),
                        width: highlight.width(),
                        prefix_width: highlight_prefix.width(),
                    },
                })
            }
        }

        struct InterimHighlight {
            start: LocalByteIndex,
            end: Option<LocalByteIndex>,
        }

        LinesWithHighlight {
            file: &file.name,
            first: first_line.unwrap().resolve(file).unwrap(),
            last: last_line.map(|line| line.resolve(file).unwrap()),
        }
    }
}

impl std::ops::Index<SourceFileIndex> for SourceMap {
    type Output = SourceFile;

    fn index(&self, index: SourceFileIndex) -> &Self::Output {
        &self.files[index]
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, index_map::Index)]
pub struct SourceFileIndex(usize);

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct LinesWithHighlight<'a> {
    pub file: &'a FileName,
    pub first: LineWithHighlight<'a>,
    /// This is `None` if the last is the first line.
    pub last: Option<LineWithHighlight<'a>>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct LineWithHighlight<'a> {
    /// One-indexed line number.
    pub number: u32,
    /// The content of the entire line that contains the to-be-highlighted snippet.
    ///
    /// It may contain the whole snippet or only the starting or the ending part of it
    /// if the snippet spans multiple lines.
    pub content: &'a str,
    pub highlight: Highlight,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Highlight {
    pub start: u32,
    pub end: u32,
    pub width: usize,
    pub prefix_width: usize,
}

/// A source file.
///
/// Obtained by and contained within a [source map](SourceMap).
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct SourceFile {
    name: FileName,
    /// The component the source file belongs to, if any.
    // @Beacon @Task make this a plain String again!
    content: Arc<String>,
    span: Span,
    // @Task rename to owner & maybe make this a dyn* Any?
    #[cfg_attr(not(feature = "lsp"), allow(dead_code))]
    component: Option<ComponentIndex>,
}

impl SourceFile {
    /// Create a new source file.
    ///
    /// The [byte index](ByteIndex) `start` locates the file in a [source map](SourceMap).
    fn new(
        name: impl Into<FileName>,
        content: Arc<String>,
        start: ByteIndex,
        component: Option<ComponentIndex>,
    ) -> Self {
        Self {
            span: Span::with_length(start, content.len().try_into().unwrap()),
            name: name.into(),
            component,
            content,
        }
    }

    pub fn name(&self) -> &FileName {
        &self.name
    }

    pub fn component(&self) -> Option<ComponentIndex> {
        self.component
    }

    pub fn content(&self) -> &str {
        &self.content
    }

    pub fn local_span(&self) -> LocalSpan {
        self.span.local(self)
    }
}

impl Spanning for SourceFile {
    fn span(&self) -> Span {
        self.span
    }
}

impl std::ops::Index<LocalSpan> for SourceFile {
    type Output = str;

    fn index(&self, index: LocalSpan) -> &Self::Output {
        &self.content[Range::from(index)]
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum FileName {
    Anonymous,
    Stdin,
    Path(CanonicalPathBuf),
    Virtual(&'static str),
}

impl FileName {
    pub fn path(&self) -> Option<&CanonicalPath> {
        obtain!(self, Self::Path(path) => path)
    }
}

impl From<CanonicalPathBuf> for FileName {
    fn from(path: CanonicalPathBuf) -> Self {
        Self::Path(path)
    }
}

impl From<&'static str> for FileName {
    fn from(name: &'static str) -> Self {
        Self::Virtual(name)
    }
}
