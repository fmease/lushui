use super::{ByteIndex, LocalByteIndex, LocalSpan, Span, Spanning};
use index_map::IndexMap;
use std::{
    borrow::Borrow,
    default::default,
    io,
    ops::Range,
    path::{Path, PathBuf},
};
use unicode_width::UnicodeWidthStr;

#[cfg(test)]
mod test;

/// A mapping from [index](SourceFileIndex) to [source file](SourceFile).
///
/// Most prominently, the index – an offset and obtained by adding a source file to this map –
/// is the key component to define [`Span`] (via [`ByteIndex`]).
///
/// The source files are laid out next to each other and padded on their left (at their start)
/// by one byte (in the sense of `Span::length(_) == 1`) to reserve space for _end of input_
/// pseudo tokens (e.g. [`EndOfInput`][eoi]) and the _end of input_ virtual location.  
/// Additionally, this frees up the byte index `0` and allows the [default `Span`](Span::default)
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
///
/// [eoi]: crate::syntax::token::TokenKind::EndOfInput
#[derive(Default)]
pub struct SourceMap {
    files: IndexMap<SourceFileIndex, SourceFile>,
}

impl SourceMap {
    fn next_offset(&self) -> ByteIndex {
        // + 1 for the padding
        self.files
            .last()
            .map(|file| file.span().end)
            .unwrap_or_default()
            + 1
    }

    /// Open a file given its path and add it as a [`SourceFile`] to the map.
    pub fn load(&mut self, path: PathBuf) -> Result<SourceFileIndex, io::Error> {
        let source = std::fs::read_to_string(&path)?;
        Ok(self.add(Some(path), source))
    }

    /// Add text to the map creating a [`SourceFile`] in the process.
    pub(crate) fn add(&mut self, path: Option<PathBuf>, source: String) -> SourceFileIndex {
        self.files
            .insert(SourceFile::new(path, source, self.next_offset()))
    }

    pub(crate) fn file(&self, span: Span) -> &SourceFile {
        debug_assert!(span != default());

        // @Task do binary search (by span)
        self.files
            .values()
            .find(|file| file.span().contains(span.start))
            .unwrap()
    }

    /// Resolve a span to the string content it points to.
    ///
    /// This treats line breaks verbatim.
    pub(crate) fn snippet(&self, span: Span) -> &str {
        let file = self.file(span);
        let span = span.local(file);
        &file[span]
    }

    /// Resolve a span to various information useful for highlighting.
    // @Task update docs
    // @Task better name
    pub(crate) fn lines(&self, span: Span) -> Lines<'_> {
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
                    current_line.highlight = Some(Highlight {
                        start: index,
                        end: None,
                    });
                }
            }

            if index == span.start {
                current_line.highlight = Some(Highlight {
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
                current_line.highlight = Some(Highlight {
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
            number: u32,
            start: Option<LocalByteIndex>,
            end: Option<LocalByteIndex>,
            highlight: Option<Highlight>,
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

            fn resolve(self, file: &SourceFile) -> Option<Line<'_>> {
                let start = self.start?;
                let highlight = self.highlight?;

                let end = self.end?;

                Some(Line {
                    number: self.number,
                    content: &file[LocalSpan::new(start, end)],
                    highlight_width: file[LocalSpan::new(highlight.start, highlight.end?)].width(),
                    highlight_padding_width: match start <= highlight.start {
                        true => file[LocalSpan::new(start, highlight.start)].width(),
                        false => 0,
                    },
                    // @Beacon @Beacon @Beacon @Bug this uses #bytes but it should use #chars
                    // @Question or should it use graphemes??
                    // highlight_start_column: usize::from(highlight.start + 1 - start),
                    // @Temporary
                    highlight_start_column: match start <= highlight.start {
                        true => (highlight.start - start + 1).into(),
                        false => 0,
                    },
                })
            }
        }

        struct Highlight {
            start: LocalByteIndex,
            end: Option<LocalByteIndex>,
        }

        Lines {
            path: file.path.as_deref(),
            first_line: first_line.unwrap().resolve(file).unwrap(),
            last_line: last_line.map(|line| line.resolve(file).unwrap()),
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
#[cfg_attr(test, derive(PartialEq, Eq))]
pub(crate) struct Lines<'a> {
    pub(crate) path: Option<&'a Path>,
    pub(crate) first_line: Line<'a>,
    /// This is `None` if the last is the first line.
    pub(crate) last_line: Option<Line<'a>>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub(crate) struct Line<'a> {
    pub(crate) number: u32,
    /// The content of the entire line that contains the to-be-highlighted snippet.
    ///
    /// It may contain the whole snippet or only the starting or the ending part of it
    /// if the snippet spans multiple lines.
    pub(crate) content: &'a str,
    /// The Unicode width of the to-be-highlighted snippet.
    pub(crate) highlight_width: usize,
    // @Beacon @Beacon @Beacon @Task docs
    pub(crate) highlight_padding_width: usize,
    // @Beacon @Beacon @Beacon @Bug this uses #bytes but it should use #chars
    pub(crate) highlight_start_column: usize,
}

/// A source file.
///
/// Obtained by and contained within a [source map](SourceMap).
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct SourceFile {
    path: Option<PathBuf>,
    content: String,
    span: Span,
}

impl SourceFile {
    /// Create a new source file.
    ///
    /// The [byte index](ByteIndex) `start` locates the file in a [source map](SourceMap).
    pub(crate) fn new(path: Option<PathBuf>, content: String, start: ByteIndex) -> Self {
        Self {
            span: Span::with_length(start, content.len().try_into().unwrap()),
            path,
            content,
        }
    }

    pub(crate) fn path(&self) -> Option<&Path> {
        self.path.as_deref()
    }

    pub(crate) fn content(&self) -> &str {
        &self.content
    }

    pub(crate) fn local_span(&self) -> LocalSpan {
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
