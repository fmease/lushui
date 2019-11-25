use crate::lexer;
use crate::parser;
use std::fmt;
// use colored::Color;

use std::ops::RangeInclusive;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    // @Temporary constness
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn merge(self, other: Self) -> Self {
        Self {
            start: self.start,
            end: other.end,
        }
    }

    // because From is too general and breaks inference
    pub fn range(self) -> RangeInclusive<usize> {
        self.start..=self.end
    }
}

// @Task @Beacon @Beacon @Beacon remove this trait and replace all impls with Display
// @Note the idea is that types with special treatment of indentation (having even further indented elements)
// overwrite `display_indented_with` and implement `display_with` as `self.display_indented_with(0)`
// of course, this is bad design and we sure can do better
// next to the boilerplate-y impl of display_with, the code for display_indented_with needs to repeat the
// `" ".repeat(indentation * lexer::INDENTATION_IN_SPACES)` logic which breaks abstraction!
// @Note we could split this into `DisplayWithSource` and `DisplayIndentedWithSource` and use specialization to
// implement each in terms of each other (where of course, one impl wins to make it unambiguous)
// @Update removed the indent-logic *later*
// @Task define indentation-logic (it is inherently connected to displaying, obviously)

pub enum Error {
    Lex(lexer::Error),
    Parse(parser::Error),
}

impl Error {
    fn span(&self) -> &Span {
        match self {
            Self::Lex(error) => &error.span,
            Self::Parse(error) => &error.span,
        }
    }

    // @Note once we have more error types, we'll get to the point where
    // it might not make sense to display code
    // @Task don't print the whole line: set limit of 50~ characters (a window) @Note actually, don't: it complicates everything
    // and i am sure rustc doesn't do this either
    // @Question filename or filepath?
    pub fn display(&self, source: &str, filename: Option<&str>) -> String {
        let kind = match self {
            Self::Lex(error) => error.kind.to_string(),
            Self::Parse(error) => error.kind.to_string(),
        };
        let (start, _end, line, rel) = locations_and_line_from_span(source, self.span());
        format!(
            "{space} > {path}:{location}\n\
             {space} |\n\
             {space} | {source}\n\
             {space} | {underline_space}{underline}\n\
             {space} |\n\
             {space} = error: {message}\n\
             ",
            space = "",
            message = kind,
            path = filename.unwrap_or("<anonymous>"),
            location = start,
            source = &source[line.range()],
            underline_space = " ".repeat(rel.start),
            underline = "^".repeat(rel.end + 1 - rel.start)
        )
    }
}

impl From<lexer::Error> for Error {
    fn from(error: lexer::Error) -> Self {
        Self::Lex(error)
    }
}

impl From<parser::Error> for Error {
    fn from(error: parser::Error) -> Self {
        Self::Parse(error)
    }
}

struct Location {
    line: usize,
    column: usize,
}

impl Location {
    fn increment_line(&mut self) {
        self.line += 1;
        self.column = 1;
    }

    fn increment_column(&mut self) {
        self.column += 1;
    }
}

impl Default for Location {
    fn default() -> Self {
        Self { line: 1, column: 1 }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

// @Bug does not handle multiline spans
// @Question what concrete information returned to we really need?
// the `end` location is useless i think
fn locations_and_line_from_span(source: &str, span: &Span) -> (Location, Location, Span, Span) {
    let mut start = Location::default();
    let mut end = Location::default();
    let mut found_start = false;
    let mut found_end = false;
    let mut index_line_start = 0;
    let mut index_line_end = index_line_start;
    let mut relative_start = 0;
    let mut relative_end = 0;

    for (index, character) in source.char_indices() {
        if index == span.start {
            found_start = true;
        }
        if index == span.end {
            found_end = true;
        }

        index_line_end = index;

        if character == '\n' {
            if found_start && found_end {
                break;
            }

            index_line_start = index + 1;
            index_line_end = index_line_start;

            if !found_start {
                start.increment_line();
                relative_start = 0;
            }
            if !found_end {
                end.increment_line();
                relative_end = 0; // @Question ????
            }
        } else {
            if !found_start {
                start.increment_column();
                relative_start += character.len_utf8();
            }
            if !found_end {
                end.increment_column();
                relative_end += character.len_utf8();
            }
        }
    }

    // @Bug we should not need to decr by 1
    // @Note the saturating_sub is just a hotfix, remove the whole thing nonetheless!
    (
        start,
        end,
        Span::new(index_line_start, index_line_end.saturating_sub(1)),
        Span::new(relative_start, relative_end),
    )
}
