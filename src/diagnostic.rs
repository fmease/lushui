//! The diagnostic system.
//!
//! ## Issues
//!
//! * does not support subdiagnostics yet
//! * does not support multiline spans
//! * does not feature error handling abstractions like diagnostic buffers

use crate::span::{SourceMap, Span};

type CowStr = std::borrow::Cow<'static, str>;

pub type Result<T, E = Diagnostic> = std::result::Result<T, E>;

pub type Diagnostics = Vec<Diagnostic>;

pub struct Diagnostic {
    raw: Box<RawDiagnostic>,
}

impl std::ops::Deref for Diagnostic {
    type Target = RawDiagnostic;

    fn deref(&self) -> &Self::Target {
        &self.raw
    }
}

impl std::ops::DerefMut for Diagnostic {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.raw
    }
}

// @Note the design of the diagnostic system is still not set.
// one big question: subdiagnostics: when, how?
pub struct RawDiagnostic {
    level: Level,
    message: CowStr,
    code: Option<Code>,
    highlights: Vec<Highlight>,
}

// @Task be able to have errors associated with a file but not a snippet
// @Note I still want to rely on `Span`
impl Diagnostic {
    pub fn new(level: Level, code: impl Into<Option<Code>>, message: impl Into<CowStr>) -> Self {
        Self {
            raw: Box::new(RawDiagnostic {
                level,
                code: code.into(),
                message: message.into(),
                highlights: Vec::new(),
            }),
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        let role = self.choose_role();

        self.highlights.push(Highlight {
            span,
            label: None,
            role,
        });
        self
    }

    pub fn with_labeled_span(mut self, span: Span, label: impl Into<CowStr>) -> Self {
        let role = self.choose_role();

        self.highlights.push(Highlight {
            span,
            label: Some(label.into()),
            role,
        });
        self
    }

    fn choose_role(&self) -> Role {
        if self.highlights.is_empty() {
            Role::Primary
        } else {
            Role::Secondary
        }
    }

    pub fn spans<'a>(&'a self) -> Vec<Span> {
        self.highlights
            .iter()
            .map(|highlight| highlight.span)
            .collect()
    }

    pub fn emit(mut self, map: Option<&SourceMap>) {
        eprintln!("{}", self.display(map));
        eprintln!();
    }

    // @Task handle multiline spans (needs support from crate::span)
    // @Task if the span equals the span of the entire file, don't output its content
    // @Task if two spans reside on the same line, print them inline not above each other (maybe)
    fn display(&mut self, map: Option<&SourceMap>) -> String {
        let header = format!(
            "{:#}{}: {}",
            self.level,
            self.code
                .map(|code| format!("[{:?}]", code).color(self.raw.level.color()))
                .unwrap_or_default(),
            self.message.bright_white().bold()
        );
        self.highlights
            .sort_unstable_by_key(|highlight| highlight.span);

        let mut message = header;

        if !self.highlights.is_empty() {
            let primary_highlight = self
                .highlights
                .iter()
                .position(|highlight| highlight.role == Role::Primary)
                .unwrap();

            if self.highlights[primary_highlight].span == Span::SHAM {
                message += &format!("\n {arrow} ??? ??? ???", arrow = ">".bright_blue().bold());
                return message;
            }

            let map = map.unwrap();

            let resolved_spans: Vec<_> = self
                .highlights
                .iter()
                .map(|highlight| map.resolve_span(highlight.span))
                .collect();

            let primary_file_name = &resolved_spans[primary_highlight].filename;

            let largest_line_number = resolved_spans
                .iter()
                .map(|span| span.first.number)
                .max()
                .unwrap() as usize;

            let padding_len = largest_line_number.to_string().len();
            let padding = " ".repeat(padding_len);
            let bar = "|".bright_blue().bold();

            let primary_span = &resolved_spans[primary_highlight];

            message += &format!(
                "\n\
                {padding} {arrow} {file}:{line}:{column}",
                arrow = ">".bright_blue().bold(),
                file = primary_span.filename,
                line = primary_span.first.number,
                column = primary_span.first.highlight.start() + 1,
                padding = padding,
            );

            for (highlight, span) in self.highlights.iter().zip(&resolved_spans) {
                if highlight.role != Role::Primary && &span.filename != primary_file_name {
                    message += &format!(
                        "\n\
                        {padding} {bar}\n\
                        {padding} {arrow} {file}",
                        arrow = "~".bright_blue().bold(),
                        bar = bar,
                        file = span.filename,
                        padding = padding,
                    );
                }

                message += &format!(
                    "\n\
                    {padding} {bar}\n\
                    {line:>padding_len$} {bar} {snippet}\
                    {padding} {bar} {highlight_padding}{highlight} {label}",
                    line = span.first.number,
                    snippet = span.first.content,
                    padding = padding,
                    padding_len = padding_len,
                    highlight_padding = " ".repeat(span.first.highlight_prefix_width()),
                    highlight = highlight
                        .role
                        .symbol()
                        .repeat(span.first.highlight_width())
                        .color(highlight.role.color(self.level.color()))
                        .bold(),
                    label = highlight
                        .label
                        .as_ref()
                        .map(|label| label.color(highlight.role.color(self.level.color())))
                        .unwrap_or_default(),
                    bar = bar
                );
            }
        }

        message
    }
}

#[derive(Clone, Copy)]
pub enum Level {
    Bug,
    Fatal,
    Error,
    Warning,
    Note,
    Help,
}

use colored::{Color, Colorize};

impl Level {
    fn to_str(self) -> &'static str {
        match self {
            Self::Bug => "internal compiler error",
            Self::Fatal | Self::Error => "error",
            Self::Warning => "warning",
            Self::Note => "note",
            Self::Help => "help",
        }
    }

    fn color(self) -> Color {
        match self {
            Self::Bug | Self::Fatal | Self::Error => Color::BrightRed,
            Self::Warning => Color::BrightYellow,
            Self::Note | Self::Help => Color::BrightBlue,
        }
    }
}

use std::fmt;

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !f.alternate() {
            f.write_str(self.to_str())
        } else {
            write!(f, "{}", self.to_str().color(self.color()).bold())
        }
    }
}

#[derive(PartialEq, Eq)]
struct Highlight {
    span: Span,
    role: Role,
    label: Option<CowStr>,
}

// @Note multiple primaries don't merge right now but have undefined behavior/should be an error
// @Note we have this design because we want to ergonomically sort by span (primary is not necessarily
// the first to be previewed)
#[derive(PartialEq, Eq)]
enum Role {
    Primary,
    Secondary,
}

impl Role {
    fn color(&self, primary: Color) -> Color {
        match self {
            Self::Primary => primary,
            Self::Secondary => Color::BrightBlue,
        }
    }

    fn symbol(&self) -> &'static str {
        match self {
            Self::Primary => "^",
            Self::Secondary => "-",
        }
    }
}

/// Diagnostic code.
///
/// Used for language-related error in contrast to errors emitted because of
/// faulty interactions with the CLI.
#[derive(Debug, Clone, Copy)]
#[forbid(missing_docs)]
pub enum Code {
    /// Illegal character encountered.
    E000,
    /// Unbalanced (round) brackets.
    E001,
    /// Trailing dash on identifier.
    E002,
    /// Invalid indentation.
    E003,
    /// Unterminated text literal.
    E004,
    /// Unexpected token.
    E010,
    /// Undefined attribute.
    E011,
    /// Definitionless declaration.
    E012,
    /// Illegal attribute target.
    E013,
    /// Mutually exclusive attributes.
    E014,
    /// Duplicate definitions.
    E020,
    /// Undefined binding.
    E021,
    /// Value used as a module.
    E022,
    /// Module used as a value.
    E023,
    /// Circular declaration.
    E024,
    /// Missing type annotation for lambda literal parameter or pattern.
    E030,
    /// Illegal function application.
    E031,
    /// Type mismatch.
    E032,
    /// Invalid constructor.
    E033,
    /// Missing program entry.
    E050,
    /// Unregistered foreign binding.
    E060,
    /// Foreign type not declared.
    E061,
    /// Invalid inherent type.
    E062,
    /// Inherent type not declared.
    E063,
    /// Contracted case analysis cases buggy.
    W000,
    /// Implicitness unimplemented.
    W001,
}
