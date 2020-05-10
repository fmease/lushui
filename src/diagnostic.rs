//! The diagnostic system.
//!
//! ## Issues
//!
//! * does not support subdiagnostics yet
//! * does not support multiline spans
//! * cannot correctly align code when it's from different files
//! * does not feature error handling abstractions like diagnostic buffers

use unicode_width::UnicodeWidthStr;

use crate::span::{SourceMap, Span};

type CowStr = std::borrow::Cow<'static, str>;

pub type Result<T, E = Diagnostic> = std::result::Result<T, E>;

pub type Diagnostics = Vec<Diagnostic>;

pub struct Diagnostic {
    inner: Box<InnerDiagnostic>,
}

// @Note the design of the diagnostic system is still not set.
// one big question: subdiagnostics: when, how?
struct InnerDiagnostic {
    level: Level,
    message: CowStr,
    code: Option<Code>,
    spans: Vec<EnrichedSpan>,
}

// @Task be able to have errors associated with a file but not a snippet
// @Note I still want to rely on `Span`
impl Diagnostic {
    pub fn new(level: Level, code: impl Into<Option<Code>>, message: impl Into<CowStr>) -> Self {
        Self {
            inner: Box::new(InnerDiagnostic {
                level,
                code: code.into(),
                message: message.into(),
                spans: Vec::new(),
            }),
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.inner.spans.push(EnrichedSpan {
            span,
            label: None,
            role: self.choose_role(),
        });
        self
    }

    pub fn with_labeled_span(mut self, span: Span, label: impl Into<CowStr>) -> Self {
        self.inner.spans.push(EnrichedSpan {
            span,
            label: Some(label.into()),
            role: self.choose_role(),
        });
        self
    }

    fn choose_role(&self) -> Role {
        if self.inner.spans.is_empty() {
            Role::Primary
        } else {
            Role::Secondary
        }
    }

    pub fn emit(mut self, map: Option<&SourceMap>) {
        eprintln!("{}", self.display(map));
        eprintln!();
    }

    // @Task handle multiline spans (needs support from crate::span)
    // @Task if the span equals the span of the entire file, don't output its content
    // @Task if two spans reside on the same line, print them inline not above each other (maybe)
    // @Bug file number padding does not work if span are from different files
    fn display(&mut self, map: Option<&SourceMap>) -> String {
        let header = format!(
            "{:#}{}: {}",
            self.inner.level,
            self.inner
                .code
                .map(|code| format!("[{:?}]", code).color(self.inner.level.color()))
                .unwrap_or_default(),
            self.inner.message.bright_white().bold()
        );
        self.inner.spans.sort_unstable_by_key(|span| span.span);

        let mut message = header;

        if let Some(span) = self
            .inner
            .spans
            .iter()
            .find(|span| span.role == Role::Primary)
        {
            if span.span == Span::SHAM {
                message += &format!("\n {arrow} ??? ??? ???", arrow = ">".bright_blue().bold());
                return message;
            }

            let map = map.unwrap();
            let lines = map.resolve_span(span.span);
            let line_number = lines.first.number.to_string();
            let padding = " ".repeat(line_number.len());

            message += &format!(
                "\n{padding} {arrow} {file}:{line}:{column}",
                arrow = ">".bright_blue().bold(),
                file = lines.filename,
                line = line_number,
                column = lines.first.highlight.start() + 1,
                padding = padding,
            );

            let primary_span = span;
            let mut primary_lines = Some(lines);

            for span in &self.inner.spans {
                message.push_str(&self.display_preview(
                    if span == primary_span {
                        primary_lines.take().unwrap()
                    } else {
                        map.resolve_span(span.span)
                    },
                    span,
                ));
            }
        }

        message
    }

    fn display_preview(&self, lines: crate::span::Lines, span: &EnrichedSpan) -> String {
        let line_number = lines.first.number.to_string();
        let padding = " ".repeat(line_number.len());
        let highlight = lines.first.highlight;
        format!(
            "\n\
            {padding} {bar}\n\
            {line} {bar} {snippet}{padding} {bar} {highlight_padding}{highlight} {label}",
            line = line_number,
            snippet = lines.first.content,
            padding = padding,
            highlight_padding = " ".repeat(lines.first.content[..*highlight.start()].width()),
            highlight = span
                .role
                .symbol()
                .repeat(lines.first.content[highlight].width())
                .color(span.role.color(self.inner.level.color()))
                .bold(),
            label = span
                .label
                .as_ref()
                .map(|label| label.color(span.role.color(self.inner.level.color())))
                .unwrap_or_default(),
            bar = "|".bright_blue().bold()
        )
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
struct EnrichedSpan {
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
