//! The diagnostic system.

use crate::{
    span::{SourceMap, Span, Spanning},
    HashSet,
};
use std::{
    borrow::Cow,
    ops::{Deref, DerefMut},
};

type CowStr = Cow<'static, str>;

pub type Result<T, E = Diagnostic> = std::result::Result<T, E>;
// @Question bad name?
pub type Results<T> = Result<T, Diagnostics>;

pub type Diagnostics = HashSet<Diagnostic>;

// @Question is this indirection actually worth it?
#[derive(Hash, PartialEq, Eq)]
pub struct Diagnostic(Box<RawDiagnostic>);

impl Deref for Diagnostic {
    type Target = RawDiagnostic;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Diagnostic {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Hash, PartialEq, Eq, Debug)]
pub struct RawDiagnostic {
    level: Level,
    message: Option<CowStr>,
    code: Option<Code>,
    highlights: Vec<Highlight>,
    subdiagnostics: Vec<Subdiagnostic>,
}
impl Diagnostic {
    pub fn new(level: Level) -> Self {
        Self(Box::new(RawDiagnostic {
            level,
            code: None,
            message: None,
            highlights: Vec::new(),
            subdiagnostics: Vec::new(),
        }))
    }

    pub fn bug() -> Self {
        Self::new(Level::Bug)
    }

    pub fn error() -> Self {
        Self::new(Level::Error)
    }

    pub fn warning() -> Self {
        Self::new(Level::Warning)
    }

    pub fn with_message(mut self, message: impl Into<CowStr>) -> Self {
        self.message = Some(message.into());
        self
    }

    pub fn with_code(mut self, code: Code) -> Self {
        self.code = Some(code);
        self
    }

    pub fn with_span(mut self, spanning: &impl Spanning) -> Self {
        let role = self.choose_role();
        let span = spanning.span();

        self.check_span(span);
        self.highlights.push(Highlight {
            span,
            label: None,
            role,
        });
        self
    }

    pub fn with_labeled_span(mut self, spanning: &impl Spanning, label: impl Into<CowStr>) -> Self {
        let role = self.choose_role();
        let span = spanning.span();

        self.check_span(span);
        self.highlights.push(Highlight {
            span,
            label: Some(label.into()),
            role,
        });
        self
    }

    /// Verify that a span is not a sham.
    fn check_span(&self, span: Span) {
        #[cfg(debug_assertions)]
        if span == Span::SHAM {
            panic!("sham span added to diagnostic {:#?}", **self);
        }
    }

    pub fn with_note(mut self, message: impl Into<CowStr>) -> Self {
        self.subdiagnostics.push(Subdiagnostic {
            kind: SubdiagnosticKind::Note,
            message: message.into(),
        });
        self
    }

    pub fn with_help(mut self, message: impl Into<CowStr>) -> Self {
        self.subdiagnostics.push(Subdiagnostic {
            kind: SubdiagnosticKind::Help,
            message: message.into(),
        });
        self
    }

    pub fn when(self, condition: bool, builder: impl FnOnce(Self) -> Self) -> Self {
        match condition {
            true => builder(self),
            false => self,
        }
    }

    pub fn when_some<T>(self, value: Option<T>, builder: impl FnOnce(Self, T) -> Self) -> Self {
        match value {
            Some(value) => builder(self, value),
            None => self,
        }
    }

    fn choose_role(&self) -> Role {
        if self.highlights.is_empty() {
            Role::Primary
        } else {
            Role::Secondary
        }
    }

    pub fn spans<'a>(&'a self) -> Vec<Span> {
        let mut spans: Vec<_> = self
            .highlights
            .iter()
            .map(|highlight| highlight.span)
            .collect();
        spans.sort();
        spans
    }

    pub fn emit(mut self, map: Option<&SourceMap>) {
        eprintln!("{}", self.display(map));
        eprintln!();
    }

    // @Task if the span equals the span of the entire file, don't output its content
    // @Bug cannot nicely handle non-primary highlights if they have sham locations (currently skipped)
    fn display(&mut self, map: Option<&SourceMap>) -> String {
        let mut header = format!(
            "{:#}{}",
            self.level,
            self.code
                .map(|code| format!("[{:?}]", code).color(self.level.color()))
                .unwrap_or_default(),
        );

        if let Some(message) = &self.message {
            header += &format!(": {}", message.bright_white().bold());
        }

        self.highlights.sort_by_key(|highlight| highlight.span);

        let mut message = header;

        if !self.highlights.is_empty() {
            let primary_highlight = self
                .highlights
                .iter()
                .position(|highlight| highlight.role == Role::Primary)
                .unwrap();

            let map = map.unwrap();

            let resolved_spans: Vec<_> = self
                .highlights
                .iter()
                .map(|highlight| map.resolve_span(highlight.span))
                .collect();

            let primary_span = &resolved_spans[primary_highlight];

            let largest_line_number = resolved_spans
                .iter()
                .map(|span| span.first_line.number)
                .max()
                .unwrap() as usize;

            let padding_len = largest_line_number.to_string().len();
            let padding = " ".repeat(padding_len);
            let bar = "|".bright_blue().bold();

            message += &format!(
                "\n\
                {padding} {arrow} {file}:{line}:{column}",
                arrow = ">".bright_blue().bold(),
                file = primary_span.path.to_string_lossy(),
                line = primary_span.first_line.number,
                column = primary_span.first_line.highlight_start_column,
                padding = padding,
            );

            for (highlight, span) in self.highlights.iter().zip(&resolved_spans) {
                // @Question should we really compare 2 `Path`s here? isn't it more robust to
                // compare SourceFile indices?
                if span.path != primary_span.path && highlight.role != Role::Primary {
                    message += &format!(
                        "\n\
                        {padding} {bar}\n\
                        {padding} {arrow} {file}",
                        arrow = "~".bright_blue().bold(),
                        bar = bar,
                        file = span.path.to_string_lossy(),
                        padding = padding,
                    );
                }

                let role_color = highlight.role.color(self.level.color());

                match &span.final_line {
                    // the snippet spans a single line
                    None => {
                        message += &format!(
                            "\n\
                            {padding} {bar}\n\
                            {line:>padding_len$} {bar} {snippet}\
                            {padding} {bar} {highlight_padding}{highlight} {label}",
                            line = span.first_line.number,
                            snippet = span.first_line.content,
                            padding = padding,
                            padding_len = padding_len,
                            highlight_padding = " ".repeat(span.first_line.highlight_prefix_width),
                            highlight = highlight
                                .role
                                .symbol()
                                .repeat(span.first_line.highlight_width)
                                .color(role_color)
                                .bold(),
                            label = highlight
                                .label
                                .as_ref()
                                .map(|label| label.color(role_color))
                                .unwrap_or_default(),
                            bar = bar
                        );
                    }
                    // the snippet spans multiple lines
                    Some(final_line) => {
                        let ellipsis = if final_line.number - span.first_line.number > 1 {
                            format!(
                                "{ellipsis:<padding_len$}",
                                ellipsis = "...",
                                padding_len = padding_len
                            )
                        } else {
                            format!("{padding} {bar}", padding = padding, bar = bar)
                        };
                        // the upper arm
                        message += &format!(
                            "\n\
                            {padding} {bar}\n\
                            {line:>padding_len$} {bar}   {snippet}\
                            {ellipsis:>padding_len$}  {highlight_horizontal_arm}{highlight_hand}\n",
                            line = span.first_line.number,
                            snippet = span.first_line.content,
                            padding = padding,
                            padding_len = padding_len,
                            ellipsis = ellipsis,
                            highlight_horizontal_arm = "_"
                                .repeat(span.first_line.highlight_prefix_width + 1)
                                .color(role_color)
                                .bold(),
                            // the hand is currently not dependent on the Unicode width of the first character
                            highlight_hand = highlight.role.symbol().color(role_color).bold(),
                            bar = bar
                        );
                        // the connector and the lower arm
                        message += &format!(
                            "{line:>padding_len$} {bar} {highlight_vertical_arm} {snippet}\
                            {padding} {bar} {highlight_vertical_arm}{highlight_horizontal_arm}{highlight_hand} {label}",
                            line = final_line.number,
                            snippet = final_line.content,
                            padding = padding,
                            padding_len = padding_len,
                            // the arm is currently not dependent on the Unicode width of the last character
                            highlight_horizontal_arm = "_"
                                .repeat(final_line.highlight_width)
                                .color(role_color)
                                .bold(),
                            highlight_vertical_arm = "|".color(role_color).bold(),
                            // the hand is currently not dependent on the Unicode width of the 1st character
                            highlight_hand = highlight.role.symbol().color(role_color).bold(),
                            label = highlight
                                .label
                                .as_ref()
                                .map(|label| label.color(role_color))
                                .unwrap_or_default(),
                            bar = bar
                        );
                    }
                }
            }
        }

        for subdiagnostic in &self.subdiagnostics {
            message += &format!("\n{}", subdiagnostic);
        }

        message
    }
}

#[derive(Hash, PartialEq, Eq, Debug)]
struct Subdiagnostic {
    kind: SubdiagnosticKind,
    message: CowStr,
}

impl fmt::Display for Subdiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.kind, self.message)
    }
}

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
enum SubdiagnosticKind {
    Note,
    Help,
}

impl SubdiagnosticKind {
    const fn to_str(self) -> &'static str {
        match self {
            Self::Note => "note",
            Self::Help => "help",
        }
    }
}

impl fmt::Display for SubdiagnosticKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_str().bright_blue().bold())
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum Level {
    Bug,
    Error,
    Warning,
}

use colored::{Color, Colorize};

impl Level {
    const fn to_str(self) -> &'static str {
        match self {
            Self::Bug => "internal compiler error",
            Self::Error => "error",
            Self::Warning => "warning",
        }
    }

    const fn color(self) -> Color {
        match self {
            Self::Bug | Self::Error => Color::BrightRed,
            Self::Warning => Color::BrightYellow,
        }
    }
}

use std::fmt;

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !f.alternate() {
            write!(f, "{}", self.to_str())
        } else {
            write!(f, "{}", self.to_str().color(self.color()).bold())
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
struct Highlight {
    span: Span,
    role: Role,
    label: Option<CowStr>,
}

// @Note multiple primaries don't merge right now but have undefined behavior/should be an error
// @Note we have this design because we want to ergonomically sort by span (primary is not necessarily
// the first to be previewed)
#[derive(PartialEq, Eq, Hash, Debug)]
enum Role {
    Primary,
    Secondary,
}

impl Role {
    const fn color(&self, primary: Color) -> Color {
        match self {
            Self::Primary => primary,
            Self::Secondary => Color::BrightBlue,
        }
    }

    const fn symbol(&self) -> &'static str {
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
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
#[forbid(missing_docs)]
pub enum Code {
    /// Unbalanced (round) brackets.
    E001,
    /// Trailing dash on identifier.
    E002,
    /// Invalid indentation.
    E003,
    /// Unterminated text literal.
    E004,
    /// Unreadable number literal.
    E005,
    /// Invalid number literal suffix.
    E006,
    /// Number literal does not fit type.
    E007,
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
    /// Missing mandatory type annotations.
    E015,
    /// Unable to load external module.
    E016,
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
    /// Bare use of crate or super.
    E025,
    /// Crate or super inside nested path.
    E026,
    /// Missing type annotation for lambda literal parameter or pattern.
    E030,
    /// Illegal function application.
    E031,
    /// Type mismatch.
    E032,
    /// Invalid constructor.
    E033,
    /// Invalid position of binder in pattern.
    E034,
    /// Type analysis
    E035,
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
    /// Implicitness unimplemented.
    W001,
}

impl Code {
    // @Task
    #[allow(dead_code)]
    pub const fn explain(self) -> &'static str {
        loop {}
    }
}
