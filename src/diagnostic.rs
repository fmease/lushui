//! The diagnostic system.

use crate::span::{SourceMap, Span, Spanning};

type CowStr = std::borrow::Cow<'static, str>;

pub type Result<T, E = Diagnostic> = std::result::Result<T, E>;

pub use std::collections::HashSet as Bag;

pub type Diagnostics = Bag<Diagnostic>;

#[derive(Hash, PartialEq, Eq)]
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
#[derive(Hash, PartialEq, Eq)]
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

    pub fn with_span(mut self, spanning: &impl Spanning) -> Self {
        let role = self.choose_role();

        self.highlights.push(Highlight {
            span: spanning.span(),
            label: None,
            role,
        });
        self
    }

    pub fn with_labeled_span(mut self, spanning: &impl Spanning, label: impl Into<CowStr>) -> Self {
        let role = self.choose_role();

        self.highlights.push(Highlight {
            span: spanning.span(),
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
                .map(|span| span.first_line.number)
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
                line = primary_span.first_line.number,
                column = primary_span.first_line.highlight_start_column,
                padding = padding,
            );

            for (highlight, span) in self.highlights.iter().zip(&resolved_spans) {
                if &span.filename != primary_file_name && highlight.role != Role::Primary {
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
                                "{ellipsis:<padding_len$} ",
                                ellipsis = "...",
                                padding_len = padding_len
                            )
                        } else {
                            format!("{padding} {bar}", padding = padding, bar = bar)
                        };
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

        message
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
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

#[derive(PartialEq, Eq, Hash)]
struct Highlight {
    span: Span,
    role: Role,
    label: Option<CowStr>,
}

// @Note multiple primaries don't merge right now but have undefined behavior/should be an error
// @Note we have this design because we want to ergonomically sort by span (primary is not necessarily
// the first to be previewed)
#[derive(PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
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
    /// Missing mandatory type annotations.
    E015,
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
    /// Implicitness unimplemented.
    W001,
}
