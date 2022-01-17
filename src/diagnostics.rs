//! The diagnostics system.
//!
//! # Unimplemented Features
//!
//! * (maybe) subdiagnostics with a span
//! * emitting JSON
//! * display style: rich (current system) <-> short
//! * a rust script (in /misc) that finds the lowest [Code] that can be used
//!   as well as any unused error codes (searching src/)
//! * unit tests for the formatter
//! * unindenting long lines of highlighted source code (i.e. mapping initial whitespace to
//!   a single one)
//! * warning API/manager which can understand lushui's allows/denies/… directives
//!
//! # Issues
//!
//! * diagnostics with primary and secondary spans are not *that* readable because of
//!   all those lengthy paths. That didn't use to be the case, maybe we should some
//!   rules when the paths can be omitted
//! * cannot handle overly long lines of highlighted code (does not look tidy anymore)

use crate::{
    span::{SourceMap, Span, Spanning},
    utility::Str,
};
use colored::{Color, Colorize};
pub use reporter::Reporter;
use std::{
    borrow::Cow,
    collections::BTreeSet,
    fmt::{self, Debug},
    iter::once,
    path::Path,
};
use unicode_width::UnicodeWidthStr;

pub mod reporter;
#[cfg(test)]
mod test;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct UnboxedDiagnostic {
    highlights: BTreeSet<Highlight>,
    subdiagnostics: Vec<Subdiagnostic>,
    severity: Severity,
    code: Option<Code>,
    message: Option<Str>,
}

/// A complex error message optionally with source location information.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
#[must_use]
pub struct Diagnostic(Box<UnboxedDiagnostic>);

impl Diagnostic {
    /// Create a bare-bones diagnostic with a certain level of severity.
    fn new(severity: Severity) -> Self {
        Self(Box::new(UnboxedDiagnostic {
            severity,
            code: None,
            message: None,
            highlights: BTreeSet::new(),
            subdiagnostics: Vec::new(),
        }))
    }

    /// Create a diagnostic for an internal compiler error (ICE).
    pub fn bug() -> Self {
        Self::new(Severity::Bug)
    }

    /// Create a diagnostic for a user error.
    pub fn error() -> Self {
        Self::new(Severity::Error)
    }

    /// Create a diagnostic for a warning.
    pub fn warning() -> Self {
        Self::new(Severity::Warning)
    }

    /// Create a diagnostic for an internal debugging message.
    pub fn debug() -> Self {
        Self::new(Severity::Debug)
    }

    /// Create a diagnostic for an unimplemented language feature.
    pub(crate) fn unimplemented(message: impl Into<Str>) -> Self {
        Self::error().message(format!("{} not supported yet", message.into()))
    }

    /// Add an (error) code to the diagnostic.
    pub fn code(mut self, code: Code) -> Self {
        self.0.code = Some(code);
        self
    }

    /// Add a text message describing the issue.
    ///
    /// # Strict Guidelines
    ///
    /// * No line breaks
    /// * Do not start the message with an upper case letter
    /// * Single sentence only without a final period or exclamation mark
    /// * When quoting code, surround it with backticks
    /// * Try not to quote complex expressions (the error messages for
    ///   type mismatches disobey this rule right now, this needs to change)
    /// * The message should be able to stand on its own without the additional
    ///   information provided by labels and subdiagnostics
    pub fn message(mut self, message: impl Into<Str>) -> Self {
        self.0.message = Some(message.into());
        self
    }

    fn span(mut self, spanning: impl Spanning, label: Option<Str>, role: Role) -> Self {
        self.0.highlights.insert(Highlight {
            span: spanning.span(),
            label: label.map(Into::into),
            role,
        });
        self
    }

    /// Reference a code snippet as one of the focal points of the diagnostic.
    pub(crate) fn primary_span(self, spanning: impl Spanning) -> Self {
        self.span(spanning, None, Role::Primary)
    }

    /// Reference and label a code snippet as one of the focal points of the diagnostic.
    pub(crate) fn labeled_primary_span(
        self,
        spanning: impl Spanning,
        label: impl Into<Str>,
    ) -> Self {
        self.span(spanning, Some(label.into()), Role::Primary)
    }

    /// Reference a code snippet as auxiliary information for the diagnostic.
    pub(crate) fn secondary_span(self, spanning: impl Spanning) -> Self {
        self.span(spanning, None, Role::Secondary)
    }

    /// Reference and label a code snippet as auxiliary information for the diagnostic.
    pub(crate) fn labeled_secondary_span(
        self,
        spanning: impl Spanning,
        label: impl Into<Str>,
    ) -> Self {
        self.span(spanning, Some(label.into()), Role::Secondary)
    }

    fn spans<I>(mut self, spannings: I, label: Option<Str>, role: Role) -> Self
    where
        I: Iterator<Item: Spanning>,
    {
        self.0
            .highlights
            .extend(spannings.map(|spanning| Highlight {
                span: spanning.span(),
                label: label.clone(),
                role,
            }));
        self
    }

    /// Reference several equally important code snippets.
    pub(crate) fn primary_spans<I>(self, spannings: I) -> Self
    where
        I: Iterator<Item: Spanning>,
    {
        self.spans(spannings, None, Role::Primary)
    }

    /// Reference and label several very and equally important code snippets.
    pub(crate) fn labeled_primary_spans<I>(self, spannings: I, label: impl Into<Str>) -> Self
    where
        I: Iterator<Item: Spanning>,
    {
        self.spans(spannings, Some(label.into()), Role::Primary)
    }

    fn subdiagnostic(mut self, severity: Subseverity, message: Str) -> Self {
        self.0
            .subdiagnostics
            .push(Subdiagnostic { severity, message });
        self
    }

    /// Add further clarifying information.
    ///
    /// # Strict Guidelines
    ///
    /// * Same rules as for [`Self::message`] apply
    /// * It is allowed to use colons `:` but try not to
    /// * May span multiple lines
    pub fn note(self, message: impl Into<Str>) -> Self {
        self.subdiagnostic(Subseverity::Note, message.into())
    }

    /// Add steps or tips to solve the diagnosed issue.
    ///
    /// # Strict Guidelines
    ///
    /// * Same rules as for [`Self::message`] apply
    /// * Do not pose a question like `did you mean …?`
    /// * It is allowed to use colons `:` but try not to
    /// * May span multiple lines
    pub fn help(self, message: impl Into<Str>) -> Self {
        self.subdiagnostic(Subseverity::Help, message.into())
    }

    /// Add an internal debugging message.
    pub fn subdebug(self, message: impl Into<Str>) -> Self {
        self.subdiagnostic(Subseverity::Debug, message.into())
    }

    /// Add to the diagnostic depending on a boolean condition.
    pub fn if_(self, condition: bool, builder: impl FnOnce(Self) -> Self) -> Self {
        match condition {
            true => builder(self),
            false => self,
        }
    }

    /// Add to the diagnostic if the given resource exists.
    pub fn if_present<T>(self, value: Option<T>, builder: impl FnOnce(Self, T) -> Self) -> Self {
        match value {
            Some(value) => builder(self, value),
            None => self,
        }
    }

    /// Report the diagnostic.
    pub fn report(self, reporter: &Reporter) {
        reporter.report(self);
    }

    /// Format the diagnostic for the use in a terminal.
    ///
    /// # Panics
    ///
    /// Panics if the diagnostic refers to code snippets by [Span] but no source
    /// map is provided.
    // @Task if the span equals the span of the entire file, don't output its content
    // @Task add back the alorithm which reduces the amount of paths printed
    // @Beacon @Beacon @Beacon @Task special case trailing line break in subdiagnostics,
    // etc
    fn format_for_terminal(&self, map: Option<&SourceMap>) -> String {
        let mut message = String::new();

        // header
        {
            let severity = self.0.severity.to_string();
            let code = &self
                .0
                .code
                .map(|code| format!("[{code}]").color(self.0.severity.color()))
                .unwrap_or_default();

            message += &format!("{severity}{code}");
        }

        // text message
        if let Some(message_text) = &self.0.message {
            let message_text = message_text.bold();
            message += &format!(": {message_text}");
        }

        let padding;

        if !self.0.highlights.is_empty() {
            let map = map.unwrap();

            let all_lines = self
                .0
                .highlights
                .iter()
                .map(|highlight| map.lines(highlight.span))
                .collect::<Vec<_>>();

            let mut padding_width = 0;

            padding = {
                let mut largest_line_number = all_lines
                    .iter()
                    .flat_map(|span| {
                        once(span.first_line.number)
                            .chain(span.last_line.as_ref().map(|line| line.number))
                    })
                    .max()
                    .unwrap();

                while largest_line_number > 0 {
                    largest_line_number /= 10;
                    padding_width += 1;
                }

                " ".repeat(padding_width)
            };

            let bar = "|".color(FRAME_COLOR).bold();

            for (highlight, lines) in self.0.highlights.iter().zip(all_lines) {
                // path, line number and column
                {
                    let arrow = "-->".color(FRAME_COLOR).bold();
                    message += &format!("\n{padding}{arrow} ");

                    let file = lines.path.map(Path::to_string_lossy).unwrap_or_default();
                    let line = lines.first_line.number;
                    let column = lines.first_line.highlight_start_column;
                    // unbelieveably wasteful memory-wise but inevitable due to the API of `colored`
                    message += &format!("{file}:{line}:{column}")
                        .color(FRAME_COLOR)
                        .to_string();
                }

                let role_color = highlight.role.color(self.0.severity.color());

                let label = match &highlight.label {
                    Some(label) => label,
                    None => &Cow::Borrowed(""),
                };

                match &lines.last_line {
                    // the snippet spans a single line
                    None => {
                        let line_number = lines.first_line.number;
                        let snippet = lines.first_line.content;
                        let highlight_padding_width = lines.first_line.highlight_padding_width;
                        let zero_length_highlight = lines.first_line.highlight_width == 0;

                        let snippet_padding =
                            match zero_length_highlight && highlight_padding_width == 0 {
                                true => " ",
                                false => "",
                            };

                        message += &format!(
                            "\n\
                            {padding} {bar}\n\
                            {line_number:>padding_width$} {bar} {snippet_padding}{snippet}\n"
                        );

                        let underline_padding = " ".repeat(match zero_length_highlight {
                            true => highlight_padding_width.saturating_sub(1),
                            false => highlight_padding_width,
                        });
                        let underline = if !zero_length_highlight {
                            highlight
                                .role
                                .symbol()
                                .repeat(lines.first_line.highlight_width)
                        } else {
                            "><".to_owned()
                        };
                        let underline = underline.color(role_color).bold();

                        // the underline and the label
                        {
                            let mut label_lines = label.split('\n');

                            {
                                let label_line = label_lines.next().unwrap().color(role_color);

                                message += &format!(
                                    "{padding} {bar} {underline_padding}{underline} {label_line}"
                                );
                            }

                            let spacing = " ".repeat(
                                lines.first_line.highlight_padding_width
                                    + if zero_length_highlight {
                                        1
                                    } else {
                                        lines.first_line.highlight_width
                                    },
                            );

                            for label_line in label_lines {
                                let label_line = label_line.color(role_color);

                                message += &format!("\n{padding} {bar} {spacing} {label_line}");
                            }
                        }
                    }
                    // the snippet spans multiple lines
                    Some(final_line) => {
                        let ellipsis_or_bar = if final_line.number - lines.first_line.number > 1 {
                            "...".into()
                        } else {
                            format!(" {bar} ")
                        };

                        // the upper arm
                        {
                            let line_number = lines.first_line.number;
                            let snippet = lines.first_line.content;
                            let horizontal_arm = "_"
                                .repeat(lines.first_line.highlight_padding_width + 1)
                                .color(role_color)
                                .bold();
                            // the hand is currently not dependent on the Unicode width of the first character
                            let hand = highlight.role.symbol().color(role_color).bold();

                            message += &format!(
                                "\n\
                                {padding} {bar}\n\
                                {line_number:>padding_width$} {bar}   {snippet}\n\
                                {padding}{ellipsis_or_bar} {horizontal_arm}{hand}\n"
                            );
                        }
                        // the connector and the lower arm
                        {
                            let line_number = final_line.number;
                            let snippet = &final_line.content;
                            // the arm is currently not dependent on the Unicode width of the last character
                            let horizontal_arm = "_"
                                .repeat(final_line.highlight_width)
                                .color(role_color)
                                .bold();
                            let vertical_arm = "|".color(role_color).bold();
                            // the hand is currently not dependent on the Unicode width of the 1st character
                            let hand = highlight.role.symbol().color(role_color).bold();

                            message += &format!(
                                "{line_number:>padding_width$} {bar} {vertical_arm} {snippet}\n"
                            );

                            // the lower arm and the label
                            {
                                let mut label_lines = label.split('\n');

                                {
                                    let label_line = label_lines.next().unwrap().color(role_color);

                                    message += &format!(
                                    "{padding} {bar} {vertical_arm}{horizontal_arm}{hand} {label_line}"
                                );
                                }

                                let spacing = " ".repeat(1 + final_line.highlight_width + 1);

                                for label_line in label_lines {
                                    let label_line = label_line.color(role_color);

                                    message += &format!("\n{padding} {bar} {spacing} {label_line}");
                                }
                            }
                        }
                    }
                }

                message += &format!("\n{padding} {bar}");
            }
        } else {
            padding = " ".into();
        }

        for subdiagnostic in &self.0.subdiagnostics {
            message += &format!("\n{padding}{}: ", subdiagnostic.severity);

            let mut message_lines = subdiagnostic.message.split('\n');

            if let Some(message_line) = message_lines.next() {
                message += message_line;
            }

            for message_line in message_lines {
                let severity_spacing = " ".repeat(subdiagnostic.severity.name().width() + 1);
                message += &format!("\n{padding}{severity_spacing} {message_line}",);
            }
        }

        message
    }
}

const FRAME_COLOR: Color = Color::BrightBlue;
const ERROR_COLOR: Color = Color::BrightRed;
const WARNING_COLOR: Color = Color::BrightYellow;
const HELP_COLOR: Color = Color::BrightCyan;
const DEBUG_COLOR: Color = Color::BrightMagenta;

/// Part of a [complex error message](Diagnostic) providing extra text messages.
#[derive(PartialEq, Eq, Clone, PartialOrd, Ord)]
struct Subdiagnostic {
    severity: Subseverity,
    message: Str,
}

/// Level of severity of a diagnostic.
#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord)]
enum Severity {
    /// An internal compiler error (ICE).
    Bug,
    /// A user error.
    Error,
    Warning,
    Debug,
}

impl Severity {
    const fn name(self) -> &'static str {
        match self {
            Self::Bug => "internal compiler error",
            Self::Error => "error",
            Self::Warning => "warning",
            Self::Debug => "internal debugging message",
        }
    }

    const fn color(self) -> Color {
        match self {
            Self::Bug | Self::Error => ERROR_COLOR,
            Self::Warning => WARNING_COLOR,
            Self::Debug => DEBUG_COLOR,
        }
    }
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name().color(self.color()).bold())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord)]
enum Subseverity {
    /// Auxiliary note.
    Note,
    /// Steps to solve an issue.
    Help,
    Debug,
}

impl Subseverity {
    const fn name(self) -> &'static str {
        match self {
            Self::Note => "note",
            Self::Help => "help",
            Self::Debug => "debug",
        }
    }

    const fn color(self) -> Color {
        match self {
            Self::Note | Self::Help => HELP_COLOR,
            Self::Debug => DEBUG_COLOR,
        }
    }
}

impl fmt::Display for Subseverity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name().color(self.color()).bold())
    }
}

/// A highlighted code snippet.
#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
struct Highlight {
    span: Span,
    role: Role,
    label: Option<Str>,
}

/// The role of a highlighted code snippet — focal point or auxiliary note.
#[derive(PartialEq, Eq, Debug, Clone, Copy, PartialOrd, Ord)]
enum Role {
    /// Focal point of the diagnostic.
    Primary,
    /// Auxilary note of the diagnostic.
    Secondary,
}

impl Role {
    const fn color(self, primary: Color) -> Color {
        match self {
            Self::Primary => primary,
            Self::Secondary => HELP_COLOR,
        }
    }

    const fn symbol(self) -> &'static str {
        match self {
            Self::Primary => "^",
            Self::Secondary => "-",
        }
    }
}

/// A numeric code identifying a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[forbid(missing_docs)]
pub enum Code {
    /// _Permanently unassigned_.
    #[cfg(test)]
    E000,
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
    /// Duplicate or conflicting attributes.
    E006,
    /// Number literal does not fit type.
    E007,
    /// Number literal does not fit.
    E008,
    /// Re-export of private binding.
    E009,
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
    /// Unable to load out-of-line module.
    E016,
    /// Undefined lint.
    E018,
    /// Attribute arguments arity mismatch.
    E019,
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
    /// Invalid unnamed path hanger.
    E025,
    /// `capsule` or `super` inside nested path.
    E026,
    /// Attribute argument type mismatch.
    E027,
    /// Unexpected named attribute argument.
    E028,
    /// Use of private binding.
    E029,
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
    /// Type analysis.
    E035,
    /// Invalid capsule name.
    E036,
    /// Exposure reach not an ancestor of definition-site namespace.
    E037,
    /// Use of internal binding.
    E038,
    /// Redefinition of known binding.
    E039,
    /// Redefinition of intrinsic binding.
    E040,
    /// Module header is not the first declaration.
    E041,
    /// Intrinsic declaration with a body.
    E042,
    /// Missing program entry.
    E050,
    /// Missing intrinsic binding.
    E060,
    /// Unrecognized intrinsic binding.
    E061,
    /// Missing known binding.
    E062,
    /// Unrecognized known binding.
    E063,
    /// Metadata: Type mismatch.
    E800,
    /// Metadata: Unknown key.
    E801,
    /// Metadata: Missing key.
    E802,
    /// Metadata: Duplicate keys.
    E803,
}

impl Code {
    /// Provide detailled explanations and code examples per code.
    // @Task
    #[allow(dead_code, clippy::unused_self)]
    pub(crate) const fn explanation(self) -> &'static str {
        todo!()
    }
}

impl fmt::Display for Code {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}
