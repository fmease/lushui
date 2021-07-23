//! The diagnostics system.
//!
//! We currently solely emit to stderr and as a result, we only format for the use in
//! a terminal (TUI). In the future, we might want to emit as JSON or HTML, too.
//!
//! ## Unimplemented Features
//!
//! * multiline subdiagnostic messages that are properly indented (aligned with the first
//!   character of the message)
//! * (maybe) subdiagnostics with a span
//! * emitting JSON
//! * display style: rich (current system) <-> short
//! * a rust script (in /misc) that finds the lowest [Code] that can be used
//!   as well as any unused error codes (searching src/)
//! * unit tests for the formatter
//! * unindenting long lines of highlighted source code (i.e. mapping initial whitespace to
//!   a single one)
//! * warning API/manager which can understand lushui's allows/denies/… directives
//! * unifying error handling API
//!
//! ## Issues
//!
//! * diagnostics with primary and secondary spans are not *that* readable because of
//!   all those lengthy paths. That didn't use to be the case, maybe we should some
//!   rules when the paths can be omitted
//! * cannot handle overly long lines of highlighted code (does not look tidy anymore)

use crate::{
    span::{SourceMap, Span, Spanning},
    Str,
};
use colored::{Color, Colorize};
pub use handler::Handler;
use std::{borrow::Cow, collections::BTreeSet, fmt::Debug, iter::once};
use unicode_width::UnicodeWidthStr;

pub mod handler;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct UnboxedDiagnostic {
    highlights: BTreeSet<Highlight>,
    subdiagnostics: Vec<Subdiagnostic>,
    level: Level,
    code: Option<Code>,
    message: Option<Str>,
}

/// A complex error message optionally with source location information.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
// @Beacon @Task once we stop storing any those (except inside Handler), we need to unbox this!
#[must_use]
pub struct Diagnostic(Box<UnboxedDiagnostic>);

impl Diagnostic {
    /// Create a bare-bones diagnostic with a certain level of severity.
    fn new(level: Level) -> Self {
        Self(Box::new(UnboxedDiagnostic {
            level,
            code: None,
            message: None,
            highlights: BTreeSet::new(),
            subdiagnostics: Vec::new(),
        }))
    }

    /// Diagnostic of an internal compiler error.
    pub fn bug() -> Self {
        Self::new(Level::Bug)
    }

    /// Diagnostic of a user error.
    pub fn error() -> Self {
        Self::new(Level::Error)
    }

    /// Diagnostic of a warning.
    pub fn warning() -> Self {
        Self::new(Level::Warning)
    }

    /// Diagnostic of an internal debugging message.
    pub fn debug() -> Self {
        Self::new(Level::Debug)
    }

    /// Diagnostic of an unimplemented language feature.
    pub fn unimplemented(message: impl Into<Str>) -> Self {
        Self::error().message(format!("{} not supported yet", message.into()))
    }

    /// Add a suitable code to the diagnostic.
    pub fn code(mut self, code: Code) -> Self {
        self.0.code = Some(code);
        self
    }

    /// Add a text message describing the issue.
    ///
    /// ## Strict Guidelines
    ///
    /// * No line breaks
    /// * Do not start the message with an upper case letter
    /// * Single sentence only without a final period or exclamation mark
    /// * When quoting code, surround them in backticks
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
    pub fn primary_span(self, spanning: impl Spanning) -> Self {
        self.span(spanning, None, Role::Primary)
    }

    /// Reference and label a code snippet as one of the focal points of the diagnostic.
    pub fn labeled_primary_span(self, spanning: impl Spanning, label: impl Into<Str>) -> Self {
        self.span(spanning, Some(label.into()), Role::Primary)
    }

    /// Reference a code snippet as auxiliary information for the diagnostic.
    pub fn secondary_span(self, spanning: impl Spanning) -> Self {
        self.span(spanning, None, Role::Secondary)
    }

    /// Reference and label a code snippet as auxiliary information for the diagnostic.
    pub fn labeled_secondary_span(self, spanning: impl Spanning, label: impl Into<Str>) -> Self {
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
    pub fn primary_spans<I>(self, spannings: I) -> Self
    where
        I: Iterator<Item: Spanning>,
    {
        self.spans(spannings, None, Role::Primary)
    }

    /// Reference and label several very and equally important code snippets.
    pub fn labeled_primary_spans<I>(self, spannings: I, label: impl Into<Str>) -> Self
    where
        I: Iterator<Item: Spanning>,
    {
        self.spans(spannings, Some(label.into()), Role::Primary)
    }

    fn subdiagnostic(mut self, level: Sublevel, message: Str) -> Self {
        self.0.subdiagnostics.push(Subdiagnostic { level, message });
        self
    }

    /// Add further clarifying information.
    ///
    /// ## Strict Guidelines
    ///
    /// * Same rules as for [Self::message] apply
    /// * It is allowed to use colons `:` but try not to
    /// * May span multiple lines
    pub fn note(self, message: impl Into<Str>) -> Self {
        self.subdiagnostic(Sublevel::Note, message.into())
    }

    /// Add steps or tips to solve the diagnosed issue.
    ///
    /// ## Strict Guidelines
    ///
    /// * Same rules as for [Self::message] apply
    /// * Do not pose a question like `did you mean …?`
    /// * It is allowed to use colons `:` but try not to
    /// * May span multiple lines
    pub fn help(self, message: impl Into<Str>) -> Self {
        self.subdiagnostic(Sublevel::Help, message.into())
    }

    /// Add an internal debugging message.
    pub fn subdebug(self, message: impl Into<Str>) -> Self {
        self.subdiagnostic(Sublevel::Debug, message.into())
    }

    /// Add to the diagnostic depending on a boolean condition.
    pub fn when(self, condition: bool, builder: impl FnOnce(Self) -> Self) -> Self {
        match condition {
            true => builder(self),
            false => self,
        }
    }

    /// Add to the diagnostic if the given resource exists.
    pub fn when_present<T>(self, value: Option<T>, builder: impl FnOnce(Self, T) -> Self) -> Self {
        match value {
            Some(value) => builder(self, value),
            None => self,
        }
    }

    pub fn emit(self, handler: &Handler) {
        handler.emit(self);
    }

    /// Emit the diagnostic to `stderr`.
    ///
    /// ## Panics
    ///
    /// Panics if the diagnostic refers to code snippets by [Span] but no source
    /// map is provided.
    pub fn emit_to_stderr(&self, map: Option<&SourceMap>) {
        eprintln!("{}", self.format_for_terminal(map));
        eprintln!();
    }

    /// Format the diagnostic for the use in a terminal.
    ///
    /// ## Panics
    ///
    /// Panics if the diagnostic refers to code snippets by [Span] but no source
    /// map is provided.
    // @Task if the span equals the span of the entire file, don't output its content
    // @Task add back the alorithm which reduces the amount of paths printed
    fn format_for_terminal(&self, map: Option<&SourceMap>) -> String {
        let mut message = String::new();

        // header
        {
            let level = self.0.level.to_string();
            let code = &self
                .0
                .code
                .map(|code| format!("[{code}]").color(self.0.level.color()))
                .unwrap_or_default();

            message += &format!("{level}{code}");
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
                .map(|highlight| map.lines_from_span(highlight.span))
                .collect::<Vec<_>>();

            let mut padding_length = 0;

            padding = {
                let mut largest_line_number = all_lines
                    .iter()
                    .flat_map(|span| {
                        once(span.first_line.number)
                            .chain(span.final_line.as_ref().map(|line| line.number))
                    })
                    .max()
                    .unwrap();

                while largest_line_number > 0 {
                    largest_line_number /= 10;
                    padding_length += 1;
                }

                " ".repeat(padding_length)
            };

            let bar = "|".color(FRAME_COLOR).bold();

            for (highlight, lines) in self.0.highlights.iter().zip(all_lines) {
                // path, line number and column
                {
                    let arrow = "-->".color(FRAME_COLOR).bold();
                    message += &format!("\n{padding}{arrow} ");

                    let file = lines.path.to_string_lossy();
                    let line = lines.first_line.number;
                    let column = lines.first_line.highlight_start_column;
                    // unbelieveably wasteful memory-wise but inevitable due to the API of `colored`
                    message += &format!("{file}:{line}:{column}")
                        .color(FRAME_COLOR)
                        .to_string();
                }

                let role_color = highlight.role.color(self.0.level.color());

                let label = match &highlight.label {
                    Some(label) => label,
                    None => &Cow::Borrowed(""),
                };

                match &lines.final_line {
                    // the snippet spans a single line
                    None => {
                        let line = lines.first_line.number;
                        // @Question does this *always* end in a line break?
                        let snippet = lines.first_line.content;
                        let underline_padding = " ".repeat(lines.first_line.highlight_prefix_width);
                        let underline = highlight
                            .role
                            .symbol()
                            .repeat(lines.first_line.highlight_width)
                            .color(role_color)
                            .bold();

                        message += &format!(
                            "\n\
                            {padding} {bar}\n\
                            {line:>padding_length$} {bar} {snippet}"
                        );

                        // the underline and the label
                        {
                            let mut label_lines = label.split('\n');

                            {
                                let label_line = label_lines.next().unwrap().color(role_color);

                                message += &format!(
                                    "{padding} {bar} {underline_padding}{underline} {label_line}"
                                );
                            }

                            let space = " ".repeat(
                                lines.first_line.highlight_prefix_width
                                    + lines.first_line.highlight_width,
                            );

                            for label_line in label_lines {
                                let label_line = label_line.color(role_color);

                                message += &format!("\n{padding} {bar} {space} {label_line}");
                            }
                        }
                    }
                    // the snippet spans multiple lines
                    Some(final_line) => {
                        // @Task improve the look & feel of it; it could be better!
                        let ellipsis = if final_line.number - lines.first_line.number > 1 {
                            const ELLIPSIS: &str = "...";
                            let padding_length = ELLIPSIS.len() + padding_length - 1;

                            format!("{ELLIPSIS:<padding_length$}")
                        } else {
                            format!("{padding} {bar}")
                        };

                        // the upper arm
                        {
                            let line = lines.first_line.number;
                            // @Question does this *always* end in a line break?
                            let snippet = lines.first_line.content;
                            let horizontal_arm = "_"
                                .repeat(lines.first_line.highlight_prefix_width + 1)
                                .color(role_color)
                                .bold();
                            // the hand is currently not dependent on the Unicode width of the first character
                            let hand = highlight.role.symbol().color(role_color).bold();

                            message += &format!(
                                "\n\
                                {padding} {bar}\n\
                                {line:>padding_length$} {bar}   {snippet}\
                                {ellipsis:>padding_length$}  {horizontal_arm}{hand}\n"
                            );
                        }
                        // the connector and the lower arm
                        {
                            let line = final_line.number;
                            // @Question does this *always* end in a line break?
                            let snippet = &final_line.content;
                            // the arm is currently not dependent on the Unicode width of the last character
                            let horizontal_arm = "_"
                                .repeat(final_line.highlight_width)
                                .color(role_color)
                                .bold();
                            let vertical_arm = "|".color(role_color).bold();
                            // the hand is currently not dependent on the Unicode width of the 1st character
                            let hand = highlight.role.symbol().color(role_color).bold();

                            message +=
                                &format!("{line:>padding_length$} {bar} {vertical_arm} {snippet}");

                            // the lower arm and the label
                            {
                                let mut label_lines = label.split('\n');

                                {
                                    let label_line = label_lines.next().unwrap().color(role_color);

                                    message += &format!(
                                    "{padding} {bar} {vertical_arm}{horizontal_arm}{hand} {label_line}"
                                );
                                }

                                let space = " ".repeat(1 + final_line.highlight_width + 1);

                                for label_line in label_lines {
                                    let label_line = label_line.color(role_color);

                                    message += &format!("\n{padding} {bar} {space} {label_line}");
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
            message += &format!("\n{padding}{}: ", subdiagnostic.level);

            let mut message_lines = subdiagnostic.message.split('\n');

            if let Some(message_line) = message_lines.next() {
                message += message_line;
            }

            for message_line in message_lines {
                let level_space = " ".repeat(subdiagnostic.level.name().width() + 1);
                message += &format!("\n{padding}{level_space} {message_line}",);
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
    level: Sublevel,
    message: Str,
}

/// Level of severity of a diagnostic.
#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord)]
enum Level {
    /// Internal compiler error.
    Bug,
    /// User error.
    Error,
    Warning,
    Debug,
}

impl Level {
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

use std::fmt;

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name().color(self.color()).bold())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord)]
enum Sublevel {
    /// Auxiliary note.
    Note,
    /// Steps to solve an issue.
    Help,
    Debug,
}

impl Sublevel {
    const fn name(self) -> &'static str {
        match self {
            Self::Note => "note",
            Self::Help => "help",
            Self::Debug => "internal debugging message",
        }
    }

    const fn color(self) -> Color {
        match self {
            Self::Note | Self::Help => HELP_COLOR,
            Self::Debug => DEBUG_COLOR,
        }
    }
}

impl fmt::Display for Sublevel {
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
    const fn color(&self, primary: Color) -> Color {
        match self {
            Self::Primary => primary,
            Self::Secondary => HELP_COLOR,
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[forbid(missing_docs)]
pub enum Code {
    /// Exposure reach not an ancestor of definition-site namespace.
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
    /// Unable to load external module.
    E016,
    /// Field declared outside of constructor declaration.
    E017,
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
    /// Crate or super inside nested path.
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
    /// Missing program entry.
    E050,
    /// Unregistered foreign binding.
    E060,
    /// Undefined foreign type.
    E061,
    /// Invalid inherent type.
    E062,
    /// Undefined inherent type.
    E063,
}

impl Code {
    /// Provide detailled explanations and code examples per code.
    // @Task
    #[allow(dead_code)]
    pub const fn explain(self) -> &'static str {
        todo!()
    }
}

impl fmt::Display for Code {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}
