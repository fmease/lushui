//! The diagnostics system.
//!
//! We currently solely emit to stderr and as a result, we only format for the use in
//! a terminal (TUI). In the future, we might want to emit as JSON or HTML, too.
//!
//! Sadly, we have several different error handling and diagnostics APIs following distinct
//! philosphies. We you them all. Although in many cases, it's "one module, one
//! error handling API". Each API has their own advantages and disadvantages but none
//! fulfill all of our requirements (which I haven't written down yet).
//! I am on the search for a more general API.
//! Those APIs are defined in [crate::support] (that might change) but I document them
//! here for searchability.
//!
//! ## Error Handling APIs
//!
//! Meta: Task: Expand and add description.
//!
//! * [crate::support::accumulate_errors] + [crate::support::TransposeExt] + [Diagnostics::err_or]
//! * [crate::support::InvalidFallback] + [crate::support::TryIn]
//! * custom diagnostics (new struct or enum impl'ing `From`) buffer in a stateful "context" struct
//! * `Error` enum with `Error::Unrecoverable` and `From` impl's (complements all of the above)
//!
//! ## Error Handling API Requirements
//!
//! Meta: Elaborate.
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
    support::AsDebug,
    HashSet, OnceCell, Str,
};
use colored::{Color, Colorize};
use std::{
    default::default,
    hash::Hash,
    iter::{once, FromIterator},
    mem::size_of,
    sync::{Arc, RwLock},
};

/// A successful value or – by default – a [Diagnostic].
///
/// Mainly intended for errors and maybe also warnings.
pub type Result<T, E = Diagnostic> = std::result::Result<T, E>;

/// A successful value or a [buffer](Diagnostics) of [diagnostics](Diagnostic).
pub type Results<T> = Result<T, Diagnostics>;

/// A buffer of [diagnostics](Diagnostic).
#[derive(Default)]
#[cfg_attr(test, derive(PartialEq, Eq, Debug))]
pub struct Diagnostics(Vec<Diagnostic>);

impl Diagnostics {
    pub fn insert(&mut self, diagnostic: Diagnostic) {
        self.0.push(diagnostic);
    }

    pub fn inserted(mut self, diagnostic: Diagnostic) -> Self {
        self.insert(diagnostic);
        self
    }

    pub fn extended<I: IntoIterator<Item = Diagnostic>>(mut self, other: I) -> Self {
        self.0.extend(other);
        self
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Return successful value if the error buffer is empty.
    pub fn err_or<T>(self, ok: T) -> Results<T> {
        self.0.is_empty().then(|| ok).ok_or(self)
    }

    /// Construct the successful value if the error buffer is empty.
    pub fn err_or_else<T, F: FnOnce() -> T>(self, ok: F) -> Results<T> {
        self.0.is_empty().then(ok).ok_or(self)
    }

    /// Pseudo-move for cases where rustc does not understand the control flow.
    pub fn take(&mut self) -> Self {
        std::mem::take(self)
    }
}

impl IntoIterator for Diagnostics {
    type Item = Diagnostic;

    // @Note we are laying bare implementation details, not good
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FromIterator<Diagnostic> for Diagnostics {
    fn from_iter<T: IntoIterator<Item = Diagnostic>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl std::iter::Extend<Diagnostic> for Diagnostics {
    fn extend<T: IntoIterator<Item = Diagnostic>>(&mut self, iter: T) {
        self.0.extend(iter)
    }
}

#[derive(Hash, PartialEq, Eq)]
struct UnboxedDiagnostic {
    level: Level,
    code: Option<Code>,
    message: Option<Str>,
    highlights: Vec<Highlight>,
    subdiagnostics: Vec<Subdiagnostic>,
    is_cancelled: bool,
}

// We don't want to pollute the stack with a humonguously big error
// structure. The happy paths come first (e.g. `()`, `u32`) as
// they are more common. They should not suffer (memory-wise).
const _: () = assert!(size_of::<Diagnostic>() == size_of::<usize>());

// Not sure in which case a `Clone` implementation for diagnostics
// makes sense. Take a look at `Diagnostics::take` as a possible
// alternative. Anyway, if you actually want to implement it,
// make sure to cancel the thing to be cloned (or this one??)
// so the drop code does not panic. As you can see, this makes the
// clone act a bit like a move just like the `take` …
static_assertions::assert_not_impl_any!(Diagnostic: Clone);

/// A complex error message.
#[derive(Hash, PartialEq, Eq)]
pub struct Diagnostic(Box<UnboxedDiagnostic>);

impl Diagnostic {
    /// Create a bare-bones diagnostic with a certain level of severity.
    fn new(level: Level) -> Self {
        Self(Box::new(UnboxedDiagnostic {
            level,
            code: None,
            message: None,
            highlights: Vec::new(),
            subdiagnostics: Vec::new(),
            is_cancelled: false,
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

    /// Diagnostic of an auxiliary note.
    pub fn note() -> Self {
        Self::new(Level::Note)
    }

    /// Diagnostic of steps to solve an issue.
    pub fn help() -> Self {
        Self::new(Level::Help)
    }

    /// Diagnostic of an internal debugging message.
    pub fn debug() -> Self {
        Self::new(Level::Debug)
    }

    /// Diagnostic of an unimplemented language feature.
    pub fn unimplemented(message: impl Into<Str>) -> Self {
        Self::error().with_message(format!("{} not supported yet", message.into()))
    }

    /// Cancel (disable) the diagnostic.
    ///
    /// With this, you acknowledge that the diagnostic was constructed but
    /// is never going to be emitted. This prevents a panic on drop.
    ///
    /// Prefer not constructing the diagnostic in the first place as it is
    /// expensive in terms of memory.
    pub fn cancel(&mut self) {
        self.0.is_cancelled = true;
    }

    /// Indicate whether the diagnostic was cancelled (disabled).
    ///
    /// The diagnostic will no longer be emitted.
    pub fn is_cancelled(&self) -> bool {
        self.0.is_cancelled
    }

    /// Add a suitable code to the diagnostic.
    pub fn with_code(mut self, code: Code) -> Self {
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
    pub fn with_message(mut self, message: impl Into<Str>) -> Self {
        self.0.message = Some(message.into());
        self
    }

    fn with_span(mut self, spanning: &impl Spanning, label: Option<Str>, role: Role) -> Self {
        self.0.highlights.push(Highlight {
            span: spanning.span(),
            label: label.map(Into::into),
            role,
        });
        self
    }

    /// Reference a code snippet as one of the focal points of the diagnostic.
    pub fn with_primary_span(self, spanning: &impl Spanning) -> Self {
        self.with_span(spanning, None, Role::Primary)
    }

    /// Reference and label a code snippet as one of the focal points of the diagnostic.
    pub fn with_labeled_primary_span(
        self,
        spanning: &impl Spanning,
        label: impl Into<Str>,
    ) -> Self {
        self.with_span(spanning, Some(label.into()), Role::Primary)
    }

    /// Reference a code snippet as auxiliary information for the diagnostic.
    pub fn with_secondary_span(self, spanning: &impl Spanning) -> Self {
        self.with_span(spanning, None, Role::Secondary)
    }

    /// Reference and label a code snippet as auxiliary information for the diagnostic.
    pub fn with_labeled_secondary_span(
        self,
        spanning: &impl Spanning,
        label: impl Into<Str>,
    ) -> Self {
        self.with_span(spanning, Some(label.into()), Role::Secondary)
    }

    fn with_spans<I>(mut self, spannings: I, label: Option<Str>, role: Role) -> Self
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
    pub fn with_primary_spans<I>(self, spannings: I) -> Self
    where
        I: Iterator<Item: Spanning>,
    {
        self.with_spans(spannings, None, Role::Primary)
    }

    /// Reference and label several very and equally important code snippets.
    pub fn with_labeled_primary_spans<I>(self, spannings: I, label: impl Into<Str>) -> Self
    where
        I: Iterator<Item: Spanning>,
    {
        self.with_spans(spannings, Some(label.into()), Role::Primary)
    }

    fn with_subdiagnostic(mut self, level: Level, message: Str) -> Self {
        self.0.subdiagnostics.push(Subdiagnostic { level, message });
        self
    }

    /// Add further clarifying information.
    ///
    /// ## Strict Guidelines
    ///
    /// * Same rules as for [Self::with_message] apply
    /// * It is allowed to use colons `:` but try not to
    /// * May span multiple lines
    pub fn with_note(self, message: impl Into<Str>) -> Self {
        self.with_subdiagnostic(Level::Note, message.into())
    }

    /// Add steps or tips to solve the diagnosed issue.
    ///
    /// ## Strict Guidelines
    ///
    /// * Same rules as for [Self::with_message] apply
    /// * Do not pose a question like `did you mean …?`
    /// * It is allowed to use colons `:` but try not to
    /// * May span multiple lines
    pub fn with_help(self, message: impl Into<Str>) -> Self {
        self.with_subdiagnostic(Level::Help, message.into())
    }

    /// Add an internal debugging message.
    pub fn with_debug(self, message: impl Into<Str>) -> Self {
        self.with_subdiagnostic(Level::Debug, message.into())
    }

    /// Add to the diagnostic depending on a boolean condition.
    pub fn when(self, condition: bool, builder: impl FnOnce(Self) -> Self) -> Self {
        match condition {
            true => builder(self),
            false => self,
        }
    }

    /// Add to the diagnostic if the given resource exists.
    pub fn when_some<T>(self, value: Option<T>, builder: impl FnOnce(Self, T) -> Self) -> Self {
        match value {
            Some(value) => builder(self, value),
            None => self,
        }
    }

    pub fn sorted_spans(&self) -> Vec<Span> {
        let mut spans: Vec<_> = self
            .0
            .highlights
            .iter()
            .map(|highlight| highlight.span)
            .collect();
        spans.sort();
        spans
    }

    fn hashed(&self) -> u64 {
        use std::hash::Hasher;
        let mut hasher = rustc_hash::FxHasher::default();
        self.hash(&mut hasher);
        hasher.finish()
    }

    /// Emit the diagnostic to stderr.
    ///
    /// If the diagnostic [was cancelled](Self::cancel), it does not emit it and returns `false`
    /// instead of `true`.
    ///
    /// ## Panics
    ///
    /// Panics if the diagnostic refers to code snippets by [Span] but no source
    /// map is provided.
    // @Beacon @Beacon @Bug this does not handle hash collisions, we might lose some diagnostics!
    #[must_use]
    pub fn emit_to_stderr(mut self, map: Option<&SourceMap>) -> bool {
        // we use this static instead of definining `Diagnostics` to a `HashSet` as it is really difficult to
        // correctly `cancel` duplicate diagnostic in every single case (`insert`, `extend`, `FromIterator`, …)
        // with that approach.
        static ALREADY_EMITTED_DIAGNOSTICS: OnceCell<Arc<RwLock<HashSet<u64>>>> = OnceCell::new();

        if self.is_cancelled() {
            return false;
        }

        let hash = self.hashed();
        let already_emitted_diagnostics = ALREADY_EMITTED_DIAGNOSTICS.get_or_init(default);

        if already_emitted_diagnostics
            .try_read()
            .unwrap()
            .contains(&hash)
        {
            self.cancel();
            return false;
        }

        eprintln!("{}", self.format_for_terminal(map));
        eprintln!();

        already_emitted_diagnostics
            .try_write()
            .unwrap()
            .insert(hash);
        self.cancel();

        true
    }

    /// Format a diagnostic for the use in a terminal.
    ///
    /// ## Panics
    ///
    /// Panics if the diagnostic refers to code snippets by [Span] but no source
    /// map is provided.
    // @Task if the span equals the span of the entire file, don't output its content
    // @Task add back the alorithm which reduces the amount of paths printed
    fn format_for_terminal(&mut self, map: Option<&SourceMap>) -> String {
        let mut message = String::new();

        // header
        {
            let level = self.0.level.to_string();
            let code = &self
                .0
                .code
                .map(|code| format!("[{:?}]", code).color(self.0.level.color()))
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
            // order highlights by file and position in text from top to bottom
            self.0.highlights.sort_by_key(|highlight| highlight.span);

            let map = map.unwrap();

            let resolved_spans = self
                .0
                .highlights
                .iter()
                .map(|highlight| map.resolve_span(highlight.span))
                .collect::<Vec<_>>();

            let mut padding_length = 0;

            padding = {
                let mut largest_line_number = resolved_spans
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

            for (highlight, resolved_span) in self.0.highlights.iter().zip(resolved_spans) {
                // path, line number and column
                {
                    let arrow = "-->".color(FRAME_COLOR).bold();
                    message += &format!("\n{padding}{arrow} ");

                    let file = resolved_span.path.to_string_lossy();
                    let line = resolved_span.first_line.number;
                    let column = resolved_span.first_line.highlight_start_column;
                    // unbelieveably wasteful memory-wise but inevitable due to the API of `colored`
                    message += &format!("{file}:{line}:{column}")
                        .color(FRAME_COLOR)
                        .to_string();
                }

                let role_color = highlight.role.color(self.0.level.color());

                match &resolved_span.final_line {
                    // the snippet spans a single line
                    None => {
                        let line = resolved_span.first_line.number;
                        let snippet = resolved_span.first_line.content;
                        let label = highlight
                            .label
                            .as_ref()
                            .map(|label| label.color(role_color))
                            .unwrap_or_default();
                        let highlight_padding =
                            " ".repeat(resolved_span.first_line.highlight_prefix_width);
                        let highlight = highlight
                            .role
                            .symbol()
                            .repeat(resolved_span.first_line.highlight_width)
                            .color(role_color)
                            .bold();

                        message += &format!(
                            "\n\
                            {padding} {bar}\n\
                            {line:>padding_length$} {bar} {snippet}\
                            {padding} {bar} {highlight_padding}{highlight} {label}"
                        );
                    }
                    // the snippet spans multiple lines
                    Some(final_line) => {
                        // @Task improve the look & feel of it; it could be better!
                        let ellipsis = if final_line.number - resolved_span.first_line.number > 1 {
                            const ELLIPSIS: &str = "...";
                            let padding_length = ELLIPSIS.len() + padding_length - 1;

                            format!("{ELLIPSIS:<padding_length$}")
                        } else {
                            format!("{padding} {bar}")
                        };

                        // the upper arm
                        {
                            let line = resolved_span.first_line.number;
                            let snippet = resolved_span.first_line.content;
                            let highlight_horizontal_arm = "_"
                                .repeat(resolved_span.first_line.highlight_prefix_width + 1)
                                .color(role_color)
                                .bold();
                            // the hand is currently not dependent on the Unicode width of the first character
                            let highlight_hand = highlight.role.symbol().color(role_color).bold();

                            message += &format!(
                                "\n\
                                {padding} {bar}\n\
                                {line:>padding_length$} {bar}   {snippet}\
                                {ellipsis:>padding_length$}  {highlight_horizontal_arm}{highlight_hand}\n" 
                            );
                        }
                        // the connector and the lower arm
                        {
                            let line = final_line.number;
                            let snippet = final_line.content;
                            // the arm is currently not dependent on the Unicode width of the last character
                            let highlight_horizontal_arm = "_"
                                .repeat(final_line.highlight_width)
                                .color(role_color)
                                .bold();
                            let highlight_vertical_arm = "|".color(role_color).bold();
                            // the hand is currently not dependent on the Unicode width of the 1st character
                            let highlight_hand = highlight.role.symbol().color(role_color).bold();
                            let label = highlight
                                .label
                                .as_ref()
                                .map(|label| label.color(role_color))
                                .unwrap_or_default();

                            message += &format!(
                                "{line:>padding_length$} {bar} {highlight_vertical_arm} {snippet}\
                                {padding} {bar} {highlight_vertical_arm}{highlight_horizontal_arm}{highlight_hand} {label}",
                            );
                        }
                    }
                }

                message += &format!("\n{padding} {bar}");
            }
        } else {
            padding = " ".into();
        }

        for subdiagnostic in &self.0.subdiagnostics {
            message += &format!("\n{padding}{subdiagnostic}");
        }

        message
    }
}

impl fmt::Debug for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn option<T: fmt::Debug>(option: &Option<T>) -> Str {
            option
                .as_ref()
                .map_or::<Str, _>("None".into(), |message| format!("{:?}", message).into())
        }

        f.debug_struct("Diagnostic")
            .field("level", &self.0.level)
            .field("code", &option(&self.0.code).as_debug())
            .field("message", &option(&self.0.message).as_debug())
            .field("subs", &self.0.subdiagnostics)
            .finish()
    }
}

impl Drop for Diagnostic {
    fn drop(&mut self) {
        if self.is_cancelled() {
            return;
        }

        const LENIENT: bool = false; // for debugging

        if LENIENT {
            let _ = Diagnostic::warning()
                .with_message("diagnostic not emitted")
                .with_note(format!("{:#?}", self))
                .emit_to_stderr(None);
            return;
        }

        if std::thread::panicking() {
            return;
        }

        panic!("a diagnostic was not emitted: {:#?}", self);
    }
}

const FRAME_COLOR: Color = Color::BrightBlue;
const ERROR_COLOR: Color = Color::BrightRed;
const WARNING_COLOR: Color = Color::BrightYellow;
const HELP_COLOR: Color = Color::BrightCyan;
const DEBUG_COLOR: Color = Color::BrightMagenta;

/// Part of a [complex error message](Diagnostic) providing extra text messages.
#[derive(Hash, PartialEq, Eq, Clone)]
struct Subdiagnostic {
    level: Level,
    message: Str,
}

impl fmt::Debug for Subdiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Sub")
            .field("level", &self.level)
            .field("message", &self.message)
            .finish()
    }
}

impl fmt::Display for Subdiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:}: {}", self.level, self.message)
    }
}

/// Level of severity of a diagnostic.
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
enum Level {
    Bug,
    Error,
    Warning,
    Note,
    Help,
    Debug,
}

impl Level {
    const fn name(self) -> &'static str {
        match self {
            Self::Bug => "internal compiler error",
            Self::Error => "error",
            Self::Warning => "warning",
            Self::Note => "note",
            Self::Help => "help",
            Self::Debug => "internal debugging message",
        }
    }

    const fn color(self) -> Color {
        match self {
            Self::Bug | Self::Error => ERROR_COLOR,
            Self::Warning => WARNING_COLOR,
            Self::Note | Self::Help => HELP_COLOR,
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

/// A highlighted code snippet.
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
struct Highlight {
    span: Span,
    role: Role,
    label: Option<Str>,
}

/// The role of a highlighted code snippet — focal point or auxiliary note.
#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
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
    /// Duplicate or conflicting attributes.
    E006,
    /// Number literal does not fit type.
    E007,
    /// Number literal does not fit.
    E008,
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
    /// Implicitness unimplemented.
    W001,
}

impl Code {
    /// Provide detailled explanations and code examples per code.
    // @Task
    #[allow(dead_code)]
    pub const fn explain(self) -> &'static str {
        todo!()
    }
}

/// Something that can buffer warnings.
pub trait Warn {
    fn diagnostics(&mut self) -> &mut Diagnostics;

    fn warn(&mut self, mut diagnostic: Diagnostic) {
        diagnostic.0.level = Level::Warning;
        self.diagnostics().insert(diagnostic);
    }
}
