//! The diagnostics system.
//!
//! # Unimplemented Features
//!
//! * (maybe) subdiagnostics with a span
//! * display style: rich (current system) <-> short
//! * a rust script (in /misc) that finds the lowest [`ErrorCode`] that can be used
//!   as well as any unused error codes (searching `compiler/`)

use crate::{
    span::{SourceMap, Span, Spanning},
    utility::Str,
};
pub use code::{Code, ErrorCode, LintCode};
use derivation::Str;
use internal::UnboxedUntaggedDiagnostic;
pub use reporter::Reporter;
use std::{
    collections::BTreeSet,
    fmt::Debug,
    ops::{Deref, DerefMut},
};

mod code;
mod format;
mod lsp;
pub mod reporter;

/// A complex diagnostic message, optionally with source locations.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
#[must_use]
pub struct Diagnostic<const S: Severity = { Severity::Error }>(UntaggedDiagnostic);

impl<const S: Severity> Diagnostic<S> {
    fn new() -> Self {
        Self(Box::new(UnboxedUntaggedDiagnostic {
            severity: S,
            code: None,
            message: None,
            highlights: BTreeSet::new(),
            subdiagnostics: Vec::new(),
        }))
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
        self.message = Some(message.into());
        self
    }

    fn span(mut self, spanning: impl Spanning, label: Option<Str>, role: Role) -> Self {
        self.highlights.insert(Highlight {
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
        self.highlights.extend(spannings.map(|spanning| Highlight {
            span: spanning.span(),
            label: label.clone(),
            role,
        }));
        self
    }

    /// Reference several equally important code snippets.
    pub(crate) fn primary_spans<I>(self, spannings: I) -> Self
    where
        I: IntoIterator<Item: Spanning>,
    {
        self.spans(spannings.into_iter(), None, Role::Primary)
    }

    /// Reference and label several very and equally important code snippets.
    pub(crate) fn labeled_primary_spans<I>(self, spannings: I, label: impl Into<Str>) -> Self
    where
        I: IntoIterator<Item: Spanning>,
    {
        self.spans(spannings.into_iter(), Some(label.into()), Role::Primary)
    }

    fn subdiagnostic(mut self, severity: Subseverity, message: Str) -> Self {
        self.subdiagnostics
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

    pub fn with(self, builder: impl FnOnce(Self) -> Self) -> Self {
        builder(self)
    }

    /// Report the diagnostic.
    pub fn report(self, reporter: &Reporter) -> reporter::report::ReportOutput<S>
    where
        Diagnostic<S>: reporter::report::Report,
    {
        reporter.report(self)
    }
}

impl Diagnostic<{ Severity::Bug }> {
    /// Create a diagnostic for an internal compiler error (ICE).
    pub fn bug() -> Self {
        Self::new()
    }
}

impl Diagnostic<{ Severity::Error }> {
    /// Create a diagnostic for a user error.
    pub fn error() -> Self {
        Self::new()
    }

    pub fn code(mut self, code: ErrorCode) -> Self {
        self.code = Some(Code::Error(code));
        self
    }
}

impl Diagnostic<{ Severity::Warning }> {
    /// Create a diagnostic for a warning.
    pub fn warning() -> Self {
        Self::new()
    }

    pub fn code(mut self, code: LintCode) -> Self {
        self.code = Some(Code::Lint(code));
        self
    }
}

impl Diagnostic<{ Severity::Debug }> {
    /// Create a diagnostic for an internal debugging message.
    pub fn debug() -> Self {
        Self::new()
    }
}

impl<const S: Severity> Deref for Diagnostic<S> {
    type Target = UnboxedUntaggedDiagnostic;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<const S: Severity> DerefMut for Diagnostic<S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub(crate) type UntaggedDiagnostic = Box<UnboxedUntaggedDiagnostic>;

pub(crate) mod internal {
    use super::{BTreeSet, Code, Highlight, Severity, Str, Subdiagnostic};

    #[derive(PartialEq, Eq, PartialOrd, Ord)]
    pub struct UnboxedUntaggedDiagnostic {
        // Highlights come first since they should have the highest priority when ordering.
        // This places diagnostics close to “source order” (with buffered reporters):
        // Diagnostics for locations higher up in the file come first or “above” (in the
        // terminal for example), those lower down in the source also come last in the output.
        pub(super) highlights: BTreeSet<Highlight>,
        pub(super) subdiagnostics: Vec<Subdiagnostic>,
        pub(super) code: Option<Code>,
        pub(super) message: Option<Str>,
        pub(super) severity: Severity,
    }
}

impl UnboxedUntaggedDiagnostic {
    // @Question worth the method?
    pub(super) fn format(&self, map: Option<&SourceMap>) -> String {
        format::format(self, map)
    }
}

/// Part of a [complex error message](Diagnostic) providing extra text messages.
#[derive(PartialEq, Eq, Clone, PartialOrd, Ord)]
struct Subdiagnostic {
    severity: Subseverity,
    message: Str,
}

/// Level of severity of a diagnostic.
#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub enum Severity {
    /// An internal compiler error (ICE).
    Bug,
    /// A user error.
    Error,
    Warning,
    Debug,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord, Str)]
#[format(dash_case)]
enum Subseverity {
    /// An auxiliary note.
    Note,
    /// A message containing steps to solve an issue.
    Help,
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
    /// A focal point of the diagnostic.
    Primary,
    /// An auxilary note of the diagnostic.
    Secondary,
}
