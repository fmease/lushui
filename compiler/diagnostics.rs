//! The diagnostics system.
//!
//! # Unimplemented Features
//!
//! * (maybe) subdiagnostics with a span
//! * emitting JSON
//! * display style: rich (current system) <-> short
//! * a rust script (in /misc) that finds the lowest [Code] that can be used
//!   as well as any unused error codes (searching src/)

use crate::{
    span::{Span, Spanning},
    utility::Str,
};
use derivation::Str;
use reporter::ErasedReportedError;
pub use reporter::Reporter;
use std::{
    collections::BTreeSet,
    fmt::{self, Debug},
};

mod format;
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

    pub fn with(self, builder: impl FnOnce(Self) -> Self) -> Self {
        builder(self)
    }

    /// Report the diagnostic.
    pub fn report(self, reporter: &Reporter) -> ErasedReportedError {
        reporter.report(self)
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
enum Severity {
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
    /// Auxiliary note.
    Note,
    /// Steps to solve an issue.
    Help,
    Debug,
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

/// A numeric code identifying a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[forbid(missing_docs)]
pub enum Code {
    /// _Permanently unassigned_ (used for tests).
    E000,
    /// _Permanently unassigned_ (used for tests).
    E001,
    /// _Permanently unassigned_ (used for tests).
    E002,
    /// _Permanently unassigned_ (used for tests).
    E003,
    /// _Permanently unassigned_ (used for tests).
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
    /// Attempt to access subbinding of non-namespace.
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
    /// `topmost` or `super` inside nested path.
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
    /// Invalid word (package name, component name, metadata key, …).
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
    /// Attempt to construct a type with a literal that does not supported it.
    E043,
    /// Unbalanced bracket.
    E044,
    /// Trailing dash on identifier.
    E045,
    /// Invalid indentation.
    E046,
    /// Unterminated text literal.
    E047,
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
    /// Metadata: Unknown entry.
    E801,
    /// Metadata: Missing entry.
    E802,
    /// Metadata: Duplicate entries.
    E803,
}

impl Code {
    pub(crate) const fn explanation(self) -> Option<&'static str> {
        #[allow(clippy::match_single_binding)]
        match self {
            // @Task
            _ => None,
        }
    }
}

impl fmt::Display for Code {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}
