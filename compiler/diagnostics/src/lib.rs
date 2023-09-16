//! The diagnostics system.
#![feature(adt_const_params, associated_type_bounds, negative_impls)]
#![allow(incomplete_features)] // adt_const_params

use derivation::Str;
use reporter::Report;
use span::{Span, Spanning};
use std::{
    collections::BTreeSet,
    fmt::Debug,
    marker::ConstParamTy,
    ops::{Deref, DerefMut},
    path::PathBuf,
};
use utility::Str;

pub use code::{Code, ErrorCode, LintCode};
pub use reporter::Reporter;

mod code;
mod render;

pub mod error;
pub mod reporter;

/// A complex diagnostic message, optionally with source locations.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
#[must_use]
pub struct Diagnostic<const S: Severity = { Severity::Error }> {
    untagged: UntaggedDiagnostic,
}

impl<const S: Severity> Diagnostic<S> {
    fn new() -> Self {
        Self {
            untagged: Box::new(UnboxedUntaggedDiagnostic::new(S)),
        }
    }

    /// Add a text message describing the issue.
    ///
    /// # Strict Guidelines
    ///
    /// * The message should not contain any line breaks (beware when embedding source code snippets!)
    /// * The message should not start with an upper case letter
    /// * The message should not end in a punctuation mark (like a period)
    /// * Surround source code snippets with (directional) single quotation marks,
    ///   i.e. `‘` (U+2018) to the left and `’` (U+2019) to the right
    /// * Try not to embed source code snippets that tend to be rather large
    ///   (this definitely applies to e.g. arbitrary *expressions* but less so to arbitrary *identifiers*)
    /// * The message should be able to stand on its own without the additional
    ///   information provided by labels and subdiagnostics. Exceptions are possible
    pub fn message(mut self, message: impl Into<Str>) -> Self {
        self.untagged.message = Some(message.into());
        self
    }

    fn _span(mut self, spanning: impl Spanning, label: Option<Str>, role: Role) -> Self {
        self.untagged.highlights.insert(Highlight {
            span: spanning.span(),
            label: label.map(Into::into),
            role,
        });
        self
    }

    /// Reference and label a code snippet as one of the focal points of the diagnostic.
    pub fn span(self, spanning: impl Spanning, label: impl Into<Str>) -> Self {
        self._span(spanning, Some(label.into()), Role::Primary)
    }

    /// Reference a code snippet as one of the focal points of the diagnostic.
    pub fn unlabeled_span(self, spanning: impl Spanning) -> Self {
        self._span(spanning, None, Role::Primary)
    }

    /// Reference and label a code snippet as auxiliary information for the diagnostic.
    pub fn label(self, spanning: impl Spanning, label: impl Into<Str>) -> Self {
        self._span(spanning, Some(label.into()), Role::Secondary)
    }

    #[cfg(test)]
    fn unlabeled_secondary_span(self, spanning: impl Spanning) -> Self {
        self._span(spanning, None, Role::Secondary)
    }

    #[allow(clippy::needless_pass_by_value)] // irrelevant
    fn _spans<I>(mut self, spannings: I, label: Option<Str>, role: Role) -> Self
    where
        I: Iterator<Item: Spanning>,
    {
        let spannings = spannings.map(|spanning| Highlight {
            span: spanning.span(),
            label: label.clone(),
            role,
        });
        self.untagged.highlights.extend(spannings);
        self
    }

    /// Reference and label several very and equally important code snippets.
    pub fn spans<I>(self, spannings: I, label: impl Into<Str>) -> Self
    where
        I: IntoIterator<Item: Spanning>,
    {
        self._spans(spannings.into_iter(), Some(label.into()), Role::Primary)
    }

    /// Reference several equally important code snippets.
    pub fn unlabeled_spans<I>(self, spannings: I) -> Self
    where
        I: IntoIterator<Item: Spanning>,
    {
        self._spans(spannings.into_iter(), None, Role::Primary)
    }

    fn subdiagnostic(mut self, severity: Subseverity, message: Str) -> Self {
        self.untagged
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

    pub fn suggest(
        mut self,
        span: impl Spanning,
        message: impl Into<Str>,
        substitution: impl Into<Substitution>,
    ) -> Self {
        self.untagged.suggestions.push(Suggestion {
            message: message.into(),
            span: span.span(),
            substitution: substitution.into(),
        });
        self
    }

    /// Reference a path in the diagnostic.
    ///
    /// Useful if the given path is not registered in the [source map] (e.g., if the
    /// path does not point to a file that could not be opened) or if one would like to
    /// highlight the entirety of a file without the need to reside to the whole [file span]
    /// which might divert attention from the intend of the diagnostic.
    ///
    /// A diagnostic may only ever have a single such path.
    /// Calling this function again overwrites the previous one.
    ///
    /// [source map]: span::SourceMap
    /// [file span]: span::SourceFile::span
    pub fn path(mut self, path: PathBuf) -> Self {
        self.untagged.path = Some(path);
        self
    }

    pub fn with(self, builder: impl FnOnce(Self) -> Self) -> Self {
        builder(self)
    }

    /// Report the diagnostic.
    pub fn report(self, reporter: &Reporter) -> <Diagnostic<S> as Report>::Output
    where
        Diagnostic<S>: Report,
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

impl Diagnostic {
    /// Create a diagnostic for a user error.
    pub fn error() -> Self {
        Self::new()
    }

    pub fn code(mut self, code: ErrorCode) -> Self {
        self.untagged.code = Some(Code::Error(code));
        self
    }

    pub fn embed<T: error::PossiblyErroneous, H: error::Handler>(self, handler: H) -> T {
        handler.embed(self)
    }

    pub fn handle<H: error::Handler>(self, handler: H) {
        let _: reporter::ErasedReportedError = handler.embed(self);
    }
}

impl Diagnostic<{ Severity::Warning }> {
    /// Create a diagnostic for a warning.
    pub fn warning() -> Self {
        Self::new()
    }

    pub fn code(mut self, code: LintCode) -> Self {
        self.untagged.code = Some(Code::Lint(code));
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
        &self.untagged
    }
}

// This impl would allow users to retag a diagnostic.
impl<const S: Severity> !DerefMut for Diagnostic<S> {}

pub type UntaggedDiagnostic = Box<UnboxedUntaggedDiagnostic>;

// @Task rethink ordering: message should be higher I guess
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct UnboxedUntaggedDiagnostic {
    pub path: Option<PathBuf>,
    // @Task update comment
    // Highlights come first since they should have the highest priority when ordering.
    // This places diagnostics close to “source order” (with buffered reporters):
    // Diagnostics for locations higher up in the file come first or “above” (in the
    // terminal for example), those lower down in the source also come last in the output.
    pub highlights: BTreeSet<Highlight>,
    pub subdiagnostics: Vec<Subdiagnostic>,
    pub suggestions: Vec<Suggestion>,
    pub code: Option<Code>,
    pub message: Option<Str>,
    pub severity: Severity,
}

impl UnboxedUntaggedDiagnostic {
    fn new(severity: Severity) -> Self {
        Self {
            path: None,
            highlights: BTreeSet::new(),
            subdiagnostics: Vec::new(),
            suggestions: Vec::new(),
            code: None,
            message: None,
            severity,
        }
    }
}

/// A highlighted code snippet.
#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub struct Highlight {
    pub span: Span,
    pub role: Role,
    pub label: Option<Str>,
}

/// The role of a highlighted code snippet — focal point or auxiliary note.
#[derive(PartialEq, Eq, Debug, Clone, Copy, PartialOrd, Ord)]
pub enum Role {
    /// A focal point of the diagnostic.
    Primary,
    /// An auxilary note of the diagnostic.
    Secondary,
}

/// Part of a [complex error message](Diagnostic) providing extra text messages.
#[derive(PartialEq, Eq, Clone, PartialOrd, Ord)]
pub struct Subdiagnostic {
    pub severity: Subseverity,
    pub message: Str,
}

/// Level of severity of a diagnostic.
#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord, ConstParamTy)]
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
pub enum Subseverity {
    /// An auxiliary note.
    Note,
    /// A message containing steps to solve an issue.
    Help,
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub struct Suggestion {
    pub message: Str,
    pub span: Span,
    pub substitution: Substitution,
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Default)]
pub struct Substitution {
    pub parts: Vec<SubstitutionPart>,
}

impl Substitution {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn str(mut self, value: impl Into<Str>) -> Self {
        self.parts.push(SubstitutionPart::Str(value.into()));
        self
    }

    pub fn placeholder(mut self, name: impl Into<Str>) -> Self {
        self.parts.push(SubstitutionPart::Placeholder(name.into()));
        self
    }
}

impl<S: Into<Str>> From<S> for Substitution {
    fn from(value: S) -> Self {
        Substitution::new().str(value)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]

pub enum SubstitutionPart {
    Str(Str),
    Placeholder(Str),
}
