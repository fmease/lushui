//! The diagnostic reporter.

use super::{Diagnostic, ErrorCode, Severity, UntaggedDiagnostic};
use span::SourceMap;
use std::{
    collections::BTreeSet,
    default::default,
    mem,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex, RwLock, RwLockReadGuard,
    },
};
use utilities::{pluralize, Conjunction, ListingExt};

// @Task diagnostic formatting options
// like display style: verbose (current default) vs. terse

/// A diagnostic reporter.
pub struct Reporter {
    kind: ReporterKind,
    map: Option<Arc<RwLock<SourceMap>>>,
}

impl Reporter {
    fn new(kind: ReporterKind) -> Self {
        Self { kind, map: None }
    }

    pub fn silent() -> Self {
        Self::new(ReporterKind::Silent)
    }

    pub fn buffer(diagnostics: Buffer) -> Self {
        Self::new(ReporterKind::Buffer(diagnostics))
    }

    pub fn stderr() -> Self {
        Self::new(ReporterKind::Stderr)
    }

    pub fn buffered_stderr(reported_any_errors: Arc<AtomicBool>) -> Self {
        Self::new(ReporterKind::BufferedStderr(StderrBuffer {
            errors: default(),
            warnings: default(),
            reported_any_errors,
        }))
    }

    #[must_use]
    pub fn with_map(mut self, map: Arc<RwLock<SourceMap>>) -> Self {
        self.map = Some(map);
        self
    }

    fn map(&self) -> Option<RwLockReadGuard<'_, SourceMap>> {
        self.map.as_ref().map(|map| map.read().unwrap())
    }

    // @Task only return ErasedReportedError for non-silent reporters (@Bug)
    pub(super) fn report<const S: Severity>(
        &self,
        diagnostic: Diagnostic<S>,
    ) -> <Diagnostic<S> as Report>::Output
    where
        Diagnostic<S>: Report,
    {
        self.report_untagged(diagnostic.untagged);
        Diagnostic::<S>::OUTPUT
    }

    fn report_untagged(&self, diagnostic: UntaggedDiagnostic) {
        match &self.kind {
            ReporterKind::Silent => {}
            ReporterKind::Buffer(diagnostics) => {
                diagnostics.lock().unwrap().insert(diagnostic);
            }
            ReporterKind::Stderr => stderr_print(&diagnostic.format(self.map().as_deref())),
            ReporterKind::BufferedStderr(buffer) => match diagnostic.severity {
                Severity::Bug | Severity::Error => {
                    buffer.errors.lock().unwrap().insert(diagnostic);
                }
                Severity::Warning => {
                    buffer.warnings.lock().unwrap().insert(diagnostic);
                }
                Severity::Debug => {
                    stderr_print(&diagnostic.format(self.map().as_deref()));
                }
            },
        }
    }
}

impl Drop for Reporter {
    fn drop(&mut self) {
        if let ReporterKind::BufferedStderr(buffer) = &self.kind {
            buffer.report(self.map().as_deref());
        }
    }
}

enum ReporterKind {
    Silent,
    Buffer(Buffer),
    Stderr,
    BufferedStderr(StderrBuffer),
}

pub type Buffer = Arc<Mutex<BTreeSet<UntaggedDiagnostic>>>;

struct StderrBuffer {
    errors: Mutex<BTreeSet<UntaggedDiagnostic>>,
    warnings: Mutex<BTreeSet<UntaggedDiagnostic>>,
    reported_any_errors: Arc<AtomicBool>,
}

impl StderrBuffer {
    fn report(&self, map: Option<&SourceMap>) {
        let warnings = mem::take(&mut *self.warnings.lock().unwrap());

        for warning in &warnings {
            stderr_print(&warning.format(map));
        }

        if !warnings.is_empty() {
            Self::report_warning_summary(warnings, map);
        }

        let errors = mem::take(&mut *self.errors.lock().unwrap());

        for error in &errors {
            stderr_print(&error.format(map));
        }

        if !errors.is_empty() {
            self.reported_any_errors.store(true, Ordering::SeqCst);
            Self::report_error_summary(errors, map);
        }
    }

    fn report_error_summary(errors: BTreeSet<UntaggedDiagnostic>, map: Option<&SourceMap>) {
        let explained_codes: BTreeSet<_> = errors
            .iter()
            .filter_map(|error| error.code)
            .filter(|&code| ErrorCode::try_from(code).unwrap().explanation().is_some())
            .collect();

        let summary = Diagnostic::error()
            .message(pluralize!(
                errors.len(),
                "aborting due to previous error",
                format!("aborting due to {} previous errors", errors.len()),
            ))
            .with(|it| {
                if !explained_codes.is_empty() {
                    it.note(format!(
                        "the {errors} {codes} {have} a detailed explanation",
                        errors = pluralize!(explained_codes.len(), "error"),
                        codes = explained_codes.iter().list(Conjunction::And),
                        have = pluralize!(explained_codes.len(), "has", "have"),
                    ))
                    // @Task don't use the CLI syntax outside of the driver (separation of concerns)
                    .help(pluralize!(
                        explained_codes.len(),
                        format!(
                            "run ‘lushui explain {}’ to view it",
                            explained_codes.first().unwrap()
                        ),
                        "run ‘lushui explain <CODES...>’ to view a selection of them"
                    ))
                } else {
                    it
                }
            })
            .format(map);

        stderr_print(&summary);
    }

    fn report_warning_summary(warnings: BTreeSet<UntaggedDiagnostic>, map: Option<&SourceMap>) {
        let summary = Diagnostic::warning()
            .message(format!(
                "emitted {} {}",
                warnings.len(),
                pluralize!(warnings.len(), "warning")
            ))
            .format(map);

        stderr_print(&summary);
    }
}

fn stderr_print(message: &impl std::fmt::Display) {
    eprintln!("{message}");
    eprintln!();
}

pub trait Report {
    type Output;
    const OUTPUT: Self::Output;
}

impl Report for Diagnostic<{ Severity::Bug }> {
    type Output = ErasedReportedError;
    const OUTPUT: Self::Output = ErasedReportedError::new();
}

impl Report for Diagnostic<{ Severity::Error }> {
    type Output = ErasedReportedError;
    const OUTPUT: Self::Output = ErasedReportedError::new();
}

impl Report for Diagnostic<{ Severity::Warning }> {
    type Output = ();
    const OUTPUT: Self::Output = ();
}

impl Report for Diagnostic<{ Severity::Debug }> {
    type Output = ();
    const OUTPUT: Self::Output = ();
}

/// A witness to / token for a [reported](Diagnostic::report) error.
///
/// A value of this type is a proof that an error was reported (neglecting buffering).
/// Using this as an error type instead of let's say `()` makes it a bit harder to
/// accidentally / thoughtlessly return an error without reporting anything
/// (which would lead to an internal compiler error in `main`) since such a witness
/// can only be constructed by [`Diagnostic::report`] or by [`Self::new_unchecked`].
/// Ideally, the name of the latter function would force the user to think twice
/// before committing to it.
///
/// Values of this type are isomorphic to the zero-sized type `()` and thus memory-wise
/// incredibly cheap (this isomorphism `()` is however not an API / ABI guarantee).
/// The word _erased_ in the name alludes to the fact that a costly error [`Diagnostic`]
/// has been turned into "nothing in size" (simplifying).
///
/// As an aside, we generally try to avoid using [`Diagnostic`]s themselves as error types
/// and rather report them right away enabling us to report several errors during
/// compilation without the need to resort to returning lists / sets of diagnostics (
/// which would make error handling awkward since they would need to be combined in all
/// fallible functions resulting in many small allocations affecting performance I think).
///
/// # Soundness Holes
///
/// The code base has not been fully adapted to this design yet. Therefore there are still quite a
/// few soundness holes: Values of this type can be obtained
///
/// * from a silent reporter
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ErasedReportedError(());

impl ErasedReportedError {
    const fn new() -> Self {
        Self(())
    }

    // @Task add documentation
    pub const fn new_unchecked() -> Self {
        Self::new()
    }
}
