//! The diagnostic reporter.

// @Task support formatting as JSON, maybe also as HTML
// @Task support formatting diagnostics in a short form
// @Note we probably need to restructure the enum to
// a struct containing configuration options

use super::{Diagnostic, Severity};
use crate::{
    span::SourceMap,
    utility::{pluralize, Conjunction, ListingExt},
};
use std::{
    collections::BTreeSet,
    default::default,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex, RwLock, RwLockReadGuard,
    },
};

/// The diagnostic reporter.
pub struct Reporter(ReporterKind);

impl Reporter {
    // @Task only return ErasedReportedError for error diagnostics! (@Bug)
    // @Task only return ErasedReportedError for non-silent reporters (@Bug)
    pub(super) fn report(&self, diagnostic: Diagnostic) -> ErasedReportedError {
        match &self.0 {
            ReporterKind::Silent => {}
            ReporterKind::Stderr(reporter) => reporter.report(diagnostic),
            ReporterKind::BufferedStderr(reporter) => reporter.report_or_buffer(diagnostic),
        }

        ErasedReportedError(())
    }
}

enum ReporterKind {
    Silent,
    Stderr(StderrReporter),
    BufferedStderr(BufferedStderrReporter),
}

pub struct SilentReporter;

impl From<SilentReporter> for Reporter {
    fn from(_: SilentReporter) -> Self {
        Self(ReporterKind::Silent)
    }
}

pub struct StderrReporter {
    map: Option<Arc<RwLock<SourceMap>>>,
}

impl StderrReporter {
    pub fn new(map: Option<Arc<RwLock<SourceMap>>>) -> Self {
        Self { map }
    }

    fn report(&self, diagnostic: Diagnostic) {
        let map = self.map.as_ref().map(|map| map.read().unwrap());
        print_to_stderr(&diagnostic.format_for_terminal(map.as_deref()));
    }
}

impl From<StderrReporter> for Reporter {
    fn from(reporter: StderrReporter) -> Self {
        Self(ReporterKind::Stderr(reporter))
    }
}

pub struct BufferedStderrReporter {
    errors: Mutex<BTreeSet<Diagnostic>>,
    reported_any_errors: Arc<AtomicBool>,
    warnings: Mutex<BTreeSet<Diagnostic>>,
    map: Arc<RwLock<SourceMap>>,
}

impl BufferedStderrReporter {
    pub fn new(map: Arc<RwLock<SourceMap>>, reported_any_errors: Arc<AtomicBool>) -> Self {
        Self {
            errors: default(),
            reported_any_errors,
            warnings: default(),
            map,
        }
    }

    fn map(&self) -> RwLockReadGuard<'_, SourceMap> {
        self.map.read().unwrap()
    }

    fn report_or_buffer(&self, diagnostic: Diagnostic) {
        match diagnostic.0.severity {
            Severity::Bug | Severity::Error => {
                self.errors.lock().unwrap().insert(diagnostic);
            }
            Severity::Warning => {
                self.warnings.lock().unwrap().insert(diagnostic);
            }
            Severity::Debug => {
                print_to_stderr(&diagnostic.format_for_terminal(Some(&self.map())));
            }
        };
    }

    fn report_buffered_diagnostics(&self) {
        let warnings = std::mem::take(&mut *self.warnings.lock().unwrap());

        for warning in &warnings {
            print_to_stderr(&warning.format_for_terminal(Some(&self.map())));
        }

        if !warnings.is_empty() {
            let summary = Diagnostic::warning()
                .message(format!(
                    "emitted {} {}",
                    warnings.len(),
                    pluralize!(warnings.len(), "warning")
                ))
                .format_for_terminal(Some(&self.map()));

            print_to_stderr(&summary);
        }

        let errors = std::mem::take(&mut *self.errors.lock().unwrap());

        for error in &errors {
            print_to_stderr(&error.format_for_terminal(Some(&self.map())));
        }

        if !errors.is_empty() {
            self.reported_any_errors.store(true, Ordering::SeqCst);

            let explained_codes: BTreeSet<_> = errors
                .iter()
                .filter_map(|error| error.0.code)
                .filter(|code| code.explanation().is_some())
                .collect();

            let summary = Diagnostic::error()
                .message(pluralize!(
                    errors.len(),
                    "aborting due to previous error",
                    format!("aborting due to {} previous errors", errors.len()),
                ))
                .with(|error| {
                    if !explained_codes.is_empty() {
                        error
                            .note(format!(
                                "the {errors} {codes} {have} a detailed explanation",
                                errors = pluralize!(explained_codes.len(), "error"),
                                codes = explained_codes.iter().list(Conjunction::And),
                                have = pluralize!(explained_codes.len(), "has", "have"),
                            ))
                            // @Task don't use the CLI syntax in the lib, only in the bin (sep. of concerns)
                            .help(pluralize!(
                                explained_codes.len(),
                                format!(
                                    "run `lushui explain {}` to view it",
                                    explained_codes.first().unwrap()
                                ),
                                "run `lushui explain <CODES...>` to view a selection of them"
                            ))
                    } else {
                        error
                    }
                })
                .format_for_terminal(Some(&self.map()));

            print_to_stderr(&summary);
        }
    }
}

impl Drop for BufferedStderrReporter {
    fn drop(&mut self) {
        self.report_buffered_diagnostics();
    }
}

impl From<BufferedStderrReporter> for Reporter {
    fn from(reporter: BufferedStderrReporter) -> Self {
        Self(ReporterKind::BufferedStderr(reporter))
    }
}

fn print_to_stderr(message: &impl std::fmt::Display) {
    eprintln!("{message}");
    eprintln!();
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
/// * from a [`SilentReporter`]
/// * by reporting non-error diagnostics (warnings and internal debug messages)
#[derive(Clone, Copy, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct ErasedReportedError(());

impl ErasedReportedError {
    // @Task docs
    pub fn new_unchecked() -> Self {
        Self(())
    }
}
