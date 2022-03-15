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
    pub(super) fn report(&self, diagnostic: Diagnostic) -> ErrorReported {
        match &self.0 {
            ReporterKind::Silent => {}
            ReporterKind::Stderr(reporter) => reporter.report(diagnostic),
            ReporterKind::BufferedStderr(reporter) => reporter.report_or_buffer(diagnostic),
        }

        ErrorReported(())
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

            let codes: BTreeSet<_> = errors.iter().filter_map(|error| error.0.code).collect();

            let summary = Diagnostic::error()
                .message(pluralize!(
                    errors.len(),
                    "aborting due to previous error",
                    format!("aborting due to {} previous errors", errors.len()),
                ))
                // @Note this not actually implemented yet
                // @Task only do this for any `code` where `code.explain().is_some()`
                .if_(!codes.is_empty(), |this| {
                    this.note(format!(
                        "the {errors} {codes} {have} a detailed explanation",
                        errors = pluralize!(codes.len(), "error"),
                        codes = codes.iter().list(Conjunction::And),
                        have = pluralize!(codes.len(), "has", "have"),
                    ))
                    // @Task don't use the CLI syntax in the lib, only in the bin (sep. of concerns)
                    .help(pluralize!(
                        codes.len(),
                        format!(
                            "run `lushui explain {}` to view it",
                            codes.iter().next().unwrap(),
                        ),
                        "run `lushui explain <CODES...>` to view a selection of them"
                    ))
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

// @Beacon @Beacon @Note happens quite often (look at the impl of Word::parse!): ErrorReported obtained via a
// SilentReporter. Technically correct but undesirable.

// @Beacon @Task docs
#[derive(Clone, Copy, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct ErrorReported(());

impl ErrorReported {
    pub fn new_unchecked() -> Self {
        Self(())
    }
}
