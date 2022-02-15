//! The diagnostic reporter.

// @Task support formatting as JSON, maybe also as HTML
// @Task support formatting diagnostics in a short form
// @Note we probably need to restructure the enum to
// a struct containing configuration options

use super::{Diagnostic, Severity};
use crate::{
    format::{ordered_listing, pluralize, Conjunction},
    span::SharedSourceMap,
    utility::obtain,
};
use std::{cell::RefCell, collections::BTreeSet, default::default};

/// The diagnostic reporter.
#[non_exhaustive]
pub enum Reporter {
    Silent,
    Stderr(StderrReporter),
    BufferedStderr(BufferedStderrReporter),
}

impl Reporter {
    pub(super) fn report(&self, diagnostic: Diagnostic) -> ErrorReported {
        match self {
            Self::Silent => {}
            Self::Stderr(reporter) => reporter.report(diagnostic),
            Self::BufferedStderr(reporter) => reporter.report_or_buffer(diagnostic),
        }

        ErrorReported(())
    }
}

pub struct SilentReporter;

impl From<SilentReporter> for Reporter {
    fn from(_: SilentReporter) -> Self {
        Self::Silent
    }
}

pub struct StderrReporter {
    map: Option<SharedSourceMap>,
}

impl StderrReporter {
    pub fn new(map: Option<SharedSourceMap>) -> Self {
        Self { map }
    }

    fn report(&self, diagnostic: Diagnostic) {
        let map = self.map.as_ref().map(|map| map.borrow());
        print_to_stderr(&diagnostic.format_for_terminal(map.as_deref()));
    }
}

impl From<StderrReporter> for Reporter {
    fn from(reporter: StderrReporter) -> Self {
        Self::Stderr(reporter)
    }
}

pub struct BufferedStderrReporter {
    errors: RefCell<BTreeSet<Diagnostic>>,
    warnings: RefCell<BTreeSet<Diagnostic>>,
    map: SharedSourceMap,
}

impl BufferedStderrReporter {
    pub fn new(map: SharedSourceMap) -> Self {
        Self {
            errors: default(),
            warnings: default(),
            map,
        }
    }

    fn report_or_buffer(&self, diagnostic: Diagnostic) {
        match diagnostic.0.severity {
            Severity::Bug | Severity::Error => {
                self.errors.borrow_mut().insert(diagnostic);
            }
            Severity::Warning => {
                self.warnings.borrow_mut().insert(diagnostic);
            }
            Severity::Debug => {
                print_to_stderr(&diagnostic.format_for_terminal(Some(&self.map.borrow())));
            }
        };
    }

    /// Release the buffer of diagnostics.
    ///
    /// Writes all buffered bugs, errors and warnings to stderr and clears the buffer.
    /// Returns the number of reported errors and bugs.
    pub fn release_buffer(&self) -> usize {
        let warnings = std::mem::take(&mut *self.warnings.borrow_mut());

        for warning in &warnings {
            print_to_stderr(&warning.format_for_terminal(Some(&self.map.borrow())));
        }

        if !warnings.is_empty() {
            let summary = Diagnostic::warning()
                .message(format!(
                    "emitted {} {}",
                    warnings.len(),
                    pluralize!(warnings.len(), "warning")
                ))
                .format_for_terminal(Some(&self.map.borrow()));

            print_to_stderr(&summary);
        }

        let errors = std::mem::take(&mut *self.errors.borrow_mut());

        for error in &errors {
            print_to_stderr(&error.format_for_terminal(Some(&self.map.borrow())));
        }

        if !errors.is_empty() {
            let codes: BTreeSet<_> = errors.iter().filter_map(|error| error.0.code).collect();

            let summary = Diagnostic::error()
                .message(pluralize!(
                    errors.len(),
                    "aborting due to previous error",
                    format!("aborting due to {} previous errors", errors.len()),
                ))
                // @Note this not actually implemented yet
                .if_(!codes.is_empty(), |this| {
                    this.note(format!(
                        "the {errors} {codes} {have} a detailed explanation",
                        errors = pluralize!(codes.len(), "error"),
                        codes = ordered_listing(codes.iter(), Conjunction::And),
                        have = pluralize!(codes.len(), "has", "have"),
                    ))
                    .help(pluralize!(
                        codes.len(),
                        format!(
                            "run `lushui explain {}` to view it",
                            codes.iter().next().unwrap(),
                        ),
                        "run `lushui explain <codes...>` to view a selection of them"
                    ))
                })
                .format_for_terminal(Some(&self.map.borrow()));

            print_to_stderr(&summary);
        }

        errors.len()
    }
}

impl Drop for BufferedStderrReporter {
    fn drop(&mut self) {
        if !(std::thread::panicking()
            || self.errors.borrow().is_empty() && self.warnings.borrow().is_empty())
        {
            panic!("the buffer of the stderr reporter was not released before destruction");
        }
    }
}

impl From<BufferedStderrReporter> for Reporter {
    fn from(reporter: BufferedStderrReporter) -> Self {
        Self::BufferedStderr(reporter)
    }
}

impl TryFrom<Reporter> for BufferedStderrReporter {
    type Error = ();

    fn try_from(reporter: Reporter) -> Result<Self, Self::Error> {
        obtain!(reporter, Reporter::BufferedStderr(reporter) => reporter).ok_or(())
    }
}

fn print_to_stderr(message: &impl std::fmt::Display) {
    eprintln!("{message}");
    eprintln!();
}

// @Beacon @Task docs
#[derive(Clone, Copy, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct ErrorReported(());

impl ErrorReported {
    pub(crate) fn error_will_be_reported_unchecked() -> Self {
        Self(())
    }
}
