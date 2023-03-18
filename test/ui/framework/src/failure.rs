use crate::{path::shorten, terminal_width, Stream};
use colored::Colorize;
use derivation::{Elements, FromStr, Str};
use diagnostics::Diagnostic;
use difference::{Changeset, Difference};
use span::{SourceMap, Span};
use std::{io::Write, path::PathBuf, process::ExitStatus, time::Duration};
use utilities::{pluralize, Str};

pub(crate) struct FailedTest {
    pub(crate) path: PathBuf,
    pub(crate) failure: Failure,
}

impl FailedTest {
    pub(crate) fn new(path: PathBuf, kind: Failure) -> Self {
        Self {
            path,
            failure: kind,
        }
    }

    pub(crate) fn print(
        &self,
        diff_view: DiffView,
        map: &SourceMap,
        sink: &mut dyn Write,
    ) -> std::io::Result<()> {
        match &self.failure {
            Failure::UnexpectedExitStatus(status) => {
                // @Task if it's an unexpected pass and if no test tag was specified (smh. get that info),
                //       mention that ‘fail check’ is implied if not specified otherwise

                let code: Str = match status.code() {
                    Some(code) => format!("code ‘{code}’").into(),
                    None => "a code".into(),
                };

                let (adverb, verb, noun) = match status.success() {
                    true => ("successfully", "‘fail’", "success"),
                    false => ("unsuccessfully", "‘pass’ (succeed)", "failure"),
                };

                let diagnostic = Diagnostic::error().path(shorten(&self.path).into())
                    .message(format!("the compiler unexpectedly exited {adverb}"))
                    .note(format!(
                        "expected the compiler to {verb} but it exited with {code} indicating {noun}",
                    ));

                write!(sink, "{}", diagnostic.format(None))
            }
            Failure::GoldenFileMismatch {
                golden,
                actual,
                stream,
            } => {
                let diagnostic = Diagnostic::error()
                    .path(shorten(&self.path).into())
                    .message(format!(
                        "the {stream} output of the compiler differs from the expected one"
                    ));

                writeln!(sink, "{}", diagnostic.format(None))?;
                writeln!(sink, "{}", "-".repeat(terminal_width()).bright_black())?;

                match diff_view {
                    DiffView::Unified => {
                        let changes = Changeset::new(golden, actual, "\n");
                        write_differences_with_ledge(&changes.diffs, sink)?;
                    }
                    DiffView::Split => {
                        write!(sink, "{}", golden.yellow())?;
                        writeln!(sink, "{}", "-".repeat(terminal_width()).bright_black())?;
                        write!(sink, "{actual}")?;
                    }
                }

                write!(sink, "{}", "-".repeat(terminal_width()).bright_black())
            }
            Failure::InvalidTest {
                message,
                note,
                span,
            } => {
                let diagnostic = Diagnostic::error()
                    .path(shorten(&self.path).into())
                    .message(message.clone())
                    .with(|error| match note {
                        Some(note) => error.note(note.clone()),
                        None => error,
                    })
                    .with(|error| match span {
                        Some(span) => error.unlabeled_span(span),
                        None => error,
                    });

                write!(sink, "{}", diagnostic.format(None))
            }
            Failure::InvalidConfiguration(error) => {
                // @Temporary
                let diagnostic = Diagnostic::error()
                    .message(error.message.clone())
                    .unlabeled_span(error.span);

                write!(sink, "{}", diagnostic.format(Some(map)))
            }
            Failure::Timeout { global, local } => {
                let timeout = match local {
                    None => global.unwrap(),
                    Some(local) => *local,
                }
                .as_secs();

                let issuer = match (local, global) {
                    (Some(_), None) => "set in the test configuration",
                    (Some(_), Some(_)) => {
                        "set in the test configuration \
                         overwriting the command-line option"
                    }
                    (None, _) => "set via the command-line option",
                };

                let diagnostic = Diagnostic::error()
                    .path(shorten(&self.path).into())
                    .message("the test ran longer than the specified timeout")
                    .note(format!(
                        "the timeout is {timeout} {} ({issuer})",
                        pluralize!(timeout, "second"),
                    ));

                write!(sink, "{}", diagnostic.format(None))
            }
        }?;

        writeln!(sink)
    }
}

// @Task smh. differentiate between unexpected-*{compiler,program}*-{pass,fail}
pub(crate) enum Failure {
    UnexpectedExitStatus(ExitStatus),
    GoldenFileMismatch {
        golden: String,
        actual: String,
        stream: Stream,
    },
    InvalidTest {
        message: Str,
        note: Option<Str>,
        // @Temporary
        span: Option<Span>,
    },
    Timeout {
        global: Option<Duration>,
        local: Option<Duration>,
    },
    InvalidConfiguration(crate::configuration::Error),
}

#[derive(Clone, Copy, FromStr, Str, Elements, Default)]
#[format(dash_case)]
pub(crate) enum DiffView {
    #[default]
    Unified,
    Split,
}

// @Task highlight changes within lines
fn write_differences_with_ledge(
    differences: &[Difference],
    sink: &mut dyn Write,
) -> std::io::Result<()> {
    for difference in differences {
        match difference {
            Difference::Same(lines) => {
                for line in lines.lines() {
                    writeln!(sink, "{} {line}", " ".on_bright_white())?;
                }
            }
            Difference::Add(lines) => {
                for line in lines.lines().chain(lines.is_empty().then_some("")) {
                    writeln!(sink, "{} {}", "+".black().on_green(), line.green())?;
                }
            }
            Difference::Rem(lines) => {
                for line in lines.lines().chain(lines.is_empty().then_some("")) {
                    writeln!(sink, "{} {}", "-".black().on_red(), line.red())?;
                }
            }
        }
    }

    Ok(())
}
