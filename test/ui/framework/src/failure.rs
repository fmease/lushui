use crate::{terminal_width, Stream};
use colored::Colorize;
use diagnostics::Diagnostic;
use difference::{Changeset, Difference};
use std::{fmt, path::PathBuf, process::ExitStatus, time::Duration};
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
}

impl fmt::Display for FailedTest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let path = crate::path::shorten(&self.path);
        let path = path.to_string_lossy();

        writeln!(f)?;
        writeln!(f, "{}", crate::section_header(&path))?;
        writeln!(f)?;
        write!(f, "{}", self.failure)
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
    },
    Timeout {
        global: Option<Duration>,
        local: Option<Duration>,
    },
    InvalidConfiguration(crate::configuration::Error),
}

impl fmt::Display for Failure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedExitStatus(status) => {
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

                let diagnostic = Diagnostic::error()
                    .message(format!("the compiler unexpectedly exited {adverb}"))
                    .note(format!(
                        "expected the compiler to {verb} but it exited with {code} indicating {noun}",
                    ));

                write!(f, "{}", diagnostic.format(None))
            }
            Self::GoldenFileMismatch {
                golden,
                actual,
                stream,
            } => {
                let diagnostic = Diagnostic::error().message(format!(
                    "the {stream} output of the compiler differs from the expected one"
                ));

                writeln!(f, "{}", diagnostic.format(None))?;
                writeln!(f, "{}", "-".repeat(terminal_width()).bright_black())?;

                let changes = Changeset::new(golden, actual, "\n");
                write_differences_with_ledge(&changes.diffs, f)?;

                write!(f, "{}", "-".repeat(terminal_width()).bright_black())
            }
            Self::InvalidTest { message, note } => {
                let diagnostic =
                    Diagnostic::error()
                        .message(message.clone())
                        .with(|error| match note {
                            Some(note) => error.note(note.clone()),
                            None => error,
                        });

                write!(f, "{}", diagnostic.format(None))
            }
            Self::InvalidConfiguration(error) => {
                // @Temporary
                let diagnostic = Diagnostic::error().message(error.message.clone());

                write!(f, "{}", diagnostic.format(None))
            }
            Self::Timeout { global, local } => {
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
                    .message("the test ran longer than the specified timeout")
                    .note(format!(
                        "the timeout is {timeout} {} ({issuer})",
                        pluralize!(timeout, "second"),
                    ));

                write!(f, "{}", diagnostic.format(None))
            }
        }
    }
}

// @Task highlight changes within lines
fn write_differences_with_ledge(
    differences: &[Difference],
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    for difference in differences {
        match difference {
            Difference::Same(lines) => {
                for line in lines.lines() {
                    writeln!(f, "{} {line}", " ".on_bright_white())?;
                }
            }
            Difference::Add(lines) => {
                for line in lines.lines().chain(lines.is_empty().then_some("")) {
                    writeln!(f, "{} {}", "+".black().on_green(), line.green())?;
                }
            }
            Difference::Rem(lines) => {
                for line in lines.lines().chain(lines.is_empty().then_some("")) {
                    writeln!(f, "{} {}", "-".black().on_red(), line.red())?;
                }
            }
        }
    }

    Ok(())
}
