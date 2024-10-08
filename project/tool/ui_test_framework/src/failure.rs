use crate::{Stream, path::shorten, terminal_width};
use derivation::{Elements, FromStr, Str};
use diagnostics::Diagnostic;
use span::{SourceMap, Span};
use std::{
    io::{self, Write},
    path::PathBuf,
    process::ExitStatus,
    time::Duration,
};
use utility::{
    Changeset, ChangesetExt, Str,
    paint::{AnsiColor, Painter},
    pluralize,
};

pub(crate) struct FailedTest {
    pub(crate) path: PathBuf,
    pub(crate) failure: Failure,
}

impl FailedTest {
    pub(crate) fn new(path: PathBuf, kind: Failure) -> Self {
        Self { path, failure: kind }
    }

    pub(crate) fn print(
        &self,
        diff_view: DiffView,
        map: &SourceMap,
        painter: &mut Painter,
    ) -> io::Result<()> {
        // FIXME(diag_infra): In crate `diagnostic` we could expose a lifetime-generic `Diagnostic` type
        //                    which would allow us to avoid the `to_path_buf` calls (& pot. more).
        //                    Contrary to the compiler which uses a `Reporter` that buffers diagnostics
        //                    (and thus lends itself to "`'static`" fields) we `render` them immediately.

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

                let diagnostic = Diagnostic::error().path(shorten(&self.path).to_path_buf())
                    .message(format!("the compiler unexpectedly exited {adverb}"))
                    .note(format!(
                        "expected the compiler to {verb} but it exited with {code} indicating {noun}",
                    ));

                diagnostic.render(None, painter)
            }
            Failure::GoldenFileMismatch { golden, actual, stream } => {
                let diagnostic =
                    Diagnostic::error().path(shorten(&self.path).to_path_buf()).message(format!(
                        "the {stream} output of the compiler differs from the expected one"
                    ));

                diagnostic.render(None, painter)?;
                writeln!(painter)?;
                painter.set(AnsiColor::BrightBlack)?;
                writeln!(painter, "{}", "-".repeat(terminal_width()))?;
                painter.unset()?;

                match diff_view {
                    DiffView::Unified => {
                        Changeset::new(golden, actual, "\n").render_with_ledge(painter)?;
                    }
                    DiffView::Split => {
                        painter.set(AnsiColor::Yellow)?;
                        write!(painter, "{golden}")?;
                        painter.unset()?;
                        painter.set(AnsiColor::BrightBlack)?;
                        writeln!(painter, "{}", "-".repeat(terminal_width()))?;
                        painter.unset()?;
                        write!(painter, "{actual}")?;
                    }
                }

                painter.set(AnsiColor::BrightBlack)?;
                write!(painter, "{}", "-".repeat(terminal_width()))?;
                painter.unset()
            }
            Failure::InvalidTest { message, note, span } => {
                let diagnostic = Diagnostic::error()
                    .path(shorten(&self.path).to_path_buf())
                    .message(message.clone())
                    .with(|it| match note {
                        Some(note) => it.note(note.clone()),
                        None => it,
                    })
                    .with(|it| match span {
                        Some(span) => it.unlabeled_span(span),
                        None => it,
                    });

                diagnostic.render(None, painter)
            }
            Failure::InvalidConfiguration(error) => {
                // @Temporary
                let diagnostic =
                    Diagnostic::error().message(error.message.clone()).unlabeled_span(error.span);

                diagnostic.render(Some(map), painter)
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
                    .path(shorten(&self.path).to_path_buf())
                    .message("the test ran longer than the specified timeout")
                    .note(format!(
                        "the timeout is {timeout} {} ({issuer})",
                        pluralize!(timeout, "second"),
                    ));

                diagnostic.render(None, painter)
            }
        }?;

        writeln!(painter)
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
