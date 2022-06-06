use crate::{terminal_width, Stream};
use colored::Colorize;
use difference::{Changeset, Difference};
use std::{borrow::Cow, fmt, process::ExitStatus};
use unicode_width::UnicodeWidthStr;

type Str = std::borrow::Cow<'static, str>;

pub(crate) struct Failure {
    pub(crate) file: File,
    pub(crate) kind: FailureKind,
}

impl Failure {
    pub(crate) fn new(file: File, kind: FailureKind) -> Self {
        Self { file, kind }
    }
}

impl fmt::Display for Failure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let title = format!("==== {} ", self.file);
        let bar = "=".repeat(terminal_width().saturating_sub(title.width()));

        writeln!(f)?;
        writeln!(f, "{title}{bar}")?;
        writeln!(f)?;
        write!(f, "{}", self.kind)
    }
}

pub(crate) struct File {
    pub(crate) path: String,
    pub(crate) type_: FileType,
}

impl File {
    pub(crate) fn new(path: String, type_: FileType) -> Self {
        Self { path, type_ }
    }
}

impl fmt::Display for File {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.path, self.type_)
    }
}

pub(crate) enum FileType {
    SourceFile,
    Package,
    PackageManifest,
    Auxiliary,
    MetadataSourceFile,
    Golden,
    Other,
}

impl fmt::Display for FileType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::SourceFile => "source file",
            Self::Package => "package",
            Self::PackageManifest => "package manifest",
            Self::Auxiliary => "auxiliary file",
            Self::MetadataSourceFile => "metadata source file",
            Self::Golden => "golden file",
            Self::Other => "uncategorized",
        })
    }
}

pub(crate) enum FailureKind {
    UnexpectedPass,
    UnexpectedFail(ExitStatus),
    GoldenFileMismatch {
        golden: String,
        actual: String,
        stream: Stream,
    },
    InvalidFile {
        reason: Str,
    },
    Timeout,
}

impl FailureKind {
    pub(crate) fn invalid_file(reason: impl Into<Str>) -> Self {
        Self::InvalidFile {
            reason: reason.into(),
        }
    }
}

impl fmt::Display for FailureKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedPass => {
                write!(
                    f,
                    "{}",
                    "Expected the compiler to fail but it actually exited successfully i.e. with exit code ‘0’."
                        .red()
                )?;
            }
            Self::UnexpectedFail(status) => {
                write!(
                    f,
                    "{}",
                    format!(
                        "Expected the compiler to exit successfully but it actually failed with {}.",
                        match status.code() {
                            Some(code) => format!("the non-zero exit code ‘{code}’").into(),
                            None => Cow::from("a non-zero exit code"),
                        },
                    )
                    .red()
                )?;
            }
            Self::GoldenFileMismatch {
                golden,
                actual,
                stream,
            } => {
                writeln!(
                    f,
                    "{}",
                    format!(
                        "The actual output of the compiler on {stream} differs from the expected golden one:"
                    )
                    .red()
                )?;
                writeln!(f)?;
                writeln!(f, "{}", "-".repeat(terminal_width()).bright_black())?;

                let changes = Changeset::new(golden, actual, "\n");
                write_differences_with_ledge(&changes.diffs, f)?;

                write!(f, "{}", "-".repeat(terminal_width()).bright_black())?;
            }
            Self::InvalidFile { reason } => {
                write!(
                    f,
                    "{}",
                    format!(
                        "For being in the test folder, the file has an incorrect form:\n{reason}."
                    )
                    .red()
                )?;
            }
            Self::Timeout => {
                write!(
                    f,
                    "{}",
                    "The test timed out: It ran longer than the specified timeout.".red()
                )?;
            }
        }

        Ok(())
    }
}

// the provided Display implementation for Changesets is problematic when whitespace differs
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
                for line in lines.lines().chain(lines.is_empty().then(|| "")) {
                    writeln!(f, "{} {}", "+".black().on_green(), line.green())?;
                }
            }
            Difference::Rem(lines) => {
                for line in lines.lines().chain(lines.is_empty().then(|| "")) {
                    writeln!(f, "{} {}", "-".black().on_red(), line.red())?;
                }
            }
        }
    }

    Ok(())
}
