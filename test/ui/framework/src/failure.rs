use crate::{terminal_width, Stream};
use colored::Colorize;
use difference::{Changeset, Difference};
use std::fmt;
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
        write!(f, "  {}", self.kind)
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
    SingleFilePackage,
    Package,
    PackageManifest,
    Auxiliary,
    Golden,
    Other,
}

impl fmt::Display for FileType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::SingleFilePackage => "single-file package",
            Self::Package => "package",
            Self::PackageManifest => "package manifest",
            Self::Auxiliary => "auxiliary file",
            Self::Golden => "golden file",
            Self::Other => "uncategorized",
        })
    }
}

pub(crate) enum FailureKind {
    UnexpectedPass,
    // @Task make this UnexpectedFail(ExitStatus) once stable
    UnexpectedFail {
        code: Option<i32>,
    },
    GoldenFileMismatch {
        golden: String,
        actual: String,
        stream: Stream,
    },
    InvalidFile {
        reason: Str,
    },
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
                    "expected the test to fail but the Lushui compiler exited successfully".red()
                )?;
            }
            Self::UnexpectedFail { code } => {
                write!(
                    f,
                    "{}",
                    format!(
                        "expected the test to pass but the Lushui compiler failed{}",
                        match code {
                            Some(code) => format!(" with exit code {code}"),
                            None => String::new(),
                        }
                    )
                    .red()
                )?;
            }
            Self::GoldenFileMismatch {
                golden,
                actual,
                stream,
            } => {
                writeln!(f, "{}", format!("the actual {stream} of the Lushui compiler does not match the expected golden {stream}:").red())?;
                writeln!(f)?;
                writeln!(f, "{}", "-".repeat(terminal_width()).bright_black())?;

                let changes = Changeset::new(golden, actual, "\n");
                write_differences_with_ledge(&changes.diffs, f)?;

                write!(f, "{}", "-".repeat(terminal_width()).bright_black())?;
            }
            Self::InvalidFile { reason } => {
                write!(f, "{}: {reason}", "the file is invalid".red())?;
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
