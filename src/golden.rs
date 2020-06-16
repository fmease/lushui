//! The golden test runner.

//  @Beacon document that you need to use `cargo test -- --show-output`
// be able to read arguments (should be possible) accepting the argument `--golden-disallow-ignored`
// to list all ignored tests (and fail)
// @Beacon @Beacon @Task create *a lot* of parsing tests and some type checker tests

// @Task add more flags to the general CLI
// `--only-parse`, `--only-desugar` and `--only-resolve` (or under `-Z`) which do NOT output to
// stderr/stdout (just use `--print-ast` etc NEXT to the preceeding ones)

// @Task have a flag to update golden files

use crate::{has_file_extension, span::SourceMap};
use colored::Colorize;
use std::{collections::HashMap, fmt, fs::read_to_string, path::Path, process::Command};

const TEST_DIRECTORY_NAME: &str = "tests";

// @Task improve `Error`-reporting: it looks messy!
#[test]
fn run() -> Result<(), Error> {
    let test_directory = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(TEST_DIRECTORY_NAME)
        .read_dir()
        .map_err(Error::InaccessibleTestsDirectory)?;

    let mut map = SourceMap::default();

    // @Bug does not work on Windows
    let program_path = Path::new(env!("CARGO_MANIFEST_DIR")).join(match cfg!(debug_assertions) {
        true => "target/debug/lushui",
        false => "target/release/lushui",
    });

    let badge_ok = "ok".green();
    let badge_failed = "FAILED".red();
    let badge_ignored = "ignored".yellow();

    let mut number_of_ignored_tests = 0u32;
    let mut number_of_passed_tests = 0u32;
    let mut number_of_failed_tests = 0u32;

    let mut failures: HashMap<String, Vec<Failure>> = HashMap::new();

    for dir_entry in test_directory {
        let entry = dir_entry.map_err(Error::InvalidTestsDirectoryEntry)?;
        let r#type = entry
            .file_type()
            .map_err(Error::TestsDirectoryEntryTypeUnavailable)?;
        let path = entry.path();
        let file_name = path
            .file_name()
            .and_then(|name| name.to_str())
            .ok_or(Error::IllegalTestFileName)?;

        if !r#type.is_file() {
            continue;
        }

        // handled later together with the corresponding lushui file
        if has_file_extension(&path, "stdout") || has_file_extension(&path, "stderr") {
            continue;
        }

        print!("test {} ... ", file_name);

        // @Beacon @Task use Error::FailedLoadingSourceFile adfter adjusting sig of map.load
        let source_file = map.load(path.to_str().unwrap()).unwrap_or_else(|diag| {
            diag.emit(Some(&map));
            panic!();
        });

        let config = TestConfiguration::parse(source_file.content())
            .map_err(Error::InvalidTestFileHeader)?;

        if config.tag == TestTag::Ignore {
            number_of_ignored_tests += 1;
            println!("{}", badge_ignored);
            continue;
        }

        let golden_stdout_path = path.with_extension("stdout");
        let golden_stderr_path = path.with_extension("stderr");

        if !golden_stdout_path.exists() {
            std::fs::File::create(&golden_stdout_path).map_err(Error::UnableToCreateGoldenFile)?;
        };

        if !golden_stderr_path.exists() {
            std::fs::File::create(&golden_stderr_path).map_err(Error::UnableToCreateGoldenFile)?;
        };

        let golden_stdout =
            read_to_string(golden_stdout_path).map_err(Error::GoldenFileInaccessible)?;

        let golden_stderr =
            read_to_string(golden_stderr_path).map_err(Error::GoldenFileInaccessible)?;

        let mut program_arguments = config.program_arguments.into_iter();

        // @Note @Beacon hacky, relying on too much stuff, better extend TestConfiguration DSL
        // to have commands Run and Check
        // @Temporary code below!!
        let output = Command::new(&program_path)
            .arg(
                program_arguments
                    .next()
                    .ok_or(Error::InvalidTestFileHeader(ParseError::MissingArgument))?,
            )
            .arg(&path)
            .args(program_arguments.collect::<Vec<_>>())
            .output()
            .map_err(Error::FailedRunningCompilerProcess)?;

        let failures = failures.entry(file_name.to_owned()).or_default();

        match config.tag {
            TestTag::Pass => {
                if !output.status.success() {
                    failures.push(Failure::UnexpectedFail {
                        code: output.status.code(),
                    });
                }
            }
            TestTag::Fail => {
                if output.status.success() {
                    failures.push(Failure::UnexpectedPass);
                }
            }
            TestTag::Ignore => unreachable!(),
        }

        let stdout = String::from_utf8(output.stdout).unwrap();
        let stderr = String::from_utf8(output.stderr).unwrap();

        if stdout != golden_stdout {
            failures.push(Failure::GoldenFileMismatch {
                golden: golden_stdout,
                actual: stdout,
                stream: Stream::Stdout,
            });
        }

        if stderr != golden_stderr {
            failures.push(Failure::GoldenFileMismatch {
                golden: golden_stderr,
                actual: stderr,
                stream: Stream::Stderr,
            });
        }

        if failures.is_empty() {
            number_of_passed_tests += 1;
            println!("{}", badge_ok);
        } else {
            number_of_failed_tests += 1;
            println!("{}", &badge_failed);
        }
    }

    for (file_name, file_failures) in failures {
        if file_failures.is_empty() {
            continue;
        }

        println!();
        println!("---- {} ----", file_name);
        println!();

        for file_failure in file_failures {
            println!("{}", file_failure);
        }
    }

    let number_of_recognized_tests = number_of_passed_tests + number_of_failed_tests;
    let number_of_tests = number_of_recognized_tests + number_of_ignored_tests;

    println!();
    println!(
        "golden test result: {}. {} total. {} passed ({:.2}%); {} failed; {} ignored",
        match number_of_failed_tests {
            0 => badge_ok,
            _ => badge_failed,
        },
        number_of_tests,
        number_of_passed_tests,
        // NaN if total number of tests, who cares?
        number_of_passed_tests as f32 * 100.0 / number_of_recognized_tests as f32,
        number_of_failed_tests,
        number_of_ignored_tests
    );

    Ok(())
}

// @Bug large size difference between variants
enum Failure {
    UnexpectedPass,
    UnexpectedFail {
        code: Option<i32>,
    },
    GoldenFileMismatch {
        golden: String,
        actual: String,
        stream: Stream,
    },
}

#[derive(Clone, Copy)]
enum Stream {
    Stdout,
    Stderr,
}

impl fmt::Display for Stream {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stream::Stdout => write!(f, "stdout"),
            Stream::Stderr => write!(f, "stderr"),
        }
    }
}

impl fmt::Display for Failure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedPass => {
                write!(f, "expected test to fail but compiler exited successfully")?
            }
            Self::UnexpectedFail { code } => {
                write!(f, "expected test to pass but compiler failed")?;
                if let Some(code) = code {
                    write!(f, "with exit code {}", code)?;
                }
            }
            Self::GoldenFileMismatch {
                golden,
                actual,
                stream,
            } => {
                let beam_start = ">".repeat(50);
                let beam_end = "<".repeat(50);

                writeln!(
                    f,
                    "{stream} of the compiler does not match the golden {stream}",
                    stream = stream
                )?;
                writeln!(f, "{}", format!("golden {}:", stream).yellow().bold())?;
                writeln!(f, "{}", format!("{}", beam_start).yellow().bold())?;
                writeln!(f, "{}", golden.yellow())?;
                writeln!(f, "{}", format!("{}", beam_end).yellow().bold())?;

                writeln!(f, "{}", format!("actual {}:", stream).bold())?;
                writeln!(f, "{}", format!("{}", beam_start).bold())?;
                write!(f, "{}", actual)?;
                writeln!(f, "{}", format!("{}", beam_end).bold())?;
            }
        }

        Ok(())
    }
}

struct TestConfiguration<'a> {
    tag: TestTag,
    program_arguments: Vec<&'a str>,
}

impl<'a> TestConfiguration<'a> {
    fn parse(source: &'a str) -> Result<Self, ParseError> {
        use ParseError::*;

        const PREFIX: &str = ";; TEST ";

        let arguments = source
            .lines()
            .next()
            .and_then(|line| line.strip_prefix(PREFIX))
            .ok_or(MissingPrefix)?;

        let mut arguments = arguments.split_ascii_whitespace();

        let tag = arguments.next().ok_or(MissingArgument)?;

        Ok(TestConfiguration {
            tag: match tag {
                "ignore" => TestTag::Ignore,
                "pass" => TestTag::Pass,
                "fail" => TestTag::Fail,
                tag => return Err(InvalidArgument(tag.to_owned())),
            },
            program_arguments: arguments.collect(),
        })
    }
}

#[derive(PartialEq, Eq)]
enum TestTag {
    Ignore,
    Pass,
    Fail,
}

#[derive(Debug)]
enum ParseError {
    MissingPrefix,
    MissingArgument,
    InvalidArgument(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingPrefix => write!(f, "missing prefix"),
            Self::MissingArgument => write!(f, "missing argument"),
            Self::InvalidArgument(argument) => write!(f, "invalid argument `{}`", argument),
        }
    }
}

use std::io;

enum Error {
    InaccessibleTestsDirectory(io::Error),
    InvalidTestsDirectoryEntry(io::Error),
    // @Task add file name
    IllegalTestFileName,
    TestsDirectoryEntryTypeUnavailable(io::Error),
    // @Task add cause (need to change signature of span::SourceMap::load
    //from ret. Diag. to a custom Error)
    FailedLoadingSourceFile,
    // @Task add filename
    InvalidTestFileHeader(ParseError),
    // @Task add filename and stream
    UnableToCreateGoldenFile(io::Error),
    // @Task add filename
    GoldenFileInaccessible(io::Error),
    FailedRunningCompilerProcess(io::Error),
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::*;

        let message = match self {
            InaccessibleTestsDirectory(error) => {
                format!("the tests/ directory is not accessible because {}", error)
            }
            InvalidTestsDirectoryEntry(error) => format!(
                "an entry of the tests/ directory is not valid because {}",
                error
            ),
            IllegalTestFileName => format!("the file name of a test file is illegal"),
            TestsDirectoryEntryTypeUnavailable(error) => format!(
                "unable to obtain the file type of an entry in tests/ because {}",
                error
            ),
            FailedLoadingSourceFile => format!("failed to load a lushui source file because: ?"),
            InvalidTestFileHeader(error) => {
                format!("the header of a test file is not valid because {}", error)
            }
            UnableToCreateGoldenFile(error) => {
                format!("unable to create golden file because {}", error)
            }
            GoldenFileInaccessible(error) => {
                format!("unable to read golden file because {}", error)
            }
            FailedRunningCompilerProcess(error) => {
                format!("failed to run the compiler as a process because {}", error)
            }
        };

        write!(f, "{}", message.bright_red().bold())
    }
}
