//! The golden test runner.
//!
//! To see the output of this runner when `cargo test`ing, pass the flag
//! `--show-output`. If you'd like to only run golden tests, execute
//! `cargo t golden::run -- --show-output`.
//!
//! An inherent issue of the design of this specific test runner is the need to
//! rebuild the compiler before running the tests, otherwise the old binary is
//! picked up. Maybe there is a solution which is stays compatible with the principle
//! of using `cargo test` to drive the test suite and not some shell script (which
//! could make use of `cargo build` to automatically rebuild before running the tests).
//!
//! ## Pending Tasks
//!
//! * create a mode where the golden files of failing golden tests are
//!   overwritten/updated to the actual output (controlled by a flag or
//!   an environment variable). one can then use git to actually commit
//!   to the changes or not
//! * be able to locate the binary on Windows
//! * fix the output for a mismatch between golden and actual output: We print
//!   an extra line break which is not part of the content
//! * improve error reporting (printing `Error`s)
//! * make it possible to escape dollar signs in golden stderr files
//! * (not strictly related to this runner) create more relevant tests
//! * somehow prevent test writers from adding golden stderr files which
//!   don't use `${DIRECTORY}` but a local path
//! * verify some unwraps and replace them if necessary

use crate::{has_file_extension, span::SourceMap};
use colored::Colorize;
use std::{
    collections::HashMap,
    fmt,
    fs::{read_to_string, File},
    io::Write,
    path::Path,
    process::Command,
};

const TEST_DIRECTORY_NAME: &str = "tests";
const GOLDEN_STDERR_VARIABLE_DIRECTORY: &str = "${DIRECTORY}";

#[test]
fn run() -> Result<(), Error> {
    let test_directory_path = Path::new(env!("CARGO_MANIFEST_DIR")).join(TEST_DIRECTORY_NAME);
    let test_directory_path_str = test_directory_path.to_str().unwrap();
    let test_directory = test_directory_path
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
    let badge_generated = "generated".blue();

    let mut number_of_ignored_tests = 0u32;
    let mut number_of_passed_tests = 0u32;
    let mut number_of_failed_tests = 0u32;

    let mut failures: HashMap<String, Vec<Failure>> = HashMap::new();

    for dir_entry in test_directory {
        let entry = dir_entry.map_err(Error::InvalidTestsDirectoryEntry)?;
        let type_ = entry
            .file_type()
            .map_err(Error::TestsDirectoryEntryTypeUnavailable)?;
        let path = entry.path();
        let file_name = path
            .file_name()
            .and_then(|name| name.to_str())
            .ok_or(Error::IllegalTestFileName)?;

        if !type_.is_file() {
            continue;
        }

        // handled later together with the corresponding lushui file
        if has_file_extension(&path, "stdout") || has_file_extension(&path, "stderr") {
            continue;
        }

        print!("test {} ... ", file_name);

        let source_file = map
            .load(path.to_str().unwrap())
            .map_err(|_| Error::FailedLoadingSourceFile)?;

        let config = TestConfiguration::parse(source_file.content())
            .map_err(Error::InvalidTestFileHeader)?;

        if config.tag == TestTag::Ignore {
            number_of_ignored_tests += 1;
            println!("{}", badge_ignored);
            continue;
        }

        let golden_stdout_path = path.with_extension("stdout");
        let golden_stderr_path = path.with_extension("stderr");

        let output = Command::new(&program_path)
            // .env("NO_COLOR", "") // not necessary apparently
            .arg("--sort-diagnostics") // for deterministic output
            .args(config.program_arguments)
            .arg(&path)
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

        let mut some_stream_was_generated = false;

        if golden_stdout_path.exists() {
            let golden_stdout =
                read_to_string(golden_stdout_path).map_err(Error::GoldenFileInaccessible)?;
            let stdout = String::from_utf8(output.stdout).unwrap();

            if stdout != golden_stdout {
                failures.push(Failure::GoldenFileMismatch {
                    golden: golden_stdout,
                    actual: stdout,
                    stream: Stream::Stdout,
                });
            }
        } else {
            File::create(&golden_stdout_path)
                .map_err(Error::UnableToCreateGoldenFile)?
                .write_all(&output.stdout)
                .unwrap();

            some_stream_was_generated = true;
        }

        let stderr = String::from_utf8(output.stderr).unwrap();

        if golden_stderr_path.exists() {
            let golden_stderr =
                read_to_string(golden_stderr_path).map_err(Error::GoldenFileInaccessible)?;
            // @Task don't replace if preceeded by another `$` (and replace `$$` with `$` in a second step)
            let golden_stderr =
                golden_stderr.replace(GOLDEN_STDERR_VARIABLE_DIRECTORY, test_directory_path_str);

            if stderr != golden_stderr {
                failures.push(Failure::GoldenFileMismatch {
                    golden: golden_stderr,
                    actual: stderr,
                    stream: Stream::Stderr,
                });
            }
        } else {
            let stderr = stderr.replace(test_directory_path_str, GOLDEN_STDERR_VARIABLE_DIRECTORY);

            File::create(&golden_stderr_path)
                .map_err(Error::UnableToCreateGoldenFile)?
                .write_all(stderr.as_bytes())
                .unwrap();

            some_stream_was_generated = true;
        };

        if some_stream_was_generated {
            print!("{}, ", badge_generated);
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

    let golden_tests_failed = number_of_failed_tests != 0;

    println!();
    println!(
        "golden test result: {}. {} total. {} passed ({:.2}%); {} failed; {} ignored",
        if golden_tests_failed {
            badge_failed
        } else {
            badge_ok
        },
        number_of_tests,
        number_of_passed_tests,
        // NaN if total number of tests, who cares?
        number_of_passed_tests as f32 * 100.0 / number_of_recognized_tests as f32,
        number_of_failed_tests,
        number_of_ignored_tests
    );

    if golden_tests_failed {
        return Err(Error::TestsFailed);
    }

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
        write!(f, "{}: ", "test failure".red().bold())?;
        match self {
            Self::UnexpectedPass => {
                write!(f, "expected test to fail but compiler exited successfully")?
            }
            Self::UnexpectedFail { code } => {
                write!(f, "expected test to pass but compiler failed")?;
                if let Some(code) = code {
                    write!(f, " with exit code {}", code)?;
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
    TestsFailed,
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
            TestsFailed => format!("some golden tests failed"),
        };

        write!(f, "{}", message.bright_red().bold())
    }
}
