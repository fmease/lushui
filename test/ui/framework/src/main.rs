#![feature(default_free_fn, const_option)]
#![forbid(rust_2018_idioms, unused_must_use)]

// @Task
// * replace some panics with Testsubfailures
// * strip rustc warnings from stderr

use std::{
    borrow::Cow,
    default::default,
    fmt,
    fs::{read_to_string, File},
    io::Write,
    num::NonZeroUsize,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    str::FromStr,
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};

use colored::Colorize;
use difference::{Changeset, Difference};
use unicode_width::UnicodeWidthStr;

const DEFAULT_NUMBER_TEST_THREADS_UNKNOWN_AVAILABLE_PARALLELISM: NonZeroUsize =
    NonZeroUsize::new(4).unwrap();

fn lushui_compiler_source_path() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}

fn default_test_directory_path() -> String {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../tests")
        .canonicalize()
        .unwrap()
        .to_str()
        .unwrap()
        .to_owned()
}

const AUTHOR: &str = "León Orell Valerian Liehr <liehr.exchange@gmx.net>";

struct Application {
    release_mode: bool,
    gilding: bool,
    loose_filters: Vec<String>,
    exact_filters: Vec<String>,
    number_test_threads: NonZeroUsize,
    test_folder_path: String,
}

impl Application {
    fn new() -> Self {
        let default_test_directory_path = default_test_directory_path();
        let available_parallelism = std::thread::available_parallelism()
            .ok()
            .unwrap_or(DEFAULT_NUMBER_TEST_THREADS_UNKNOWN_AVAILABLE_PARALLELISM);
        let available_parallelism = available_parallelism.to_string();

        let matches = clap::App::new(env!("CARGO_PKG_NAME"))
            .version(env!("CARGO_PKG_VERSION"))
            .author(AUTHOR)
            .about(env!("CARGO_PKG_DESCRIPTION"))
            .arg(
                clap::Arg::with_name("release")
                    .long("release")
                    .help("Builds the Lushui compiler in release mode (i.e. with optimizations)"),
            )
            .arg(
                clap::Arg::with_name("gild")
                    .long("gild")
                    .help("Updates golden files of all included failing tests to the current compiler output"),
            )
            .arg(
                clap::Arg::with_name("loose-filter")
                    .long("filter")
                    .short("f")
                    .takes_value(true)
                    .multiple(true)
                    .help(
                        "Excludes tests whose file path (relative to the test folder path, without extension) \
                         does not contain the given filter (and which do not match any other filter)"
                    ),
            )
            .arg(
                clap::Arg::with_name("exact-filter")
                    .long("filter-exact")
                    .short("F")
                    .takes_value(true)
                    .multiple(true)
                    .help(
                        "Excludes tests whose file path (relative to the test folder path, without extension) \
                        does not equal the given filter (and which do not match any other filter)"
                    ),
            )
            .arg(
                clap::Arg::with_name("number-test-threads")
                    .long("test-threads")
                    .takes_value(true)
                    .validator(|input| {
                        NonZeroUsize::from_str(&input)
                            .map(|_| ())
                            .map_err(|error| error.to_string())
                    })
                    .default_value(&available_parallelism)
                    .help("The number of threads to use during test execution"),
            )
            .arg(
                clap::Arg::with_name("test-folder-path")
                    .long("test-folder")
                    .takes_value(true)
                    .default_value(&default_test_directory_path)
                    .help("The path to the folder containing the test files"),
            )
            .get_matches();

        Application {
            release_mode: matches.is_present("release"),
            gilding: matches.is_present("gild"),
            loose_filters: matches
                .values_of("loose-filter")
                .map_or(Vec::new(), |value| value.map(ToString::to_string).collect()),
            exact_filters: matches
                .values_of("exact-filter")
                .map_or(Vec::new(), |value| value.map(ToString::to_string).collect()),
            number_test_threads: matches
                .value_of("number-test-threads")
                .map(|input| input.parse().unwrap())
                .unwrap(),
            test_folder_path: matches.value_of("test-folder-path").unwrap().to_owned(),
        }
    }
}

const SEPARATOR_WIDTH: usize = 100;

fn main() {
    if main_().is_err() {
        // all destructors have been run
        std::process::exit(1);
    }
}

fn main_() -> Result<(), ()> {
    let application = Application::new();

    if application.gilding {
        print!("Do you really want to gild all valid failing tests? [y/N] ");
        std::io::stdout().flush().unwrap();

        let mut answer = String::new();
        std::io::stdin().read_line(&mut answer).unwrap();
        let answer = answer.trim();

        if !(answer.eq_ignore_ascii_case("y") || answer.eq_ignore_ascii_case("yes")) {
            return Err(());
        }

        println!();
    }

    println!("Building the Lushui compiler...");

    // @Task capture stderr (errors + warnings)
    // save those warnings so we can remove it as the prefix of the following tests
    // but also add a note that some stuff was written to stderr (but status is ok)
    let status = Command::new("cargo")
        .current_dir(lushui_compiler_source_path())
        .arg("build")
        .args(if application.release_mode {
            &["--release"][..]
        } else {
            &[]
        })
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .unwrap();
    assert!(status.success()); // @Temporary

    println!();
    println!("{}", "=".repeat(SEPARATOR_WIDTH));
    println!();

    let test_folder_path: &'static _ = Box::leak(application.test_folder_path.into_boxed_str());
    // @Beacon @Question alternative to Arcs??? since we have a fixed amount of threads
    // currently, the Arc are deallocated "when joining" (just before but yeah)
    let test_folder_entries = Arc::new(Mutex::new(
        walkdir::WalkDir::new(test_folder_path).into_iter(),
    ));
    let statistics: Arc<Mutex<Statistics>> = default();
    let failures: Arc<Mutex<Vec<_>>> = default();
    let loose_filters: &'static _ = application.loose_filters.leak();
    let exact_filters: &'static _ = application.exact_filters.leak();
    let gilding = application.gilding;
    let number_test_threads = application.number_test_threads.into();

    let suite_time = Instant::now();

    let handles: Vec<_> = (0..number_test_threads)
        .map(|_| {
            let shared_entries = test_folder_entries.clone();
            let shared_statistics = statistics.clone();
            let shared_failures = failures.clone();

            std::thread::spawn(move || {
                let chunk_size = number_test_threads;
                let mut entries = Vec::with_capacity(chunk_size);
                let mut statistics = Statistics::default();
                let mut failures = Vec::new();

                loop {
                    {
                        let mut shared_entries = shared_entries.lock().unwrap();

                        for _ in 0..chunk_size {
                            match shared_entries.next() {
                                Some(entry) => entries.push(entry),
                                None => break,
                            }
                        }
                    }

                    if entries.is_empty() {
                        break;
                    }

                    for entry in entries.drain(..) {
                        let entry = entry.unwrap();
                        let path = entry.path();

                        // @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Task
                        // check if this folder is a package and invoke
                        // lushui with `lushui check <folder>` without passing --quiet!!
                        if entry.file_type().is_dir() {
                            continue;
                        }

                        // @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Task
                        // skip metadatafiles
                        if !entry.file_type().is_file() {
                            panic!("invalid file type");
                        }

                        if has_file_extension(path, "stdout") || has_file_extension(path, "stderr")
                        {
                            // handled later together with the corresponding Lushui file
                            // @Task throw an error if no corresp. Lushui file is found
                            continue;
                        }

                        if !has_file_extension(path, "lushui") {
                            panic!("illegal extension");
                        }

                        let legible_path = path
                            .to_str()
                            .unwrap()
                            .strip_prefix(test_folder_path)
                            .unwrap()
                            .strip_prefix('/')
                            .unwrap()
                            .strip_suffix("lushui")
                            .unwrap()
                            .strip_suffix('.')
                            .unwrap();

                        if !(loose_filters.is_empty() && exact_filters.is_empty()) {
                            let is_included = loose_filters.iter().any(|filter| legible_path.contains(filter))
                                || exact_filters.iter().any(|filter| legible_path == filter);

                            if !is_included {
                                statistics.skipped_tests += 1;
                                continue;
                            }
                        }

                        let mut failure = Failure {
                            path: legible_path.to_owned(),
                            subfailures: Vec::new(),
                        };

                        const PATH_PADDING: usize = 80;

                        let file = read_to_string(entry.path()).unwrap();

                        let configuration = match Configuration::parse(&file) {
                            Ok(configuration) => configuration,
                            Err(error) => {
                                let message = format!("file {legible_path:<PATH_PADDING$} {}", "INVALID".red());
                                statistics.invalid_files += 1;
                                failure.subfailures.push(Subfailure::Invalid {
                                    reason: error.to_string().into(),
                                });
                                println!("{}", message);
                                failures.push(failure);
                                continue;
                            }
                        };

                        if let TestTag::Auxiliary = configuration.tag {
                            let mut message = format!("aux  {legible_path:<PATH_PADDING$}");
                            let mut invalid = false;

                            if configuration.arguments.is_empty() {
                                invalid = true;
                                failure.subfailures.push(Subfailure::Invalid {
                                    reason: "the auxiliary file does not declare its users".into(),
                                });
                            } else {
                                // @Note validation not (yet) that sophisticated
                                // we should probably check if the auxiliary file is actually used by
                                // every single alleged user

                                for user in configuration.arguments {
                                    let path = Path::new(test_folder_path)
                                        .join(user)
                                        .with_extension("lushui");

                                    if !path.exists() {
                                        if !invalid {
                                            invalid = true;
                                        }
                                        failure
                                            .subfailures
                                            .push(Subfailure::Invalid { reason: format!("the user `{user}` of the auxiliary file does not exist").into() });
                                    }
                                }
                            }

                            if invalid {
                                message += &format!(" {}", "INVALID".red());
                                statistics.invalid_files += 1;
                                println!("{}", message);
                                failures.push(failure);
                            }

                            continue;
                        }

                        let mut message = format!("test {legible_path:<PATH_PADDING$}");

                        if let TestTag::Ignore = configuration.tag {
                            message += &format!(" {}", "ignored".yellow());
                            statistics.ignored_tests += 1;
                            println!("{}", message);
                            continue;
                        }

                        let time = Instant::now();

                        // @Beacon @Question how can we filter out rustc's warnings from stderr?
                        // @Task automatically set --no-core for -Zparse-only, -Zlex-only tests
                        let output = Command::new("cargo")
                            .current_dir(lushui_compiler_source_path())
                            .args(&["run", "--quiet", "--"])
                            .args(&configuration.arguments)
                            .arg(
                                if configuration
                                    .arguments
                                    .first()
                                    .map_or(false, |&command| command == "run")
                                {
                                    "--capsule-type=executable"
                                } else {
                                    "--capsule-type=library"
                                },
                            )
                            .arg("--quiet")
                            .arg(entry.path())
                            .output()
                            .unwrap();

                        let duration = time.elapsed();
                        message += &format!("{}", format!(" {duration:.2?}").bright_black());

                        match (configuration.tag, output.status.success()) {
                            (TestTag::Pass, true) | (TestTag::Fail, false) => {}
                            (TestTag::Pass, false) => {
                                failure.subfailures.push(Subfailure::UnexpectedFail {
                                    code: output.status.code(),
                                })
                            }
                            (TestTag::Fail, true) => {
                                failure.subfailures.push(Subfailure::UnexpectedPass)
                            }
                            (TestTag::Ignore | TestTag::Auxiliary, _) => unreachable!(),
                        }

                        let stdout_was_gilded = match check_against_golden_file(
                            test_folder_path,
                            &entry.path().with_extension("stdout"),
                            output.stdout,
                            Stream::Stdout,
                            gilding,
                        ) {
                            Ok(gilded) => gilded,
                            Err(error) => {
                                failure.subfailures.push(error);
                                false
                            }
                        };

                        let stderr_was_gilded = match check_against_golden_file(
                            test_folder_path,
                            &entry.path().with_extension("stderr"),
                            output.stderr,
                            Stream::Stderr,
                            gilding,
                        ) {
                            Ok(gilded) => gilded,
                            Err(error) => {
                                failure.subfailures.push(error);
                                false
                            }
                        };

                        if stdout_was_gilded || stderr_was_gilded {
                            statistics.gilded_tests += 1;
                            message += &format!(" {}", "gilded".blue());

                            if stdout_was_gilded {
                                message += &format!(" {}", "stdout".blue());
                            };
                            if stderr_was_gilded {
                                message += &format!(" {}", "stderr".blue());
                            };
                        } else if failure.subfailures.is_empty() {
                            message += &format!(" {}", "ok".green());
                            statistics.passed_tests += 1;
                        } else {
                            message += &format!(" {}", "FAIL".red());
                            statistics.failed_tests += 1;
                            failures.push(failure);
                        }

                        println!("{}", message);
                    }

                    *shared_statistics.lock().unwrap() += &std::mem::take(&mut statistics);
                    shared_failures.lock().unwrap().append(&mut failures);
                }
            })
        })
        .collect();

    handles
        .into_iter()
        .for_each(|handle| handle.join().unwrap());

    let duration = suite_time.elapsed();

    let statistics = Arc::try_unwrap(statistics)
        .unwrap_or_else(|_| unreachable!())
        .into_inner()
        .unwrap();

    let failures = Arc::try_unwrap(failures)
        .unwrap_or_else(|_| unreachable!())
        .into_inner()
        .unwrap();

    for failure in failures {
        println!();
        println!(
            "=== {} {}",
            failure.path,
            "=".repeat(
                SEPARATOR_WIDTH
                    .saturating_sub(failure.path.width())
                    .saturating_sub(5)
            )
        );
        println!();

        for subfailure in failure.subfailures {
            println!("  {subfailure}");
        }
    }

    let summary = Summary {
        statistics,
        gilding,
        duration,
        filter: loose_filters,
    };

    println!();
    println!("{}", summary);

    if !summary.statistics.tests_passed() {
        return Err(());
    }

    Ok(())
}

fn check_against_golden_file(
    test_directory_path: &str,
    golden_file_path: &Path,
    actual: Vec<u8>,
    stream: Stream,
    gilding: bool,
) -> Result<bool, Subfailure> {
    let actual = String::from_utf8(actual).unwrap();

    let golden = if golden_file_path.exists() {
        stream.preprocess_before_comparison(
            read_to_string(golden_file_path).unwrap(),
            test_directory_path,
        )
    } else {
        String::new()
    };

    if actual != golden {
        if !gilding {
            Err(Subfailure::GoldenFileMismatch {
                golden,
                actual,
                stream,
            })
        } else {
            let actual = stream.preprocess_before_generating(actual, test_directory_path);

            File::create(golden_file_path)
                .unwrap()
                .write_all(actual.as_bytes())
                .unwrap();

            Ok(true)
        }
    } else {
        Ok(false)
    }
}

// `failed_tests` does not necessarily equal `gilded_tests` since the former includes
// invalid tests which are not gilded
#[derive(Default, Clone)]
struct Statistics {
    ignored_tests: usize,
    passed_tests: usize,
    failed_tests: usize,
    gilded_tests: usize,
    skipped_tests: usize,
    invalid_files: usize,
}

impl Statistics {
    fn tests_passed(&self) -> bool {
        self.failed_tests == 0 && self.invalid_files == 0
    }

    fn executed_tests(&self) -> usize {
        self.passed_tests + self.gilded_tests + self.failed_tests
    }

    fn ratio_passed_vs_executed_tests(&self) -> f32 {
        let ratio = match self.executed_tests() {
            0 => 1.0,
            executed_tests => self.passed_tests as f32 / executed_tests as f32,
        };

        ratio * 100.0
    }

    // @Note misleading name
    fn total_amount_tests(&self) -> usize {
        self.executed_tests() + self.ignored_tests
    }

    // @Note horrid name
    fn unfiltered_total_amount_tests(&self) -> usize {
        self.total_amount_tests() + self.skipped_tests
    }

    // @Note horrible name
    fn filter_ratio(&self) -> f32 {
        let ratio = self.total_amount_tests() as f32 / self.unfiltered_total_amount_tests() as f32;
        ratio * 100.0
    }
}

impl std::ops::Add for &Statistics {
    type Output = Statistics;

    fn add(self, other: Self) -> Self::Output {
        Statistics {
            ignored_tests: self.ignored_tests + other.ignored_tests,
            passed_tests: self.passed_tests + other.passed_tests,
            failed_tests: self.failed_tests + other.failed_tests,
            gilded_tests: self.gilded_tests + other.gilded_tests,
            skipped_tests: self.skipped_tests + other.skipped_tests,
            invalid_files: self.invalid_files + other.invalid_files,
        }
    }
}

impl std::ops::AddAssign<&Self> for Statistics {
    fn add_assign(&mut self, other: &Self) {
        *self = &*self + other;
    }
}

struct Summary {
    statistics: Statistics,
    gilding: bool,
    duration: Duration,
    filter: &'static [String],
}

impl fmt::Display for Summary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", "=".repeat(SEPARATOR_WIDTH))?;
        writeln!(f)?;

        let status = if self.statistics.tests_passed() {
            if !self.gilding {
                "ALL TESTS PASSED!".green()
            } else if self.statistics.gilded_tests == 0 {
                "ALL TESTS PASSED WITHOUT GILDING!".green()
            } else {
                "SOME TESTS WERE GILDED!".blue()
            }
        } else {
            let mut message = String::new();

            if self.statistics.failed_tests > 0 {
                message += "SOME TESTS FAILED!";
            }

            if self.statistics.invalid_files > 0 {
                if self.statistics.failed_tests > 0 {
                    message.push(' ');
                }

                message += "SOME INVALID FILES FOUND!";
            }

            message.red()
        };

        write!(
            f,
            "  {status}
    {passed} {label_passed} ({ratio:.2}%)",
            passed = self.statistics.passed_tests,
            label_passed = if !self.gilding {
                "passed".green()
            } else {
                "passed without gilding".green()
            },
            ratio = self.statistics.ratio_passed_vs_executed_tests(),
        )?;

        if self.gilding {
            write!(
                f,
                " | {gilded} {label_gilded}",
                gilded = self.statistics.gilded_tests,
                label_gilded = "gilded".blue(),
            )?;
        }

        if self.statistics.invalid_files > 0 {
            write!(
                f,
                " | {invalid} {label_invalid}",
                invalid = self.statistics.invalid_files,
                label_invalid = "invalid".red(),
            )?;
        }

        writeln!(f, " | {failed} {label_failed} | {ignored} {label_ignored} | {filtered_out} filtered out | {total} in total ({filter_ratio:.2}% of {unfiltered_total})
    {duration}\
",
            failed = self.statistics.failed_tests,
            label_failed = "failed".red(),
            ignored = self.statistics.ignored_tests,
            label_ignored = "ignored".yellow(),
            total = self.statistics.total_amount_tests(),
            filtered_out = self.statistics.skipped_tests,
            unfiltered_total = self.statistics.unfiltered_total_amount_tests(),
            filter_ratio = self.statistics.filter_ratio(),
            duration = format!("{:.2?}", self.duration).bright_black(),
        )?;

        if !self.filter.is_empty() {
            writeln!(f, "    filtered by\n        {}", self.filter.join(" "))?;
        }

        writeln!(f)?;
        writeln!(f, "{}", "=".repeat(SEPARATOR_WIDTH))
    }
}

const FILE_PREFIX: &str = ";;; TEST ";

struct Configuration<'a> {
    tag: TestTag,
    arguments: Vec<&'a str>,
}

impl<'a> Configuration<'a> {
    fn parse(source: &'a str) -> Result<Self, ParseError> {
        use ParseError::*;

        // @Bug the prefix `;;; TEST` (no final space) leads to ParseError::MissingTag but
        // ideally, it should lead to ParseError::MissingTag. we need to apply trimming beforehand
        // (and restructure the code a bit)
        let arguments = source
            .lines()
            .next()
            .and_then(|line| line.strip_prefix(FILE_PREFIX))
            .ok_or(MissingPrefix)?;

        let mut arguments = arguments.split_ascii_whitespace();

        let tag = arguments.next().ok_or(MissingTag)?;

        Ok(Configuration {
            tag: match tag {
                "ignore" => TestTag::Ignore,
                "pass" => TestTag::Pass,
                "fail" => TestTag::Fail,
                "auxiliary" => TestTag::Auxiliary,
                tag => return Err(InvalidTag(tag.to_owned())),
            },
            arguments: arguments.collect(),
        })
    }
}

enum TestTag {
    Auxiliary,
    Fail,
    Ignore,
    Pass,
}

impl TestTag {
    const VALUES: &'static str = "`auxiliary`, `fail`, `ignore` and `pass`";
}

#[derive(Debug)]
enum ParseError {
    MissingPrefix,
    MissingTag,
    InvalidTag(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingPrefix => write!(f, "the file is missing the prefix `{FILE_PREFIX}`"),
            Self::MissingTag => write!(
                f,
                "the file is missing a tag; valid tags are {}",
                TestTag::VALUES
            ),
            Self::InvalidTag(argument) => {
                write!(
                    f,
                    "the file contains the invalid tag `{argument}`; valid tags are {}",
                    TestTag::VALUES
                )
            }
        }
    }
}

#[derive(Clone, Copy)]
enum Stream {
    Stdout,
    Stderr,
}

impl Stream {
    fn preprocess_before_comparison(self, stream: String, test_directory_path: &str) -> String {
        match self {
            Self::Stdout => stream,
            // @Task don't replace if preceeded by another `$` (and replace `$$` with `$` in a second step)
            Self::Stderr => stream.replace(GOLDEN_STDERR_VARIABLE_DIRECTORY, test_directory_path),
        }
    }

    fn preprocess_before_generating(self, stream: String, test_directory_path: &str) -> String {
        match self {
            Self::Stdout => stream,
            Self::Stderr => stream.replace(test_directory_path, GOLDEN_STDERR_VARIABLE_DIRECTORY),
        }
    }
}

impl fmt::Display for Stream {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Stdout => write!(f, "stdout"),
            Self::Stderr => write!(f, "stderr"),
        }
    }
}

const GOLDEN_STDERR_VARIABLE_DIRECTORY: &str = "${DIRECTORY}";

struct Failure {
    path: String,
    subfailures: Vec<Subfailure>,
}

enum Subfailure {
    UnexpectedPass,
    UnexpectedFail {
        code: Option<i32>,
    },
    GoldenFileMismatch {
        golden: String,
        actual: String,
        stream: Stream,
    },
    Invalid {
        reason: Cow<'static, str>,
    },
}

impl fmt::Display for Subfailure {
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
                            Some(code) => format!(" with exit code {}", code),
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
                writeln!(f, "{}", "-".repeat(SEPARATOR_WIDTH).bright_black())?;

                let changes = Changeset::new(golden, actual, "\n");
                write_differences_with_ledge(&changes.diffs, f)?;

                write!(f, "{}", "-".repeat(SEPARATOR_WIDTH).bright_black())?;
            }
            Self::Invalid { reason } => {
                write!(f, "{}: {}", "the file is invalid".red(), reason)?;
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
                    writeln!(f, "{} {}", " ".on_bright_white(), line)?;
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

fn has_file_extension(path: &Path, extension: &str) -> bool {
    path.extension().and_then(|extension| extension.to_str()) == Some(extension)
}
