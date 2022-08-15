#![feature(
    default_free_fn,
    const_option,
    once_cell,
    let_else,
    str_split_as_str,
    drain_filter
)]

use colored::Colorize;
use configuration::{Configuration, Mode, TestTag, Timeout};
use failure::{FailedTest, Failure};
use joinery::JoinableIterator;
use std::{
    collections::BTreeSet,
    default::default,
    fmt, fs,
    io::Write,
    mem,
    path::{Path, PathBuf},
    process::{Command, ExitCode, Stdio},
    sync::{Arc, LazyLock, Mutex},
    time::{Duration, Instant},
};
use summary::{TestSuiteStatistics, TestSuiteSummary};
use utilities::pluralize;

mod cli;
mod configuration;
mod failure;
mod summary;

fn main() -> ExitCode {
    match try_main() {
        Ok(_) => ExitCode::SUCCESS,
        Err(_) => ExitCode::FAILURE,
    }
}

// @Beacon @Bug Currently we are traversing the whole test folder and filtering out
//              any tests that don't match the given list of paths.
//              However, that means we are wasting a huge amount of time.
//              This does not scale.
// @Task        Instead, only pass the relevant paths to `walkdir` (smh.) (if possible)

fn try_main() -> Result<(), ()> {
    let arguments = cli::arguments()?;

    if arguments.gilding == Gilding::Yes {
        print!("Do you really want to gild all (valid) failing tests? [y/N] ");
        std::io::stdout().flush().unwrap();

        let mut answer = String::new();
        std::io::stdin().read_line(&mut answer).unwrap();
        let answer = answer.trim();

        if !(answer.eq_ignore_ascii_case("y") || answer.eq_ignore_ascii_case("yes")) {
            return Err(());
        }

        println!();
    }

    println!("Building the compiler ...");

    let output = Command::new("cargo")
        .args(["+nightly", "--color=always", "build", "--manifest-path"])
        .arg(path::compiler_manifest())
        .args(match arguments.compiler_build_mode {
            CompilerBuildMode::Debug => &[],
            CompilerBuildMode::Release => &["--release"][..],
        })
        .stdin(Stdio::null())
        .output()
        .unwrap();

    if !output.status.success() {
        eprintln!("{}", "The compiler failed to build.".red());

        eprintln!();
        eprintln!("{}", section_header("RUST-CARGO STDERR"));
        eprintln!();

        eprintln!("{}", String::from_utf8_lossy(&output.stderr));

        return Err(());
    }

    println!();
    println!("{}", section_separator());
    println!();

    // @Task find an alternative to the Arcs (we know how many threads we use)
    let test_folder_entries = Arc::new(Mutex::new(
        walkdir::WalkDir::new(path::test_folder()).into_iter(),
    ));
    let statistics: Arc<Mutex<TestSuiteStatistics>> = default();
    let failed_tests: Arc<Mutex<Vec<_>>> = default();
    let number_test_threads = arguments.number_test_threads.into();
    let diff_view = arguments.diff_view;
    let arguments = arguments.into();

    let suite_time = Instant::now();

    #[allow(clippy::needless_collect)] // false positive
    let handles: Vec<_> = (0..number_test_threads)
        .map(|_| {
            let shared_entries = test_folder_entries.clone();
            let shared_statistics = statistics.clone();
            let shared_failures = failed_tests.clone();

            std::thread::spawn(move || {
                let chunk_size = number_test_threads;
                let mut entries = Vec::with_capacity(chunk_size);
                let mut statistics = TestSuiteStatistics::default();
                let mut failures = Vec::new();

                loop {
                    {
                        let mut shared_entries = shared_entries.lock().unwrap();

                        for _ in 0..chunk_size {
                            let entry = shared_entries.next();
                            match entry {
                                Some(entry) => entries.push(entry),
                                None => break,
                            }
                        }
                    }

                    if entries.is_empty() {
                        break;
                    }

                    for entry in entries.drain(..) {
                        handle_test_folder_entry(
                            &entry.unwrap(),
                            arguments,
                            &mut statistics,
                            &mut failures,
                        );
                    }

                    *shared_statistics.lock().unwrap() += &mem::take(&mut statistics);
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

    let failed_tests = Arc::try_unwrap(failed_tests)
        .unwrap_or_else(|_| unreachable!())
        .into_inner()
        .unwrap();

    let mut stdout = std::io::stdout().lock();

    for failed_test in &failed_tests {
        failed_test.print(diff_view, &mut stdout).unwrap();
    }

    if !failed_tests.is_empty() {
        let mut failed_tests = failed_tests;

        let invalid_tests: BTreeSet<_> = failed_tests
            .drain_filter(|test| matches!(test.failure, Failure::InvalidTest { .. }))
            .map(|test| test.path)
            .collect();

        let failed_tests: BTreeSet<_> = failed_tests
            .into_iter()
            .map(|failure| failure.path)
            .collect();

        if !failed_tests.is_empty() {
            writeln!(stdout).unwrap();
            writeln!(stdout, "{}", section_header("FAILED TESTS")).unwrap();
            writeln!(stdout).unwrap();

            for failed_test in failed_tests {
                let path = path::shorten(&failed_test);
                let path = path.to_string_lossy();
                writeln!(stdout, "  * {}", path).unwrap();
            }
        }

        if !invalid_tests.is_empty() {
            writeln!(stdout).unwrap();
            writeln!(stdout, "{}", section_header("INVALID TESTS")).unwrap();
            writeln!(stdout).unwrap();

            for invalid_test in invalid_tests {
                let path = path::shorten(&invalid_test);
                let path = path.to_string_lossy();
                writeln!(stdout, "  * {}", path).unwrap();
            }
        }
    }

    let summary = TestSuiteSummary {
        statistics,
        gilding: arguments.gilding,
        duration,
    };

    if summary.statistics.total_amount() == 0 {
        // no additional separator necessary if no tests were run
    } else {
        writeln!(stdout).unwrap();
        writeln!(stdout, "{}", section_separator()).unwrap();
        writeln!(stdout).unwrap();
    }

    writeln!(stdout, "{summary}").unwrap();

    writeln!(stdout, "{}", section_separator()).unwrap();
    writeln!(stdout).unwrap();

    if !summary.statistics.passed() {
        return Err(());
    }

    Ok(())
}

#[derive(Clone, Copy)]
struct SharedArguments {
    paths: &'static [PathBuf],
    gilding: Gilding,
    timeout: Option<Duration>,
    compiler_build_mode: CompilerBuildMode,
    inspecting: Inspecting,
}

impl From<cli::Arguments> for SharedArguments {
    fn from(arguments: cli::Arguments) -> Self {
        Self {
            paths: arguments.paths.leak(),
            gilding: arguments.gilding,
            timeout: arguments.timeout,
            compiler_build_mode: arguments.compiler_build_mode,
            inspecting: arguments.inspecting,
        }
    }
}

fn handle_test_folder_entry(
    entry: &walkdir::DirEntry,
    arguments: SharedArguments,
    statistics: &mut TestSuiteStatistics,
    failures: &mut Vec<FailedTest>,
) {
    // @Task improve error handling situation (too verbose and repetitive)

    let path = entry.path();

    // ignore folders; packages are recognized by their manifests (`package.metadata`) not their folder
    if entry.file_type().is_dir() {
        return;
    }

    // disallow symbolic links for now
    if entry.file_type().is_symlink() {
        print_file_status(path, Status::Invalid, None);
        failures.push(FailedTest::new(
            path.to_owned(),
            Failure::InvalidTest {
                message: "symbolic links are not allowed".into(),
                note: None,
            },
        ));
        statistics.invalid += 1;
        return;
    }

    // ignore
    // * `.gitignore` files which come with (generated) packages
    // * README files which allow testers to add notes to folders of tests
    if path.ends_with(".gitignore") || path.ends_with("README.txt") {
        return;
    }

    // skip golden files (after optional `--inspect` validation)
    // since they are processed when encountering Lushui files and packages
    if let Some(extension) = path.extension()
    && (extension == "stdout" || extension == "stderr")
    {
        if arguments.inspecting == Inspecting::Yes
            && !path.with_extension("lushui").exists()
            && !path.with_extension("metadata").exists()
        {
            print_file_status(path, Status::Invalid, None);
            failures.push(FailedTest::new(
                path.to_owned(),
                Failure::InvalidTest {
                    message: "the golden file does not have a corresponding test file".into(),
                    note: None,
                },
            ));
            statistics.invalid += 1;
        }

        return;
    }

    let Ok(type_) = classify_test(path) else {
        print_file_status(path, Status::Invalid, None);
        failures.push(FailedTest::new(
            path.to_owned(),
            Failure::InvalidTest {
                message: "the file extension is not supported".into(),
                note: Some(
                    "the file extension has to be one of \
                    ‘metadata’, ‘lushui’, ‘stderr’ or ‘stdout’"
                        .into(),
                ),
            },
        ));
        statistics.invalid += 1;
        return;
    };

    if !arguments.paths.is_empty()
        && !arguments
            .paths
            .iter()
            .any(|filter| path.starts_with(filter))
    {
        statistics.skipped += 1;
        return;
    }

    // parse the test configuration
    let test_source = fs::read_to_string(entry.path()).unwrap();
    let configuration = match Configuration::parse(&test_source, type_) {
        Ok(configuration) => configuration,
        Err(error) => {
            failures.push(FailedTest::new(
                path.to_owned(),
                Failure::InvalidConfiguration(error),
            ));
            print_file_status(path, Status::Invalid, None);
            statistics.invalid += 1;
            return;
        }
    };

    // skip auxiliary files (after optional `--inspect` validation)
    if let TestTag::Auxiliary { users } = configuration.tag {
        if arguments.inspecting == Inspecting::Yes {
            let result = validate_auxiliary_file(&users, path, failures);

            if result.is_err() {
                print_file_status(path, Status::Invalid, None);
                statistics.invalid += 1;
            }
        }

        return;
    }

    if let TestTag::Ignore = configuration.tag {
        print_file_status(path, Status::Ignored, None);
        statistics.ignored += 1;
        return;
    }

    let time = Instant::now();
    let output = compile(
        path,
        &configuration,
        type_,
        arguments.timeout,
        arguments.compiler_build_mode,
    );
    let duration = time.elapsed();

    let mut failed = false;

    const TIMEOUT_EXIT_STATUS: i32 = 124;

    if let Some(TIMEOUT_EXIT_STATUS) = output.status.code() {
        failures.push(FailedTest::new(
            path.to_owned(),
            Failure::Timeout {
                global: arguments.timeout,
                local: match configuration.timeout {
                    Timeout::Inherited => None,
                    Timeout::Overwritten(duration) => Some(duration.unwrap()),
                },
            },
        ));
        failed = true;
    } else {
        match (configuration.tag, output.status.success()) {
            (TestTag::Pass { .. }, false) | (TestTag::Fail { .. }, true) => {
                failures.push(FailedTest::new(
                    path.to_owned(),
                    Failure::UnexpectedExitStatus(output.status),
                ));
                failed = true;
            }
            (TestTag::Pass { .. }, true) | (TestTag::Fail { .. }, false) => {}
            (TestTag::Ignore | TestTag::Auxiliary { .. }, _) => unreachable!(),
        }
    }

    let mut gilded = match check_against_golden_file(
        &entry.path().with_extension("stdout"),
        output.stdout,
        Stream::Stdout,
        arguments.gilding,
    ) {
        Ok(gilded) => gilded,
        Err(error) => {
            failures.push(FailedTest::new(path.to_owned(), error));
            failed = true;
            false
        }
    };

    gilded |= match check_against_golden_file(
        &entry.path().with_extension("stderr"),
        output.stderr,
        Stream::Stderr,
        arguments.gilding,
    ) {
        Ok(gilded) => gilded,
        Err(error) => {
            failures.push(FailedTest::new(path.to_owned(), error));
            failed = true;
            false
        }
    };

    let status = if gilded {
        statistics.gilded += 1;
        Status::Gilded
    } else if failed {
        statistics.failed += 1;
        Status::Failure
    } else {
        statistics.passed += 1;
        Status::Ok
    };

    print_file_status(path, status, Some(duration));
}

pub(crate) fn print_file_status(path: &Path, status: Status, duration: Option<Duration>) {
    let padding = terminal_width() * 4 / 5;
    let mut stdout = std::io::stdout().lock();

    write!(
        stdout,
        "  {:<padding$} {}",
        path::shorten(path).to_string_lossy(),
        status
    )
    .unwrap();

    if let Some(duration) = duration {
        write!(stdout, " {}", format!("{duration:.2?}").bright_black()).unwrap();
    }

    writeln!(stdout).unwrap();
}

#[derive(Clone, Copy)]
pub(crate) enum Status {
    Ok,
    Ignored,
    Failure,
    Invalid,
    Gilded,
}

impl fmt::Display for Status {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Ok => "ok".green(),
                Self::Ignored => "ignored".yellow(),
                Self::Failure => "FAIL".red(),
                Self::Invalid => "INVALID".red(),
                Self::Gilded => "gilded".blue(),
            }
        )
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub(crate) enum TestType {
    SourceFile,
    Package,
    MetadataSourceFile,
}

impl fmt::Display for TestType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::SourceFile => "source file",
            Self::Package => "package",
            Self::MetadataSourceFile => "metadata source file",
        })
    }
}

fn classify_test(path: &Path) -> Result<TestType, ()> {
    let extension = path.extension().ok_or(())?;

    const METADATA_TEST_SUITE_NAME: &str = "metadata";

    if extension == "lushui" {
        Ok(TestType::SourceFile)
    } else if extension == "metadata" {
        if path
            .strip_prefix(path::test_folder())
            .unwrap()
            .starts_with(METADATA_TEST_SUITE_NAME)
        {
            Ok(TestType::MetadataSourceFile)
        } else {
            Ok(TestType::Package)
        }
    } else {
        Err(())
    }
}

fn validate_auxiliary_file(
    users: &[&str],
    path: &Path,
    failures: &mut Vec<FailedTest>,
) -> Result<(), ()> {
    if users.is_empty() {
        failures.push(FailedTest::new(
            path.to_owned(),
            Failure::InvalidTest {
                message: "the auxiliary file does not declare its users".into(),
                note: None,
            },
        ));
        return Err(());
    }
    // @Note validation not (yet) that sophisticated
    // we should probably check if the auxiliary file is actually used by
    // every single alleged user

    let mut invalid_users = Vec::new();

    for user in users {
        let user_path = Path::new(path::test_folder()).join(user);

        // @Task check if !*.lushui are folders
        // @Task use try_exists
        if !(user_path.exists() || user_path.with_extension("lushui").exists()) {
            invalid_users.push(user);
        }
    }

    if !invalid_users.is_empty() {
        let user_count = invalid_users.len();

        failures.push(FailedTest::new(
            path.to_owned(),
            Failure::InvalidTest {
                message: format!(
                    "the alleged {} {} of the auxiliary file {} not exist",
                    pluralize!(user_count, "user"),
                    invalid_users
                        .into_iter()
                        .map(|user| format!("‘{user}’"))
                        .join_with(", "),
                    pluralize!(user_count, "does", "do"),
                )
                .into(),
                note: None,
            },
        ));

        return Err(());
    }

    Ok(())
}

fn compile(
    path: &Path,
    configuration: &Configuration<'_>,
    type_: TestType,
    timeout: Option<Duration>,
    mode: CompilerBuildMode,
) -> std::process::Output {
    let compiler = path::compiler(mode);
    let mut command;

    let timeout = match configuration.timeout {
        Timeout::Inherited => timeout,
        Timeout::Overwritten(timeout) => timeout,
    };

    match timeout {
        Some(timeout) => {
            let timeout = match timeout.as_secs() {
                0 => "0.0001".to_string(),
                timeout => timeout.to_string(),
            };

            command = Command::new("timeout");
            command.arg(timeout);
            command.arg(compiler);
        }
        None => {
            command = Command::new(compiler);
        }
    }

    command.arg("--quiet");

    if type_ == TestType::SourceFile {
        command.arg("file");
    }

    let (TestTag::Pass { mode } | TestTag::Fail { mode }) = configuration.tag else {
        unreachable!();
    };

    {
        use TestType::*;

        command.arg(match (type_, mode) {
            (SourceFile | Package, Mode::Check) => "check",
            (SourceFile | Package, Mode::Build) => "build",
            (SourceFile | Package, Mode::Run) => "run",
            (MetadataSourceFile, Mode::Check) => "metadata",
            (MetadataSourceFile, _) => unreachable!(),
        });
    }

    // The default component type for source files is library. This is highly unpractical for the current majority of
    // our source file tests which check the correctness of the compiler in regards to language features and
    // which do not want to run anything. So let's overwrite it unless the test configuration wants to `run` the code
    // or manually supplied a component type. Keep in mind, this is only a simple heuristic.
    // @Task find a more principled approach
    if type_ == TestType::SourceFile
        && mode != Mode::Run
        && configuration
            .compiler_args
            .iter()
            .all(|argument| !argument.starts_with("--component-type"))
    {
        command.arg("--component-type=library");
    }

    command.args(&configuration.compiler_args);
    command.envs(&configuration.compiler_env_vars);

    match type_ {
        TestType::SourceFile | TestType::MetadataSourceFile => {
            command.arg(path);
        }
        TestType::Package => {
            // the folder containing the package manifest
            command.arg(path.parent().unwrap());
        }
    }

    command.output().unwrap()
}

fn check_against_golden_file(
    golden_file_path: &Path,
    actual: Vec<u8>,
    stream: Stream,
    gilding: Gilding,
) -> Result<bool, Failure> {
    let actual = String::from_utf8(actual).unwrap();

    let golden = if golden_file_path.exists() {
        stream.preprocess_before_comparison(fs::read_to_string(golden_file_path).unwrap())
    } else {
        String::new()
    };

    if actual != golden {
        if gilding == Gilding::No {
            Err(Failure::GoldenFileMismatch {
                golden,
                actual,
                stream,
            })
        } else {
            let actual = stream.preprocess_before_generating(actual);
            fs::write(golden_file_path, actual).unwrap();
            Ok(true)
        }
    } else {
        Ok(false)
    }
}

#[derive(Clone, Copy)]
enum Stream {
    Stdout,
    Stderr,
}

impl Stream {
    const TEST_FOLDER_PATH_VARIABLE: &'static str = "${TEST_FOLDER}";
    const DISTRIBUTED_LIBRARIES_PATH_VARIABLE: &'static str = "${DISTRIBUTED_LIBRARIES_FOLDER}";

    fn preprocess_before_comparison(self, stream: String) -> String {
        match self {
            Self::Stdout => stream,
            // @Task replace this naive replacement method: implement
            //       * escaping via `$$`
            //       * simultaneous replacement
            Self::Stderr => stream
                .replace(Self::TEST_FOLDER_PATH_VARIABLE, path::test_folder())
                .replace(
                    Self::DISTRIBUTED_LIBRARIES_PATH_VARIABLE,
                    path::distributed_libraries(),
                ),
        }
    }

    fn preprocess_before_generating(self, stream: String) -> String {
        match self {
            Self::Stdout => stream,
            // @Task replace this naive replacement method: implement
            //       * escaping via `$$`
            //       * simultaneous replacement
            Self::Stderr => stream
                .replace(path::test_folder(), Self::TEST_FOLDER_PATH_VARIABLE)
                .replace(
                    path::distributed_libraries(),
                    Self::DISTRIBUTED_LIBRARIES_PATH_VARIABLE,
                ),
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

fn section_separator() -> String {
    "=".repeat(terminal_width())
}

fn section_header(title: &str) -> String {
    use unicode_width::UnicodeWidthStr;

    let mut header = format!("==== {title} ");
    let width = header.width();
    header += &"=".repeat(terminal_width().saturating_sub(width));
    header
}

fn terminal_width() -> usize {
    static WIDTH: LazyLock<usize> =
        LazyLock::new(|| terminal_size::terminal_size().map_or(120, |(width, _)| width.0 as _));

    *WIDTH
}

#[derive(Clone, Copy)]
enum CompilerBuildMode {
    Debug,
    Release,
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum Gilding {
    Yes,
    No,
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum Inspecting {
    Yes,
    No,
}

mod path {
    use std::{
        borrow::Cow,
        env::consts::EXE_EXTENSION,
        path::{Path, PathBuf},
        sync::LazyLock,
    };

    use crate::CompilerBuildMode;

    pub(crate) fn shorten(path: &Path) -> Cow<'_, Path> {
        pathdiff::diff_paths(path, current_folder()).map_or(path.into(), Into::into)
    }

    pub(crate) fn current_folder() -> &'static Path {
        static PATH: LazyLock<PathBuf> = LazyLock::new(|| std::env::current_dir().unwrap());

        &PATH
    }

    pub(crate) fn compiler_manifest() -> &'static Path {
        static PATH: LazyLock<PathBuf> =
            LazyLock::new(|| Path::new(env!("CARGO_MANIFEST_DIR")).join("../../../Cargo.toml"));

        &PATH
    }

    pub(crate) fn compiler(mode: CompilerBuildMode) -> &'static Path {
        static DEBUG_PATH: LazyLock<PathBuf> = LazyLock::new(|| path("debug"));
        static RELEASE_PATH: LazyLock<PathBuf> = LazyLock::new(|| path("release"));

        fn path(mode: &str) -> PathBuf {
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("../../../target")
                .join(mode)
                .join("main")
                .with_extension(EXE_EXTENSION)
        }

        match mode {
            CompilerBuildMode::Debug => &DEBUG_PATH,
            CompilerBuildMode::Release => &RELEASE_PATH,
        }
    }

    // @Task return a Path if possible
    pub(crate) fn test_folder() -> &'static str {
        static PATH: LazyLock<String> = LazyLock::new(|| {
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("../test")
                .canonicalize()
                .unwrap()
                .to_str()
                .unwrap()
                .to_owned()
        });

        &PATH
    }

    // @Task return a Path if possible
    pub(crate) fn distributed_libraries() -> &'static str {
        static PATH: LazyLock<String> = LazyLock::new(|| {
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("../../../library")
                .canonicalize()
                .unwrap()
                .to_str()
                .unwrap()
                .to_owned()
        });

        &PATH
    }
}
