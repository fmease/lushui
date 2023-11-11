#![feature(const_option, lazy_cell, let_chains, str_split_remainder, extract_if)]

use configuration::{Configuration, Mode, TestTag, Timeout};
use failure::{FailedTest, Failure};
use joinery::JoinableIterator;
use span::SourceMap;
use std::{
    collections::BTreeSet,
    fmt, fs,
    io::{self, Write},
    path::{Path, PathBuf},
    process::{Command, ExitCode, Stdio},
    sync::{Arc, LazyLock, Mutex},
    time::{Duration, Instant},
};
use summary::{TestSuiteStatistics, TestSuiteSummary};
use utility::{
    paint::{AnsiColor, ColorChoice, Painter},
    pluralize,
};

mod cli;
mod configuration;
mod failure;
mod summary;

fn main() -> ExitCode {
    match try_main() {
        Ok(()) => ExitCode::SUCCESS,
        Err(()) => ExitCode::FAILURE,
    }
}

// FIXME: Currently we are traversing the whole test folder and filtering out
//        any tests that don't match the given list of paths.
//        However, that means we are wasting a huge amount of time.
//        This does not scale.
//        Instead, only pass the relevant paths to `walkdir` (smh.) (if possible)

fn try_main() -> Result<(), ()> {
    // FIXME: Add `--color`.
    let arguments = cli::arguments()?;

    let mut stdout = Painter::stdout(ColorChoice::Always);
    let mut stderr = Painter::stderr(ColorChoice::Auto);

    if arguments.gilding == Gilding::Yes && !confirm_gilding(&mut stdout).unwrap() {
        return Err(());
    }

    title("note", AnsiColor::Cyan, &mut stdout).unwrap();
    writeln!(stdout, "building the compiler...").unwrap();
    stdout.flush().unwrap();

    let output = Command::new("cargo")
        .args(["+nightly", "--color=always", "build", "--manifest-path"])
        .arg(path::compiler_manifest())
        .args(match arguments.compiler_build_mode {
            #[allow(clippy::needless_borrow)] // false positive, maybe #9111
            CompilerBuildMode::Debug => &[],
            CompilerBuildMode::Release => &["--release"][..],
        })
        .stdin(Stdio::null())
        .output()
        .unwrap();

    if !output.status.success() {
        title("error", AnsiColor::Red, &mut stderr).unwrap();
        writeln!(stderr, "the compiler failed to build").unwrap();

        writeln!(stderr).unwrap();
        writeln!(stderr, "{}", section_header("RUST/CARGO STDERR")).unwrap();
        writeln!(stderr).unwrap();
        stderr.write_all(&output.stderr).unwrap();
        stderr.flush().unwrap();

        return Err(());
    }

    writeln!(stderr).unwrap();
    writeln!(stderr, "{}", section_separator()).unwrap();
    writeln!(stderr).unwrap();
    stderr.flush().unwrap();

    // Unlock stdout and stderr.
    drop(stdout);
    drop(stderr);

    let test_folder_entries = Arc::new(Mutex::new(
        walkdir::WalkDir::new(path::test_folder()).into_iter(),
    ));
    let number_test_threads = arguments.number_test_threads.into();
    let diff_view = arguments.diff_view;
    let arguments = arguments.into();

    let suite_time = Instant::now();

    #[allow(clippy::needless_collect)] // false positive
    let handles: Vec<_> = (0..number_test_threads)
        .map(|_| {
            let shared_entries = test_folder_entries.clone();

            std::thread::spawn(move || {
                let chunk_size = number_test_threads;
                let mut entries = Vec::with_capacity(chunk_size);
                let mut statistics = TestSuiteStatistics::default();
                let mut failed_tests = Vec::new();
                let mut map = SourceMap::default();

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
                            &mut failed_tests,
                            &mut map,
                        );
                    }
                }

                (statistics, failed_tests, map)
            })
        })
        .collect();

    let thread_local_data: Vec<_> = handles
        .into_iter()
        .map(|handle| handle.join().unwrap())
        .collect();

    let duration = suite_time.elapsed();
    let mut statistics = TestSuiteStatistics::default();
    let mut failed_tests = Vec::new();
    let mut stdout = Painter::stdout(ColorChoice::Always);

    for (local_statistics, mut local_failed_tests, map) in thread_local_data {
        if !local_failed_tests.is_empty() {
            writeln!(stdout).unwrap();
            writeln!(stdout, "{}", section_separator()).unwrap();
            writeln!(stdout).unwrap();

            for failed_test in &local_failed_tests {
                failed_test.print(diff_view, &map, &mut stdout).unwrap();
                writeln!(stdout).unwrap();
            }
        }

        statistics += &local_statistics;
        failed_tests.append(&mut local_failed_tests);
    }

    if !failed_tests.is_empty() {
        let invalid_tests: BTreeSet<_> = failed_tests
            .extract_if(|test| {
                matches!(
                    test.failure,
                    Failure::InvalidTest { .. } | Failure::InvalidConfiguration(_)
                )
            })
            .map(|test| test.path)
            .collect();

        let failed_tests: BTreeSet<_> = failed_tests
            .into_iter()
            .map(|failure| failure.path)
            .collect();

        if !failed_tests.is_empty() {
            writeln!(stdout, "{}", section_header("FAILED TESTS")).unwrap();
            writeln!(stdout).unwrap();

            for failed_test in failed_tests {
                writeln!(stdout, "  {}", path::shorten(&failed_test).display()).unwrap();
            }
        }

        if !invalid_tests.is_empty() {
            writeln!(stdout).unwrap();
            writeln!(stdout, "{}", section_header("INVALID TESTS")).unwrap();
            writeln!(stdout).unwrap();

            for invalid_test in invalid_tests {
                writeln!(stdout, "  {}", path::shorten(&invalid_test).display()).unwrap();
            }
        }
    }

    let summary = TestSuiteSummary {
        statistics,
        gilding: arguments.gilding,
        duration,
    };

    if summary.statistics.total_amount() == 0 {
        // No additional separator necessary if no tests were run.
    } else {
        writeln!(stdout).unwrap();
        writeln!(stdout, "{}", section_separator()).unwrap();
        writeln!(stdout).unwrap();
    }

    summary.render(&mut stdout).unwrap();
    writeln!(stdout).unwrap();

    writeln!(stdout, "{}", section_separator()).unwrap();
    writeln!(stdout).unwrap();

    stdout.flush().unwrap();

    if !summary.statistics.passed() {
        return Err(());
    }

    Ok(())
}

fn confirm_gilding(painter: &mut Painter) -> io::Result<bool> {
    title("prompt", AnsiColor::Yellow, painter)?;

    write!(
        painter,
        "would you really like to gild all valid failing tests? ["
    )?;
    painter.set(AnsiColor::Green)?;
    write!(painter, "y")?;
    painter.unset()?;
    write!(painter, "/")?;
    painter.set(AnsiColor::Red).unwrap();
    write!(painter, "N")?;
    painter.unset()?;
    write!(painter, "] ")?;
    painter.flush()?;

    let mut answer = String::new();
    io::stdin().read_line(&mut answer)?;
    let answer = answer.trim();

    if answer.eq_ignore_ascii_case("y") || answer.eq_ignore_ascii_case("yes") {
        writeln!(painter)?;
        painter.flush()?;

        return Ok(true);
    }

    Ok(false)
}

fn title(content: &str, color: AnsiColor, painter: &mut Painter) -> io::Result<()> {
    painter.set(color)?;
    write!(painter, "{content}")?;
    painter.unset()?;
    write!(painter, ": ")
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
    failed_tests: &mut Vec<FailedTest>,
    map: &mut SourceMap,
) {
    // @Task improve error handling situation (too verbose and repetitive)

    let path = entry.path();

    // ignore folders; packages are recognized by their manifests (`package.recnot`) not their folder
    if entry.file_type().is_dir() {
        return;
    }

    // disallow symbolic links for now
    if entry.file_type().is_symlink() {
        print_file_status(path, Status::Invalid, None).unwrap();
        failed_tests.push(FailedTest::new(
            path.to_owned(),
            Failure::InvalidTest {
                message: "symbolic links are not allowed".into(),
                note: None,
                span: None,
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
            && !path.with_extension("recnot").exists()
        {
            print_file_status(path, Status::Invalid, None).unwrap();
            failed_tests.push(FailedTest::new(
                path.to_owned(),
                Failure::InvalidTest {
                    message: "the golden file does not have a corresponding test file".into(),
                    note: None,
                    span: None,
                },
            ));
            statistics.invalid += 1;
        }

        return;
    }

    let Ok(type_) = classify_test(path) else {
        print_file_status(path, Status::Invalid, None).unwrap();
        failed_tests.push(FailedTest::new(
            path.to_owned(),
            Failure::InvalidTest {
                message: "the file extension is not supported".into(),
                note: Some(
                    "the file extension has to be one of \
                     ‘lushui’, ‘recnot’, ‘stderr’ or ‘stdout’"
                        .into(),
                ),
                span: None,
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
    let file = map.load(&path::shorten(path), None).unwrap();
    let configuration = match Configuration::parse(&map[file], type_, map) {
        Ok(configuration) => configuration,
        Err(error) => {
            failed_tests.push(FailedTest::new(
                path.to_owned(),
                Failure::InvalidConfiguration(error),
            ));
            print_file_status(path, Status::Invalid, None).unwrap();
            statistics.invalid += 1;
            return;
        }
    };

    // skip auxiliary files (after optional `--inspect` validation)
    if let TestTag::Auxiliary { users } = configuration.tag {
        if arguments.inspecting == Inspecting::Yes {
            let result = validate_auxiliary_file(&users, path, failed_tests);

            if result.is_err() {
                print_file_status(path, Status::Invalid, None).unwrap();
                statistics.invalid += 1;
            }
        }

        return;
    }

    if let TestTag::Ignore = configuration.tag {
        print_file_status(path, Status::Ignored, None).unwrap();
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
        failed_tests.push(FailedTest::new(
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
                failed_tests.push(FailedTest::new(
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
        &path.with_extension("stdout"),
        output.stdout,
        Stream::Stdout,
        arguments.gilding,
    ) {
        Ok(gilded) => gilded,
        Err(error) => {
            failed_tests.push(FailedTest::new(path.to_owned(), error));
            failed = true;
            false
        }
    };

    gilded |= match check_against_golden_file(
        &path.with_extension("stderr"),
        output.stderr,
        Stream::Stderr,
        arguments.gilding,
    ) {
        Ok(gilded) => gilded,
        Err(error) => {
            failed_tests.push(FailedTest::new(path.to_owned(), error));
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

    print_file_status(path, status, Some(duration)).unwrap();
}

pub(crate) fn print_file_status(
    path: &Path,
    status: Status,
    duration: Option<Duration>,
) -> io::Result<()> {
    let padding = terminal_width() * 4 / 5;
    let mut stdout = Painter::stdout(ColorChoice::Auto);

    write!(stdout, "  {:<padding$} ", path::shorten(path).display())?;
    stdout.set(status.color())?;
    write!(stdout, "{}", status.name())?;
    stdout.unset()?;

    if let Some(duration) = duration {
        stdout.set(AnsiColor::BrightBlack)?;
        write!(stdout, " {duration:.2?}")?;
        stdout.unset()?;
    }

    writeln!(stdout)?;
    stdout.flush()
}

#[derive(Clone, Copy)]
pub(crate) enum Status {
    Ok,
    Ignored,
    Failure,
    Invalid,
    Gilded,
}

impl Status {
    const fn name(self) -> &'static str {
        match self {
            Self::Ok => "ok",
            Self::Ignored => "ignored",
            Self::Failure => "FAIL",
            Self::Invalid => "INVALID",
            Self::Gilded => "gilded",
        }
    }

    const fn color(self) -> AnsiColor {
        match self {
            Self::Ok => AnsiColor::Green,
            Self::Ignored => AnsiColor::Yellow,
            Self::Failure | Self::Invalid => AnsiColor::Red,
            Self::Gilded => AnsiColor::Blue,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub(crate) enum TestType {
    SourceFile,
    Package,
    RecnotSourceFile,
}

impl fmt::Display for TestType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::SourceFile => "source file",
            Self::Package => "package",
            Self::RecnotSourceFile => "Recnot source file",
        })
    }
}

fn classify_test(path: &Path) -> Result<TestType, ()> {
    let extension = path.extension().ok_or(())?;

    const RECNOT_TEST_SUITE_NAME: &str = "recnot";

    if extension == "lushui" {
        Ok(TestType::SourceFile)
    } else if extension == "recnot" {
        if path
            .strip_prefix(path::test_folder())
            .unwrap()
            .starts_with(RECNOT_TEST_SUITE_NAME)
        {
            Ok(TestType::RecnotSourceFile)
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
    // @Beacon @Task make the list of users compulsively non-empty in
    //               crate::configuration
    if users.is_empty() {
        failures.push(FailedTest::new(
            path.to_owned(),
            Failure::InvalidTest {
                message: "the auxiliary file does not declare its users".into(),
                note: None,
                span: None,
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
                // @Task actually point to the undefined user
                span: None,
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
            (RecnotSourceFile, Mode::Check) => "recnot",
            (RecnotSourceFile, _) => unreachable!(),
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
        TestType::SourceFile | TestType::RecnotSourceFile => {
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
                .join("../../../test/ui")
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
