#![feature(default_free_fn, const_option, once_cell, let_else)]
#![forbid(rust_2018_idioms, unused_must_use)]
#![warn(clippy::pedantic)]
#![allow(
    clippy::items_after_statements,
    clippy::enum_glob_use,
    clippy::must_use_candidate,
    clippy::missing_errors_doc,
    clippy::too_many_lines,
    clippy::module_name_repetitions,
    clippy::match_bool,
    clippy::empty_enum,
    clippy::single_match_else,
    clippy::if_not_else,
    clippy::blocks_in_if_conditions, // too many false positives with rustfmt's output
    clippy::semicolon_if_nothing_returned, // @Temporary false positives with let/else, still
)]

use colored::Colorize;
use configuration::{Configuration, Language, TestTag};
use failure::{Failure, FailureKind, File, FileType};
use joinery::JoinableIterator;
use std::{
    borrow::Cow,
    default::default,
    ffi::OsStr,
    fmt, fs,
    io::Write,
    lazy::SyncLazy,
    mem,
    path::{Path, PathBuf},
    process::{Command, ExitCode, Stdio},
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};
use summary::{TestSuiteStatistics, TestSuiteSummary};

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

fn try_main() -> Result<(), ()> {
    let application = cli::Application::new();

    if application.gilding == Gilding::Yes {
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

    println!("Building the compiler...");

    // @Task capture stderr (errors + warnings)
    // save those warnings so we can remove it as the prefix of the following tests
    // but also add a note that some stuff was written to stderr (but status is ok)
    let status = Command::new("cargo")
        .arg("build")
        .arg("--manifest-path")
        .arg(compiler_manifest_path())
        .args(match application.compiler_build_mode {
            CompilerBuildMode::Debug => &[],
            CompilerBuildMode::Release => &["--release"][..],
        })
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .unwrap();

    if !status.success() {
        eprintln!("{}", "The compiler failed to build.".red());
        return Err(());
    }

    println!();
    println!("{}", "=".repeat(terminal_width()));
    println!();

    // @Beacon @Question alternative to Arcs??? since we have a fixed amount of threads
    // currently, the Arc are deallocated "when joining" (just before but yeah)
    let test_folder_entries = Arc::new(Mutex::new(
        walkdir::WalkDir::new(test_folder_path()).into_iter(),
    ));
    let statistics: Arc<Mutex<TestSuiteStatistics>> = default();
    let failures: Arc<Mutex<Vec<_>>> = default();
    let filters = Filters {
        strict: application.strict_filters.leak(),
        loose: application.loose_filters.leak(),
    };
    let number_test_threads = application.number_test_threads.into();

    let suite_time = Instant::now();

    #[allow(clippy::needless_collect)] // false positive
    let handles: Vec<_> = (0..number_test_threads)
        .map(|_| {
            let shared_entries = test_folder_entries.clone();
            let shared_statistics = statistics.clone();
            let shared_failures = failures.clone();

            std::thread::spawn(move || {
                let chunk_size = number_test_threads;
                let mut entries = Vec::with_capacity(chunk_size);
                let mut statistics = TestSuiteStatistics::default();
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
                        handle_test_folder_entry(
                            &entry.unwrap(),
                            filters,
                            application.gilding,
                            application.timeout,
                            application.inspecting,
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

    let failures = Arc::try_unwrap(failures)
        .unwrap_or_else(|_| unreachable!())
        .into_inner()
        .unwrap();

    for failure in failures {
        println!("{failure}");
    }

    let summary = TestSuiteSummary {
        statistics,
        gilding: application.gilding,
        duration,
        filters,
    };

    if summary.statistics.total_amount() != 0 {
        // no additional separator necessary if no tests were run

        println!();
        println!("{}", "=".repeat(terminal_width()));
        println!();
    }

    println!("{summary}");

    println!("{}", "=".repeat(terminal_width()));
    println!();

    if !summary.statistics.passed() {
        return Err(());
    }

    Ok(())
}

fn handle_test_folder_entry(
    entry: &walkdir::DirEntry,
    filters: Filters,
    gilding: Gilding,
    timeout: Option<Duration>,
    inspecting: Inspecting,
    statistics: &mut TestSuiteStatistics,
    failures: &mut Vec<Failure>,
) {
    // @Task improve error handling situation (too verbose and repetitive)

    let path = entry.path();

    // ignore folders; packages are recognized by their manifests (`package.metadata`) not their folder
    if entry.file_type().is_dir() {
        return;
    }

    let shortened_path = path.strip_prefix(test_folder_path()).unwrap();

    // disallow symbolic links for now
    if entry.file_type().is_symlink() {
        let path = shortened_path.to_string_lossy().to_string();
        print_file_status(&path, Status::Invalid, None);
        failures.push(Failure::new(
            File::new(path, FileType::Other),
            FailureKind::invalid_file("Symbolic links are not allowed"),
        ));
        statistics.invalid += 1;
        return;
    }

    // ignore gitignores which come with (generated) packages
    if shortened_path.ends_with(".gitignore") {
        return;
    }

    // disallow files without an extension
    let Some(extension) = shortened_path.extension().and_then(OsStr::to_str) else {
            let path = shortened_path.to_string_lossy().to_string();
            print_file_status(&path, Status::Invalid, None);
            failures.push(Failure::new(
                File::new(path, FileType::Other),
                FailureKind::invalid_file("The file does not have an extension or it is not valid UTF-8"),
            ));
            statistics.invalid += 1;
            return;
        };

    // skip golden files (after optional `--inspect` validation)
    // since they are processed when encountering Lushui files and packages
    if extension == "stdout" || extension == "stderr" {
        #[allow(clippy::collapsible_if)]
        if inspecting == Inspecting::Yes {
            if !path.with_extension("lushui").exists() && !path.with_extension("metadata").exists()
            {
                let path = shortened_path.to_string_lossy().to_string();
                print_file_status(&path, Status::Invalid, None);
                failures.push(Failure::new(
                    File::new(path, FileType::Golden),
                    FailureKind::invalid_file(
                        "The golden file does not have a corresponding test file",
                    ),
                ));
                statistics.invalid += 1;
            }
        }

        return;
    }

    let type_ = match extension {
        "lushui" => TestType::SourceFile,
        // @Beacon @Task instead of deciding *here* if it's a Package or a MetadataSourceFile
        //               and instead of judging by its file name, parse the test configuration
        //               and discern the TestType by the TestTag:
        //                   tag "metadata" => TestType::MetadataSourceFile
        //                   tag _ => TestType::Package
        "metadata" if shortened_path.ends_with("package.metadata") => TestType::Package,
        "metadata" => TestType::MetadataSourceFile,
        _ => {
            let path = shortened_path.to_string_lossy().to_string();
            print_file_status(&path, Status::Invalid, None);
            failures.push(Failure::new(
                File::new(path, FileType::Other),
                FailureKind::invalid_file(
                    "The file extension is not one of `metadata`, `lushui`, `stderr` or `stdout`",
                ),
            ));
            statistics.invalid += 1;
            return;
        }
    };

    // obtain a "legible path" (valid UTF-8) to be shown to the user
    let legible_path = {
        let legible_path: Cow<'_, _> = match type_ {
            TestType::SourceFile | TestType::MetadataSourceFile => {
                shortened_path.with_extension("").into()
            }
            TestType::Package => shortened_path.parent().unwrap().into(),
        };

        match legible_path.to_str() {
            Some(path) => path.to_owned(),
            None => {
                let path = shortened_path.to_string_lossy().to_string();
                print_file_status(&path, Status::Invalid, None);
                failures.push(Failure::new(
                    File::new(path, type_.file_type_package_means_manifest()),
                    FailureKind::invalid_file("The file path contains invalid UTF-8"),
                ));
                statistics.invalid += 1;
                return;
            }
        }
    };

    // apply the supplied filters and skip the current file if they do
    if !filters.is_empty() {
        let is_included = filters
            .loose
            .iter()
            .any(|filter| legible_path.contains(filter))
            || filters.strict.iter().any(|filter| legible_path == *filter);

        if !is_included {
            statistics.skipped += 1;
            return;
        }
    }

    let language = match type_ {
        TestType::Package | TestType::MetadataSourceFile => Language::Metadata,
        TestType::SourceFile => Language::Lushui,
    };

    // parse the test configuration
    let test_file = fs::read_to_string(entry.path()).unwrap();
    let configuration = match Configuration::parse(&test_file, language) {
        Ok(configuration) => configuration,
        Err(error) => {
            failures.push(Failure::new(
                File::new(
                    legible_path.clone(),
                    type_.file_type_package_means_manifest(),
                ),
                FailureKind::invalid_file(error.to_string()),
            ));
            print_file_status(&legible_path, Status::Invalid, None);
            statistics.invalid += 1;
            return;
        }
    };

    // skip auxiliary files (after optional `--inspect` validation)
    if let TestTag::Auxiliary = configuration.tag {
        if inspecting == Inspecting::Yes {
            let result = validate_auxiliary_file(&configuration.arguments, &legible_path, failures);

            if result.is_err() {
                print_file_status(&legible_path, Status::Invalid, None);
                statistics.invalid += 1;
            }
        }

        return;
    }

    if let TestTag::Ignore = configuration.tag {
        print_file_status(&legible_path, Status::Ignored, None);
        statistics.ignored += 1;
        return;
    }

    let time = Instant::now();
    let output = compile(path, &configuration.arguments, type_, timeout);
    let duration = time.elapsed();

    let mut failed = false;

    // @Task simplify and DRY this logic!
    if let Some(124) = output.status.code() {
        failures.push(Failure::new(
            File::new(legible_path.clone(), type_.into()),
            FailureKind::Timeout,
        ));
        failed = true;
    } else {
        match (configuration.tag, output.status.success()) {
            (TestTag::Pass, true) | (TestTag::Fail, false) => {}
            (TestTag::Pass, false) => {
                failures.push(Failure::new(
                    File::new(legible_path.clone(), type_.into()),
                    FailureKind::UnexpectedFail(output.status),
                ));
                failed = true;
            }
            (TestTag::Fail, true) => {
                failures.push(Failure::new(
                    File::new(legible_path.clone(), type_.into()),
                    FailureKind::UnexpectedPass,
                ));
                failed = true;
            }
            (TestTag::Ignore | TestTag::Auxiliary, _) => unreachable!(),
        }
    }

    let mut gilded = match check_against_golden_file(
        &entry.path().with_extension("stdout"),
        output.stdout,
        Stream::Stdout,
        gilding,
    ) {
        Ok(gilded) => gilded,
        Err(error) => {
            failures.push(Failure::new(
                File::new(legible_path.clone(), type_.into()),
                error,
            ));
            failed = true;
            false
        }
    };

    gilded |= match check_against_golden_file(
        &entry.path().with_extension("stderr"),
        output.stderr,
        Stream::Stderr,
        gilding,
    ) {
        Ok(gilded) => gilded,
        Err(error) => {
            failures.push(Failure::new(
                File::new(legible_path.clone(), type_.into()),
                error,
            ));
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

    print_file_status(&legible_path, status, Some(duration));
}

pub(crate) fn print_file_status(path: &str, status: Status, duration: Option<Duration>) {
    let padding = terminal_width() * 4 / 5;
    let mut stdout = std::io::stdout().lock();

    write!(stdout, "  {:<padding$} {}", path, status).unwrap();

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

fn validate_auxiliary_file(
    arguments: &[&str],
    legible_path: &str,
    failures: &mut Vec<Failure>,
) -> Result<(), ()> {
    if arguments.is_empty() {
        failures.push(Failure::new(
            File::new(legible_path.to_owned(), FileType::Auxiliary),
            FailureKind::invalid_file("The auxiliary file does not declare its users"),
        ));
        return Err(());
    }
    // @Note validation not (yet) that sophisticated
    // we should probably check if the auxiliary file is actually used by
    // every single alleged user

    let mut invalid_users = Vec::new();

    for user in arguments {
        let user_path = Path::new(test_folder_path()).join(user);

        // @Task check if !*.lushui are folders
        // @Task use try_exists
        if !(user_path.exists() || user_path.with_extension("lushui").exists()) {
            invalid_users.push(user);
        }
    }

    if !invalid_users.is_empty() {
        let user_count = invalid_users.len();

        failures.push(Failure::new(
            File::new(legible_path.to_owned(), FileType::Auxiliary),
            FailureKind::invalid_file(format!(
                "The alleged {} {} of the auxiliary file {} not exist",
                if user_count == 1 { "user" } else { "users" },
                invalid_users
                    .into_iter()
                    .map(|user| format!("`{user}`"))
                    .join_with(", "),
                if user_count == 1 { "does" } else { "do" },
            )),
        ));

        return Err(());
    }

    Ok(())
}

fn compile(
    path: &Path,
    arguments: &[&str],
    type_: TestType,
    timeout: Option<Duration>,
) -> std::process::Output {
    // @Beacon @Question how can we filter out rustc's warnings from stderr?

    let mut command = Command::new(if timeout.is_some() {
        "timeout"
    } else {
        "cargo"
    });

    if let Some(timeout) = timeout {
        command.arg(timeout.as_secs().to_string());
        command.arg("cargo");
    }

    command
        .args(["run", "--quiet"])
        .arg("--manifest-path")
        .arg(compiler_manifest_path())
        .arg("--")
        .arg("--quiet");

    if type_ == TestType::SourceFile {
        command.arg("file");
    }

    command.args(arguments);

    // The default component type for source files is library. This is highly unpractical for the current majority of
    // our source file tests which check the correctness of the compiler in regards to language features and
    // which do not want to run anything. So let's overwrite it unless the test configuration wants to `run` the code
    // or manually supplied a component type. Keep in mind, this is only a simple heuristic.
    if type_ == TestType::SourceFile
        && arguments.first().map_or(false, |&command| command != "run")
        && arguments
            .iter()
            .all(|argument| !argument.starts_with("--component-type"))
    {
        command.arg("--component-type=library");
    }

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

#[derive(PartialEq, Eq, Clone, Copy)]
enum TestType {
    SourceFile,
    Package,
    MetadataSourceFile,
}

impl TestType {
    pub(crate) fn file_type_package_means_manifest(self) -> FileType {
        match self {
            Self::SourceFile => FileType::SourceFile,
            Self::Package => FileType::PackageManifest,
            Self::MetadataSourceFile => FileType::MetadataSourceFile,
        }
    }
}

impl From<TestType> for FileType {
    fn from(type_: TestType) -> Self {
        match type_ {
            TestType::SourceFile => Self::SourceFile,
            TestType::Package => Self::Package,
            TestType::MetadataSourceFile => Self::MetadataSourceFile,
        }
    }
}

fn check_against_golden_file(
    golden_file_path: &Path,
    actual: Vec<u8>,
    stream: Stream,
    gilding: Gilding,
) -> Result<bool, FailureKind> {
    let actual = String::from_utf8(actual).unwrap();

    let golden = if golden_file_path.exists() {
        stream.preprocess_before_comparison(fs::read_to_string(golden_file_path).unwrap())
    } else {
        String::new()
    };

    if actual != golden {
        if gilding == Gilding::No {
            Err(FailureKind::GoldenFileMismatch {
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
struct Filters {
    strict: &'static [String],
    loose: &'static [String],
}

impl Filters {
    const fn is_empty(self) -> bool {
        self.strict.is_empty() && self.loose.is_empty()
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
                .replace(Self::TEST_FOLDER_PATH_VARIABLE, test_folder_path())
                .replace(
                    Self::DISTRIBUTED_LIBRARIES_PATH_VARIABLE,
                    distributed_libraries_path(),
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
                .replace(test_folder_path(), Self::TEST_FOLDER_PATH_VARIABLE)
                .replace(
                    distributed_libraries_path(),
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

fn compiler_manifest_path() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../../Cargo.toml")
}

fn terminal_width() -> usize {
    static TERMINAL_WIDTH: SyncLazy<usize> =
        SyncLazy::new(|| terminal_size::terminal_size().map_or(100, |size| size.0 .0 as _));

    *TERMINAL_WIDTH
}

fn test_folder_path() -> &'static str {
    static TEST_FOLDER_PATH: SyncLazy<String> = SyncLazy::new(|| {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../tests")
            .canonicalize()
            .unwrap()
            .to_str()
            .unwrap()
            .to_owned()
    });

    &TEST_FOLDER_PATH
}

fn distributed_libraries_path() -> &'static str {
    static TEST_FOLDER_PATH: SyncLazy<String> = SyncLazy::new(|| {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../../libraries")
            .canonicalize()
            .unwrap()
            .to_str()
            .unwrap()
            .to_owned()
    });

    &TEST_FOLDER_PATH
}

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
