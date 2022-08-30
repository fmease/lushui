use crate::{failure::DiffView, CompilerBuildMode, Gilding, Inspecting};
use clap::{
    builder::{PathBufValueParser, TypedValueParser},
    Arg, ArgAction, Command,
};
use derivation::Elements;
use diagnostics::{Diagnostic, Reporter};
use std::{ffi::OsStr, num::NonZeroUsize, path::PathBuf, time::Duration};

pub(crate) fn arguments() -> Result<Arguments, ()> {
    let available_parallelism =
        std::thread::available_parallelism().map(|number| number.to_string());
    let available_parallelism = available_parallelism.as_deref();

    let number_test_threads = {
        let argument = Arg::new(option::NUMBER_TEST_THREADS)
            .short('T')
            .long("test-threads")
            .value_name("NUMBER")
            .value_parser(NonZeroUsizeParser)
            .help("Set the number of OS threads to use during test execution");

        match available_parallelism {
            Ok(available_parallelism) => argument.default_value(available_parallelism),
            Err(_) => argument.required(true),
        }
    };

    let matches = Command::new(env!("CARGO_PKG_NAME"))
        .version(env!("CARGO_PKG_VERSION"))
        .about(env!("CARGO_PKG_DESCRIPTION"))
        .arg(
            Arg::new(option::RELEASE)
                .long("release")
                .short('r')
                .help("Build the Lushui compiler in release mode, with optimizations"),
        )
        .arg(Arg::new(option::GILD).long("gild").short('g').help(
            "Update the golden files of all included failing tests to the current compiler output",
        ))
        .arg(
            Arg::new(option::TIMEOUT)
                .long("timeout")
                .short('t')
                .value_name("DURATION")
                .value_parser(DurationParser)
                .help("Impose a timeout in seconds to every single test"),
        )
        .arg(number_test_threads)
        .arg(
            Arg::new(option::INSPECT)
                .long("inspect")
                .short('I')
                .help("Inspect the test suite for issues"),
        )
        .arg(
            Arg::new(option::DIFF_VIEW)
                .long("diff-view")
                .value_name("FORMAT")
                .value_parser(DiffViewParser)
                .help("Set the format of diff views"),
        )
        .arg(
            Arg::new(argument::PATHS)
                .value_name("PATH")
                .multiple_values(true)
                .action(ArgAction::Append)
                .value_parser(PathBufValueParser::new())
                .help(
                    "The paths to test files and folders inside of the test folder. \
                     If provided, only the given tests are included",
                ),
        )
        .get_matches();

    let mut paths = Vec::new();
    let mut success = true;

    for path in matches
        .get_many::<PathBuf>(argument::PATHS)
        .unwrap_or_default()
    {
        match path.canonicalize() {
            Ok(path) => paths.push(path),
            Err(error) => {
                if success {
                    success = false;
                }

                Diagnostic::error()
                    .message("could not load the test file")
                    .note(format!("‘{}’: {error}", path.display()))
                    .report(&Reporter::stderr());
            }
        }
    }

    if !success {
        return Err(());
    }

    Ok(Arguments {
        compiler_build_mode: if matches.contains_id(option::RELEASE) {
            CompilerBuildMode::Release
        } else {
            CompilerBuildMode::Debug
        },
        gilding: if matches.contains_id(option::GILD) {
            Gilding::Yes
        } else {
            Gilding::No
        },
        paths,
        timeout: matches.get_one(option::TIMEOUT).copied(),
        number_test_threads: *matches.get_one(option::NUMBER_TEST_THREADS).unwrap(),
        diff_view: matches
            .get_one(option::DIFF_VIEW)
            .copied()
            .unwrap_or_default(),
        inspecting: if matches.contains_id(option::INSPECT) {
            Inspecting::Yes
        } else {
            Inspecting::No
        },
    })
}

pub(crate) struct Arguments {
    pub(crate) compiler_build_mode: CompilerBuildMode,
    pub(crate) gilding: Gilding,
    pub(crate) paths: Vec<PathBuf>,
    pub(crate) timeout: Option<Duration>,
    pub(crate) number_test_threads: NonZeroUsize,
    pub(crate) diff_view: DiffView,
    pub(crate) inspecting: Inspecting,
}

mod option {
    pub(super) const DIFF_VIEW: &str = "diff-view";
    pub(super) const GILD: &str = "gild";
    pub(super) const INSPECT: &str = "inspect";
    pub(super) const NUMBER_TEST_THREADS: &str = "number-test-threads";
    pub(super) const RELEASE: &str = "release";
    pub(super) const TIMEOUT: &str = "timeout";
}

mod argument {
    pub(super) const PATHS: &str = "paths";
}

#[derive(Clone)]
struct NonZeroUsizeParser;

impl TypedValueParser for NonZeroUsizeParser {
    type Value = NonZeroUsize;

    fn parse_ref(
        &self,
        _: &clap::Command<'_>,
        _: Option<&Arg<'_>>,
        source: &OsStr,
    ) -> Result<Self::Value, clap::Error> {
        let source: &str = parse_utf8(source)?;

        source.parse().map_err(|_| {
            // @Task smh. avoid using `Error::raw` and smh. pass along the context.
            //       https://github.com/clap-rs/clap/discussions/4029
            clap::Error::raw(
                clap::ErrorKind::InvalidValue,
                format!("‘{source}’ is not a valid non-zero usize\n"),
            )
        })
    }
}

#[derive(Clone)]
struct DurationParser;

impl TypedValueParser for DurationParser {
    type Value = Duration;

    fn parse_ref(
        &self,
        _: &clap::Command<'_>,
        _: Option<&Arg<'_>>,
        source: &OsStr,
    ) -> Result<Self::Value, clap::Error> {
        let source: &str = parse_utf8(source)?;

        let seconds = source.parse().map_err(|_| {
            // @Task smh. avoid using `Error::raw` and smh. pass along the context.
            //       https://github.com/clap-rs/clap/discussions/4029
            clap::Error::raw(
                clap::ErrorKind::InvalidValue,
                format!("‘{source}’ is not a valid number of seconds\n"),
            )
        })?;

        Ok(Duration::from_secs(seconds))
    }
}

#[derive(Clone)]
struct DiffViewParser;

impl TypedValueParser for DiffViewParser {
    type Value = DiffView;

    fn parse_ref(
        &self,
        _: &clap::Command<'_>,
        _: Option<&Arg<'_>>,
        source: &OsStr,
    ) -> Result<Self::Value, clap::Error> {
        let source: &str = parse_utf8(source)?;

        source.parse().map_err(|_| {
            // @Task smh. avoid using `Error::raw` and smh. pass along the context.
            //       https://github.com/clap-rs/clap/discussions/4029
            clap::Error::raw(
                clap::ErrorKind::InvalidValue,
                format!("‘{source}’ is not a valid diff-view format\n"),
            )
        })
    }

    fn possible_values(
        &self,
    ) -> Option<Box<dyn Iterator<Item = clap::PossibleValue<'static>> + '_>> {
        Some(Box::new(
            DiffView::elements().map(|format| clap::PossibleValue::new(format.name())),
        ))
    }
}

fn parse_utf8(source: &OsStr) -> Result<&str, clap::Error> {
    source.to_str().ok_or_else(|| {
        // @Task smh. avoid using `Error::raw` and smh. pass along the context.
        //       https://github.com/clap-rs/clap/discussions/4029
        clap::Error::raw(
            clap::ErrorKind::InvalidUtf8,
            format!("‘{}’ is not valid UTF-8\n", source.to_string_lossy()),
        )
    })
}
