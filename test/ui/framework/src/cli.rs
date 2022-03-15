use crate::{CompilerBuildMode, Gilding, Inspecting};
use clap::{Arg, Command};
use std::{num::NonZeroUsize, str::FromStr};

pub(crate) struct Application {
    pub(crate) compiler_build_mode: CompilerBuildMode,
    pub(crate) gilding: Gilding,
    pub(crate) strict_filters: Vec<String>,
    pub(crate) loose_filters: Vec<String>,
    pub(crate) number_test_threads: NonZeroUsize,
    pub(crate) inspecting: Inspecting,
}

impl Application {
    pub(crate) fn new() -> Self {
        const STRICT_FILTERS: &str = "strict-filters";
        const LOOSE_FILTERS: &str = "loose-filters";
        const NUMBER_TEST_THREADS: &str = "number-test-threads";

        let available_parallelism =
            std::thread::available_parallelism().map(|number| number.to_string());
        let available_parallelism = available_parallelism.as_deref();

        let number_test_threads = {
            let argument = Arg::new(NUMBER_TEST_THREADS)
                .short('T')
                .long("test-threads")
                .value_name("NUMBER")
                .validator(|input| {
                    NonZeroUsize::from_str(input)
                        .map(drop)
                        .map_err(|error| error.to_string())
                })
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
                Arg::new("release")
                    .long("release")
                    .short('r')
                    .help("Build the Lushui compiler in release mode, with optimizations"),
            )
            .arg(Arg::new("gild").long("gild").short('g').help(
                "Update golden files of all included failing tests to the current compiler output",
            ))
            .arg(
                Arg::new(STRICT_FILTERS)
                    .long("filter-strictly")
                    .short('F')
                    .value_name("PATH")
                    .multiple_occurrences(true)
                    .help(
                        "Exclude tests whose file path does not equal the given filter \
                         (and which do not match any other filter); \
                         those paths are relative to the test folder path and lack an extension",
                    ),
            )
            .arg(
                Arg::new(LOOSE_FILTERS)
                    .long("filter-loosely")
                    .short('f')
                    .value_name("PATH")
                    .multiple_occurrences(true)
                    .help(
                        "Exclude tests whose file path does not contain the given filter \
                         (and which do not match any other filter); \
                         those paths are relative to the test folder path and lack an extension",
                    ),
            )
            .arg(number_test_threads)
            .arg(
                Arg::new("inspect")
                    .long("inspect")
                    .short('I')
                    .help("Inspect the test suite for issues"),
            )
            .get_matches();

        Application {
            compiler_build_mode: if matches.is_present("release") {
                CompilerBuildMode::Release
            } else {
                CompilerBuildMode::Debug
            },
            gilding: if matches.is_present("gild") {
                Gilding::Yes
            } else {
                Gilding::No
            },
            strict_filters: matches
                .values_of(STRICT_FILTERS)
                .map_or(Vec::new(), |value| value.map(ToString::to_string).collect()),
            loose_filters: matches
                .values_of(LOOSE_FILTERS)
                .map_or(Vec::new(), |value| value.map(ToString::to_string).collect()),
            number_test_threads: matches
                .value_of(NUMBER_TEST_THREADS)
                .map(|input| input.parse().unwrap())
                .unwrap(),
            inspecting: if matches.is_present("inspect") {
                Inspecting::Yes
            } else {
                Inspecting::No
            },
        }
    }
}
