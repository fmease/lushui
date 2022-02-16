use super::{
    default_test_directory_path, DEFAULT_NUMBER_TEST_THREADS_UNKNOWN_AVAILABLE_PARALLELISM,
};
use clap::{App, Arg};
use std::{num::NonZeroUsize, str::FromStr};

pub(crate) struct Application {
    pub(crate) release_mode: bool,
    pub(crate) gilding: bool,
    pub(crate) loose_filters: Vec<String>,
    pub(crate) exact_filters: Vec<String>,
    pub(crate) number_test_threads: NonZeroUsize,
    pub(crate) test_folder_path: String,
}

impl Application {
    pub(crate) fn new() -> Self {
        let default_test_directory_path = default_test_directory_path();
        let available_parallelism = std::thread::available_parallelism()
            .ok()
            .unwrap_or(DEFAULT_NUMBER_TEST_THREADS_UNKNOWN_AVAILABLE_PARALLELISM);
        let available_parallelism = available_parallelism.to_string();

        let matches = App::new(env!("CARGO_PKG_NAME"))
            .version(env!("CARGO_PKG_VERSION"))
            .about(env!("CARGO_PKG_DESCRIPTION"))
            .arg(
                Arg::new("release")
                    .long("release")
                    .short('r')
                    .help("Builds the Lushui compiler in release mode (i.e. with optimizations)"),
            )
            .arg(
                Arg::new("gild")
                    .long("gild")
                    .help("Updates golden files of all included failing tests to the current compiler output"),
            )
            .arg(
                Arg::new("loose-filter")
                    .long("filter")
                    .short('f')
                    .takes_value(true)
                    .multiple_occurrences(true)
                    .help(
                        "Excludes tests whose file path (relative to the test folder path, without extension) \
                         does not contain the given filter (and which do not match any other filter)"
                    ),
            )
            .arg(
                Arg::new("exact-filter")
                    .long("filter-exact")
                    .short('F')
                    .takes_value(true)
                    .multiple_occurrences(true)
                    .help(
                        "Excludes tests whose file path (relative to the test folder path, without extension) \
                        does not equal the given filter (and which do not match any other filter)"
                    ),
            )
            .arg(
                Arg::new("number-test-threads")
                    .long("test-threads")
                    .takes_value(true)
                    .validator(|input| {
                        NonZeroUsize::from_str(input)
                            .map(drop)
                            .map_err(|error| error.to_string())
                    })
                    .default_value(&available_parallelism)
                    .help("The number of threads to use during test execution"),
            )
            .arg(
                Arg::new("test-folder-path")
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
