use std::{path::PathBuf, str::FromStr};

use clap::{AppSettings, Arg, SubCommand};
use lushui::package::CrateType;

const VERSION: &str = concat!(
    env!("CARGO_PKG_VERSION"),
    " (",
    env!("GIT_COMMIT_HASH"),
    " ",
    env!("GIT_COMMIT_DATE"),
    ")"
);

pub fn arguments() -> (Command, Options) {
    let source_file_argument = Arg::with_name("PATH")
        .index(1)
        .help("The path to a source file or a package folder");

    let package_creation_options = [
        Arg::with_name("binary")
            .long("binary")
            .short("b")
            .help("Creates a binary (executable) crate in the package"),
        Arg::with_name("library")
            .long("library")
            .short("l")
            .help("Creates a library crate in the package"),
    ];

    let matches = clap::App::new(env!("CARGO_PKG_NAME"))
        .version(VERSION)
        .author(env!("CARGO_PKG_AUTHORS"))
        .about(env!("CARGO_PKG_DESCRIPTION"))
        // .setting(AppSettings::HidePossibleValuesInHelp)
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .arg(
            Arg::with_name("quiet")
                .long("quiet")
                .short("q")
                .global(true)
                .help("No status output printed to stdout"),
        )
        .arg(Arg::with_name("no-core").long("no-core").global(true).help(
            "Removes the dependency to the library `core` from the given single-file package",
        ))
        .arg(
            Arg::with_name("crate-type")
                .long("crate-type")
                .takes_value(true)
                .global(true)
                .possible_values(&["binary", "library"])
                .help("Sets the crate type of the given single-file package"),
        )
        .arg(
            Arg::with_name("interpreter")
                .long("interpreter")
                .takes_value(true)
                .global(true)
                .possible_values(&["byte-code", "tree-walk"])
                .help("Sets the interpreter"),
        )
        .arg(
            Arg::with_name("unstable-options")
                .short("Z")
                .takes_value(true)
                .multiple(true)
                .number_of_values(1)
                .global(true)
                .possible_values(&[
                    "help",
                    // @Task smh group
                    "emit-tokens",
                    "emit-ast",
                    "emit-lowered-ast",
                    "emit-hir",
                    "emit-untyped-scope",
                    "emit-scope",
                    "emit-times",
                    // @Beacon @Task rename to show-declaration-indices
                    "show-binding-indices",
                    "lex-only",
                    "parse-only",
                    "lower-only",
                    "resolve-only",
                ])
                .help("Sets unstable options (options excluded from any stability guarantees)"),
        )
        .subcommand(
            SubCommand::with_name("build")
                .visible_alias("b")
                .setting(AppSettings::DisableVersion)
                .about("Compiles the given source file or package or the current package")
                .arg(source_file_argument.clone()),
        )
        .subcommand(
            SubCommand::with_name("check")
                .visible_alias("c")
                .setting(AppSettings::DisableVersion)
                .about("Type-checks the given source file or package or the current package")
                .arg(source_file_argument.clone()),
        )
        .subcommand(
            SubCommand::with_name("explain")
                .setting(AppSettings::DisableVersion)
                .about("Explains given error codes")
                .arg(
                    Arg::with_name("CODES")
                        .index(1)
                        .multiple(true)
                        .required(true)
                        .help("The error codes that need explanation"),
                ),
        )
        .subcommand(
            SubCommand::with_name("initialize")
                .visible_alias("init")
                .setting(AppSettings::DisableVersion)
                .about("Creates a new package in the current folder")
                .args(&package_creation_options),
        )
        .subcommand(
            SubCommand::with_name("new")
                .setting(AppSettings::DisableVersion)
                .about("Creates a new package")
                .arg(
                    Arg::with_name("NAME")
                        .index(1)
                        .required(true)
                        .help("The name of the package"),
                )
                .args(&package_creation_options),
        )
        .subcommand(
            SubCommand::with_name("run")
                .visible_alias("r")
                .setting(AppSettings::DisableVersion)
                .about("Compiles and runs the given source file or package or the current package")
                .arg(source_file_argument),
        )
        .get_matches();

    let command = matches.subcommand.as_ref().unwrap();

    let command = match &*command.name {
        name @ ("build" | "check" | "run") => Command::Build {
            mode: match name {
                "build" => BuildMode::Build,
                "check" => BuildMode::Check,
                "run" => BuildMode::Run,
                _ => unreachable!(),
            },
            suboptions: BuildOptions {
                path: command.matches.value_of_os("PATH").map(Into::into),
            },
        },
        "explain" => Command::Explain,
        name @ ("initialize" | "new") => Command::Generate {
            mode: match name {
                "initialize" => GenerationMode::Initialize,
                "new" => GenerationMode::New {
                    package_name: command.matches.value_of("NAME").unwrap().into(),
                },
                _ => unreachable!(),
            },
            suboptions: {
                let library = command.matches.is_present("library");

                GenerationOptions {
                    library,
                    // implicitly set when no explicit crate type specified
                    binary: command.matches.is_present("binary") || !library,
                }
            },
        },
        _ => unreachable!(), // handled by clap
    };

    let mut emit = Emissions::default();
    let mut show_binding_indices = false;
    let mut pass_restriction = None;

    if let Some(unstable_options) = matches.values_of("unstable-options") {
        for unstable_option in unstable_options {
            match unstable_option {
                "help" => todo!(),
                "emit-tokens" => emit.tokens = true,
                "emit-ast" => emit.ast = true,
                "emit-lowered-ast" => emit.lowered_ast = true,
                "emit-hir" => emit.hir = true,
                "emit-untyped-scope" => emit.untyped_scope = true,
                "emit-scope" => emit.scope = true,
                "emit-times" => emit.times = true,
                "show-binding-indices" => show_binding_indices = true,
                "lex-only" => {
                    PassRestriction::update(&mut pass_restriction, PassRestriction::Lexer);
                }
                "parse-only" => {
                    PassRestriction::update(&mut pass_restriction, PassRestriction::Parser);
                }
                "lower-only" => {
                    PassRestriction::update(&mut pass_restriction, PassRestriction::Lowerer);
                }
                "resolve-only" => {
                    PassRestriction::update(&mut pass_restriction, PassRestriction::Resolver);
                }
                _ => unreachable!(), // handled by clap
            }
        }
    }

    let options = Options {
        quiet: matches.is_present("quiet"),
        no_core: matches.is_present("no-core"),
        crate_type: matches
            .value_of("crate-type")
            .map(|input| input.parse().unwrap()),
        interpreter: matches
            .value_of("interpreter")
            .map(|input| input.parse().unwrap())
            .unwrap_or_default(),
        emit,
        show_binding_indices,
        pass_restriction,
    };

    (command, options)
}

// @Task add --color=always|never|auto
pub struct Options {
    pub quiet: bool,
    pub no_core: bool,
    pub crate_type: Option<CrateType>,
    pub interpreter: Interpreter,
    pub emit: Emissions,
    pub show_binding_indices: bool,
    pub pass_restriction: Option<PassRestriction>,
}

#[derive(Default)]
pub enum Interpreter {
    #[default] // the most stable one
    TreeWalk,
    ByteCode,
}

impl FromStr for Interpreter {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(match input {
            "tree-walk" => Self::TreeWalk,
            "byte-code" => Self::ByteCode,
            _ => return Err(()),
        })
    }
}

// @Note bad name
#[derive(Default)]
#[allow(clippy::struct_excessive_bools)]
pub struct Emissions {
    pub tokens: bool,
    pub ast: bool,
    pub lowered_ast: bool,
    pub hir: bool,
    pub untyped_scope: bool,
    pub scope: bool,
    pub times: bool,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum PassRestriction {
    Resolver,
    Lowerer,
    Parser,
    Lexer,
}

impl PassRestriction {
    pub fn update(this: &mut Option<Self>, other: Self) {
        *this = std::cmp::max(*this, Some(other));
    }
}

impl FromStr for PassRestriction {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(match input {
            "resolve-only" => Self::Resolver,
            "lower-only" => Self::Lowerer,
            "parse-only" => Self::Parser,
            "lex-only" => Self::Lexer,
            _ => return Err(()),
        })
    }
}

pub enum Command {
    Build {
        mode: BuildMode,
        suboptions: BuildOptions,
    },
    Explain,
    Generate {
        mode: GenerationMode,
        suboptions: GenerationOptions,
    },
}

pub enum BuildMode {
    Check,
    Build,
    Run,
}

pub struct BuildOptions {
    pub path: Option<PathBuf>,
}

pub enum GenerationMode {
    Initialize,
    New { package_name: String },
}

pub struct GenerationOptions {
    pub library: bool,
    pub binary: bool,
}
