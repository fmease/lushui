use std::{
    mem::take,
    path::{Path, PathBuf},
    str::FromStr,
};

use clap::AppSettings;

const VERSION: &str = concat!(
    env!("CARGO_PKG_VERSION"),
    " (",
    env!("GIT_COMMIT_HASH"),
    " ",
    env!("GIT_COMMIT_DATE"),
    ")"
);

#[derive(Debug)] // @Temporary

pub struct Application {
    pub command: Command,
    pub interpreter: Interpreter,
    pub dump: DumpInformation,
    pub show_binding_indices: bool,
    pub phase_restriction: Option<PhaseRestriction>,
}

impl Application {
    pub fn new() -> Self {
        let source_file_argument = clap::Arg::with_name("FILE")
            .index(1)
            .help("The source file to run");

        let matches = clap::App::new(env!("CARGO_PKG_NAME"))
            .version(VERSION)
            .author(env!("CARGO_PKG_AUTHORS"))
            .about(env!("CARGO_PKG_DESCRIPTION"))
            // .setting(AppSettings::HidePossibleValuesInHelp)
            .setting(AppSettings::SubcommandRequiredElseHelp)
            .arg(
                clap::Arg::with_name("interpreter")
                    .long("interpreter")
                    .takes_value(true)
                    .global(true)
                    .possible_values(&["tree-walk", "byte-code"])
                    .help("@Task"),
            )
            .arg(
                clap::Arg::with_name("unstable-options")
                    .short("Z")
                    .takes_value(true)
                    .multiple(true)
                    .number_of_values(1)
                    .global(true)
                    .possible_values(&[
                        "help",
                        // @Task smh group
                        "dump-tokens",
                        "dump-ast",
                        "dump-lowered-ast",
                        "dump-hir",
                        "dump-untyped-scope",
                        "dump-scope",
                        "show-binding-indices",
                        "lex-only",
                        "parse-only",
                        "lower-only",
                        "resolve-names-only",
                    ])
                    .help("@Task"),
            )
            .subcommand(
                clap::SubCommand::with_name("build")
                    .visible_alias("b")
                    .setting(AppSettings::DisableVersion)
                    .about("Compiles the given program")
                    .arg(source_file_argument.clone()),
            )
            .subcommand(
                clap::SubCommand::with_name("check")
                    .visible_alias("c")
                    .setting(AppSettings::DisableVersion)
                    .about("Type-checks the given program")
                    .arg(source_file_argument.clone()),
            )
            .subcommand(
                clap::SubCommand::with_name("run")
                    .visible_alias("r")
                    .setting(AppSettings::DisableVersion)
                    .about("Compiles and runs the given program")
                    .arg(source_file_argument),
            )
            .get_matches();

        let command = matches.subcommand.as_ref().unwrap();
        let command = match &*command.name {
            "build" => Command::Build {
                source_file_path: PathBuf::from(command.matches.value_of_os("FILE").unwrap()),
            },
            "check" => Command::Check {
                source_file_path: PathBuf::from(command.matches.value_of_os("FILE").unwrap()),
            },
            "run" => Command::Run {
                source_file_path: PathBuf::from(command.matches.value_of_os("FILE").unwrap()),
            },
            _ => unreachable!(),
        };

        let mut dump = DumpInformation::default();
        let mut show_binding_indices = false;
        let mut phase_restriction = None;

        if let Some(unstable_options) = matches.values_of("unstable-options") {
            for unstable_option in unstable_options {
                match unstable_option {
                    "help" => todo!(),
                    "dump-tokens" => dump.tokens = true,
                    "dump-ast" => dump.ast = true,
                    "dump-lowered-ast" => dump.lowered_ast = true,
                    "dump-hir" => dump.hir = true,
                    "dump-untyped-scope" => dump.untyped_scope = true,
                    "dump-scope" => dump.scope = true,
                    "show-binding-indices" => show_binding_indices = true,
                    "lex-only" => {
                        PhaseRestriction::update(&mut phase_restriction, PhaseRestriction::Lexer);
                    }
                    "parse-only" => {
                        PhaseRestriction::update(&mut phase_restriction, PhaseRestriction::Parser);
                    }
                    "lower-only" => {
                        PhaseRestriction::update(&mut phase_restriction, PhaseRestriction::Lowerer);
                    }
                    "resolve-names-only" => {
                        PhaseRestriction::update(
                            &mut phase_restriction,
                            PhaseRestriction::Resolver,
                        );
                    }
                    _ => unreachable!(),
                }
            }
        }

        Self {
            command,
            interpreter: matches
                .value_of("interpreter")
                .map(|input| input.parse().unwrap())
                .unwrap_or_default(),
            dump,
            show_binding_indices,
            phase_restriction,
        }
    }
}

#[derive(Debug)] // @Temporary
pub enum Interpreter {
    TreeWalk,
    ByteCode,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::TreeWalk // most stable
    }
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
#[derive(Debug)] // @Temporary
#[derive(Default)]

pub struct DumpInformation {
    pub tokens: bool,
    pub ast: bool,
    pub lowered_ast: bool,
    pub hir: bool,
    pub untyped_scope: bool,
    pub scope: bool,
}

#[derive(Debug)] // @Temporary
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum PhaseRestriction {
    Lexer,
    Parser,
    Lowerer,
    Resolver,
}

impl PhaseRestriction {
    pub fn update(this: &mut Option<Self>, other: Self) {
        *this = std::cmp::max(*this, Some(other));
    }
}

impl FromStr for PhaseRestriction {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(match input {
            "lex-only" => Self::Lexer,
            "parse-only" => Self::Parser,
            "lower-only" => Self::Lowerer,
            "resolve-names-only" => Self::Resolver,
            _ => return Err(()),
        })
    }
}

// @Task use PathBuf instead
#[derive(Debug)] // @Temporary

pub enum Command {
    Build { source_file_path: PathBuf },
    Check { source_file_path: PathBuf },
    Run { source_file_path: PathBuf },
}
