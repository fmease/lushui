use std::{path::PathBuf, str::FromStr};

use clap::{AppSettings, Arg, SubCommand};

const VERSION: &str = concat!(
    env!("CARGO_PKG_VERSION"),
    " (",
    env!("GIT_COMMIT_HASH"),
    " ",
    env!("GIT_COMMIT_DATE"),
    ")"
);

pub fn arguments() -> (Command, Options) {
    let source_file_argument = Arg::with_name("FILE")
        .index(1)
        .help("The source file to run");

    let package_creation_options = [
        Arg::with_name("binary")
            .long("binary")
            .short("b")
            .help("@Task"),
        Arg::with_name("library")
            .long("library")
            .short("l")
            .help("@Task"),
    ];

    let matches = clap::App::new(env!("CARGO_PKG_NAME"))
        .version(VERSION)
        .author(env!("CARGO_PKG_AUTHORS"))
        .about(env!("CARGO_PKG_DESCRIPTION"))
        // .setting(AppSettings::HidePossibleValuesInHelp)
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .arg(
            Arg::with_name("unlink-core")
                .long("unlink-core")
                .global(true)
                .help("Ceases linking with the library `core`"),
        )
        .arg(
            Arg::with_name("interpreter")
                .long("interpreter")
                .takes_value(true)
                .global(true)
                .possible_values(&["tree-walk", "byte-code"])
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
                    "dump-tokens",
                    "dump-ast",
                    "dump-lowered-ast",
                    "dump-hir",
                    "dump-untyped-scope",
                    "dump-scope",
                    // @Beacon @Task rename to show-declaration-indices
                    "show-binding-indices",
                    "lex-only",
                    "parse-only",
                    "lower-only",
                    "resolve-only",
                ])
                .help("@Task"),
        )
        .subcommand(
            SubCommand::with_name("build")
                .visible_alias("b")
                .setting(AppSettings::DisableVersion)
                .about("Compiles the given program")
                .arg(source_file_argument.clone()),
        )
        .subcommand(
            SubCommand::with_name("check")
                .visible_alias("c")
                .setting(AppSettings::DisableVersion)
                .about("Type-checks the given program")
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
                        .help("@Task"),
                ),
        )
        .subcommand(
            SubCommand::with_name("initialize")
                .visible_alias("init")
                .setting(AppSettings::DisableVersion)
                .about("@Task")
                .args(&package_creation_options),
        )
        .subcommand(
            SubCommand::with_name("new")
                .setting(AppSettings::DisableVersion)
                .about("@Task")
                .arg(
                    Arg::with_name("NAME")
                        .index(1)
                        .required(true)
                        .help("Creates a new package with given name"),
                )
                .args(&package_creation_options),
        )
        .subcommand(
            SubCommand::with_name("run")
                .visible_alias("r")
                .setting(AppSettings::DisableVersion)
                .about("Compiles and runs the given program")
                .arg(source_file_argument),
        )
        .get_matches();

    let command = matches.subcommand.as_ref().unwrap();
    let source_file_path = command.matches.value_of_os("FILE").map(Into::into);

    let command = match &*command.name {
        "build" => Command::Build,
        "check" => Command::Check,
        "explain" => Command::Explain,
        "initialize" | "new" => Command::Generate {
            mode: match &*command.name {
                "initialize" => GenerationMode::Initialize,
                "new" => GenerationMode::New {
                    package_name: command.matches.value_of("NAME").unwrap().into(),
                },
                _ => unreachable!(),
            },
            options: {
                let library = command.matches.is_present("library");

                GenerationOptions {
                    library,
                    // implicitly set when no explicit crate type specified
                    binary: command.matches.is_present("binary") || !library,
                }
            },
        },
        "run" => Command::Run,
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
                "resolve-only" => {
                    PhaseRestriction::update(&mut phase_restriction, PhaseRestriction::Resolver);
                }
                _ => unreachable!(),
            }
        }
    }

    let options = Options {
        source_file_path,
        unlink_core: matches.is_present("unlink-core"),
        interpreter: matches
            .value_of("interpreter")
            .map(|input| input.parse().unwrap())
            .unwrap_or_default(),
        dump,
        show_binding_indices,
        phase_restriction,
    };

    (command, options)
}

// @Task add --color=always|never|auto
// @Task add --link=<crate-name>
pub struct Options {
    // @Task don't store this here in Application but in Command
    // (after merging those three commands maybe??)
    pub source_file_path: Option<PathBuf>,
    pub unlink_core: bool,
    pub interpreter: Interpreter,
    pub dump: DumpInformation,
    pub show_binding_indices: bool,
    pub phase_restriction: Option<PhaseRestriction>,
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

pub struct DumpInformation {
    pub tokens: bool,
    pub ast: bool,
    pub lowered_ast: bool,
    pub hir: bool,
    pub untyped_scope: bool,
    pub scope: bool,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum PhaseRestriction {
    Resolver,
    Lowerer,
    Parser,
    Lexer,
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
            "resolve-only" => Self::Resolver,
            "lower-only" => Self::Lowerer,
            "parse-only" => Self::Parser,
            "lex-only" => Self::Lexer,
            _ => return Err(()),
        })
    }
}

pub enum Command {
    Build,
    Check,
    Explain,
    Generate {
        mode: GenerationMode,
        options: GenerationOptions,
    },
    Run,
}

pub enum GenerationMode {
    Initialize,
    New { package_name: String },
}

pub struct GenerationOptions {
    pub library: bool,
    pub binary: bool,
}
