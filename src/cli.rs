use std::{cmp::max, default::default, path::PathBuf, str::FromStr};

use clap::{AppSettings, Arg, SubCommand};
use discriminant::Elements;
use lushui::{
    diagnostics::{reporter::StderrReporter, Diagnostic},
    package::CrateType,
};

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

    let build_options = [
        Arg::with_name("no-core").long("no-core").help(
            "Removes the dependency to the library `core` from the given single-file package",
        ),
        Arg::with_name("crate-type")
            .long("crate-type")
            .takes_value(true)
            .possible_values(&["binary", "library"])
            .help("Sets the crate type of the given single-file package"),
        Arg::with_name("interpreter")
            .long("interpreter")
            .takes_value(true)
            .possible_values(&["byte-code", "tree-walk"])
            .help("Sets the interpreter"),
    ];

    let matches = clap::App::new(env!("CARGO_PKG_NAME"))
        .version(super::VERSION)
        .about(env!("CARGO_PKG_DESCRIPTION"))
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .arg(
            Arg::with_name("quiet")
                .long("quiet")
                .short("q")
                .global(true)
                .help("No status output printed to stdout"),
        )
        .arg(
            Arg::with_name("unstable-options")
                .short("Z")
                .takes_value(true)
                .multiple(true)
                .number_of_values(1)
                .global(true)
                .hidden(true) // mentioned in AFTER_HELP
                .help("Sets unstable options (options not subject to any stability guarantees)"),
        )
        .subcommand(
            SubCommand::with_name("build")
                .visible_alias("b")
                .setting(AppSettings::DisableVersion)
                .about("Compiles the given source file or package or the current package")
                .arg(&source_file_argument)
                .args(&build_options),
        )
        .subcommand(
            SubCommand::with_name("check")
                .visible_alias("c")
                .setting(AppSettings::DisableVersion)
                .about("Type-checks the given source file or package or the current package")
                .arg(&source_file_argument)
                .args(&build_options),
        )
        .subcommand(
            SubCommand::with_name("document")
                .visible_aliases(&["doc", "d"])
                .setting(AppSettings::DisableVersion)
                .about("Document the given source file or package or the current package")
                .arg(&source_file_argument)
                .args(&build_options)
                .arg(
                    Arg::with_name("open")
                        .long("open")
                        .help("Opens the documentation in a browser after building it"),
                )
                .arg(
                    Arg::with_name("no-dependencies")
                        .long("no-deps")
                        .help("The dependencies are not documented"),
                ),
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
                .arg(&source_file_argument)
                .args(&build_options),
        )
        .after_help(AFTER_HELP)
        .get_matches();

    let command = matches.subcommand.as_ref().unwrap();

    let command = match &*command.name {
        name @ ("build" | "check" | "run" | "document") => Command::Build {
            mode: match name {
                "build" => BuildMode::Build,
                "check" => BuildMode::Check,
                "run" => BuildMode::Run,
                "document" => BuildMode::Document {
                    options: DocumentationOptions {
                        open: command.matches.is_present("open"),
                        no_dependencies: command.matches.is_present("no-dependencies"),
                    },
                },
                _ => unreachable!(),
            },
            options: BuildOptions {
                path: command.matches.value_of_os("PATH").map(Into::into),
                no_core: command.matches.is_present("no-core"),
                crate_type: command
                    .matches
                    .value_of("crate-type")
                    .map(|input| input.parse().unwrap()),
                interpreter: command
                    .matches
                    .value_of("interpreter")
                    .map(|input| input.parse().unwrap())
                    .unwrap_or_default(),
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
            options: {
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

    let mut options = Options {
        quiet: matches.is_present("quiet"),
        ..default()
    };

    if let Some(unstable_options) = matches.values_of("unstable-options") {
        for unstable_option in unstable_options {
            use UnstableOption::*;

            let Ok(unstable_option) = unstable_option.parse() else {
                Diagnostic::error()
                    .message(format!("invalid unstable option `{unstable_option}`"))
                    .report(&StderrReporter::new(None).into());
                std::process::exit(1);
            };

            match unstable_option {
                Help => {
                    let mut message =
                        "UNSTABLE OPTIONS (not subject to any stability guarantees):\n".to_string();
                    for option in UnstableOption::elements() {
                        message +=
                            &format!("    -Z {:<25}    {}\n", option.syntax(), option.help());
                    }
                    println!("{message}");
                    std::process::exit(0);
                }
                Internals => options.internals = true,
                EmitTokens => options.emit_tokens = true,
                EmitAst => options.emit_ast = true,
                EmitLoweredAst => options.emit_lowered_ast = true,
                EmitHir => options.emit_hir = true,
                EmitUntypedScope => options.emit_untyped_scope = true,
                EmitScope => options.emit_scope = true,
                Durations => options.durations = true,
                ShowIndices => options.show_indices = true,
                LexOnly | ParseOnly | LowerOnly | ResolveOnly => {
                    options.pass_restriction = max(
                        options.pass_restriction,
                        Some(match unstable_option {
                            LexOnly => PassRestriction::Lexer,
                            ParseOnly => PassRestriction::Parser,
                            LowerOnly => PassRestriction::Lowerer,
                            ResolveOnly => PassRestriction::Resolver,
                            _ => unreachable!(),
                        }),
                    );
                }
                AsciiDoc => options.asciidoc = true,
                LoremIpsum(amount) => options.lorem_ipsum = amount,
            }
        }
    }

    (command, options)
}

const AFTER_HELP: &str = "\
ADDITIONAL HELP:
    -Z help    Prints unstable options (options not subject to any stability guarantees)
";

// @Task add --color=always|never|auto
#[derive(Default)]
#[allow(clippy::struct_excessive_bools)]
pub struct Options {
    pub quiet: bool,
    pub internals: bool,
    pub emit_tokens: bool,
    pub emit_ast: bool,
    pub emit_lowered_ast: bool,
    pub emit_hir: bool,
    pub emit_untyped_scope: bool,
    pub emit_scope: bool,
    pub durations: bool,
    pub show_indices: bool,
    pub pass_restriction: Option<PassRestriction>,
    pub asciidoc: bool,
    pub lorem_ipsum: Option<usize>,
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

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum PassRestriction {
    Resolver,
    Lowerer,
    Parser,
    Lexer,
}

pub enum Command {
    Build {
        mode: BuildMode,
        options: BuildOptions,
    },
    Explain,
    Generate {
        mode: GenerationMode,
        options: GenerationOptions,
    },
}

pub enum BuildMode {
    Check,
    Build,
    Run,
    Document { options: DocumentationOptions },
}

pub struct BuildOptions {
    pub path: Option<PathBuf>,
    pub no_core: bool,
    pub crate_type: Option<CrateType>,
    pub interpreter: Interpreter,
}

pub enum GenerationMode {
    Initialize,
    New { package_name: String },
}

pub struct GenerationOptions {
    pub library: bool,
    pub binary: bool,
}

pub struct DocumentationOptions {
    pub open: bool,
    pub no_dependencies: bool,
}

#[derive(Clone, Copy, Elements)]
enum UnstableOption {
    Help,
    Internals,
    EmitTokens,
    EmitAst,
    EmitLoweredAst,
    EmitHir,
    EmitUntypedScope,
    EmitScope,
    ShowIndices,
    Durations,
    LexOnly,
    ParseOnly,
    LowerOnly,
    ResolveOnly,
    AsciiDoc,
    LoremIpsum(Option<usize>),
}

impl UnstableOption {
    const fn syntax(self) -> &'static str {
        match self {
            Self::Help => "help",
            Self::Internals => "internals",
            Self::EmitTokens => "emit-tokens",
            Self::EmitAst => "emit-ast",
            Self::EmitLoweredAst => "emit-lowered-ast",
            Self::EmitHir => "emit-hir",
            Self::EmitUntypedScope => "emit-untyped-scope",
            Self::EmitScope => "emit-scope",
            Self::ShowIndices => "show-indices",
            Self::Durations => "durations",
            Self::LexOnly => "lex-only",
            Self::ParseOnly => "parse-only",
            Self::LowerOnly => "lower-only",
            Self::ResolveOnly => "resolve-only",
            Self::AsciiDoc => "asciidoc",
            Self::LoremIpsum(_) => "lorem-ipsum=[<amount=1>]",
        }
    }

    const fn help(self) -> &'static str {
        match self {
            Self::Help => "Prints help information and halts",
            Self::Internals => "Enables internal language and library features",
            Self::EmitTokens => "Emits the tokens of the current crate output by the lexer",
            Self::EmitAst => "Emits the abstract syntax tree (AST) of the current crate output by the parser",
            Self::EmitLoweredAst => "Emits the lowered AST of the current crate",
            Self::EmitHir => {
                "Emits the high-level intermediate representation (HIR) of the current crate output by the resolver"
            }
            Self::EmitUntypedScope => "Emits the untyped scope of the current crate output by the resolver",
            Self::EmitScope => "Emits the typed scope of the current crate output by the typer",
            Self::ShowIndices => "Shows the internal indices of bindings in other output or error messages",
            Self::Durations => "Prints the duration of each pass through the current crate",
            Self::LexOnly => "Halts the execution after lexing the current crate",
            Self::ParseOnly => "Halts the execution after parsing the current crate",
            Self::LowerOnly => "Halts the execution after lowering the current crate",
            Self::ResolveOnly => "Halts the execution after resolving the names of the current crate",
            Self::AsciiDoc => "Interprets documentation comments as AsciiDoc when generating documentation",
            Self::LoremIpsum(_) => "Replaces the documentation of every declaration with `amount` paragraphs of Lorem Ipsum",
        }
    }
}

// @Task derive this
impl FromStr for UnstableOption {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(match input {
            "help" => Self::Help,
            "internals" => Self::Internals,
            "emit-tokens" => Self::EmitTokens,
            "emit-ast" => Self::EmitAst,
            "emit-lowered-ast" => Self::EmitLoweredAst,
            "emit-hir" => Self::EmitHir,
            "emit-untyped-scope" => Self::EmitUntypedScope,
            "emit-scope" => Self::EmitScope,
            "show-indices" => Self::ShowIndices,
            "durations" => Self::Durations,
            "lex-only" => Self::LexOnly,
            "parse-only" => Self::ParseOnly,
            "lower-only" => Self::LowerOnly,
            "resolve-only" => Self::ResolveOnly,
            "asciidoc" => Self::AsciiDoc,
            "lorem-ipsum" => Self::LoremIpsum(Some(1)),
            _ => match input.split_once('=').ok_or(())? {
                ("lorem-ipsum", amount) => Self::LoremIpsum(Some(amount.parse().map_err(|_| ())?)),
                _ => return Err(()),
            },
        })
    }
}
