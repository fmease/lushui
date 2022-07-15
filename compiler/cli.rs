use clap::{Arg, ArgMatches};
use colored::Colorize;
use derivation::{Elements, FromStr, Str};
use lushui::{
    codegen::Options as CodegenOptions, component::ComponentType, documenter, error::Result,
};
use std::{cmp::max, default::default, path::PathBuf};

/// Unstable environment variable to control if internal commands are shown.
const LUSHUI_DEVELOPER_ENV_VAR: &str = "LUSHUI_DEVELOPER";

pub(crate) fn arguments() -> Result<(Command, GlobalOptions)> {
    let metadata_subcommand_disclaimer = &*METADATA_SUBCOMMAND_DISCLAIMER.red().to_string();

    let package_path_argument = Arg::new(argument::PATH).allow_invalid_utf8(true).help(
        "The path to a folder containing a package. Defaults to the local package \
         i.e. the first package whose manifest is found starting the search in the current folder \
         then looking through each parent folder.",
    );

    let backend_option = Arg::new(option::BACKEND)
        .long("backend")
        .short('b')
        .takes_value(true)
        .value_name("BACKEND")
        // @Task get rid of closure once fn name takes self by value
        .possible_values(Backend::elements().map(|element| element.name()))
        .help("Set the backend");

    let file_build_arguments = [
        Arg::new(argument::PATH)
            .allow_invalid_utf8(true)
            .required(true)
            .help("The path to a source file"),
        Arg::new(option::NO_CORE)
            .long("no-core")
            .short('0')
            // @Task rephrase this not using the word "remove"
            .help("Remove the dependency to the standard library ‘core’"),
        Arg::new(option::COMPONENT_TYPE)
            .long("component-type")
            .short('t')
            .value_name("TYPE")
            // @Task write the closure as a function binding once StaticStr derives self by value
            .possible_values(ComponentType::elements().map(|type_| type_.name()))
            .help("Set the component type"),
    ];

    let documentation_arguments = [
        Arg::new(option::OPEN)
            .long("open")
            .short('o')
            .help("Open the documentation in a browser"),
        Arg::new(option::NO_DEPENDENCIES)
            .long("no-deps")
            .help("Prevent the dependencies from being documented"),
    ];

    let package_creation_arguments = [
        Arg::new(option::NO_CORE)
            .long("no-core")
            .help("Do not make the new package dependent on the standard library ‘core’"),
        Arg::new(option::EXECUTABLE)
            .long("executable")
            .visible_alias("exe")
            .help("Create an executable component in the new package"),
        Arg::new(option::LIBRARY)
            .long("library")
            .visible_alias("lib")
            .help("Create a library component in the new package"),
    ];

    let target_libraries_options = [
        Arg::new(option::TARGET_ALL_LIBRARIES)
            .long("libraries")
            .visible_alias("libs")
            .help("Target all libraries"),
        Arg::new(option::TARGET_LIBRARIES)
            .long(ComponentType::Library.name())
            .visible_alias(ComponentType::Library.short_name())
            .value_name(argument::NAME)
            .help("Target only the given or primary library"),
    ];

    let target_all_executables_option = Arg::new(option::TARGET_ALL_EXECUTABLES)
        .long("executables")
        .visible_alias("exes")
        .help("Target all executables");

    let target_executable_option = Arg::new(option::TARGET_EXECUTABLES)
        .long(ComponentType::Executable.name())
        .visible_alias(ComponentType::Executable.short_name())
        .value_name(argument::NAME)
        .help("Target only the given or primary executable");

    let target_executables_option = target_executable_option.clone().multiple_occurrences(true);

    let unstable_options = Arg::new(option::UNSTABLE_OPTION)
        .short('Z')
        .value_name("OPTION")
        .multiple_occurrences(true)
        .help("Set an unstable option. See ‘-Z help’ for details");

    // @Task use `try_get_matches` (no real block, just def an error type and smh exit with code 2 instead of 1 on error)
    let matches = clap::Command::new(env!("CARGO_PKG_NAME"))
        .version(super::VERSION)
        .about(env!("CARGO_PKG_DESCRIPTION"))
        .subcommand_required(true)
        .arg_required_else_help(true)
        .arg(
            Arg::new(option::QUIET)
                .long("quiet")
                .short('q')
                .global(true)
                .help("Suppress status output from being printed to stdout"),
        )
        .subcommands([
            clap::Command::new(subcommand::CHECK)
                .visible_alias("c")
                .about("Check the given or local package for errors")
                .args([&package_path_argument, &unstable_options])
                .args(&target_libraries_options)
                .args([&target_all_executables_option, &target_executables_option]),
            clap::Command::new(subcommand::BUILD)
                .visible_alias("b")
                .about("Compile the given or local package")
                .args([&package_path_argument, &backend_option, &unstable_options])
                .args(&target_libraries_options)
                .args([&target_all_executables_option, &target_executables_option]),
            clap::Command::new(subcommand::RUN)
                .visible_alias("r")
                .about("Run the given or local package")
                .args([
                    &package_path_argument,
                    &backend_option,
                    &unstable_options,
                    &target_executable_option,
                ]),
            clap::Command::new(subcommand::DOCUMENT)
                .visible_aliases(&["doc", "d"])
                .about("Document the given or local package")
                .args([
                    &package_path_argument,
                    &unstable_options,
                    &target_all_executables_option,
                    &target_executables_option,
                ])
                .args(&documentation_arguments)
                .args(target_libraries_options),
            clap::Command::new(subcommand::FILE)
                .visible_alias("f")
                .subcommand_required(true)
                .arg_required_else_help(true)
                .about("Commands for handling single source files")
                .subcommands([
                    clap::Command::new(subcommand::CHECK)
                        .visible_alias("c")
                        .about("Check the given source file for errors")
                        .args(&file_build_arguments)
                        .arg(&unstable_options),
                    clap::Command::new(subcommand::BUILD)
                        .visible_alias("b")
                        .about("Compile the given source file")
                        .args(&file_build_arguments)
                        .args([&backend_option, &unstable_options]),
                    clap::Command::new(subcommand::RUN)
                        .visible_alias("r")
                        .about("Run the given source file")
                        .args(&file_build_arguments)
                        .args([&backend_option, &unstable_options]),
                    clap::Command::new(subcommand::DOCUMENT)
                        .visible_aliases(&["doc", "d"])
                        .about("Document the given source file")
                        .args(file_build_arguments)
                        .arg(unstable_options)
                        .args(documentation_arguments),
                ]),
            #[cfg(feature = "lsp")]
            clap::Command::new(subcommand::SERVE).about("Launch an LSP server"),
            clap::Command::new(subcommand::EXPLAIN)
                .about("Explain given error codes")
                .arg(
                    Arg::new(argument::CODES)
                        .multiple_occurrences(true)
                        .required(true)
                        .help("The error codes that need explanation"),
                ),
            clap::Command::new(subcommand::INITIALIZE)
                .visible_alias("init")
                .about("Create a new package in the current folder")
                .args(&package_creation_arguments),
            clap::Command::new(subcommand::NEW)
                .about("Create a new package")
                .arg(
                    Arg::new(argument::NAME)
                        .required(true)
                        .help("The name of the package"),
                )
                .args(package_creation_arguments),
            clap::Command::new(subcommand::METADATA)
                .about("Check a metadata file for syntax errors")
                .hide(hide_internal_commands())
                .after_help(metadata_subcommand_disclaimer)
                .arg(
                    Arg::new(argument::PATH)
                        .allow_invalid_utf8(true)
                        .required(true)
                        .help("The path to the metadata file"),
                ),
        ])
        .get_matches();

    let command = match matches.subcommand().unwrap() {
        (
            command @ (subcommand::BUILD
            | subcommand::CHECK
            | subcommand::RUN
            | subcommand::DOCUMENT),
            matches,
        ) => {
            let (mode, unstable_build_options) = match command {
                subcommand::BUILD | subcommand::RUN => {
                    let (unstable_build_options, unstable_compilation_options) =
                        unstable::deserialize(matches).map(unstable::Or::split)?;
                    let options =
                        CompilationOptions::deserialize(matches, unstable_compilation_options);

                    let mode = match command {
                        subcommand::BUILD => BuildMode::Build { options },
                        subcommand::RUN => BuildMode::Run { options },
                        _ => unreachable!(),
                    };

                    (mode, unstable_build_options)
                }
                subcommand::CHECK => (BuildMode::Check, unstable::deserialize(matches)?),
                subcommand::DOCUMENT => {
                    let (unstable_build_options, unstable_documentation_options) =
                        unstable::deserialize(matches).map(unstable::Or::split)?;

                    let mode = BuildMode::Document {
                        options: DocumentationOptions::deserialize(
                            matches,
                            unstable_documentation_options,
                        ),
                    };

                    (mode, unstable_build_options)
                }
                _ => unreachable!(),
            };
            Command::BuildPackage {
                options: PackageBuildOptions::deserialize(matches, unstable_build_options, &mode),
                mode,
            }
        }
        (subcommand::FILE, matches) => {
            let (command, matches) = matches.subcommand().unwrap();

            let (mode, unstable_build_options) = match command {
                subcommand::CHECK => (BuildMode::Check, unstable::deserialize(matches)?),
                subcommand::BUILD | subcommand::RUN => {
                    let (unstable_build_options, unstable_compilation_options) =
                        unstable::deserialize(matches).map(unstable::Or::split)?;
                    let options =
                        CompilationOptions::deserialize(matches, unstable_compilation_options);

                    let mode = match command {
                        subcommand::BUILD => BuildMode::Build { options },
                        subcommand::RUN => BuildMode::Run { options },
                        _ => unreachable!(),
                    };

                    (mode, unstable_build_options)
                }
                subcommand::DOCUMENT => {
                    let (unstable_build_options, unstable_documentation_options) =
                        unstable::deserialize(matches).map(unstable::Or::split)?;

                    let mode = BuildMode::Document {
                        options: DocumentationOptions::deserialize(
                            matches,
                            unstable_documentation_options,
                        ),
                    };

                    (mode, unstable_build_options)
                }
                _ => unreachable!(),
            };

            Command::BuildFile {
                mode,
                options: FileBuildOptions::deserialize(matches, unstable_build_options),
            }
        }
        #[cfg(feature = "lsp")]
        (subcommand::SERVE, _matches) => Command::Serve,
        (subcommand::EXPLAIN, _matches) => Command::Explain,
        (command @ (subcommand::INITIALIZE | subcommand::NEW), matches) => Command::CreatePackage {
            mode: match command {
                subcommand::INITIALIZE => PackageCreationMode::Initialize,
                subcommand::NEW => PackageCreationMode::New {
                    package_name: matches.value_of(argument::NAME).unwrap().into(),
                },
                _ => unreachable!(),
            },
            options: PackageCreationOptions::deserialize(matches),
        },
        (subcommand::METADATA, matches) => Command::Metadata {
            path: matches.value_of_os(argument::PATH).unwrap().into(),
        },
        _ => unreachable!(),
    };

    Ok((command, GlobalOptions::deserialize(&matches)))
}

mod subcommand {
    pub(super) const BUILD: &str = "build";
    pub(super) const CHECK: &str = "check";
    pub(super) const DOCUMENT: &str = "document";
    pub(super) const EXPLAIN: &str = "explain";
    pub(super) const FILE: &str = "file";
    pub(super) const INITIALIZE: &str = "initialize";
    pub(super) const METADATA: &str = "metadata";
    pub(super) const NEW: &str = "new";
    pub(super) const RUN: &str = "run";
    #[cfg(feature = "lsp")]
    pub(super) const SERVE: &str = "serve";
}

mod argument {
    pub(super) const CODES: &str = "CODES";
    pub(super) const NAME: &str = "NAME";
    pub(super) const PATH: &str = "PATH";
}

mod option {
    pub(super) const COMPONENT_TYPE: &str = "component_type";
    pub(super) const BACKEND: &str = "backend";
    pub(super) const EXECUTABLE: &str = "executable";
    pub(super) const LIBRARY: &str = "library";
    pub(super) const NO_CORE: &str = "no_core";
    pub(super) const NO_DEPENDENCIES: &str = "no_dependencies";
    pub(super) const OPEN: &str = "open";
    pub(super) const QUIET: &str = "quiet";
    pub(super) const TARGET_ALL_EXECUTABLES: &str = "target-all-executables";
    pub(super) const TARGET_ALL_LIBRARIES: &str = "target-all-libraries";
    pub(super) const TARGET_EXECUTABLES: &str = "target-executables";
    pub(super) const TARGET_LIBRARIES: &str = "target-libraries";
    pub(super) const UNSTABLE_OPTION: &str = "unstable-option";
}

fn hide_internal_commands() -> bool {
    std::env::var_os(LUSHUI_DEVELOPER_ENV_VAR).map_or(true, |variable| variable == "0")
}

const METADATA_SUBCOMMAND_DISCLAIMER: &str = "\
    This subcommand is not subject to any stability guarantees.\n\
    It may be CHANGED in its behavior or REMOVED ENTIRELY at any time and without further notice.\n\
    If this subcommand is executed, the program behavior and\n\
    especially the form of the program output MUST NOT BE RELIED UPON.";

pub enum Command {
    BuildPackage {
        mode: BuildMode,
        options: PackageBuildOptions,
    },
    BuildFile {
        mode: BuildMode,
        options: FileBuildOptions,
    },
    #[cfg(feature = "lsp")]
    Serve,
    Explain,
    CreatePackage {
        mode: PackageCreationMode,
        options: PackageCreationOptions,
    },
    Metadata {
        path: PathBuf,
    },
}

// @Task add --color=always|never|auto
pub struct GlobalOptions {
    pub quiet: bool,
}

impl GlobalOptions {
    fn deserialize(matches: &ArgMatches) -> GlobalOptions {
        Self {
            quiet: matches.is_present(option::QUIET),
        }
    }
}

pub enum BuildMode {
    Check,
    Build { options: CompilationOptions },
    Run { options: CompilationOptions },
    Document { options: DocumentationOptions },
}

pub struct PackageBuildOptions {
    pub general: BuildOptions,
    pub path: Option<PathBuf>,
    pub targets: BuildTargets,
}

impl PackageBuildOptions {
    fn deserialize(
        matches: &ArgMatches,
        unstable_options: Vec<unstable::BuildOption>,
        mode: &BuildMode,
    ) -> Self {
        Self {
            path: matches.value_of_os(argument::PATH).map(Into::into),
            general: BuildOptions::deserialize(unstable_options),
            targets: BuildTargets::deserialize(matches, mode),
        }
    }
}

pub struct BuildTargets(/* empty means any */ Vec<BuildTarget>);

impl BuildTargets {
    fn deserialize(matches: &ArgMatches, mode: &BuildMode) -> Self {
        let should_run = matches!(mode, BuildMode::Run { .. });

        let all_libraries = !should_run && matches.is_present(option::TARGET_ALL_LIBRARIES);
        let all_executables = !should_run && matches.is_present(option::TARGET_ALL_EXECUTABLES);

        if all_libraries && all_executables {
            return Self(Vec::new());
        }

        let mut targets = Vec::new();

        if all_libraries {
            targets.push(BuildTarget {
                name: None,
                type_: Some(ComponentType::Library),
            });
        } else if !should_run && let Some(libraries) = matches.values_of(option::TARGET_LIBRARIES) {
            targets.extend(libraries.into_iter().map(|name| BuildTarget {
                name: Some(name.to_owned()),
                type_: Some(ComponentType::Library),
            }));
        }

        if all_executables {
            targets.push(BuildTarget {
                name: None,
                type_: Some(ComponentType::Executable),
            });
        } else if let Some(executables) = matches.values_of(option::TARGET_EXECUTABLES) {
            targets.extend(executables.into_iter().map(|name| BuildTarget {
                name: Some(name.to_owned()),
                type_: Some(ComponentType::Executable),
            }));
        }

        Self(targets)
    }
}

#[allow(dead_code)] // @Temporary
pub struct BuildTarget {
    // @Task parse it already, make it a Word
    name: Option<String>,
    type_: Option<ComponentType>,
}

pub struct FileBuildOptions {
    pub path: PathBuf,
    pub general: BuildOptions,
    pub no_core: bool,
    pub component_type: Option<ComponentType>,
}

impl FileBuildOptions {
    fn deserialize(matches: &ArgMatches, unstable_options: Vec<unstable::BuildOption>) -> Self {
        Self {
            path: matches.value_of_os(argument::PATH).unwrap().into(),
            general: BuildOptions::deserialize(unstable_options),
            no_core: matches.is_present(option::NO_CORE),
            component_type: matches
                .value_of(option::COMPONENT_TYPE)
                .map(|input| input.parse().unwrap()),
        }
    }
}

#[derive(Default)]
pub struct BuildOptions {
    pub internals: bool,
    pub emit_tokens: bool,
    pub emit_ast: bool,
    pub emit_lowered_ast: bool,
    pub emit_hir: bool,
    pub emit_untyped_bindings: bool,
    pub emit_bindings: bool,
    pub timing: bool,
    pub pass_restriction: Option<PassRestriction>,
}

impl BuildOptions {
    fn deserialize(unstable_options: Vec<unstable::BuildOption>) -> Self {
        let mut options = Self::default();

        for unstable_option in unstable_options {
            use unstable::BuildOption::*;

            match unstable_option {
                Internals => options.internals = true,
                EmitTokens => options.emit_tokens = true,
                EmitAst => options.emit_ast = true,
                EmitLoweredAst => options.emit_lowered_ast = true,
                EmitHir => options.emit_hir = true,
                EmitUntypedBindings => options.emit_untyped_bindings = true,
                EmitBindings => options.emit_bindings = true,
                Timing => options.timing = true,
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
            }
        }

        options
    }
}

#[derive(Default)]
pub struct CompilationOptions {
    pub backend: Backend,
    pub general: CodegenOptions,
}

impl CompilationOptions {
    fn deserialize(
        matches: &ArgMatches,
        unstable_options: Vec<unstable::CompilationOption>,
    ) -> Self {
        #[allow(unused_mut, clippy::needless_update)]
        let mut options = Self {
            backend: matches
                .value_of(option::BACKEND)
                .map(|input| input.parse().unwrap())
                .unwrap_or_default(),
            ..default()
        };

        for unstable_option in unstable_options {
            #[allow(unused_imports)]
            use unstable::CompilationOption::*;

            match unstable_option {
                #[cfg(feature = "cranelift")]
                EmitClif => options.general.emit_clif = true,
                #[cfg(feature = "llvm")]
                EmitLlvmIr => options.general.emit_llvm_ir = true,
                #[cfg(feature = "cranelift")]
                VerifyClif => options.general.verify_clif = true,
                #[cfg(feature = "llvm")]
                VerifyLlvmIr => options.general.verify_llvm_ir = true,
            }
        }

        // @Beacon @Beacon @Beacon @Task disallow conflicting options
        // * backend == Llvm && (emit_clif || verify_clif)
        // * backend == Cranelift && (emit_llvm_ir || verify_llvm_ir)

        options
    }
}

#[derive(Default, FromStr, Str, Elements)]
#[format(dash_case)]
pub enum Backend {
    /// HIRI – The HIR interpreter.
    #[default]
    Hiri,
    /// Cranelift – The CLIF generator.
    #[cfg(feature = "cranelift")]
    Cranelift,
    /// LLVM – The LLVM-IR generator.
    #[cfg(feature = "llvm")]
    Llvm,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum PassRestriction {
    Resolver,
    Lowerer,
    Parser,
    Lexer,
}

pub enum PackageCreationMode {
    Initialize,
    New { package_name: String },
}

#[derive(Clone, Copy)]
pub struct PackageCreationOptions {
    pub no_core: bool,
    pub library: bool,
    pub executable: bool,
}

impl PackageCreationOptions {
    fn deserialize(matches: &ArgMatches) -> Self {
        let library = matches.is_present(option::LIBRARY);

        Self {
            no_core: matches.is_present(option::NO_CORE),
            library,
            // implicitly set when no explicit component type specified
            executable: matches.is_present(option::EXECUTABLE) || !library,
        }
    }
}

#[derive(Default)]
pub struct DocumentationOptions {
    pub open: bool,
    pub no_dependencies: bool,
    pub general: documenter::Options,
}

impl DocumentationOptions {
    fn deserialize(
        matches: &ArgMatches,
        unstable_options: Vec<unstable::DocumentationOption>,
    ) -> Self {
        let mut options = Self {
            open: matches.is_present(option::OPEN),
            no_dependencies: matches.is_present(option::NO_DEPENDENCIES),
            ..default()
        };

        for unstable_option in unstable_options {
            use unstable::DocumentationOption::*;

            match unstable_option {
                AsciiDoc => options.general.asciidoc = true,
                LoremIpsum(amount) => options.general.lorem_ipsum = amount,
            }
        }

        options
    }
}

mod unstable {
    use clap::ArgMatches;
    use colored::Colorize;
    use derivation::{Elements, FromStr, Str};
    use lushui::{
        diagnostics::{Diagnostic, Reporter},
        error::Result,
        utility::{pluralize, Conjunction, ListingExt, QuoteExt},
    };
    use std::{fmt::Write, iter::once, str::FromStr};

    const HELP_OPTION: &str = "help";
    const SEPARATOR: &str = "=";

    pub(super) fn deserialize<O: UnstableOption>(matches: &ArgMatches) -> Result<Vec<O>> {
        let mut options = Vec::new();
        let mut invalid_options = Vec::new();

        if let Some(unparsed_options) = matches.values_of(super::option::UNSTABLE_OPTION) {
            for option in unparsed_options {
                if option == HELP_OPTION {
                    help::<O>();
                }

                if let Ok(option) = option.parse() {
                    options.push(option);
                } else {
                    invalid_options.push(option);
                }
            }
        }

        if !invalid_options.is_empty() {
            Err(Diagnostic::error()
                .message(format!(
                    "invalid unstable {} {}",
                    pluralize!(invalid_options.len(), "option"),
                    invalid_options
                        .into_iter()
                        .map(QuoteExt::quote)
                        .list(Conjunction::And)
                ))
                .report(&Reporter::stderr()))
        } else {
            Ok(options)
        }
    }

    pub(super) trait UnstableOption:
        Copy + Elements + FromStr<Err: Into<ParsingError>>
    {
        fn syntax(self) -> &'static str;
        fn help(self) -> &'static str;
    }

    fn help<O: UnstableOption>() {
        let mut message = "UNSTABLE OPTIONS:\n".yellow().to_string();
        let mut elements: Vec<_> = O::elements()
            .map(|element| (element.syntax(), element.help()))
            .chain(once((HELP_OPTION, "Print help information and halt")))
            .collect();

        let padding = elements
            .iter()
            .map(|(syntax, _)| syntax.len())
            .max()
            .unwrap_or_default();

        elements.sort_by_key(|&(syntax, _)| syntax);

        for (syntax, help) in elements {
            writeln!(
                message,
                "    {} {:<padding$}     {help}",
                "-Z".green(),
                syntax.green(),
            )
            .unwrap();
        }

        writeln!(message).unwrap();

        write!(message, "{}", "\
            These options are not subject to any stability guarantees.\n\
            They may be CHANGED in their behavior or REMOVED ENTIRELY at any time and without further notice.\n\
            If this program is executed with any of these options specified,\n\
            its behavior and especially the form of its output MUST NOT BE RELIED UPON.\
        ".red()).unwrap();

        println!("{message}");
        // @Beacon @Task don't use this function!
        std::process::exit(0);
    }

    #[derive(Clone, Copy, Elements, Str, FromStr)]
    #[format(dash_case)]
    #[str(syntax)]
    pub(super) enum BuildOption {
        EmitAst,
        EmitBindings,
        EmitHir,
        EmitLoweredAst,
        EmitTokens,
        EmitUntypedBindings,
        Internals,
        LexOnly,
        LowerOnly,
        ParseOnly,
        ResolveOnly,
        Timing,
    }

    impl UnstableOption for BuildOption {
        fn syntax(self) -> &'static str {
            BuildOption::syntax(&self)
        }

        fn help(self) -> &'static str {
            match self {
                Self::EmitAst => "Emit the abstract syntax tree (AST) of the current component output by the parser",
                Self::EmitBindings => "Emit the (typed) bindings of the current component after type checking",
                Self::EmitHir => "Emit the high-level intermediate representation (HIR) of the current component output by the resolver",
                Self::EmitLoweredAst => "Emit the lowered AST of the current component",
                Self::EmitTokens => "Emit the tokens of the current component output by the lexer",
                Self::EmitUntypedBindings => "Emit the (untyped) bindings of the current component after name resolution",
                Self::Internals => "Enable internal language and library features",
                Self::LexOnly => "Halt the execution after lexing the current component",
                Self::LowerOnly => "Halt the execution after lowering the current component",
                Self::ParseOnly => "Halt the execution after parsing the current component",
                Self::ResolveOnly => "Halt the execution after resolving the names of the current component",
                Self::Timing => "Print the time of each pass through the current component",
            }
        }
    }

    #[derive(Clone, Copy, Elements, Str, FromStr)]
    #[format(dash_case)]
    #[str(syntax)]
    pub(super) enum CompilationOption {
        #[cfg(feature = "cranelift")]
        EmitClif,
        #[cfg(feature = "llvm")]
        EmitLlvmIr,
        #[cfg(feature = "cranelift")]
        VerifyClif,
        #[cfg(feature = "llvm")]
        VerifyLlvmIr,
        // @Task
        // #[cfg(feature = "llvm")]
        // Linker(Option<String>),
    }

    impl UnstableOption for CompilationOption {
        fn syntax(self) -> &'static str {
            CompilationOption::syntax(&self)
        }

        fn help(self) -> &'static str {
            match self {
                #[cfg(feature = "cranelift")]
                Self::EmitClif => "Emit the generated CLIF",
                #[cfg(feature = "llvm")]
                Self::EmitLlvmIr => "Emit the generated LLVM-IR",
                #[cfg(feature = "cranelift")]
                Self::VerifyClif => "Verify the generated CLIF",
                #[cfg(feature = "llvm")]
                Self::VerifyLlvmIr => "Verify the generated LLVM-IR",
            }
        }
    }

    #[derive(Clone, Copy, Elements)]
    pub(super) enum DocumentationOption {
        AsciiDoc,
        LoremIpsum(Option<usize>),
    }

    impl UnstableOption for DocumentationOption {
        // @Task derive this smh
        fn syntax(self) -> &'static str {
            match self {
                Self::AsciiDoc => "asciidoc",
                Self::LoremIpsum(_) => "lorem-ipsum=<AMOUNT=1>",
            }
        }

        fn help(self) -> &'static str {
            match self {
                Self::AsciiDoc => "Interpret documentation comments as AsciiDoc when generating documentation",
                Self::LoremIpsum(_) => "Replace the documentation of every declaration with ‘amount’ paragraphs of Lorem Ipsum",
            }
        }
    }

    impl FromStr for DocumentationOption {
        type Err = ParsingError;

        fn from_str(source: &str) -> Result<Self, Self::Err> {
            let mut parts = source.splitn(2, SEPARATOR);
            let key = parts.next().ok_or(ParsingError::InvalidSyntax)?;
            let value = parts.next();

            // @Task derive the key mapping logic smh
            let option = match key {
                "asciidoc" => Self::AsciiDoc,
                "lorem-ipsum" => {
                    return Ok(Self::LoremIpsum(
                        value
                            .map(str::parse)
                            .transpose()
                            .map_err(|_| ParsingError::InvalidSyntax)?,
                    ))
                }
                _ => return Err(ParsingError::UndefinedOption),
            };

            if value.is_some() {
                return Err(ParsingError::InvalidSyntax);
            }

            Ok(option)
        }
    }

    #[derive(Clone, Copy)]
    pub(super) enum Or<A, B> {
        Left(A),
        Right(B),
    }

    impl<A, B> Or<A, B> {
        pub(super) fn split(this: Vec<Self>) -> (Vec<A>, Vec<B>) {
            let mut lefts = Vec::new();
            let mut rights = Vec::new();

            for it in this {
                match it {
                    Self::Left(left) => lefts.push(left),
                    Self::Right(right) => rights.push(right),
                }
            }

            (lefts, rights)
        }
    }

    impl<A: UnstableOption, B: UnstableOption> UnstableOption for Or<A, B> {
        fn syntax(self) -> &'static str {
            match self {
                Self::Left(option) => option.syntax(),
                Self::Right(option) => option.syntax(),
            }
        }

        fn help(self) -> &'static str {
            match self {
                Self::Left(option) => option.help(),
                Self::Right(option) => option.help(),
            }
        }
    }

    impl<A: Elements, B: Elements> Elements for Or<A, B> {
        type Iter = impl Iterator<Item = Self>;

        fn elements() -> Self::Iter {
            A::elements()
                .map(Self::Left)
                .chain(B::elements().map(Self::Right))
        }
    }

    impl<A, B> FromStr for Or<A, B>
    where
        A: FromStr<Err: Into<ParsingError>>,
        B: FromStr<Err: Into<ParsingError>>,
    {
        type Err = ParsingError;

        fn from_str(source: &str) -> Result<Self, Self::Err> {
            use ParsingError::*;

            match source.parse().map_err(Into::into) {
                Ok(option) => Ok(Self::Left(option)),
                Err(UndefinedOption) => source.parse().map(Self::Right).map_err(Into::into),
                Err(InvalidSyntax) => Err(InvalidSyntax),
            }
        }
    }

    pub(super) enum ParsingError {
        UndefinedOption,
        InvalidSyntax,
    }

    impl From<()> for ParsingError {
        fn from(_: ()) -> Self {
            Self::UndefinedOption
        }
    }
}
