use clap::{Arg, ArgMatches};
use derivation::{Elements, FromStr, Str};
use lushui::{component::ComponentType, error::Result};
use std::{cmp::max, path::PathBuf};

const TARGET_ALL_LIBRARIES_OPTION: &str = "target-all-libraries";
const TARGET_LIBRARIES_OPTION: &str = "target-libraries";
const TARGET_ALL_EXECUTABLES_OPTION: &str = "target-all-executables";
const TARGET_EXECUTABLES_OPTION: &str = "target-executables";
const UNSTABLE_OPTION: &str = "unstable-option";

pub(crate) fn arguments() -> Result<(Command, GlobalOptions)> {
    const CHECK_SUBCOMMAND: &str = "check";
    const BUILD_SUBCOMMAND: &str = "build";
    const RUN_SUBCOMMAND: &str = "run";
    const DOCUMENT_SUBCOMMAND: &str = "document";
    const FILE_SUBCOMMAND: &str = "file";
    const EXPLAIN_SUBCOMMAND: &str = "explain";
    const INITIALIZE_SUBCOMMAND: &str = "initialize";
    const NEW_SUBCOMMAND: &str = "new";

    let package_path_argument = Arg::new("PATH").allow_invalid_utf8(true).help(
        "The path to a folder containing a package. Defaults to the local package \
         i.e. the first package whose manifest is found starting the search in the current folder \
         then looking through each parent folder.",
    );

    let engine_option = Arg::new("engine")
        .long("engine")
        .takes_value(true)
        .value_name("ENGINE")
        // @Task get rid of closure once fn name takes self by value
        .possible_values(Engine::elements().map(|element| element.name()))
        .help("Set the engine");

    let file_build_arguments = [
        Arg::new("PATH")
            .allow_invalid_utf8(true)
            .required(true)
            .help("The path to a source file"),
        Arg::new("no-core")
            .long("no-core")
            .short('0')
            // @Task rephrase this not using the word "remove"
            .help("Remove the dependency to the standard library `core`"),
        Arg::new("component-type")
            .long("component-type")
            .short('t')
            .value_name("TYPE")
            // @Task write the closure as a function binding once StaticStr derives self by value
            .possible_values(ComponentType::elements().map(|type_| type_.name()))
            .help("Set the component type"),
    ];

    let documentation_arguments = [
        Arg::new("open")
            .long("open")
            .help("Open the documentation in a browser"),
        Arg::new("no-dependencies")
            .long("no-deps")
            .help("Prevent the dependencies from being documented"),
    ];

    let package_creation_arguments = [
        Arg::new("no-core")
            .long("no-core")
            .help("Do not make the new package dependent on the standard library `core`"),
        Arg::new("executable")
            .long("executable")
            .visible_alias("exe")
            .help("Create an executable component in the new package"),
        Arg::new("library")
            .long("library")
            .visible_alias("lib")
            .help("Create a library component in the new package"),
    ];

    let target_libraries_options = [
        Arg::new(TARGET_ALL_LIBRARIES_OPTION)
            .long("libraries")
            .visible_alias("libs")
            .help("Target all libraries"),
        Arg::new(TARGET_LIBRARIES_OPTION)
            .long("library")
            .visible_alias("lib")
            .value_name("NAME")
            .help("Target only the given or primary library"),
    ];

    let target_all_executables_option = Arg::new(TARGET_ALL_EXECUTABLES_OPTION)
        .long("executables")
        .visible_alias("exes")
        .help("Target all executables");

    let target_executable_option = Arg::new(TARGET_EXECUTABLES_OPTION)
        .long("executable")
        .visible_alias("exe")
        .value_name("NAME")
        .help("Target only the given or primary executable");

    let target_executables_option = target_executable_option.clone().multiple_occurrences(true);

    let unstable_options = Arg::new(UNSTABLE_OPTION)
        .short('Z')
        .value_name("OPTION")
        .multiple_occurrences(true)
        .help("Set an unstable option. See `-Z help` for details");

    // @Task use `try_get_matches` (no real block, just def an error type and smh exit with code 2 instead of 1 on error)
    let matches = clap::Command::new(env!("CARGO_PKG_NAME"))
        .version(super::VERSION)
        .about(env!("CARGO_PKG_DESCRIPTION"))
        .subcommand_required(true)
        .arg_required_else_help(true)
        .arg(
            Arg::new("quiet")
                .long("quiet")
                .short('q')
                .global(true)
                .help("Suppress status output from being printed to stdout"),
        )
        .subcommands([
            clap::Command::new(CHECK_SUBCOMMAND)
                .visible_alias("c")
                .about("Check the given or local package for errors")
                .args([&package_path_argument, &engine_option, &unstable_options])
                .args(&target_libraries_options)
                .args([&target_all_executables_option, &target_executables_option]),
            clap::Command::new(BUILD_SUBCOMMAND)
                .visible_alias("b")
                .about("Compile the given or local package")
                .args([&package_path_argument, &engine_option, &unstable_options])
                .args(&target_libraries_options)
                .args([&target_all_executables_option, &target_executables_option]),
            clap::Command::new(RUN_SUBCOMMAND)
                .visible_alias("r")
                .about("Run the given or local package")
                .args([
                    &package_path_argument,
                    &engine_option,
                    &unstable_options,
                    &target_executable_option,
                ]),
            clap::Command::new(DOCUMENT_SUBCOMMAND)
                .visible_aliases(&["doc", "d"])
                .about("Document the given or local package")
                .args([
                    &package_path_argument,
                    &engine_option,
                    &unstable_options,
                    &target_all_executables_option,
                    &target_executables_option,
                ])
                .args(&documentation_arguments)
                .args(target_libraries_options),
            clap::Command::new(FILE_SUBCOMMAND)
                .visible_alias("f")
                .subcommand_required(true)
                .arg_required_else_help(true)
                .about("Commands for handling single source files")
                .subcommands([
                    clap::Command::new(CHECK_SUBCOMMAND)
                        .visible_alias("c")
                        .about("Check the given source file for errors")
                        .args(&file_build_arguments)
                        .args([&engine_option, &unstable_options]),
                    clap::Command::new(BUILD_SUBCOMMAND)
                        .visible_alias("b")
                        .about("Compile the given source file")
                        .args(&file_build_arguments)
                        .args([&engine_option, &unstable_options]),
                    clap::Command::new(RUN_SUBCOMMAND)
                        .visible_alias("r")
                        .about("Run the given source file")
                        .args(&file_build_arguments)
                        .args([&engine_option, &unstable_options]),
                    clap::Command::new(DOCUMENT_SUBCOMMAND)
                        .visible_aliases(&["doc", "d"])
                        .about("Document the given source file")
                        .args(file_build_arguments)
                        .args([engine_option, unstable_options])
                        .args(documentation_arguments),
                ]),
            clap::Command::new(EXPLAIN_SUBCOMMAND)
                .about("Explain given error codes")
                .arg(
                    Arg::new("CODES")
                        .multiple_occurrences(true)
                        .required(true)
                        .help("The error codes that need explanation"),
                ),
            clap::Command::new(INITIALIZE_SUBCOMMAND)
                .visible_alias("init")
                .about("Create a new package in the current folder")
                .args(&package_creation_arguments),
            clap::Command::new(NEW_SUBCOMMAND)
                .about("Create a new package")
                .arg(
                    Arg::new("NAME")
                        .required(true)
                        .help("The name of the package"),
                )
                .args(package_creation_arguments),
        ])
        .get_matches();

    let command = match matches.subcommand().unwrap() {
        (command @ (BUILD_SUBCOMMAND | CHECK_SUBCOMMAND | RUN_SUBCOMMAND), matches) => {
            let mode = match command {
                BUILD_SUBCOMMAND => BuildMode::Build,
                CHECK_SUBCOMMAND => BuildMode::Check,
                RUN_SUBCOMMAND => BuildMode::Run,
                _ => unreachable!(),
            };
            Command::BuildPackage {
                options: PackageBuildOptions::deserialize(
                    matches,
                    unstable::deserialize(matches)?,
                    &mode,
                ),
                mode,
            }
        }
        (DOCUMENT_SUBCOMMAND, matches) => {
            let (unstable_build_options, unstable_documentation_options) =
                unstable::deserialize(matches).map(unstable::Or::split)?;
            let mode = BuildMode::Document {
                options: DocumentationOptions::deserialize(matches, unstable_documentation_options),
            };

            Command::BuildPackage {
                options: PackageBuildOptions::deserialize(matches, unstable_build_options, &mode),
                mode,
            }
        }
        (FILE_SUBCOMMAND, matches) => {
            let (command, matches) = matches.subcommand().unwrap();

            let (unstable_build_options, unstable_documentation_options) =
                unstable::deserialize(matches).map(unstable::Or::split)?;

            match command {
                DOCUMENT_SUBCOMMAND => Command::BuildFile {
                    mode: BuildMode::Document {
                        options: DocumentationOptions::deserialize(
                            matches,
                            unstable_documentation_options,
                        ),
                    },
                    options: FileBuildOptions::deserialize(matches, unstable_build_options),
                },
                command => Command::BuildFile {
                    mode: match command {
                        CHECK_SUBCOMMAND => BuildMode::Check,
                        BUILD_SUBCOMMAND => BuildMode::Build,
                        RUN_SUBCOMMAND => BuildMode::Run,
                        _ => unreachable!(),
                    },
                    options: FileBuildOptions::deserialize(
                        matches,
                        unstable::deserialize(matches)?,
                    ),
                },
            }
        }
        (EXPLAIN_SUBCOMMAND, _matches) => Command::Explain,
        (command @ (INITIALIZE_SUBCOMMAND | NEW_SUBCOMMAND), matches) => Command::CreatePackage {
            mode: match command {
                INITIALIZE_SUBCOMMAND => PackageCreationMode::Initialize,
                NEW_SUBCOMMAND => PackageCreationMode::New {
                    package_name: matches.value_of("NAME").unwrap().into(),
                },
                _ => unreachable!(),
            },
            options: PackageCreationOptions::deserialize(matches),
        },
        _ => unreachable!(),
    };

    Ok((command, GlobalOptions::deserialize(&matches)))
}

pub enum Command {
    BuildPackage {
        mode: BuildMode,
        options: PackageBuildOptions,
    },
    BuildFile {
        mode: BuildMode,
        options: FileBuildOptions,
    },
    Explain,
    CreatePackage {
        mode: PackageCreationMode,
        options: PackageCreationOptions,
    },
}

// @Task add --color=always|never|auto
pub struct GlobalOptions {
    pub quiet: bool,
}

impl GlobalOptions {
    fn deserialize(matches: &ArgMatches) -> GlobalOptions {
        Self {
            quiet: matches.is_present("quiet"),
        }
    }
}

#[derive(Default, FromStr, Str, Elements)]
#[format(dash_case)]
pub enum Engine {
    #[default]
    TreeWalkInterpreter,
    ByteCodeInterpreter,
}

pub enum BuildMode {
    Check,
    Build,
    Run,
    Document { options: DocumentationOptions },
}

pub struct PackageBuildOptions {
    pub general: BuildOptions,
    pub path: Option<PathBuf>,
    pub targets: BuildTargets,
    pub engine: Engine,
}

impl PackageBuildOptions {
    fn deserialize(
        matches: &ArgMatches,
        unstable_options: Vec<unstable::BuildOption>,
        mode: &BuildMode,
    ) -> Self {
        Self {
            path: matches.value_of_os("PATH").map(Into::into),
            general: BuildOptions::deserialize(unstable_options),
            targets: BuildTargets::deserialize(matches, mode),
            engine: matches
                .value_of("engine")
                .map(|input| input.parse().unwrap())
                .unwrap_or_default(),
        }
    }
}

pub struct BuildTargets(/* empty means any */ Vec<BuildTarget>);

impl BuildTargets {
    fn deserialize(matches: &ArgMatches, mode: &BuildMode) -> Self {
        let should_run = matches!(mode, BuildMode::Run);

        let all_libraries = !should_run && matches.is_present(TARGET_ALL_LIBRARIES_OPTION);
        let all_executables = !should_run && matches.is_present(TARGET_ALL_EXECUTABLES_OPTION);

        if all_libraries && all_executables {
            return Self(Vec::new());
        }

        let mut targets = Vec::new();

        if all_libraries {
            targets.push(BuildTarget {
                name: None,
                type_: Some(ComponentType::Library),
            });
        } else if !should_run && let Some(libraries) = matches.values_of(TARGET_LIBRARIES_OPTION) {
            targets.extend(libraries.into_iter().map(|name| BuildTarget {
                name: Some(name.to_owned()),
                type_: Some(ComponentType::Library),
            }))
        }

        if all_executables {
            targets.push(BuildTarget {
                name: None,
                type_: Some(ComponentType::Executable),
            });
        } else if let Some(executables) = matches.values_of(TARGET_EXECUTABLES_OPTION) {
            targets.extend(executables.into_iter().map(|name| BuildTarget {
                name: Some(name.to_owned()),
                type_: Some(ComponentType::Executable),
            }))
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
    pub engine: Engine,
}

impl FileBuildOptions {
    fn deserialize(matches: &ArgMatches, unstable_options: Vec<unstable::BuildOption>) -> Self {
        Self {
            path: matches.value_of_os("PATH").unwrap().into(),
            general: BuildOptions::deserialize(unstable_options),
            no_core: matches.is_present("no-core"),
            component_type: matches
                .value_of("component-type")
                .map(|input| input.parse().unwrap()),
            engine: matches
                .value_of("engine")
                .map(|input| input.parse().unwrap())
                .unwrap_or_default(),
        }
    }
}

#[derive(Default)]
#[allow(clippy::struct_excessive_bools)] // not a state machine
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
        let library = matches.is_present("library");

        Self {
            no_core: matches.is_present("no-core"),
            library,
            // implicitly set when no explicit component type specified
            executable: matches.is_present("executable") || !library,
        }
    }
}

pub struct DocumentationOptions {
    pub open: bool,
    pub no_dependencies: bool,
    // unstable
    pub asciidoc: bool,
    // unstable
    pub lorem_ipsum: Option<usize>,
}

impl DocumentationOptions {
    fn deserialize(
        matches: &ArgMatches,
        unstable_options: Vec<unstable::DocumentationOption>,
    ) -> Self {
        let mut options = Self {
            open: matches.is_present("open"),
            no_dependencies: matches.is_present("no-dependencies"),
            asciidoc: false,
            lorem_ipsum: None,
        };

        for unstable_option in unstable_options {
            use unstable::DocumentationOption::*;

            match unstable_option {
                AsciiDoc => options.asciidoc = true,
                LoremIpsum(amount) => options.lorem_ipsum = amount,
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
        diagnostics::{reporter::StderrReporter, Diagnostic},
        error::Result,
        utility::{pluralize, Conjunction, QuoteExt, UnorderedListingExt},
    };
    use std::{
        iter::{once, Chain, Map},
        str::FromStr,
    };

    const HELP_OPTION: &str = "help";

    pub(super) fn deserialize<O: UnstableOption>(matches: &ArgMatches) -> Result<Vec<O>> {
        let mut options = Vec::new();
        let mut invalid_options = Vec::new();

        if let Some(unparsed_options) = matches.values_of(super::UNSTABLE_OPTION) {
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
                .report(&StderrReporter::new(None).into()))
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
            message += &format!(
                "    {} {:<padding$}     {help}\n",
                "-Z".green(),
                syntax.green(),
            );
        }

        for &line in DISCLAIMER {
            message += &format!("\n    {}", line.red());
        }

        println!("{message}");
        // @Beacon @Task don't use this function!
        std::process::exit(0);
    }

    const DISCLAIMER: &[&str] = &[
        "These options are not subject to any stability guarantees.",
        "They MAY BE CHANGED in their behavior or REMOVED ENTIRELY at any time and without further notice.",
        "If this program is executed with any of these options specified,",
        "its behavior and especially the form of its output MUST NOT BE RELIED UPON.",
    ];

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

    #[derive(Clone, Copy, Elements)]
    pub(super) enum DocumentationOption {
        AsciiDoc,
        LoremIpsum(Option<usize>),
    }

    impl UnstableOption for DocumentationOption {
        fn syntax(self) -> &'static str {
            match self {
                Self::AsciiDoc => "asciidoc",
                Self::LoremIpsum(_) => "lorem-ipsum=[<amount=1>]",
            }
        }

        fn help(self) -> &'static str {
            match self {
                Self::AsciiDoc => "Interpret documentation comments as AsciiDoc when generating documentation",
                Self::LoremIpsum(_) => "Replace the documentation of every declaration with `amount` paragraphs of Lorem Ipsum",
            }
        }
    }

    impl FromStr for DocumentationOption {
        type Err = ParsingError;

        fn from_str(source: &str) -> Result<Self, Self::Err> {
            Ok(match source {
                "asciidoc" => Self::AsciiDoc,
                "lorem-ipsum" => Self::LoremIpsum(Some(1)),
                _ => match source
                    .split_once('=')
                    .ok_or(ParsingError::UndefinedOption)?
                {
                    ("lorem-ipsum", amount) => Self::LoremIpsum(Some(
                        amount.parse().map_err(|_| ParsingError::InvalidSyntax)?,
                    )),
                    _ => return Err(ParsingError::UndefinedOption),
                },
            })
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
        type Iter = Chain<Map<A::Iter, fn(A) -> Self>, Map<B::Iter, fn(B) -> Self>>;

        fn elements() -> Self::Iter {
            A::elements()
                .map(Self::Left as _)
                .chain(B::elements().map(Self::Right as _))
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
