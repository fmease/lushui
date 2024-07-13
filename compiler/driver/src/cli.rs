use crate::create::PackageCreationOptions;
use clap::{
    builder::{PossibleValue, TypedValueParser, ValueParser},
    Arg, ArgAction, ArgMatches,
};
use derivation::{Elements, FromStr, Str};
use diagnostics::error::Result;
use lexer::word::Word;
use package::ComponentFilter;
use session::unit::CompTy;
use std::{cmp::max, ffi::OsStr, io::Write, path::PathBuf};
use utility::{
    default,
    paint::{paint_to_string, AnsiColor, ColorChoice},
};

// @Task update the color scheme of the `-Zhelp` output from clap-3-style to clap-4-style
//       i.e. from upper-case section titles and the use of yellow & green to
//       bold & underlined section titles and the use of bold letters

// FIXME: Make use of `clap`'s `Lazy`.

pub(crate) fn arguments() -> Result<(Command, GlobalOptions)> {
    let short_version = format!(
        "{} ({} {})",
        env!("VERSION"),
        env!("SHORT_COMMIT_HASH"),
        env!("COMMIT_DATE"),
    );

    let long_version = format!(
        "\
        {}\n\
        commit hash : {}\n\
        commit date : {}\n\
        features    : {}\n\
        profile     : {}\n\
        host        : {}",
        env!("VERSION"),
        env!("COMMIT_HASH"),
        env!("COMMIT_DATE"),
        env!("FEATURES"),
        env!("PROFILE"),
        env!("TARGET"),
    );

    let recnot_subcommand_disclaimer = paint_to_string(
        |painter| {
            painter.set(AnsiColor::Red)?;
            write!(painter, "{RECNOT_SUBCOMMAND_DISCLAIMER}")?;
            painter.unset()
        },
        ColorChoice::Auto,
    )
    .unwrap();

    let path_arg = Arg::new(argument::PATH).value_parser(ValueParser::path_buf());

    let pkg_path_arg = path_arg.clone().help(
        "The path to a folder containing a package. Defaults to the local package \
         i.e. the first package whose manifest is found starting the search in the current folder \
         then looking through each parent folder",
    );

    let backend_opt = Arg::new(option::BACKEND)
        .long("backend")
        .short('b')
        .value_name("BACKEND")
        .value_parser(BackendParser)
        .help("Set the backend");

    let comp_ty_opt = Arg::new(option::COMPONENT_TYPE)
        .long("component-type")
        .short('t')
        .value_name("TYPE")
        .value_parser(ComponentTypeParser);

    let file_build_args = [
        path_arg
            .clone()
            .required(true)
            .help("The path to a source file"),
        Arg::new(option::NO_CORE)
            .long("no-core")
            .short('0')
            .action(ArgAction::SetTrue)
            .help("Drop the default dependency on the standard library ‘core’"),
        comp_ty_opt.clone().help("Set the component type"),
    ];

    let doc_args = [
        Arg::new(option::OPEN)
            .long("open")
            .short('o')
            .action(ArgAction::SetTrue)
            .help("Open the documentation in a browser"),
        Arg::new(option::NO_DEPENDENCIES)
            .long("no-deps")
            .action(ArgAction::SetTrue)
            .help("Prevent the dependencies from being documented"),
    ];

    let pkg_creation_args = [
        Arg::new(option::NO_CORE)
            .long("no-core")
            .action(ArgAction::SetTrue)
            .help("Drop the default dependency on the standard library ‘core’"),
        Arg::new(option::EXECUTABLE)
            .long("executable")
            .visible_alias("exe")
            .action(ArgAction::SetTrue)
            .help("Create an executable component in the new package"),
        Arg::new(option::LIBRARY)
            .long("library")
            .visible_alias("lib")
            .action(ArgAction::SetTrue)
            .help("Create a library component in the new package"),
    ];

    let filter_opts = [
        Arg::new(option::COMPONENT)
            .long("component")
            .short('c')
            .value_name(argument::NAME)
            .action(ArgAction::Append)
            .value_parser(WordParser)
            .help("Target only the given component"),
        comp_ty_opt
            .action(ArgAction::Append)
            .help("Target only components of the given type"),
    ];

    let unstable_opts = Arg::new(option::UNSTABLE_OPTION)
        .short('Z')
        .value_name("OPTION")
        .action(ArgAction::Append)
        .help("Set an unstable option. See ‘-Z help’ for details");

    // @Task use `try_get_matches` (no real block, just def an error type and smh exit with code 2 instead of 1 on error)
    let matches = clap::Command::new("lushui")
        .bin_name("lushui")
        .version(short_version)
        .long_version(long_version)
        .about("The reference compiler of the Lushui programming language")
        .subcommand_required(true)
        .arg_required_else_help(true)
        .args([
            Arg::new(option::QUIET)
                .long("quiet")
                .short('q')
                .global(true)
                .action(ArgAction::SetTrue)
                .help("Suppress status output from being printed to stdout"),
            Arg::new(option::COLOR)
                .long("color")
                .global(true)
                .value_name("WHEN")
                .value_parser(ColorChoiceParser)
                .help("Control when to use color"),
        ])
        .subcommands([
            clap::Command::new(subcommand::CHECK)
                .visible_alias("c")
                .about("Check the given or local package for errors")
                .args([&pkg_path_arg, &unstable_opts])
                .args(&filter_opts),
            clap::Command::new(subcommand::BUILD)
                .visible_alias("b")
                .about("Compile the given or local package")
                .args([&pkg_path_arg, &backend_opt, &unstable_opts])
                .args(&filter_opts),
            clap::Command::new(subcommand::RUN)
                .visible_alias("r")
                .about("Run the given or local package")
                .args([&pkg_path_arg, &backend_opt, &unstable_opts])
                .args(&filter_opts),
            clap::Command::new(subcommand::DOCUMENT)
                .visible_aliases(["doc", "d"])
                .about("Document the given or local package")
                .args([&pkg_path_arg, &unstable_opts])
                .args(&filter_opts)
                .args(&doc_args),
            clap::Command::new(subcommand::FILE)
                .visible_alias("f")
                .subcommand_required(true)
                .arg_required_else_help(true)
                .about("Commands for handling single source files")
                .subcommands([
                    clap::Command::new(subcommand::CHECK)
                        .visible_alias("c")
                        .about("Check the given source file for errors")
                        .args(&file_build_args)
                        .arg(&unstable_opts),
                    clap::Command::new(subcommand::BUILD)
                        .visible_alias("b")
                        .about("Compile the given source file")
                        .args(&file_build_args)
                        .args([&backend_opt, &unstable_opts]),
                    clap::Command::new(subcommand::RUN)
                        .visible_alias("r")
                        .about("Run the given source file")
                        .args(&file_build_args)
                        .args([&backend_opt, &unstable_opts]),
                    clap::Command::new(subcommand::DOCUMENT)
                        .visible_aliases(["doc", "d"])
                        .about("Document the given source file")
                        .args(file_build_args)
                        .arg(unstable_opts)
                        .args(doc_args),
                ]),
            #[cfg(feature = "lsp")]
            clap::Command::new(subcommand::SERVE).about("Launch an LSP server"),
            clap::Command::new(subcommand::EXPLAIN)
                .about("Explain given error codes")
                .arg(
                    Arg::new(argument::CODES)
                        .action(ArgAction::Append)
                        .required(true)
                        .help("The error codes that need explanation"),
                ),
            clap::Command::new(subcommand::INITIALIZE)
                .visible_alias("init")
                .about("Create a new package in the current folder")
                .args(&pkg_creation_args),
            clap::Command::new(subcommand::NEW)
                .about("Create a new package in a new folder")
                .arg(
                    Arg::new(argument::NAME)
                        .required(true)
                        .value_parser(WordParser)
                        .help("The name of the package"),
                )
                .args(pkg_creation_args),
            clap::Command::new(subcommand::RECNOT)
                .about("Check a Recnot file for syntax errors")
                .hide(true)
                .after_help(recnot_subcommand_disclaimer)
                .arg(path_arg.required(true).help("The path to the Recnot file")),
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
            let (mode, unstable_build_opts) = match command {
                subcommand::BUILD | subcommand::RUN => {
                    let (unstable_build_options, unstable_compilation_opts) =
                        unstable::deserialize(matches).map(unstable::Or::split)?;
                    let options =
                        CompilationOptions::deserialize(matches, unstable_compilation_opts);

                    let mode = match command {
                        subcommand::BUILD => BuildMode::Compile { opts: options },
                        subcommand::RUN => BuildMode::Run { opts: options },
                        _ => unreachable!(),
                    };

                    (mode, unstable_build_options)
                }
                subcommand::CHECK => (BuildMode::Check, unstable::deserialize(matches)?),
                subcommand::DOCUMENT => {
                    let (unstable_build_opts, unstable_doc_opts) =
                        unstable::deserialize(matches).map(unstable::Or::split)?;

                    let mode = BuildMode::Document {
                        opts: DocumentationOptions::deserialize(matches, unstable_doc_opts),
                    };

                    (mode, unstable_build_opts)
                }
                _ => unreachable!(),
            };
            Command::BuildPackage {
                mode,
                opts: PackageBuildOptions::deserialize(matches, unstable_build_opts),
            }
        }
        (subcommand::FILE, matches) => {
            let (command, matches) = matches.subcommand().unwrap();

            let (mode, unstable_build_options) = match command {
                subcommand::CHECK => (BuildMode::Check, unstable::deserialize(matches)?),
                subcommand::BUILD | subcommand::RUN => {
                    let (unstable_build_opts, unstable_compilation_opts) =
                        unstable::deserialize(matches).map(unstable::Or::split)?;
                    let options =
                        CompilationOptions::deserialize(matches, unstable_compilation_opts);

                    let mode = match command {
                        subcommand::BUILD => BuildMode::Compile { opts: options },
                        subcommand::RUN => BuildMode::Run { opts: options },
                        _ => unreachable!(),
                    };

                    (mode, unstable_build_opts)
                }
                subcommand::DOCUMENT => {
                    let (unstable_build_opts, unstable_doc_opts) =
                        unstable::deserialize(matches).map(unstable::Or::split)?;

                    let mode = BuildMode::Document {
                        opts: DocumentationOptions::deserialize(matches, unstable_doc_opts),
                    };

                    (mode, unstable_build_opts)
                }
                _ => unreachable!(),
            };

            Command::BuildFile {
                mode,
                opts: FileBuildOptions::deserialize(matches, unstable_build_options),
            }
        }
        #[cfg(feature = "lsp")]
        (subcommand::SERVE, _matches) => Command::Serve,
        (subcommand::EXPLAIN, _matches) => Command::Explain,
        (command @ (subcommand::INITIALIZE | subcommand::NEW), matches) => Command::CreatePackage {
            mode: match command {
                subcommand::INITIALIZE => PackageCreationMode::Initialize,
                subcommand::NEW => PackageCreationMode::New {
                    pkg_name: matches.get_one(argument::NAME).copied().unwrap(),
                },
                _ => unreachable!(),
            },
            opts: PackageCreationOptions::deserialize(matches),
        },
        (subcommand::RECNOT, matches) => Command::Recnot {
            path: matches.get_one(argument::PATH).cloned().unwrap(),
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
    pub(super) const NEW: &str = "new";
    pub(super) const RECNOT: &str = "recnot";
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
    pub(super) const BACKEND: &str = "backend";
    pub(super) const COLOR: &str = "color";
    pub(super) const COMPONENT_TYPE: &str = "component_type";
    pub(super) const COMPONENT: &str = "component";
    pub(super) const EXECUTABLE: &str = "executable";
    pub(super) const LIBRARY: &str = "library";
    pub(super) const NO_CORE: &str = "no_core";
    pub(super) const NO_DEPENDENCIES: &str = "no_dependencies";
    pub(super) const OPEN: &str = "open";
    pub(super) const QUIET: &str = "quiet";
    pub(super) const UNSTABLE_OPTION: &str = "unstable_option";
}

const RECNOT_SUBCOMMAND_DISCLAIMER: &str = "\
    This subcommand is not subject to any stability guarantees.\n\
    It may be CHANGED in its behavior or REMOVED ENTIRELY at any time and without further notice.\n\
    If this subcommand is executed, the program behavior and\n\
    especially the form of the program output MUST NOT BE RELIED UPON.";

pub(crate) enum Command {
    BuildPackage {
        mode: BuildMode,
        opts: PackageBuildOptions,
    },
    BuildFile {
        mode: BuildMode,
        opts: FileBuildOptions,
    },
    #[cfg(feature = "lsp")]
    Serve,
    Explain,
    CreatePackage {
        mode: PackageCreationMode,
        opts: PackageCreationOptions,
    },
    Recnot {
        path: PathBuf,
    },
}

pub(crate) struct GlobalOptions {
    pub(crate) quiet: bool,
    pub(crate) color: ColorChoice,
}

impl GlobalOptions {
    fn deserialize(matches: &ArgMatches) -> GlobalOptions {
        Self {
            quiet: matches.get_flag(option::QUIET),
            color: matches.get_one(option::COLOR).copied().unwrap_or_default(),
        }
    }
}

#[derive(Clone)]
struct ColorChoiceParser;

impl TypedValueParser for ColorChoiceParser {
    type Value = ColorChoice;

    fn parse_ref(
        &self,
        _: &clap::Command,
        _: Option<&Arg>,
        source: &std::ffi::OsStr,
    ) -> Result<Self::Value, clap::Error> {
        let source = parse_utf8(source)?;

        source.parse().map_err(|()| {
            // @Task smh. avoid using `Error::raw` and smh. pass along the context.
            //       https://github.com/clap-rs/clap/discussions/4029
            clap::Error::raw(
                clap::error::ErrorKind::InvalidValue,
                format!("‘{source}’ is not a valid color choice\n"),
            )
        })
    }

    fn possible_values(&self) -> Option<Box<dyn Iterator<Item = PossibleValue>>> {
        Some(Box::new(
            ColorChoice::elements().map(|mode| PossibleValue::new(mode.name())),
        ))
    }
}

#[derive(Str)]
#[format(dash_case)]
pub(crate) enum BuildMode {
    Check,
    Compile { opts: CompilationOptions },
    Run { opts: CompilationOptions },
    Document { opts: DocumentationOptions },
}

pub(crate) struct PackageBuildOptions {
    pub(crate) general: BuildOptions,
    pub(crate) path: Option<PathBuf>,
    pub(crate) filter: ComponentFilter,
}

impl PackageBuildOptions {
    fn deserialize(matches: &ArgMatches, unstable_opts: Vec<unstable::BuildOption>) -> Self {
        Self {
            path: matches.get_one(argument::PATH).cloned(),
            general: BuildOptions::deserialize(unstable_opts),
            filter: ComponentFilter::deserialize(matches),
        }
    }
}

trait DeserializeExt {
    fn deserialize(matches: &ArgMatches) -> Self;
}

impl DeserializeExt for ComponentFilter {
    fn deserialize(matches: &ArgMatches) -> Self {
        let mut filter = Self::default();

        if let Some(components) = matches.get_many(option::COMPONENT) {
            filter.names.extend(components);
        }

        if let Some(types) = matches.get_many::<CompTy>(option::COMPONENT_TYPE) {
            filter.types.extend(types);
        }

        filter
    }
}

pub(crate) struct FileBuildOptions {
    pub(crate) path: PathBuf,
    pub(crate) general: BuildOptions,
    pub(crate) no_core: bool,
    pub(crate) comp_ty: Option<CompTy>,
}

impl FileBuildOptions {
    fn deserialize(matches: &ArgMatches, unstable_opts: Vec<unstable::BuildOption>) -> Self {
        Self {
            path: matches.get_one(argument::PATH).cloned().unwrap(),
            general: BuildOptions::deserialize(unstable_opts),
            no_core: matches.get_flag(option::NO_CORE),
            comp_ty: matches.get_one(option::COMPONENT_TYPE).copied(),
        }
    }
}

#[derive(Default)]
pub(crate) struct BuildOptions {
    pub(crate) internals: bool,
    pub(crate) emit_tokens: bool,
    pub(crate) emit_ast: bool,
    pub(crate) emit_lo_ast: bool,
    pub(crate) emit_hir: bool,
    pub(crate) emit_untyped_bindings: bool,
    pub(crate) emit_bindings: bool,
    pub(crate) timing: bool,
    pub(crate) pass_restriction: Option<PassRestriction>,
}

impl BuildOptions {
    fn deserialize(unstable_opts: Vec<unstable::BuildOption>) -> Self {
        let mut options = Self::default();

        for unstable_option in unstable_opts {
            use unstable::BuildOption::*;

            match unstable_option {
                Internals => options.internals = true,
                EmitTokens => options.emit_tokens = true,
                EmitAst => options.emit_ast = true,
                EmitLoAst => options.emit_lo_ast = true,
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
pub(crate) struct CompilationOptions {
    pub(crate) backend: Backend,
    #[cfg(feature = "cranelift")]
    pub(crate) emit_clif: bool,
    #[cfg(feature = "llvm")]
    pub(crate) emit_llvm_ir: bool,
    #[cfg(feature = "cranelift")]
    pub(crate) verify_clif: bool,
    #[cfg(feature = "llvm")]
    pub(crate) verify_llvm_ir: bool,
}

impl CompilationOptions {
    fn deserialize(matches: &ArgMatches, unstable_opts: Vec<unstable::CompilationOption>) -> Self {
        #[allow(unused_mut, clippy::needless_update)]
        let mut opts = Self {
            // @Task instead of unwrap_or_default use clap's way sth sth default_value
            backend: matches
                .get_one(option::BACKEND)
                .copied()
                .unwrap_or_default(),
            ..default()
        };

        for unstable_opt in unstable_opts {
            #[allow(unused_imports)]
            use unstable::CompilationOption::*;

            match unstable_opt {
                #[cfg(feature = "cranelift")]
                EmitClif => opts.emit_clif = true,
                #[cfg(feature = "llvm")]
                EmitLlvmIr => opts.emit_llvm_ir = true,
                #[cfg(feature = "cranelift")]
                VerifyClif => opts.verify_clif = true,
                #[cfg(feature = "llvm")]
                VerifyLlvmIr => opts.verify_llvm_ir = true,
            }
        }

        // @Beacon @Task disallow conflicting options
        // * backend == Llvm && (emit_clif || verify_clif)
        // * backend == Cranelift && (emit_llvm_ir || verify_llvm_ir)

        opts
    }
}

#[derive(Default, FromStr, Str, Elements, Clone, Copy)]
#[format(dash_case)]
pub(crate) enum Backend {
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
pub(crate) enum PassRestriction {
    Resolver,
    Lowerer,
    Parser,
    Lexer,
}

pub(crate) enum PackageCreationMode {
    Initialize,
    New { pkg_name: Word },
}

impl PackageCreationOptions {
    fn deserialize(matches: &ArgMatches) -> Self {
        let library = matches.get_flag(option::LIBRARY);

        Self {
            no_core: matches.get_flag(option::NO_CORE),
            library,
            // implicitly set when no explicit component type specified
            executable: matches.get_flag(option::EXECUTABLE) || !library,
        }
    }
}

#[derive(Default)]
pub(crate) struct DocumentationOptions {
    pub(crate) open: bool,
    pub(crate) no_deps: bool,
    pub(crate) general: documenter::Options,
}

impl DocumentationOptions {
    fn deserialize(
        matches: &ArgMatches,
        unstable_opts: Vec<unstable::DocumentationOption>,
    ) -> Self {
        let mut options = Self {
            open: matches.get_flag(option::OPEN),
            no_deps: matches.get_flag(option::NO_DEPENDENCIES),
            ..default()
        };

        for unstable_opt in unstable_opts {
            use unstable::DocumentationOption::*;

            match unstable_opt {
                AsciiDoc => options.general.asciidoc = true,
                LoremIpsum(amount) => options.general.lorem_ipsum = amount,
            }
        }

        options
    }
}

#[derive(Clone)]
struct WordParser;

impl TypedValueParser for WordParser {
    type Value = Word;

    fn parse_ref(
        &self,
        _: &clap::Command,
        _: Option<&Arg>,
        source: &OsStr,
    ) -> Result<Self::Value, clap::Error> {
        let source = parse_utf8(source)?;

        Word::parse(source.to_owned()).map_err(|()| {
            // @Task smh. avoid using `Error::raw` and smh. pass along the context.
            //       https://github.com/clap-rs/clap/discussions/4029
            clap::Error::raw(
                clap::error::ErrorKind::InvalidValue,
                format!("‘{source}’ is not a valid word\n"),
            )
        })
    }
}

#[derive(Clone, Copy)]
struct ComponentTypeParser;

impl TypedValueParser for ComponentTypeParser {
    type Value = CompTy;

    fn parse_ref(
        &self,
        _: &clap::Command,
        _: Option<&Arg>,
        source: &OsStr,
    ) -> Result<Self::Value, clap::Error> {
        let source = parse_utf8(source)?;

        // @Task smh. also support the shorthands (e.g. `exe`, `lib`)
        source.parse().map_err(|()| {
            // @Task smh. avoid using `Error::raw` and smh. pass along the context.
            //       https://github.com/clap-rs/clap/discussions/4029
            clap::Error::raw(
                clap::error::ErrorKind::InvalidValue,
                format!("‘{source}’ is not a valid component type\n"),
            )
        })
    }

    fn possible_values(&self) -> Option<Box<dyn Iterator<Item = PossibleValue>>> {
        Some(Box::new(
            CompTy::elements().map(|ty| PossibleValue::new(ty.name())),
        ))
    }
}

#[derive(Clone, Copy)]
struct BackendParser;

impl TypedValueParser for BackendParser {
    type Value = Backend;

    fn parse_ref(
        &self,
        _: &clap::Command,
        _: Option<&Arg>,
        source: &OsStr,
    ) -> Result<Self::Value, clap::Error> {
        let source = parse_utf8(source)?;

        source.parse().map_err(|()| {
            // @Task smh. avoid using `Error::raw` and smh. pass along the context.
            //       https://github.com/clap-rs/clap/discussions/4029
            clap::Error::raw(
                clap::error::ErrorKind::InvalidValue,
                format!("‘{source}’ is not a valid backend\n"),
            )
        })
    }

    fn possible_values(&self) -> Option<Box<dyn Iterator<Item = PossibleValue>>> {
        Some(Box::new(
            Backend::elements().map(|backend| PossibleValue::new(backend.name())),
        ))
    }
}

mod unstable {
    use clap::ArgMatches;
    use derivation::{Elements, FromStr, Str};
    use diagnostics::{error::Result, Diag, Reporter};
    use std::{
        io::{self, Write},
        iter::once,
        str::FromStr,
    };
    use utility::{
        paint::{AnsiColor, ColorChoice, Effects, Painter},
        pluralize, Conjunction, ListingExt, QuoteExt,
    };

    const HELP_OPTION: &str = "help";
    const SEPARATOR: &str = "=";

    pub(super) fn deserialize<O: UnstableOption>(matches: &ArgMatches) -> Result<Vec<O>> {
        let mut options = Vec::new();
        let mut invalid_options = Vec::new();

        if let Some(unparsed_options) = matches.get_many::<String>(super::option::UNSTABLE_OPTION) {
            for option in unparsed_options {
                if option == HELP_OPTION {
                    help::<O>().unwrap();
                }

                if let Ok(option) = option.parse() {
                    options.push(option);
                } else {
                    invalid_options.push(option);
                }
            }
        }

        if !invalid_options.is_empty() {
            Err(Diag::error()
                .message(format!(
                    "invalid unstable {} {}",
                    pluralize!(invalid_options.len(), "option"),
                    invalid_options
                        .into_iter()
                        .map(QuoteExt::quote)
                        .list(Conjunction::And)
                ))
                .report(&Reporter::stderr(ColorChoice::Auto)))
        } else {
            Ok(options)
        }
    }

    pub(super) trait UnstableOption:
        Copy + Elements + FromStr<Err: Into<ParseError>>
    {
        fn syntax(self) -> &'static str;
        fn help(self) -> &'static str;
    }

    fn help<O: UnstableOption>() -> io::Result<!> {
        let mut stdout = Painter::stdout(ColorChoice::Auto);

        stdout.set(Effects::BOLD | Effects::UNDERLINE)?;
        writeln!(stdout, "Unstable Options:")?;
        stdout.unset()?;

        let mut elems: Vec<_> = O::elements()
            .map(|elem| (elem.syntax(), elem.help()))
            .chain(once((HELP_OPTION, "Print help information and halt")))
            .collect();

        let padding = elems
            .iter()
            .map(|(syntax, _)| syntax.len())
            .max()
            .unwrap_or_default();

        elems.sort_by_key(|&(syntax, _)| syntax);

        for (syntax, help) in elems {
            stdout.set(Effects::BOLD)?;
            write!(stdout, "    -Z {syntax:<padding$}     ")?;
            stdout.unset()?;
            writeln!(stdout, "{help}")?;
        }

        writeln!(stdout)?;

        stdout.set(AnsiColor::Red)?;
        writeln!(stdout, "\
            These options are not subject to any stability guarantees.\n\
            They may be CHANGED in their behavior or REMOVED ENTIRELY at any time and without further notice.\n\
            If this program is executed with any of these options specified,\n\
            its behavior and especially the form of its output MUST NOT BE RELIED UPON.\
        ")?;
        stdout.unset()?;

        // FIXME: Don't use `exit` since it doesn't allow destructors to run.
        //        Instead, bubble up this piece of information and handle it
        //        in `main`.
        stdout.flush()?;
        std::process::exit(0)
    }

    #[derive(Clone, Copy, Elements, Str, FromStr)]
    #[format(dash_case)]
    #[str(syntax)]
    pub(super) enum BuildOption {
        EmitAst,
        EmitBindings,
        EmitHir,
        EmitLoAst,
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
                Self::EmitLoAst => "Emit the lowered AST (Lo-AST) of the current component",
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
        type Err = ParseError;

        fn from_str(source: &str) -> Result<Self, Self::Err> {
            let mut parts = source.splitn(2, SEPARATOR);
            let key = parts.next().ok_or(ParseError::InvalidSyntax)?;
            let value = parts.next();

            // @Task derive the key mapping logic smh
            let option = match key {
                "asciidoc" => Self::AsciiDoc,
                "lorem-ipsum" => {
                    return Ok(Self::LoremIpsum(
                        value
                            .map(str::parse)
                            .transpose()
                            .map_err(|_| ParseError::InvalidSyntax)?,
                    ))
                }
                _ => return Err(ParseError::UndefinedOption),
            };

            if value.is_some() {
                return Err(ParseError::InvalidSyntax);
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
        A: FromStr<Err: Into<ParseError>>,
        B: FromStr<Err: Into<ParseError>>,
    {
        type Err = ParseError;

        fn from_str(source: &str) -> Result<Self, Self::Err> {
            use ParseError::*;

            match source.parse().map_err(Into::into) {
                Ok(option) => Ok(Self::Left(option)),
                Err(UndefinedOption) => source.parse().map(Self::Right).map_err(Into::into),
                Err(InvalidSyntax) => Err(InvalidSyntax),
            }
        }
    }

    pub(super) enum ParseError {
        UndefinedOption,
        InvalidSyntax,
    }

    impl From<()> for ParseError {
        fn from((): ()) -> Self {
            Self::UndefinedOption
        }
    }
}

fn parse_utf8(source: &OsStr) -> Result<&str, clap::Error> {
    source.to_str().ok_or_else(|| {
        // @Task smh. avoid using `Error::raw` and smh. pass along the context.
        //       https://github.com/clap-rs/clap/discussions/4029
        clap::Error::raw(
            clap::error::ErrorKind::InvalidUtf8,
            format!("‘{}’ is not valid UTF-8\n", source.to_string_lossy()),
        )
    })
}
