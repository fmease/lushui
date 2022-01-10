#![feature(
    backtrace,
    derive_default_enum,
    decl_macro,
    default_free_fn,
    let_else,
    label_break_value
)]
#![forbid(rust_2018_idioms, unused_must_use)]
#![warn(clippy::pedantic)]
#![allow(
    clippy::result_unit_err, // using a reporter to forward information
    clippy::items_after_statements,
    clippy::enum_glob_use,
    clippy::must_use_candidate,
    clippy::missing_errors_doc,
    clippy::too_many_lines,
    clippy::module_name_repetitions,
    clippy::match_bool,
    clippy::empty_enum,
    clippy::single_match_else,
    clippy::if_not_else,
    clippy::blocks_in_if_conditions, // too many false positives with rustfmt's output
    clippy::similar_names, // too many false positives (#6479)
    clippy::semicolon_if_nothing_returned, // @Temporary false positives with let/else, still
    clippy::same_functions_in_if_condition, // @Temporary false positives with const generics (#8139)
    clippy::return_self_not_must_use, // @Temporary false positives with must-use-types (#8140)
    clippy::needless_pass_by_value, // @Temporary
    clippy::missing_panics_doc, // @Temporary
)]

use cli::{BuildMode, Command, PassRestriction};
use colored::Colorize;
use lushui::{
    diagnostics::{
        reporter::{BufferedStderrReporter, StderrReporter},
        Code, Diagnostic, Reporter,
    },
    documenter,
    error::{outcome, Result},
    format::{DisplayWith, IOError},
    package::{find_package, BuildQueue, CrateType, PackageManifest, DEFAULT_SOURCE_FOLDER_NAME},
    resolver::{self, PROGRAM_ENTRY_IDENTIFIER},
    span::{SharedSourceMap, SourceMap, Spanned},
    syntax::{lexer, lowerer, parser, CrateName},
    typer, FILE_EXTENSION,
};
use std::{
    default::default,
    time::{Duration, Instant},
};

mod cli;

const VERSION: &str = concat!(
    env!("CARGO_PKG_VERSION"),
    " (",
    env!("GIT_COMMIT_HASH"),
    " ",
    env!("GIT_COMMIT_DATE"),
    ")"
);

fn main() {
    if main_().is_err() {
        // all destructors have been run
        std::process::exit(1);
    }
}

fn main_() -> Result {
    set_panic_hook();

    let (command, options) = cli::arguments();

    let map = SourceMap::shared();
    let reporter = BufferedStderrReporter::new(map.clone()).into();

    let result = execute_command(command, options, &map, &reporter);

    let reporter: BufferedStderrReporter = reporter.try_into().unwrap();
    let number_of_errors_reported = reporter.release_buffer();

    if number_of_errors_reported > 0 || result.is_err() {
        assert!(
            number_of_errors_reported > 0,
            "some errors occurred but none were reported",
        );

        return Err(());
    }

    Ok(())
}

fn execute_command(
    command: Command,
    options: cli::Options,
    map: &SharedSourceMap,
    reporter: &Reporter,
) -> Result {
    use Command::*;

    match command {
        Build {
            mode,
            options: build_options,
        } => build_package(mode, build_options, options, map, reporter),
        Explain => todo!(),
        Generate {
            mode,
            options: generation_options,
        } => match mode {
            cli::GenerationMode::Initialize => todo!(),
            cli::GenerationMode::New { package_name } => {
                generate_package(package_name, generation_options, reporter)
            }
        },
    }
}

/// Check, build or run a given package.
fn build_package(
    mode: cli::BuildMode,
    build_options: cli::BuildOptions,
    options: cli::Options,
    map: &SharedSourceMap,
    reporter: &Reporter,
) -> Result {
    let mut build_queue = BuildQueue::new(map.clone(), reporter);

    let path = build_options
        .path
        .map(|path| {
            path.canonicalize().map_err(|error| {
                Diagnostic::error()
                    // @Question code?
                    .message("the path to the source file or the package folder is invalid")
                    .note(IOError(error, &path).to_string())
                    .report(reporter);
            })
        })
        .transpose()?;

    match path {
        Some(path) if path.is_file() => {
            build_queue.process_single_file_package(
                path,
                build_options.crate_type.unwrap_or(CrateType::Executable),
                build_options.no_core,
            )?;
        }
        path => {
            if build_options.no_core {
                Diagnostic::error()
                    .message("the flag `--no-core` is only available for single-file packages")
                    .help(
                        "to achieve the equivalent for normal packages, make sure `core` is\n\
                         absent from the list of dependencies in the package manifest",
                    )
                    .report(reporter);
            }
            if build_options.crate_type.is_some() {
                // @Task add help explaining the equivalent for normal packages
                Diagnostic::error()
                    .message("the option `--crate-type` is only available for single-file packages")
                    .report(reporter);
            }

            match path {
                Some(path) => build_queue.process_package(&path)?,
                None => {
                    // @Beacon @Task dont unwrap, handle the error cases
                    let path = std::env::current_dir().unwrap();
                    let Some(path) = find_package(&path) else {
                        Diagnostic::error()
                            .message(
                                "neither the current folder nor any of its parents is a package",
                            )
                            .note(format!(
                                "none of the folders contain a package manifest file named `{}`",
                                PackageManifest::FILE_NAME
                            ))
                            .report(reporter);
                        return Err(());
                    };

                    build_queue.process_package(path)?;
                }
            };
        }
    };

    let (mut session, unbuilt_crates) = build_queue.finalize();

    // @Note we don't try to handle duplicate names yet
    // cargo disallows them since its lock-file format is flat
    // npm allows them since it stores transitive dependencies in the folder
    // of the respective dependent crate (in the build folder)
    // it's a tiny edge case but I feel like we should allow a transitive dependency to
    // be named exactly like the goal crate
    // (analogous cases for direct and transitive dependencies follow the same way)
    // since the end user might not have control over those dependencies to patch them
    let crates: Vec<_> = unbuilt_crates
        .values()
        .map(|crate_| crate_.meta.clone())
        .collect();

    for mut crate_ in unbuilt_crates.into_values() {
        // @Task abstract over this as print_status_report and report w/ label="Running" for
        // goal crate if mode==Run (in addition to initial "Building")
        if !options.quiet {
            let label = match mode {
                BuildMode::Check => "Checking",
                BuildMode::Build | BuildMode::Run => "Building",
                // @Bug this should not be printed for non-goal crates and --no-deps
                BuildMode::Document { .. } => "Documenting",
            };
            let label = label.green().bold();
            // @Beacon @Task don't use package path but crate path
            let path = &crate_.package(&session).path.to_string_lossy();
            // @Task print version
            println!(
                "   {label} {} ({path})",
                if crate_.in_goal_package(&session)
                    && crate_.meta.is_ambiguously_named_within_package
                {
                    format!("{} ({})", crate_.meta.name, crate_.meta.type_)
                } else {
                    crate_.meta.name.to_string()
                }
            );
        }

        macro check_pass_restriction($restriction:expr) {
            if crate_.is_goal(&session) && options.pass_restriction == Some($restriction) {
                return Ok(());
            }
        }

        if options.durations {
            eprintln!("Execution times by pass:");
        }

        let source_file = map
            .borrow_mut()
            .load(crate_.meta.path.clone())
            .map_err(|error| {
                // this error case can be reached with crates specified in library or executable manifests
                // @Question any other cases?

                // @Task provide more context for transitive dependencies of the goal package
                Diagnostic::error()
                    .message(format!(
                        "could not load {} crate `{}`",
                        crate_.meta.type_, crate_.meta.name,
                    ))
                    .note(IOError(error, &crate_.meta.path).to_string())
                    .report(reporter);
            })?;

        let time = Instant::now();

        let outcome!(tokens, token_health) = lexer::lex(&map.borrow()[source_file], reporter)?;

        let duration = time.elapsed();

        if crate_.is_goal(&session) && options.emit_tokens {
            for token in &tokens {
                eprintln!("{:?}", token);
            }
        }

        print_pass_duration("Lexing", duration, &options);
        check_pass_restriction!(PassRestriction::Lexer);
        let time = Instant::now();

        // @Beacon @Task fix this ugly mess, create clean helpers
        let module_name = Spanned::new(default(), crate_.meta.name.clone()).into();
        let declaration =
            parser::parse_file(&tokens, source_file, module_name, map.clone(), reporter)?;

        let duration = time.elapsed();

        assert!(token_health.is_untainted()); // parsing succeeded

        if crate_.is_goal(&session) && options.emit_ast {
            eprintln!("{declaration:#?}");
        }

        print_pass_duration("Parsing", duration, &options);
        check_pass_restriction!(PassRestriction::Parser);
        let time = Instant::now();

        let lowering_options = lowerer::Options {
            internal_features_enabled: options.internals || crate_.is_core_library(&session),
            keep_documentation_comments: matches!(mode, BuildMode::Document { .. }),
        };
        let outcome!(mut declarations, health_of_lowerer) =
            lowerer::lower(declaration, lowering_options, map.clone(), reporter);
        let duration = time.elapsed();

        let crate_root = declarations.pop().unwrap();

        if crate_.is_goal(&session) && options.emit_lowered_ast {
            eprintln!("{}", crate_root);
        }

        print_pass_duration("Lowering", duration, &options);
        if health_of_lowerer.is_tainted() {
            return Err(());
        }
        check_pass_restriction!(PassRestriction::Lowerer);
        let time = Instant::now();
        let crate_root =
            resolver::resolve_declarations(crate_root, &mut crate_, &session, reporter)?;
        let duration = time.elapsed();

        if options.emit_hir {
            eprintln!("{}", crate_root.with((&crate_, &session)));
        }
        if crate_.is_goal(&session) && options.emit_untyped_scope {
            eprintln!("{}", crate_.with(&session));
        }

        print_pass_duration("Name resolution", duration, &options);
        check_pass_restriction!(PassRestriction::Resolver);
        let time = Instant::now();
        typer::check(&crate_root, &mut crate_, &mut session, reporter)?;
        let duration = time.elapsed();

        if crate_.is_goal(&session) && options.emit_scope {
            eprintln!("{}", crate_.with(&session));
        }

        print_pass_duration("Type checking & inference", duration, &options);

        // @Task move out of main.rs
        if crate_.is_executable() && crate_.program_entry.is_none() {
            Diagnostic::error()
                .code(Code::E050)
                .message(format!(
                    "the crate `{}` is missing a program entry named `{}`",
                    crate_.meta.name, PROGRAM_ENTRY_IDENTIFIER,
                ))
                .report(reporter);
            return Err(());
        }

        'ending: {
            match &mode {
                BuildMode::Run => {
                    if crate_.is_goal(&session) {
                        if !crate_.is_executable() {
                            // @Question code?
                            Diagnostic::error()
                                .message(format!(
                                    "the package `{}` does not contain any executable to run",
                                    crate_.package(&session).name,
                                ))
                                .report(reporter);
                            return Err(());
                        }

                        let result = typer::interpreter::evaluate_main_function(
                            &crate_, &session, reporter,
                        )?;

                        println!("{}", result.with((&crate_, &session)));
                    }
                }
                BuildMode::Build => {
                    // @Temporary not just builds, also runs ^^

                    lushui::compiler::compile_and_interpret_declaration(&crate_root, &crate_)
                        .unwrap_or_else(|_| panic!());
                }
                BuildMode::Document {
                    options: documentation_options,
                } => {
                    // @Task implement `--open`ing

                    // @Bug leads to broken links, the documenter has to handle this itself
                    if documentation_options.no_dependencies && !crate_.is_goal(&session) {
                        break 'ending;
                    }

                    let time = Instant::now();
                    let documenter_options = documenter::Options {
                        asciidoc: options.asciidoc,
                        lorem_ipsum: options.lorem_ipsum,
                    };
                    documenter::document(
                        &crate_root,
                        documenter_options,
                        &crates,
                        &crate_,
                        &session,
                        reporter,
                    )?;
                    let duration = time.elapsed();

                    print_pass_duration("Documentation", duration, &options);
                }
                BuildMode::Check => {}
            }
        }

        session.add(crate_);
    }

    Ok(())
}

fn print_pass_duration(pass: &str, duration: Duration, options: &cli::Options) {
    const PADDING: usize = 30;

    if options.durations {
        println!("  {pass:<PADDING$}{duration:?}");
    }
}

fn generate_package(
    name: String,
    generation_options: cli::GenerationOptions,
    reporter: &Reporter,
) -> Result {
    use std::fs;

    let name = CrateName::parse(&name).map_err(|error| error.report(reporter))?;

    // @Task handle errors properly
    let current_path = std::env::current_dir().unwrap();
    let package_path = current_path.join(name.as_str());
    fs::create_dir(&package_path).unwrap();
    let source_folder_path = package_path.join(DEFAULT_SOURCE_FOLDER_NAME);
    fs::create_dir(&source_folder_path).unwrap();
    fs::write(
        package_path.join(PackageManifest::FILE_NAME),
        format!(
            "\
name: \"{name}\",
version: \"0.0.0\",
dependencies: {{
    core: {{}},
}},
"
        ),
    )
    .unwrap();
    fs::write(
        package_path.join(".gitignore"),
        "\
build/
",
    )
    .unwrap();

    if generation_options.library {
        let path = source_folder_path
            .join(CrateType::Library.default_root_file_stem())
            .with_extension(FILE_EXTENSION);

        fs::File::create(path).unwrap();
    }

    if generation_options.executable {
        let path = source_folder_path
            .join(CrateType::Executable.default_root_file_stem())
            .with_extension(FILE_EXTENSION);

        let content = "main: extern.core.text.Text =\n    \"Hello there!\"";

        fs::write(path, content).unwrap();
    }

    Ok(())
}

fn set_panic_hook() {
    std::panic::set_hook(Box::new(|information| {
        let payload = information.payload();

        let message = payload
            .downcast_ref::<&str>()
            .copied()
            .or_else(|| payload.downcast_ref::<String>().map(|payload| &payload[..]))
            .unwrap_or("unknown cause")
            .to_owned();

        let backtrace = std::env::var("LUSHUI_BACKTRACE")
            .map_or(false, |variable| variable != "0")
            .then(std::backtrace::Backtrace::force_capture);

        Diagnostic::bug()
            .message(message)
            .if_present(information.location(), |this, location| {
                this.note(format!("at `{location}`"))
            })
            .note(std::thread::current().name().map_or_else(
                || "in an unnamed thread".into(),
                |name| format!("in thread `{name}`"),
            ))
            .note("the compiler unexpectedly panicked. this is a bug. we would appreciate a bug report")
            .note(format!("lushui {VERSION}"))
            .if_(backtrace.is_none(), |this| {
                this.help(
                    "rerun with the environment variable `LUSHUI_BACKTRACE=1` to display a backtrace",
                )
            })
            .if_present(backtrace, |this, backtrace| {
                this.note(format!("with the following backtrace:\n{backtrace}"))
            })
            .report(&StderrReporter::new(None).into());
    }));
}
