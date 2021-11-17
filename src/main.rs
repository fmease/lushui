#![feature(backtrace, derive_default_enum, decl_macro, default_free_fn, let_else)]
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
    clippy::needless_pass_by_value, // @Temporary
    clippy::missing_panics_doc, // @Temporary
    clippy::semicolon_if_nothing_returned, // @Beacon @Temporary false postives with let/else's
)]

use std::{default::default, time::Instant};

use cli::{BuildMode, Command, PassRestriction};
use colored::Colorize;
use lushui::{
    diagnostics::{
        reporter::{BufferedStderrReporter, StderrReporter},
        Code, Diagnostic, Reporter,
    },
    error::{outcome, Result},
    format::{DisplayWith, IOError},
    package::{find_package, BuildQueue, CrateType, PackageManifest, DEFAULT_SOURCE_FOLDER_NAME},
    resolver::{self, PROGRAM_ENTRY_IDENTIFIER},
    span::{SharedSourceMap, SourceMap, Spanned},
    syntax::{CrateName, Lexer, Lowerer, Parser},
    typer::Typer,
    FILE_EXTENSION,
};
use resolver::Resolver;

mod cli;

fn main() {
    if main_().is_err() {
        // all destructors have been run
        std::process::exit(1);
    }
}

fn main_() -> Result {
    set_panic_hook();

    let (command, options) = cli::arguments();

    // @Task get rid of this!
    lushui::set_global_options(lushui::GlobalOptions {
        show_binding_indices: options.show_binding_indices,
    });

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
        Build { mode, suboptions } => build_package(mode, suboptions, options, map, reporter),
        Explain => todo!(),
        Generate { mode, suboptions } => match mode {
            cli::GenerationMode::Initialize => todo!(),
            cli::GenerationMode::New { package_name } => {
                create_new_package(package_name, suboptions, options, reporter)
            }
        },
    }
}

/// Check, build or run a given package.
fn build_package(
    mode: cli::BuildMode,
    suboptions: cli::BuildOptions,
    options: cli::Options,
    map: &SharedSourceMap,
    reporter: &Reporter,
) -> Result {
    let mut build_queue = BuildQueue::new(map.clone(), reporter);

    let path = suboptions
        .path
        .map(|path| {
            path.canonicalize().map_err(|error| {
                Diagnostic::error()
                    // @Question code?
                    .message("the path to the source file or the package folder is invalid")
                    .note(IOError(error, &path).to_string())
                    .report(reporter)
            })
        })
        .transpose()?;

    match path {
        Some(path) if path.is_file() => {
            build_queue.process_single_file_package(
                path,
                options.crate_type.unwrap_or(CrateType::Binary),
                options.no_core,
            )?;
        }
        path => {
            if options.no_core {
                Diagnostic::error()
                    .message("the flag `--no-core` is only available for single-file packages")
                    .help(
                        "to achieve the equivalent for normal packages, make sure `core` is\n\
                         absent from the list of dependencies in the package manifest",
                    )
                    .report(reporter);
            }
            if options.crate_type.is_some() {
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

    let (mut session, unbuilt_crates) = build_queue.into_session_and_unbuilt_crates();

    // @Note not extensible to multiple binary crates
    let goal_crate = unbuilt_crates.last().unwrap().index;

    for mut crate_ in unbuilt_crates.into_values() {
        let is_goal_crate = crate_.index == goal_crate;

        if !options.quiet {
            // @Task write `Checking` if `lushui check`ing
            let label = "Building".green().bold();
            let package = &session[crate_.package];
            let name = &package.name;
            let path = package.path.to_string_lossy();
            // @Task print version
            println!("   {label} {name} ({path})");
        }

        macro check_pass_restriction($restriction:expr) {
            if is_goal_crate && options.pass_restriction == Some($restriction) {
                return Ok(());
            }
        }

        if options.emit.times {
            eprintln!("Execution times by pass:");
        }

        const EXECUTION_TIME_PASS_NAME_PADDING: usize = 30;

        let source_file = map
            .borrow_mut()
            .load(crate_.path.clone())
            .map_err(|error| {
                // this error case can be reached with crates specified in library or binary manifests
                // @Question any other cases?

                // @Task provide more context for transitive dependencies of the goal package
                Diagnostic::error()
                    // @Temporary using the name of the package instead of the actual crate name
                    .message(format!(
                        "could not load {} crate `{}`",
                        crate_.type_, session[crate_.package].name,
                    ))
                    .note(IOError(error, &crate_.path).to_string())
                    .report(reporter)
            })?;

        let time = Instant::now();

        let outcome!(tokens, token_health) =
            Lexer::new(&map.borrow()[source_file], reporter).lex()?;

        let duration = time.elapsed();

        if is_goal_crate && options.emit.tokens {
            for token in &tokens {
                eprintln!("{:?}", token);
            }
        }

        if options.emit.times {
            eprintln!(
                "  {:<EXECUTION_TIME_PASS_NAME_PADDING$}{duration:?}",
                "Lexing"
            );
        }

        check_pass_restriction!(PassRestriction::Lexer);

        let time = Instant::now();

        // @Beacon @Task fix this ugly mess, create clean helpers
        // @Note add one point `Package.name` might become `Spanned<_>` and we
        // could just use the span, that might be very weird, though
        let declaration = Parser::new(source_file, &tokens, map.clone(), reporter)
            .parse(Spanned::new(default(), session[crate_.package].name.clone()).into())?;

        let duration = time.elapsed();

        assert!(token_health.is_untainted()); // parsing succeeded

        if is_goal_crate && options.emit.ast {
            eprintln!("{declaration:#?}");
        }

        if options.emit.times {
            eprintln!(
                "  {:<EXECUTION_TIME_PASS_NAME_PADDING$}{duration:?}",
                "Parsing"
            );
        }

        check_pass_restriction!(PassRestriction::Parser);

        let time = Instant::now();

        let outcome!(mut declarations, health_of_lowerer) =
            Lowerer::new(map.clone(), reporter).lower_declaration(declaration);

        let duration = time.elapsed();

        let declaration = declarations.pop().unwrap();

        if is_goal_crate && options.emit.lowered_ast {
            eprintln!("{}", declaration);
        }

        if options.emit.times {
            eprintln!(
                "  {:<EXECUTION_TIME_PASS_NAME_PADDING$}{duration:?}",
                "Lowering"
            );
        }

        if health_of_lowerer.is_tainted() {
            return Err(());
        }

        check_pass_restriction!(PassRestriction::Lowerer);

        let time = Instant::now();

        let mut resolver = Resolver::new(&mut crate_, &session, reporter);
        let declaration = resolver.resolve_declaration(declaration)?;

        let duration = time.elapsed();

        if options.emit.hir {
            eprintln!("{}", declaration.with((&crate_, &session)));
        }
        if is_goal_crate && options.emit.untyped_scope {
            eprintln!("{}", crate_.with(&session));
        }

        if options.emit.times {
            eprintln!(
                "  {:<EXECUTION_TIME_PASS_NAME_PADDING$}{duration:?}",
                "Name resolution"
            );
        }

        check_pass_restriction!(PassRestriction::Resolver);

        // @Note this condition seems weird but the final result is
        // that we only ever call register_foreign_bindings once
        // (unless we start supporting --extern for single-file packages
        // (which are also --no-core)). ideally, we would not
        // store those "FFI bindings" (more like "intrinstincs")
        // in crate::resolver::scope::Crate but in a separate location! (@Task)
        if options.no_core || crate_.is_core_library(&session) {
            crate_.register_foreign_bindings();
        }

        let time = Instant::now();

        let mut typer = Typer::new(&mut crate_, &session, reporter);
        typer.infer_types_in_declaration(&declaration)?;

        let duration = time.elapsed();

        if is_goal_crate && options.emit.scope {
            eprintln!("{}", typer.crate_.with(&session));
        }

        if options.emit.times {
            eprintln!(
                "  {:<EXECUTION_TIME_PASS_NAME_PADDING$}{duration:?}",
                "Type checking & inference"
            );
        }

        if typer.crate_.type_ == CrateType::Binary && typer.crate_.program_entry.is_none() {
            // @Temporary using the name of the package instead of the actual crate name
            Diagnostic::error()
                .code(Code::E050)
                .message(format!(
                    "the crate `{}` is missing a program entry named `{}`",
                    session[crate_.package].name, PROGRAM_ENTRY_IDENTIFIER,
                ))
                .report(reporter);
            return Err(());
        }

        if let BuildMode::Run = mode {
            if is_goal_crate {
                if typer.crate_.type_ != CrateType::Binary {
                    // @Question code?
                    Diagnostic::error()
                        .message(format!(
                            "the package `{}` does not contain any binary to run",
                            session[crate_.package].name,
                        ))
                        .report(reporter);
                    return Err(());
                }

                let result = typer.interpreter().run()?;

                println!("{}", result.with((&crate_, &session)));
            }
        }
        // @Temporary
        else if let BuildMode::Build = mode {
            // @Temporary not just builds, also runs ^^

            lushui::compiler::compile_and_interpret_declaration(&declaration, &crate_)
                .unwrap_or_else(|_| panic!());
        }

        session.add(crate_);
    }

    Ok(())
}

fn create_new_package(
    name: String,
    suboptions: cli::GenerationOptions,
    _options: cli::Options,
    reporter: &Reporter,
) -> Result {
    // @Task initialize git repository (unless `--vsc=none` (…))

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

    if suboptions.library {
        let path = source_folder_path
            .join(CrateType::Library.default_root_file_stem())
            .with_extension(FILE_EXTENSION);

        fs::File::create(path).unwrap();
    }

    if suboptions.binary {
        let path = source_folder_path
            .join(CrateType::Binary.default_root_file_stem())
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

        let backtrace = std::backtrace::Backtrace::force_capture();

        Diagnostic::bug()
            .message(message)
            .if_present(information.location(), |this, location| {
                this.note(format!("at `{location}`"))
            })
            .note(std::thread::current().name().map_or_else(
                || Str::from("in an unnamed thread"),
                |name| format!("in thread `{name}`").into(),
            ))
            .note(format!("with the following backtrace:\n{backtrace}"))
            .report(&StderrReporter::new(None).into());
    }));
}

type Str = std::borrow::Cow<'static, str>;
