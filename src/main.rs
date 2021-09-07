#![feature(backtrace, format_args_capture, derive_default_enum, decl_macro)]
#![forbid(rust_2018_idioms, unused_must_use)]

use std::{convert::TryInto, time::Instant};

use cli::{Command, PhaseRestriction};
use colored::Colorize;
use lushui::{
    diagnostics::{
        reporter::{BufferedStderrReporter, StderrReporter},
        Diagnostic, Reporter,
    },
    error::{outcome, ReportedExt, Result},
    format::DisplayWith,
    lexer::Lexer,
    lowerer::Lowerer,
    package::{BuildQueue, CrateType, PackageManifest, DEFAULT_SOURCE_FOLDER_NAME},
    parser::{ast::Identifier, Parser},
    resolver,
    span::{SharedSourceMap, SourceMap, Span},
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
    use cli::GenerationMode;
    use Command::*;

    match command {
        Check | Run | Build => check_run_or_build_package(command, options, map, reporter),
        Explain => todo!(),
        Generate {
            mode,
            options: generation_options,
        } => match mode {
            GenerationMode::Initialize => todo!(),
            GenerationMode::New { package_name } => {
                create_new_package(package_name, generation_options, options, reporter)
            }
        },
    }
}

fn check_run_or_build_package(
    command: Command,
    options: cli::Options,
    map: &SharedSourceMap,
    reporter: &Reporter,
) -> Result {
    let mut build_queue = BuildQueue::new(map.clone(), reporter);

    match options.source_file_path {
        Some(source_file_path) => {
            // @Question before building core, should we check the existence of source_file_path?

            build_queue.process_single_file_package(source_file_path, options.unlink_core)
        }
        None => {
            if options.unlink_core {
                // @Temporary message
                // @Task add note explaining how one needs to remove the
                // explicit `core` dep in the manifest to achieve the wanted behavior
                Diagnostic::error()
                    .message("option --unlink-core only works with explicit source file paths")
                    .report(&reporter);
            }

            // @Task dont unwrap, handle the error cases
            let package_path = std::env::current_dir().unwrap();
            build_queue.process_package(package_path)
        }
    }?;

    let (unbuilt_crates, mut built_crates) = build_queue.into_unbuilt_and_built();

    let goal_crate = unbuilt_crates.last().unwrap().index;

    for mut crate_ in unbuilt_crates.into_values() {
        let is_goal_crate = crate_.index == goal_crate;

        if !options.quiet {
            // @Task write `Checking` if `lushui check`ing
            let label = "Building".green().bold();
            let package = &built_crates[crate_.package];
            let name = &package.name;
            let path = package.path.to_string_lossy();
            // @Task print version
            println!("   {label} {name} ({path})");
        }

        macro check_phase_restriction($restriction:expr $(, $( $action:tt )* )?) {
            if is_goal_crate && options.phase_restriction == Some($restriction) {
                $( $( $action )*; )?
                return Ok(())
            }
        }

        if options.dump.times {
            eprintln!("Execution times by phase:");
        }

        const EXECUTION_TIME_PHASE_NAME_PADDING: usize = 30;

        let source_file = map
            .borrow_mut()
            .load(crate_.path.clone())
            .reported(reporter)?;

        let time = Instant::now();

        let outcome!(tokens, health_of_lexer) =
            Lexer::new(&map.borrow()[source_file], &reporter).lex()?;

        let duration = time.elapsed();

        if options.dump.tokens {
            for token in &tokens {
                eprintln!("{:?}", token);
            }
        }

        if options.dump.times {
            eprintln!(
                "  {:<EXECUTION_TIME_PHASE_NAME_PADDING$}{duration:?}",
                "Lexing"
            );
        }

        check_phase_restriction!(
            PhaseRestriction::Lexer,
            if health_of_lexer.is_tainted() {
                return Err(());
            }
        );

        let time = Instant::now();

        let declaration = Parser::new(source_file, &tokens, map.clone(), &reporter).parse(
            // @Beacon @Note yikes!! we just unwrapped that from a string!
            Identifier::new(built_crates[crate_.package].name.clone().into(), Span::SHAM),
        )?;

        let duration = time.elapsed();

        assert!(health_of_lexer.is_untainted()); // parsing succeeded

        if options.dump.ast {
            eprintln!("{declaration:#?}");
        }

        if options.dump.times {
            eprintln!(
                "  {:<EXECUTION_TIME_PHASE_NAME_PADDING$}{duration:?}",
                "Parsing"
            );
        }

        check_phase_restriction!(PhaseRestriction::Parser);

        let time = Instant::now();

        let outcome!(mut declarations, health_of_lowerer) =
            Lowerer::new(map.clone(), &reporter).lower_declaration(declaration);

        let duration = time.elapsed();

        let declaration = declarations.pop().unwrap();

        if options.dump.lowered_ast {
            eprintln!("{}", declaration);
        }

        if options.dump.times {
            eprintln!(
                "  {:<EXECUTION_TIME_PHASE_NAME_PADDING$}{duration:?}",
                "Lowering"
            );
        }

        if health_of_lowerer.is_tainted() {
            return Err(());
        }

        check_phase_restriction!(PhaseRestriction::Lowerer);

        let time = Instant::now();

        let mut resolver = Resolver::new(&mut crate_, &built_crates, &reporter);
        let declaration = resolver.resolve_declaration(declaration)?;

        let duration = time.elapsed();

        if options.dump.hir {
            eprintln!("{}", declaration.with((&crate_, &built_crates)));
        }
        if options.dump.untyped_scope {
            eprintln!("{}", crate_.with(&built_crates));
        }

        if options.dump.times {
            eprintln!(
                "  {:<EXECUTION_TIME_PHASE_NAME_PADDING$}{duration:?}",
                "Name resolution"
            );
        }

        check_phase_restriction!(PhaseRestriction::Resolver);

        // @Beacon @Temporary only in `core`
        crate_.register_foreign_bindings();

        let time = Instant::now();

        let mut typer = Typer::new(&mut crate_, &built_crates, &reporter);
        typer.infer_types_in_declaration(&declaration)?;

        let duration = time.elapsed();

        if options.dump.scope {
            eprintln!("{}", typer.scope.with(&built_crates));
        }

        if options.dump.times {
            eprintln!(
                "  {:<EXECUTION_TIME_PHASE_NAME_PADDING$}{duration:?}",
                "Type checking & inference"
            );
        }

        // @Beacon @Task dont check for program_entry in scope.run() or compile_and_interp
        // but here (a static error) (if the CLI dictates to run it)

        if let Command::Run = command {
            if is_goal_crate {
                let result = typer.interpreter().run()?;

                println!("{}", result.with((&crate_, &built_crates)));
            }
        }
        // @Temporary
        else if let Command::Build = command {
            // @Temporary not just builds, also runs ^^

            lushui::compiler::compile_and_interpret_declaration(&declaration, &crate_)
                .unwrap_or_else(|_| panic!());
        }

        built_crates.add(crate_);
    }

    Ok(())
}

// @Task initialize git repository (unless `--vsc=none` or similar)
fn create_new_package(
    name: String,
    generation_options: cli::GenerationOptions,
    _options: cli::Options,
    _reporter: &Reporter,
) -> Result {
    use std::fs;

    // @Task verify name is a valid crate name

    // @Task handle errors properly
    let current_path = std::env::current_dir().unwrap();
    let package_path = current_path.join(&name);
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

    if generation_options.library {
        let path = source_folder_path
            .join(CrateType::Library.default_root_file_stem())
            .with_extension(FILE_EXTENSION);

        fs::File::create(path).unwrap();
    }

    if generation_options.binary {
        let path = source_folder_path
            .join(CrateType::Binary.default_root_file_stem())
            .with_extension(FILE_EXTENSION);

        let content = "main: crates.core.text.Text =\n    \"hello there!\"";

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
            .unwrap_or_else(|| "unknown cause")
            .to_owned();

        let backtrace = std::backtrace::Backtrace::force_capture();

        Diagnostic::bug()
            .message(message)
            .when_present(information.location(), |this, location| {
                this.note(format!("at `{location}`"))
            })
            .note(
                std::thread::current()
                    .name()
                    .map(|name| format!("in thread `{name}`").into())
                    .unwrap_or(Str::from("in an unnamed thread")),
            )
            .note(format!("with the following backtrace:\n{backtrace}"))
            .report(&StderrReporter::new(None).into());
    }));
}

type Str = std::borrow::Cow<'static, str>;
