#![feature(backtrace, format_args_capture, derive_default_enum, decl_macro)]
#![forbid(rust_2018_idioms, unused_must_use)]

use std::path::PathBuf;

use cli::{Command, PhaseRestriction};
use lushui::{
    diagnostics::{
        reporter::{BufferedStderrReporter, StderrReporter},
        Diagnostic, Reporter,
    },
    error::{Outcome, Result},
    format::DisplayWith,
    lexer::Lexer,
    lowerer::Lowerer,
    package::{
        distributed_libraries_path, find_package, CrateBuildQueue, CrateType, Package,
        PackageManifest, CORE_PACKAGE_NAME, DEFAULT_SOURCE_FOLDER_NAME,
    },
    parser::{ast::Identifier, Parser},
    resolver::{self, CrateScope},
    span::{SharedSourceMap, SourceMap, Span},
    typer::Typer,
    FILE_EXTENSION,
};
use resolver::Resolver;
use rustc_hash::FxHashMap as HashMap;
use util::Str;

mod cli;
mod util;

fn main() {
    if main_().is_err() {
        // all destructors have been run
        std::process::exit(1);
    }
}

fn main_() -> Result<(), ()> {
    set_panic_hook();

    let (command, options) = cli::arguments();

    // @Task get rid of this!
    lushui::set_global_options(lushui::GlobalOptions {
        show_binding_indices: options.show_binding_indices,
    });

    let map = SourceMap::shared();
    let reporter = BufferedStderrReporter::new(map.clone()).into();

    let result = execute_command(command, options, &map, &reporter);

    let reporter = match reporter {
        Reporter::BufferedStderr(reporter) => reporter,
        _ => unreachable!(),
    };

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
    let mut crates = CrateBuildQueue::default();

    match options.source_file_path {
        Some(source_file_path) => process_single_file_package(
            source_file_path,
            &mut crates,
            options.unlink_core,
            reporter,
        ),
        None => process_package(&mut crates, options.unlink_core, reporter),
    }?;

    let (unbuilt_crates, mut built_crates) = crates.into_unbuilt_and_built();

    // // @Temporary
    // unbuilt_crates.iter().for_each(|crate_| {
    //     eprintln!(
    //         "crate to build: {} ({:?}/{:?}) {} | {:?} || {:?}",
    //         built_crates[crate_.package].name,
    //         crate_.package,
    //         crate_.index,
    //         crate_.type_,
    //         crate_.path,
    //         built_crates[crate_.package].path,
    //     )
    // });

    let goal_crate = unbuilt_crates.last().unwrap().index;

    for mut crate_ in unbuilt_crates {
        // @Beacon @Task print banner "compiling crate xyz" here unless --quiet/-q
        // eprintln!("  [building {}]", built_crates[crate_.package].name);

        let source_file = map
            .borrow_mut()
            .load(crate_.path.clone())
            .map_err(|error| Diagnostic::from(error).report(&reporter))?;

        let Outcome {
            value: tokens,
            health,
        } = Lexer::new(map.borrow().get(source_file), &reporter).lex()?;

        {
            if options.dump.tokens {
                for token in &tokens {
                    eprintln!("{:?}", token);
                }
            }
            if options.phase_restriction == Some(PhaseRestriction::Lexer) {
                if health.is_tainted() {
                    return Err(());
                }
                return Ok(());
            }
        }

        let declaration = Parser::new(source_file, &tokens, map.clone(), &reporter).parse(
            // @Beacon @Note yikes!! we just unwrapped that from a string!
            Identifier::new(built_crates[crate_.package].name.clone().into(), Span::SHAM),
        )?;
        if health.is_tainted() {
            return Err(());
        }
        {
            if options.dump.ast {
                eprintln!("{declaration:#?}");
            }
            if options.phase_restriction == Some(PhaseRestriction::Parser) {
                return Ok(());
            }
        }

        let Outcome {
            value: mut declarations,
            health,
        } = Lowerer::new(map.clone(), &reporter).lower_declaration(declaration);

        if health.is_tainted() {
            return Err(());
        }

        let declaration = declarations.pop().unwrap();

        {
            if options.dump.lowered_ast {
                eprintln!("{}", declaration);
            }
            if options.phase_restriction == Some(PhaseRestriction::Lowerer) {
                return Ok(());
            }
        }

        let mut resolver = Resolver::new(&mut crate_, &built_crates, &reporter);
        let declaration = resolver.resolve_declaration(declaration)?;

        {
            if options.dump.hir {
                eprintln!("{}", declaration.with((&crate_, &built_crates)));
            }
            if options.dump.untyped_scope {
                eprintln!("{}", crate_.with(&built_crates));
            }
            if options.phase_restriction == Some(PhaseRestriction::Resolver) {
                return Ok(());
            }
        }

        // @Beacon @Temporary only in `core`
        crate_.register_foreign_bindings();

        let is_goal_crate = crate_.index == goal_crate;

        let mut typer = Typer::new(&mut crate_, &built_crates, &reporter);
        typer.infer_types_in_declaration(&declaration)?;

        {
            if options.dump.scope {
                eprintln!("{}", typer.scope.with(&built_crates));
            }
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

fn process_package(crates: &mut CrateBuildQueue, unlink_core: bool, reporter: &Reporter) -> Result {
    if unlink_core {
        // @Temporary message
        // @Beacon @Question does clap support this in a built-in way?:
        // @Task add note explaining how one needs to remove the
        // explicit `core` dep in the manifest to achieve the wanted behavior
        Diagnostic::error()
            .message("option --unlink-core only works with explicit source file paths")
            .report(&reporter);
    }

    // @Task dont unwrap, handle the error cases
    let path = std::env::current_dir().unwrap();
    let path = find_package(&path).unwrap();

    // @Task verify name and version matches (unless overwritten!)
    // @Task don't return early here!
    // @Task don't bubble up with `?` but once `open` returns a proper error type,
    // report a custom diagnostic saying ~ "could not find a package manifest for the package XY
    // specified as a path dependency" (sth sim to this)
    let manifest = PackageManifest::from_package_path(path, &reporter)?;

    let package = Package::from_manifest_details(path.to_owned(), manifest.details);
    let package = crates.add_package(package);

    // @Note we probably need to disallow referencing the same package through different
    // names from the same package to be able to generate a correct lock-file
    let resolved_dependencies =
        crates.enqueue_dependencies(path, &manifest.crates.dependencies, reporter)?;

    crates.resolve_library_and_binary_manifests(
        package,
        manifest.crates.library,
        manifest.crates.binary,
        reporter,
    )?;
    crates.add_resolved_dependencies(package, resolved_dependencies);

    Ok(())
}

fn process_single_file_package(
    source_file_path: PathBuf,
    crates: &mut CrateBuildQueue,
    unlink_core: bool,
    reporter: &Reporter,
) -> Result {
    let crate_name = lushui::lexer::parse_crate_name(source_file_path.clone(), &reporter)
        .map_err(|error| error.report(&reporter))?;

    // @Task dont unwrap, handle error case
    // joining with "." since it might return "" which would fail to canonicalize
    let path = source_file_path
        .parent()
        .unwrap()
        .join(".")
        .canonicalize()
        .unwrap();

    // @Note wasteful name cloning
    let package = Package::single_file_package(crate_name.as_str().to_owned(), path);
    let package = crates.add_package(package);

    let mut resolved_dependencies = HashMap::default();

    if !unlink_core {
        let core_path = distributed_libraries_path().join(CORE_PACKAGE_NAME);

        // @Question custom message for not finding the core library?
        let core_manifest = PackageManifest::from_package_path(&core_path, &reporter)?;

        let core_package = Package::from_manifest_details(core_path.clone(), core_manifest.details);
        let core_package = crates.add_package(core_package);

        // @Note we probably need to disallow referencing the same package through different
        // names from the same package to be able to generate a correct lock-fil
        let resolved_transitive_core_dependencies = crates.enqueue_dependencies(
            &core_path,
            &core_manifest.crates.dependencies,
            reporter,
        )?;

        crates.resolve_library_manifest(core_package, core_manifest.crates.library);
        crates.add_resolved_dependencies(core_package, resolved_transitive_core_dependencies);

        resolved_dependencies.insert(
            CORE_PACKAGE_NAME.to_string(),
            crates[core_package].library.unwrap(),
        );
    }

    let binary = crates
        .enqueue(|index| CrateScope::new(index, package, source_file_path, CrateType::Binary));
    crates[package].binaries.push(binary);
    crates.add_resolved_dependencies(package, resolved_dependencies);

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
{{
    name: '{name}',
    version: '0.0.0',
    dependencies: {{
        core: {{}},
    }},
}}"
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
