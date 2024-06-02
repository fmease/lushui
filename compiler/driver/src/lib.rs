#![feature(decl_macro, let_chains, impl_trait_in_assoc_type, never_type)]

use ast::Render;
use cli::{Backend, BuildMode, Command, PassRestriction};
use diagnostics::{error::Result, reporter::ErasedReportedError, Diagnostic, ErrorCode, Reporter};
use hir_format::Display as _;
use lo_ast::Display as _;
use package::{find_package, resolve_file, resolve_package};
use resolver::ProgramEntryExt;
use session::{package::ManifestPath, unit::ComponentType, Context, Session};
use span::SourceMap;
use std::{
    borrow::Cow,
    io::Write,
    path::Path,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, RwLock,
    },
};
use utility::{
    default, displayed,
    paint::{epaint, paint, AnsiColor, ColorChoice},
    FormatError, PROGRAM_ENTRY,
};

mod cli;
mod create;

pub fn main() -> Result {
    set_panic_hook();

    let (command, options) = cli::arguments()?;

    let map: Arc<RwLock<SourceMap>> = default();
    // @Bug creating a buffered-stderr-reporter up here is not great at all
    // for BuildMode::Serve! there, we do not want to use it! (we want to
    // use the LSP to communicate server errors)
    // @Task smh break this code / construction up / modularize it
    let reported_any_errors: Arc<AtomicBool> = default();
    let reporter =
        Reporter::buffered_stderr(options.color, reported_any_errors.clone()).with_map(map.clone());

    let result = execute_command(command, &options, &map, reporter);

    let reported_any_errors = reported_any_errors.load(Ordering::SeqCst);

    if let Err(error) = result {
        assert!(
            reported_any_errors,
            "an error occurred but nothing was reported"
        );
        return Err(error);
    }

    // @Task get rid of this once everyone complies
    if reported_any_errors {
        return Err(ErasedReportedError::new_unchecked());
    }

    Ok(())
}

fn execute_command(
    command: Command,
    global_options: &cli::GlobalOptions,
    map: &Arc<RwLock<SourceMap>>,
    reporter: Reporter,
) -> Result {
    match command {
        Command::BuildPackage { mode, options } => {
            let mut context = match &options.path {
                Some(path) => {
                    // Intentionally not `!path.is_dir()` to exclude broken symlinks.
                    if path.is_file() {
                        // Give a more useful diagnostic than the generic "could not load" one.
                        return Err(Diagnostic::error()
                            .path(path.clone())
                            .message("the path does not refer to a folder")
                            .help(format!(
                                "consider running ‘lushui file {} {} [OPTIONS]’ instead \
                                 (where ‘file’ precedes the subcommand)\n\
                                 to operate on single source files",
                                mode.name(),
                                path.display(),
                            ))
                            .report(&reporter));
                    }

                    resolve_package(path, &options.filter, map, reporter)?
                }
                None => {
                    let current_folder_path = match std::env::current_dir() {
                        Ok(path) => path,
                        Err(error) => {
                            // FIXME: Improve this diagnostic.
                            return Err(Diagnostic::error()
                                .message("could not read the current folder")
                                .note(error.format())
                                .report(&reporter));
                        }
                    };

                    let Some(path) = find_package(&current_folder_path) else {
                        return Err(Diagnostic::error()
                            .message(
                                "neither the current folder nor any of its parents is a package",
                            )
                            .note(format!(
                                "none of the folders contain a package manifest file named ‘{}’",
                                ManifestPath::FILE_NAME,
                            ))
                            .report(&reporter));
                    };
                    resolve_package(path, &options.filter, map, reporter)?
                }
            };

            build_components(&mode, &options.general, global_options, &mut context)
        }
        Command::BuildFile { mode, options } => {
            // Intentionally not `!path.is_file()` to exclude broken symlinks.
            if options.path.is_dir() {
                // Give a more useful diagnostic than the generic “could not load” one.
                return Err(Diagnostic::error()
                    .path(options.path.clone())
                    .message("the path does not refer to a file")
                    .help(format!(
                        "consider running ‘lushui {} {} [OPTIONS]’ instead \
                        (where ‘file’ does not precede the subcommand)\n\
                        to operate on packages",
                        mode.name(),
                        options.path.display(),
                    ))
                    .report(&reporter));
            }

            let mut context = resolve_file(
                &options.path,
                None,
                options.component_type.unwrap_or(ComponentType::Executable),
                options.no_core,
                map,
                reporter,
            )?;

            build_components(&mode, &options.general, global_options, &mut context)
        }
        // FIXME: Support package paths.
        // FIXME: Add filter options
        // FIXME: Use `https://lib.rs/crates/termtree`
        Command::Tree => {
            let current_folder_path = match std::env::current_dir() {
                Ok(path) => path,
                Err(error) => {
                    // FIXME: Improve this diagnostic.
                    return Err(Diagnostic::error()
                        .message("could not read the current folder")
                        .note(error.format())
                        .report(&reporter));
                }
            };

            let Some(path) = find_package(&current_folder_path) else {
                return Err(Diagnostic::error()
                    .message("neither the current folder nor any of its parents is a package")
                    .note(format!(
                        "none of the folders contain a package manifest file named ‘{}’",
                        ManifestPath::FILE_NAME,
                    ))
                    .report(&reporter));
            };
            let _context = resolve_package(path, &default(), map, reporter)?;

            // let index = context.root_component().index;
            // let unit = &units[index];
            // eprintln!(
            //     "index={:?}  name={:?}  path={:?}",
            //     index, unit.name, unit.path
            // );

            // for dependency in &unit.dependencies {
            //     eprintln!(">> {dependency:?}");
            // }

            Ok(())
        }
        #[cfg(feature = "lsp")]
        Command::Serve => {
            tokio::runtime::Builder::new_multi_thread()
                .enable_all()
                .build()
                .unwrap()
                .block_on(server::serve(map.clone()));
            Ok(())
        }
        Command::Explain => Err(Diagnostic::error()
            .message("the subcommand ‘explain’ is not implemented yet")
            .report(&reporter)),
        Command::CreatePackage { mode, options } => match mode {
            cli::PackageCreationMode::Initialize => Err(Diagnostic::error()
                .message("the subcommand ‘initialize’ is not implemented yet")
                .report(&reporter)),
            cli::PackageCreationMode::New { package_name } => {
                create::create_package(package_name, &options, &reporter)
            }
        },
        Command::Recnot { path } => check_recnot_file(&path, map, &reporter),
    }
}

fn build_components(
    mode: &cli::BuildMode,
    options: &cli::BuildOptions,
    global_options: &cli::GlobalOptions,
    context: &mut Context,
) -> Result {
    for component in context.components().keys() {
        let mut session = Session::new(component, context);
        build_component(mode, options, global_options, &mut session)?;
    }

    if let BuildMode::Document { options } = mode
        && options.open
    {
        // @Task smh open in background and immediately disown the child process afterwards
        if let Err(error) = open::that(documenter::index_page(context)) {
            return Err(Diagnostic::error()
                .message("could not open the generated documentation")
                .note(error.format())
                .report(context.reporter()));
        }
    }

    Ok(())
}

#[allow(clippy::needless_pass_by_value)] // by design
fn build_component(
    mode: &cli::BuildMode,
    options: &cli::BuildOptions,
    global_options: &cli::GlobalOptions,
    session: &mut Session<'_>,
) -> Result {
    // @Task abstract over this as print_status_report and report w/ label="Running" for
    // root component if mode==Run (in addition to initial "Building")
    if !global_options.quiet {
        let label = match mode {
            BuildMode::Check => "Checking",
            BuildMode::Compile { .. } | BuildMode::Run { .. } => "Building",
            // FIXME: This shouldn't be printed for non-root components and `--no-deps`.
            BuildMode::Document { .. } => "Documenting",
        };

        paint(
            |stdout| {
                stdout.set(AnsiColor::Green.on_default().bold())?;
                write!(stdout, "   {label} ")?;
                stdout.unset()?;

                writeln!(
                    stdout,
                    "{} ({})",
                    session.component().name,
                    session.component().path.bare.display()
                )
            },
            global_options.color,
        )
        .unwrap();
    }

    macro restriction_point($restriction:ident) {
        if session.is_root_component()
            && options.pass_restriction == Some(PassRestriction::$restriction)
        {
            return Ok(());
        }
    }

    macro time(#![doc = $name:literal] $( $block:tt )+) {
        let time = std::time::Instant::now();
        $( $block )+
        let duration = time.elapsed();

        if options.timing {
            println!("  {:<30}{duration:?}", $name.trim_start());
        }
    }

    if options.timing {
        eprintln!("Execution times by pass:");
    }

    let path = session.component().path.as_deref();

    // @Task don't unconditionally halt execution on failure here but continue (with tainted health)
    // and mark the component as "erroneous" (not yet implemented) so we can print more errors.
    // "Erroneous" components should not lead to further errors in the name resolver etc.
    let file = session
        .map()
        .load(path.bare, Some(session.component().index))
        .map_err(|error| {
            use std::fmt::Write;

            let mut message = format!(
                "could not load the {} component ‘{}’",
                session.component().type_,
                session.component().name,
            );
            if let Some(package) = session.package() {
                write!(message, " in package ‘{}’", session[package].name).unwrap();
            }

            // @Task improve message, add label
            Diagnostic::error()
                .message(message)
                .path(path.bare.into())
                .unlabeled_span(path)
                .note(error.format())
                .report(session.reporter())
        })?;

    time! {
        //! Lexing

        let tokens = syntax::lex(file, session);
    };

    if session.is_root_component() && options.emit_tokens {
        for token in &tokens.tokens {
            eprintln!("{token:?}");
        }
    }

    restriction_point! { Lexer }

    time! {
        //! Parsing

        let component_root = syntax::parse_root_module_file(tokens, file, session)?;
    }

    if session.is_root_component() && options.emit_ast {
        epaint(
            |painter| component_root.render(default(), painter),
            global_options.color,
        )
        .unwrap();
        eprintln!();
    }

    restriction_point! { Parser }

    // @Task get rid of this! move into session / component of root package!
    let lowering_options = lowerer::Options {
        internal_features_enabled: options.internals || session.is_core_library(),
        keep_documentation_comments: matches!(mode, BuildMode::Document { .. }),
    };

    time! {
        //! Lowering

        let component_root =
            lowerer::lower_file(component_root, lowering_options, session)?;
    }

    if session.is_root_component() && options.emit_lo_ast {
        eprintln!("{}", displayed(|f| component_root.write(f)));
    }

    restriction_point! { Lowerer }

    time! {
        //! Name Resolution

        let component_root =
            resolver::resolve_declarations(component_root, session)?;
    }

    if session.is_root_component() && options.emit_hir {
        eprintln!("{}", displayed(|f| component_root.write(session, f)));
    }
    if session.is_root_component() && options.emit_untyped_bindings {
        eprintln!("{}", displayed(|f| session.component().write(session, f)));
    }

    restriction_point! { Resolver }

    time! {
        //! Type Checking and Inference

        typer::check(&component_root, session)?;
    }

    if session.is_root_component() && options.emit_bindings {
        eprintln!("{}", displayed(|f| session.component().write(session, f)));
    }

    // @Task move out of main.rs
    // @Update @Task move into respective backends: LLVM and interpreter!
    if session.component().type_ == ComponentType::Executable
        && session.look_up_program_entry().is_none()
    {
        return Err(Diagnostic::error()
            .code(ErrorCode::E050)
            .message(format!(
                "the component ‘{}’ does not contain a ‘{PROGRAM_ENTRY}’ function in its root module",
                session.component().name
            ))
            .unlabeled_span(&session.shared_map()[file])
            .report(session.reporter()));
    }

    match &mode {
        BuildMode::Run { options } => {
            if session.is_root_component() {
                if session.component().type_ != ComponentType::Executable {
                    // @Question code?
                    // @Note I don't like this code here at all, it's hacky and not principled!
                    // @Question why should the message differ??
                    return Err(Diagnostic::error()
                        .message(match session.package() {
                            Some(package) => format!(
                                "the package ‘{}’ does not contain any executable to run",
                                session[package].name,
                            ),
                            None => "the component is not an executable".into(),
                        })
                        .report(session.reporter()));
                }

                match options.backend {
                    Backend::Hiri => {
                        let result = typer::interpreter::evaluate_main_function(session)?;
                        println!("{}", displayed(|f| result.write(session, f)));
                    }
                    #[cfg(feature = "cranelift")]
                    Backend::Cranelift => {
                        // @Task spawn Command where the path is session.build_folder() + ...
                        return Err(Diagnostic::error().message(
                            "running executables built with the Cranelift backend is not supported yet",
                        )
                        .report(session.reporter()));
                    }
                    #[cfg(feature = "llvm")]
                    Backend::Llvm => {
                        // @Task spawn Command where the path is session.build_folder() + ...
                        return Err(Diagnostic::error().message(
                            "running executables built with the LLVM backend is not supported yet",
                        )
                        .report(session.reporter()));
                    }
                }
            }
        }
        BuildMode::Compile { options } => match options.backend {
            Backend::Hiri => {
                // @Task smh print this earlier than the status info (“Building”)
                return Err(Diagnostic::error()
                    .message("HIRI does not support compilation")
                    .report(session.reporter()));
            }
            #[cfg(feature = "cranelift")]
            Backend::Cranelift => {
                if !session.is_root_component() {
                    return Err(Diagnostic::error()
                        .message("extern components cannot be built yet with the Cranelift backend")
                        .report(session.reporter()));
                }

                codegen_cranelift::compile_and_link(
                    codegen_cranelift::Options {
                        emit_clif: options.emit_clif,
                        verify_clif: options.verify_clif,
                    },
                    &component_root,
                    session,
                )?;
            }
            #[cfg(feature = "llvm")]
            Backend::Llvm => {
                if !session.is_root_component() {
                    return Err(Diagnostic::error()
                        .message("extern components cannot be built yet with the LLVM backend")
                        .report(session.reporter()));
                }

                codegen_llvm::compile_and_link(
                    codegen_llvm::Options {
                        emit_llvm_ir: options.emit_llvm_ir,
                        verify_llvm_ir: options.verify_llvm_ir,
                    },
                    &component_root,
                    session,
                )?;
            }
        },
        BuildMode::Document { options } => {
            // @Bug leads to broken links, @Task the documenter has to handle this itself
            if options.no_dependencies && !session.is_root_component() {
                return Ok(());
            }

            time! {
                //! Documentation Generation

                documenter::document_component(
                    &component_root,
                    options.general,
                    session,
                )?;
            }
        }
        // already done at this point
        BuildMode::Check => {}
    }

    Ok(())
}

fn check_recnot_file(path: &Path, map: &Arc<RwLock<SourceMap>>, reporter: &Reporter) -> Result {
    let file = map.write().unwrap().load(path, None).map_err(|error| {
        Diagnostic::error()
            .message("could not load the file")
            .path(path.into())
            .note(error.format())
            .report(reporter)
    })?;

    recnot::parse(file, map, reporter).map(drop)
}

fn set_panic_hook() {
    std::panic::set_hook(Box::new(|information| {
        let payload = information.payload();

        let message = payload
            .downcast_ref::<&str>()
            .copied()
            .or_else(|| payload.downcast_ref::<String>().map(String::as_str))
            .unwrap_or("unknown cause")
            .to_owned();

        let backtrace = std::env::var("LUSHUI_BACKTRACE")
            .map_or(false, |variable| variable != "0")
            .then(std::backtrace::Backtrace::force_capture);

        Diagnostic::bug()
            .message(message)
            .with(|it| match information.location() {
                Some(location) => it.note(format!("at ‘{location}’")),
                None => it,
            })
            .note(std::thread::current().name().map_or_else(
                || Cow::from("in an unnamed thread"),
                |name| format!("in thread ‘{name}’").into(),
            ))
            // @Task the last two sentences should be shown for all `Diagnostic::bug()`s
            .note("the compiler unexpectedly panicked. this is a bug. we would appreciate a bug report")
            .note(format!(
                "lushui {} ({} {})",
                env!("VERSION"),
                env!("SHORT_COMMIT_HASH"),
                env!("COMMIT_DATE")
            ))
            .with(|it| match backtrace {
                Some(backtrace) => it.note(format!("with the following backtrace:\n{backtrace}")),
                None => it.help(
                    "rerun with the environment variable ‘LUSHUI_BACKTRACE=1’ to display a backtrace",
                ),
            })
            // FIXME: respect `--color`
            .report(&Reporter::stderr(ColorChoice::Auto));
    }));
}
