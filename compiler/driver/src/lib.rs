#![feature(
    decl_macro,
    default_free_fn,
    let_chains,
    associated_type_bounds,
    type_alias_impl_trait
)]

use ast::Debug;
use cli::{Backend, BuildMode, ColorMode, Command, PassRestriction};
use colored::Colorize;
use diagnostics::{error::Result, reporter::ErasedReportedError, Diagnostic, ErrorCode, Reporter};
use hir_format::Display as _;
use lowered_ast::Display as _;
use package::{find_package, resolve_file, resolve_package, MANIFEST_FILE_NAME};
use resolver::ProgramEntryExt;
use session::{BuildSession, Component, ComponentType, Components};
use span::SourceMap;
use std::{
    borrow::Cow,
    default::default,
    path::Path,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, RwLock,
    },
};
use utilities::{displayed, FormatError};

mod cli;
mod create;

pub fn main() -> Result {
    set_panic_hook();

    let (command, options) = cli::arguments()?;

    match options.color {
        ColorMode::Always => colored::control::set_override(true),
        ColorMode::Never => colored::control::set_override(false),
        ColorMode::Auto => {}
    }

    let map: Arc<RwLock<SourceMap>> = default();
    // @Bug creating a buffered-stderr-reporter up here is not great at all
    // for BuildMode::Serve! there, we do not want to use it! (we want to
    // use the LSP to communicate server errors)
    // @Task smh break this code / construction up / modularize it
    let reported_any_errors: Arc<AtomicBool> = default();
    let reporter = Reporter::buffered_stderr(reported_any_errors.clone()).with_map(map.clone());

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
    use Command::*;

    match command {
        BuildPackage { mode, options } => {
            let (components, session) = match &options.path {
                Some(path) => {
                    // intentionally not `!path.is_dir()` to exclude broken symlinks
                    if path.is_file() {
                        // give a more useful diagnostic than the generic "could not load" one
                        return Err(Diagnostic::error()
                            .message(format!(
                                "the path ‘{}’ does not refer to a folder",
                                path.display()
                            ))
                            .help(
                                "consider running ‘lushui file <SUBCOMMAND> <PATH> [OPTIONS]’ \
                                 (with ‘file’ preceeding the subcommand)\n\
                                 instead to operate on single source files",
                            )
                            .report(&reporter));
                    }

                    resolve_package(path, options.filter, map, reporter)?
                }
                None => {
                    let current_folder_path = match std::env::current_dir() {
                        Ok(path) => path,
                        Err(error) => {
                            // @Task improve message
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
                                "none of the folders contain a package manifest file named ‘{MANIFEST_FILE_NAME}’",
                            ))
                            .report(&reporter));
                    };
                    resolve_package(path, options.filter, map, reporter)?
                }
            };

            build_components(components, &mode, &options.general, global_options, session)
        }
        BuildFile { mode, options } => {
            // intentionally not `!path.is_file()` to exclude broken symlinks
            if options.path.is_dir() {
                // give a more useful diagnostic than the generic "could not load" one
                return Err(Diagnostic::error()
                    .message(format!(
                        "the path ‘{}’ does not refer to a file",
                        options.path.display()
                    ))
                    .help(
                        "consider running ‘lushui <SUBCOMMAND> <PATH> [OPTIONS]’ \
                         (without ‘file’ preceeding the subcommand)\n\
                         instead to operate on packages",
                    )
                    .report(&reporter));
            }

            let (components, session) = resolve_file(
                &options.path,
                None,
                options.component_type.unwrap_or(ComponentType::Executable),
                options.no_core,
                map,
                reporter,
            )?;

            build_components(components, &mode, &options.general, global_options, session)
        }
        #[cfg(feature = "lsp")]
        Serve => {
            tokio::runtime::Builder::new_multi_thread()
                .enable_all()
                .build()
                .unwrap()
                .block_on(server::serve(map.clone()));
            Ok(())
        }
        Explain => Err(Diagnostic::error()
            .message("the subcommand ‘explain’ is not implemented yet")
            .report(&reporter)),
        CreatePackage { mode, options } => match mode {
            cli::PackageCreationMode::Initialize => Err(Diagnostic::error()
                .message("the subcommand ‘initialize’ is not implemented yet")
                .report(&reporter)),
            cli::PackageCreationMode::New { package_name } => {
                create::create_package(package_name, &options, &reporter)
            }
        },
        Metadata { path } => check_metadata_file(&path, map, &reporter),
    }
}

fn build_components(
    components: Components,
    mode: &cli::BuildMode,
    options: &cli::BuildOptions,
    global_options: &cli::GlobalOptions,
    mut session: BuildSession,
) -> Result {
    for mut component in components.into_values() {
        build_component(&mut component, mode, options, global_options, &mut session)?;
        session.add(component);
    }

    if let BuildMode::Document { options } = mode && options.open {
        // @Task smh open in background and immediately disown the child process afterwards
        if let Err(error) = open::that(documenter::index_page(&session)) {
            return Err(Diagnostic::error()
                .message("could not open the generated documentation")
                .note(error.format())
                .report(session.reporter()));
        }
    }

    Ok(())
}

fn build_component(
    component: &mut Component,
    mode: &cli::BuildMode,
    options: &cli::BuildOptions,
    global_options: &cli::GlobalOptions,
    session: &mut BuildSession,
) -> Result {
    // @Task abstract over this as print_status_report and report w/ label="Running" for
    // target component if mode==Run (in addition to initial "Building")
    if !global_options.quiet {
        let label = match mode {
            BuildMode::Check => "Checking",
            BuildMode::Compile { .. } | BuildMode::Run { .. } => "Building",
            // @Bug this should not be printed for non-target components and --no-deps
            BuildMode::Document { .. } => "Documenting",
        };
        let label = label.green().bold();
        println!(
            "   {label} {} ({})",
            if session.in_target_package(component.index()) {
                format!("{}", component.name())
            } else {
                component.name().to_string()
            },
            component.path().bare.display()
        );
    }

    macro restriction_point($restriction:ident) {
        if component.is_target(&session)
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

    let path = component.path();

    // @Task don't unconditionally halt execution on failure here but continue (with tainted health)
    // and mark the component as "erroneous" (not yet implemented) so we can print more errors.
    // "Erroneous" components should not lead to further errors in the name resolver etc.
    let file = session
        .map()
        .load(path.bare.to_owned(), Some(component.index()))
        .map_err(|error| {
            use std::fmt::Write;

            let mut message = format!(
                "could not load the {} component ‘{}’",
                component.type_(),
                component.name(),
            );
            if let Some(package) = session.package_of(component.index()) {
                write!(message, " in package ‘{}’", session[package].name).unwrap();
            }

            // @Task improve message, add label
            Diagnostic::error()
                .message(message)
                .path(path.bare.into())
                .primary_span(path)
                .note(error.format())
                .report(session.reporter())
        })?;

    time! {
        //! Lexing

        let tokens = syntax::lex(file, session);
    };

    if component.is_target(session) && options.emit_tokens {
        for token in &tokens.tokens {
            eprintln!("{token:?}");
        }
    }

    restriction_point! { Lexer }

    time! {
        //! Parsing

        let component_root = syntax::parse_root_module_file(tokens, file, session)?;
    }

    if component.is_target(session) && options.emit_ast {
        eprintln!("{}", displayed(|f| component_root.write(f)));
    }

    restriction_point! { Parser }

    // @Task get rid of this! move into session / component of target package!
    let lowering_options = lowerer::Options {
        internal_features_enabled: options.internals || component.is_core_library(session),
        keep_documentation_comments: matches!(mode, BuildMode::Document { .. }),
    };

    time! {
        //! Lowering

        let component_root =
            lowerer::lower_file(component_root, lowering_options, component, session)?;
    }

    if component.is_target(session) && options.emit_lowered_ast {
        eprintln!("{}", displayed(|f| component_root.write(f)));
    }

    restriction_point! { Lowerer }

    time! {
        //! Name Resolution

        let component_root =
            resolver::resolve_declarations(component_root, component, session)?;
    }

    if options.emit_hir {
        eprintln!(
            "{}",
            displayed(|f| component_root.write((component, session), f))
        );
    }
    if component.is_target(session) && options.emit_untyped_bindings {
        eprintln!("{}", displayed(|f| component.write(session, f)));
    }

    restriction_point! { Resolver }

    time! {
        //! Type Checking and Inference

        typer::check(&component_root, component, session)?;
    }

    if component.is_target(session) && options.emit_bindings {
        eprintln!("{}", displayed(|f| component.write(session, f)));
    }

    // @Task move out of main.rs
    // @Update @Task move into respective backends: LLVM and interpreter!
    if component.is_executable() && component.look_up_program_entry(session).is_none() {
        return Err(Diagnostic::error()
            .code(ErrorCode::E050)
            .message(format!(
                "the component ‘{}’ does not contain a ‘{}’ function in its root module",
                component.name(),
                Component::PROGRAM_ENTRY_IDENTIFIER,
            ))
            .primary_span(&session.shared_map()[file])
            .report(session.reporter()));
    }

    match &mode {
        BuildMode::Run { options } => {
            if component.is_target(session) {
                if !component.is_executable() {
                    // @Question code?
                    // @Note I don't like this code here at all, it's hacky and not principled!
                    // @Question why should the message differ??
                    return Err(Diagnostic::error()
                        .message(match session.package_of(component.index()) {
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
                        let result =
                            typer::interpreter::evaluate_main_function(component, session)?;
                        println!("{}", displayed(|f| result.write((component, session), f)));
                    }
                    #[cfg(feature = "cranelift")]
                    Backend::Cranelift => {
                        // @Task spawn Command where the path is session.build_folder() + ...
                        return Err(Diagnostic::error().message(
                            "running executables built with the Cranelift backend is not yet supported",
                        )
                        .report(session.reporter()));
                    }
                    #[cfg(feature = "llvm")]
                    Backend::Llvm => {
                        // @Task spawn Command where the path is session.build_folder() + ...
                        return Err(Diagnostic::error().message(
                            "running executables built with the LLVM backend is not yet supported",
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
            Backend::Cranelift => codegen_cranelift::compile_and_link(
                codegen_cranelift::Options {
                    emit_clif: options.emit_clif,
                    verify_clif: options.verify_clif,
                },
                &component_root,
                component,
                session,
            )?,
            #[cfg(feature = "llvm")]
            Backend::Llvm => {
                codegen_llvm::compile_and_link(
                    codegen_llvm::Options {
                        emit_llvm_ir: options.emit_llvm_ir,
                        verify_llvm_ir: options.verify_llvm_ir,
                    },
                    &component_root,
                    component,
                    session,
                )?;
            }
        },
        BuildMode::Document { options } => {
            // @Bug leads to broken links, @Task the documenter has to handle this itself
            if options.no_dependencies && !component.is_target(session) {
                return Ok(());
            }

            time! {
                //! Documentation Generation

                documenter::document_component(
                    &component_root,
                    options.general,
                    component,
                    session,
                )?;
            }
        }
        // already done at this point
        BuildMode::Check => {}
    }

    Ok(())
}

fn check_metadata_file(path: &Path, map: &Arc<RwLock<SourceMap>>, reporter: &Reporter) -> Result {
    let file = map
        .write()
        .unwrap()
        .load(path.to_owned(), None)
        .map_err(|error| {
            Diagnostic::error()
                .message("could not load the file")
                .path(path.into())
                .note(error.format())
                .report(reporter)
        })?;

    metadata::parse(file, map, reporter).map(drop)
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
            .with(|error| match information.location() {
                Some(location) => error.note(format!("at ‘{location}’")),
                None => error,
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
            .with(|error| match backtrace {
                Some(backtrace) => error.note(format!("with the following backtrace:\n{backtrace}")),
                None => error.help(
                    "rerun with the environment variable ‘LUSHUI_BACKTRACE=1’ to display a backtrace",
                ),
            })
            .report(&Reporter::stderr());
    }));
}
