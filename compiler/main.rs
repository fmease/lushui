#![feature(
    backtrace,
    decl_macro,
    default_free_fn,
    let_else,
    associated_type_bounds,
    let_chains,
    type_alias_impl_trait
)]
#![forbid(rust_2018_idioms, unused_must_use)]
#![warn(clippy::pedantic)]
#![allow(
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
    clippy::similar_names,
    clippy::blocks_in_if_conditions, // too many false positives with rustfmt's output
)]

use cli::{BuildMode, Command, PassRestriction};
use colored::Colorize;
use lushui::{
    component::{Component, ComponentOutline, ComponentType, Components},
    diagnostics::{reporter::ErasedReportedError, Diagnostic, ErrorCode, Reporter},
    documenter,
    error::Result,
    package::{find_package, resolve_file, resolve_package, MANIFEST_FILE_NAME},
    resolver,
    session::BuildSession,
    span::SourceMap,
    syntax::{lexer, lowerer, parser, Word},
    typer,
    utility::{DisplayWith, IOError},
    FILE_EXTENSION,
};
use std::{
    default::default,
    io,
    path::Path,
    process::ExitCode,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, RwLock,
    },
};

mod cli;

const VERSION: &str = concat!(env!("CARGO_PKG_VERSION"), " (", env!("GIT_DATA"), ")");

fn main() -> ExitCode {
    match try_main() {
        Ok(_) => ExitCode::SUCCESS,
        Err(_) => ExitCode::FAILURE,
    }
}

fn try_main() -> Result<()> {
    set_panic_hook();

    let (command, options) = cli::arguments()?;

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
                                path.to_string_lossy()
                            ))
                            .help(
                                "consider running ‘lushui file <SUBCOMMAND> <PATH> [OPTIONS]’ \
                                 (with ‘file’ preceeding the subcommand)\n\
                                 instead to operate on single source files",
                            )
                            .report(&reporter));
                    }

                    resolve_package(path, map, reporter)?
                }
                None => {
                    let current_folder_path = match std::env::current_dir() {
                        Ok(path) => path,
                        Err(error) => {
                            // @Task improve message
                            // @Task more principled io::Error handling please
                            return Err(Diagnostic::error()
                                .message("could not read the current folder")
                                .note(error.to_string())
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
                    resolve_package(path, map, reporter)?
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
                        options.path.to_string_lossy()
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
        Serve => {
            tokio::runtime::Builder::new_multi_thread()
                .enable_all()
                .build()
                .unwrap()
                .block_on(lushui::server::serve(map.clone()));
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
                create_package(&package_name, options, &reporter)
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
    // @Note we don't try to handle duplicate names yet
    // cargo disallows them since its lock-file format is flat
    // npm allows them since it stores transitive dependencies in the folder
    // of the respective dependent component (in the build folder)
    // it's a tiny edge case but I feel like we should allow a transitive dependency to
    // be named exactly like the goal component
    // (analogous cases for direct and transitive dependencies follow the same way)
    // since the end user might not have control over those dependencies to patch them
    //
    // only used in the documenter
    // @Task smh get rid of this (move this into session?)
    let component_outline: Vec<_> = components.values().map(Component::outline).collect();

    for mut component in components.into_values() {
        build_component(
            &mut component,
            &component_outline,
            mode,
            options,
            global_options,
            &mut session,
        )?;
        session.add(component);
    }

    Ok(())
}

fn build_component(
    component: &mut Component,
    component_outlines: &[ComponentOutline],
    mode: &cli::BuildMode,
    options: &cli::BuildOptions,
    global_options: &cli::GlobalOptions,
    session: &mut BuildSession,
) -> Result {
    // @Task abstract over this as print_status_report and report w/ label="Running" for
    // goal component if mode==Run (in addition to initial "Building")
    if !global_options.quiet {
        let label = match mode {
            BuildMode::Check => "Checking",
            BuildMode::Build | BuildMode::Run => "Building",
            // @Bug this should not be printed for non-goal components and --no-deps
            BuildMode::Document { .. } => "Documenting",
        };
        let label = label.green().bold();
        let path = &component.path().bare.to_string_lossy();
        println!(
            "   {label} {} ({path})",
            if session.in_goal_package(component.index()) {
                format!("{} [{}]", component.name(), component.type_())
            } else {
                component.name().to_string()
            }
        );
    }

    macro restriction_point($restriction:ident) {
        if component.is_goal(&session)
            && options.pass_restriction == Some(PassRestriction::$restriction)
        {
            return Ok(());
        }
    }

    macro time(#![name = $name:literal] $( $block:tt )+) {
        let time = std::time::Instant::now();
        $( $block )+
        let duration = time.elapsed();

        if options.timing {
            println!("  {:<30}{duration:?}", $name);
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
                .primary_span(path)
                .note(IOError(error, path.bare).to_string())
                .report(session.reporter())
        })?;

    time! {
        #![name = "Lexing"]
        let tokens = lexer::lex(file, session)?.value;
    };

    if component.is_goal(session) && options.emit_tokens {
        for token in &tokens {
            eprintln!("{token:?}");
        }
    }

    restriction_point! { Lexer }

    time! {
        #![name = "Parsing"]
        let component_root = parser::parse_root_module_file(&tokens, file, session)?;
    }

    if component.is_goal(session) && options.emit_ast {
        eprintln!("{component_root:#?}");
    }

    restriction_point! { Parser }

    // @Task get rid of this! move into session / component of target package!
    let lowering_options = lowerer::Options {
        internal_features_enabled: options.internals || component.is_core_library(session),
        keep_documentation_comments: matches!(mode, BuildMode::Document { .. }),
    };

    time! {
        #![name = "Lowering"]
        let component_root =
            lowerer::lower_file(component_root, lowering_options, component, session)?;
    }

    if component.is_goal(session) && options.emit_lowered_ast {
        eprintln!("{component_root}");
    }

    restriction_point! { Lowerer }

    time! {
        #![name = "Name Resolution"]
        let component_root =
            resolver::resolve_declarations(component_root, component, session)?;
    }

    if options.emit_hir {
        eprintln!("{}", component_root.with((component, session)));
    }
    if component.is_goal(session) && options.emit_untyped_bindings {
        eprintln!("{}", component.with(session));
    }

    restriction_point! { Resolver }

    time! {
        #![name = "Type Checking and Inference"]
        typer::check(&component_root, component, session)?;
    }

    if component.is_goal(session) && options.emit_bindings {
        eprintln!("{}", component.with(session));
    }

    // @Task move out of main.rs
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
        BuildMode::Run => {
            if component.is_goal(session) {
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

                let result = typer::interpreter::evaluate_main_function(component, session)?;

                println!("{}", result.with((component, session)));
            }
        }
        BuildMode::Build => todo!(),
        BuildMode::Document {
            options: documentation_options,
        } => {
            // @Bug leads to broken links, @Task the documenter has to handle this itself
            if documentation_options.no_dependencies && !component.is_goal(session) {
                return Ok(());
            }

            let documenter_options = documenter::Options {
                asciidoc: documentation_options.asciidoc,
                lorem_ipsum: documentation_options.lorem_ipsum,
            };

            time! {
                #![name = "Documentation Generation"]
                let index_page_path = documenter::document(
                    &component_root,
                    documenter_options,
                    component_outlines,
                    component,
                    session,
                )?;
            }

            if documentation_options.open {
                if let Err(error) = open::that(&index_page_path) {
                    // Not sure if this should be a user error or an internal compiler error.
                    // There is no way of knowing the cause of the I/O error.
                    // Is it more likely that the user misconfigured their browser setup or
                    // that the index page is erroneously missing or in some other way faulty?
                    // I have no idea.
                    return Err(Diagnostic::error()
                        .message("could not open the generated documentation")
                        // @Task don't use io::Error::to_string directly but custom printing code (cf. `IOError`)
                        .note(error.to_string())
                        .report(session.reporter()));
                }
            }
        }
        // already done at this point
        BuildMode::Check => {}
    }

    Ok(())
}

const SOURCE_FOLDER_NAME: &str = "source";
const LIBRARY_FILE_STEM: &str = "library";
const EXECUTABLE_FILE_STEM: &str = "main";

fn create_package(name: &str, options: cli::PackageCreationOptions, reporter: &Reporter) -> Result {
    use std::fs;

    let name = Word::parse(name.to_owned()).map_err(|_| {
        // @Task DRY @Question is the common code justified?
        Diagnostic::error()
            .code(ErrorCode::E036)
            .message(format!("the package name ‘{name}’ is not a valid word"))
            .report(reporter)
    })?;

    // @Task handle errors properly
    let current_path = std::env::current_dir().unwrap();
    let package_path = current_path.join(name.as_str());
    fs::create_dir(&package_path).unwrap();
    let source_folder_path = package_path.join(SOURCE_FOLDER_NAME);
    fs::create_dir(&source_folder_path).unwrap();
    {
        let package_manifest =
            io::BufWriter::new(fs::File::create(package_path.join(MANIFEST_FILE_NAME)).unwrap());

        create_package_manifest(&name, options, package_manifest).unwrap();
    }
    fs::write(package_path.join(".gitignore"), "build/\n").unwrap();

    if options.library {
        let path = source_folder_path
            .join(LIBRARY_FILE_STEM)
            .with_extension(FILE_EXTENSION);

        fs::File::create(path).unwrap();
    }

    if options.executable {
        let path = source_folder_path
            .join(EXECUTABLE_FILE_STEM)
            .with_extension(FILE_EXTENSION);

        let content = "main: extern.core.text.Text =\n    \"Hello there!\"";

        fs::write(path, content).unwrap();
    }

    Ok(())
}

fn create_package_manifest(
    name: &Word,
    options: cli::PackageCreationOptions,
    mut sink: impl io::Write,
) -> io::Result<()> {
    {
        write!(sink, "name: ")?;

        if name.as_str() == "false" || name.as_str() == "true" {
            write!(sink, r#""{name}""#)?;
        } else {
            write!(sink, "{name}")?;
        }

        writeln!(sink, ",")?;
    }

    writeln!(sink, r#"version: "0.0.0","#)?;
    writeln!(sink)?;

    writeln!(sink, "components: [")?;

    if options.library {
        writeln!(sink, "    {{")?;
        writeln!(sink, "        type: library,")?;
        writeln!(
            sink,
            r#"        path: "{SOURCE_FOLDER_NAME}/{LIBRARY_FILE_STEM}.lushui","#
        )?;
        writeln!(sink)?;
        writeln!(sink, "        dependencies: {{")?;
        if !options.no_core {
            writeln!(sink, "            core: {{ provider: distribution }},")?;
        }
        writeln!(sink, "        }},")?;
        writeln!(sink, "    }},")?;
    }

    if options.executable {
        writeln!(sink, "    {{")?;
        writeln!(sink, "        type: executable,")?;
        writeln!(
            sink,
            r#"        path: "{SOURCE_FOLDER_NAME}/{EXECUTABLE_FILE_STEM}.lushui","#
        )?;
        writeln!(sink)?;
        writeln!(sink, "        dependencies: {{")?;
        if options.library {
            writeln!(sink, "            {name}: {{}},")?;
        }
        if !options.no_core {
            writeln!(sink, "            core: {{ provider: distribution }},")?;
        }
        writeln!(sink, "        }},")?;
        writeln!(sink, "    }},")?;
    }

    writeln!(sink, "],")
}

fn check_metadata_file(path: &Path, map: &Arc<RwLock<SourceMap>>, reporter: &Reporter) -> Result {
    let file = map
        .write()
        .unwrap()
        .load(path.to_owned(), None)
        .map_err(|error| {
            Diagnostic::error()
                .message("could not load the file")
                .note(IOError(error, path).to_string())
                .report(reporter)
        })?;

    lushui::metadata::parse(file, map, reporter).map(drop)
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
            .with(|error| match information.location() {
                Some(location) => error.note(format!("at ‘{location}’")),
                None => error,
            })
            .note(std::thread::current().name().map_or_else(
                || "in an unnamed thread".into(),
                |name| format!("in thread ‘{name}’"),
            ))
            // @Task the last two sentences should be shown for all `Diagnostic::bug()`s
            .note("the compiler unexpectedly panicked. this is a bug. we would appreciate a bug report")
            .note(format!("lushui {VERSION}"))
            .with(|error| match backtrace {
                Some(backtrace) => error.note(format!("with the following backtrace:\n{backtrace}")),
                None => error.help(
                    "rerun with the environment variable ‘LUSHUI_BACKTRACE=1’ to display a backtrace",
                ),
            })
            .report(&Reporter::stderr());
    }));
}
