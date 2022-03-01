#![feature(
    backtrace,
    derive_default_enum,
    decl_macro,
    default_free_fn,
    let_else,
    associated_type_bounds
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
    clippy::blocks_in_if_conditions, // too many false positives with rustfmt's output
    clippy::similar_names, // too many false positives (#6479)
    clippy::semicolon_if_nothing_returned, // @Temporary false positives with let/else, still
    clippy::same_functions_in_if_condition, // @Temporary false positives with const generics (#8139)
    clippy::needless_pass_by_value, // @Temporary
    clippy::missing_panics_doc, // @Temporary
    clippy::needless_borrow // @Temporary false positives (#8408 I believe)
)]

use cli::{BuildMode, Command, PassRestriction};
use colored::Colorize;
use lushui::{
    component::{Component, ComponentMetadata, ComponentType, Components},
    diagnostics::{
        reporter::{BufferedStderrReporter, ErrorReported, StderrReporter},
        Code, Diagnostic, Reporter,
    },
    documenter,
    error::Result,
    package::{find_package, resolve_file, resolve_package, PackageManifest},
    resolver::{self, PROGRAM_ENTRY_IDENTIFIER},
    session::BuildSession,
    span::{SourceMap, SourceMapCell},
    syntax::{lexer, lowerer, parser, Word},
    typer,
    utility::{DisplayWith, IOError},
    FILE_EXTENSION,
};
use std::io;

mod cli;

const VERSION: &str = concat!(env!("CARGO_PKG_VERSION"), " (", env!("GIT_DATA"), ")");

fn main() {
    if main_().is_err() {
        // all destructors have been run
        std::process::exit(1);
    }
}

fn main_() -> Result {
    set_panic_hook();

    let (command, options) = cli::arguments()?;

    let map = SourceMap::cell();
    let reporter = BufferedStderrReporter::new(map.clone()).into();

    let result = execute_command(command, options, &map, &reporter);

    let reporter: BufferedStderrReporter = reporter.try_into().unwrap();
    let number_of_errors_reported = reporter.release_buffer();

    if number_of_errors_reported > 0 || result.is_err() {
        assert!(
            number_of_errors_reported > 0,
            "some errors occurred but none were reported",
        );

        return Err(ErrorReported::new_unchecked());
    }

    Ok(())
}

fn execute_command(
    command: Command,
    global_options: cli::GlobalOptions,
    map: &SourceMapCell,
    reporter: &Reporter,
) -> Result {
    use Command::*;

    match command {
        BuildPackage { mode, options } => {
            let (components, session) = match &options.path {
                Some(path) => resolve_package(path, map.clone(), reporter)?,
                None => {
                    let current_folder_path = match std::env::current_dir() {
                        Ok(path) => path,
                        Err(error) => {
                            // @Task improve message
                            // @Task more principled io::Error handling please
                            return Err(Diagnostic::error()
                                .message("could not read the current folder")
                                .note(error.to_string())
                                .report(reporter));
                        }
                    };

                    let Some(path) = find_package(&current_folder_path) else {
                        return Err(Diagnostic::error()
                            .message(
                                "neither the current folder nor any of its parents is a package",
                            )
                            .note(format!(
                                "none of the folders contain a package manifest file named `{}`",
                                PackageManifest::FILE_NAME
                            ))
                            .report(reporter));
                    };
                    resolve_package(path, map.clone(), reporter)?
                }
            };

            build_components(
                components,
                mode,
                options.general,
                global_options,
                session,
                map,
                reporter,
            )
        }
        BuildFile { mode, options } => {
            let (components, session) = resolve_file(
                &options.path,
                options.component_type.unwrap_or(ComponentType::Executable),
                options.no_core,
                map.clone(),
                reporter,
            )?;

            build_components(
                components,
                mode,
                options.general,
                global_options,
                session,
                map,
                reporter,
            )
        }
        Explain => todo!(),
        CreatePackage { mode, options } => match mode {
            cli::PackageCreationMode::Initialize => todo!(),
            cli::PackageCreationMode::New { package_name } => {
                create_package(package_name, options, reporter)
            }
        },
    }
}

fn build_components(
    components: Components,
    mode: cli::BuildMode,
    options: cli::BuildOptions,
    global_options: cli::GlobalOptions,
    mut session: BuildSession,
    map: &SourceMapCell,
    reporter: &Reporter,
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
    let component_metadata: Vec<_> = components
        .values()
        .map(|component| component.metadata.clone())
        .collect();

    for mut component in components.into_values() {
        build_component(
            &mut component,
            &component_metadata,
            &mode,
            &options,
            &global_options,
            &mut session,
            map,
            reporter,
        )?;
        session.add(component);
    }

    Ok(())
}

#[allow(clippy::too_many_arguments)] // find a way too reduce them
fn build_component(
    component: &mut Component,
    component_metadata: &[ComponentMetadata],
    mode: &cli::BuildMode,
    options: &cli::BuildOptions,
    global_options: &cli::GlobalOptions,
    mut session: &mut BuildSession,
    map: &SourceMapCell,
    reporter: &Reporter,
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
        // @Beacon @Task don't use package path but component path
        let path = &component.package(&session).path.to_string_lossy();
        // @Task print version
        println!(
            "   {label} {} ({path})",
            if component.in_goal_package(&session)
                && component.metadata.is_ambiguously_named_within_package
            {
                format!("{} ({})", component.name(), component.type_())
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

    let file = map
        .borrow_mut()
        .load(path.value.to_owned())
        .map_err(|error| {
            // @Task improve message, add label
            Diagnostic::error()
                .message(format!(
                    "could not load the {} component `{}` in package `{}`",
                    component.type_(),
                    component.name(),
                    component.package(&session).name,
                ))
                .primary_span(path)
                .note(IOError(error, path.value).to_string())
                .report(reporter)
        })?;

    time! {
        #![name = "Lexing"]
        let tokens = lexer::lex(&map.borrow()[file], reporter)?.value;
    };

    if component.is_goal(&session) && options.emit_tokens {
        for token in &tokens {
            eprintln!("{token:?}");
        }
    }

    restriction_point! { Lexer }

    time! {
        #![name = "Parsing"]
        let component_root = parser::parse_root_module_file(&tokens, file, map.clone(), reporter)?;
    }

    if component.is_goal(&session) && options.emit_ast {
        eprintln!("{component_root:#?}");
    }

    restriction_point! { Parser }

    let lowering_options = lowerer::Options {
        internal_features_enabled: options.internals || component.is_core_library(&session),
        keep_documentation_comments: matches!(mode, BuildMode::Document { .. }),
    };

    time! {
        #![name = "Lowering"]
        let component_root =
        lowerer::lower_file(component_root, lowering_options, map.clone(), reporter)?;
    }

    if component.is_goal(&session) && options.emit_lowered_ast {
        eprintln!("{component_root}");
    }

    restriction_point! { Lowerer }

    time! {
        #![name = "Name Resolution"]
        let component_root =
            resolver::resolve_declarations(component_root, component, &mut session, reporter)?;
    }

    if options.emit_hir {
        eprintln!("{}", component_root.with((&component, &session)));
    }
    if component.is_goal(&session) && options.emit_untyped_scope {
        eprintln!("{}", component.with(&session));
    }

    restriction_point! { Resolver }

    time! {
        #![name = "Type Checking and Inference"]
        typer::check(&component_root, component, &mut session, reporter)?;
    }

    if component.is_goal(&session) && options.emit_scope {
        eprintln!("{}", component.with(&session));
    }

    // @Task move out of main.rs
    if component.is_executable() && component.entry.is_none() {
        return Err(Diagnostic::error()
            .code(Code::E050)
            .message(format!(
                "the component `{}` is missing a program entry named `{PROGRAM_ENTRY_IDENTIFIER}`",
                component.name(),
            ))
            .report(reporter));
    }

    match &mode {
        BuildMode::Run => {
            if component.is_goal(&session) {
                if !component.is_executable() {
                    // @Question code?
                    return Err(Diagnostic::error()
                        .message(format!(
                            "the package `{}` does not contain any executable to run",
                            component.package(&session).name,
                        ))
                        .report(reporter));
                }

                let result =
                    typer::interpreter::evaluate_main_function(&component, &session, reporter)?;

                println!("{}", result.with((&component, &session)));
            }
        }
        BuildMode::Build => {
            // @Temporary not just builds, also runs ^^

            lushui::compiler::compile_and_interpret_declaration(&component_root, &component)
                .unwrap_or_else(|_| panic!());
        }
        BuildMode::Document {
            options: documentation_options,
        } => {
            // @Task implement `--open`ing

            // @Bug leads to broken links, the documenter has to handle this itself
            if documentation_options.no_dependencies && !component.is_goal(&session) {
                return Ok(());
            }

            let documenter_options = documenter::Options {
                asciidoc: documentation_options.asciidoc,
                lorem_ipsum: documentation_options.lorem_ipsum,
            };

            time! {
                #![name = "Documentation Generation"]
                documenter::document(
                    &component_root,
                    documenter_options,
                    component_metadata,
                    &component,
                    &session,
                    reporter,
                )?;
            }
        }
        BuildMode::Check => {}
    }

    Ok(())
}

const SOURCE_FOLDER_NAME: &str = "source";
const LIBRARY_FILE_STEM: &str = "library";
const EXECUTABLE_FILE_STEM: &str = "main";

// @Task generalize the name to a path!
fn create_package(
    name: String,
    options: cli::PackageCreationOptions,
    reporter: &Reporter,
) -> Result {
    use std::fs;

    let name = Word::parse(name.clone()).map_err(|_| {
        // @Task DRY @Question is the common code justified?
        Diagnostic::error()
            .code(Code::E036)
            .message(format!("the package name `{name}` is not a valid word"))
            .report(reporter)
    })?;

    // @Task handle errors properly
    let current_path = std::env::current_dir().unwrap();
    let package_path = current_path.join(name.as_str());
    fs::create_dir(&package_path).unwrap();
    let source_folder_path = package_path.join(SOURCE_FOLDER_NAME);
    fs::create_dir(&source_folder_path).unwrap();
    {
        let package_manifest = io::BufWriter::new(
            fs::File::create(package_path.join(PackageManifest::FILE_NAME)).unwrap(),
        );

        generate_package_manifest(&name, options, package_manifest).unwrap();
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

fn generate_package_manifest(
    name: &Word,
    options: cli::PackageCreationOptions,
    mut sink: impl io::Write,
) -> io::Result<()> {
    writeln!(sink, r#"name: "{name}","#)?;
    writeln!(sink, r#"version: "0.0.0","#)?;
    writeln!(sink)?;

    writeln!(sink, "components: [")?;

    if options.library {
        writeln!(sink, "    {{")?;
        writeln!(sink, r#"        type: "library","#)?;
        writeln!(
            sink,
            r#"        path: "{SOURCE_FOLDER_NAME}/{LIBRARY_FILE_STEM}.lushui","#
        )?;
        writeln!(sink)?;
        writeln!(sink, "        dependencies: {{")?;
        writeln!(sink, r#"            core: {{ provider: "distribution" }},"#)?;
        writeln!(sink, "        }},")?;
        writeln!(sink, "    }},")?;
    }

    if options.executable {
        writeln!(sink, "    {{")?;
        writeln!(sink, r#"        type: "executable","#)?;
        writeln!(
            sink,
            r#"        path: "{SOURCE_FOLDER_NAME}/{EXECUTABLE_FILE_STEM}.lushui","#
        )?;
        writeln!(sink)?;
        writeln!(sink, "        dependencies: {{")?;

        if options.library {
            writeln!(sink, "            {name}: {{}},")?;
        }

        writeln!(sink, r#"            core: {{ provider: "distribution" }},"#)?;
        writeln!(sink, "        }},")?;
        writeln!(sink, "    }},")?;
    }

    writeln!(sink, "],")
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
