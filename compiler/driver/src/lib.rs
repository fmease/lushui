#![feature(decl_macro, let_chains, impl_trait_in_assoc_type, never_type)]

use ast::Render;
use cli::{Backend, BuildMode, Command, PassRestriction};
use diagnostics::{error::Result, reporter::ErasedReportedError, Diag, ErrorCode, Reporter};
use hir_format::Display as _;
use index_map::IndexMap;
use lo_ast::Display as _;
use package::{find_package, resolve_file, resolve_package};
use resolver::ProgramEntryExt;
use session::{
    package::ManifestPath,
    unit::{BuildUnit, CompTy},
    Context, Session,
};
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
    CompIdx, FormatError, PROGRAM_ENTRY,
};

mod cli;
mod create;

pub fn main() -> Result {
    set_panic_hook();

    let (command, opts) = cli::arguments()?;

    let map: Arc<RwLock<SourceMap>> = default();
    // @Bug creating a buffered-stderr-reporter up here is not great at all
    // for BuildMode::Serve! there, we do not want to use it! (we want to
    // use the LSP to communicate server errors)
    // @Task smh break this code / construction up / modularize it
    let reported_any_errors: Arc<AtomicBool> = default();
    let rep =
        Reporter::buffered_stderr(opts.color, reported_any_errors.clone()).with_map(map.clone());

    let res = execute_command(command, &opts, &map, rep);

    let reported_any_errors = reported_any_errors.load(Ordering::SeqCst);

    if let Err(error) = res {
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
    global_opts: &cli::GlobalOptions,
    map: &Arc<RwLock<SourceMap>>,
    rep: Reporter,
) -> Result {
    use Command::*;

    match command {
        BuildPackage { mode, opts } => {
            let (comps, mut cx) = match &opts.path {
                Some(path) => {
                    // intentionally not `!path.is_dir()` to exclude broken symlinks
                    if path.is_file() {
                        // give a more useful diagnostic than the generic "could not load" one
                        return Err(Diag::error()
                            .path(path.clone())
                            .message("the path does not refer to a folder")
                            .help(format!(
                                "consider running ‘lushui file {} {} [OPTIONS]’ instead \
                                 (where ‘file’ precedes the subcommand)\n\
                                 to operate on single source files",
                                mode.name(),
                                path.display(),
                            ))
                            .report(&rep));
                    }

                    resolve_package(path, &opts.filter, map, rep)?
                }
                None => {
                    let curr_dir_path = match std::env::current_dir() {
                        Ok(path) => path,
                        Err(error) => {
                            // @Task improve message
                            return Err(Diag::error()
                                .message("could not read the current folder")
                                .note(error.format())
                                .report(&rep));
                        }
                    };

                    let Some(path) = find_package(&curr_dir_path) else {
                        return Err(Diag::error()
                            .message(
                                "neither the current folder nor any of its parents is a package",
                            )
                            .note(format!(
                                "none of the folders contain a package manifest file named ‘{}’",
                                ManifestPath::FILE_NAME,
                            ))
                            .report(&rep));
                    };
                    resolve_package(path, &opts.filter, map, rep)?
                }
            };

            build_units(comps, &mode, &opts.general, global_opts, &mut cx)
        }
        BuildFile { mode, opts } => {
            // intentionally not `!path.is_file()` to exclude broken symlinks
            if opts.path.is_dir() {
                // give a more useful diagnostic than the generic "could not load" one
                return Err(Diag::error()
                    .path(opts.path.clone())
                    .message("the path does not refer to a file")
                    .help(format!(
                        "consider running ‘lushui {} {} [OPTIONS]’ instead \
                        (where ‘file’ does not precede the subcommand)\n\
                        to operate on packages",
                        mode.name(),
                        opts.path.display(),
                    ))
                    .report(&rep));
            }

            let (comps, mut cx) = resolve_file(
                &opts.path,
                None,
                opts.comp_ty.unwrap_or(CompTy::Executable),
                opts.no_core,
                map,
                rep,
            )?;

            build_units(comps, &mode, &opts.general, global_opts, &mut cx)
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
        Explain => Err(Diag::error()
            .message("the subcommand ‘explain’ is not implemented yet")
            .report(&rep)),
        CreatePackage { mode, opts } => match mode {
            cli::PackageCreationMode::Initialize => Err(Diag::error()
                .message("the subcommand ‘initialize’ is not implemented yet")
                .report(&rep)),
            cli::PackageCreationMode::New { pkg_name } => create::create_pkg(pkg_name, &opts, &rep),
        },
        Recnot { path } => check_recnot_file(&path, map, &rep),
    }
}

fn build_units(
    units: IndexMap<CompIdx, BuildUnit>,
    mode: &cli::BuildMode,
    opts: &cli::BuildOptions,
    global_opts: &cli::GlobalOptions,
    cx: &mut Context,
) -> Result {
    for unit in units.into_values() {
        cx.at(unit, |unit, session| {
            build_unit(unit, mode, opts, global_opts, session)
        })?;
    }

    if let BuildMode::Document { opts } = mode
        && opts.open
    {
        // @Task smh open in background and immediately disown the child process afterwards
        if let Err(error) = open::that(documenter::index_page(cx)) {
            return Err(Diag::error()
                .message("could not open the generated documentation")
                .note(error.format())
                .report(cx.rep()));
        }
    }

    Ok(())
}

#[allow(clippy::needless_pass_by_value)] // by design
fn build_unit(
    unit: BuildUnit,
    mode: &cli::BuildMode,
    opts: &cli::BuildOptions,
    global_opts: &cli::GlobalOptions,
    sess: &mut Session<'_>,
) -> Result {
    // @Task abstract over this as print_status_report and report w/ label="Running" for
    // root component if mode==Run (in addition to initial "Building")
    if !global_opts.quiet {
        paint(
            |stdout| {
                let label = match mode {
                    BuildMode::Check => "Checking",
                    BuildMode::Compile { .. } | BuildMode::Run { .. } => "Building",
                    // FIXME: This shouldn't be printed for non-root components and `--no-deps`.
                    BuildMode::Document { .. } => "Documenting",
                };
                stdout.set(AnsiColor::Green.on_default().bold())?;
                write!(stdout, "   {label} ")?;
                stdout.unset()?;

                writeln!(stdout, "{} ({})", unit.name, unit.path.bare.display())
            },
            global_opts.color,
        )
        .unwrap();
    }

    macro restriction_point($restriction:ident) {
        if unit.is_root(&sess) && opts.pass_restriction == Some(PassRestriction::$restriction) {
            return Ok(());
        }
    }

    macro time(#![doc = $name:literal] $( $block:tt )+) {
        let time = std::time::Instant::now();
        $( $block )+
        let duration = time.elapsed();

        if opts.timing {
            println!("  {:<30}{duration:?}", $name.trim_start());
        }
    }

    if opts.timing {
        eprintln!("Execution times by pass:");
    }

    let path = unit.path.as_deref();

    // @Task don't unconditionally halt execution on failure here but continue (with tainted health)
    // and mark the component as "erroneous" (not yet implemented) so we can print more errors.
    // "Erroneous" components should not lead to further errors in the name resolver etc.
    let file = sess
        .map()
        .load(path.bare, Some(unit.index))
        .map_err(|error| {
            use std::fmt::Write;

            let mut message = format!("could not load the {} component ‘{}’", unit.ty, unit.name,);
            if let Some(package) = sess.pkg() {
                write!(message, " in package ‘{}’", sess[package].name).unwrap();
            }

            // @Task improve message, add label
            Diag::error()
                .message(message)
                .path(path.bare.into())
                .unlabeled_span(path)
                .note(error.format())
                .report(sess.rep())
        })?;

    time! {
        //! Lexing

        let tokens = syntax::lex(file, sess);
    };

    if unit.is_root(sess) && opts.emit_tokens {
        for token in &tokens.tokens {
            eprintln!("{token:?}");
        }
    }

    restriction_point! { Lexer }

    time! {
        //! Parsing

        let comp_root = syntax::parse_root_module_file(tokens, file, sess)?;
    }

    if unit.is_root(sess) && opts.emit_ast {
        epaint(|p| comp_root.render(default(), p), global_opts.color).unwrap();
        eprintln!();
    }

    restriction_point! { Parser }

    // @Task get rid of this! move into session / component of root package!
    let lowering_opts = lowerer::Options {
        internal_features_enabled: opts.internals || unit.is_core_lib(sess),
        keep_doc_comments: matches!(mode, BuildMode::Document { .. }),
    };

    time! {
        //! Lowering

        let comp_root =
            lowerer::lower_file(comp_root, lowering_opts, sess)?;
    }

    if unit.is_root(sess) && opts.emit_lo_ast {
        eprintln!("{}", displayed(|f| comp_root.write(f)));
    }

    restriction_point! { Lowerer }

    time! {
        //! Name Resolution

        let comp_root =
            resolver::resolve_decls(comp_root, sess)?;
    }

    if unit.is_root(sess) && opts.emit_hir {
        eprintln!("{}", displayed(|f| comp_root.write(sess, f)));
    }
    if unit.is_root(sess) && opts.emit_untyped_bindings {
        eprintln!("{}", displayed(|f| sess.comp().write(sess, f)));
    }

    restriction_point! { Resolver }

    time! {
        //! Type Checking and Inference

        typer::check(&comp_root, sess)?;
    }

    if unit.is_root(sess) && opts.emit_bindings {
        eprintln!("{}", displayed(|f| sess.comp().write(sess, f)));
    }

    // @Task move out of main.rs
    // @Update @Task move into respective backends: LLVM and interpreter!
    if unit.ty == CompTy::Executable && sess.look_up_program_entry().is_none() {
        return Err(Diag::error()
            .code(ErrorCode::E050)
            .message(format!(
                "the component ‘{}’ does not contain a ‘{PROGRAM_ENTRY}’ function in its root module",
                unit.name
            ))
            .unlabeled_span(&sess.shared_map()[file])
            .report(sess.rep()));
    }

    match &mode {
        BuildMode::Run { opts } => {
            if unit.is_root(sess) {
                if unit.ty != CompTy::Executable {
                    // @Question code?
                    // @Note I don't like this code here at all, it's hacky and not principled!
                    // @Question why should the message differ??
                    return Err(Diag::error()
                        .message(match sess.pkg() {
                            Some(pkg) => format!(
                                "the package ‘{}’ does not contain any executable to run",
                                sess[pkg].name,
                            ),
                            None => "the component is not an executable".into(),
                        })
                        .report(sess.rep()));
                }

                match opts.backend {
                    Backend::Hiri => {
                        let res = typer::interp::eval_main_func(sess)?;
                        println!("{}", displayed(|f| res.write(sess, f)));
                    }
                    #[cfg(feature = "cranelift")]
                    Backend::Cranelift => {
                        // @Task spawn Command where the path is session.build_folder() + ...
                        return Err(Diag::error().message(
                            "running executables built with the Cranelift backend is not supported yet",
                        )
                        .report(sess.rep()));
                    }
                    #[cfg(feature = "llvm")]
                    Backend::Llvm => {
                        // @Task spawn Command where the path is session.build_folder() + ...
                        return Err(Diag::error().message(
                            "running executables built with the LLVM backend is not supported yet",
                        )
                        .report(sess.rep()));
                    }
                }
            }
        }
        BuildMode::Compile { opts } => match opts.backend {
            Backend::Hiri => {
                // @Task smh print this earlier than the status info (“Building”)
                return Err(Diag::error()
                    .message("HIRI does not support compilation")
                    .report(sess.rep()));
            }
            #[cfg(feature = "cranelift")]
            Backend::Cranelift => {
                if !unit.is_root(sess) {
                    return Err(Diag::error()
                        .message("extern components cannot be built yet with the Cranelift backend")
                        .report(sess.rep()));
                }

                codegen_cranelift::compile_and_link(
                    codegen_cranelift::Options {
                        emit_clif: opts.emit_clif,
                        verify_clif: opts.verify_clif,
                    },
                    &comp_root,
                    sess,
                )?;
            }
            #[cfg(feature = "llvm")]
            Backend::Llvm => {
                if !unit.is_root(sess) {
                    return Err(Diag::error()
                        .message("extern components cannot be built yet with the LLVM backend")
                        .report(sess.rep()));
                }

                codegen_llvm::compile_and_link(
                    codegen_llvm::Options {
                        emit_llvm_ir: opts.emit_llvm_ir,
                        verify_llvm_ir: opts.verify_llvm_ir,
                    },
                    &comp_root,
                    sess,
                )?;
            }
        },
        BuildMode::Document { opts } => {
            // @Bug leads to broken links, @Task the documenter has to handle this itself
            if opts.no_deps && !unit.is_root(sess) {
                return Ok(());
            }

            time! {
                //! Documentation Generation

                documenter::document_comp(
                    &comp_root,
                    opts.general,
                    sess,
                )?;
            }
        }
        // already done at this point
        BuildMode::Check => {}
    }

    Ok(())
}

fn check_recnot_file(path: &Path, map: &Arc<RwLock<SourceMap>>, rep: &Reporter) -> Result {
    let file = map.write().unwrap().load(path, None).map_err(|error| {
        Diag::error()
            .message("could not load the file")
            .path(path.into())
            .note(error.format())
            .report(rep)
    })?;

    recnot::parse(file, map, rep).map(drop)
}

fn set_panic_hook() {
    std::panic::set_hook(Box::new(|info| {
        let payload = info.payload();

        let message = payload
            .downcast_ref::<&str>()
            .copied()
            .or_else(|| payload.downcast_ref::<String>().map(String::as_str))
            .unwrap_or("unknown cause")
            .to_owned();

        let backtrace = std::env::var("LUSHUI_BACKTRACE")
            .map_or(false, |variable| variable != "0")
            .then(std::backtrace::Backtrace::force_capture);

        Diag::bug()
            .message(message)
            .with(|it| match info.location() {
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
