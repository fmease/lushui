#![feature(backtrace, format_args_capture, derive_default_enum, unwrap_infallible)]
#![forbid(rust_2018_idioms, unused_must_use)]

use cli::{Command, PhaseRestriction};
use indexed_vec::Idx;
use lushui::{
    crates::{Binary, BinaryManifest, Crate, CrateIndex, CrateStore, Library, Manifest},
    diagnostics::{
        reporter::{BufferedStderrReporter, StderrReporter},
        Diagnostic, Reporter,
    },
    error::{Outcome, Result},
    format::DisplayWith,
    lexer::Lexer,
    lowerer::Lowerer,
    parser::{ast::Identifier, Parser},
    resolver,
    span::{SourceMap, Span},
    typer::Typer,
};
use resolver::Resolver;
use rustc_hash::FxHashMap as HashMap;
use std::{cell::RefCell, path::Path, rc::Rc};

mod cli;

type Str = std::borrow::Cow<'static, str>;

fn main() {
    if main_().is_err() {
        std::process::exit(1);
    }
}

fn main_() -> Result<(), ()> {
    set_panic_hook();

    let application = cli::Application::new();

    // @Task get rid of this!
    lushui::set_global_options(lushui::Options {
        show_binding_indices: application.show_binding_indices,
    });

    // @Question one sourcemap for all crates or one for each?
    let map = Rc::new(RefCell::new(SourceMap::default()));
    let reporter = BufferedStderrReporter::new(map.clone()).into();

    let result: Result = (|| {
        match &application.command {
            Command::Check | Command::Run | Command::Build => {
                // @Temporary
                let mut next_crate_index = CrateIndex::new(0);
                let mut next_crate_index = || {
                    let index = next_crate_index;
                    next_crate_index = CrateIndex::new(next_crate_index.index() + 1);
                    index
                };

                let mut built_crates = CrateStore::default();

                // @Temporary architecture
                // question make this some kind of CrateStore' ?
                // so we're gonna have like 2 of these: done + WIP?
                let mut crates_to_build = Vec::new();

                // @Temporary
                if application.source_file_path.is_some() && !application.unlink_core {
                    let core_library_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("libs/core");
                    let manifest = Manifest::open(&core_library_path).into_ok();
                    crates_to_build.push(Crate::from_manifest(
                        false,
                        next_crate_index(),
                        core_library_path,
                        manifest,
                    ))
                }

                crates_to_build.push(match application.source_file_path {
                    Some(source_file_path) => {
                        let crate_name =
                            lushui::lexer::parse_crate_name(source_file_path.clone(), &reporter)
                                .map_err(|error| error.report(&reporter))?;

                        Crate::from_manifest(
                            true,
                            next_crate_index(),
                            // @Task dont unwrap, handle error case
                            source_file_path.parent().unwrap().to_owned(),
                            Manifest {
                                // @Task we can do better here
                                name: crate_name.as_str().to_owned(),
                                version: "0.0.0".to_owned(),
                                description: String::new(),
                                private: true,
                                // @Bug we don't want to signal "check for yourself if there exists source/library.lushui" with None
                                // but to explicitly set the library to None
                                // meaning Manifest is not the correct type we want here!!
                                library: None,
                                binary: Some(BinaryManifest {
                                    path: Some(source_file_path),
                                }),
                                dependencies: HashMap::default(),
                            },
                        )
                    }
                    None => {
                        // @Task search parent directories as well!!
                        // @Task dont unwrap, handle the error cases
                        let path = std::env::current_dir().unwrap();
                        let manifest = Manifest::open(&path).into_ok();

                        Crate::from_manifest(true, next_crate_index(), path, manifest)
                    }
                });

                // @Task forall crates in dependencies "compile"/… them
                // and in the end crates.add() them
                for mut crate_ in crates_to_build {
                    // @Beacon @Beacon @Task create CLI-flag -q/--quiet
                    // and don't print this if it is set
                    // eprintln!(
                    //     "building crate `{}` ({:?})",
                    //     crate_.name, crate_.scope.owner
                    // ); // @Temporary

                    // @Temporary
                    let source_file_path = match (&crate_.library, &crate_.binary) {
                        (None, None) => panic!(),
                        (None, Some(Binary { path })) | (Some(Library { path }), None) => path,
                        (Some(_), Some(_)) => todo!(),
                    };

                    let source_file = map
                        .borrow_mut()
                        .load(source_file_path.clone())
                        .map_err(|error| Diagnostic::from(error).report(&reporter))?;

                    let Outcome {
                        value: tokens,
                        health,
                    } = Lexer::new(map.borrow().get(source_file), &reporter).lex()?;

                    {
                        if application.dump.tokens {
                            for token in &tokens {
                                eprintln!("{:?}", token);
                            }
                        }
                        if application.phase_restriction == Some(PhaseRestriction::Lexer) {
                            if health.is_tainted() {
                                return Err(());
                            }
                            return Ok(());
                        }
                    }

                    let declaration = Parser::new(source_file, &tokens, map.clone(), &reporter)
                        .parse(Identifier::new(crate_.name.clone().into(), Span::SHAM))?;
                    if health.is_tainted() {
                        return Err(());
                    }
                    {
                        if application.dump.ast {
                            eprintln!("{declaration:#?}");
                        }
                        if application.phase_restriction == Some(PhaseRestriction::Parser) {
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
                        if application.dump.lowered_ast {
                            eprintln!("{}", declaration);
                        }
                        if application.phase_restriction == Some(PhaseRestriction::Lowerer) {
                            return Ok(());
                        }
                    }

                    let mut resolver = Resolver::new(&mut crate_.scope, &built_crates, &reporter);
                    let declaration = resolver.resolve_declaration(declaration)?;

                    {
                        if application.dump.hir {
                            eprintln!("{}", declaration.with((&crate_.scope, &built_crates)));
                        }
                        if application.dump.untyped_scope {
                            eprintln!("{}", crate_.scope.with(&built_crates));
                        }
                        if application.phase_restriction == Some(PhaseRestriction::Resolver) {
                            return Ok(());
                        }
                    }

                    // @Beacon @Temporary only in `core`
                    crate_.scope.register_foreign_bindings();

                    let mut typer = Typer::new(&mut crate_.scope, &built_crates, &reporter);
                    typer.infer_types_in_declaration(&declaration)?;

                    {
                        if application.dump.scope {
                            eprintln!("{}", typer.scope.with(&built_crates));
                        }
                    }

                    // @Beacon @Task dont check for program_entry in scope.run() or compile_and_interp
                    // but here (a static error) (if the CLI dictates to run it)

                    if let Command::Run = application.command {
                        if crate_.is_main {
                            let result = typer.interpreter().run()?;

                            println!("{}", result.with((&crate_.scope, &built_crates)));
                        }
                    }
                    // @Temporary
                    else if let Command::Build = application.command {
                        // @Temporary not just builds, also runs ^^

                        lushui::compiler::compile_and_interpret_declaration(
                            &declaration,
                            &crate_.scope,
                        )
                        .unwrap_or_else(|_| panic!());
                    }

                    // @Note awkward: crate_.scope.owner
                    built_crates.add(crate_.scope.owner, crate_);
                }
            }
        }

        Ok(())
    })();

    let reporter = match reporter {
        Reporter::BufferedStderr(reporter) => reporter,
        _ => unreachable!(),
    };

    let number_of_errors_reported = reporter.release_buffer();

    if result.is_err() {
        assert!(
            number_of_errors_reported > 0,
            "some errors occurred but none were reported",
        );
        return Err(());
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
