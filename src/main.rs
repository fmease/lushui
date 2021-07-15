#![feature(backtrace, format_args_capture)]
#![forbid(rust_2018_idioms, unused_must_use)]
#![allow(unused_imports)]

use lushui::{
    diagnostics::{Diagnostic, Handler},
    documenter::Documenter,
    error::{Outcome, Result},
    format::DisplayWith,
    lexer::Lexer,
    lowerer::Lowerer,
    parser::Parser,
    resolver,
    span::SourceMap,
    typer::Typer,
};
use resolver::{CrateScope, Resolver};
use std::{cell::RefCell, fs::File, io::BufWriter, rc::Rc};

use cli::{Command, PhaseRestriction};

mod cli;

fn main() {
    if main_().is_err() {
        std::process::exit(1);
    }
}

fn main_() -> Result<(), ()> {
    set_panic_hook();

    let application = cli::Application::new();

    lushui::set_global_options(lushui::Options {
        show_binding_indices: application.show_binding_indices,
    });

    let map = Rc::new(RefCell::new(SourceMap::default()));
    let handler = Handler::buffered_stderr(map.clone());

    let result: Result = (|| {
        match &application.command {
            Command::Check { source_file_path }
            | Command::Run { source_file_path }
            | Command::Build { source_file_path } => {
                let source_file = map
                    .borrow_mut()
                    .load(source_file_path.clone())
                    .map_err(|error| Diagnostic::from(error).emit(&handler))?;

                let crate_name = lushui::lexer::parse_crate_name(source_file_path, &handler)
                    .map_err(|error| error.emit(&handler))?;

                let Outcome {
                    value: tokens,
                    health,
                } = Lexer::new(map.borrow().get(source_file), &handler).lex()?;

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

                let declaration = Parser::new(source_file, &tokens, map.clone(), &handler)
                    .parse(crate_name.clone())?;
                if health.is_tainted() {
                    return Err(());
                }
                if application.dump.ast {
                    eprintln!("{declaration:#?}");
                }
                if application.phase_restriction == Some(PhaseRestriction::Parser) {
                    return Ok(());
                }

                let Outcome {
                    value: mut declarations,
                    health,
                } = Lowerer::new(map.clone(), &handler).lower_declaration(declaration);

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

                let mut scope = CrateScope::default();
                let mut resolver = Resolver::new(&mut scope, &handler);
                let declaration = resolver.resolve_declaration(declaration)?;

                {
                    if application.dump.hir {
                        eprintln!("{}", declaration.with(&scope));
                    }
                    if application.dump.untyped_scope {
                        eprintln!("{scope:#?}");
                    }
                    if application.phase_restriction == Some(PhaseRestriction::Resolver) {
                        return Ok(());
                    }
                }

                scope.register_foreign_bindings();

                let mut typer = Typer::new(&mut scope, &handler);
                typer.infer_types_in_declaration(&declaration)?;

                {
                    if application.dump.scope {
                        eprintln!("{:#?}", typer.scope);
                    }
                }

                // @Beacon @Task dont check for program_entry in scope.run() or compile_and_interp
                // but here (a static error) (if the CLI dictates to run it)

                if let Command::Run { .. } = application.command {
                    let result = typer.interpreter().run()?;

                    println!("{}", result.with(&scope));
                }
                // @Temporary
                else if let Command::Build { .. } = application.command {
                    // @Temporary not just builds, also runs ^^

                    lushui::compiler::compile_and_interpret_declaration(&declaration, &scope)
                        .unwrap_or_else(|_| panic!());
                }
            }
        }

        Ok(())
    })();

    handler.emit_buffered_diagnostics();

    if result.is_err() {
        assert!(handler.system_health().is_tainted());
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
            .when_present(std::thread::current().name(), |this, name| {
                this.note(format!("in thread `{name}`"))
            })
            .note(format!("with the following backtrace:\n{backtrace}"))
            .emit_to_stderr(None);
    }));
}
