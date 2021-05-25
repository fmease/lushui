#![forbid(rust_2018_idioms, unused_must_use)]

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
use std::{
    cell::RefCell,
    fs::File,
    io::BufWriter,
    path::{Path, PathBuf},
    rc::Rc,
};
use structopt::StructOpt;

const VERSION: &str = concat!(
    env!("CARGO_PKG_VERSION"),
    " (",
    env!("GIT_COMMIT_HASH"),
    " ",
    env!("GIT_COMMIT_DATE"),
    ")"
);

// @Beacon @Task rewrite the argument parsing logic to be less DRY
// and also move it to a file module

#[derive(StructOpt)]
#[structopt(version = VERSION, author, about)]
struct Arguments {
    /// Use rustc panic hook with RUST_BACKTRACE=1
    #[structopt(long, short = "B")]
    panic_with_backtrace: bool,

    #[structopt(subcommand)]
    command: Command,
}

impl Arguments {
    // @Task remove (create function instead which uses unwrap_or(false))
    // @Note what a cruel poilerplate :'(
    fn merged(&self) -> MergedCommandArguments<'_> {
        let mut only_lex_ = false;
        let mut print_tokens_ = false;
        let mut only_parse_ = false;
        let mut print_ast_ = false;
        let mut only_lower_ = false;
        let mut print_lowered_ast_ = false;
        let mut display_crate_indices_ = false;
        let mut only_resolve_ = false;
        let mut print_hir_ = false;
        let mut print_scope_ = false;
        let mut print_typed_scope_ = false;
        let file_: &Path;

        match &self.command {
            Command::Check {
                file,
                only_lex,
                print_tokens,
                only_parse,
                print_ast,
                only_lower,
                print_lowered_ast,
                print_typed_scope,
                only_resolve,
                print_hir,
                print_scope,
                display_crate_indices,
            } => {
                file_ = file;
                only_lex_ = *only_lex;
                print_tokens_ = *print_tokens;
                only_parse_ = *only_parse;
                print_ast_ = *print_ast;
                only_lower_ = *only_lower;
                print_lowered_ast_ = *print_lowered_ast;
                print_typed_scope_ = *print_typed_scope;
                only_resolve_ = *only_resolve;
                print_hir_ = *print_hir;
                print_scope_ = *print_scope;
                display_crate_indices_ = *display_crate_indices;
            }
            Command::Run {
                file,
                print_scope: print_interpreter_scope,
                display_crate_indices,
            } => {
                file_ = file;
                print_typed_scope_ = *print_interpreter_scope;
                display_crate_indices_ = *display_crate_indices;
            }
            Command::Build { file } => {
                file_ = file;
            }
            Command::Highlight { file } => {
                file_ = file;
            }
            Command::Document { file } => {
                file_ = file;
            }
        }

        MergedCommandArguments {
            only_lex: only_lex_,
            print_tokens: print_tokens_,
            only_parse: only_parse_,
            print_ast: print_ast_,
            only_lower: only_lower_,
            print_lowered_ast: print_lowered_ast_,
            display_crate_indices: display_crate_indices_,
            only_resolve: only_resolve_,
            print_hir: print_hir_,
            print_scope: print_scope_,
            print_typed_scope: print_typed_scope_,
            file: file_,
        }
    }
}

struct MergedCommandArguments<'a> {
    only_lex: bool,
    print_tokens: bool,
    only_parse: bool,
    print_ast: bool,
    only_lower: bool,
    print_lowered_ast: bool,
    display_crate_indices: bool,
    only_resolve: bool,
    print_hir: bool,
    print_scope: bool,
    print_typed_scope: bool,
    file: &'a Path,
}

// @Task gather all print flags under a common --print=THING option
#[derive(StructOpt)]
enum Command {
    /// Type check a given program
    Check {
        /// Only run the lexer
        #[structopt(long)]
        only_lex: bool,

        /// Print the tokens emitted by the lexer
        #[structopt(long)]
        print_tokens: bool,

        /// Only run the parser
        #[structopt(long)]
        only_parse: bool,

        /// Print the AST
        #[structopt(long)]
        print_ast: bool,

        /// Only lower
        #[structopt(long)]
        only_lower: bool,

        /// Print the lowered AST
        #[structopt(long)]
        print_lowered_ast: bool,

        /// Display crate indices when emitting a resolved HIR
        #[structopt(long)]
        display_crate_indices: bool,

        /// Only run the resolver
        #[structopt(long)]
        only_resolve: bool,

        /// Print the HIR
        #[structopt(long)]
        print_hir: bool,

        /// Print the crate scope after name resolution
        #[structopt(long)]
        print_scope: bool,

        /// Print the crate scope after type checking
        #[structopt(long)]
        print_typed_scope: bool,

        /// Set the source file
        #[structopt(name = "FILE")]
        file: PathBuf,
    },
    /// Type check and run a given program
    Run {
        /// Display crate indices when emitting a resolved HIR
        #[structopt(long)]
        display_crate_indices: bool,

        /// Print the crate scope after name resolution
        #[structopt(long)]
        print_scope: bool,

        /// Set the source file
        #[structopt(name = "FILE")]
        file: PathBuf,
    },
    /// Compile the code to bytecode.
    Build {
        /// Set the source file
        #[structopt(name = "FILE")]
        file: PathBuf,
    },
    /// Generate syntax highlighting for a given file in HTML
    Highlight {
        /// Set the source file
        #[structopt(name = "FILE")]
        file: PathBuf,
    },
    /// Generate HTML documentation.
    Document {
        /// Set the source file
        #[structopt(name = "FILE")]
        file: PathBuf,
    },
}

// @Task add --engine|e=twi|bci

fn main() {
    let arguments = Arguments::from_args();

    if arguments.panic_with_backtrace {
        std::env::set_var("RUST_BACKTRACE", "1");
    } else {
        set_panic_hook();
    }

    let merged_arguments = arguments.merged();

    lushui::OPTIONS
        .set(lushui::Options {
            display_crate_indices: merged_arguments.display_crate_indices,
        })
        .unwrap_or_else(|_| unreachable!());

    let map = Rc::new(RefCell::new(SourceMap::default()));
    let handler = Handler::buffered_stderr(map.clone());

    let result: Result = (|| {
        let path = merged_arguments.file;
        let source_file = map
            .borrow_mut()
            .load(path.to_owned())
            .map_err(|error| Diagnostic::from(error).emit(&handler))?;

        let crate_name =
            lushui::parse_crate_name(path, &handler).map_err(|error| error.emit(&handler))?;

        let Outcome {
            value: tokens,
            health,
        } = Lexer::new(map.borrow().get(source_file), &handler).lex()?;

        if merged_arguments.print_tokens {
            eprintln!("{:#?}", tokens);
        }
        if merged_arguments.only_lex {
            if health.is_tainted() {
                return Err(());
            }
            return Ok(());
        }

        let declaration =
            Parser::new(source_file, &tokens, map.clone(), &handler).parse(crate_name.clone())?;
        if health.is_tainted() {
            return Err(());
        }
        if merged_arguments.print_ast {
            eprintln!("{:#?}", declaration);
        }
        if merged_arguments.only_parse {
            return Ok(());
        }

        match arguments.command {
            Command::Check { .. } | Command::Run { .. } | Command::Build { .. } => {
                let Outcome {
                    value: mut declarations,
                    health,
                } = Lowerer::new(map.clone(), &handler).lower_declaration(declaration);

                if health.is_tainted() {
                    return Err(());
                }

                let declaration = declarations.pop().unwrap();

                {
                    if merged_arguments.print_lowered_ast {
                        eprintln!("{}", declaration);
                    }
                    if merged_arguments.only_lower {
                        return Ok(());
                    }
                }

                let mut scope = CrateScope::default();
                let mut resolver = Resolver::new(&mut scope, &handler);
                let declaration = resolver.resolve_declaration(declaration)?;

                {
                    if merged_arguments.print_hir {
                        eprintln!("{}", declaration.with(&scope));
                    }
                    if merged_arguments.print_scope {
                        eprintln!("{:#?}", scope);
                    }
                    if merged_arguments.only_resolve {
                        return Ok(());
                    }
                }

                scope.register_foreign_bindings();

                let mut typer = Typer::new(&mut scope, &handler);
                typer.infer_types_in_declaration(&declaration)?;

                {
                    if merged_arguments.print_typed_scope {
                        eprintln!("{:#?}", typer.scope);
                    }
                }

                // @Beacon @Task dont check for program_entry in scope.run() or compile_and_interp
                // but here (a static error) (if the CLI dictates to run it)

                if let Command::Run { .. } = arguments.command {
                    let result = typer.interpreter().run()?;

                    println!("{}", result.with(&scope));
                }
                // @Temporary
                else if let Command::Build { .. } = arguments.command {
                    // @Temporary not just builds, also runs ^^

                    lushui::compiler::compile_and_interpret_declaration(&declaration, &scope)
                        .unwrap_or_else(|_| panic!());
                }
            }
            Command::Highlight { .. } => {
                Diagnostic::unimplemented("operation").emit(&handler);
                return Err(());
            }
            Command::Document { .. } => {
                // @Task error handling
                let mut file = BufWriter::new(File::create(path.with_extension("html")).unwrap());
                let documenter = Documenter::new(&mut file, map);
                // @Temporary @Task
                documenter.document(&declaration).unwrap();
            }
        }

        Ok(())
    })();

    handler.emit_buffered_diagnostics();

    // if result.is_err() || handler.system_health().is_tainted() {
    //     // @Task use ExitStatus
    //     std::process::exit(1);
    // }

    if result.is_err() {
        // I think
        assert!(handler.system_health().is_tainted());
        // @Task use ExitStatus
        std::process::exit(1);
    }
}

// @Task print a stacktrace
fn set_panic_hook() {
    std::panic::set_hook(Box::new(|information| {
        let payload = information.payload();

        let mut message = payload
            .downcast_ref::<&str>()
            .copied()
            .or_else(|| payload.downcast_ref::<String>().map(|payload| &payload[..]))
            .unwrap_or_else(|| "unknown cause")
            .to_owned();
        if let Some(location) = information.location() {
            message += &format!(" at {}", location);
        }

        Diagnostic::bug().message(message).emit_to_stderr(None);
    }));
}
