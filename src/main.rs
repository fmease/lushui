#![forbid(rust_2018_idioms, unused_must_use)]

use lushui::{
    diagnostics::{Diagnostic, Diagnostics, Results},
    documenter::Documenter,
    error::ManyErrExt,
    format::{pluralize, s_pluralize, DisplayWith},
    lexer::Lexer,
    lowerer::Lowerer,
    parser::Parser,
    resolver,
    span::SourceMap,
    typer::Typer,
};
use resolver::{CrateScope, Resolver};
use std::{
    fs::File,
    io::BufWriter,
    path::{Path, PathBuf},
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

    #[structopt(long)]
    sort_diagnostics: bool,

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

    let mut map = SourceMap::default();
    let mut warnings = Diagnostics::default();

    let result: Results = (|| {
        let path = merged_arguments.file;
        let source_file = map.load(path).map_err(Into::into).many_err()?;

        let crate_name = lushui::parse_crate_name(path).many_err()?;

        let tokens = Lexer::new(&map[source_file], &mut warnings).lex()?;
        if merged_arguments.print_tokens {
            eprintln!("{:#?}", tokens);
        }
        if merged_arguments.only_lex {
            return Ok(());
        }

        let declaration =
            Parser::new(&map, source_file, &tokens, &mut warnings).parse(crate_name.clone())?;
        if merged_arguments.print_ast {
            eprintln!("{:#?}", declaration);
        }
        if merged_arguments.only_parse {
            return Ok(());
        }

        match arguments.command {
            Command::Check { .. } | Command::Run { .. } | Command::Build { .. } => {
                let declaration = Lowerer::new(&mut map, &mut warnings)
                    .lower_declaration(declaration)?
                    .pop()
                    .unwrap();

                {
                    if merged_arguments.print_lowered_ast {
                        eprintln!("{}", declaration);
                    }
                    if merged_arguments.only_lower {
                        return Ok(());
                    }
                }

                let mut scope = CrateScope::default();
                let mut resolver = Resolver::new(&mut scope, &mut warnings);
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

                let mut typer = Typer::new(&mut scope, &mut warnings);
                typer.infer_types_in_declaration(&declaration)?;

                {
                    if merged_arguments.print_typed_scope {
                        eprintln!("{:#?}", typer.scope);
                    }
                }

                // @Beacon @Task dont check for program_entry in scope.run() or compile_and_interp
                // but here (a static error) (if the CLI dictates to run it)

                if let Command::Run { .. } = arguments.command {
                    let result = typer.interpreter().run().many_err()?;

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
                return Err(Diagnostic::unimplemented("operation")).many_err()
            }
            Command::Document { .. } => {
                // @Task error handling
                let mut file = BufWriter::new(File::create(path.with_extension("html")).unwrap());
                let documenter = Documenter::new(&mut file, &map, &mut warnings);
                // @Temporary @Task
                documenter.document(&declaration).unwrap();
            }
        }

        Ok(())
    })();

    if !warnings.is_empty() {
        let amount = emit_diagnostics(warnings, &mut map, arguments.sort_diagnostics);

        const MINIMUM_AMOUNT_WARNINGS_FOR_SUMMARY: usize = 0;

        // @Question should we print this at all?
        if amount >= MINIMUM_AMOUNT_WARNINGS_FOR_SUMMARY {
            let _ = Diagnostic::warning()
                .with_message(format!(
                    "emitted {} {}",
                    amount,
                    s_pluralize!(amount, "warning")
                ))
                .emit_to_stderr(Some(&map));
        }
    }

    if let Err(errors) = result {
        let amount = emit_diagnostics(errors, &mut map, arguments.sort_diagnostics);

        let _ = Diagnostic::error()
            .with_message(pluralize(amount, "aborting due to previous error", || {
                format!("aborting due to {} previous errors", amount)
            }))
            .emit_to_stderr(Some(&map));

        // @Task instead of this using this function, return a std::process::ExitCode
        // from main once stable again. I am not sure but it could be that right now,
        // destructors are not run
        std::process::exit(1);
    }
}

fn emit_diagnostics(diagnostics: Diagnostics, map: &mut SourceMap, sort: bool) -> usize {
    if sort {
        let mut diagnostics: Vec<_> = diagnostics.into_iter().collect();
        diagnostics.sort_by_key(|error| error.sorted_spans());

        diagnostics
            .into_iter()
            .map(|diagnostic| diagnostic.emit_to_stderr(Some(&map)))
            .filter(|&emitted| emitted)
            .count()
    } else {
        diagnostics
            .into_iter()
            .map(|diagnostic| diagnostic.emit_to_stderr(Some(&map)))
            .filter(|&emitted| emitted)
            .count()
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

        let _ = Diagnostic::bug().with_message(message).emit_to_stderr(None);
    }));
}
