#![forbid(rust_2018_idioms, unused_must_use)]

use lushui::{
    diagnostic::{Diagnostic, Results},
    interpreter,
    lexer::Lexer,
    parser::Parser,
    resolver,
    span::SourceMap,
    support::{pluralize, ManyErrExt},
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
        let mut arguments = MergedCommandArguments::default();

        match &self.command {
            Command::Check {
                file,
                only_lex,
                print_tokens,
                only_parse,
                print_ast,
                only_desugar,
                print_desugar_hir,
                print_interpreter_scope,
                only_resolve,
                print_resolver_hir,
                print_resolver_scope,
                display_crate_indices,
            } => {
                arguments.file = file;
                arguments.only_lex = *only_lex;
                arguments.print_tokens = *print_tokens;
                arguments.only_parse = *only_parse;
                arguments.print_ast = *print_ast;
                arguments.only_desugar = *only_desugar;
                arguments.print_desugar_hir = *print_desugar_hir;
                arguments.print_interpreter_scope = *print_interpreter_scope;
                arguments.only_resolve = *only_resolve;
                arguments.print_resolver_hir = *print_resolver_hir;
                arguments.print_resolver_scope = *print_resolver_scope;
                arguments.display_crate_indices = *display_crate_indices;
            }
            Command::Run {
                file,
                print_interpreter_scope,
                display_crate_indices,
            } => {
                arguments.file = file;
                arguments.print_interpreter_scope = *print_interpreter_scope;
                arguments.display_crate_indices = *display_crate_indices;
            }
            Command::Highlight { file } => {
                arguments.file = file;
            }
        }

        arguments
    }
}

#[derive(Default)]
struct MergedCommandArguments<'a> {
    only_lex: bool,
    print_tokens: bool,
    only_parse: bool,
    print_ast: bool,
    only_desugar: bool,
    print_desugar_hir: bool,
    display_crate_indices: bool,
    only_resolve: bool,
    print_resolver_hir: bool,
    print_resolver_scope: bool,
    print_interpreter_scope: bool,
    file: &'a str,
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

        /// Only desugar
        #[structopt(long)]
        only_desugar: bool,

        /// Print the HIR emitted after desugaring
        #[structopt(long)]
        print_desugar_hir: bool,

        /// Display crate indices when emitting a resolved HIR
        #[structopt(long)]
        display_crate_indices: bool,

        /// Only run the resolver
        #[structopt(long)]
        only_resolve: bool,

        /// Print the HIR emitted by the resolver
        #[structopt(long)]
        print_resolver_hir: bool,

        /// Print the crate scope of the resolver
        #[structopt(long)]
        print_resolver_scope: bool,

        /// Print the crate scope of the interpreter
        #[structopt(long)]
        print_interpreter_scope: bool,

        /// Set the source file
        #[structopt(name = "FILE")]
        file: String,
    },
    /// Type check and run a given program
    Run {
        /// Display crate indices when emitting a resolved HIR
        #[structopt(long)]
        display_crate_indices: bool,

        /// Print the crate scope of the interpreter
        #[structopt(long)]
        print_interpreter_scope: bool,

        /// Set the source file
        #[structopt(name = "FILE")]
        file: String,
    },
    /// Generate syntax highlighting for a given file in HTML
    Highlight {
        /// Set the source file
        #[structopt(name = "FILE")]
        file: String,
    },
}

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

    let result: Results<()> = (|| {
        let path = merged_arguments.file;
        let source_file = map.load(path).many_err()?;

        let crate_name = lushui::parse_crate_name(path).many_err()?;

        let tokens = Lexer::new(&source_file).lex()?;
        if merged_arguments.print_tokens {
            eprintln!("{:#?}", tokens);
        }
        if merged_arguments.only_lex {
            return Ok(());
        }

        let node = Parser::new(source_file, &tokens)
            .parse_top_level(crate_name.clone())
            .many_err()?;
        if merged_arguments.print_ast {
            eprintln!("{:#?}", node);
        }
        if merged_arguments.only_parse {
            return Ok(());
        }

        match arguments.command {
            Command::Check { .. } | Command::Run { .. } => {
                let node = node.desugar(&mut map)?.remove(0);
                if merged_arguments.print_desugar_hir {
                    eprintln!("{}", node);
                }
                if merged_arguments.only_desugar {
                    return Ok(());
                }

                let mut scope = resolver::CrateScope::default();

                let node = node.resolve(&mut scope)?;
                if merged_arguments.print_resolver_hir {
                    eprintln!("{}", node);
                }
                if merged_arguments.print_resolver_scope {
                    eprintln!("{:#?}", scope);
                }
                if merged_arguments.only_resolve {
                    return Ok(());
                }

                let mut scope = interpreter::CrateScope::new(scope);
                node.infer_type(&mut scope).many_err()?;
                if merged_arguments.print_interpreter_scope {
                    eprintln!("{:#?}", scope);
                }

                if matches!(arguments.command, Command::Run {..}) {
                    let result = scope.run().many_err()?;

                    println!("{}", result);
                }
            }
            Command::Highlight { .. } => {
                return Err(Diagnostic::fatal().with_message("operation not supported yet"))
                    .many_err()
            }
        }

        Ok(())
    })();

    if let Err(errors) = result {
        let amount = errors.len();

        if arguments.sort_diagnostics {
            let mut errors: Vec<_> = errors.into_iter().collect();
            errors.sort_by_key(|error| error.spans());

            for error in errors {
                error.emit(Some(&map));
            }
        } else {
            for error in errors {
                error.emit(Some(&map));
            }
        }

        Diagnostic::fatal()
            .with_message(pluralize(amount, "aborting due to previous error", || {
                format!("aborting due to {} previous errors", amount)
            }))
            .emit(Some(&map));

        // @Task instead of this using this function, return a std::process::ExitCode
        // from main once stable again. I am not sure but it could be that right now,
        // destructors are not run
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

        Diagnostic::bug().with_message(message).emit(None);
    }));
}
