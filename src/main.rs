#![forbid(rust_2018_idioms, unused_must_use)]

use std::path::Path;
use structopt::StructOpt;

use lushui::{
    diagnostic::*,
    interpreter,
    lexer::{parse_identifier, Lexer},
    parser::{Identifier, Parser},
    resolver,
    span::{SourceMap, Span},
    support::{pluralize, ManyErrExt},
};

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

    #[structopt(subcommand)]
    command: Command,
}

impl Arguments {
    // @Note what a cruel poilerplate :'(
    fn merged(&self) -> MergedCommandArguments<'_> {
        let mut arguments = MergedCommandArguments::default();

        match &self.command {
            Command::Check {
                file,
                print_tokens,
                print_ast,
                print_desugar_hir,
                print_interpreter_scope,
                print_resolver_hir,
                print_resolver_scope,
                display_crate_indices,
            } => {
                arguments.file = file;
                arguments.print_tokens = *print_tokens;
                arguments.print_ast = *print_ast;
                arguments.print_desugar_hir = *print_desugar_hir;
                arguments.print_interpreter_scope = *print_interpreter_scope;
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
    print_tokens: bool,
    print_ast: bool,
    print_desugar_hir: bool,
    display_crate_indices: bool,
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
        /// Print the tokens emitted by the lexer
        #[structopt(long)]
        print_tokens: bool,

        /// Print the AST
        #[structopt(long)]
        print_ast: bool,

        /// Print the HIR emitted after desugaring
        #[structopt(long)]
        print_desugar_hir: bool,

        /// Display crate indices when emitting a resolved HIR
        #[structopt(long)]
        display_crate_indices: bool,

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

    let result: Result<(), Diagnostics> = (|| {
        let file = merged_arguments.file;
        let path = Path::new(file);
        let file = map.load(file).many_err()?;
        let file_stem = path.file_stem().unwrap();
        let file_extension = path.extension();

        if file_extension.and_then(|extension| extension.to_str()) != Some(lushui::FILE_EXTENSION) {
            Diagnostic::new(
                Level::Warning,
                None,
                "missing or non-standard file extension",
            )
            .emit(None);
        }

        let crate_name = Identifier::new(
            (|| parse_identifier(file_stem.to_str()?.to_owned()))()
                .ok_or_else(|| {
                    Diagnostic::new(
                        Level::Fatal,
                        None,
                        format!(
                            "`{}` is not a valid crate name",
                            file_stem.to_string_lossy()
                        ),
                    )
                })
                .many_err()?,
            Span::SHAM,
        );

        let tokens = Lexer::new(&file).lex()?;
        if merged_arguments.print_tokens {
            eprintln!("{:#?}", tokens);
        }

        let node = Parser::new(file, &tokens)
            .parse_top_level(crate_name.clone())
            .many_err()?;
        if merged_arguments.print_ast {
            eprintln!("{:?}", node);
        }

        match arguments.command {
            Command::Check { .. } | Command::Run { .. } => {
                let node = node.desugar(&mut map)?;
                if merged_arguments.print_desugar_hir {
                    eprintln!("{}", node);
                }

                let mut scope = resolver::CrateScope::default();

                let node = node.resolve(&mut scope)?;
                if merged_arguments.print_resolver_hir {
                    eprintln!("{}", node);
                }
                if merged_arguments.print_resolver_scope {
                    eprintln!("{:#?}", scope);
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
                return Err(Diagnostic::new(
                    Level::Fatal,
                    None,
                    "operation not supported yet",
                ))
                .many_err()
            }
        }

        Ok(())
    })();

    if let Err(errors) = result {
        let amount = errors.len();

        for error in errors {
            error.emit(Some(&map));
        }

        Diagnostic::new(
            Level::Fatal,
            None,
            pluralize(amount, "aborting due to previous error", || {
                format!("aborting due to {} previous errors", amount)
            }),
        )
        .emit(Some(&map));
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

        Diagnostic::new(Level::Bug, None, message).emit(None);
    }));
}
