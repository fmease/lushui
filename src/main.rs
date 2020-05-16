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

// @Task gather all print flags under a common --print=THING option
#[derive(StructOpt)]
#[structopt(version = VERSION, author, about)]
struct Arguments {
    /// Print the tokens emitted by the lexer
    #[structopt(long)]
    print_tokens: bool,

    /// Print the AST
    #[structopt(long)]
    print_ast: bool,

    /// Print the HIR emitted after desugaring
    #[structopt(long)]
    print_hir: bool,

    /// Display crate indices when emitting the resolved HIR
    #[structopt(long)]
    display_crate_indices: bool,

    /// Print the HIR emitted by the resolver
    #[structopt(long)]
    print_hir_resolved: bool,

    /// Print the evaluated module scope
    #[structopt(long)]
    print_scope: bool,

    /// Use rustc panic hook with RUST_BACKTRACE=1
    #[structopt(long, short = "B")]
    panic_with_backtrace: bool,

    #[structopt(subcommand)]
    command: Command,
}

#[derive(StructOpt)]
enum Command {
    /// Type check a given program
    Check {
        /// Set the source file
        #[structopt(name = "FILE")]
        file: String,
    },
    /// Type check and run a given program
    Run {
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

impl Command {
    fn file(&self) -> &str {
        match self {
            Self::Check { file, .. } => file,
            Self::Run { file, .. } => file,
            Self::Highlight { file, .. } => file,
        }
    }
}

fn main() {
    let arguments = Arguments::from_args();

    if arguments.panic_with_backtrace {
        std::env::set_var("RUST_BACKTRACE", "1");
    } else {
        set_panic_hook();
    }

    lushui::OPTIONS
        .set(lushui::Options {
            display_crate_indices: arguments.display_crate_indices,
        })
        .unwrap_or_else(|_| unreachable!());

    let mut map = SourceMap::default();

    let result: Result<(), Diagnostics> = (|| {
        let file = arguments.command.file();
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
        if arguments.print_tokens {
            eprintln!("{:#?}", tokens);
        }

        let node = Parser::new(file, &tokens)
            .parse_top_level(crate_name.clone())
            .many_err()?;
        if arguments.print_ast {
            eprintln!("{:?}", node);
        }

        match arguments.command {
            Command::Check { .. } | Command::Run { .. } => {
                let node = node.desugar(&mut map)?;
                if arguments.print_hir {
                    eprintln!("{}", node);
                }

                let mut scope = resolver::CrateScope::default();

                let node = node.resolve(&mut scope)?;
                if arguments.print_hir_resolved {
                    eprintln!("{}", node);
                }

                let mut scope = interpreter::CrateScope::new(scope);
                node.infer_type(&mut scope).many_err()?;
                if arguments.print_scope {
                    eprintln!("{:?}", scope);
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
