#![forbid(rust_2018_idioms, unused_must_use)]

use clap::{App, Arg};
use lushui::{
    diagnostic::{Diagnostic, Diagnostics, Level, Result},
    interpreter,
    lexer::{parse_identifier, Lexer},
    parser::{Identifier, Parser},
    resolver,
    span::{SourceMap, Span},
    support::ManyErrExt,
};
use std::{borrow::Cow, path::Path};

const NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = concat!(env!("CARGO_PKG_VERSION"), " ", env!("GIT_COMMIT_HASH"));
const DESCRIPTION: &str = env!("CARGO_PKG_DESCRIPTION");

struct Arguments<'a> {
    tokens: bool,
    ast: bool,
    hir: bool,
    resolved_hir: bool,
    scope: bool,
    file: &'a str,
}

mod flags {
    pub const TOKENS: &str = "tokens";
    pub const AST: &str = "ast";
    pub const HIR: &str = "hir";
    pub const RESOLVED_HIR: &str = "resolved-hir";
    pub const SCOPE: &str = "scope";
    pub const FILE: &str = "FILE";
}

fn main() {
    // #[cfg(FALSE)]
    set_panic_hook();

    let matches = App::new(NAME)
        .version(VERSION)
        .about(DESCRIPTION)
        .arg(
            Arg::with_name(flags::TOKENS)
                .long(flags::TOKENS)
                .help("Print the tokens emitted by the lexer"),
        )
        .arg(
            Arg::with_name(flags::AST)
                .long(flags::AST)
                .help("Print the AST"),
        )
        .arg(
            Arg::with_name(flags::HIR)
                .long(flags::HIR)
                .help("Print the HIR emitted after desugaring"),
        )
        .arg(
            Arg::with_name(flags::RESOLVED_HIR)
                .long(flags::RESOLVED_HIR)
                .help("Print the HIR emitted by the resolver"),
        )
        .arg(
            Arg::with_name(flags::SCOPE)
                .long(flags::SCOPE)
                .short("s")
                .help("Print the evaluated module scope"),
        )
        .arg(
            Arg::with_name(flags::FILE)
                .required(true)
                .help("Set the source file"),
        )
        .get_matches();

    let arguments = Arguments {
        tokens: matches.is_present(flags::TOKENS),
        ast: matches.is_present(flags::AST),
        hir: matches.is_present(flags::HIR),
        resolved_hir: matches.is_present(flags::RESOLVED_HIR),
        scope: matches.is_present(flags::SCOPE),
        file: matches.value_of(flags::FILE).unwrap().into(),
    };

    let mut map = SourceMap::default();

    let result: Result<(), Diagnostics> = (|| {
        let file = map.load(arguments.file).many_err()?;
        let file_stem = Path::new(arguments.file).file_stem().unwrap();

        let crate_name = Identifier::new(
            (|| parse_identifier(file_stem.to_str()?.to_owned()))().ok_or_else(|| {
                vec![Diagnostic::new(
                    Level::Fatal,
                    None,
                    format!(
                        "`{}` is not a valid crate name",
                        file_stem.to_string_lossy()
                    ),
                )]
            })?,
            Span::DUMMY,
        );

        let tokens = Lexer::new(&file).lex()?;
        if arguments.tokens {
            println!("{:#?}", tokens);
        }

        let node = Parser::new(file, &tokens)
            .parse_top_level(crate_name.clone())
            .many_err()?;
        if arguments.ast {
            println!("{:?}", node);
        }

        let node = node.desugar()?;
        if arguments.hir {
            println!("{}", node);
        }

        let mut krate = resolver::Crate::new(crate_name);

        let node = node.resolve(krate.root(), &mut krate, &mut map)?;
        if arguments.resolved_hir {
            eprintln!("{}", node);
        }

        let mut scope = interpreter::ModuleScope::new();
        node.infer_type_and_evaluate(&mut scope).many_err()?;
        if arguments.scope {
            eprintln!("{:?}", scope);
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
            match amount {
                1 => Cow::from("aborting due to previous error"),
                amount => format!("aborting due to {} previous errors", amount).into(),
            },
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
