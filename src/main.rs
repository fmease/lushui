#![forbid(rust_2018_idioms, unused_must_use)]

use lushui::{
    diagnostic::{Diagnostic, Level, Result},
    interpreter,
    lexer::Lexer,
    parser::Parser,
    resolver,
    span::SourceMap,
    support::ManyExt,
};

const NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = concat!(env!("CARGO_PKG_VERSION"), " ", env!("GIT_COMMIT_HASH"));
const DESCRIPTION: &str = env!("CARGO_PKG_DESCRIPTION");

struct Arguments<'a> {
    tokens: bool,
    ast: bool,
    hir: bool,
    resolved_hir: bool,
    file: &'a str,
}

mod flags {
    pub const TOKENS: &str = "tokens";
    pub const AST: &str = "ast";
    pub const HIR: &str = "hir";
    pub const RESOLVED_HIR: &str = "resolved-hir";
    pub const FILE: &str = "FILE";
}

fn main() {
    // #[cfg(FALSE)]
    set_panic_hook();

    use clap::{App, Arg};

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
        file: matches.value_of(flags::FILE).unwrap().into(),
    };

    let mut map = SourceMap::default();

    let result: Result<(), Vec<Diagnostic>> = (|| {
        let file = map.load(arguments.file).many()?;

        let tokens = Lexer::new(&file).lex()?;
        if arguments.tokens {
            println!("{:#?}", tokens);
        }

        let node = Parser::new(file, &tokens)
            .parse_file_module_no_header()
            .many()?;
        if arguments.ast {
            println!("{:?}", node);
        }

        let node = node.desugar()?;
        if arguments.hir {
            println!("{}", node);
        }

        let node = node.resolve(&mut resolver::ModuleScope::default(), &mut map)?;
        if arguments.resolved_hir {
            eprintln!("{}", node);
        }

        let mut scope = interpreter::ModuleScope::new();
        node.infer_type_and_evaluate(&mut scope).many()?;

        eprintln!("{:?}", scope);

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
            format!("aborting due to {} previous errors", amount),
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
