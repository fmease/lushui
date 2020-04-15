#![forbid(rust_2018_idioms, unused_must_use)]

use lushui::{
    diagnostic::{Diagnostic, Level, Result},
    interpreter,
    lexer::Lexer,
    parser::Parser,
    resolver,
    span::SourceMap,
};

const NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = concat!(env!("CARGO_PKG_VERSION"), " ", env!("GIT_COMMIT_HASH"));
const DESCRIPTION: &str = env!("CARGO_PKG_DESCRIPTION");

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
                .help("Sets the source file"),
        )
        .get_matches();

    let mut map = SourceMap::default();

    let result: Result<()> = (|| {
        let source_file_path = matches.value_of(flags::FILE).unwrap();

        let file = map
            .load(source_file_path)
            .map_err(|error| Diagnostic::new(Level::Fatal, None, error.to_string()))?;

        let tokens = handle_multiple_errors(&map, Lexer::new(&file).lex())?;
        if matches.is_present(flags::TOKENS) {
            println!("{:#?}", tokens);
        }

        let mut parser = Parser::new(&tokens);
        let node = parser.parse_file_module_no_header(&file.name)?;
        // @Beacon @Task use parser.session.module_files (better name plz) to load more file
        if matches.is_present(flags::AST) {
            println!("{:?}", node);
        }

        let node = handle_multiple_errors(&map, node.desugar())?;
        if matches.is_present(flags::HIR) {
            println!("{}", node);
        }

        let node =
            handle_multiple_errors(&map, node.resolve(&mut resolver::ModuleScope::default()))?;
        if matches.is_present(flags::RESOLVED_HIR) {
            eprintln!("{}", node);
        }

        let mut scope = interpreter::ModuleScope::new();
        node.infer_type_and_evaluate(&mut scope)?;

        eprintln!("{:?}", scope);

        Ok(())
    })();

    if let Err(error) = result {
        error.emit(Some(&map));
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

// @Temporary use an error buffer in general! @Note does not report actual number of errors!
fn handle_multiple_errors<T>(
    map: &SourceMap,
    result: Result<T, impl IntoIterator<Item = Diagnostic>>,
) -> Result<T> {
    match result {
        Ok(value) => Ok(value),
        Err(errors) => {
            let errors = errors.into_iter();
            let (amount, _) = errors.size_hint();

            for error in errors {
                error.emit(Some(map));
            }
            Err(Diagnostic::new(
                Level::Fatal,
                None,
                format!("aborting due to {} previous errors", amount),
            ))
        }
    }
}
