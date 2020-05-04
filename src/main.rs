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

struct Arguments<'a> {
    tokens: bool,
    ast: bool,
    hir: bool,
    resolved_hir: bool,
    scope: bool,
    file_path: &'a str,
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

    // @Task gather all print arguments under a common --print=THING argument
    let matches = App::new(lushui::NAME)
        .version(lushui::VERSION)
        .about(lushui::DESCRIPTION)
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
        file_path: matches.value_of(flags::FILE).unwrap().into(),
    };

    let mut map = SourceMap::default();

    let result: Result<(), Diagnostics> = (|| {
        let file_path = Path::new(arguments.file_path);
        let file = map.load(arguments.file_path).many_err()?;
        let file_stem = file_path.file_stem().unwrap();
        let file_extension = file_path.extension();

        if file_extension.and_then(|extension| extension.to_str()) != Some(lushui::FILE_EXTENSION) {
            Diagnostic::new(
                Level::Warning,
                None,
                "missing or non-standard file extension",
            )
            .emit(None);
        }

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

        let node = node.desugar(&mut map)?;
        if arguments.hir {
            println!("{}", node);
        }

        let mut resolver_scope = resolver::CrateScope::default();

        let node = node.resolve(None, &mut resolver_scope)?;
        if arguments.resolved_hir {
            eprintln!("{}", node);
        } else {
            // @Temporary see note below
            eprintln!("the resolver succeeded");
        }

        // @Beacon @Temporary we are working on the resolver
        // the type checker won't handle the new system yet
        if false {
            let mut scope = interpreter::CrateScope::new();
            node.infer_type(&mut scope).many_err()?;
            if arguments.scope {
                eprintln!("{:?}", scope);
            }

            if let Some(program_entry) = resolver_scope.program_entry {
                let result =
                    interpreter::evaluate_program_entry(program_entry, &scope).many_err()?;

                println!("{}", result);
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
