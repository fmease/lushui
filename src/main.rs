#![forbid(rust_2018_idioms, unused_must_use)]

use lushuic::{
    desugar,
    diagnostic::Diagnostic,
    // interpreter, @Temporary
    lexer::Lexer,
    parser::{declaration::parse_file_module_no_header, Parser},
    resolver,
    span::SourceMap,
};

fn main() {
    // #[cfg(FALSE)]
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

        Diagnostic::bug(message, None).emit(None);
    }));

    let mut map = SourceMap::default();

    let result: Result<(), Diagnostic> = (|| {
        let mut arguments = std::env::args().skip(1);

        let path = arguments
            .next()
            .ok_or_else(|| Diagnostic::fatal("no source file path supplied".to_owned(), None))?;

        let mut lexer = Lexer::load(&mut map, path.into())
            .map_err(|error| Diagnostic::fatal(error.to_string(), None))?;
        lexer.lex()?;
        let tokens = lexer.into_tokens();
        // dbg!("{:#?}", &tokens);

        let mut parser = Parser::new(&tokens);
        let node = parse_file_module_no_header(&mut parser)?;

        let node = desugar::desugar_declaration(node);
        // eprintln!("{}", &node);

        // @Temporary
        let _node = match resolver::resolve_declaration(&mut resolver::Map::default(), node) {
            Ok(node) => node,
            Err(errors) => {
                let amount = errors.len();

                for error in errors {
                    error.emit(Some(&mut map));
                }
                return Err(Diagnostic::fatal(
                    format!("aborting due to {} previous errors", amount),
                    None,
                ));
            }
        };

        // @Temporary comment
        // let scope = interpreter::ModuleScope::new();
        // interpreter::evaluate_declaration(&node, scope.clone())
        //     .map_err(|error| Diagnostic::fatal(error.to_string(), None))?;
        // eprintln!("{:?}", scope);

        Ok(())
    })();

    if let Err(error) = result {
        error.emit(Some(&map));
    }
}
