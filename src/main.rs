#![forbid(rust_2018_idioms, unused_must_use)]

use lushuic::{
    diagnostic::{Diagnostic, Level},
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

        Diagnostic::new(Level::Bug, message).emit(None);
    }));

    let mut map = SourceMap::default();

    let result: Result<(), Diagnostic> = (|| {
        let mut arguments = std::env::args().skip(1);

        let path = arguments
            .next()
            .ok_or_else(|| Diagnostic::new(Level::Fatal, "no source file path supplied"))?;

        let mut lexer = Lexer::load(&mut map, path.into())
            .map_err(|error| Diagnostic::new(Level::Fatal, error.to_string()))?;
        lexer.lex()?;
        let tokens = lexer.into_tokens();
        // eprintln!("{:#?}", &tokens);

        let mut parser = Parser::new(&tokens);
        let node = parse_file_module_no_header(&mut parser)?;

        let node = node.desugar();
        // eprintln!("{}", &node);

        // @Temporary
        let _node = match node.resolve(&mut resolver::ModuleScope::default()) {
            Ok(node) => node,
            Err(errors) => {
                let amount = errors.len();

                for error in errors {
                    error.emit(Some(&mut map));
                }
                return Err(Diagnostic::new(
                    Level::Fatal,
                    format!("aborting due to {} previous errors", amount),
                ));
            }
        };

        eprintln!("{}", _node);

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
