#![forbid(rust_2018_idioms, unused_must_use)]

use lushuic::{
    diagnostic::{Diagnostic, Level},
    interpreter,
    lexer::Lexer,
    parser::{declaration::parse_file_module_no_header, Parser},
    resolver,
    span::SourceMap,
};

fn main() {
    #[cfg(FALSE)]
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

        let file = map
            .load(path.into())
            .map_err(|error| Diagnostic::new(Level::Fatal, error.to_string()))?;

        let tokens = Lexer::new(&file).lex()?;
        // eprintln!("{:#?}", &tokens);

        let mut parser = Parser::new(&tokens);
        let node = parse_file_module_no_header(&mut parser)?;

        let node = node.desugar();
        eprintln!("{}", &node);

        // @Temporary
        let node = match node.resolve(&mut resolver::ModuleScope::default()) {
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
        eprintln!("{}", node);

        let mut scope = interpreter::ModuleScope::new();
        node.infer_type_and_evaluate(&mut scope)?;

        eprintln!("{:?}", scope);

        Ok(())
    })();

    if let Err(error) = result {
        error.emit(Some(&map));
    }
}
