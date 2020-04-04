#![forbid(rust_2018_idioms, unused_must_use)]

use lushui::{
    diagnostic::{Diagnostic, Level},
    interpreter,
    lexer::Lexer,
    parser::Parser,
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

        Diagnostic::new(Level::Bug, None, message).emit(None);
    }));

    let mut map = SourceMap::default();

    let result: Result<(), Diagnostic> = (|| {
        let mut arguments = std::env::args().skip(1);

        let path = arguments
            .next()
            .ok_or_else(|| Diagnostic::new(Level::Fatal, None, "no source file path supplied"))?;

        let file = map
            .load(path.into())
            .map_err(|error| Diagnostic::new(Level::Fatal, None, error.to_string()))?;

        let tokens = handle_multiple_errors(&map, Lexer::new(&file).lex())?;
        // eprintln!("{:#?}", &tokens);

        let node = Parser::new(&tokens).parse_file_module_no_header()?;

        let node = node.desugar()?;
        eprintln!("{}", &node);

        let node =
            handle_multiple_errors(&map, node.resolve(&mut resolver::ModuleScope::default()))?;
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

// @Temporary use an error buffer in general! @Note does not report actual number of errors!
fn handle_multiple_errors<T>(
    map: &SourceMap,
    result: Result<T, impl IntoIterator<Item = Diagnostic>>,
) -> Result<T, Diagnostic> {
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
