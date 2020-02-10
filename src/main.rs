#![forbid(rust_2018_idioms, unused_must_use)]

use lushuic::{hir, interpreter, lexer, parser};

fn main() {
    // @Task improve error handling
    let mut arguments = std::env::args().skip(1);
    let path = arguments.next().expect("no source file path supplied");

    let mut map = lushuic::span::SourceMap::default();
    let mut lexer = lexer::Lexer::load(&mut map, std::path::Path::new(&path)).unwrap();
    // @Temporary error handling until we implement span_to_snippet and other Diagnostic display components
    lexer.lex().unwrap_or_else(|_| panic!());

    let tokens = lexer.into_tokens();
    dbg!("{:#?}", &tokens);

    let mut parser = parser::Parser::new(&tokens);
    // @Temporary error handling until we implement span_to_snippet and other Diagnostic display components
    let node = parser::Declaration::Module(Box::new(
        parser::declaration::parse_file_module_no_header(&mut parser).unwrap_or_else(|_| panic!()),
    ));

    let node = hir::lower_declaration(node);
    // eprintln!("{}", &node);

    // type checking and interpreting
    let scope = interpreter::ModuleScope::new();
    // @Temporary error handling until we implement span_to_snippet and other Diagnostic display components
    interpreter::evaluate_declaration(&node, scope.clone())
        .map_err(|error| error.to_string())
        .unwrap_or_else(|_| panic!());
    eprintln!("{:?}", scope);
}
