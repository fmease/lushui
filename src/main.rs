#![forbid(rust_2018_idioms, unused_must_use)]

use lushuic::{error, hir, interpreter, lexer, parser};

use std::fs::File;
use std::io::{BufReader, Read};

fn main() {
    // @Task improve error handling
    let mut arguments = std::env::args().skip(1);
    let source_path = arguments.next().expect("no source file path supplied");

    let file = File::open(&source_path).unwrap();
    let mut buf_reader = BufReader::new(file);
    let mut source = String::new();
    buf_reader.read_to_string(&mut source).unwrap();

    drop(buf_reader);

    if let Err(error) = test(&source, &source_path) {
        eprintln!("{}", error);
    }
}

fn test(source: &str, filename: &str) -> Result<(), String> {
    // lexing
    let tokens = lexer::lex(source).map_err(|error| {
        error::display(&error.kind.to_string(), error.span, source, Some(filename))
    })?;
    // dbg!("{:#?}", &tokens);

    // parsing
    let mut context = parser::Context::new(&tokens);
    let node = parser::Declaration::Module(Box::new(
        parser::declaration::parse_file_module_no_header(&mut context).map_err(|error| {
            error::display(&error.kind.to_string(), error.span, source, Some(filename))
        })?,
    ));
    // eprintln!("{:#?}", &node);

    // lowering to intermediate representation
    let node = hir::lower_declaration(node);
    // eprintln!("{}", &node);

    // type checking and interpreting
    let scope = interpreter::ModuleScope::new();
    interpreter::evaluate_declaration(&node, scope.clone()).map_err(|error| error.to_string())?;
    eprintln!("{:?}", scope);

    Ok(())
}
