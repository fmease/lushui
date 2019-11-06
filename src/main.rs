#![forbid(rust_2018_idioms, unused_must_use)]

use lushuic::{effluvium, error, hir, lexer, parser};

use std::fs::File;
use std::io::{BufReader, Read};



fn main() {
    // @Task improve error handling
    let mut arguments = std::env::args().skip(1);
    let source_path = arguments.next().expect("no source file path supplied");

    let file = File::open(source_path).unwrap();
    let mut buf_reader = BufReader::new(file);
    let mut source = String::new();
    buf_reader.read_to_string(&mut source).unwrap();

    test(&source).unwrap_or_else(|error| panic!("{}", error));
}

fn test(source: &str) -> Result<(), String> {
    // tokens
    let tokens =
        lexer::lex(source).map_err(|error| error::Error::from(error).display(source, None))?;

    // AST
    let mut context = parser::Context::new(&tokens);
    let node = parser::parse_file_module_no_header(&mut context)
        .map_err(|error| error::Error::from(error).display(source, None))?;

    // HIR
    let node = hir::lower_declaration(&node);

    // Effluvium
    let (context, mut state) = effluvium::initial();
    effluvium::evaluate_declaration(&node, context.clone(), &mut state)
        .map_err(|error| error.to_string())?;
    eprintln!("{}", context);

    Ok(())
}
