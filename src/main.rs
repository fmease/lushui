#![forbid(rust_2018_idioms, unused_must_use)]

use lushuic::{effluvium, error, hir, lexer, parser};

fn main() {
    // let source = "'let the (A: 'Type) (x: A): A = x";
    let source = r"\(A: 'Type) (x: A): A => x";

    test(source).unwrap_or_else(|error| panic!("{}", error));
}

fn test(source: &str) -> Result<(), String> {
    eprintln!("!!!! SOURCE !!!!");
    eprintln!("{}", source);

    // tokens
    let tokens =
        lexer::lex(source).map_err(|error| error::Error::from(error).display(source, None))?;
    // eprintln!("!!!! TOKENS !!!!");
    // dbg!(&tokens);
    // print_banner();

    // AST
    let mut context = parser::Context::new(&tokens);
    // let node = parser::parse_declaration(&mut context)
    //     .map_err(|error| error::Error::from(error).display(source, None))?;
    let node = parser::parse_expression(&mut context)
        .map_err(|error| error::Error::from(error).display(source, None))?;
    eprintln!("!!!! AST NODE !!!!");
    dbg!(&node);

    // HIR
    let node = hir::lower_expression(&node);
    // let node = hir::lower_declaration(&node);
    eprintln!("!!!! HIR NODE !!!!");
    eprintln!("lowered: {}", node);

    // Effluvium
    eprintln!("!!!! EFFLUVIUM !!!!");
    let (context, mut state) = effluvium::initial();
    let infered_type = effluvium::infer_type(&node, context.clone(), &mut state)
        .map_err(|error| format!("{}", error))?;
    eprintln!("infered_type: {}", infered_type);
    let value = effluvium::normalize(&node, context.clone(), &mut state)
        .map_err(|error| format!("{}", error))?;
    eprintln!("value: {}", value);
    // effluvium::evaluate(node, context.clone(), &mut state)?; // @Task implement
    dbg!(context);

    Ok(())
}
