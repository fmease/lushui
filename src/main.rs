#![forbid(rust_2018_idioms, unused_must_use)]

use lushuic::{effluvium, error, hir, lexer, parser};

fn main() {
    let source = "'let the (A: 'Type) (x: A): A = x";

    test(source).unwrap_or_else(|error| panic!("{}", error));
}

fn test(source: &str) -> Result<(), Box<dyn std::error::Error>> {
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
    let node = parser::parse_declaration(&mut context)
        .map_err(|error| error::Error::from(error).display(source, None))?;
    // let node = parser::parse_expression(&mut context)?;
    eprintln!("!!!! AST NODE !!!!");
    dbg!(&node);

    // HIR
    // let node = hir::lower_expression(&node);
    let node = hir::lower_declaration(&node);
    eprintln!("!!!! HIR NODE !!!!");
    eprintln!("lowered: {}", node);

    // Effluvium
    eprintln!("!!!! EFFLUVIUM !!!!");
    let (context, mut state) = effluvium::initial();
    // node.register(context, &mut state).unwrap_or_else(|error| panic!("{}", error));
    // effluvium::evaluate(node, context.clone(), &mut state)?; // @Task implement
    // let infered_type = node
    //     .infer_type(context.clone(), &mut state)
    //     .unwrap_or_else(|error| panic!("{}", error));
    // let value = node
    //     .normalize(context.clone(), &mut state)
    //     .unwrap_or_else(|error| panic!("{}", error));
    eprintln!("!!!! EFFLUVIUM STUFF !!!!");
    dbg!(context);

    Ok(())
}
