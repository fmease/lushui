#![forbid(rust_2018_idioms, unused_must_use)]

use lushuic::{effluvium, error, hir, lexer, parser};

fn main() {
    // let source = "'let x: Int -> Int -> Int = memoize (\\x y => f x y)\n";
    // let source = "'let x: Int -> Int -> Int = 'let T = 'Type 'in identity T";
    // let source = "'let T = 'Type 'in identity T";
    // let source = "\\x => \\y => \\z => Unit'";
    // let source = "";
    // let source = "\\a (,b: String) (c c' c'': Array Int) (d: Bool) e f g h: 'Type => Unit";
    // let source = r"'let compose (,A B C: 'Type) (f: (A) -> B) (g: (,z: B) -> C): A -> C = \x => g (f x)";
    // let source = r#"'let f (x: Int) (y: Text) (z: Bool): Bool = z 'in Tuple' (f a) (f b)"#;
    // let source = "alpha -> beta; gamma";

    let source = "'let the (A: 'Type) (x: A): A = x";
    // let source = r"\(A: 'Type) (x: A): A => x";

    if let Err(error) = test(source) {
        eprintln!("{}", error.display(source, None));
    }
}

fn test(source: &str) -> Result<(), error::Error> {
    eprintln!("!!!! SOURCE !!!!");
    eprintln!("{}", source);
    print_banner();

    // tokens
    let tokens = lexer::lex(source)?;
    // eprintln!("!!!! TOKENS !!!!");
    // dbg!(&tokens);
    // print_banner();

    // AST
    let mut context = parser::Context::new(&tokens);
    let node = parser::parse_declaration(&mut context)?;
    // let node = parser::parse_expression(&mut context)?;
    eprintln!("!!!! AST NODE !!!!");
    dbg!(&node);
    print_banner();

    // HIR
    // let node = hir::lower_expression(&node);
    let node = hir::lower_declaration(&node);
    eprintln!("!!!! HIR NODE !!!!");
    eprintln!("lowered: {}", node);
    print_banner();

    // Effluvium
    let node = effluvium::Declaration::from_hir(node);
    // let node = effluvium::Expr::from_hir(node);
    eprintln!("!!!! EFFLUVIUM NODE !!!!");
    // dbg!(&node);
    eprintln!("{}", node);
    print_banner();
    let (context, mut state) = effluvium::initial();
    // node.register(context, &mut state).unwrap_or_else(|error| panic!("{}", error));
    node.evaluate(context.clone(), &mut state)
        .unwrap_or_else(|error| panic!("{}", error));
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

// @Temporary
fn print_banner() {
    eprintln!("{0}\n{0}", "#".repeat(20));
}
