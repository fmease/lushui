#![forbid(rust_2018_idioms, unused_must_use)]

use lushuic::{
    error::{self, DisplayWithSource},
    hir, lexer, parser,
};

fn main() {
    // let source = "'let x: Int -> Int -> Int = memoize (\\x y => f x y)\n";
    // let source = "'let x: Int -> Int -> Int = 'let T = 'Type 'in identity T";
    // let source = "'let T = 'Type 'in identity T";
    // let source = "\\x => \\y => \\z => Unit'";
    // let source = "";
    // let source = "\\a (,b: String) (c c' c'': Array Int) (d: Bool) e f g h: 'Type => Unit";
    // let source = r"'let compose (,A B C: 'Type) (f: (A) -> B) (g: (,z: B) -> C): A -> C = \x => g (f x)";
    // let source = r#"'let f (x: Int) (y: Text) (z: Bool): Bool = z 'in Tuple' (f a) (f b)"#;
    let source = "foo (,alpha) beta (,gamma)";

    if let Err(error) = test(source) {
        eprintln!("{}", error.display(source, None));
    }
}

fn test(source: &str) -> Result<(), error::Error> {
    let banner = "#".repeat(20);

    let tokens = lexer::lex(source)?;
    // dbg!(&tokens);
    eprintln!("{}", banner);

    let mut context = parser::Context::new(&tokens);
    // let node = parser::parse_declaration(&mut context)?;
    let node = parser::parse_expression(&mut context)?;
    dbg!(&node);
    eprintln!("{}\n{}", banner, banner);

    eprintln!("source: {}", source);

    let node = hir::lower_expression(&node);
    // let node = hir::lower_declaration(&node);
    eprintln!("lowered: {}", node.display_with(source));

    Ok(())
}
