#![forbid(rust_2018_idioms, unused_must_use)]

use lushuic::{error::{self, DisplayWithSource}, lexer, parser, hir};

fn main() {
    // let source = "'let x: Int -> Int -> Int = memoize (\\x y => f x y)\n";
    // let source = "'let x: Int -> Int -> Int = 'let T = 'Type 'in identity T";
    // let source = "'let T = 'Type 'in identity T";
    // let source = "\\x => \\y => \\z => Unit'";
    // let source = "";
    let source = "\\a (,b: String) (c c' c'': Array Int) (d: Bool) e f g h: 'Type => Unit";

    if let Err(error) = test(source) {
        eprintln!("{}", error.display(source, None));
    }
}

fn test(source: &str) -> Result<(), error::Error> {
    let banner = "#".repeat(20);

    let tokens = lexer::lex(source)?;
    dbg!(&tokens);
    
    eprintln!("{}", banner);

    let mut context = parser::Context::new(&tokens);
    // let node = parser::parse_let_declaration(&mut context)?;
    let node = parser::parse_expression(&mut context)?;
    dbg!(&node);
    
    eprintln!("{}\n{}", banner, banner);

    eprintln!("source expression: {}", source);

    let node = hir::lower_expression(&node);
    eprintln!("lowered expression: {}", node.display_with(source));

    Ok(())
}
