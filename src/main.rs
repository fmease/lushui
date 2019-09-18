#![forbid(bare_trait_objects, unused_must_use)]

use lushuic::{error, lexer, parser};

fn main() {
    // let input = include_str!("test.lushui");

    // match lexer::lex(input) {
    //     Ok(tokens) => {
    //         for token in tokens {
    //             print!(
    //                 r#"({:?} {:?} "{}") "#,
    //                 token.kind,
    //                 token.span.clone(),
    //                 input[token.span].escape_debug()
    //             );
    //         }
    //         println!();
    //     }
    //     Err(error) => {
    //         eprintln!("{}", error);
    //     }
    // };

    //     let source = "
    // 'let foo (A B: 'Googol) {X: Xoo}: Ball = Bank
    // ";
    // let source = include_str!("test.lushui");

    // let source = "'let x: Int -> Int -> Int = memoize (\\x y => f x y)\n";
    // let source = "'let x: Int -> Int -> Int = 'let T = 'Type 'in identity T";
    // let source = "'let T = 'Type 'in identity T";
    // let source = "\\a {b: String} (c c' c'': Array Int) (d: Bool) e f g h: 'Type => Unit";
    // let source = "\\x => \\y => \\z => Unit'";

    let source = "";

    if let Err(error) = test(source) {
        eprintln!("{}", error.display(source, None));
    }
}

fn test(source: &str) -> Result<(), error::Error> {
    let tokens = lexer::lex(source)?;
    dbg!(&tokens);
    let mut context = parser::Context::new(&tokens);
    // let node = parser::parse_let_declaration(&mut context)?;
    let node = parser::parse_expression(&mut context)?;
    let _ = dbg!(node);
    Ok(())
}
