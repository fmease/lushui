//! The front-end concerned with lexing, parsing and lowering.

pub mod ast;
pub mod lexer;
pub mod lowered_ast;
pub mod lowerer;
pub mod parser;
pub mod token;

pub use lexer::Lexer;
pub use lowerer::Lowerer;
pub use parser::Parser;

use crate::{
    diagnostics::Reporter,
    error::{outcome, Result},
    span::{SharedSourceMap, SourceFileIndex},
    utility::{obtain, Atom},
};
use token::TokenKind;

/// Lex and parse a given file module.
///
/// This is a convenience function combining [`Lexer::lex`] and [`Parser::parse`].
pub fn parse(
    file: SourceFileIndex,
    module_binder: ast::Identifier,
    map: SharedSourceMap,
    reporter: &Reporter,
) -> Result<ast::Declaration> {
    let tokens = Lexer::new(&map.borrow()[file], reporter).lex()?.value;
    Parser::new(file, &tokens, map, reporter).parse(module_binder)
}

/// Utility to parse identifiers from a string.
///
/// Used for non-lushui code like crate names.
pub fn parse_identifier(source: String) -> Option<Atom> {
    let outcome!(mut tokens, health) = lexer::lex(source).ok()?;

    if health.is_tainted() {
        return None;
    }

    let mut tokens = tokens.drain(..).map(|token| token.data);

    obtain!(
        (tokens.next()?, tokens.next()?),
        (TokenKind::Identifier(atom), TokenKind::EndOfInput) => atom
    )
}
