//! The front-end concerned with lexing, parsing and lowering.

use crate::{
    diagnostics::Reporter,
    error::Result,
    span::{SharedSourceMap, SourceFileIndex},
};
pub use word::Word;

// @Beacon @Task if ast, lowered ast etc are pub(crate) anyway, make the individual AST nodes private as well!
pub(crate) mod ast;
pub mod lexer;
pub(crate) mod lowered_ast;
pub mod lowerer;
pub mod parser;
pub(crate) mod token;
pub(crate) mod word;

/// Lex and parse the file of a root module or an out-of-line module.
///
/// This is a convenience function combining [`lexer::lex`] and [`parser::parse_module_file`].
pub(crate) fn parse_module_file(
    file: SourceFileIndex,
    binder: ast::Identifier,
    map: SharedSourceMap,
    reporter: &Reporter,
) -> Result<ast::Declaration> {
    let tokens = lexer::lex(&map.borrow()[file], reporter)?.value;
    parser::parse_module_file(&tokens, file, binder, map, reporter)
}

// @Task try to get rid of file+map params (just take a &str (maybe, but what about span info?))!
pub(crate) fn parse_path(
    file: SourceFileIndex,
    map: SharedSourceMap,
    reporter: &Reporter,
) -> Result<ast::Path> {
    let tokens = lexer::lex(&map.borrow()[file], reporter)?.value;
    parser::parse_path(&tokens, file, map, reporter)
}
