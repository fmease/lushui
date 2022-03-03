//! The front-end concerned with lexing, parsing and lowering.

use crate::{error::Result, session::BuildSession, span::SourceFileIndex};
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
    session: &BuildSession,
) -> Result<ast::Declaration> {
    parser::parse_module_file(&lexer::lex(file, session)?.value, file, binder, session)
}

pub(crate) fn parse_path(file: SourceFileIndex, session: &BuildSession) -> Result<ast::Path> {
    parser::parse_path(&lexer::lex(file, session)?.value, file, session)
}
