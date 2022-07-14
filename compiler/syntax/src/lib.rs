//! The front-end concerned with lexing, parsing and lowering.

use lushui_error::Result;
use lushui_session::BuildSession;
use lushui_span::SourceFileIndex;

/// Lex and parse the file of a root module or an out-of-line module.
///
/// This is a convenience function combining [`lexer::lex`] and [`parser::parse_module_file`].
pub fn parse_module_file(
    file: SourceFileIndex,
    binder: lushui_ast::Identifier,
    session: &BuildSession,
) -> Result<lushui_ast::Declaration> {
    lushui_parser::parse_module_file(&lushui_lexer::lex(file, session)?.bare, file, binder, session)
}

pub fn parse_path(
    file: SourceFileIndex,
    session: &BuildSession,
) -> Result<lushui_ast::Path> {
    lushui_parser::parse_path(&lushui_lexer::lex(file, session)?.bare, file, session)
}
