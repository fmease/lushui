//! The front-end concerned with lexing, parsing and lowering.

use error::{Outcome, Result};
use session::BuildSession;
use span::SourceFileIndex;
use token::Token;

/// Lex source code into an array of tokens
///
/// The health of the tokens can be ignored if the tokens are fed into the parser
/// immediately after lexing since the parser will handle invalid tokens.
pub fn lex(file: SourceFileIndex, session: &BuildSession) -> Result<Outcome<Vec<Token>>> {
    lexer::lex(&session.shared_map()[file], session.reporter())
}

/// Lex and parse the file of a root module or an out-of-line module.
///
/// This is a convenience function combining [`lexer::lex`] and [`parser::parse_module_file`].
pub fn parse_module_file(
    file: SourceFileIndex,
    binder: ast::Identifier,
    session: &BuildSession,
) -> Result<ast::Declaration> {
    parser::parse_module_file(
        &lex(file, session)?.bare,
        file,
        binder,
        &session.shared_map(),
        session.reporter(),
    )
}

/// Parse the file of a root module / component root.
pub fn parse_root_module_file(
    tokens: &[Token],
    file: SourceFileIndex,
    session: &BuildSession,
) -> Result<ast::Declaration> {
    parser::parse_root_module_file(tokens, file, &session.shared_map(), session.reporter())
}

pub fn parse_path(file: SourceFileIndex, session: &BuildSession) -> Result<ast::Path> {
    parser::parse_path(
        &lex(file, session)?.bare,
        file,
        &session.shared_map(),
        session.reporter(),
    )
}
