//! The front-end concerned with lexing, parsing and lowering.
use diagnostics::error::Result;
use session::Session;
use span::SrcFileIdx;
use utility::default;

/// Lex source code into an array of tokens
///
/// The health of the tokens can be ignored if the tokens are fed into the parser
/// immediately after lexing since the parser will handle invalid tokens.
pub fn lex(file: SrcFileIdx, sess: &Session<'_>) -> lexer::Outcome {
    lexer::lex(&sess.shared_map()[file], &default())
}

/// Lex and parse the file of a root module or an out-of-line module.
///
/// This is a convenience function combining [`lexer::lex`] and [`parser::parse_module_file`].
pub fn parse_module_file(
    file: SrcFileIdx,
    binder: ast::Ident,
    sess: &Session<'_>,
) -> Result<ast::Decl> {
    parser::parse_module_file(
        lex(file, sess),
        file,
        binder,
        &sess.shared_map(),
        sess.rep(),
    )
}

/// Parse the file of a root module / component root.
pub fn parse_root_module_file(
    tokens: lexer::Outcome,
    file: SrcFileIdx,
    sess: &Session<'_>,
) -> Result<ast::Decl> {
    parser::parse_root_module_file(tokens, file, &sess.shared_map(), sess.rep())
}

pub fn parse_path(file: SrcFileIdx, sess: &Session<'_>) -> Result<ast::Path> {
    parser::parse_path(lex(file, sess), file, &sess.shared_map(), sess.rep())
}
