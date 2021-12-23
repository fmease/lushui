//! The front-end concerned with lexing, parsing and lowering.

pub(crate) mod ast;
pub mod lexer;
pub(crate) mod lowered_ast;
pub mod lowerer;
pub mod parser;
pub(crate) mod token;

pub use crate_name::CrateName;

use crate::{
    diagnostics::Reporter,
    error::Result,
    span::{SharedSourceMap, SourceFileIndex},
};

/// Lex and parse a given file module.
///
/// This is a convenience function combining [`lexer::lex`] and [`parser::parse`].
pub(crate) fn parse(
    file: SourceFileIndex,
    module: ast::Identifier,
    map: SharedSourceMap,
    reporter: &Reporter,
) -> Result<ast::Declaration> {
    let tokens = lexer::lex(&map.borrow()[file], reporter)?.value;
    parser::parse(&tokens, file, module, map, reporter)
}

pub(crate) mod crate_name {
    use super::{ast::Identifier, lexer, token::TokenKind};
    use crate::{
        diagnostics::{Code, Diagnostic},
        error::outcome,
        span::{Spanned, Spanning},
        utility::{obtain, Atom},
    };
    use std::fmt;

    const CORE_PACKAGE_NAME: &str = "core";

    #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
    pub struct CrateName(Atom);

    impl CrateName {
        pub fn parse(name: &str) -> Result<Self, Diagnostic> {
            match parse_word(name.into()) {
                Some(identifier) => Ok(Self(identifier)),
                None => Err(Diagnostic::invalid_crate_name(name)),
            }
        }

        pub(crate) fn parse_spanned(
            Spanned { value: name, span }: Spanned<&str>,
        ) -> Result<Spanned<Self>, Diagnostic> {
            Self::parse(name)
                .map(|name| Spanned::new(span, name))
                .map_err(|error| error.primary_span(span))
        }

        pub(crate) fn from_identifier(identifier: Identifier) -> Result<Spanned<Self>, Diagnostic> {
            if identifier.is_word() {
                Ok(Spanned::new(
                    identifier.span(),
                    Self(identifier.into_atom()),
                ))
            } else {
                Err(Diagnostic::invalid_crate_name(identifier.as_str()))
            }
        }

        pub(crate) fn core_package_name() -> Self {
            Self(CORE_PACKAGE_NAME.into())
        }

        pub fn as_str(&self) -> &str {
            &self.0
        }
    }

    impl fmt::Display for CrateName {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.as_str())
        }
    }

    impl From<Spanned<CrateName>> for Identifier {
        fn from(name: Spanned<CrateName>) -> Self {
            Self::new_unchecked(name.value.0, name.span)
        }
    }

    impl Diagnostic {
        fn invalid_crate_name(name: &str) -> Self {
            Self::error()
                .code(Code::E036)
                .message(format!("`{name}` is not a valid crate name"))
        }
    }

    // @Beacon @Beacon @Beacon @Note might need to move! (once we have Word)
    fn parse_word(source: String) -> Option<Atom> {
        let outcome!(mut tokens, health) = lexer::lex_string(source).ok()?;

        if health.is_tainted() {
            return None;
        }

        let mut tokens = tokens.drain(..).map(|token| token.value);

        obtain!(
            (tokens.next()?, tokens.next()?),
            (TokenKind::Word(atom), TokenKind::EndOfInput) => atom
        )
    }
}
