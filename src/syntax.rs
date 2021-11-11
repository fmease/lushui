//! The front-end concerned with lexing, parsing and lowering.

pub mod ast;
pub mod lexer;
pub mod lowered_ast;
pub mod lowerer;
pub mod parser;
pub mod token;

pub use crate_name::CrateName;
pub use lexer::Lexer;
pub use lowerer::Lowerer;
pub use parser::Parser;

use crate::{
    diagnostics::Reporter,
    error::Result,
    span::{SharedSourceMap, SourceFileIndex},
};

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

pub mod crate_name {
    use super::{ast::Identifier, lexer, token::TokenKind};
    use crate::{
        diagnostics::{Code, Diagnostic},
        error::outcome,
        metadata::Key,
        span::{Spanned, Spanning},
        utility::{obtain, Atom},
    };
    use std::fmt;

    const CORE_PACKAGE_NAME: &str = "core";

    #[derive(Clone, PartialEq, Eq, Hash, Debug)]
    pub struct CrateName(Atom);

    impl CrateName {
        pub fn parse(name: &str) -> Result<Self, Diagnostic> {
            match parse_word(name.into()) {
                Some(identifier) => Ok(Self(identifier)),
                None => Err(Diagnostic::invalid_crate_name(name)),
            }
        }

        pub fn parse_spanned(
            Spanned { value: name, span }: Spanned<&str>,
        ) -> Result<Spanned<Self>, Diagnostic> {
            Self::parse(name)
                .map(|name| Spanned::new(span, name))
                .map_err(|error| error.primary_span(span))
        }

        pub fn from_identifier(identifier: Identifier) -> Result<Spanned<Self>, Diagnostic> {
            if identifier.is_word() {
                Ok(Spanned::new(
                    identifier.span(),
                    Self(identifier.into_atom()),
                ))
            } else {
                Err(Diagnostic::invalid_crate_name(identifier.as_str()))
            }
        }

        pub fn core_package_name() -> Self {
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

    impl std::borrow::Borrow<CrateName> for Key<CrateName> {
        fn borrow(&self) -> &CrateName {
            &self.value
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
        let outcome!(mut tokens, health) = lexer::lex(source).ok()?;

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
