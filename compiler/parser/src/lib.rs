//! The syntactic analyzer (parser).
//!
//! It is a handwritten top-down recursive-descent parser with bounded look-ahead &
//! look-behind and no backtracking.
//!
//! Currently, there is no error recovery whatsoever. Syntax errors are considered
//! fatal.
//!
//! # Grammar Notation
//!
//! Most subparsers are accompanied by a grammar snippet. These snippets are written
//! in an EBNF-flavor called *grammar* which is informally defined below:
//!
//! | Notation  | Name                                | Definition or Remark                                          |
//! |-----------|-------------------------------------|---------------------------------------------------------------|
//! | `; C`     | Comment                             | Stretches until the end of the line                           |
//! | `N ::= R` | Definition                          | Defines non-terminal `A` by rule `R`                          |
//! | `A B`     | Sequence                            | Rule `B` immediately followed by rule `A` modulo lexed tokens |
//! | `(A)`     | Grouping                            | To escape default precedence                                  |
//! | <code>A &vert; B</code>   | Ordered Alternative                 | Either `A` or `B` first trying `A` then `B`                   |
//! | `A?`      | Option                              | `A` or nothing (ε)                                            |
//! | `A*`      | Kleene Star (Multiplicity)          | Arbitrarily long sequence of `A`s                             |
//! | `A+`      | Kleene Plus (Positive Multiplicity) | Arbitrarily long non-empty sequence of `A`s                   |
//! | `"T"`     | Terminal                            | Lexed token by textual content                                |
//! | `#T`      | Named Terminal                      | Lexed token by name                                           |
//! | `(> A)`   | Positive Look-Ahead                 | Bounded                                                       |
//! | `(< A)`   | Positive Look-Behind                | Bounded                                                       |
//! | `⟪M⟫`     | Metavariable                        | Draws from a finite domain                                    |
//! | `⟪⟪ L ⟫⟫` | Metalanguage expression             | Mirrors the object language                                   |
#![feature(decl_macro, let_chains)]
#![allow(clippy::unnested_or_patterns)] // false positive with macros, see #9899

use ast::{Declaration, Identifier};
use base::Parser;
use diagnostics::{Diagnostic, ErrorCode, Reporter, error::Result};
use lexer::word::Word;
use span::{SourceFileIndex, SourceMap, Spanned};

mod base;
mod common;
mod declaration;
mod expression;
mod pattern;
mod synonym;
#[cfg(test)]
mod test;

/// Parse the file of a root module / component root.
pub fn parse_root_module_file(
    tokens: lexer::Outcome,
    file: SourceFileIndex,
    map: &SourceMap,
    reporter: &Reporter,
) -> Result<Declaration> {
    // @Task don't use unwrap(), handle errors properly
    let name = map[file].name().path().unwrap().file_stem().unwrap().to_str().unwrap();

    let binder = Word::parse(name.to_owned()).map_err(|()| {
        Diagnostic::error()
            .code(ErrorCode::E036)
            .message(format!("the name of the root module ‘{name}’ is not a valid word"))
            .report(reporter)
    })?;
    let binder = Spanned::bare(binder).into();

    parse_module_file(tokens, file, binder, map, reporter)
}

/// Parse the file of a root module or an out-of-line module.
pub fn parse_module_file(
    tokens: lexer::Outcome,
    file: SourceFileIndex,
    binder: Identifier,
    map: &SourceMap,
    reporter: &Reporter,
) -> Result<Declaration> {
    parse(tokens, |parser| parser.parse_top_level(binder), file, map, reporter)
}

pub fn parse_path(
    tokens: lexer::Outcome,
    file: SourceFileIndex,
    map: &SourceMap,
    reporter: &Reporter,
) -> Result<ast::Path> {
    parse(tokens, |parser| parser.parse_path(), file, map, reporter)
}

fn parse<T>(
    tokens: lexer::Outcome,
    parser: impl FnOnce(&mut Parser<'_>) -> Result<T>,
    file: SourceFileIndex,
    map: &SourceMap,
    reporter: &Reporter,
) -> Result<T> {
    let mut health = Ok(());

    for error in tokens.errors {
        let error = error::invalid_token(error).report(reporter);

        if health.is_ok() {
            health = Err(error);
        }
    }

    let result = parser(&mut Parser::new(tokens.tokens, file, map, reporter));

    health.and(result)
}

mod error {
    #[allow(clippy::wildcard_imports)] // private inline module
    use super::*;
    use lexer::token::{INDENTATION, IndentationError};

    pub(super) fn invalid_token(error: lexer::Error) -> Diagnostic {
        use lexer::BareError::*;

        match error.bare {
            InvalidIndentation(difference, indentation_error) => Diagnostic::error()
                .code(ErrorCode::E046)
                .message(format!("invalid indentation consisting of {} spaces", difference.0))
                .unlabeled_span(error.span)
                .note(match indentation_error {
                    IndentationError::Misaligned => {
                        format!("indentation needs to be a multiple of {}", INDENTATION.0)
                    }
                    IndentationError::TooDeep => format!(
                        "indentation is greater than {} and therefore too deep",
                        INDENTATION.0
                    ),
                }),
            InvalidToken(token) => {
                let message = format!("found invalid character U+{:04X} ‘{token}’", token as u32);

                // @Task code
                Diagnostic::error().message(message).span(error.span, "unexpected token")
            }
            UnbalancedBracket(bracket) => Diagnostic::error()
                .code(ErrorCode::E044)
                .message(format!("unbalanced {} bracket", bracket.kind))
                .span(
                    error.span,
                    format!("has no matching {} {} bracket", !bracket.orientation, bracket.kind),
                ),
            // @Task improve message, mention closing it with quotes
            UnterminatedTextLiteral => Diagnostic::error()
                .code(ErrorCode::E047)
                .message("unterminated text literal")
                .unlabeled_span(error.span),
        }
    }
}
