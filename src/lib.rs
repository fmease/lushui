//! The reference compiler of lushui.
//!
//! It's not a compiler yet, still a bug-ridden feature-incomplete tree-walk interpreter
//! with type-checking.
//!
//! ## Passes
//!
//! For both the TWI (tree-walk interpreter) and the BCI (byte code interpreter),
//! the front-end consists of the three passes/stages lexing, parsing and lowering
//! defined in [lexer], [parser] and [lowerer] respectively. They also share a middle-end
//! immediately after the front-end being made up of name resolution and type checking in
//! [resolver] and [typer] respectively. After that, the passes differ.
//! The TWI runs the interpreter which is also being used for type checking in both passes,
//! namely [typer::interpreter]. On the other hand, BCI first compiles in [compiler] and
//! finally runs the program with [compiler::interpreter] (meta: that last part is not true
//! yet as that engine is WIP).
//!
//! | Engine | Passes and Outputs |
//! |--------|--------------------|
//! | <abbr title="tree-walk interpreter">TWI</abbr> | [**lexing**][0]: [tokens][1] → [**parsing**][2]: [AST][3] → [**lowering**][4]: [desugared AST][5] → [**name resolution**][6]: [HIR][7] → [**type checking**][8]: [HIR][7] → [**interpreting**](typer::interpreter): [HIR][7] |
//! | <abbr title="byte code interpreter">BCI</abbr> | [**lexing**][0]: [tokens][1] → [**parsing**][2]: [AST][3] → [**lowering**][4]: [desugared AST][5] → [**name resolution**][6]: [HIR][7] → [**type checking**][8]: [HIR][7] → [**compiling**](compiler): ? → [**interpreting**](compiler::interpreter): ? |
//! | Documenter | ? → [**documenter**](documenter): HTML |
//! | Formatter | ? |
//! | Highligher | ? |
//! | REPL | ? |
//!
//! [0]: lexer
//! [1]: lexer::token
//! [2]: parser
//! [3]: ast
//! [4]: lowerer
//! [5]: lowered_ast
//! [6]: resolver
//! [7]: hir
//! [8]: typer

#![feature(
    decl_macro,
    associated_type_defaults,
    never_type,
    bool_to_option,
    default_free_fn,
    min_const_generics,
    or_patterns,
    never_type_fallback,
    const_panic,
    map_first_last
)]
#![forbid(rust_2018_idioms, unused_must_use)]

pub mod compiler;
pub mod diagnostic;
pub mod documenter;
mod entity;
#[cfg(test)]
mod golden;
mod grow_array;
mod item;
pub mod lexer;
pub mod lowerer;
pub mod parser;
pub mod resolver;
pub mod span;
pub mod support;
pub mod typer;

const FILE_EXTENSION: &str = "lushui";

/// Amount of spaces making up one unit of indentation.
pub const INDENTATION_IN_SPACES: usize = 4;

use lowerer::lowered_ast;
use num_bigint::{BigInt as Int, BigUint as Nat};
use once_cell::sync::OnceCell;
use parser::ast;
use resolver::hir;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use smallvec::smallvec;
use std::path::Path;
use string_cache::DefaultAtom as Atom;

type Str = std::borrow::Cow<'static, str>;

type SmallVec<T, const N: usize> = smallvec::SmallVec<[T; N]>;

pub static OPTIONS: OnceCell<Options> = OnceCell::new();

pub struct Options {
    pub display_crate_indices: bool,
}

// @Task move
fn has_file_extension(path: &Path, required_extension: &str) -> bool {
    path.extension().and_then(|extension| extension.to_str()) == Some(required_extension)
}

use diagnostic::Diagnostic;

pub fn parse_crate_name(file: impl AsRef<Path>) -> Result<ast::Identifier, Diagnostic> {
    let file = file.as_ref();

    if !has_file_extension(file.as_ref(), FILE_EXTENSION) {
        Diagnostic::warning()
            .with_message("missing or non-standard file extension")
            .emit(None);
    }

    // @Question does unwrap ever fail in a real-world example?
    let stem = file.file_stem().unwrap();

    let atom = (|| lexer::parse_identifier(stem.to_str()?.to_owned()))().ok_or_else(|| {
        Diagnostic::error().with_message(format!(
            "`{}` is not a valid crate name",
            stem.to_string_lossy()
        ))
    })?;

    Ok(ast::Identifier::new(atom, span::Span::SHAM))
}
