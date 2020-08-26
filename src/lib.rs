//! The reference compiler of lushui.
//!
//! It's not a compiler yet, still a bug-ridden feature-incomplete tree-walk interpreter
//! with type-checking.

#![feature(
    decl_macro,
    move_ref_pattern,
    associated_type_defaults,
    never_type,
    bool_to_option,
    default_free_fn,
    const_generics,
    const_in_array_repeat_expressions,
    or_patterns,
    // untagged_unions,
)]
#![forbid(rust_2018_idioms, unused_must_use)]
// @Note noted ^^ we will only do very basic stuff w/ const generics
#![allow(incomplete_features)]

pub mod compiler;
pub mod desugar;
pub mod diagnostic;
pub mod documenter;
mod entity;
#[cfg(test)]
mod golden;
mod grow_array;
mod hir;
pub mod lexer;
pub mod parser;
pub mod resolver;
pub mod span;
pub mod support;
pub mod typer;

use parser::ast;

const FILE_EXTENSION: &str = "lushui";

/// Amount of spaces making up one unit of indentation.
pub const INDENTATION_IN_SPACES: usize = 4;

use num_bigint::BigInt as Int;
use num_bigint::BigUint as Nat;
use smallvec::smallvec;
use string_cache::DefaultAtom as Atom;

use once_cell::sync::OnceCell;

type SmallVec<T, const N: usize> = smallvec::SmallVec<[T; N]>;

pub static OPTIONS: OnceCell<Options> = OnceCell::new();

pub struct Options {
    pub display_crate_indices: bool,
}

use std::path::Path;

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
