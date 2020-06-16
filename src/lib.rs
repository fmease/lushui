//! The reference compiler of lushui.
//!
//! It's not a compiler yet, still a bug-ridden feature-incomplete tree-walk interpreter
//! with type-checking.
//!

#![feature(
    decl_macro,
    move_ref_pattern,
    associated_type_defaults,
    never_type,
    bool_to_option,
    // untagged_unions
)]
#![forbid(rust_2018_idioms, unused_must_use)]

pub mod desugar;
pub mod diagnostic;
mod entity;
#[cfg(test)]
mod golden;
mod hir;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod resolver;
pub mod span;
pub mod support;
pub mod typer;

const FILE_EXTENSION: &str = "lushui";

/// Amount of spaces making up one unit of indentation.
pub const INDENTATION_IN_SPACES: usize = 4;

use num_bigint::BigUint as Nat;
use smallvec::{smallvec, SmallVec};
use string_cache::DefaultAtom as Atom;

use once_cell::sync::OnceCell;

pub static OPTIONS: OnceCell<Options> = OnceCell::new();

pub struct Options {
    pub display_crate_indices: bool,
}

use std::path::Path;

// @Task move
fn has_file_extension(path: &Path, required_extension: &str) -> bool {
    path.extension().and_then(|extension| extension.to_str()) == Some(required_extension)
}

use diagnostic::{Diagnostic, Level};

pub fn parse_crate_name(file: impl AsRef<Path>) -> Result<parser::Identifier, Diagnostic> {
    let file = file.as_ref();

    if !has_file_extension(file.as_ref(), FILE_EXTENSION) {
        Diagnostic::new(
            Level::Warning,
            None,
            "missing or non-standard file extension",
        )
        .emit(None);
    }

    // @Question does unwrap ever fail in a real-world example?
    let stem = file.file_stem().unwrap();

    let atom = (|| lexer::parse_identifier(stem.to_str()?.to_owned()))().ok_or_else(|| {
        Diagnostic::new(
            Level::Fatal,
            None,
            format!("`{}` is not a valid crate name", stem.to_string_lossy()),
        )
    })?;

    Ok(parser::Identifier::new(atom, span::Span::SHAM))
}
