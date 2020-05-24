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
    bool_to_option
)]
#![forbid(rust_2018_idioms, unused_must_use)]

pub mod desugar;
pub mod diagnostic;
mod entity;
mod hir;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod resolver;
pub mod span;
pub mod support;
pub mod typer;

pub const FILE_EXTENSION: &str = "lushui";

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
