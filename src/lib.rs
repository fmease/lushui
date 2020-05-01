//! The reference compiler of lushui.
//!
//! It's not a compiler yet, still a bug-ridden feature-incomplete tree-walk interpreter
//! (with type-checking).
//!

#![feature(decl_macro, move_ref_pattern)]
#![forbid(rust_2018_idioms, unused_must_use)]

pub mod desugar;
pub mod diagnostic;
mod hir;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod resolver;
pub mod span;
pub mod support;

pub const NAME: &str = env!("CARGO_PKG_NAME");
pub const VERSION: &str = concat!(env!("CARGO_PKG_VERSION"), " ", env!("GIT_COMMIT_HASH"));
pub const DESCRIPTION: &str = env!("CARGO_PKG_DESCRIPTION");
pub const FILE_EXTENSION: &str = "lushui";

use num_bigint::BigUint as Nat;
use smallvec::{smallvec, SmallVec};
use string_cache::DefaultAtom as Atom;
