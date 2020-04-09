//! The reference compiler of lushui called `lushuic`.
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
mod support;

use num_bigint::BigUint as Nat;
use smallvec::{smallvec, SmallVec};
use string_cache::DefaultAtom as Atom;
