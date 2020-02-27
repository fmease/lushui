//! The reference compiler of lushui called `lushuic`.
//!
//! It's not a compiler yet, still a bug-ridden feature-incomplete tree-walk interpreter
//! (with type-checking).
//!
//! ## `rustc` Features
//!
//! * `decl_macro` for out-of-order macro declarations which work with the module system
//!   and also look way better syntactically

#![feature(decl_macro)]
#![forbid(rust_2018_idioms, unused_must_use)]

pub mod desugar;
pub mod diagnostic;
// pub mod interpreter; // @Temporary comment during rewrite of name resolution
pub mod lexer;
pub mod parser;
pub mod resolver;
pub mod span;

use num_bigint::BigUint as Nat;
use string_cache::DefaultAtom as Atom;
