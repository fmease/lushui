//! The reference compiler of lushui called `lushuic`.
//! 
//! It's not a compiler yet, still a bug-ridden feature-incomplete tree-walk interpreter
//! (with type-checking).
//! 
//! ## `rustc` Features
//! 
//! * `matches_macro` for expressive code
//! * `decl_macro` for out-of-order macro declarations which work with the module system
//!   and also look way better syntactically
//! * `const_if_match` and `const_panic` for ensuring that certain types won't grow in size

#![feature(matches_macro, const_if_match, const_panic, decl_macro)]
#![forbid(rust_2018_idioms, unused_must_use)]

pub mod lexer;
pub mod parser;
pub mod hir;
pub mod error;
pub mod interpreter;

