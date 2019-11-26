#![feature(box_patterns)]
#![forbid(rust_2018_idioms, unused_must_use)]

pub mod lexer;
pub mod parser;
pub mod hir;
pub mod error;
// @Note this is our tree-walk backend (which does type-checking *and* evaluation)
// type-checking should (kind-of) be front-end (back-end-independent)
// @Temporary naming
// @Task split off type-checking from this backend
pub mod effluvium;
