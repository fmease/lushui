//! The reference compiler of lushui.
//!
//! It's not a compiler yet, still a bug-ridden feature-incomplete tree-walk interpreter
//! with type-checking.
//!
//! # Passes
//!
//! The front-end consists of the three passes
//!
//! 1. [lexing][syntax::lexer]
//! 2. [parsing][syntax::parser]
//! 3. [lowering][syntax::lowerer]
//!
//! The middle-end is made up of
//!
//! 1. [name resolution][resolver]
//! 2. [type checking][typer]
//!
//! Regarding the back-end: The tree-walk interpreter runs [`typer::interpreter`].
//!
//! | Backend | Passes and Outputs |
//! |--------|--------------------|
//! | (tree-walk) interpreter | [**lexing**][0]: [tokens][1] → [**parsing**][2]: [AST][3] → [**lowering**][4]: [lowered AST][5] <br> → [**name resolution**][6]: [HIR][7] → [**type checking**][8]: [HIR][7] <br> → [**interpreting**](typer::interpreter): [HIR][7] |
//!
//! [0]: syntax::lexer
//! [1]: syntax::token
//! [2]: syntax::parser
//! [3]: syntax::ast
//! [4]: syntax::lowerer
//! [5]: syntax::lowered_ast
//! [6]: resolver
//! [7]: hir
//! [8]: typer

#![feature(
    decl_macro,
    never_type,
    default_free_fn,
    never_type_fallback,
    stmt_expr_attributes,
    associated_type_bounds,
    label_break_value,
    generic_associated_types,
    try_trait_v2,
    adt_const_params,
    let_else,
    let_chains,
    min_specialization,
    iter_intersperse,
    type_changing_struct_update,
    slice_take,
    const_trait_impl,
    map_first_last
)]
#![deny(rust_2018_idioms, unused_must_use)]
#![allow(incomplete_features)] // adt_const_params
#![allow(rustdoc::private_intra_doc_links)] // we always use `--document-private-items`
#![warn(clippy::pedantic)]
#![allow(
    clippy::items_after_statements,
    clippy::enum_glob_use,
    clippy::must_use_candidate,
    clippy::missing_errors_doc,
    clippy::too_many_lines,
    clippy::module_name_repetitions,
    clippy::match_bool,
    clippy::empty_enum,
    clippy::single_match_else,
    clippy::if_not_else,
    clippy::blocks_in_if_conditions, // too many false positives with rustfmt's output
    clippy::trait_duplication_in_bounds, // @Temporary false positives (#8757)
    clippy::needless_pass_by_value, // @Temporary
    clippy::missing_panics_doc, // @Temporary
)]

pub mod component;
pub mod diagnostics;
pub mod documenter;
mod entity;
pub mod error;
mod hir;
mod item;
pub mod metadata;
pub mod package;
pub mod resolver;
pub mod server;
pub mod session;
pub mod span;
pub mod syntax;
pub mod typer;
pub mod utility;

/// The file extension of a Lushui source code file.
pub const FILE_EXTENSION: &str = "lushui";
