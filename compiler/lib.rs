//! The reference compiler of Lushui.
//!
//! # Passes
//!
//! The front-end consists of the three passes
//!
//! 1. [lexing][syntax::lexer] (output: [tokens](syntax::token))
//! 2. [parsing][syntax::parser] (output: [AST](syntax::ast))
//! 3. [lowering][syntax::lowerer] (output: [lowered AST](syntax::lowered_ast))
//!
//! The middle-end is made up of
//!
//! 1. [name resolution][resolver] (output: [HIR](hir))
//! 2. [type checking][typer]
//!
//! The back-end is one of
//!
//! * [(tree-walk) interpreting](typer::interpreter) (from the middle-end)
#![cfg_attr(
    feature = "llvm",
    doc = " * [LLVM-IR code generation, compilation and linking](backend) (output: LLVM-IR)"
)]
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

#[cfg(feature = "llvm")]
pub mod backend;
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
