//! The reference compiler of lushui.
//!
//! It's not a compiler yet, still a bug-ridden feature-incomplete tree-walk interpreter
//! with type-checking.
//!
//! # Passes
//!
//! For both tree-walk interpreter and the byte-code interpreter,
//! the front-end consists of the three passes/stages lexing, parsing and lowering
//! defined in [`syntax::lexer`], [`syntax::parser`] and [`syntax::lowerer`] respectively.
//! They also share a middle-end
//! immediately after the front-end being made up of name resolution and type checking in
//! [resolver] and [typer] respectively. After that, the passes differ.
//! The TWI runs the interpreter which is also being used for type checking in both passes,
//! namely [`typer::interpreter`]. On the other hand, BCI first compiles in [compiler] and
//! finally runs the program with [`compiler::interpreter`] (meta: that last part is not true
//! yet as that engine is WIP).
//!
//! | Engine | Passes and Outputs |
//! |--------|--------------------|
//! | tree-walk interpreter | [**lexing**][0]: [tokens][1] → [**parsing**][2]: [AST][3] → [**lowering**][4]: [lowered AST][5] <br> → [**name resolution**][6]: [HIR][7] → [**type checking**][8]: [HIR][7] <br> → [**interpreting**](typer::interpreter): [HIR][7] |
//! | byte-code interpreter | [**lexing**][0]: [tokens][1] → [**parsing**][2]: [AST][3] → [**lowering**][4]: [lowered AST][5] <br> → [**name resolution**][6]: [HIR][7] → [**type checking**][8]: [HIR][7] <br> → [**compiling**](compiler): ? → [**interpreting**](compiler::interpreter): ? |
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

pub mod compiler;
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
pub mod session;
pub mod span;
pub mod syntax;
pub mod typer;
pub mod utility;

/// The file extension of a Lushui source code file.
pub const FILE_EXTENSION: &str = "lushui";
