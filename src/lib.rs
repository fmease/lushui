//! The reference compiler of lushui.
//!
//! It's not a compiler yet, still a bug-ridden feature-incomplete tree-walk interpreter
//! with type-checking.
//!
//! ## Passes
//!
//! For both the TWI (tree-walk interpreter) and the BCI (byte code interpreter),
//! the front-end consists of the three passes/stages lexing, parsing and lowering
//! defined in [lexer], [parser] and [lowerer] respectively. They also share a middle-end
//! immediately after the front-end being made up of name resolution and type checking in
//! [resolver] and [typer] respectively. After that, the passes differ.
//! The TWI runs the interpreter which is also being used for type checking in both passes,
//! namely [typer::interpreter]. On the other hand, BCI first compiles in [compiler] and
//! finally runs the program with [compiler::interpreter] (meta: that last part is not true
//! yet as that engine is WIP).
//!
//! | Engine | Passes and Outputs |
//! |--------|--------------------|
//! | <abbr title="tree-walk interpreter">TWI</abbr> | [**lexing**][0]: [tokens][1] → [**parsing**][2]: [AST][3] → [**lowering**][4]: [lowered AST][5] <br> → [**name resolution**][6]: [HIR][7] → [**type checking**][8]: [HIR][7] <br> → [**interpreting**](typer::interpreter): [HIR][7] |
//! | <abbr title="byte code interpreter">BCI</abbr> | [**lexing**][0]: [tokens][1] → [**parsing**][2]: [AST][3] → [**lowering**][4]: [lowered AST][5] <br> → [**name resolution**][6]: [HIR][7] → [**type checking**][8]: [HIR][7] <br> → [**compiling**](compiler): ? → [**interpreting**](compiler::interpreter): ? |
//! | Documenter | ? → [**documenter**](documenter): HTML |
//! | Formatter | ? |
//! | Highlighter | ? |
//! | REPL | ? |
//!
//! [0]: lexer
//! [1]: lexer::token
//! [2]: parser
//! [3]: ast
//! [4]: lowerer
//! [5]: lowered_ast
//! [6]: resolver
//! [7]: hir
//! [8]: typer

#![feature(
    decl_macro,
    associated_type_defaults,
    never_type,
    default_free_fn,
    never_type_fallback,
    const_panic,
    map_first_last,
    stmt_expr_attributes,
    format_args_capture,
    associated_type_bounds,
    label_break_value,
    type_ascription,
    derive_default_enum,
    generic_associated_types,
    path_try_exists,
    extend_one,
    try_trait_v2
)]
#![deny(rust_2018_idioms, unused_must_use)]

pub mod compiler;
pub mod diagnostics;
mod entity;
pub mod error;
pub mod format;
mod grow_array;
mod item;
pub mod lexer;
pub mod lowerer;
pub mod metadata;
pub mod package;
pub mod parser;
pub mod resolver;
pub mod span;
pub mod typer;
mod util;

/// The file extension of a Lushui source code file.
pub const FILE_EXTENSION: &str = "lushui";

use lowerer::lowered_ast;
use once_cell::sync::OnceCell;
use parser::ast;
use resolver::hir;

// @Task remove this options stuff!

static OPTIONS: OnceCell<GlobalOptions> = OnceCell::new();

pub struct GlobalOptions {
    pub show_binding_indices: bool,
}

pub fn set_global_options(options: GlobalOptions) {
    OPTIONS.set(options).unwrap_or_else(|_| unreachable!());
}
