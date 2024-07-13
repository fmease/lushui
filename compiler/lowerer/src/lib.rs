//! The lowering stage (AST simplification and early validation).
//!
//! Higher-level syntactic constructs in the AST are simplified and rewritten
//! in terms of lower-level language primitives and undergo several checks since
//! they might be more liberal than the intermediate languages allow.
//!
//! This pass does the following:
//!
//! * lower let-bindings to lambda literals
//! * lower parameters to simple lambda literals
//! * open out-of-line modules (this will probably move to the parser in the future
//!   for parallel reading and independent error reporting)
//! * simplify use-declarations by unfolding use-path trees (note: this should be removed)
//! * parses general attributes into concrete ones and
//! * validates their location (item target), uniqueness if applicable,
//!   exclusivity rules
//! * checks if the required type annotations on top-level declarations are present
//! * gates a lot of unsupported features
#![feature(
    decl_macro,
    slice_take,
    min_specialization,
    if_let_guard,
    iter_collect_into,
    let_chains
)]

// @Task ungate named arguments but validate them in the resolver (and/or typer)
// @Task disallow conflicting parameter names

use diagnostics::{
    error::{Handler, Health, Outcome, PossiblyErroneous, Result},
    Diag,
};
use session::Session;

mod attr;
mod decl;
mod expr;
mod pat;

/// Lower a file.
pub fn lower_file(decl: ast::Decl, opts: Options, sess: &Session<'_>) -> Result<lo_ast::Decl> {
    let mut lowerer = Lowerer {
        opts,
        sess,
        health: Health::Untainted,
    };
    let mut declaration = lowerer.lower_decl(decl);
    let root = declaration.pop().unwrap();
    Outcome::new(root, lowerer.health).into()
}

/// The state of the lowering pass.
struct Lowerer<'a> {
    opts: Options,
    sess: &'a Session<'a>,
    health: Health,
}

impl Handler for &mut Lowerer<'_> {
    fn embed<T: PossiblyErroneous>(self, diag: Diag) -> T {
        let error = diag.report(self.sess.rep());
        self.health.taint(error);
        T::error(error)
    }
}

// @Task get rid of this by smh. moving this information into the session
#[derive(Default)]
pub struct Options {
    /// Specifies if internal language and library features are enabled.
    pub internal_features_enabled: bool,
    /// Specifies if documentation comments should be kept in the Lo-AST.
    pub keep_doc_comments: bool,
}
