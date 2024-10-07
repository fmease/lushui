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
#![feature(decl_macro, slice_take, min_specialization, if_let_guard, iter_collect_into, let_chains)]

// @Task ungate named arguments but validate them in the resolver (and/or typer)
// @Task disallow conflicting parameter names

use diagnostics::{
    Diagnostic,
    error::{Handler, Health, Outcome, PossiblyErroneous, Result},
};
use session::Session;

mod attribute;
mod declaration;
mod expression;
mod pattern;

/// Lower a file.
pub fn lower_file(
    declaration: ast::Declaration,
    options: Options,
    session: &Session<'_>,
) -> Result<lo_ast::Declaration> {
    let mut lowerer = Lowerer { options, session, health: Health::Untainted };
    let mut declaration = lowerer.lower_declaration(declaration);
    let root = declaration.pop().unwrap();
    Outcome::new(root, lowerer.health).into()
}

/// The state of the lowering pass.
struct Lowerer<'a> {
    options: Options,
    session: &'a Session<'a>,
    health: Health,
}

impl Handler for &mut Lowerer<'_> {
    fn embed<T: PossiblyErroneous>(self, diagnostic: Diagnostic) -> T {
        let error = diagnostic.report(self.session.reporter());
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
    pub keep_documentation_comments: bool,
}
