//! The definition of the textual representation of the [HIR](hir).
#![feature(associated_type_defaults)]

use joinery::JoinableIterator;
use lexer::{token::INDENTATION, CharExt};
use session::{
    component::{Comp, DeclIdxExt, LocalDeclIdxExt},
    Session,
};
use std::{collections::VecDeque, fmt};
use utility::Atom;

#[cfg(test)]
mod test;

// FIXME: Migrate this from `Formatter` to `Painter` and properly color the HIR for
//        `-Zemit-hir`.

pub trait Display {
    fn write(&self, sess: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

impl Display for hir::Decl {
    fn write(&self, sess: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_decl(self, 0, sess, f)
    }
}

// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled
// @Task display attributes
fn write_decl(
    decl: &hir::Decl,
    depth: usize,
    sess: &Session<'_>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use hir::BareDecl::*;

    match &decl.bare {
        Func(func) => {
            write!(f, "{}: ", func.binder)?;
            func.ty.write(sess, f)?;
            if let Some(body) = &func.body {
                f.write_str(" = ")?;
                body.write(sess, f)?;
            }
            writeln!(f)
        }
        DataTy(ty) => {
            write!(f, "data {}: ", ty.binder)?;
            ty.ty.write(sess, f)?;

            if let Some(constructors) = &ty.ctors {
                writeln!(f, " of")?;

                for constructor in constructors {
                    let depth = depth + 1;
                    write!(f, "{}", " ".repeat(depth * INDENTATION.0))?;
                    constructor.write(sess, f)?;
                }
            } else {
                writeln!(f)?;
            }

            Ok(())
        }
        Ctor(ctor) => {
            write!(f, "{}: ", ctor.binder)?;
            ctor.ty.write(sess, f)?;
            writeln!(f)
        }
        Module(module) => {
            writeln!(f, "module {} of", module.binder)?;
            for declaration in &module.decls {
                let depth = depth + 1;
                write!(f, "{}", " ".repeat(depth * INDENTATION.0))?;
                write_decl(declaration, depth, sess, f)?;
            }
            Ok(())
        }
        Use(use_) => match &use_.binder {
            Some(binder) => writeln!(f, "use {} as {binder}", use_.target),
            None => writeln!(f, "use {}", use_.target),
        },
        Error(_) => writeln!(f, "⟨error⟩"),
    }
}

impl Display for hir::Expr {
    fn write(&self, sess: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_pi_ty_or_lower(self, sess, f)
    }
}

fn write_pi_ty_or_lower(
    expr: &hir::Expr,
    sess: &Session<'_>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use hir::BareExpr::*;

    // In here, we format `Lambda` and `CaseAnalysis` as a pi-type-literal-or-lower instead of
    // a lowered expression — which you might have expected from reading through the grammar and the parser.
    // The reason for this is the way we treat bracketed expressions: We do not represent them in the AST
    // (as their own nodes).
    // We could add more checks in the implementation of the pretty-printer since we "lost" information.
    // But actually, we do not need extra checks if we use a different grammar from the parser's.
    // Note that, syntactically, applications in lushui are so flexible that they actually
    // allow bare complex expressions as arguments e.g. `call \x => x` and `call do pure unit`.
    // Haskell adopted this change at some point, too, with the extension `BlockArguments`.
    // Read https://typeclasses.com/ghc/block-arguments and/or
    // https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/block_arguments.html?highlight=xblockarguments.
    // This leads to the phenomenon that `(read \this => this) alpha` and `read (\this => this) alpha`
    // parse to the identical AST modulo spans.
    // However for pretty-printing, we only use the last form — `read (\this => this) alpha` — to avoid
    // extra checks. For the case above, this decision is neither liberal nor conservate resulting in
    // an equal amount of brackets (that being one). This is not the case for `topmost.take (\it => it)` which
    // we *might* want to print as `topmost.take \it => it`. This would probably require passing some flags to
    // the formatting functions and adding more checks.
    //
    // See also `crate::syntax::parser::test::application_lambda_literal_argument_{lax,strict}_grouping` and the
    // comment at the grammar definition of `Pi-Type-Literal-Or-Lower` (in `/misc/grammar/lushui.grammar`)
    // for further details.
    match &expr.bare {
        PiTy(pi_ty) => {
            if pi_ty.binder.is_none() && pi_ty.kind == hir::ParamKind::Explicit {
                write_app_or_lower(&pi_ty.domain, sess, f)?;
            } else {
                f.write_str("For ")?;

                if pi_ty.kind == hir::ParamKind::Implicit {
                    f.write_str("'")?;
                }
                if pi_ty.kind == hir::ParamKind::Context {
                    f.write_str("[")?;

                    if let Some(binder) = pi_ty.binder {
                        write!(f, "{binder}: ")?;
                    }

                    pi_ty.domain.write(sess, f)?;
                    f.write_str("]")?;
                } else {
                    let binder = pi_ty.binder.map_or(Atom::UNDERSCORE, hir::Ident::bare);

                    write!(f, "({binder}: ")?;
                    pi_ty.domain.write(sess, f)?;
                    f.write_str(")")?;
                }
            }

            f.write_str(" -> ")?;
            write_pi_ty_or_lower(&pi_ty.codomain, sess, f)
        }
        LamLit(lambda) => {
            f.write_str("for ")?;

            if lambda.kind == hir::ParamKind::Implicit {
                f.write_str("'")?;
            }
            if lambda.kind == hir::ParamKind::Context {
                f.write_str("[")?;

                if let Some(binder) = lambda.binder {
                    write!(f, "{binder}: ")?;
                }

                // Although it's not statically guaranteed, the domain of context parameters has exist.
                // Let's not unwrap though for robustness.
                if let Some(domain) = &lambda.domain {
                    domain.write(sess, f)?;
                }

                f.write_str("]")?;
            } else {
                let binder = lambda.binder.map_or(Atom::UNDERSCORE, hir::Ident::bare);

                if let Some(domain) = &lambda.domain {
                    write!(f, "({binder}: ")?;
                    domain.write(sess, f)?;
                    f.write_str(")")?;
                } else {
                    write!(f, "{binder}")?;
                }
            }

            if let Some(codomain) = &lambda.codomain {
                write!(f, ": ")?;
                codomain.write(sess, f)?;
            }

            write!(f, " => ")?;
            lambda.body.write(sess, f)
        }
        // @Task fix indentation
        CaseAnalysis(analysis) => {
            write!(f, "case ")?;
            analysis.scrutinee.write(sess, f)?;
            writeln!(f, " of")?;
            for case in &analysis.cases {
                case.pat.write(sess, f)?;
                writeln!(f, " => ")?;
                case.body.write(sess, f)?;
            }
            Ok(())
        }
        _ => write_app_or_lower(expr, sess, f),
    }
}

// @Task write named arguments
fn write_app_or_lower(
    expr: &hir::Expr,
    sess: &Session<'_>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use hir::BareExpr::*;

    match &expr.bare {
        App(app) => {
            write_app_or_lower(&app.callee, sess, f)?;

            f.write_str(" ")?;

            if app.kind == hir::ParamKind::Implicit {
                f.write_str("'")?;
            }
            if app.kind == hir::ParamKind::Context {
                f.write_str("[")?;
                app.arg.write(sess, f)?;
                f.write_str("]")
            } else {
                write_lower_expr(&app.arg, sess, f)
            }
        }
        IntrApp(app) => {
            write!(f, "{}", app.callee)?;

            for arg in &app.args {
                write!(f, " ")?;
                write_lower_expr(arg, sess, f)?;
            }

            Ok(())
        }
        _ => write_lower_expr(expr, sess, f),
    }
}

fn write_lower_expr(
    expr: &hir::Expr,
    sess: &Session<'_>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use hir::BareExpr::*;

    for attr in &expr.attrs.0 {
        write!(f, "{attr} ")?;
    }

    match &expr.bare {
        NumLit(num) => write!(f, "{num}"),
        TextLit(text) => write!(f, "{text}"),
        Binding(binding) => write!(f, "{}", sess.binder_to_path(binding.0)),
        Proj(proj) => {
            write_lower_expr(&proj.basis, sess, f)?;
            write!(f, "::{}", proj.field)
        }
        // @Task
        RecLit(_rec) => f.write_str("⟨rec-lit⟩"),
        IO(io) => {
            // @Temporary format
            write!(f, "⟨io {}", io.index)?;
            for arg in &io.args {
                f.write_str(" ")?;
                arg.write(sess, f)?;
            }
            f.write_str("⟩")
        }
        Substed(substed) => {
            f.write_str("⟨substed ")?;
            substed.subst.write(sess, f)?;
            f.write_str(" ")?;
            substed.expr.write(sess, f)?;
            f.write_str("⟩")
        }
        Error(_) => write!(f, "⟨error⟩"),
        _ => {
            write!(f, "(")?;
            expr.write(sess, f)?;
            write!(f, ")")
        }
    }
}

// @Beacon @Task respect / incorporate precedence for a prettier output (just like we do with expressions)
impl Display for hir::Pat {
    fn write(&self, sess: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use hir::BarePat::*;

        match &self.bare {
            NumLit(num) => write!(f, "{num}"),
            TextLit(text) => write!(f, "{text}"),
            Binding(binding) => write!(f, "{}", sess.binder_to_path(binding.0)),
            LetBinding(binder) => write!(f, "(let {binder})"),
            App(app) => {
                write!(f, "(")?;
                app.callee.write(sess, f)?;
                write!(f, ") (")?;
                app.arg.write(sess, f)?;
                write!(f, ")")
            }
            Error(_) => write!(f, "⟨error⟩"),
        }
    }
}

impl Display for hir::Subst {
    fn write(&self, sess: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Shift(amount) => write!(f, "shift {amount}"),
            Self::Use(substitution, expression) => {
                expression.write(sess, f)?;
                write!(f, "[")?;
                substitution.write(sess, f)?;
                write!(f, "]")
            }
        }
    }
}

impl Display for hir::Exposure {
    fn write(&self, sess: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unrestricted => write!(f, "unrestricted"),
            Self::Restricted(reach) => {
                write!(f, "‘")?;
                reach.lock().unwrap().write(sess, f)?;
                write!(f, "’")
            }
        }
    }
}

impl Display for hir::ExposureReach {
    fn write(&self, sess: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Resolved(reach) => {
                let reach = reach.global(sess);
                write!(f, "{}", sess.index_to_path(reach))
            }
            // should not be reachable
            Self::PartiallyResolved(reach) => write!(f, "{reach:?}"),
        }
    }
}

impl Display for hir::ValueView {
    fn write(&self, sess: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Reducible(expression) => {
                write!(f, "⟨reducible ")?;
                expression.write(sess, f)?;
                write!(f, "⟩")
            }
            Self::Neutral => write!(f, "⟨neutral⟩"),
        }
    }
}

impl Display for hir::Entity {
    fn write(&self, sess: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // @Task improve output for overly long lines (when the identifier is too long or more
        // importantly when the entity kind (esp. expressions within it) are big)

        let parent = self
            .parent
            .map(|parent| format!("{parent:?}."))
            .unwrap_or_default();
        write!(
            f,
            "{:>4?}<   {:<40} ↦ ",
            self.exp,
            format!("{parent}{}", self.src)
        )?;
        self.kind.write(sess, f)
    }
}

impl Display for hir::EntityKind {
    fn write(&self, sess: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use hir::EntityKind::*;

        write!(f, "{:>19}   ", self.precise_name())?;

        match self {
            Module { namespace } | DataTyUntyped { namespace } => write!(f, "{namespace:?}"),
            Use { target } => write!(f, "{target:?}"),
            Func { ty, expression } => {
                match expression {
                    Some(expression) => expression.write(sess, f),
                    None => write!(f, "⟨none⟩"),
                }?;

                write!(f, " : ")?;
                ty.write(sess, f)
            }
            DataTy {
                ty,
                ctors: constructors,
                ..
            } => {
                ty.write(sess, f)?;

                if !constructors.is_empty() {
                    write!(f, "; {}", constructors.iter().join_with(' '))?;
                }

                Ok(())
            }
            Ctor { ty } | FuncIntr { ty, .. } => ty.write(sess, f),
            _ => Ok(()),
        }
    }
}

pub trait SessionExt {
    /// The textual representation of the path of the given binding.
    ///
    /// If the binding is local meaning it represents a function parameter or a binder in a pattern,
    /// this method will return a single identifier that could be used in the corresponding function or
    /// case analysis case to refer to it until the point it gets shadowed (if any) but not outside of
    /// that environment.
    ///
    /// If it's not local, this method will return [the path relative to the root of the current
    /// component][Self::index_to_path].
    ///
    /// # Example Output
    ///
    /// * `x`
    /// * `alpha`
    /// * `topmost`
    /// * `topmost.alpha`
    /// * `topmost.gamma.<?//`
    /// * `extern.core`
    /// * `extern.core.nat.Nat`
    fn binder_to_path(&self, binder: hir::Ident) -> String;

    /// The textual representation of the path of the given binding relative to the root of the current component.
    ///
    /// Rephrased it returns a path that could be used in any expression in any module of the current component
    /// to unambiguously (including shadowing) refer to the given binding ignoring exposure.
    /// If the binding is defined in this component, it will always start with the path hanger `topmost`,
    /// otherwise it will start with `extern` followed by the respective name of the external component.
    ///
    /// # Example Output
    ///
    /// * `topmost`
    /// * `topmost.alpha`
    /// * `topmost.gamma.<?//`
    /// * `extern.core`
    /// * `extern.core.nat.Nat`
    fn index_to_path(&self, index: hir::DeclIdx) -> String;

    fn local_index_to_path(&self, index: hir::LocalDeclIdx) -> String;
}

impl SessionExt for Session<'_> {
    fn binder_to_path(&self, binder: hir::Ident) -> String {
        use hir::Index::*;

        match binder.idx {
            Decl(index) => self.index_to_path(index),
            DeBruijn(_) | Param => binder.to_string(),
        }
    }

    fn index_to_path(&self, index: hir::DeclIdx) -> String {
        let root = ast::BareHanger::Topmost.name().to_owned();

        self.comp().index_with_root_to_path(index, root, self)
    }

    // @Question can we rewrite this function to not rely on Session?
    fn local_index_to_path(&self, index: hir::LocalDeclIdx) -> String {
        self.index_to_path(index.global(self))
    }
}

pub trait ComponentExt {
    fn index_with_root_to_path(
        &self,
        index: hir::DeclIdx,
        root: String,
        sess: &Session<'_>,
    ) -> String;

    /// The textual representation of the path to the given binding relative to a component root
    /// prefixed with name of the corresponding component.
    ///
    /// Rephrased, it returns a path that could be used in any dependent components (reverse dependencies)
    /// to refer to the binding ignoring exposure as long as one would prepend the path hanger `extern`.
    ///
    /// # Example Output
    ///
    /// * `core.nat.Nat` (`core` referring to a component)
    /// * `json.parse` (`json` referring to a component)
    fn local_idx_with_root_to_extern_path(&self, index: hir::LocalDeclIdx, root: String) -> String;

    // @Task add documentation
    fn local_idx_to_path_segments(&self, index: hir::LocalDeclIdx) -> VecDeque<Atom>;
}

impl ComponentExt for Comp {
    fn index_with_root_to_path(
        &self,
        index: hir::DeclIdx,
        root: String,
        sess: &Session<'_>,
    ) -> String {
        match index.local(self) {
            Some(index) => self.local_idx_with_root_to_extern_path(index, root),
            None => {
                let component = &sess.look_up_comp(index.comp());
                let root = format!("{}.{}", ast::BareHanger::Extern.name(), component.name());

                component.index_with_root_to_path(index, root, sess)
            }
        }
    }

    fn local_idx_with_root_to_extern_path(&self, index: hir::LocalDeclIdx, root: String) -> String {
        let entity = &self[index];
        // @Task rewrite this recursive approach to an iterative one!
        let Some(parent) = entity.parent else {
            return root;
        };

        let mut parent_path = self.local_idx_with_root_to_extern_path(parent, root);

        let parent_is_symbol = parent_path.chars().next_back().unwrap().is_symbol();

        if parent_is_symbol {
            parent_path.push(' ');
        }

        parent_path.push('.');

        if entity.src.is_symbol() && parent_is_symbol {
            parent_path.push(' ');
        }

        parent_path += entity.src.to_str();
        parent_path
    }

    fn local_idx_to_path_segments(&self, mut index: hir::LocalDeclIdx) -> VecDeque<Atom> {
        let mut segments = VecDeque::new();

        while let Some(parent) = self[index].parent {
            segments.push_front(self[index].src.bare());
            index = parent;
        }

        segments
    }
}

impl Display for Comp {
    fn write(&self, sess: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{} ({:?})", self.name(), self.idx())?;

        writeln!(f, "  bindings:")?;

        for (index, entity) in &self.bindings {
            write!(f, "    {index:?}: ")?;
            entity.write(sess, f)?;
            writeln!(f)?;
        }

        Ok(())
    }
}
