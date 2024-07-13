use super::{
    decl_id,
    node::{Attributable, Elem},
};
use hir::DeclIdx;
use hir_format::{ComponentExt, Display, SessionExt};
use joinery::JoinableIterator;
use session::Session;
use std::fmt::Write;
use utility::{displayed, Atom};

pub(super) fn fmt_expr(expr: &hir::Expr, url_prefix: &str, sess: &Session<'_>) -> String {
    let mut f = Formatter::new(url_prefix, sess);
    f.fmt_expr(expr);
    f.finish()
}

pub(super) fn decl_url_fragment(index: DeclIdx, sess: &Session<'_>) -> String {
    Formatter::new("./", sess).decl_url_fragment(index)
}

struct Formatter<'a> {
    url_prefix: &'a str,
    sess: &'a Session<'a>,
    output: String,
}

impl<'a> Formatter<'a> {
    fn new(url_prefix: &'a str, sess: &'a Session<'a>) -> Self {
        Self {
            url_prefix,
            sess,
            output: String::new(),
        }
    }

    fn write(&mut self, content: &str) {
        self.output += content;
    }

    fn finish(self) -> String {
        self.output
    }

    fn fmt_expr(&mut self, expression: &hir::Expr) {
        self.fmt_pi_ty_or_lower(expression);
    }

    fn fmt_pi_ty_or_lower(&mut self, expr: &hir::Expr) {
        use hir::BareExpr::*;

        match &expr.bare {
            PiTy(pi_ty) => {
                if pi_ty.binder.is_none() && pi_ty.kind == hir::ParamKind::Explicit {
                    self.fmt_app_or_lower(&pi_ty.domain);
                } else {
                    self.write("For ");

                    if pi_ty.kind == hir::ParamKind::Implicit {
                        self.write("'");
                    }
                    if pi_ty.kind == hir::ParamKind::Context {
                        self.write("[");

                        if let Some(binder) = pi_ty.binder {
                            self.write(binder.to_str());
                            self.write(": ");
                        }

                        self.fmt_expr(&pi_ty.domain);

                        self.write("]");
                    } else {
                        let binder = pi_ty
                            .binder
                            .map_or(Atom::UNDERSCORE, hir::Ident::bare)
                            .to_str();

                        self.write("(");
                        self.write(binder);
                        self.write(": ");
                        self.fmt_expr(&pi_ty.domain);
                        self.write(")");
                    }
                }

                self.write(" -&gt; ");
                self.fmt_pi_ty_or_lower(&pi_ty.codomain);
            }
            LamLit(lambda) => {
                self.write("for ");
                if lambda.kind == hir::ParamKind::Implicit {
                    self.write("'");
                }
                if lambda.kind == hir::ParamKind::Context {
                    self.write("[");

                    if let Some(binder) = lambda.binder {
                        self.write(binder.to_str());
                        self.write(": ");
                    }

                    if let Some(domain) = &lambda.domain {
                        self.write(": ");
                        self.fmt_expr(domain);
                    }

                    self.write("]");
                } else {
                    let binder = lambda
                        .binder
                        .map_or(Atom::UNDERSCORE, hir::Ident::bare)
                        .to_str();

                    if let Some(domain) = &lambda.domain {
                        self.write("(");
                        self.write(binder);
                        self.write(": ");
                        self.fmt_expr(domain);
                        self.write(")");
                    } else {
                        self.write(binder);
                    }
                }

                if let Some(codomain) = &lambda.codomain {
                    self.write(": ");
                    self.fmt_expr(codomain);
                }

                self.write(" =&gt; ");
                self.fmt_expr(&lambda.body);
            }
            // @Task fix indentation
            CaseAnalysis(analysis) => {
                self.write("case ");
                self.fmt_expr(&analysis.scrutinee);
                self.write(" of {");

                // @Task spacing
                for case in &analysis.cases {
                    self.fmt_pat(&case.pat);
                    self.write(" =&gt; ");
                    self.fmt_expr(&case.body);
                    self.write(";");
                }

                self.write("}");
            }
            _ => self.fmt_app_or_lower(expr),
        }
    }

    // @Task write named arguments
    fn fmt_app_or_lower(&mut self, expr: &hir::Expr) {
        use hir::BareExpr::*;

        match &expr.bare {
            App(app) => {
                self.fmt_app_or_lower(&app.callee);
                self.write(" ");
                if app.kind == hir::ParamKind::Implicit {
                    self.write("'");
                }
                if app.kind == hir::ParamKind::Context {
                    self.write("[");
                    self.fmt_expr(&app.arg);
                    self.write("]");
                } else {
                    self.fmt_lower_expr(&app.arg);
                }
            }
            IntrApp(app) => {
                self.write(&app.callee.to_string());

                for arg in &app.args {
                    self.write(" ");
                    self.fmt_lower_expr(arg);
                }
            }
            _ => self.fmt_lower_expr(expr),
        }
    }

    fn fmt_lower_expr(&mut self, expr: &hir::Expr) {
        use hir::BareExpr::*;

        for attr in &expr.attrs.0 {
            self.write(&attr.to_string());
            self.write(" ");
        }

        match &expr.bare {
            NumLit(num) => self.write(&num.to_string()),
            TextLit(text) => self.write(&text.to_string()),
            Binding(binding) => self.fmt_binder(&binding.0),
            // @Task
            Proj(_proj) => self.write("⟨proj⟩"),
            RecLit(_rec) => self.write("⟨rec-lit⟩"),
            IO(io) => {
                self.write("⟨io ");
                self.write(&io.index.to_string());
                self.write("⟩");

                for arg in &io.args {
                    self.write(" ");
                    self.fmt_expr(arg);
                }
            }
            Substed(substed) => {
                self.write("⟨substed ");
                write!(
                    self.output,
                    "{}",
                    displayed(|f| substed.subst.write(self.sess, f))
                )
                .unwrap();
                self.fmt_expr(&substed.expr);
                self.write("⟩");
            }
            Error(_) => self.write("⟨error⟩"),
            _ => {
                self.write("(");
                self.fmt_expr(expr);
                self.write(")");
            }
        }
    }

    // @Task @Beacon update bracket business
    fn fmt_pat(&mut self, pat: &hir::Pat) {
        use hir::BarePat::*;

        match &pat.bare {
            NumLit(num) => self.write(&num.to_string()),
            TextLit(text) => self.write(&text.to_string()),
            Binding(binding) => self.fmt_binder(&binding.0),
            LetBinding(binder) => {
                self.write("(let ");
                self.write(binder.to_str());
                self.write(")");
            }
            App(app) => {
                self.write("("); // @Temporary
                self.fmt_pat(&app.callee);
                self.write(")"); // @Temporary
                self.write("("); // @Temporary
                self.fmt_pat(&app.arg);
                self.write(")"); // @Temporary
            }
            Error(_) => self.write("⟨error⟩"),
        }
    }

    fn module_url_fragment(&self, index: DeclIdx) -> String {
        let comp = self.sess.comp_of(index);

        let mut segments = comp.local_idx_to_path_segments(index.local_unchecked());
        segments.push_front(comp.name().into_inner());

        format!(
            "{}{}/index.html",
            self.url_prefix,
            segments
                .into_iter()
                .map(Atom::to_str)
                .map(urlencoding::encode)
                .join_with("/")
        )
    }

    fn decl_url_fragment(&self, index: DeclIdx) -> String {
        use hir::EntityKind::*;

        let binder = self.sess[index].src.to_str();

        match self.sess[index].kind {
            Use { .. } => "#".to_string(), // @Task
            Module { .. } => self.module_url_fragment(index),
            Func { .. } | FuncIntr { .. } | DataTy { .. } => {
                let module_link = self.module_url_fragment(self.sess.parent_of(index).unwrap());
                format!("{module_link}#{}", decl_id(binder))
            }
            Ctor { .. } => {
                let owner = self.sess.parent_of(index).unwrap();
                let module_link = self.module_url_fragment(self.sess.parent_of(owner).unwrap());

                format!(
                    "{module_link}#{}",
                    decl_id(&format!("{}.{binder}", self.sess[owner].src))
                )
            }
            _ => unreachable!(),
        }
    }

    fn fmt_binder(&mut self, binder: &hir::Ident) {
        if let Some(index) = binder.decl_idx() {
            let decl_url = self.decl_url_fragment(index);
            let path = self.sess.index_to_path(index);

            Elem::anchor(decl_url, binder.to_str())
                .attr("title", path)
                .render(&mut self.output);
        } else {
            self.write(&binder.to_string());
        }
    }
}
