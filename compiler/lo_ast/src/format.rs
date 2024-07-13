//! Formatting routines for the internal textual representation of the [Lo-AST](super).

// FIXME: If we decide to not get rid of Lo-AST in favor of HIR, color the output again.

use lexer::token::INDENTATION;
use std::fmt;
use utility::Atom;

pub trait Display {
    fn write(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

impl Display for super::Decl {
    fn write(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_decl(self, 0, f)
    }
}

// @Task reduce amount of (String) allocations
fn write_decl(decl: &super::Decl, depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use super::BareDecl::*;

    for attr in &decl.attrs.0 {
        writeln!(f, "{}{attr}", " ".repeat(depth * INDENTATION.0))?;
    }

    write!(f, "{}", " ".repeat(depth * INDENTATION.0))?;

    match &decl.bare {
        Func(func) => {
            write!(f, "{}: ", func.binder)?;
            func.ty.write(f)?;
            if let Some(body) = &func.body {
                f.write_str(" = ")?;
                body.write(f)?;
            }
            writeln!(f)
        }
        DataTy(ty) => match &ty.decls {
            Some(decls) => {
                write!(f, "data {}: ", ty.binder)?;
                ty.ty.write(f)?;
                writeln!(f, " of")?;
                for decl in decls {
                    write_decl(decl, depth + 1, f)?;
                }
                Ok(())
            }
            None => {
                write!(f, "data {}: ", ty.binder)?;
                ty.ty.write(f)?;
                writeln!(f)
            }
        },
        Ctor(ctor) => {
            write!(f, "{}: ", ctor.binder)?;
            ctor.ty.write(f)?;
            writeln!(f)
        }
        Module(module) => {
            writeln!(f, "module {} of", module.binder)?;
            for declaration in &module.decls {
                write_decl(declaration, depth + 1, f)?;
            }
            Ok(())
        }
        Use(use_) => writeln!(f, "use {} as {}", use_.target, use_.binder),
        Error(_) => f.write_str("⟨error⟩"),
    }
}

impl Display for super::Expr {
    fn write(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_pi_ty_or_lower(self, f)
    }
}

// @Task display fewer round brackets by making use of precedence
// @Note many wasted allocations (intermediate Strings)
// @Task add sequences!
fn write_pi_ty_or_lower(expr: &super::Expr, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use super::BareExpr::*;

    match &expr.bare {
        PiTy(pi_ty) => {
            if pi_ty.binder.is_none() && pi_ty.kind == ast::ParamKind::Explicit {
                fmt_app_or_lower(&pi_ty.domain, f)?;
            } else {
                f.write_str("For ")?;

                if pi_ty.kind == ast::ParamKind::Implicit {
                    f.write_str("'")?;
                }

                if pi_ty.kind == ast::ParamKind::Context {
                    f.write_str("[")?;

                    if let Some(binder) = &pi_ty.binder {
                        write!(f, "{binder}: ")?;
                    }
                    pi_ty.domain.write(f)?;

                    f.write_str("]")?;
                } else {
                    let binder = pi_ty.binder.map_or(Atom::UNDERSCORE, ast::Ident::bare);

                    write!(f, "({binder}: ")?;
                    pi_ty.domain.write(f)?;
                    f.write_str(")")?;
                }
            }

            f.write_str(" -> ")?;
            write_pi_ty_or_lower(&pi_ty.codomain, f)
        }
        LamLit(lambda) => {
            f.write_str("for ")?;

            if lambda.kind == ast::ParamKind::Implicit {
                write!(f, "'")?;
            }

            if lambda.kind == ast::ParamKind::Context {
                f.write_str("[")?;

                if let Some(binder) = &lambda.binder {
                    write!(f, "{binder}: ")?;
                }

                // Although it's not statically guaranteed, the domain of context parameters has exist.
                // Let's not unwrap though for robustness.
                if let Some(domain) = &lambda.domain {
                    domain.write(f)?;
                }

                f.write_str("]")?;
            } else {
                let binder = lambda.binder.map_or(Atom::UNDERSCORE, ast::Ident::bare);

                if let Some(domain) = &lambda.domain {
                    f.write_str("(")?;
                    write!(f, "{binder}: ")?;
                    domain.write(f)?;
                    f.write_str(")")?;
                } else {
                    write!(f, "{binder}")?;
                }
            }

            if let Some(codomain) = &lambda.codomain {
                f.write_str(": ")?;
                codomain.write(f)?;
            }
            f.write_str(" => ")?;
            lambda.body.write(f)
        }
        // @Task get rid of the delimited blocks, they are no longer part of the surface language!
        CaseAnalysis(analysis) => {
            f.write_str("case ")?;
            analysis.scrutinee.write(f)?;
            f.write_str(" of { ")?;
            let mut first = true;
            for case in &analysis.cases {
                if first {
                    first = false;
                } else {
                    write!(f, "; ")?;
                }

                case.pattern.write(f)?;
                f.write_str(" => ")?;
                case.body.write(f)?;
            }
            f.write_str(" }")
        }
        SeqLit(seq) => {
            if let Some(path) = &seq.path {
                write!(f, "{path}.")?;
            }

            f.write_str("(")?;
            let mut elems = seq.elems.bare.iter();
            if let Some(elem) = elems.next() {
                elem.write(f)?;
            }
            for elem in elems {
                f.write_str(", ")?;
                elem.write(f)?;
            }
            if seq.path.is_none() && seq.elems.bare.len() == 1 {
                f.write_str(",")?;
            }
            f.write_str(")")
        }
        RecLit(rec) => {
            if let Some(path) = &rec.path {
                write!(f, "{path}.")?;
            }
            f.write_str("{")?;
            let mut fields = rec.fields.bare.iter();
            if let Some(field) = fields.next() {
                write!(f, "{} = ", field.binder)?;
                field.body.write(f)?;
            }
            for field in fields {
                write!(f, ", {} = ", field.binder)?;
                field.body.write(f)?;
            }
            f.write_str("}")
        }
        Proj(proj) => {
            fmt_lower_expr(&proj.basis, f)?;
            write!(f, "::{}", proj.field)
        }
        _ => fmt_app_or_lower(expr, f),
    }
}

fn fmt_app_or_lower(expr: &super::Expr, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use super::BareExpr::*;

    match &expr.bare {
        App(app) => {
            fmt_app_or_lower(&app.callee, f)?;

            f.write_str(" ")?;

            if app.kind == ast::ParamKind::Implicit {
                f.write_str("'")?;
            }
            if app.kind == ast::ParamKind::Context {
                f.write_str("[")?;
                app.arg.write(f)?;
                f.write_str("]")
            } else {
                fmt_lower_expr(&app.arg, f)
            }
        }
        _ => fmt_lower_expr(expr, f),
    }
}

fn fmt_lower_expr(expr: &super::Expr, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use super::BareExpr::*;

    for attr in &expr.attrs.0 {
        write!(f, "{attr} ")?;
    }

    match &expr.bare {
        Ty => f.write_str("⟨Type⟩"),
        LocalBinding(_) => f.write_str("⟨_⟩"),
        Wildcard(wildcard) => wildcard.write(f),
        NumLit(num) => write!(f, "{num}"),
        TextLit(text) => write!(f, "{text}"),
        Path(path) => write!(f, "{path}"),
        Error(_) => f.write_str("⟨error⟩"),
        _ => {
            write!(f, "(")?;
            expr.write(f)?;
            write!(f, ")")
        }
    }
}

// @Task respect / incorporate precedence just like we do with expressions already
impl Display for super::Pat {
    fn write(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use super::BarePattern::*;

        match &self.bare {
            Wildcard(wildcard) => wildcard.write(f),
            NumLit(num) => write!(f, "{num}"),
            TextLit(text) => write!(f, "{text}"),
            Path(path) => write!(f, "{path}"),
            LetBinding(binder) => write!(f, "(let {binder})"),
            App(app) => {
                write!(f, "(")?;
                app.callee.write(f)?;
                write!(f, ") (")?;
                app.arg.write(f)?;
                write!(f, ")")
            }
            // @Task abstract over fmt'ing sequence literals (via a function over Item<_>)
            // once we have format_lower_pattern
            SeqLit(seq) => {
                if let Some(path) = &seq.path {
                    write!(f, "{path}.")?;
                }

                write!(f, "[")?;
                let mut elems = seq.elems.bare.iter();
                if let Some(elem) = elems.next() {
                    write!(f, "(")?;
                    elem.write(f)?;
                    write!(f, ")")?;
                }
                for elem in elems {
                    write!(f, "(")?;
                    elem.write(f)?;
                    write!(f, ")")?;
                }
                write!(f, "]")
            }
            Error(_) => f.write_str("⟨error⟩"),
        }
    }
}

impl Display for ast::Wildcard {
    fn write(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Silent => f.write_str("_"),
            Self::Signaling { tag } => write!(f, "?{tag}"),
        }
    }
}
