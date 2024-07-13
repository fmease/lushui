use crate::Lowerer;
use ast::{LocalBinder, ParamKind::Explicit};
use diagnostics::{error::PossiblyErroneous, Diag, ErrorCode, Substitution};
use span::{Span, Spanning};

impl Lowerer<'_> {
    /// Lower an expression.
    pub(crate) fn lower_expr(&mut self, expr: ast::Expr) -> lo_ast::Expr {
        use ast::BareExpr::*;

        let attrs = self.lower_attrs(&expr.attrs, &expr, ());

        match expr.bare {
            Wildcard(wildcard) => lo_ast::Expr::new(attrs, expr.span, wildcard.into()),
            NumLit(num) => lo_ast::Expr::new(attrs, expr.span, num.into()),
            TextLit(text) => lo_ast::Expr::new(attrs, expr.span, text.into()),
            Path(path) => lo_ast::Expr::new(attrs, expr.span, path.into()),
            App(app) => {
                if let Some(binder) = &app.binder {
                    Diag::error()
                        .message("named arguments are not supported yet")
                        .unlabeled_span(binder)
                        .handle(&mut *self);
                }

                let callee = self.lower_expr(app.callee);
                let argument = self.lower_expr(app.arg);

                lo_ast::Expr::new(
                    attrs,
                    expr.span,
                    lo_ast::App {
                        callee,
                        arg: argument,
                        kind: app.kind,
                    }
                    .into(),
                )
            }
            Proj(proj) => lo_ast::Expr::new(
                attrs,
                expr.span,
                lo_ast::Proj {
                    basis: self.lower_expr(proj.basis),
                    field: proj.field,
                }
                .into(),
            ),
            QuantifiedTy(ty) => self.lower_quantified_ty(*ty, expr.span),
            LamLit(lambda) => self.lower_lam_lit(*lambda, expr.span),
            LetBinding(binding) => self.lower_let_binding(*binding),
            UseBinding(_binding) => Diag::error()
                .message("use-bindings are not supported yet")
                .unlabeled_span(expr.span)
                .embed(&mut *self),
            CaseAnalysis(analysis) => {
                let mut cases = Vec::new();

                for case in analysis.cases {
                    cases.push(lo_ast::Case {
                        pattern: self.lower_pat(case.pattern),
                        body: self.lower_expr(case.body.clone()),
                    });
                }

                let scrutinee = self.lower_expr(analysis.scrutinee);

                lo_ast::Expr::new(
                    attrs,
                    expr.span,
                    lo_ast::CaseAnalysis { scrutinee, cases }.into(),
                )
            }
            DoBlock(_block) => Diag::error()
                .message("do blocks are not supported yet")
                .unlabeled_span(expr.span)
                .embed(&mut *self),
            SeqLit(seq) => lo_ast::Expr::new(
                attrs,
                expr.span,
                ast::SeqLit {
                    path: seq.path,
                    elems: seq.elems.map(|elements| {
                        elements
                            .into_iter()
                            .map(|element| self.lower_expr(element))
                            .collect()
                    }),
                }
                .into(),
            ),
            RecLit(rec) => lo_ast::Expr::new(
                attrs,
                expr.span,
                lo_ast::RecLit {
                    path: rec.path,
                    fields: rec.fields.map(|fields| {
                        fields
                            .into_iter()
                            .map(|field| lo_ast::Field {
                                binder: field.binder,
                                body: field.body.map_or(
                                    lo_ast::Expr::common(
                                        field.binder.span(),
                                        ast::Path::from(field.binder).into(),
                                    ),
                                    |expr| self.lower_expr(expr),
                                ),
                            })
                            .collect()
                    }),
                    base: rec.base.map(|base| self.lower_expr(base)),
                }
                .into(),
            ),
            Error(error) => PossiblyErroneous::error(error),
        }
    }

    fn lower_quantified_ty(&mut self, ty: ast::QuantifiedTy, span: Span) -> lo_ast::Expr {
        match ty.quantifier {
            ast::Quantifier::Pi => {
                // @Task transfer expression.attributes to lowered form in some way or the other.
                // Not clear yet whether I should duplicate across all lowered pi types or do sth. else.

                let mut codomain = self.lower_expr(ty.codomain);

                for param in ty.params.into_iter().rev() {
                    let domain = self.lower_param_ty_with_default(
                        param.bare.ty,
                        param.bare.kind,
                        param.span,
                    );

                    codomain = lo_ast::Expr::common(
                        span,
                        lo_ast::PiTy {
                            kind: param.bare.kind,
                            binder: param.bare.binder.and_then(LocalBinder::name),
                            domain,
                            codomain,
                        }
                        .into(),
                    );
                }

                codomain
            }
            // @Beacon @Task add UI test for this
            ast::Quantifier::Sigma => Diag::error()
                .message("sigma-type expressions are not supported yet")
                .unlabeled_span(span)
                .embed(&mut *self),
        }
    }

    fn lower_lam_lit(&mut self, lambda: ast::LamLit, span: Span) -> lo_ast::Expr {
        // @Task transfer expression.attributes to lowered form in some way or the other.
        // Not clear yet whether I should duplicate across all lowered lambdas or do sth. else.

        let mut body = self.lower_expr(lambda.body);

        let mut codomain = lambda.codomain.map(|ty| self.lower_expr(ty)).into_iter();

        for param in lambda.params.into_iter().rev() {
            let domain = param.bare.ty.map(|ty| self.lower_expr(ty));

            body = lo_ast::Expr::common(
                span,
                lo_ast::LamLit {
                    binder: param.bare.binder.and_then(LocalBinder::name),
                    domain,
                    kind: param.bare.kind,
                    codomain: codomain.next(),
                    body,
                }
                .into(),
            );
        }

        body
    }

    fn lower_let_binding(&mut self, binding: ast::LetBinding) -> lo_ast::Expr {
        let body = {
            let binder = &binding.binder;

            match binding.body {
                Some(body) => body,
                None => {
                    let span = binder
                        .span()
                        .fit_end(&binding.params)
                        .fit_end(&binding.ty)
                        .end();

                    Diag::error()
                        .code(ErrorCode::E012)
                        .message(format!("the let-binding ‘{binder}’ does not have a body"))
                        .span(span, "missing definition")
                        .suggest(
                            span,
                            "provide a definition for the let-binding",
                            Substitution::from(" = ").placeholder("value"),
                        )
                        .embed(&mut *self)
                }
            }
        };
        let mut body = self.lower_expr(body);

        let mut ty = binding.ty.map(|ty| self.lower_expr(ty)).into_iter();

        for param in binding.params.into_iter().rev() {
            let domain = param.bare.ty.map(|ty| self.lower_expr(ty));

            // @Bug don't create bare expression, use a span from somewhere
            body = lo_ast::Expr::bare(
                lo_ast::LamLit {
                    binder: param.bare.binder.and_then(LocalBinder::name),
                    domain,
                    kind: param.bare.kind,
                    codomain: ty.next(),
                    body,
                }
                .into(),
            );
        }

        let scope = self.lower_expr(binding.scope);

        // @Bug @Task transfer the attributes & span from the let-binding to the lambda(s)
        lo_ast::Expr::bare(
            lo_ast::App {
                callee: lo_ast::Expr::bare(
                    lo_ast::LamLit {
                        binder: binding.binder.name(),
                        // @Note we cannot simply lower parameters and a type annotation because
                        // in the chain (`->`) of parameters, there might always be one missing and
                        // we don't support partial type annotations yet (using `_`)
                        // @Temporary @Update @Bug -gy because we ignore above message
                        // @Task verify correct semantics
                        // @Beacon @Beacon @Beacon @Update lower those to *silent wildcards* and keep going!
                        domain: ty.next(),
                        kind: Explicit,
                        codomain: None,
                        body: scope,
                    }
                    .into(),
                ),
                arg: body,
                kind: Explicit,
            }
            .into(),
        )
    }

    pub(crate) fn lower_param_ty_with_default(
        &mut self,
        ty: Option<ast::Expr>,
        kind: ast::ParamKind,
        span: Span,
    ) -> lo_ast::Expr {
        // @Temporary get rid of this constant entirely once we support bidirectional type-inference
        const BIDIR_TYCK: bool = false;

        match ty {
            Some(ty) => self.lower_expr(ty),
            None => lo_ast::Expr::common(
                span,
                match kind {
                    ast::ParamKind::Implicit if BIDIR_TYCK => ast::Wildcard::Silent.into(),
                    _ => lo_ast::BareExpr::Ty,
                },
            ),
        }
    }
}
