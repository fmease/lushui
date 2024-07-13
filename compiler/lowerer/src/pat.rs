use crate::Lowerer;
use diagnostics::Diag;

impl Lowerer<'_> {
    /// Lower a pattern.
    pub(crate) fn lower_pat(&mut self, pat: ast::Pat) -> lo_ast::Pat {
        use ast::BarePat::*;

        let attrs = self.lower_attrs(&pat.attrs, &pat, ());

        match pat.bare {
            Wildcard(wildcard) => lo_ast::Pat::new(attrs, pat.span, wildcard.into()),
            NumLit(num) => lo_ast::Pat::new(attrs, pat.span, num.into()),
            TextLit(text) => lo_ast::Pat::new(attrs, pat.span, text.into()),
            Path(path) => lo_ast::Pat::new(attrs, pat.span, path.into()),
            LetBinding(binder) => lo_ast::Pat::new(attrs, pat.span, binder.into()),
            App(app) => {
                if let Some(binder) = &app.binder {
                    Diag::error()
                        .message("named arguments are not supported yet")
                        .unlabeled_span(binder)
                        .handle(&mut *self);
                }

                let callee = self.lower_pat(app.callee);
                let arg = self.lower_pat(app.arg);

                lo_ast::Pat::new(
                    attrs,
                    pat.span,
                    lo_ast::App {
                        callee,
                        kind: app.kind,
                        arg,
                    }
                    .into(),
                )
            }
            SeqLit(seq) => lo_ast::Pat::new(
                attrs,
                pat.span,
                ast::SeqLit {
                    path: seq.path,
                    elems: seq.elems.map(|elements| {
                        elements
                            .into_iter()
                            .map(|element| self.lower_pat(element))
                            .collect()
                    }),
                }
                .into(),
            ),
            // @Beacon @Task
            RecLit(_rec) => Diag::error()
                .message("record literals are not support yet")
                .unlabeled_span(pat.span)
                .embed(&mut *self),
        }
    }
}
