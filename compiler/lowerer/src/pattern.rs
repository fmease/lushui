use crate::Lowerer;
use diagnostics::Diagnostic;

impl Lowerer<'_> {
    /// Lower a pattern.
    pub(crate) fn lower_pattern(&mut self, pattern: ast::Pattern) -> lo_ast::Pattern {
        use ast::BarePattern::*;

        let attributes = self.lower_attributes(&pattern.attributes, &pattern, ());

        match pattern.bare {
            Wildcard(wildcard) => lo_ast::Pattern::new(attributes, pattern.span, wildcard.into()),
            NumberLiteral(literal) => {
                lo_ast::Pattern::new(attributes, pattern.span, literal.into())
            }
            TextLiteral(literal) => lo_ast::Pattern::new(attributes, pattern.span, literal.into()),
            Path(path) => lo_ast::Pattern::new(attributes, pattern.span, path.into()),
            LetBinding(binder) => lo_ast::Pattern::new(attributes, pattern.span, binder.into()),
            Application(application) => {
                if let Some(binder) = &application.binder {
                    Diagnostic::error()
                        .message("named arguments are not supported yet")
                        .unlabeled_span(binder)
                        .handle(&mut *self);
                }

                let callee = self.lower_pattern(application.callee);
                let argument = self.lower_pattern(application.argument);

                lo_ast::Pattern::new(
                    attributes,
                    pattern.span,
                    lo_ast::Application { callee, kind: application.kind, argument }.into(),
                )
            }
            SequenceLiteral(sequence) => lo_ast::Pattern::new(
                attributes,
                pattern.span,
                ast::SequenceLiteral {
                    path: sequence.path,
                    elements: sequence.elements.map(|elements| {
                        elements.into_iter().map(|element| self.lower_pattern(element)).collect()
                    }),
                }
                .into(),
            ),
            // @Beacon @Task
            RecordLiteral(_record) => Diagnostic::error()
                .message("record literals are not support yet")
                .unlabeled_span(pattern.span)
                .embed(&mut *self),
        }
    }
}
