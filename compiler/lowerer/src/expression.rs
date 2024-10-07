use crate::Lowerer;
use ast::{LocalBinder, ParameterKind::Explicit};
use diagnostics::{Diagnostic, ErrorCode, Substitution, error::PossiblyErroneous};
use span::{Span, Spanning};

impl Lowerer<'_> {
    /// Lower an expression.
    pub(crate) fn lower_expression(&mut self, expression: ast::Expression) -> lo_ast::Expression {
        use ast::BareExpression::*;

        let attributes = self.lower_attributes(&expression.attributes, &expression, ());

        match expression.bare {
            Wildcard(wildcard) => {
                lo_ast::Expression::new(attributes, expression.span, wildcard.into())
            }
            NumberLiteral(number) => {
                lo_ast::Expression::new(attributes, expression.span, number.into())
            }
            TextLiteral(text) => lo_ast::Expression::new(attributes, expression.span, text.into()),
            Path(path) => lo_ast::Expression::new(attributes, expression.span, path.into()),
            Application(application) => {
                if let Some(binder) = &application.binder {
                    Diagnostic::error()
                        .message("named arguments are not supported yet")
                        .unlabeled_span(binder)
                        .handle(&mut *self);
                }

                let callee = self.lower_expression(application.callee);
                let argument = self.lower_expression(application.argument);

                lo_ast::Expression::new(
                    attributes,
                    expression.span,
                    lo_ast::Application { callee, argument, kind: application.kind }.into(),
                )
            }
            Projection(projection) => lo_ast::Expression::new(
                attributes,
                expression.span,
                lo_ast::Projection {
                    basis: self.lower_expression(projection.basis),
                    field: projection.field,
                }
                .into(),
            ),
            QuantifiedType(type_) => self.lower_quantified_type(*type_, expression.span),
            LambdaLiteral(lambda) => self.lower_lambda_literal(*lambda, expression.span),
            LetBinding(binding) => self.lower_let_binding(*binding),
            UseBinding(_binding) => Diagnostic::error()
                .message("use-bindings are not supported yet")
                .unlabeled_span(expression.span)
                .embed(&mut *self),
            CaseAnalysis(analysis) => {
                let mut cases = Vec::new();

                for case in analysis.cases {
                    cases.push(lo_ast::Case {
                        pattern: self.lower_pattern(case.pattern),
                        body: self.lower_expression(case.body.clone()),
                    });
                }

                let scrutinee = self.lower_expression(analysis.scrutinee);

                lo_ast::Expression::new(
                    attributes,
                    expression.span,
                    lo_ast::CaseAnalysis { scrutinee, cases }.into(),
                )
            }
            DoBlock(_block) => Diagnostic::error()
                .message("do blocks are not supported yet")
                .unlabeled_span(expression.span)
                .embed(&mut *self),
            SequenceLiteral(sequence) => lo_ast::Expression::new(
                attributes,
                expression.span,
                ast::SequenceLiteral {
                    path: sequence.path,
                    elements: sequence.elements.map(|elements| {
                        elements.into_iter().map(|element| self.lower_expression(element)).collect()
                    }),
                }
                .into(),
            ),
            RecordLiteral(record) => lo_ast::Expression::new(
                attributes,
                expression.span,
                lo_ast::RecordLiteral {
                    path: record.path,
                    fields: record.fields.map(|fields| {
                        fields
                            .into_iter()
                            .map(|field| lo_ast::Field {
                                binder: field.binder,
                                body: field.body.map_or(
                                    lo_ast::Expression::common(
                                        field.binder.span(),
                                        ast::Path::from(field.binder).into(),
                                    ),
                                    |expression| self.lower_expression(expression),
                                ),
                            })
                            .collect()
                    }),
                    base: record.base.map(|base| self.lower_expression(base)),
                }
                .into(),
            ),
            Error(error) => PossiblyErroneous::error(error),
        }
    }

    fn lower_quantified_type(
        &mut self,
        type_: ast::QuantifiedType,
        span: Span,
    ) -> lo_ast::Expression {
        match type_.quantifier {
            ast::Quantifier::Pi => {
                // @Task transfer expression.attributes to lowered form in some way or the other.
                // Not clear yet whether I should duplicate across all lowered pi types or do sth. else.

                let mut codomain = self.lower_expression(type_.codomain);

                for parameter in type_.parameters.into_iter().rev() {
                    let binder = parameter.bare.binder.and_then(LocalBinder::name);
                    let kind = parameter.bare.kind;
                    let domain = self.lower_parameter_type(parameter);

                    codomain = lo_ast::Expression::common(
                        span,
                        lo_ast::PiType { kind, binder, domain, codomain }.into(),
                    );
                }

                codomain
            }
            // @Beacon @Task add UI test for this
            ast::Quantifier::Sigma => Diagnostic::error()
                .message("sigma-type expressions are not supported yet")
                .unlabeled_span(span)
                .embed(&mut *self),
        }
    }

    fn lower_lambda_literal(
        &mut self,
        lambda: ast::LambdaLiteral,
        span: Span,
    ) -> lo_ast::Expression {
        // @Task transfer expression.attributes to lowered form in some way or the other.
        // Not clear yet whether I should duplicate across all lowered lambdas or do sth. else.

        let mut body = self.lower_expression(lambda.body);

        let mut codomain = lambda.codomain.map(|type_| self.lower_expression(type_)).into_iter();

        for parameter in lambda.parameters.into_iter().rev() {
            let domain = parameter.bare.type_.map(|type_| self.lower_expression(type_));

            body = lo_ast::Expression::common(
                span,
                lo_ast::Lambda {
                    binder: parameter.bare.binder.and_then(LocalBinder::name),
                    domain,
                    kind: parameter.bare.kind,
                    codomain: codomain.next(),
                    body,
                }
                .into(),
            );
        }

        body
    }

    fn lower_let_binding(&mut self, binding: ast::LetBinding) -> lo_ast::Expression {
        let body = {
            let binder = &binding.binder;

            match binding.body {
                Some(body) => body,
                None => {
                    let span =
                        binder.span().fit_end(&binding.parameters).fit_end(&binding.type_).end();

                    Diagnostic::error()
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
        let mut body = self.lower_expression(body);

        let mut type_ = binding.type_.map(|type_| self.lower_expression(type_)).into_iter();

        for parameter in binding.parameters.into_iter().rev() {
            let domain = parameter.bare.type_.map(|type_| self.lower_expression(type_));

            // @Bug don't create bare expression, use a span from somewhere
            body = lo_ast::Expression::bare(
                lo_ast::Lambda {
                    binder: parameter.bare.binder.and_then(LocalBinder::name),
                    domain,
                    kind: parameter.bare.kind,
                    codomain: type_.next(),
                    body,
                }
                .into(),
            );
        }

        let scope = self.lower_expression(binding.scope);

        // @Bug @Task transfer the attributes & span from the let-binding to the lambda(s)
        lo_ast::Expression::bare(
            lo_ast::Application {
                callee: lo_ast::Expression::bare(
                    lo_ast::Lambda {
                        binder: binding.binder.name(),
                        // @Note we cannot simply lower parameters and a type annotation because
                        // in the chain (`->`) of parameters, there might always be one missing and
                        // we don't support partial type annotations yet (using `_`)
                        // @Temporary @Update @Bug -gy because we ignore above message
                        // @Task verify correct semantics
                        // @Beacon @Beacon @Beacon @Update lower those to *silent wildcards* and keep going!
                        domain: type_.next(),
                        kind: Explicit,
                        codomain: None,
                        body: scope,
                    }
                    .into(),
                ),
                argument: body,
                kind: Explicit,
            }
            .into(),
        )
    }

    pub(crate) fn lower_parameter_type(&mut self, parameter: ast::Parameter) -> lo_ast::Expression {
        // @Temporary get rid of this constant entirely once we support bidirectional type-inference
        const BIDIR_TYCK: bool = false;

        match parameter.bare.type_ {
            Some(type_) => self.lower_expression(type_),
            None => lo_ast::Expression::common(parameter.span.end(), match parameter.bare.kind {
                ast::ParameterKind::Implicit if BIDIR_TYCK => ast::Wildcard::Silent.into(),
                _ => lo_ast::BareExpression::Type,
            }),
        }
    }
}
