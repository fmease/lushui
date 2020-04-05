//! The desugaring stage.
//!
//! ## Issues
//!
//! * and information concerning modules
//! * paths are always simple (inherited by parser)
//! * patterns follow old format (should use the format of the parser)

use std::{iter::once, rc::Rc};

use crate::{
    diagnostic::{Code, Diagnostic, Level, Result},
    hir::{self, expr},
    parser::{self, AttributeKind, Explicitness, Identifier},
    span::Span,
};

// @Note later: Path/Vec<Segment>
impl hir::Binder for parser::Identifier {}

impl parser::Declaration {
    /// Desugar a declaration from AST.
    ///
    /// Also, filters documentation attributes and validates
    /// foreign attributes. Those checks should probably be moved somewhere
    /// else.
    pub fn desugar(mut self) -> Result<hir::Declaration<Identifier>> {
        use parser::DeclarationKind::*;

        self.validate_attributes()?;

        Ok(match self.kind {
            Value(declaration) => {
                // @Note type_annotation is currently desugared twice
                // @Task remove duplicate work
                // @Task find a way to use `Option::map` (currently does not work because of
                // partial moves, I hate those), use local bindings
                let expression = match declaration.expression {
                    Some(expression) => {
                        let mut expression = expression.desugar();
                        {
                            let mut type_annotation =
                                once(declaration.type_annotation.clone().desugar());
                            for parameter_group in declaration.parameters.iter().rev() {
                                let parameter =
                                    Some(parameter_group.type_annotation.clone().desugar());
                                for binder in parameter_group.parameters.iter().rev() {
                                    expression = expr! {
                                        Lambda[Span::dummy()] {
                                            parameter: binder.clone(),
                                            parameter_type_annotation: parameter.clone(),
                                            explicitness: parameter_group.explicitness,
                                            body_type_annotation: type_annotation.next(),
                                            body: expression,
                                        }
                                    }
                                }
                            }
                        }
                        Some(expression)
                    }
                    None => None,
                };

                hir::Declaration {
                    span: self.span,
                    kind: hir::DeclarationKind::Value(Box::new(hir::Value {
                        binder: declaration.binder,
                        type_annotation: desugar_annotated_parameters(
                            declaration.parameters,
                            declaration.type_annotation,
                        ),
                        expression,
                    })),
                    attributes: self.attributes,
                }
            }
            Data(data) => hir::Declaration {
                span: self.span,
                kind: hir::DeclarationKind::Data(Box::new(hir::Data {
                    binder: data.binder,
                    type_annotation: desugar_annotated_parameters(
                        data.parameters,
                        data.type_annotation,
                    ),
                    constructors: data.constructors.map(|constructors| {
                        constructors
                            .into_iter()
                            .map(parser::Constructor::desugar)
                            .collect()
                    }),
                })),
                attributes: self.attributes,
            },
            // @Task cumulate non-fatal errors (there are none here yet, thus it's not acute)
            Module(module) => hir::Declaration {
                span: self.span,
                kind: hir::DeclarationKind::Module(Box::new(hir::Module {
                    declarations: module
                        .declarations
                        .into_iter()
                        .map(parser::Declaration::desugar)
                        .collect::<Result<_, _>>()?,
                })),
                attributes: self.attributes,
            },
            Use => todo!(),
        })
    }

    // @Task validate that attributes are unique (e.g. there aren't multiple `_foreign_`s)
    // @Task use a more principled approach
    // @Task ensure `_foreign_` is not used on Module and Use (check against `target` (not yet
    // defined))
    fn validate_attributes(&mut self) -> Result<()> {
        use parser::DeclarationKind::*;

        self.attributes
            .retain(|attribute| !matches!(attribute.kind, AttributeKind::Documentation));

        let (bodiless, foreign) = match &self.kind {
            Value(value) => (
                value.expression.is_none(),
                self.attributes.has(AttributeKind::Foreign),
            ),
            Data(data) => (
                data.constructors.is_none(),
                self.attributes.has(AttributeKind::Foreign),
            ),
            _ => (false, false),
        };

        match (bodiless, foreign) {
            (true, false) => {
                Err(
                    Diagnostic::new(Level::Fatal, Code::E012, "declaration without a definition")
                        .with_span(self.span),
                )
            }
            (false, true) => {
                // @Task make non-fatal, @Task improve message
                // @Task add subdiagonstic note: foreign is one definition and
                // explicit body is another
                Err(Diagnostic::new(
                    Level::Fatal,
                    Code::E020,
                    "declaration has multiple definitions",
                )
                .with_labeled_span(self.span, "is foreign and has a body"))
            }
            (true, true) | (false, false) => Ok(()),
        }
    }
}

impl parser::Constructor {
    /// Desugar a constructor from AST.
    fn desugar(mut self) -> hir::Constructor<Identifier> {
        self.validate_attributes();

        hir::Constructor {
            binder: self.binder,
            type_annotation: desugar_annotated_parameters(self.parameters, self.type_annotation),
            span: self.span,
            attributes: self.attributes,
        }
    }

    // @Task ensure uniqueness and that the target matches (e.g. disallow `_foreign_` here)
    fn validate_attributes(&mut self) {
        self.attributes
            .retain(|attribute| !matches!(attribute.kind, AttributeKind::Documentation));
    }
}

impl parser::Expression {
    /// Lower an expression from AST to HIR.
    pub fn desugar(self) -> hir::Expression<Identifier> {
        use parser::ExpressionKind::*;

        match self.kind {
            PiTypeLiteral(pi) => expr! {
                PiType[self.span] {
                    parameter: pi.binder.clone(),
                    domain: pi.parameter.desugar(),
                    codomain: pi.expression.desugar(),
                    explicitness: pi.explicitness,
                }
            },
            Application(application) => expr! {
                Application[self.span] {
                    callee: application.callee.desugar(),
                    argument: application.argument.desugar(),
                    explicitness: application.explicitness,
                }
            },
            TypeLiteral => expr! { Type[self.span] },
            NatLiteral(literal) => expr! {
                Nat[self.span] {
                    value: literal.value,
                }
            },
            TextLiteral(text) => expr! {
                Text[self.span] {
                    value: text.value,
                }
            },
            Path(path) => expr! {
                Binding[self.span] {
                    binder: path.segments,
                }
            },
            LambdaLiteral(lambda) => {
                let mut expression = lambda.body.desugar();

                let mut type_annotation = lambda
                    .body_type_annotation
                    .map(parser::Expression::desugar)
                    .into_iter();

                for parameter_group in lambda.parameters.iter().rev() {
                    let parameter = parameter_group
                        .type_annotation
                        .clone()
                        .map(parser::Expression::desugar);

                    for binder in parameter_group.parameters.iter().rev() {
                        expression = expr! {
                            Lambda[Span::dummy()] {
                                parameter: binder.clone(),
                                parameter_type_annotation: parameter.clone(),
                                explicitness: parameter_group.explicitness,
                                body_type_annotation: type_annotation.next(),
                                body: expression,
                            }
                        };
                    }
                }
                expression
            }
            LetIn(let_in) => {
                let mut expression = let_in.expression.desugar();

                let mut type_annotation = let_in
                    .type_annotation
                    .map(|expression| expression.desugar())
                    .into_iter();

                for parameter_group in let_in.parameters.iter().rev() {
                    let parameter = parameter_group
                        .type_annotation
                        .clone()
                        .map(parser::Expression::desugar);
                    for binder in parameter_group.parameters.iter().rev() {
                        expression = expr! {
                            Lambda[Span::dummy()] {
                                parameter: binder.clone(),
                                parameter_type_annotation: parameter.clone(),
                                explicitness: parameter_group.explicitness,
                                body_type_annotation: type_annotation.next(),
                                body: expression,
                            }
                        };
                    }
                }

                expr! {
                    Application[Span::dummy()] {
                        callee: expr! {
                            Lambda[Span::dummy()] {
                                parameter: let_in.binder,
                                // @Note we cannot simply desugar parameters and a type annotation because
                                // in the chain (`->`) of parameters, there might always be one missing and
                                // we don't support partial type annotations yet (using `'_`)
                                // @Temporary @Update @Bug -gy because we ignore above message
                                // @Task verify correct semantics
                                parameter_type_annotation: type_annotation.next(),
                                explicitness: Explicitness::Explicit,
                                body_type_annotation: None,
                                body: let_in.scope.desugar(),
                            }
                        },
                        argument: expression,
                        explicitness: Explicitness::Explicit,
                    }
                }
            }
            UseIn => todo!(),
            CaseAnalysis(case_analysis) => {
                let mut cases = Vec::new();

                for case_group in case_analysis.cases {
                    // @Task naïvely desugaring this, results is worse error messages if the patterns don't introduce the
                    // same bindings, example: `'of Foo 'of Bar x` gives the error `x not defined` which is not *that*
                    // bad but we can do better (like Rust does) and error with `x` not defined in both arms/cases
                    if case_group.patterns.len() > 1 {
                        Diagnostic::new(
                            Level::Warning,
                            Code::W000,
                            "contracted cases not thoroughly supported yet",
                        )
                        .emit(None);
                    }

                    for pattern in case_group.patterns {
                        cases.push(hir::Case {
                            pattern: desugar_pattern(pattern),
                            body: case_group.expression.clone().desugar(),
                        });
                    }
                }

                expr! {
                    CaseAnalysis[self.span] {
                        subject: case_analysis.expression.desugar(),
                        cases,
                    }
                }
            }
        }
    }
}

/// Lower a pattern from AST to HIR.
///
/// Currently, [parser::expression::Pattern] and [Pattern] are identical (apart from forgetting span information)!
fn desugar_pattern(pattern: parser::Pattern) -> hir::Pattern<Identifier> {
    use parser::PatternKind::*;

    match pattern.kind {
        NatLiteralPattern(literal) => hir::Pattern::Nat(hir::Nat {
            value: literal.value,
        }),
        PathPattern(path) => hir::Pattern::Binding {
            binder: hir::Binding {
                binder: path.segments,
            },
            type_annotation: path.type_annotation.map(parser::Expression::desugar),
        },
        ApplicationPattern(application) => hir::Pattern::Application {
            callee: Rc::new(desugar_pattern(application.callee)),
            argument: Rc::new(desugar_pattern(application.argument)),
        },
    }
}

/// Lower annotated parameters from AST to HIR.
fn desugar_annotated_parameters(
    parameters: parser::AnnotatedParameters,
    type_annotation: parser::Expression,
) -> hir::Expression<Identifier> {
    let mut expression = type_annotation.desugar();

    for parameter_group in parameters.into_iter().rev() {
        let parameter = parameter_group.type_annotation.desugar();

        for binder in parameter_group.parameters.iter().rev() {
            expression = expr! {
                PiType[Span::dummy()] {
                    parameter: Some(binder.clone()),
                    domain: parameter.clone(),
                    codomain: expression,
                    explicitness: parameter_group.explicitness,
                }
            };
        }
    }

    expression
}
