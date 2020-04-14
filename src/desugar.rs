//! The desugaring stage.
//!
//! Additionally, this phase validates attributes.
//!
//! ## Issues
//!
//! * and information concerning modules
//! * paths are always simple (inherited by parser)
//! * patterns follow old format (should use the format of the parser)

use std::{iter::once, rc::Rc};

use crate::{
    diagnostic::{Code, Diagnostic, Diagnostics, Level, Result},
    hir::{self, decl, expr},
    parser::{self, AttributeKind, Explicitness, Identifier},
};

// @Note later: Path/Vec<Segment>
impl hir::Binder for parser::Identifier {}

impl parser::Declaration {
    /// Desugar a declaration from AST.
    ///
    /// Also, filters documentation attributes and validates
    /// foreign attributes. Those checks should probably be moved somewhere
    /// else.
    pub fn desugar(mut self) -> Result<hir::Declaration<Identifier>, Diagnostics> {
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
                                        Lambda[] {
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

                decl! {
                    Value[self.span][self.attributes] {
                        binder: declaration.binder,
                        type_annotation: desugar_annotated_parameters(
                            declaration.parameters,
                            declaration.type_annotation,
                        ),
                        expression,
                    }
                }
            }
            Data(data) => decl! {
                Data[self.span][self.attributes] {
                    binder: data.binder,
                    type_annotation: desugar_annotated_parameters(
                        data.parameters,
                        data.type_annotation,
                    ),
                    constructors: data.constructors.map(|constructors| {
                        constructors
                            .into_iter()
                            .map(parser::Declaration::desugar)
                            .collect()
                    }).transpose()?,
                }
            },
            Constructor(constructor) => decl! {
                Constructor[self.span][self.attributes] {
                    binder: constructor.binder,
                    type_annotation: desugar_annotated_parameters(constructor.parameters, constructor.type_annotation),
                }
            },
            // @Task cumulate non-fatal errors (there are none here yet, thus it's not acute)
            Module(module) => decl! {
                Module[self.span][self.attributes] {
                    binder: module.binder,
                    declarations: module
                        .declarations
                        .map(|declarations| declarations.into_iter()
                        .map(parser::Declaration::desugar)
                        .collect::<Result<_, _>>()).transpose()?,
                }
            },
            Use => todo!(),
        })
    }

    // @Task validate that attributes are unique (using Attribute::unique)
    fn validate_attributes(&mut self) -> Result<(), Diagnostics> {
        use parser::DeclarationKind::*;

        let nonconforming = self.attributes.nonconforming(self.as_attribute_target());

        if !nonconforming.is_empty() {
            return Err(nonconforming
                .into_iter()
                .map(|attribute| {
                    let message = format!(
                        "{} cannot be ascribed to a {} declaration",
                        attribute.kind,
                        self.kind_as_str()
                    );
                    Diagnostic::new(Level::Fatal, Code::E013, message)
                        .with_labeled_span(attribute.span, "misplaced attribute")
                        .with_labeled_span(self.span, "incompatible declaration")
                })
                .collect());
        }

        // not further used, unless in documenting mode (which is unimplemented)
        self.attributes
            .retain(|attribute| !matches!(attribute.kind, AttributeKind::Documentation));

        let foreign = self.attributes.get(AttributeKind::Foreign);
        let inherent = self.attributes.get(AttributeKind::Inherent);

        if let (Some(foreign), Some(inherent)) = (foreign, inherent) {
            use std::cmp::{max, min};

            return Err(vec![Diagnostic::new(
                Level::Fatal,
                Code::E014,
                "attributes `foreign` and `inherent` are mutually exclusive",
            )
            .with_span(max(foreign.span, inherent.span))
            .with_span(min(foreign.span, inherent.span))]);
        }

        let abnormally_bodiless = match &self.kind {
            Value(value) => value.expression.is_none(),
            Data(data) => data.constructors.is_none(),
            _ => false,
        };

        match (abnormally_bodiless, foreign.is_some()) {
            (true, false) => Err(vec![Diagnostic::new(
                Level::Fatal,
                Code::E012,
                "declaration without a definition",
            )
            .with_span(self.span)]),
            (false, true) => {
                // @Task make non-fatal, @Task improve message
                // @Task add subdiagonstic note: foreign is one definition and
                // explicit body is another
                Err(vec![Diagnostic::new(
                    Level::Fatal,
                    Code::E020,
                    "declaration has multiple definitions",
                )
                .with_labeled_span(self.span, "is foreign and has a body")])
            }
            (true, true) | (false, false) => Ok(()),
        }
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
                            Lambda[] {
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
                            Lambda[] {
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
                    Application[] {
                        callee: expr! {
                            Lambda[] {
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
                PiType[] {
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
