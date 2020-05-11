//! The desugaring stage.
//!
//! Additionally, this phase validates attributes.
//!
//! ## Issues
//!
//! * and information concerning modules
//! * paths are always simple (inherited by parser)
//! * patterns follow old format (should use the format of the parser)

use std::iter::once;

use crate::{
    diagnostic::{Code, Diagnostic, Diagnostics, Level, Result},
    hir::{self, decl, expr, pat, Pass},
    parser::{self, AttributeKind, Explicitness, Identifier, Path},
    span::SourceMap,
    support::ManyErrExt,
};

#[derive(Clone)]
pub enum Desugared {}

impl Pass for Desugared {
    type Binder = Identifier;
    type ReferencedBinder = Path;
    type PatternBinder = parser::PathPattern;
    type ForeignApplicationBinder = !;
}

impl parser::Declaration {
    /// Desugar a declaration from AST.
    ///
    /// Also, filters documentation attributes and validates
    /// foreign attributes. Those checks should probably be moved somewhere
    /// else.
    pub fn desugar(
        mut self,
        map: &mut SourceMap,
    ) -> Result<hir::Declaration<Desugared>, Diagnostics> {
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
                            .map(|constructor| constructor.desugar(map))
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
            Module(module) => {
                let declarations = match module.declarations {
                    Some(declarations) => declarations,
                    None => {
                        let path = std::path::Path::new(&module.file.name)
                            .ancestors()
                            .nth(1)
                            .unwrap()
                            .join(&format!("{}.{}", module.binder, crate::FILE_EXTENSION));
                        let file = map.load(path.to_str().unwrap()).many_err()?;
                        let tokens = crate::lexer::Lexer::new(&file).lex()?;
                        let node = parser::Parser::new(file, &tokens)
                            .parse_top_level(module.binder.clone())
                            .many_err()?;
                        let module = match node.kind {
                            Module(module) => module,
                            _ => unreachable!(),
                        };
                        if !node.attributes.is_empty() {
                            Diagnostic::new(
                                Level::Warning,
                                None,
                                "attributes on module headers are ignored right now",
                            )
                            .emit(None);
                        }
                        module.declarations.unwrap()
                    }
                };

                decl! {
                    Module[self.span][self.attributes] {
                        binder: module.binder,
                        file: module.file,
                        // @Task cumulate non-fatal errors (there are none here yet, thus it's not acute)
                        // @Task write a "HandleTwo" for an arbitrary amount of elements
                        // @Update @Question how about using into_iter().fold(...handle)
                        declarations: declarations.into_iter()
                                .map(|declaration| declaration.desugar(map))
                                .collect::<Result<_, _>>()?,
                    }
                }
            }
            // @Task improve upon this
            Use(declaration) => decl! {
                Use[self.span][self.attributes] {
                    binder: declaration.path.segments.last().cloned(),
                    reference: declaration.path,
                }
            },
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
    pub fn desugar(self) -> hir::Expression<Desugared> {
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
                    binder: *path,
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
                            pattern: pattern.desugar(),
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

impl parser::Pattern {
    /// Lower a pattern from AST to HIR.
    ///
    /// Currently, [parser::expression::Pattern] and [Pattern] are identical (apart from forgetting span information)!
    fn desugar(self) -> hir::Pattern<Desugared> {
        use parser::PatternKind::*;

        match self.kind {
            NatLiteralPattern(literal) => pat! {
                NatPattern[self.span] {
                    value: literal.value,
                }
            },
            PathPattern(path) => pat! {
                BindingPattern[self.span] {
                    binder: path.as_ref().clone(),
                    type_annotation: path.type_annotation.map(parser::Expression::desugar),
                }
            },
            ApplicationPattern(application) => pat! {
                ApplicationPattern[self.span] {
                    callee: application.callee.desugar(),
                    argument: application.argument.desugar(),
                }
            },
        }
    }
}

/// Lower annotated parameters from AST to HIR.
fn desugar_annotated_parameters(
    parameters: parser::AnnotatedParameters,
    type_annotation: parser::Expression,
) -> hir::Expression<Desugared> {
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
