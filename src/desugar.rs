//! The desugaring stage.
//!
//! Additionally, this phase validates attributes.
//!
//! ## Issues
//!
//! * and information concerning modules
//! * paths are always simple (inherited by parser)
//! * patterns follow old format (should use the format of the parser)

use std::{fmt, iter::once};

use crate::{
    diagnostic::{Code, Diagnostic, Diagnostics, Results},
    hir::{self, decl, expr, pat, Pass},
    parser::{self, AttributeKind, Explicit, Identifier, Path},
    smallvec,
    span::{SourceMap, Spanning},
    support::{pluralize, release, InvalidFallback, ManyErrExt, TrySoftly},
    SmallVec,
};

#[derive(Clone)]
pub enum Desugared {}

impl Pass for Desugared {
    type Binder = Identifier;
    type ReferencedBinder = Path;
    // @Temporary
    type PatternBinder = Path;
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
    ) -> Results<SmallVec<[hir::Declaration<Desugared>; 1]>> {
        use parser::DeclarationKind::*;

        self.validate_attributes()?;

        Ok(match self.kind {
            Value(declaration) => {
                let mut errors = Diagnostics::new();

                let declaration_type_annotation = match declaration.type_annotation {
                    Some(type_annotation) => type_annotation,
                    None => Err(missing_mandatory_type_annotation(
                        &self.span,
                        AnnotationTarget::Declaration,
                    ))
                    .try_softly(&mut errors),
                };

                // @Note type_annotation is currently desugared twice @Task remove duplicate work
                // @Task find a way to use `Option::map` (currently does not work because of
                // partial moves, I hate those), use local bindings
                let expression = match declaration.expression {
                    Some(expression) => {
                        let mut expression = expression.desugar();
                        {
                            let mut type_annotation =
                                once(declaration_type_annotation.clone().desugar());

                            for parameter_group in declaration.parameters.iter().rev() {
                                let parameter_type_annotation =
                                    match &parameter_group.type_annotation {
                                        Some(type_annotation) => type_annotation.clone().desugar(),
                                        None => Err(missing_mandatory_type_annotation(
                                            parameter_group,
                                            AnnotationTarget::Parameters {
                                                amount: parameter_group.parameters.len(),
                                            },
                                        ))
                                        .try_softly(&mut errors),
                                    };

                                for binder in parameter_group.parameters.iter().rev() {
                                    expression = expr! {
                                        Lambda[] {
                                            parameter: binder.clone(),
                                            parameter_type_annotation: Some(parameter_type_annotation.clone()),
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

                let type_annotation = desugar_parameters_to_annotated_ones(
                    declaration.parameters,
                    declaration_type_annotation,
                )
                .try_softly(&mut errors);

                release!(errors);

                smallvec![decl! {
                    Value[self.span][self.attributes] {
                        binder: declaration.binder,
                        type_annotation,
                        expression,
                    }
                }]
            }
            Data(data) => {
                let mut errors = Diagnostics::new();

                let data_type_annotation = match data.type_annotation {
                    Some(type_annotation) => type_annotation,
                    None => Err(missing_mandatory_type_annotation(
                        &self.span,
                        AnnotationTarget::Declaration,
                    ))
                    .try_softly(&mut errors),
                };

                let type_annotation =
                    desugar_parameters_to_annotated_ones(data.parameters, data_type_annotation)
                        .try_softly(&mut errors);

                let constructors = data.constructors.map(|constructors| {
                    constructors
                        .into_iter()
                        .flat_map(|constructor| constructor.desugar(map).try_softly(&mut errors))
                        .collect()
                });

                release!(errors);

                smallvec![decl! {
                    Data[self.span][self.attributes] {
                        binder: data.binder,
                        type_annotation,
                        constructors,
                    }
                }]
            }
            Constructor(constructor) => {
                let mut errors = Diagnostics::new();

                let constructor_type_annotation = match constructor.type_annotation {
                    Some(type_annotation) => type_annotation,
                    None => Err(missing_mandatory_type_annotation(
                        &self.span,
                        AnnotationTarget::Declaration,
                    ))
                    .try_softly(&mut errors),
                };

                let type_annotation = desugar_parameters_to_annotated_ones(
                    constructor.parameters,
                    constructor_type_annotation,
                )
                .try_softly(&mut errors);

                release!(errors);

                smallvec![decl! {
                    Constructor[self.span][self.attributes] {
                        binder: constructor.binder,
                        type_annotation,
                    }
                }]
            }
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
                            Diagnostic::warning()
                                .with_message("attributes on module headers are ignored right now")
                                .emit(None);
                        }
                        module.declarations.unwrap()
                    }
                };

                let mut errors = Diagnostics::new();

                let declarations = declarations
                    .into_iter()
                    .flat_map(|declaration| declaration.desugar(map).try_softly(&mut errors))
                    .collect();

                release!(errors);

                smallvec![decl! {
                    Module[self.span][self.attributes] {
                        binder: module.binder,
                        file: module.file,
                        declarations,
                    }
                }]
            }
            Use(declaration) => {
                use crate::span::Span;
                use parser::{
                    Attributes,
                    UseBindings::{self, *},
                };
                let mut declarations = SmallVec::new();

                fn desugar_multiple_use_bindings(
                    path: Path,
                    bindings: Vec<UseBindings>,
                    span: Span,
                    attributes: &Attributes,
                    declarations: &mut SmallVec<[hir::Declaration<Desugared>; 1]>,
                ) {
                    for binding in bindings {
                        match binding {
                            parser::UseBindings::Single { target, binder } => {
                                // if the binder is not explicitly set, look for the most-specific/last/right-most
                                // identifier of the target but if that one is `self`, look up the last identifier of
                                // the parent path
                                declarations.push(decl! {
                                    Use[span][attributes.clone()] {
                                        binder: binder.or_else(|| if !target.is_self() { &target } else { &path }
                                            .last_identifier().cloned()),
                                        target: path.clone().join(target),
                                    }
                                });
                            }
                            parser::UseBindings::Multiple {
                                path: inner_path,
                                bindings,
                            } => {
                                desugar_multiple_use_bindings(
                                    path.clone().join(inner_path),
                                    bindings,
                                    span,
                                    attributes,
                                    declarations,
                                );
                            }
                        }
                    }
                };

                match declaration.bindings {
                    Single { target, binder } => declarations.push(decl! {
                        Use[self.span][self.attributes] {
                            binder: binder.or_else(|| target.last_identifier().cloned()),
                            target,
                        }
                    }),
                    Multiple { path, bindings } => {
                        desugar_multiple_use_bindings(
                            path,
                            bindings,
                            self.span,
                            &self.attributes,
                            &mut declarations,
                        );
                    }
                }

                declarations
            }
        })
    }

    // @Task validate that attributes are unique (using Attribute::unique)
    fn validate_attributes(&mut self) -> Results<()> {
        use parser::DeclarationKind::*;

        let nonconforming = self.attributes.nonconforming(self.as_attribute_target());

        if !nonconforming.is_empty() {
            return Err(nonconforming
                .into_iter()
                .map(|attribute| {
                    Diagnostic::fatal()
                        .with_code(Code::E013)
                        .with_message(format!(
                            "{} cannot be ascribed to a {} declaration",
                            attribute.kind,
                            self.kind_as_str()
                        ))
                        .with_labeled_span(attribute, "misplaced attribute")
                        .with_labeled_span(self, "incompatible declaration")
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

            return Err(Diagnostic::fatal()
                .with_code(Code::E014)
                .with_message("attributes `foreign` and `inherent` are mutually exclusive")
                .with_span(&max(foreign.span, inherent.span))
                .with_span(&min(foreign.span, inherent.span)))
            .many_err();
        }

        let abnormally_bodiless = match &self.kind {
            Value(value) => value.expression.is_none(),
            Data(data) => data.constructors.is_none(),
            _ => false,
        };

        match (abnormally_bodiless, foreign.is_some()) {
            (true, false) => Err(Diagnostic::fatal()
                .with_code(Code::E012)
                .with_message("declaration without a definition")
                .with_span(self))
            .many_err(),
            (false, true) => {
                // @Task make non-fatal, @Task improve message
                // @Task add subdiagonstic note: foreign is one definition and
                // explicit body is another
                Err(Diagnostic::fatal()
                    .with_code(Code::E020)
                    .with_message("declaration has multiple definitions")
                    .with_labeled_span(self, "is foreign and has a body"))
                .many_err()
            }
            (true, true) | (false, false) => Ok(()),
        }
    }
}

impl InvalidFallback for SmallVec<[hir::Declaration<Desugared>; 1]> {
    fn invalid() -> Self {
        SmallVec::new()
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
            NumberLiteral(literal) => expr! { Number[self.span](literal) },
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
                                explicitness: Explicit,
                                body_type_annotation: None,
                                body: let_in.scope.desugar(),
                            }
                        },
                        argument: expression,
                        explicitness: Explicit,
                    }
                }
            }
            UseIn => todo!(),
            CaseAnalysis(case_analysis) => {
                let mut cases = Vec::new();

                for case_group in case_analysis.cases {
                    cases.push(hir::Case {
                        pattern: case_group.pattern.desugar(),
                        body: case_group.expression.clone().desugar(),
                    });
                }

                expr! {
                    CaseAnalysis[self.span] {
                        subject: case_analysis.expression.desugar(),
                        cases,
                    }
                }
            }
            Invalid => InvalidFallback::invalid(),
        }
    }
}

impl parser::Pattern {
    /// Lower a pattern from AST to HIR.
    fn desugar(self) -> hir::Pattern<Desugared> {
        use parser::PatternKind::*;

        match self.kind {
            NumberLiteral(literal) => pat! { Number[self.span](literal) },
            TextLiteral(literal) => pat! {
                Text[self.span] {
                    value: literal.value,
                }
            },
            Path(path) => pat! {
                Binding[self.span] {
                    binder: *path,
                }
            },
            Binder(binding) => pat! {
                Binder[self.span] {
                    binder: binding.binder,
                }
            },
            Deapplication(application) => pat! {
                Deapplication[self.span] {
                    callee: application.callee.desugar(),
                    argument: application.argument.desugar(),
                }
            },
        }
    }
}

/// Lower annotated parameters from AST to HIR.
fn desugar_parameters_to_annotated_ones(
    parameters: parser::Parameters,
    type_annotation: parser::Expression,
) -> Results<hir::Expression<Desugared>> {
    let mut expression = type_annotation.desugar();

    let mut errors = Diagnostics::new();

    for parameter_group in parameters.parameters.into_iter().rev() {
        let parameter_type_annotation = match parameter_group.type_annotation {
            Some(type_annotation) => type_annotation.desugar(),
            None => Err(missing_mandatory_type_annotation(
                &parameter_group,
                AnnotationTarget::Parameters {
                    amount: parameter_group.parameters.len(),
                },
            ))
            .try_softly(&mut errors),
        };

        for binder in parameter_group.parameters.iter().rev() {
            expression = expr! {
                PiType[] {
                    parameter: Some(binder.clone()),
                    domain: parameter_type_annotation.clone(),
                    codomain: expression,
                    explicitness: parameter_group.explicitness,
                }
            };
        }
    }

    release!(errors);

    Ok(expression)
}

fn missing_mandatory_type_annotation(
    spanning: &impl Spanning,
    target: AnnotationTarget,
) -> Diagnostic {
    Diagnostic::error()
        .with_code(Code::E015)
        .with_message(format!("missing mandatory type annotation on {}", target))
        .with_span(spanning)
}

enum AnnotationTarget {
    Parameters { amount: usize },
    Declaration,
}

impl fmt::Display for AnnotationTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parameters { amount } => {
                write!(f, "{}", pluralize(*amount, "parameter", || "parameters"))
            }
            Self::Declaration => write!(f, "declaration"),
        }
    }
}
