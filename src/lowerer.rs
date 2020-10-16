//! The lowering stage.
//!
//! Higher-level syntactic constructs in the AST are simplified and rewritten
//! in terms of lower-level language primitives.
//!
//! Additionally among other things, this phase validates attributes and checks if the mandatory
//! type annotations are present.

pub mod lowered_ast;

use crate::{
    ast::{self, Explicit, Path},
    diagnostic::{Code, Diagnostic, Diagnostics, Result, Results},
    smallvec,
    span::{SourceMap, Spanning},
    support::{accumulate_errors::*, pluralize, release, InvalidFallback, ManyErrExt, TrySoftly},
    SmallVec,
};
use lowered_ast::{decl, expr, pat};
use std::{fmt, iter::once};

/// The state of the lowering pass.
pub struct Lowerer<'a> {
    map: &'a mut SourceMap,
    warnings: &'a mut Diagnostics,
}

impl<'a> Lowerer<'a> {
    pub fn new(map: &'a mut SourceMap, warnings: &'a mut Diagnostics) -> Self {
        Self { map, warnings }
    }

    #[allow(dead_code)]
    fn warn(&mut self, diagnostic: Diagnostic) {
        self.warnings.insert(diagnostic);
    }

    /// Lower a declaration.
    ///
    /// Also, filters documentation attributes and validates
    /// foreign attributes. Those checks should probably be moved somewhere
    /// else.
    pub fn lower_declaration(
        &mut self,
        mut declaration: ast::Declaration,
    ) -> Results<SmallVec<lowered_ast::Declaration, 1>> {
        use ast::DeclarationKind::*;

        let mut errors = Diagnostics::default();

        if let Err(attribute_errors) = Self::validate_attributes(&mut declaration) {
            errors.extend(attribute_errors);
        }

        Ok(match declaration.kind {
            Value(value) => {
                let declaration_type_annotation = match value.type_annotation {
                    Some(type_annotation) => type_annotation,
                    None => Err(missing_mandatory_type_annotation(
                        &declaration.span,
                        AnnotationTarget::Declaration,
                    ))
                    .try_softly(&mut errors),
                };

                // @Note type_annotation is currently lowered twice @Task remove duplicate work
                // @Task find a way to use `Option::map` (currently does not work because of
                // partial moves, I hate those), use local bindings
                let expression = match value.expression {
                    Some(expression) => {
                        let mut expression =
                            self.lower_expression(expression).try_softly(&mut errors);
                        {
                            let mut type_annotation = once(
                                self.lower_expression(declaration_type_annotation.clone())
                                    .try_softly(&mut errors),
                            );

                            for parameter_group in value.parameters.iter().rev() {
                                let parameter_type_annotation =
                                    match &parameter_group.type_annotation {
                                        Some(type_annotation) => {
                                            self.lower_expression(type_annotation.clone())
                                        }
                                        None => Err(missing_mandatory_type_annotation(
                                            parameter_group,
                                            AnnotationTarget::Parameters {
                                                amount: parameter_group.parameters.len(),
                                            },
                                        ))
                                        .many_err(),
                                    }
                                    .try_softly(&mut errors);

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

                let type_annotation = self
                    .lower_parameters_to_annotated_ones(
                        value.parameters,
                        declaration_type_annotation,
                    )
                    .try_softly(&mut errors);

                release!(errors);

                smallvec![decl! {
                    Value[declaration.span][declaration.attributes] {
                        binder: value.binder,
                        type_annotation,
                        expression,
                    }
                }]
            }
            Data(data) => {
                let data_type_annotation = match data.type_annotation {
                    Some(type_annotation) => type_annotation,
                    None => Err(missing_mandatory_type_annotation(
                        &declaration.span,
                        AnnotationTarget::Declaration,
                    ))
                    .try_softly(&mut errors),
                };

                let type_annotation = self
                    .lower_parameters_to_annotated_ones(data.parameters, data_type_annotation)
                    .try_softly(&mut errors);

                let constructors = data.constructors.map(|constructors| {
                    constructors
                        .into_iter()
                        .flat_map(|constructor| {
                            self.lower_declaration(constructor).try_softly(&mut errors)
                        })
                        .collect()
                });

                release!(errors);

                smallvec![decl! {
                    Data[declaration.span][declaration.attributes] {
                        binder: data.binder,
                        type_annotation,
                        constructors,
                    }
                }]
            }
            Constructor(constructor) => {
                let constructor_type_annotation = match constructor.type_annotation {
                    Some(type_annotation) => type_annotation,
                    None => Err(missing_mandatory_type_annotation(
                        &declaration.span,
                        AnnotationTarget::Declaration,
                    ))
                    .try_softly(&mut errors),
                };

                let type_annotation = self
                    .lower_parameters_to_annotated_ones(
                        constructor.parameters,
                        constructor_type_annotation,
                    )
                    .try_softly(&mut errors);

                // @Temporary
                // @Task verify there is only a single constructor
                // @Task implement
                if constructor.record {
                    errors.insert(
                        Diagnostic::bug()
                            .with_message("records not supported yet")
                            .with_span(&declaration.span),
                    );
                }

                release!(errors);

                smallvec![decl! {
                    Constructor[declaration.span][declaration.attributes] {
                        binder: constructor.binder,
                        type_annotation,
                    }
                }]
            }
            Module(module) => {
                let declarations =
                    match module.declarations {
                        Some(declarations) => declarations,
                        // @Bug @Task disallow external module declarations inside of non-file modules
                        None => {
                            use crate::{lexer::Lexer, parser::Parser, span};

                            let path = module
                                .file
                                .path
                                .parent()
                                .unwrap()
                                .join(module.binder.as_str())
                                .with_extension(crate::FILE_EXTENSION);

                            let declaration_span = declaration.span;

                            let file = self
                                .map
                                .load(&path)
                                .map_err(|error| match error {
                                    span::Error::LoadFailure(_) => Diagnostic::error()
                                        .with_code(Code::E016)
                                        .with_message(format!(
                                            "could not load module `{}`",
                                            module.binder
                                        ))
                                        .with_span(&declaration_span)
                                        .with_note(error.message(Some(&path))),
                                    // @Task add context information
                                    error => error.into(),
                                })
                                .many_err()?;

                            let tokens = Lexer::new(&file, &mut self.warnings).lex()?;
                            let node = Parser::new(file, &tokens, &mut self.warnings)
                                .parse_top_level(module.binder.clone())
                                .many_err()?;
                            let module = match node.kind {
                                Module(module) => module,
                                _ => unreachable!(),
                            };
                            // @Temporary
                            if !node.attributes.is_empty() {
                                self.warn(Diagnostic::warning().with_message(
                                    "attributes on module headers are ignored right now",
                                ))
                            }
                            module.declarations.unwrap()
                        }
                    };

                let declarations = declarations
                    .into_iter()
                    .flat_map(|declaration| {
                        self.lower_declaration(declaration).try_softly(&mut errors)
                    })
                    .collect();

                release!(errors);

                smallvec![decl! {
                    Module[declaration.span][declaration.attributes] {
                        binder: module.binder,
                        file: module.file,
                        declarations,
                    }
                }]
            }
            // @Temporary
            Crate(_) => {
                return Err(Diagnostic::bug()
                    .with_message("crate declarations not supported yet")
                    .with_span(&declaration.span))
                .many_err()
            }
            // @Beacon @Task merge information (exposure list and attributes) with parent module
            // (through a `Context`) and ensure it's the first declaration in the whole module
            Header(_) => {
                return Err(Diagnostic::bug()
                    .with_message("module headers not supported yet")
                    .with_span(&declaration.span))
                .many_err()
            }
            // @Question (language specification) should the overarching attributes be placed
            // *above* or *below* the subdeclaration attributes? (currently, it's above)
            // depends on the order of attribute evaluation we haven't offically specified yet
            Group(group) => {
                let attributes = declaration.attributes.clone();

                let declarations = group
                    .declarations
                    .into_iter()
                    .map(|mut declaration| {
                        let mut attributes = attributes.clone();
                        attributes.append(&mut declaration.attributes);
                        declaration.attributes = attributes;
                        declaration
                    })
                    .flat_map(|declaration| {
                        self.lower_declaration(declaration).try_softly(&mut errors)
                    })
                    .collect();

                release!(errors);

                declarations
            }
            Use(use_) => {
                use crate::span::Span;

                use ast::{
                    Attributes,
                    PathTree::{self, *},
                };

                let mut declarations = SmallVec::new();

                fn lower_path_tree_multiple_paths(
                    path: Path,
                    bindings: Vec<PathTree>,
                    span: Span,
                    attributes: &Attributes,
                    declarations: &mut SmallVec<lowered_ast::Declaration, 1>,
                ) -> Result<()> {
                    for binding in bindings {
                        match binding {
                            ast::PathTree::Single { target, binder } => {
                                // if the binder is not explicitly set, look for the most-specific/last/right-most
                                // identifier of the target but if that one is `self`, look up the last identifier of
                                // the parent path
                                declarations.push(decl! {
                                    Use[span][attributes.clone()] {
                                        binder: binder.or_else(|| if !target.is_self() { &target } else { &path }
                                            .last_identifier().cloned()),
                                        target: path.clone().join(target)?,
                                    }
                                });
                            }
                            ast::PathTree::Multiple {
                                path: inner_path,
                                bindings,
                            } => {
                                lower_path_tree_multiple_paths(
                                    path.clone().join(inner_path)?,
                                    bindings,
                                    span,
                                    attributes,
                                    declarations,
                                )?;
                            }
                        }
                    }

                    Ok(())
                };

                match use_.bindings {
                    Single { target, binder } => declarations.push(decl! {
                        Use[declaration.span][declaration.attributes] {
                            binder: binder.or_else(|| target.last_identifier().cloned()),
                            target,
                        }
                    }),
                    Multiple { path, bindings } => {
                        lower_path_tree_multiple_paths(
                            path,
                            bindings,
                            declaration.span,
                            &declaration.attributes,
                            &mut declarations,
                        )
                        .many_err()?;
                    }
                }

                declarations
            }
        })
    }

    // @Task validate that attributes are unique (using Attribute::unique)
    fn validate_attributes(declaration: &mut ast::Declaration) -> Results<()> {
        use ast::{AttributeKind::*, DeclarationKind::*};

        let nonconforming = declaration
            .attributes
            .nonconforming(declaration.as_attribute_target());

        if !nonconforming.is_empty() {
            return Err(nonconforming
                .into_iter()
                .map(|attribute| {
                    Diagnostic::error()
                        .with_code(Code::E013)
                        .with_message(format!(
                            "{} cannot be ascribed to a {} declaration",
                            attribute.kind,
                            declaration.kind_as_str()
                        ))
                        .with_labeled_span(attribute, "misplaced attribute")
                        .with_labeled_span(declaration, "incompatible declaration")
                })
                .collect());
        }

        // not further used, unless in documenting mode (which is unimplemented)
        declaration
            .attributes
            .retain(|attribute| !matches!(attribute.kind, Documentation));

        // @Temporary
        let unsupported = declaration.attributes.filter(|attribute| {
            [Moving, Unstable, Deprecated, If, Allow, Warn, Deny, Forbid].contains(&attribute.kind)
        });

        if !unsupported.is_empty() {
            return Err(unsupported
                .into_iter()
                .map(|attribute| {
                    Diagnostic::error()
                        .with_message(format!("{} is not supported yet", attribute.kind,))
                        .with_span(attribute)
                })
                .collect());
        }

        let foreign = declaration.attributes.get(Foreign);
        let inherent = declaration.attributes.get(Inherent);

        if let (Some(foreign), Some(inherent)) = (foreign, inherent) {
            use std::cmp::{max, min};

            return Err(Diagnostic::error()
                .with_code(Code::E014)
                .with_message("attributes `foreign` and `inherent` are mutually exclusive")
                .with_span(&max(foreign.span, inherent.span))
                .with_span(&min(foreign.span, inherent.span)))
            .many_err();
        }

        let abnormally_bodiless = match &declaration.kind {
            Value(value) => value.expression.is_none(),
            Data(data) => data.constructors.is_none(),
            _ => false,
        };

        match (abnormally_bodiless, foreign.is_some()) {
            (true, false) => Err(Diagnostic::error()
                .with_code(Code::E012)
                .with_message("declaration without a definition")
                .with_span(declaration)
                .with_help("provide a definition for the declaration: `= VALUE`"))
            .many_err(),
            (false, true) => {
                // @Task make non-fatal, @Task improve message
                // @Task add subdiagonstic note: foreign is one definition and
                // explicit body is another
                Err(Diagnostic::error()
                    .with_code(Code::E020)
                    .with_message("declaration has multiple definitions")
                    .with_labeled_span(declaration, "is foreign and has a body"))
                .many_err()
            }
            (true, true) | (false, false) => Ok(()),
        }
    }

    /// Lower an expression from AST to HIR.
    // @Question should we provide methods on Lowerer which abstract over TrySoftly?
    pub fn lower_expression(
        &mut self,
        expression: ast::Expression,
    ) -> Results<lowered_ast::Expression> {
        use ast::ExpressionKind::*;

        Ok(match expression.kind {
            PiTypeLiteral(pi) => {
                let (domain, codomain) = (
                    self.lower_expression(pi.domain),
                    self.lower_expression(pi.codomain),
                )
                    .accumulate_err()?;

                expr! {
                    PiType[expression.span] {
                        parameter: pi.binder.clone(),
                        domain,
                        codomain,
                        explicitness: pi.explicitness,
                    }
                }
            }
            Application(application) => {
                // @Temporary
                if let Some(binder) = &application.binder {
                    self.warn(
                        Diagnostic::warning()
                            .with_message("named arguments not supported yet")
                            .with_span(binder),
                    )
                }

                let (callee, argument) = (
                    self.lower_expression(application.callee),
                    self.lower_expression(application.argument),
                )
                    .accumulate_err()?;

                expr! {
                    Application[expression.span] {
                        callee,
                        argument,
                        explicitness: application.explicitness,
                    }
                }
            }
            TypeLiteral => expr! { Type[expression.span] },
            NumberLiteral(literal) => expr! { Number[expression.span](literal) },
            TextLiteral(text) => expr! {
                Text[expression.span](text)
            },
            TypedHole(_hole) => todo!("typed holes not supported yet"),
            Path(path) => expr! {
                Binding[expression.span] {
                    binder: *path,
                }
            },
            LambdaLiteral(lambda) => {
                let mut errors = Diagnostics::default();

                let mut expression = self.lower_expression(lambda.body).try_softly(&mut errors);

                let mut type_annotation = lambda
                    .body_type_annotation
                    .map(|type_annotation| {
                        self.lower_expression(type_annotation)
                            .try_softly(&mut errors)
                    })
                    .into_iter();

                for parameter_group in lambda.parameters.iter().rev() {
                    let parameter =
                        parameter_group
                            .type_annotation
                            .clone()
                            .map(|type_annotation| {
                                self.lower_expression(type_annotation)
                                    .try_softly(&mut errors)
                            });

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

                release!(errors);

                expression
            }
            LetIn(let_in) => {
                let mut errors = Diagnostics::default();

                let mut expression = self
                    .lower_expression(let_in.expression)
                    .try_softly(&mut errors);

                let mut type_annotation = let_in
                    .type_annotation
                    .map(|type_annotation| {
                        self.lower_expression(type_annotation)
                            .try_softly(&mut errors)
                    })
                    .into_iter();

                for parameter_group in let_in.parameters.iter().rev() {
                    let parameter = parameter_group.type_annotation.clone().map(|expression| {
                        self.lower_expression(expression).try_softly(&mut errors)
                    });

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

                let body = self.lower_expression(let_in.scope).try_softly(&mut errors);

                release!(errors);

                expr! {
                    Application[] {
                        callee: expr! {
                            Lambda[] {
                                parameter: let_in.binder,
                                // @Note we cannot simply lower parameters and a type annotation because
                                // in the chain (`->`) of parameters, there might always be one missing and
                                // we don't support partial type annotations yet (using `'_`)
                                // @Temporary @Update @Bug -gy because we ignore above message
                                // @Task verify correct semantics
                                parameter_type_annotation: type_annotation.next(),
                                explicitness: Explicit,
                                body_type_annotation: None,
                                body,
                            }
                        },
                        argument: expression,
                        explicitness: Explicit,
                    }
                }
            }
            // @Beacon @Task
            UseIn(_use_in) => {
                return Err(Diagnostic::bug()
                    .with_message("use/in expressions not supported yet")
                    .with_span(&expression.span))
                .many_err()
            }
            CaseAnalysis(analysis) => {
                let mut errors = Diagnostics::default();
                let mut cases = Vec::new();

                for case_group in analysis.cases {
                    cases.push(lowered_ast::Case {
                        pattern: self
                            .lower_pattern(case_group.pattern)
                            .try_softly(&mut errors),
                        body: self
                            .lower_expression(case_group.expression.clone())
                            .try_softly(&mut errors),
                    });
                }

                let subject = self
                    .lower_expression(analysis.expression)
                    .try_softly(&mut errors);

                release!(errors);

                expr! {
                    CaseAnalysis[expression.span] {
                        subject,
                        cases,
                    }
                }
            }
            DoBlock(_block) => {
                return Err(Diagnostic::bug()
                    .with_message("do blocks not fully implemented yet")
                    .with_span(&expression.span))
                .many_err()
            }
            SequenceLiteral(_sequence) => {
                return Err(Diagnostic::bug()
                    .with_message("sequence literals not fully implemented yet")
                    .with_span(&expression.span))
                .many_err();
            }
            Invalid => InvalidFallback::invalid(),
        })
    }

    /// Lower a pattern from AST to HIR.
    fn lower_pattern(&mut self, pattern: ast::Pattern) -> Results<lowered_ast::Pattern> {
        use ast::PatternKind::*;

        Ok(match pattern.kind {
            NumberLiteral(literal) => pat! {
                Number[pattern.span](literal)
            },
            TextLiteral(literal) => pat! {
                Text[pattern.span](literal)
            },
            Path(path) => pat! {
                Binding[pattern.span] {
                    binder: *path,
                }
            },
            Binder(binding) => pat! {
                Binder[pattern.span] {
                    binder: binding.binder,
                }
            },
            Deapplication(application) => {
                // @Temporary
                if let Some(binder) = &application.binder {
                    self.warn(
                        Diagnostic::warning()
                            .with_message("named arguments not supported yet")
                            .with_span(binder),
                    )
                }

                let (callee, argument) = (
                    self.lower_pattern(application.callee),
                    self.lower_pattern(application.argument),
                )
                    .accumulate_err()?;

                pat! {
                    Deapplication[pattern.span] {
                        callee,
                        argument,
                    }
                }
            }
            SequenceLiteralPattern(_sequence) => {
                return Err(Diagnostic::bug()
                    .with_message("sequence literal patterns not supported yet")
                    .with_span(&pattern.span))
                .many_err()
            }
        })
    }

    /// Lower annotated parameters from AST to HIR.
    fn lower_parameters_to_annotated_ones(
        &mut self,
        parameters: ast::Parameters,
        type_annotation: ast::Expression,
    ) -> Results<lowered_ast::Expression> {
        let mut errors = Diagnostics::default();

        let mut expression = self
            .lower_expression(type_annotation)
            .try_softly(&mut errors);

        for parameter_group in parameters.parameters.into_iter().rev() {
            let parameter_type_annotation = match parameter_group.type_annotation {
                Some(type_annotation) => self.lower_expression(type_annotation),
                None => Err(missing_mandatory_type_annotation(
                    &parameter_group,
                    AnnotationTarget::Parameters {
                        amount: parameter_group.parameters.len(),
                    },
                ))
                .many_err(),
            }
            .try_softly(&mut errors);

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
}

fn missing_mandatory_type_annotation(
    spanning: &impl Spanning,
    target: AnnotationTarget,
) -> Diagnostic {
    Diagnostic::error()
        .with_code(Code::E015)
        .with_message(format!("missing mandatory type annotation on {}", target))
        .with_span(spanning)
        .with_help(format!(
            "provide a type annotation for the {}: `: TYPE`",
            target
        ))
}

/// Used for error reporting.
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

impl InvalidFallback for SmallVec<lowered_ast::Declaration, 1> {
    fn invalid() -> Self {
        SmallVec::new()
    }
}
