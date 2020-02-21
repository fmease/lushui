//! The high-level intermediate representation (HIR).
//!
//! This module defines the IR and the lowering from the AST emitted by the parser.
//!
//! ## Issues
//!
//! * lacks span information
//! * and information concerning modules
//! * paths are always simple (inherited by parser)
//! * it should be parametrized, maybe? by phase

mod fmt;
mod identifier;

use freestanding::freestanding;
use std::rc::Rc;

use crate::parser::{self, Explicitness};
pub use identifier::Identifier;

// @Task lower span information

// @Beacon @Beacon @Task add span information!!!
pub enum Declaration {
    Value {
        binder: Identifier,
        type_annotation: Expression,
        expression: Expression,
    },
    Data {
        binder: Identifier,
        type_annotation: Expression,
        constructors: Vec<Constructor>,
    },
    Module {
        declarations: Vec<Declaration>,
    },
    // @Task
    Use,
    Foreign {
        binder: Identifier,
        type_annotation: Expression,
    },
}

/// Lower a declaration from AST to HIR.
pub fn lower_declaration(declaration: parser::Declaration) -> Declaration {
    match declaration {
        parser::Declaration::Value(declaration) => {
            // @Note type_annotation is currently lowered twice
            // @Task remove duplicate work
            // @Temporary
            let mut expression = lower_expression(declaration.expression);

            {
                let mut type_annotation =
                    std::iter::once(lower_expression(declaration.type_annotation.clone()));

                for parameter_group in declaration.parameters.iter().rev() {
                    let parameter = Some(lower_expression(parameter_group.type_annotation.clone()));

                    for binder in parameter_group.parameters.iter().rev() {
                        expression =
                            Expression::LambdaLiteral(Rc::new(expression::LambdaLiteral {
                                parameter: Identifier::Plain(binder.clone()),
                                parameter_type_annotation: parameter.clone(),
                                explicitness: parameter_group.explicitness,
                                body_type_annotation: type_annotation.next(),
                                body: expression,
                            }));
                    }
                }
            }

            Declaration::Value {
                binder: Identifier::Plain(declaration.binder.clone()),
                type_annotation: lower_annotated_parameters(
                    declaration.parameters,
                    declaration.type_annotation,
                ),
                expression,
            }
        }
        parser::Declaration::Data(data) => Declaration::Data {
            binder: Identifier::Plain(data.binder.clone()),
            type_annotation: lower_annotated_parameters(data.parameters, data.type_annotation),
            constructors: data
                .constructors
                .into_iter()
                .map(lower_constructor)
                .collect(),
        },
        parser::Declaration::Module(module) => Declaration::Module {
            declarations: module
                .declarations
                .into_iter()
                .map(lower_declaration)
                .collect(),
        },
        parser::Declaration::Use(_use) => todo!(),
        parser::Declaration::Foreign(declaration) => Declaration::Foreign {
            binder: Identifier::Plain(declaration.binder.clone()),
            type_annotation: lower_annotated_parameters(
                declaration.parameters,
                declaration.type_annotation,
            ),
        },
    }
}

pub struct Constructor {
    pub binder: Identifier,
    pub type_annotation: Expression,
}

/// Lower a constructor from AST to HIR.
fn lower_constructor(constructor: parser::declaration::Constructor) -> Constructor {
    Constructor {
        binder: Identifier::Plain(constructor.binder.clone()),
        type_annotation: lower_annotated_parameters(
            constructor.parameters,
            constructor.type_annotation,
        ),
    }
}

pub use expression::{lower_expression, Expression};

pub mod expression {
    use super::*;

    use std::collections::VecDeque;

    // @Beacon @Beacon @Task add span information!!! @Note @Beacon now, we are in the HIR,
    // there might not be any span information if synthesize HIR nodes (common thing probably)
    // @Note we can also think about **interning** Expressions but not sure if a good idea
    #[freestanding]
    #[streamline(Rc)]
    #[derive(Clone, Debug)]
    pub enum Expression {
        PiTypeLiteral {
            parameter: Option<Identifier>,
            domain: Expression,
            codomain: Expression,
            explicitness: Explicitness,
        },
        Application {
            callee: Expression,
            argument: Expression,
            explicitness: Explicitness,
        },
        TypeLiteral,
        NatTypeLiteral,
        NatLiteral {
            value: crate::Nat,
        },
        TextTypeLiteral,
        TextLiteral {
            value: String,
        },
        Path {
            identifier: Identifier,
        },
        LambdaLiteral {
            parameter: Identifier,
            parameter_type_annotation: Option<Expression>,
            explicitness: Explicitness,
            body_type_annotation: Option<Expression>,
            body: Expression,
        },
        UseIn {},
        CaseAnalysis {
            subject: Expression,
            cases: Vec<Case>,
        },
        UnsaturatedForeignApplication {
            callee: Identifier,
            arguments: VecDeque<Expression>,
        },
    }

    const _: () = assert!(std::mem::size_of::<Expression>() == 16);

    /// Lower an expression from AST to HIR.
    pub fn lower_expression(expression: parser::Expression) -> Expression {
        use parser::ExpressionKind::*;

        match expression.kind {
            PiTypeLiteral(literal) => expr! {
                PiTypeLiteral {
                    parameter: literal.binder.clone().map(Identifier::Plain),
                    domain: lower_expression(literal.parameter),
                    codomain: lower_expression(literal.expression),
                    explicitness: literal.explicitness,
                }
            },
            Application(application) => expr! {
                Application {
                    callee: lower_expression(application.callee),
                    argument: lower_expression(application.argument),
                    explicitness: application.explicitness,
                }
            },
            TypeLiteral => Expression::TypeLiteral,
            NatTypeLiteral => Expression::NatTypeLiteral,
            NatLiteral(literal) => expr! {
                NatLiteral {
                    value: literal.value,
                }
            },
            TextTypeLiteral => Expression::TextTypeLiteral,
            TextLiteral(literal) => expr! {
                TextLiteral {
                    value: literal.value,
                }
            },
            Path(path) => expr! {
                Path {
                    identifier: Identifier::Plain(path.segments),
                }
            },
            LambdaLiteral(literal) => {
                let mut expression = lower_expression(literal.body);

                let mut type_annotation = literal
                    .body_type_annotation
                    .map(lower_expression)
                    .into_iter();

                for parameter_group in literal.parameters.iter().rev() {
                    let parameter = parameter_group
                        .type_annotation
                        .clone()
                        .map(lower_expression);

                    for binder in parameter_group.parameters.iter().rev() {
                        expression = expr! {
                            LambdaLiteral {
                                parameter: Identifier::Plain(binder.clone()),
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
                let mut expression = lower_expression(let_in.expression);

                let mut type_annotation = let_in
                    .type_annotation
                    .map(|expression| lower_expression(expression))
                    .into_iter();

                for parameter_group in let_in.parameters.iter().rev() {
                    let parameter = parameter_group
                        .type_annotation
                        .clone()
                        .map(lower_expression);
                    for binder in parameter_group.parameters.iter().rev() {
                        expression = expr! {
                            LambdaLiteral {
                                parameter: Identifier::Plain(binder.clone()),
                                parameter_type_annotation: parameter.clone(),
                                explicitness: parameter_group.explicitness,
                                body_type_annotation: type_annotation.next(),
                                body: expression,
                            }
                        };
                    }
                }

                expr! {
                    Application {
                        callee: expr! {
                            LambdaLiteral {
                                parameter: Identifier::Plain(let_in.binder),
                                // @Note we cannot simply lower parameters and a type annotation because
                                // in the chain (`->`) of parameters, there might always be one missing and
                                // we don't support partial type annotations yet (using `'_`)
                                // @Temporary @Update @Bug -gy because we ignore above message
                                // @Task verify correct semantics
                                parameter_type_annotation: type_annotation.next(),
                                explicitness: Explicitness::Explicit,
                                body_type_annotation: None,
                                body: lower_expression(let_in.scope),
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
                    // @Task naïvely lowering this, results is worse error messages if the patterns don't introduce the
                    // same bindings, example: `'of Foo 'of Bar x` gives the error `x not defined` which is not *that*
                    // bad but we can do better (like Rust does) and error with `x` not defined in both arms/cases
                    if case_group.patterns.len() > 1 {
                        eprintln!(
                            "(compiler bug warning) contracted cases not thoroughly supported yet"
                        );
                    }

                    for pattern in case_group.patterns {
                        cases.push(Case {
                            pattern: lower_pattern(pattern),
                            body: lower_expression(case_group.expression.clone()),
                        });
                    }
                }

                expr! {
                    CaseAnalysis {
                        subject: lower_expression(case_analysis.expression),
                        cases,
                    }
                }
            }
        }
    }

    impl Path {
        // currently always returns true because we don't support paths yet
        // with more than one segment in it
        /// Amount of path segments is one.
        pub fn is_simple(&self) -> bool {
            true
        }
    }

    #[derive(Clone, Debug)]
    pub struct Case {
        pub pattern: Pattern,
        pub body: Expression,
    }

    // @Task reference-count variants to reduce size of nat patterns
    // (get size of Pattern first)
    #[derive(Debug, Clone)]
    pub enum Pattern {
        NatLiteral(NatLiteral),
        Path {
            path: Path,
            type_annotation: Option<Expression>,
        },
        Application {
            callee: Rc<Pattern>,
            argument: Rc<Pattern>,
        },
    }

    /// Lower a pattern from AST to HIR.
    ///
    /// Currently, [parser::expression::Pattern] and [Pattern] are identical (apart from forgetting span information)!
    fn lower_pattern(pattern: parser::Pattern) -> Pattern {
        match pattern.kind {
            parser::PatternKind::NatLiteral(literal) => Pattern::NatLiteral(NatLiteral {
                value: literal.value,
            }),
            parser::PatternKind::Path(path) => Pattern::Path {
                path: Path {
                    identifier: Identifier::Plain(path.segments),
                },
                type_annotation: path.type_annotation.map(lower_expression),
            },
            parser::PatternKind::Application(application) => Pattern::Application {
                callee: Rc::new(lower_pattern(application.callee)),
                argument: Rc::new(lower_pattern(application.argument)),
            },
        }
    }
}

/// Lower annotated parameters from AST to HIR.
fn lower_annotated_parameters(
    parameters: parser::declaration::AnnotatedParameters,
    type_annotation: parser::Expression,
) -> Expression {
    let mut expression = lower_expression(type_annotation);

    for parameter_group in parameters.into_iter().rev() {
        let parameter = lower_expression(parameter_group.type_annotation);

        for binder in parameter_group.parameters.iter().rev() {
            expression = expr! {
                PiTypeLiteral {
                    parameter: Some(Identifier::Plain(binder.clone())),
                    domain: parameter.clone(),
                    codomain: expression,
                    explicitness: parameter_group.explicitness,
                }
            };
        }
    }

    expression
}

pub(crate) macro expr($kind:ident { $( $body:tt )+ }) {
    Expression::$kind(Rc::new(expression::$kind {
        $( $body )+
    }))
}
