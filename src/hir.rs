//! HIR — high-level intermediate representation

mod fmt;
mod identifier;

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

    // @Task @Beacon @Beacon use freestanding
    // @Beacon @Beacon @Task add span information!!! @Note @Beacon now, we are in the HIR,
    // there might not be any span information if synthesize HIR nodes (common thing probably)
    // @Note we can also think about **interning** Expressions but not sure if a good idea
    #[derive(Clone, Debug)]
    pub enum Expression {
        PiTypeLiteral(Rc<PiTypeLiteral>),
        Application(Rc<Application>),
        TypeLiteral,
        NatTypeLiteral,
        NatLiteral(Rc<NatLiteral>),
        Path(Rc<Path>),
        Hole(Rc<Hole>),
        LambdaLiteral(Rc<LambdaLiteral>),
        UseIn(Rc<UseIn>),
        CaseAnalysis(Rc<CaseAnalysis>),
        UnsaturatedForeignApplication(Rc<UnsaturatedForeignApplication>),
    }

    const _: () = assert!(std::mem::size_of::<Expression>() == 16);

    pub fn lower_expression(expression: parser::Expression) -> Expression {
        use parser::Expression::*;

        match expression {
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
            TypeLiteral(_literal) => Expression::TypeLiteral,
            NatTypeLiteral(_literal) => Expression::NatTypeLiteral,
            NatLiteral(literal) => expr! {
                NatLiteral {
                    value: literal.value,
                }
            },
            Path(path) => expr! {
                Path {
                    identifier: Identifier::Plain(path.inner),
                }
            },
            Hole(hole) => expr! {
                Hole {
                    tag: Identifier::Plain(hole.tag),
                }
            },
            LambdaLiteral(literal) => {
                let mut expression = lower_expression(literal.body);

                let mut type_annotation = literal
                    .body_type_annotation
                    .map(|expression| lower_expression(expression))
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
                                parameter_type_annotation: None,
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
            UseIn(_use_in) => todo!(),
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
                        cases.push(CaseAnalysisCase {
                            pattern: lower_pattern(pattern),
                            expression: lower_expression(case_group.expression.clone()),
                        });
                    }
                }

                expr! {
                    CaseAnalysis {
                        expression: lower_expression(case_analysis.expression),
                        cases,
                    }
                }
            }
        }
    }

    #[derive(Clone, Debug)]
    pub struct PiTypeLiteral {
        pub parameter: Option<Identifier>,
        pub domain: Expression,
        pub codomain: Expression,
        pub explicitness: Explicitness,
    }

    #[derive(Clone, Debug)]
    pub struct Application {
        pub callee: Expression,
        pub argument: Expression,
        pub explicitness: Explicitness,
    }

    #[derive(Clone, Debug)]
    pub struct NatLiteral {
        pub value: crate::lexer::Nat,
    }

    // @Note don't reference-count because it already uses interning
    #[derive(Clone, Debug)]
    pub struct Path {
        pub identifier: Identifier,
    }

    impl Path {
        // currently always returns true because we don't support paths yet
        // with more than one segment in it
        /// Amount of path segments is one.
        pub fn is_simple(&self) -> bool {
            true
        }
    }

    // @Note don't reference-count because it already uses interning
    #[derive(Clone, Debug)]
    pub struct Hole {
        pub tag: Identifier,
    }

    #[derive(Clone, Debug)]
    pub struct LambdaLiteral {
        pub parameter: Identifier,
        pub parameter_type_annotation: Option<Expression>,
        pub explicitness: Explicitness,
        pub body_type_annotation: Option<Expression>,
        pub body: Expression,
    }

    #[derive(Clone, Debug)]
    pub struct UseIn {}

    #[derive(Clone, Debug)]
    pub struct CaseAnalysis {
        pub expression: Expression,
        pub cases: Vec<CaseAnalysisCase>,
    }

    #[derive(Clone, Debug)]
    pub struct CaseAnalysisCase {
        pub pattern: Pattern,
        pub expression: Expression,
    }

    #[derive(Clone, Debug)]
    pub struct UnsaturatedForeignApplication {
        pub callee: Identifier,
        pub arguments: Vec<Expression>,
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

    // @Note currently, parser::expression::Pattern and Pattern are identical!
    // (apart from forgetting span information)
    // this means this function costs a lot of memory and time but is currently useless
    fn lower_pattern(pattern: parser::expression::Pattern) -> Pattern {
        match pattern {
            parser::expression::Pattern::NatLiteral(literal) => Pattern::NatLiteral(NatLiteral {
                value: literal.value,
            }),
            parser::expression::Pattern::Path {
                path,
                type_annotation,
                ..
            } => Pattern::Path {
                path: Path {
                    identifier: Identifier::Plain(path.inner),
                },
                type_annotation: type_annotation.map(lower_expression),
            },
            parser::expression::Pattern::Application {
                callee, argument, ..
            } => Pattern::Application {
                callee: Rc::new(lower_pattern(*callee)),
                argument: Rc::new(lower_pattern(*argument)),
            },
        }
    }
}

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
