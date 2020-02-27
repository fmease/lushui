//! The desugared AST.
//!
//! ## Issues
//!
//! * and information concerning modules
//! * paths are always simple (inherited by parser)
//! * patterns follow old format (should use the format of the parser)

// @Task fix documentation (remove mentions of `HIR` (now desugared AST))

mod fmt;

use freestanding::freestanding;
use std::{marker::PhantomData, rc::Rc};

use crate::{
    parser::{self, Explicitness, Identifier},
    span::Span,
};

// @Temporary location
pub trait Binder: std::fmt::Debug + std::fmt::Display + Clone {}

// @Note later: Path/Vec<Segment>
impl Binder for parser::Identifier {}

pub use declaration::{desugar_declaration, Constructor, Declaration, DeclarationKind};

pub mod declaration {
    use super::*;

    pub struct Declaration<B: Binder> {
        pub kind: DeclarationKind<B>,
        pub span: Span,
    }

    #[freestanding]
    #[streamline(Box)]
    pub enum DeclarationKind<B: Binder> {
        Value {
            binder: B,
            type_annotation: Expression<B>,
            expression: Expression<B>,
        },
        Data {
            binder: B,
            type_annotation: Expression<B>,
            constructors: Vec<Constructor<B>>,
        },
        Module {
            declarations: Vec<Declaration<B>>,
        },
        Use,
        Foreign {
            binder: B,
            type_annotation: Expression<B>,
        },
    }

    /// Desugar a declaration from AST.
    pub fn desugar_declaration(declaration: parser::Declaration) -> Declaration<Identifier> {
        match declaration.kind {
            parser::DeclarationKind::Value(declaration) => {
                // @Note type_annotation is currently desugared twice
                // @Task remove duplicate work
                // @Temporary
                let mut expression = desugar_expression(declaration.expression);
                {
                    let mut type_annotation =
                        std::iter::once(desugar_expression(declaration.type_annotation.clone()));
                    for parameter_group in declaration.parameters.iter().rev() {
                        let parameter =
                            Some(desugar_expression(parameter_group.type_annotation.clone()));
                        for binder in parameter_group.parameters.iter().rev() {
                            expression = expr! {
                                LambdaLiteral[Span::dummy()] {
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
                Declaration {
                    span: Span::dummy(),
                    kind: DeclarationKind::Value(Box::new(Value {
                        binder: declaration.binder,
                        type_annotation: desugar_annotated_parameters(
                            declaration.parameters,
                            declaration.type_annotation,
                        ),
                        expression,
                    })),
                }
            }
            parser::DeclarationKind::Data(data) => Declaration {
                span: Span::dummy(),
                kind: DeclarationKind::Data(Box::new(Data {
                    binder: data.binder,
                    type_annotation: desugar_annotated_parameters(
                        data.parameters,
                        data.type_annotation,
                    ),
                    constructors: data
                        .constructors
                        .into_iter()
                        .map(desugar_constructor)
                        .collect(),
                })),
            },
            parser::DeclarationKind::Module(module) => Declaration {
                span: Span::dummy(),
                kind: DeclarationKind::Module(Box::new(Module {
                    declarations: module
                        .declarations
                        .into_iter()
                        .map(desugar_declaration)
                        .collect(),
                })),
            },
            parser::DeclarationKind::Use => todo!(),
            parser::DeclarationKind::Foreign(declaration) => Declaration {
                span: Span::dummy(),
                kind: DeclarationKind::Foreign(Box::new(Foreign {
                    binder: declaration.binder,
                    type_annotation: desugar_annotated_parameters(
                        declaration.parameters,
                        declaration.type_annotation,
                    ),
                })),
            },
        }
    }
    pub struct Constructor<B: Binder> {
        pub binder: B,
        pub type_annotation: Expression<B>,
        pub span: Span,
    }

    /// Desugar a constructor from AST.
    fn desugar_constructor(
        constructor: parser::declaration::Constructor,
    ) -> Constructor<Identifier> {
        Constructor {
            binder: constructor.binder,
            type_annotation: desugar_annotated_parameters(
                constructor.parameters,
                constructor.type_annotation,
            ),
            span: Span::dummy(),
        }
    }
}

pub use expression::{desugar_expression, Expression, ExpressionKind};

pub mod expression {
    use super::*;

    use std::collections::VecDeque;

    // @Temporary only used for migration
    // pub type Expression = Expression<Identifier>;
    // pub type ExpressionKind = ExpressionKind<Identifier>;

    // @Task @Beacon @Beacon @Beacon parameterize over binder <B: Binder>
    // @Note this requites updating freestanding to support generics! (blocker)
    #[derive(Clone, Debug)]
    pub struct Expression<B: Binder> {
        pub kind: ExpressionKind<B>,
        pub span: Span,
    }

    // @Note we can also think about **interning** Expressions but not sure if a good idea
    #[freestanding]
    #[streamline(Rc)]
    #[derive(Clone, Debug)]
    pub enum ExpressionKind<B: Binder> {
        PiTypeLiteral {
            parameter: Option<B>,
            domain: Expression<B>,
            codomain: Expression<B>,
            explicitness: Explicitness,
        },
        Application {
            callee: Expression<B>,
            argument: Expression<B>,
            explicitness: Explicitness,
        },
        TypeLiteral,
        NatTypeLiteral,
        TextTypeLiteral,
        NatLiteral {
            value: crate::Nat,
            _marker: PhantomData<B>,
        },
        TextLiteral {
            value: String,
            _marker: PhantomData<B>,
        },
        Binding {
            binder: B,
        },
        LambdaLiteral {
            parameter: Identifier,
            parameter_type_annotation: Option<Expression<B>>,
            explicitness: Explicitness,
            body_type_annotation: Option<Expression<B>>,
            body: Expression<B>,
        },
        UseIn,
        CaseAnalysis {
            subject: Expression<B>,
            cases: Vec<Case<B>>,
        },
        // @Task move???
        UnsaturatedForeignApplication {
            callee: Identifier,
            arguments: VecDeque<Expression<B>>,
        },
    }

    /// Lower an expression from AST to HIR.
    pub fn desugar_expression(expression: parser::Expression) -> Expression<Identifier> {
        use parser::ExpressionKind::*;

        match expression.kind {
            PiTypeLiteral(literal) => expr! {
                PiTypeLiteral[Span::dummy()] {
                    parameter: literal.binder.clone(),
                    domain: desugar_expression(literal.parameter),
                    codomain: desugar_expression(literal.expression),
                    explicitness: literal.explicitness,
                }
            },
            Application(application) => expr! {
                Application[Span::dummy()] {
                    callee: desugar_expression(application.callee),
                    argument: desugar_expression(application.argument),
                    explicitness: application.explicitness,
                }
            },
            TypeLiteral => expr! { TypeLiteral[Span::dummy()] },
            NatTypeLiteral => expr! { NatTypeLiteral[Span::dummy()] },
            NatLiteral(literal) => expr! {
                NatLiteral[Span::dummy()] {
                    value: literal.value,
                    _marker: PhantomData,
                }
            },
            TextTypeLiteral => expr! { TextTypeLiteral[Span::dummy()] },
            TextLiteral(literal) => expr! {
                TextLiteral[Span::dummy()] {
                    value: literal.value,
                    _marker: PhantomData,
                }
            },
            Path(path) => expr! {
                Binding[Span::dummy()] {
                    binder: path.segments,
                }
            },
            LambdaLiteral(literal) => {
                let mut expression = desugar_expression(literal.body);

                let mut type_annotation = literal
                    .body_type_annotation
                    .map(desugar_expression)
                    .into_iter();

                for parameter_group in literal.parameters.iter().rev() {
                    let parameter = parameter_group
                        .type_annotation
                        .clone()
                        .map(desugar_expression);

                    for binder in parameter_group.parameters.iter().rev() {
                        expression = expr! {
                            LambdaLiteral[Span::dummy()] {
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
                let mut expression = desugar_expression(let_in.expression);

                let mut type_annotation = let_in
                    .type_annotation
                    .map(|expression| desugar_expression(expression))
                    .into_iter();

                for parameter_group in let_in.parameters.iter().rev() {
                    let parameter = parameter_group
                        .type_annotation
                        .clone()
                        .map(desugar_expression);
                    for binder in parameter_group.parameters.iter().rev() {
                        expression = expr! {
                            LambdaLiteral[Span::dummy()] {
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
                            LambdaLiteral[Span::dummy()] {
                                parameter: let_in.binder,
                                // @Note we cannot simply desugar parameters and a type annotation because
                                // in the chain (`->`) of parameters, there might always be one missing and
                                // we don't support partial type annotations yet (using `'_`)
                                // @Temporary @Update @Bug -gy because we ignore above message
                                // @Task verify correct semantics
                                parameter_type_annotation: type_annotation.next(),
                                explicitness: Explicitness::Explicit,
                                body_type_annotation: None,
                                body: desugar_expression(let_in.scope),
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
                        crate::diagnostic::Diagnostic::warn(
                            "contracted cases not thoroughly supported yet".to_owned(),
                            None,
                        )
                        .emit(None);
                    }

                    for pattern in case_group.patterns {
                        cases.push(Case {
                            pattern: desugar_pattern(pattern),
                            body: desugar_expression(case_group.expression.clone()),
                        });
                    }
                }

                expr! {
                    CaseAnalysis[Span::dummy()] {
                        subject: desugar_expression(case_analysis.expression),
                        cases,
                    }
                }
            }
        }
    }

    #[derive(Clone, Debug)]
    pub struct Case<B: Binder> {
        pub pattern: Pattern<B>,
        pub body: Expression<B>,
    }

    // @Task @Beacon @Beacon @Beacon @Beacon transform Pattern like we did in the parser
    #[derive(Debug, Clone)]
    pub enum Pattern<B: Binder> {
        NatLiteral(NatLiteral<B>),
        Binding {
            binder: Binding<B>,
            type_annotation: Option<Expression<B>>,
        },
        Application {
            callee: Rc<Pattern<B>>,
            argument: Rc<Pattern<B>>,
        },
    }

    /// Lower a pattern from AST to HIR.
    ///
    /// Currently, [parser::expression::Pattern] and [Pattern] are identical (apart from forgetting span information)!
    fn desugar_pattern(pattern: parser::Pattern) -> Pattern<Identifier> {
        match pattern.kind {
            parser::PatternKind::NatLiteral(literal) => Pattern::NatLiteral(NatLiteral {
                value: literal.value,
                _marker: PhantomData,
            }),
            parser::PatternKind::Path(path) => Pattern::Binding {
                binder: Binding {
                    binder: path.segments,
                },
                type_annotation: path.type_annotation.map(desugar_expression),
            },
            parser::PatternKind::Application(application) => Pattern::Application {
                callee: Rc::new(desugar_pattern(application.callee)),
                argument: Rc::new(desugar_pattern(application.argument)),
            },
        }
    }
}

/// Lower annotated parameters from AST to HIR.
fn desugar_annotated_parameters(
    parameters: parser::declaration::AnnotatedParameters,
    type_annotation: parser::Expression,
) -> Expression<Identifier> {
    let mut expression = desugar_expression(type_annotation);

    for parameter_group in parameters.into_iter().rev() {
        let parameter = desugar_expression(parameter_group.type_annotation);

        for binder in parameter_group.parameters.iter().rev() {
            expression = expr! {
                PiTypeLiteral[Span::dummy()] {
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

pub(crate) macro expr {
    ($kind:ident[$span:expr] { $( $body:tt )+ }) => {
        Expression {
            span: $span,
            kind: expression::ExpressionKind::$kind(Rc::new(expression::$kind {
                $( $body )+
            })),
        }
    },
    ($kind:ident[$span:expr]) => {
        Expression {
            span: $span,
            kind: expression::ExpressionKind::$kind,
        }
    }
}
