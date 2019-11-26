//! HIR — high-level intermediate representation

mod identifier;

use std::fmt;
use std::rc::Rc;

use crate::parser::{self, Explicitness};
pub use identifier::Identifier;

// @Task lower span information

// @Beacon @Beacon @Task add span information!!!
pub enum Declaration {
    Let {
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
    Use,     // @Task
    Foreign, // @Task
}

// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled (e.g. an indented data declaration doesn't have its constructors indented)
// @Task implement indentation logic (@Note for now, it's not that relevant because we don*t have modules yet, so a data
// declaration is never actually indented, also expressions which face the same issue when pretty-printing, are printed out in one single line!)
impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Let {
                binder,
                type_annotation,
                expression,
            } => write!(f, "'let {}: {} = {}", binder, type_annotation, expression),
            Self::Data {
                binder,
                type_annotation,
                constructors,
            } => write!(
                f,
                "'data {}: {}\n{}",
                binder,
                type_annotation,
                constructors
                    .into_iter()
                    .map(|constructor| format!("    {}", constructor))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            Self::Module { declarations } => {
                f.write_str("'module =\n")?;
                for declaration in declarations {
                    write!(f, "{}\n", declaration)?;
                }
                Ok(())
            }
            Self::Use => unimplemented!(),
            Self::Foreign => unimplemented!(),
        }
    }
}

pub fn lower_declaration(declaration: parser::Declaration) -> Declaration {
    match declaration {
        parser::Declaration::Let(r#let) => {
            // @Note type_annotation is currently lowered twice
            // @Task remove duplicate work
            // @Temporary
            let mut expression = lower_expression(r#let.expression);

            {
                let mut type_annotation =
                    std::iter::once(lower_expression(r#let.type_annotation.clone()));

                for parameter_group in r#let.parameters.iter().rev() {
                    let parameter = Some(lower_expression(parameter_group.type_annotation.clone()));

                    for binder in parameter_group.parameters.iter().rev() {
                        expression = Expression::LambdaLiteral(
                            Rc::new(expression::LambdaLiteral {
                                parameter: Identifier::Plain(binder.clone()),
                                parameter_type_annotation: parameter.clone(),
                                explicitness: parameter_group.explicitness,
                                body_type_annotation: type_annotation.next(),
                                body: expression,
                            }),
                            (),
                        );
                    }
                }
            }

            Declaration::Let {
                binder: Identifier::Plain(r#let.binder.clone()),
                type_annotation: lower_annotated_parameters(
                    r#let.parameters,
                    r#let.type_annotation,
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
        parser::Declaration::Use(_use) => unimplemented!(),
        parser::Declaration::Foreign(_foreign) => unimplemented!(),
    }
}

pub struct Constructor {
    pub binder: Identifier,
    pub type_annotation: Expression,
}

// @Question should the line break/indentation be part of the result? I don't think so:
// The parent context should decide (e.g. no line break on the end of output, further indentation)
impl fmt::Display for Constructor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.binder, self.type_annotation)
    }
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

    // @Beacon @Beacon @Task add span information!!! @Note @Beacon now, we are in the HIR,
    // there might not be any span information if synthesize HIR nodes (common thing probably)
    // @Note we can also think about **interning** Expressions but not sure if a good idea
    // @Beacon @Beacon @Note rename P: Phase to D: Discrimant and have Raw, Normalized, Type (more to come)
    // @Note currently waste memory at nattypeliterl, typeliteral,usein (Rc::new's on ~unit)
    #[derive(Clone, Debug)]
    pub enum Expression<P: Phase = InitialPhase> {
        PiTypeLiteral(Rc<PiTypeLiteral>, P::PiTypeLiteral),
        Application(Rc<Application>, P::Application),
        // TypeLiteral(Rc<TypeLiteral>, P::TypeLiteral),
        TypeLiteral(TypeLiteral, P::TypeLiteral),
        // NatTypeLiteral(Rc<NatTypeLiteral>, P::NatTypeLiteral),
        NatTypeLiteral(NatTypeLiteral, P::NatTypeLiteral),
        NatLiteral(Rc<NatLiteral>, P::NatLiteral),
        // Path(Rc<Path>, P::Path),
        Path(Path, P::Path),
        // Hole(Rc<Hole>, P::Hole),
        Hole(Hole, P::Hole),
        LambdaLiteral(Rc<LambdaLiteral>, P::LambdaLiteral),
        UseIn(Rc<UseIn>, P::UseIn),
        CaseAnalysis(Rc<CaseAnalysis>, P::CaseAnalysis),
    }

    // @Task display fewer round brackets by making use of precedence
    // @Note many wasted allocations (intermediate Strings)
    impl fmt::Display for Expression {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::PiTypeLiteral(literal, _) => write!(
                    f,
                    "({}{}{}) -> ({})",
                    literal.explicitness,
                    literal
                        .parameter
                        .as_ref()
                        .map(|binder| format!("{}: ", binder))
                        .unwrap_or_default(),
                    literal.domain,
                    literal.codomain,
                ),
                Self::Application(application, _) => write!(
                    f,
                    "({}) ({}{})",
                    application.expression, application.explicitness, application.argument,
                ),
                Self::TypeLiteral(_, _) => f.write_str("'Type"),
                Self::NatTypeLiteral(_, _) => f.write_str("'Nat"),
                Self::NatLiteral(literal, _) => write!(f, "{}", literal.value),
                Self::Path(path, _) => write!(f, "{}", path.identifier),
                Self::Hole(hole, _) => write!(f, "'hole {}", hole.tag),
                Self::LambdaLiteral(literal, _) => write!(
                    f,
                    "\\({}{}{}){} => ({})",
                    literal.explicitness,
                    literal.parameter,
                    literal
                        .parameter_type_annotation
                        .as_ref()
                        .map(|parameter| format!(": {}", parameter))
                        .unwrap_or_default(),
                    literal
                        .body_type_annotation
                        .as_ref()
                        .map(|type_annotation| format!(": {}", type_annotation))
                        .unwrap_or_default(),
                    literal.body
                ),
                Self::UseIn(_, _) => unimplemented!(),
                Self::CaseAnalysis(_, _) => unimplemented!(),
            }
        }
    }

    pub fn lower_expression(expression: parser::Expression) -> Expression {
        match expression {
            parser::Expression::PiTypeLiteral(literal) => Expression::PiTypeLiteral(
                Rc::new(PiTypeLiteral {
                    parameter: literal.binder.clone().map(Identifier::Plain),
                    domain: lower_expression(literal.parameter),
                    codomain: lower_expression(literal.expression),
                    explicitness: literal.explicitness,
                }),
                (),
            ),
            parser::Expression::Application(application) => Expression::Application(
                Rc::new(Application {
                    expression: lower_expression(application.expression),
                    argument: lower_expression(application.argument),
                    explicitness: application.explicitness,
                }),
                (),
            ),
            parser::Expression::TypeLiteral(_literal) => {
                Expression::TypeLiteral(TypeLiteral {}, ())
            }
            parser::Expression::NatTypeLiteral(_literal) => {
                Expression::NatTypeLiteral(NatTypeLiteral {}, ())
            }
            parser::Expression::NatLiteral(literal) => Expression::NatLiteral(
                Rc::new(NatLiteral {
                    value: literal.value,
                }),
                (),
            ),
            parser::Expression::Path(path) => Expression::Path(
                Path {
                    identifier: Identifier::Plain(path.inner),
                },
                (),
            ),
            parser::Expression::Hole(hole) => Expression::Hole(
                Hole {
                    tag: Identifier::Plain(hole.tag),
                },
                (),
            ),
            parser::Expression::LambdaLiteral(literal) => {
                let mut expression = lower_expression(literal.expression);

                let mut type_annotation = literal
                    .type_annotation
                    .map(|expression| lower_expression(expression))
                    .into_iter();

                for parameter_group in literal.parameters.iter().rev() {
                    let parameter = parameter_group
                        .type_annotation
                        .clone()
                        .map(lower_expression);

                    for binder in parameter_group.parameters.iter().rev() {
                        expression = Expression::LambdaLiteral(
                            Rc::new(LambdaLiteral {
                                parameter: Identifier::Plain(binder.clone()),
                                parameter_type_annotation: parameter.clone(),
                                explicitness: parameter_group.explicitness,
                                body_type_annotation: type_annotation.next(),
                                body: expression,
                            }),
                            (),
                        );
                    }
                }
                expression
            }
            parser::Expression::LetIn(let_in) => {
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
                        expression = Expression::LambdaLiteral(
                            Rc::new(LambdaLiteral {
                                parameter: Identifier::Plain(binder.clone()),
                                parameter_type_annotation: parameter.clone(),
                                explicitness: parameter_group.explicitness,
                                body_type_annotation: type_annotation.next(),
                                body: expression,
                            }),
                            (),
                        );
                    }
                }

                Expression::Application(
                    Rc::new(Application {
                        expression: Expression::LambdaLiteral(
                            Rc::new(LambdaLiteral {
                                parameter: Identifier::Plain(let_in.binder),
                                // @Note we cannot simply lower parameters and a type annotation because
                                // in the chain (`->`) of parameters, there might always be one missing and
                                // we don't support partial type annotations yet (using `'_`)
                                parameter_type_annotation: None,
                                explicitness: Explicitness::Explicit,
                                body_type_annotation: None,
                                body: lower_expression(let_in.scope),
                            }),
                            (),
                        ),
                        argument: expression,
                        explicitness: Explicitness::Explicit,
                    }),
                    (),
                )
            }
            parser::Expression::UseIn(_use_in) => unimplemented!(),
            // @Task
            parser::Expression::CaseAnalysis(_case_analysis) => unimplemented!(),
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
        pub expression: Expression,
        pub argument: Expression,
        pub explicitness: Explicitness,
    }

    // @Note don't rc
    #[derive(Clone, Debug)]
    pub struct TypeLiteral {}

    // @Note don't rc
    #[derive(Clone, Debug)]
    pub struct NatTypeLiteral {}

    #[derive(Clone, Debug)]
    pub struct NatLiteral {
        pub value: crate::lexer::Nat,
    }

    // @Note don't reference-count because it already uses interning
    #[derive(Clone, Debug)]
    pub struct Path {
        pub identifier: Identifier,
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
    pub struct CaseAnalysis {}
}

fn lower_annotated_parameters(
    parameters: parser::declaration::AnnotatedParameters,
    type_annotation: parser::Expression,
) -> Expression {
    let mut expression = lower_expression(type_annotation);

    for parameter_group in parameters.into_iter().rev() {
        let parameter = lower_expression(parameter_group.type_annotation);

        for binder in parameter_group.parameters.iter().rev() {
            expression = Expression::PiTypeLiteral(
                Rc::new(expression::PiTypeLiteral {
                    parameter: Some(Identifier::Plain(binder.clone())),
                    domain: parameter.clone(),
                    codomain: expression,
                    explicitness: parameter_group.explicitness,
                }),
                (),
            )
        }
    }

    expression
}

// @Temporary
#[derive(Clone, Debug)]
pub enum InitialPhase {}

pub trait Phase {
    type PiTypeLiteral;
    type Application;
    type TypeLiteral;
    type NatTypeLiteral;
    type NatLiteral;
    type Path;
    type Hole;
    type LambdaLiteral;
    type UseIn;
    type CaseAnalysis;
}

impl Phase for InitialPhase {
    type PiTypeLiteral = ();
    type Application = ();
    type TypeLiteral = ();
    type NatTypeLiteral = ();
    type NatLiteral = ();
    type Path = ();
    type Hole = ();
    type LambdaLiteral = ();
    type UseIn = ();
    type CaseAnalysis = ();
}
