//! HIR — hig-level immediate representation

use crate::parser::{self, Identifier};

// @Task pub lower_module

// @Task lower span information

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
    Module,  // @Task,
    Use,     // @Task
    Foreign, // @Task
}

// @Task impl From later
pub fn lower_declaration(declaration: parser::Declaration) -> Declaration {
    match declaration {
        // @Bug @Beacon not correctly impl (Let): lower_expression etc erases params
        parser::Declaration::Let {
            binder,
            parameters,
            type_annotation,
            expression,
        } => Declaration::Let {
            binder,
            type_annotation: lower_annotated_parameters(parameters, type_annotation),
            expression: lower_expression(expression),
        },
        parser::Declaration::Data {
            binder,
            parameters,
            type_annotation,
            constructors,
        } => Declaration::Data {
            binder,
            type_annotation: lower_annotated_parameters(parameters, type_annotation),
            constructors: constructors.into_iter().map(lower_constructor).collect(),
        },
        _ => unimplemented!(),
    }
}

pub struct Constructor {
    binder: Identifier,
    type_annotation: Expression,
}

fn lower_constructor(constructor: parser::Constructor) -> Constructor {
    Constructor {
        binder: constructor.binder,
        type_annotation: lower_annotated_parameters(
            constructor.parameters,
            constructor.type_annotation,
        ),
    }
}

// @Task improve naming (binder, parameter, etcetera)
pub enum Expression {
    PiLiteral {
        binder: Option<Identifier>,
        parameter: Box<Expression>,
        expression: Box<Expression>,
        implicit: bool,
    },
    Application {
        expression: Box<Expression>,
        argument: Box<Expression>,
        implicit: bool,
    },
    TypeLiteral,
    Identifier(Identifier),
    Hole(Identifier),
    LambdaLiteral {
        binder: Identifier,
        parameter: Option<Box<Expression>>,
        expression: Box<Expression>,
    },
    UseIn, // @Task
    Case,  // @Task
}

pub fn lower_expression(expression: parser::Expression) -> Expression {
    match expression {
        parser::Expression::PiLiteral {
            binder,
            parameter,
            expression,
            implicit,
        } => Expression::PiLiteral {
            binder,
            parameter: Box::new(lower_expression(*parameter)),
            expression: Box::new(lower_expression(*expression)),
            implicit,
        },
        parser::Expression::Application {
            expression,
            argument,
            implicit,
        } => Expression::Application {
            expression: Box::new(lower_expression(*expression)),
            argument: Box::new(lower_expression(*argument)),
            implicit,
        },
        parser::Expression::TypeLiteral => Expression::TypeLiteral,
        parser::Expression::Identifier(identifier) => Expression::Identifier(identifier),
        parser::Expression::Hole(identifier) => Expression::Hole(identifier),
        parser::Expression::LambdaLiteral {
            parameters,
            type_annotation,
            expression,
        } => lower_lambda_literal(
            parameters,
            type_annotation.map(|expression| *expression),
            *expression,
        ),
        parser::Expression::LetIn {
            binder,
            parameters,
            type_annotation,
            expression,
            scope,
        } => lower_let_in(
            binder,
            parameters,
            type_annotation.map(|expression| *expression),
            *expression,
            *scope,
        ),
        _ => unimplemented!(),
    }
}

fn lower_lambda_literal(
    parameters: parser::Parameters,
    type_annotation: Option<parser::Expression>,
    expression: parser::Expression,
) -> Expression {
    unimplemented!() // @Task
}

fn lower_let_in(
    binder: Identifier,
    parameters: parser::Parameters,
    type_annotation: Option<parser::Expression>,
    expression: parser::Expression,
    scope: parser::Expression,
) -> Expression {
    unimplemented!() // @Task
}

fn lower_annotated_parameters(
    parameters: parser::AnnotatedParameters,
    type_annotation: parser::Expression,
) -> Expression {
    unimplemented!() // @Task @Beacon
}
