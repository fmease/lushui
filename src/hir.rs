//! The high-level intermediate representation.

mod format;

use crate::{
    error::PossiblyErroneous,
    resolver::{CrateScope, FunctionScope, Identifier},
    span::{SourceFileIndex, Span},
    syntax::{
        ast::{Explicitness, ParameterAspect},
        lowered_ast::{Item, Number},
    },
    typer::interpreter,
};
use std::rc::Rc;

pub type Declaration = Item<DeclarationKind>;

impl Declaration {
    pub fn unwrap_constructor(&self) -> &Constructor {
        match &self.data {
            DeclarationKind::Constructor(constructor) => constructor,
            _ => unreachable!(),
        }
    }
}

pub enum DeclarationKind {
    Value(Box<Value>),
    Data(Box<Data>),
    Constructor(Box<Constructor>),
    Module(Box<Module>),
    Use(Box<Use>),
    Error,
}

impl PossiblyErroneous for DeclarationKind {
    fn error() -> Self {
        Self::Error
    }
}

pub struct Value {
    pub binder: Identifier,
    pub type_annotation: Expression,
    pub expression: Option<Expression>,
}

pub struct Data {
    pub binder: Identifier,
    pub type_annotation: Expression,
    pub constructors: Option<Vec<Declaration>>,
}

pub struct Constructor {
    pub binder: Identifier,
    pub type_annotation: Expression,
}

pub struct Module {
    pub binder: Identifier,
    pub file: SourceFileIndex,
    pub declarations: Vec<Declaration>,
}

pub struct Use {
    pub binder: Option<Identifier>,
    pub target: Identifier,
}

pub type Expression = Item<ExpressionKind>;

#[derive(Clone)]
pub enum ExpressionKind {
    PiType(Rc<PiType>),
    Application(Rc<Application>),
    Type,
    Number(Rc<Number>),
    Text(Rc<String>),
    Binding(Rc<Binding>),
    Lambda(Rc<Lambda>),
    UseIn,
    CaseAnalysis(Rc<CaseAnalysis>),
    Substitution(Rc<Substitution>),
    ForeignApplication(Rc<ForeignApplication>),
    Projection(Rc<Projection>),
    IO(Rc<IO>),
    Error,
}

impl PossiblyErroneous for ExpressionKind {
    fn error() -> Self {
        Self::Error
    }
}

#[derive(Clone)]
pub struct PiType {
    pub explicitness: Explicitness,
    // @Question should the concept of fieldness still exist in this pass?
    pub aspect: ParameterAspect,
    pub parameter: Option<Identifier>,
    pub domain: Expression,
    pub codomain: Expression,
}

#[derive(Clone)]
pub struct Application {
    pub callee: Expression,
    pub argument: Expression,
    pub explicitness: Explicitness,
}

#[derive(Clone)]
pub struct Binding {
    pub binder: Identifier,
}

#[derive(Clone)]
pub struct Lambda {
    pub parameter: Identifier,
    pub parameter_type_annotation: Option<Expression>,
    pub explicitness: Explicitness,
    // @Temporary
    pub laziness: Option<Span>,
    pub body_type_annotation: Option<Expression>,
    pub body: Expression,
}

#[derive(Clone)]
pub struct CaseAnalysis {
    pub subject: Expression,
    pub cases: Vec<Case>,
}

#[derive(Clone)]
pub struct Substitution {
    pub substitution: interpreter::Substitution,
    pub expression: Expression,
}

#[derive(Clone)]
pub struct ForeignApplication {
    pub callee: Identifier,
    pub arguments: Vec<Expression>,
}

// @Temporary until we have better case analysis support to replace it with
// @Task @Beacon
#[derive(Clone)]
pub struct Projection {}

#[derive(Clone)]
pub struct IO {
    pub index: usize, // @Task IOIndex
    pub arguments: Vec<Expression>,
    // @Task continuation: Option<Expression>
}

#[derive(Clone)]
pub struct Case {
    pub pattern: Pattern,
    pub body: Expression,
}

pub type Pattern = Item<PatternKind>;

#[derive(Clone)]
pub enum PatternKind {
    Number(Rc<Number>),
    Text(Rc<String>),
    Binding(Rc<Binding>),
    Binder(Rc<Binder>),
    Deapplication(Rc<Deapplication>),
    Error,
}

impl PossiblyErroneous for PatternKind {
    fn error() -> Self {
        Self::Error
    }
}

/// A binder inside of a pattern.
#[derive(Clone)]
pub struct Binder {
    pub binder: Identifier,
}

#[derive(Clone)]
pub struct Deapplication {
    pub callee: Pattern,
    pub argument: Pattern,
}

pub macro decl($( $tree:tt )+) {
    crate::item::item!(crate::hir, DeclarationKind, Box; $( $tree )+)
}

pub macro expr($( $tree:tt )+) {
    crate::item::item!(crate::hir, ExpressionKind, Rc; $( $tree )+)
}

pub macro pat($( $tree:tt )+) {
    crate::item::item!(crate::hir, PatternKind, Rc; $( $tree )+)
}
