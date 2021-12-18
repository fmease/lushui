//! The lowered abstract syntax tree (lowered AST).

// @Task rename a bunch of syntax node to make them more like the ones of the AST
// i.e. not Number but NumberLiteral etc

pub mod attributes;
mod format;

use super::ast::{Explicitness, Identifier, ParameterAspect, Path};
use crate::{
    error::PossiblyErroneous,
    span::{SourceFileIndex, Span},
};
pub use attributes::{Attribute, AttributeKind, AttributeName, Attributes};
use std::rc::Rc;

pub type Item<Kind> = crate::item::Item<Kind, attributes::Attributes>;

pub type Declaration = Item<DeclarationKind>;

pub enum DeclarationKind {
    Function(Box<Function>),
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

pub struct Function {
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
    pub binder: Identifier,
    pub target: Path,
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
    Error,
}

impl PossiblyErroneous for ExpressionKind {
    fn error() -> Self {
        Self::Error
    }
}

#[derive(Clone)]
pub struct PiType {
    // @Question should we move `aspect`, `parameter` and `domain` to
    // separate `Domain` type like in `crate::syntax::ast` or should we keep it
    // flat and also inline `Domain` in the non-lowered AST?
    pub explicitness: Explicitness,
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
    pub binder: Path,
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Number {
    Nat(crate::utility::Nat),
    Nat32(u32),
    Nat64(u64),
    Int(crate::utility::Int),
    Int32(i32),
    Int64(i64),
}

pub macro decl($( $tree:tt )+) {
    crate::item::item!(crate::syntax::lowered_ast, DeclarationKind, Box; $( $tree )+)
}

pub macro expr($( $tree:tt )+) {
    crate::item::item!(crate::syntax::lowered_ast, ExpressionKind, Rc; $( $tree )+)
}

pub macro pat($( $tree:tt )+) {
    crate::item::item!(crate::syntax::lowered_ast, PatternKind, Rc; $( $tree )+)
}
