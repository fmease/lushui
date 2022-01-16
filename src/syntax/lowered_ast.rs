//! The lowered abstract syntax tree (lowered AST).

use super::ast::{Explicitness, Identifier, NumberLiteral, Path, TextLiteral};
use crate::{
    error::PossiblyErroneous,
    span::{SourceFileIndex, Span},
};
pub(crate) use attributes::{Attribute, AttributeKind, AttributeName, Attributes};

pub(crate) mod attributes;
mod format;

pub(crate) type Item<Kind> = crate::item::Item<Kind, attributes::Attributes>;

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

impl From<Function> for DeclarationKind {
    fn from(function: Function) -> Self {
        Self::Function(Box::new(function))
    }
}

pub struct Data {
    pub binder: Identifier,
    pub type_annotation: Expression,
    pub constructors: Option<Vec<Declaration>>,
}

impl From<Data> for DeclarationKind {
    fn from(type_: Data) -> Self {
        Self::Data(Box::new(type_))
    }
}

pub struct Constructor {
    pub binder: Identifier,
    pub type_annotation: Expression,
}

impl From<Constructor> for DeclarationKind {
    fn from(constructor: Constructor) -> Self {
        Self::Constructor(Box::new(constructor))
    }
}

pub struct Module {
    pub binder: Identifier,
    pub file: SourceFileIndex,
    pub declarations: Vec<Declaration>,
}

impl From<Module> for DeclarationKind {
    fn from(module: Module) -> Self {
        Self::Module(Box::new(module))
    }
}

pub struct Use {
    pub binder: Identifier,
    pub target: Path,
}

impl From<Use> for DeclarationKind {
    fn from(use_: Use) -> Self {
        Self::Use(Box::new(use_))
    }
}

pub type Expression = Item<ExpressionKind>;

#[derive(Clone)]
pub enum ExpressionKind {
    TypeLiteral,
    Path(Box<Path>),
    NumberLiteral(Box<NumberLiteral>),
    TextLiteral(Box<TextLiteral>),
    Application(Box<Application<Expression>>),
    PiType(Box<PiType>),
    Lambda(Box<Lambda>),
    CaseAnalysis(Box<CaseAnalysis>),
    UseIn,
    Error,
}

impl PossiblyErroneous for ExpressionKind {
    fn error() -> Self {
        Self::Error
    }
}

impl From<Path> for ExpressionKind {
    fn from(path: Path) -> Self {
        Self::Path(Box::new(path))
    }
}

impl From<NumberLiteral> for ExpressionKind {
    fn from(number: NumberLiteral) -> Self {
        Self::NumberLiteral(Box::new(number))
    }
}

impl From<TextLiteral> for ExpressionKind {
    fn from(text: TextLiteral) -> Self {
        Self::TextLiteral(Box::new(text))
    }
}

impl From<Application<Expression>> for ExpressionKind {
    fn from(application: Application<Expression>) -> Self {
        Self::Application(Box::new(application))
    }
}

#[derive(Clone)]
pub struct PiType {
    pub explicitness: Explicitness,
    pub laziness: Option<Span>,
    pub parameter: Option<Identifier>,
    pub domain: Expression,
    pub codomain: Expression,
}

impl From<PiType> for ExpressionKind {
    fn from(pi: PiType) -> Self {
        Self::PiType(Box::new(pi))
    }
}

#[derive(Clone)]
pub struct Lambda {
    pub parameter: Identifier,
    pub parameter_type_annotation: Option<Expression>,
    pub explicitness: Explicitness,
    pub laziness: Option<Span>,
    pub body_type_annotation: Option<Expression>,
    pub body: Expression,
}

impl From<Lambda> for ExpressionKind {
    fn from(lambda: Lambda) -> Self {
        Self::Lambda(Box::new(lambda))
    }
}

#[derive(Clone)]
pub struct CaseAnalysis {
    pub scrutinee: Expression,
    pub cases: Vec<Case>,
}

impl From<CaseAnalysis> for ExpressionKind {
    fn from(analysis: CaseAnalysis) -> Self {
        Self::CaseAnalysis(Box::new(analysis))
    }
}

#[derive(Clone)]
pub struct Case {
    pub pattern: Pattern,
    pub body: Expression,
}

pub type Pattern = Item<PatternKind>;

#[derive(Clone)]
pub enum PatternKind {
    NumberLiteral(Box<NumberLiteral>),
    TextLiteral(Box<TextLiteral>),
    Path(Box<Path>),
    Binder(Box<Identifier>),
    Application(Box<Application<Pattern>>),
    Error,
}

impl PossiblyErroneous for PatternKind {
    fn error() -> Self {
        Self::Error
    }
}

impl From<NumberLiteral> for PatternKind {
    fn from(number: NumberLiteral) -> Self {
        Self::NumberLiteral(Box::new(number))
    }
}

impl From<TextLiteral> for PatternKind {
    fn from(text: TextLiteral) -> Self {
        Self::TextLiteral(Box::new(text))
    }
}

impl From<Path> for PatternKind {
    fn from(path: Path) -> Self {
        Self::Path(Box::new(path))
    }
}

impl From<Identifier> for PatternKind {
    fn from(identifier: Identifier) -> Self {
        Self::Binder(Box::new(identifier))
    }
}

impl From<Application<Pattern>> for PatternKind {
    fn from(application: Application<Pattern>) -> Self {
        Self::Application(Box::new(application))
    }
}

#[derive(Clone)]
pub struct Application<T> {
    pub callee: T,
    pub explicitness: Explicitness,
    pub argument: T,
}

// @Task move!
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Number {
    Nat(crate::utility::Nat),
    Nat32(u32),
    Nat64(u64),
    Int(crate::utility::Int),
    Int32(i32),
    Int64(i64),
}
