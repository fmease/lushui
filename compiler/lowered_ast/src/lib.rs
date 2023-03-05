//! The lowered abstract syntax tree (lowered AST).
#![feature(adt_const_params, decl_macro)]
#![allow(incomplete_features)] // adt_const_params

use ast::{Explicitness, Identifier, NumberLiteral, Path, SequenceLiteral, TextLiteral};
pub use attribute::{Attribute, AttributeName, Attributes, BareAttribute};
use diagnostics::{error::PossiblyErroneous, reporter::ErasedReportedError};
pub use format::Display;
use span::{SourceFileIndex, Span};

pub mod attribute;
mod format;

pub type Item<Bare> = span::item::Item<Bare, attribute::Attributes>;

pub type Declaration = Item<BareDeclaration>;

pub enum BareDeclaration {
    Function(Box<Function>),
    Data(Box<Data>),
    Constructor(Box<Constructor>),
    Module(Box<Module>),
    Use(Box<Use>),
    Error(ErasedReportedError),
}

impl PossiblyErroneous for BareDeclaration {
    fn error(error: ErasedReportedError) -> Self {
        Self::Error(error)
    }
}

pub struct Function {
    pub binder: Identifier,
    pub type_annotation: Expression,
    pub expression: Option<Expression>,
}

impl From<Function> for BareDeclaration {
    fn from(function: Function) -> Self {
        Self::Function(Box::new(function))
    }
}

pub struct Data {
    pub binder: Identifier,
    pub type_annotation: Expression,
    pub constructors: Option<Vec<Declaration>>,
}

impl From<Data> for BareDeclaration {
    fn from(type_: Data) -> Self {
        Self::Data(Box::new(type_))
    }
}

pub struct Constructor {
    pub binder: Identifier,
    pub type_annotation: Expression,
}

impl From<Constructor> for BareDeclaration {
    fn from(constructor: Constructor) -> Self {
        Self::Constructor(Box::new(constructor))
    }
}

pub struct Module {
    pub binder: Identifier,
    pub file: SourceFileIndex,
    pub declarations: Vec<Declaration>,
}

impl From<Module> for BareDeclaration {
    fn from(module: Module) -> Self {
        Self::Module(Box::new(module))
    }
}

pub struct Use {
    pub binder: Identifier,
    pub target: Path,
}

impl From<Use> for BareDeclaration {
    fn from(use_: Use) -> Self {
        Self::Use(Box::new(use_))
    }
}

pub type Expression = Item<BareExpression>;

#[derive(Clone)]
pub enum BareExpression {
    Path(Box<Path>),
    NumberLiteral(Box<NumberLiteral>),
    TextLiteral(Box<TextLiteral>),
    Application(Box<Application<Expression>>),
    SequenceLiteral(Box<SequenceLiteral<Expression>>),
    PiType(Box<PiType>),
    Lambda(Box<Lambda>),
    CaseAnalysis(Box<CaseAnalysis>),
    UseBinding,
    Error(ErasedReportedError),
}

impl PossiblyErroneous for BareExpression {
    fn error(error: ErasedReportedError) -> Self {
        Self::Error(error)
    }
}

impl From<Path> for BareExpression {
    fn from(path: Path) -> Self {
        Self::Path(Box::new(path))
    }
}

impl From<NumberLiteral> for BareExpression {
    fn from(number: NumberLiteral) -> Self {
        Self::NumberLiteral(Box::new(number))
    }
}

impl From<TextLiteral> for BareExpression {
    fn from(text: TextLiteral) -> Self {
        Self::TextLiteral(Box::new(text))
    }
}

impl From<Application<Expression>> for BareExpression {
    fn from(application: Application<Expression>) -> Self {
        Self::Application(Box::new(application))
    }
}

impl From<SequenceLiteral<Expression>> for BareExpression {
    fn from(sequence: SequenceLiteral<Expression>) -> Self {
        Self::SequenceLiteral(Box::new(sequence))
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

impl From<PiType> for BareExpression {
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

impl From<Lambda> for BareExpression {
    fn from(lambda: Lambda) -> Self {
        Self::Lambda(Box::new(lambda))
    }
}

#[derive(Clone)]
pub struct CaseAnalysis {
    pub scrutinee: Expression,
    pub cases: Vec<Case>,
}

impl From<CaseAnalysis> for BareExpression {
    fn from(analysis: CaseAnalysis) -> Self {
        Self::CaseAnalysis(Box::new(analysis))
    }
}

#[derive(Clone)]
pub struct Case {
    pub pattern: Pattern,
    pub body: Expression,
}

pub type Pattern = Item<BarePattern>;

#[derive(Clone)]
pub enum BarePattern {
    NumberLiteral(Box<NumberLiteral>),
    TextLiteral(Box<TextLiteral>),
    Path(Box<Path>),
    Binder(Box<Identifier>),
    Application(Box<Application<Pattern>>),
    SequenceLiteral(Box<SequenceLiteral<Pattern>>),
    Error(ErasedReportedError),
}

impl PossiblyErroneous for BarePattern {
    fn error(error: ErasedReportedError) -> Self {
        Self::Error(error)
    }
}

impl From<NumberLiteral> for BarePattern {
    fn from(number: NumberLiteral) -> Self {
        Self::NumberLiteral(Box::new(number))
    }
}

impl From<TextLiteral> for BarePattern {
    fn from(text: TextLiteral) -> Self {
        Self::TextLiteral(Box::new(text))
    }
}

impl From<Path> for BarePattern {
    fn from(path: Path) -> Self {
        Self::Path(Box::new(path))
    }
}

impl From<Identifier> for BarePattern {
    fn from(identifier: Identifier) -> Self {
        Self::Binder(Box::new(identifier))
    }
}

impl From<Application<Pattern>> for BarePattern {
    fn from(application: Application<Pattern>) -> Self {
        Self::Application(Box::new(application))
    }
}

impl From<SequenceLiteral<Pattern>> for BarePattern {
    fn from(sequence: SequenceLiteral<Pattern>) -> Self {
        Self::SequenceLiteral(Box::new(sequence))
    }
}

#[derive(Clone)]
pub struct Application<T> {
    pub callee: T,
    pub explicitness: Explicitness,
    pub argument: T,
}
