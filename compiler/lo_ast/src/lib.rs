//! The lowered abstract syntax tree (Lo-AST).
#![feature(adt_const_params, decl_macro)]
#![allow(incomplete_features)] // adt_const_params

use ast::{
    Identifier, LocalBinder, NumberLiteral, ParameterKind, Path, SequenceLiteral, TextLiteral,
    Wildcard,
};
use diagnostics::{error::PossiblyErroneous, reporter::ErasedReportedError};
use span::{SourceFileIndex, Spanned};

pub use attribute::{Attribute, AttributeName, Attributes, BareAttribute};
pub use format::Display;

mod format;

pub mod attribute;

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
    pub type_: Expression,
    pub body: Option<Expression>,
}

impl From<Function> for BareDeclaration {
    fn from(function: Function) -> Self {
        Self::Function(Box::new(function))
    }
}

pub struct Data {
    pub binder: Identifier,
    pub type_: Expression,
    pub declarations: Option<Vec<Declaration>>,
}

impl From<Data> for BareDeclaration {
    fn from(type_: Data) -> Self {
        Self::Data(Box::new(type_))
    }
}

pub struct Constructor {
    pub binder: Identifier,
    pub type_: Expression,
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
    /// The type of types.
    ///
    /// This construct doesn't have corresponding syntax in the surface language.
    /// It's synthesized during lowering.
    Type,
    /// A hygienic local binding.
    ///
    /// This construct doesn't have corresponding syntax in the surface language.
    /// It's synthesized during lowering.
    LocalBinding(DeBruijnLevel),
    Wildcard(Box<Wildcard>),
    NumberLiteral(Box<NumberLiteral>),
    TextLiteral(Box<TextLiteral>),
    Path(Box<Path>),
    Application(Box<Application<Expression>>),
    SequenceLiteral(Box<SequenceLiteral<Expression>>),
    RecordLiteral(Box<RecordLiteral<Expression>>),
    Projection(Box<Projection>),
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

impl From<DeBruijnLevel> for BareExpression {
    fn from(level: DeBruijnLevel) -> Self {
        Self::LocalBinding(level)
    }
}

impl From<Box<Wildcard>> for BareExpression {
    fn from(wildcard: Box<Wildcard>) -> Self {
        Self::Wildcard(wildcard)
    }
}

impl From<Wildcard> for BareExpression {
    fn from(wildcard: Wildcard) -> Self {
        Box::new(wildcard).into()
    }
}

impl From<Box<NumberLiteral>> for BareExpression {
    fn from(number: Box<NumberLiteral>) -> Self {
        Self::NumberLiteral(number)
    }
}

impl From<Box<TextLiteral>> for BareExpression {
    fn from(text: Box<TextLiteral>) -> Self {
        Self::TextLiteral(text)
    }
}

impl From<Box<Path>> for BareExpression {
    fn from(path: Box<Path>) -> Self {
        Self::Path(path)
    }
}

impl From<Path> for BareExpression {
    fn from(path: Path) -> Self {
        Box::new(path).into()
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

impl From<RecordLiteral<Expression>> for BareExpression {
    fn from(record: RecordLiteral<Expression>) -> Self {
        Self::RecordLiteral(Box::new(record))
    }
}

#[derive(Clone)]
pub struct Projection {
    pub basis: Expression,
    pub field: Identifier,
}

impl From<Projection> for BareExpression {
    fn from(projection: Projection) -> Self {
        Self::Projection(Box::new(projection))
    }
}

#[derive(Clone)]
pub struct PiType {
    pub kind: ParameterKind,
    pub binder: Option<Identifier>,
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
    pub kind: ParameterKind,
    pub binder: Option<Identifier>,
    pub domain: Option<Expression>,
    pub codomain: Option<Expression>,
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
    Wildcard(Box<Wildcard>),
    NumberLiteral(Box<NumberLiteral>),
    TextLiteral(Box<TextLiteral>),
    Path(Box<Path>),
    LetBinding(LocalBinder),
    Application(Box<Application<Pattern>>),
    SequenceLiteral(Box<SequenceLiteral<Pattern>>),
    Error(ErasedReportedError),
}

impl PossiblyErroneous for BarePattern {
    fn error(error: ErasedReportedError) -> Self {
        Self::Error(error)
    }
}

impl From<Box<Wildcard>> for BarePattern {
    fn from(wildcard: Box<Wildcard>) -> Self {
        Self::Wildcard(wildcard)
    }
}

impl From<Box<NumberLiteral>> for BarePattern {
    fn from(number: Box<NumberLiteral>) -> Self {
        Self::NumberLiteral(number)
    }
}

impl From<NumberLiteral> for BarePattern {
    fn from(number: NumberLiteral) -> Self {
        Box::new(number).into()
    }
}

impl From<Box<TextLiteral>> for BarePattern {
    fn from(text: Box<TextLiteral>) -> Self {
        Self::TextLiteral(text)
    }
}

impl From<TextLiteral> for BarePattern {
    fn from(text: TextLiteral) -> Self {
        Box::new(text).into()
    }
}

impl From<Box<Path>> for BarePattern {
    fn from(path: Box<Path>) -> Self {
        Self::Path(path)
    }
}

impl From<Path> for BarePattern {
    fn from(path: Path) -> Self {
        Box::new(path).into()
    }
}

impl From<LocalBinder> for BarePattern {
    fn from(binder: LocalBinder) -> Self {
        Self::LetBinding(binder)
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
    pub kind: ParameterKind,
    pub callee: T,
    pub argument: T,
}

#[derive(Clone)]
pub struct RecordLiteral<T> {
    pub path: Option<Path>,
    pub fields: Spanned<Vec<Field<T>>>,
    pub base: Option<T>,
}

#[derive(Clone)]

pub struct Field<T> {
    pub binder: Identifier,
    pub body: T,
}

#[derive(Clone, Copy)]
pub struct DeBruijnLevel(pub usize);
