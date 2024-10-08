//! The high-level intermediate representation.
#![feature(decl_macro)]

use diagnostics::{error::PossiblyErroneous, reporter::ErasedReportedError};
use joinery::JoinableIterator;
use span::{SourceFileIndex, Spanned};
use special::NumericType;
use std::{
    fmt,
    sync::{Arc, Mutex},
};
use utility::{Int, Nat, obtain};

pub use ast::ParameterKind;
pub use entity::{Entity, EntityKind};
pub use identifier::{DeBruijnIndex, DeclarationIndex, Identifier, Index, LocalDeclarationIndex};
pub use lo_ast::{Attribute, AttributeName, Attributes, BareAttribute, Item, attribute};

mod entity;
mod identifier;

// @Task get rid of this smh.
pub mod interfaceable;
pub mod special;

pub type Declaration = Item<BareDeclaration>;

pub enum BareDeclaration {
    Function(Box<Function>),
    Data(Box<Data>),
    Constructor(Box<Constructor>),
    Module(Box<Module>),
    Use(Box<Use>),
    Error(ErasedReportedError),
}

impl BareDeclaration {
    // @Task impl as TryFrom if possible
    pub fn constructor(&self) -> Option<&Constructor> {
        obtain!(self, BareDeclaration::Constructor(constructor) => constructor)
    }
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
    pub constructors: Option<Vec<Declaration>>,
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
    pub binder: Option<Identifier>,
    pub target: Identifier,
}

impl From<Use> for BareDeclaration {
    fn from(use_: Use) -> Self {
        Self::Use(Box::new(use_))
    }
}

pub type Expression = Item<BareExpression>;

#[derive(Clone)]
pub enum BareExpression {
    Number(Box<Number>),
    Text(Box<Text>),
    Binding(Box<Binding>),
    Application(Box<Application<Expression>>),
    IntrinsicApplication(Box<IntrinsicApplication>),
    Record(Box<Record>),
    Projection(Box<Projection>),
    PiType(Box<PiType>),
    Lambda(Box<Lambda>),
    IO(Box<IO>),
    CaseAnalysis(Box<CaseAnalysis>),
    Substituted(Box<Substituted>),
    Error(ErasedReportedError),
}

impl PossiblyErroneous for BareExpression {
    fn error(error: ErasedReportedError) -> Self {
        Self::Error(error)
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

impl From<Application<Expression>> for BareExpression {
    fn from(application: Application<Expression>) -> Self {
        Self::Application(Box::new(application))
    }
}

impl From<Number> for BareExpression {
    fn from(number: Number) -> Self {
        Self::Number(Box::new(number))
    }
}

impl From<Text> for BareExpression {
    fn from(text: Text) -> Self {
        Self::Text(Box::new(text))
    }
}

impl From<Binding> for BareExpression {
    fn from(binding: Binding) -> Self {
        Self::Binding(Box::new(binding))
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
pub struct Substituted {
    pub substitution: Substitution,
    pub expression: Expression,
}

impl From<Substituted> for BareExpression {
    fn from(substitution: Substituted) -> Self {
        Self::Substituted(Box::new(substitution))
    }
}

#[derive(Clone)]
pub enum Substitution {
    Shift(usize),
    Use(Box<Substitution>, Expression),
}

#[derive(Clone)]
pub struct IntrinsicApplication {
    pub callee: Identifier,
    pub arguments: Vec<Expression>,
}

impl From<IntrinsicApplication> for BareExpression {
    fn from(application: IntrinsicApplication) -> Self {
        Self::IntrinsicApplication(Box::new(application))
    }
}

#[derive(Clone)]
pub struct Record {
    pub type_: Spanned<DeclarationIndex>,
    pub fields: Vec<Field>,
}

impl From<Record> for BareExpression {
    fn from(record: Record) -> Self {
        Self::Record(Box::new(record))
    }
}

#[derive(Clone)]
pub struct Field {
    pub binder: ast::Identifier,
    pub body: Expression,
}

#[derive(Clone)]
pub struct Projection {
    pub basis: Expression,
    pub field: ast::Identifier,
}

impl From<Projection> for BareExpression {
    fn from(projection: Projection) -> Self {
        Self::Projection(Box::new(projection))
    }
}

#[derive(Clone)]
pub struct IO {
    pub index: usize, // @Task IOIndex
    pub arguments: Vec<Expression>,
    // @Task continuation: Option<Expression>
}

impl From<IO> for BareExpression {
    fn from(io: IO) -> Self {
        Self::IO(Box::new(io))
    }
}

#[derive(Clone)]
pub struct Case {
    pub pattern: Pattern,
    pub body: Expression,
}

pub type Pattern = Item<BarePattern>;

#[derive(Clone)]
#[allow(clippy::box_collection)]
pub enum BarePattern {
    Number(Box<Number>),
    Text(Box<Text>),
    // @Task rename Binding & LetBinding in a way that makes it understandable which
    // one of the two *defines* and which one *references*.
    Binding(Binding),
    LetBinding(LocalBinder),
    Application(Box<Application<Pattern>>),
    Error(ErasedReportedError),
}

impl PossiblyErroneous for BarePattern {
    fn error(error: ErasedReportedError) -> Self {
        Self::Error(error)
    }
}

impl From<Number> for BarePattern {
    fn from(number: Number) -> Self {
        Self::Number(Box::new(number))
    }
}

impl From<Text> for BarePattern {
    fn from(text: Text) -> Self {
        Self::Text(Box::new(text))
    }
}

impl From<Binding> for BarePattern {
    fn from(binding: Binding) -> Self {
        Self::Binding(binding)
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

#[derive(Clone)]
pub struct Binding(pub Identifier); // @Task rename 0 -> binder again

#[derive(Clone)]
pub struct Application<T> {
    pub kind: ParameterKind,
    pub callee: T,
    pub argument: T,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Number {
    Nat(Nat),
    Nat32(u32),
    Nat64(u64),
    Int(Int),
    Int32(i32),
    Int64(i64),
}

impl Number {
    pub fn parse(source: &str, type_: NumericType) -> Result<Self, ()> {
        use special::NumericType::*;

        match type_ {
            Nat => source.parse().map(Self::Nat).map_err(drop),
            Nat32 => source.parse().map(Self::Nat32).map_err(drop),
            Nat64 => source.parse().map(Self::Nat64).map_err(drop),
            Int => source.parse().map(Self::Int).map_err(drop),
            Int32 => source.parse().map(Self::Int32).map_err(drop),
            Int64 => source.parse().map(Self::Int64).map_err(drop),
        }
    }
}

impl Number {
    pub fn type_(&self) -> NumericType {
        use special::NumericType::*;

        match self {
            Self::Nat(_) => Nat,
            Self::Nat32(_) => Nat32,
            Self::Nat64(_) => Nat64,
            Self::Int(_) => Int,
            Self::Int32(_) => Int32,
            Self::Int64(_) => Int64,
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nat(value) => write!(f, "{value}"),
            Self::Nat32(value) => write!(f, "{value}"),
            Self::Nat64(value) => write!(f, "{value}"),
            Self::Int(value) => write!(f, "{value}"),
            Self::Int32(value) => write!(f, "{value}"),
            Self::Int64(value) => write!(f, "{value}"),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Text {
    Text(String),
}

impl fmt::Display for Text {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // @Task don't use Rust's Debug impl for str!
            Self::Text(text) => write!(f, "{text:?}"),
        }
    }
}

pub type LocalBinder = span::binder::LocalBinder<Identifier>;

#[derive(Clone, Default)]
pub struct Namespace {
    pub binders: Vec<DeclarationIndex>,
}

impl fmt::Debug for Namespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let binders = self.binders.iter().map(|binder| format!("{binder:?}")).join_with(' ');

        write!(f, "{binders}")
    }
}

#[derive(Clone)]
pub enum Exposure {
    Unrestricted,
    // @Task find a way to get rid of interior mutability here
    Restricted(Arc<Mutex<ExposureReach>>),
}

impl PartialEq for Exposure {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unrestricted, Self::Unrestricted) => true,
            (Self::Restricted(this), Self::Restricted(other)) => {
                *this.lock().unwrap() == *other.lock().unwrap()
            }
            _ => false,
        }
    }
}

impl Eq for Exposure {}

impl From<ExposureReach> for Exposure {
    fn from(exposure: ExposureReach) -> Self {
        Self::Restricted(Arc::new(Mutex::new(exposure)))
    }
}

impl fmt::Debug for Exposure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unrestricted => f.pad("*"),
            Self::Restricted(reach) => f.pad(&format!("{:?}", reach.lock().unwrap())),
        }
    }
}

/// How far up binding exposure _reaches_ in the tree of namespaces given by a path.
#[derive(Clone, PartialEq, Eq)]
pub enum ExposureReach {
    Resolved(LocalDeclarationIndex),
    PartiallyResolved(PartiallyResolvedPath),
}

impl fmt::Debug for ExposureReach {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::PartiallyResolved(reach) => write!(f, "{reach:?}"),
            Self::Resolved(reach) => write!(f, "{reach:?}"),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct PartiallyResolvedPath {
    /// The resolved part.
    pub namespace: LocalDeclarationIndex,
    /// The unresolved part.
    pub path: ast::Path,
}

impl fmt::Debug for PartiallyResolvedPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}.{}", self.namespace, self.path)
    }
}

// @Task find out if we can get rid of this type by letting `ModuleScope::lookup_value` resolve to the Binder
// if it's neutral
pub enum ValueView {
    Reducible(Expression),
    Neutral,
}

impl ValueView {
    pub fn is_neutral(&self) -> bool {
        matches!(self, Self::Neutral)
    }
}
