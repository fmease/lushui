//! The high-level intermediate representation.
#![feature(decl_macro, default_free_fn, never_type, never_type_fallback)]

use ast::Explicitness;
pub use entity::{Entity, EntityKind};
use error::PossiblyErroneous;
pub use identifier::{DeBruijnIndex, DeclarationIndex, Identifier, Index, LocalDeclarationIndex};
use joinery::JoinableIterator;
pub use lowered_ast::Item;
use span::{SourceFileIndex, Span};
use std::{
    fmt,
    sync::{Arc, Mutex},
};
use utilities::{obtain, Int, Nat};

mod entity;
mod identifier;
pub mod interfaceable;
pub mod intrinsic;
pub mod known;

pub type Declaration = Item<BareDeclaration>;

pub enum BareDeclaration {
    Function(Box<Function>),
    Data(Box<Data>),
    Constructor(Box<Constructor>),
    Module(Box<Module>),
    Use(Box<Use>),
    Error,
}

impl BareDeclaration {
    // @Task impl as TryFrom if possible
    pub fn constructor(&self) -> Option<&Constructor> {
        obtain!(self, BareDeclaration::Constructor(constructor) => constructor)
    }
}

impl PossiblyErroneous for BareDeclaration {
    fn error() -> Self {
        Self::Error
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
    pub binder: Option<Identifier>,
    pub target: Identifier,
}

impl From<Use> for BareDeclaration {
    fn from(use_: Use) -> Self {
        Self::Use(Box::new(use_))
    }
}

pub type Expression = Item<BareExpression>;

#[derive(Clone, Debug)]
pub enum BareExpression {
    PiType(Box<PiType>),
    Application(Box<Application<Expression>>),
    Type,
    Number(Box<Number>),
    Text(Box<Text>),
    Binding(Box<Binding>),
    Lambda(Box<Lambda>),
    UseIn,
    CaseAnalysis(Box<CaseAnalysis>),
    Substituted(Box<Substituted>),
    IntrinsicApplication(Box<IntrinsicApplication>),
    Projection(Box<Projection>),
    // @Task rename to Effect
    IO(Box<IO>),
    // @Task make this an effect
    Panic(Box<Panic>),
    Error,
}

impl PossiblyErroneous for BareExpression {
    fn error() -> Self {
        Self::Error
    }
}

#[derive(Clone, Debug)]
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

impl From<SomeSequence> for BareExpression {
    fn from(sequence: SomeSequence) -> Self {
        match sequence {}
    }
}

impl From<Binding> for BareExpression {
    fn from(binding: Binding) -> Self {
        Self::Binding(Box::new(binding))
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct CaseAnalysis {
    pub scrutinee: Expression,
    pub cases: Vec<Case>,
}

impl From<CaseAnalysis> for BareExpression {
    fn from(analysis: CaseAnalysis) -> Self {
        Self::CaseAnalysis(Box::new(analysis))
    }
}

#[derive(Clone, Debug)]
pub struct Substituted {
    pub substitution: Substitution,
    pub expression: Expression,
}

impl From<Substituted> for BareExpression {
    fn from(substitution: Substituted) -> Self {
        Self::Substituted(Box::new(substitution))
    }
}

#[derive(Clone, Debug)]
pub enum Substitution {
    Shift(usize),
    Use(Box<Substitution>, Expression),
}

#[derive(Clone, Debug)]
pub struct IntrinsicApplication {
    pub callee: Identifier,
    pub arguments: Vec<Expression>,
}

impl From<IntrinsicApplication> for BareExpression {
    fn from(application: IntrinsicApplication) -> Self {
        Self::IntrinsicApplication(Box::new(application))
    }
}

#[derive(Clone, Debug)]
pub struct Projection {
    // @Task
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
// @Temporary
pub struct Panic {
    pub message: String,
}

impl From<Panic> for BareExpression {
    fn from(panic: Panic) -> Self {
        Self::Panic(Box::new(panic))
    }
}

#[derive(Clone, Debug)]
pub struct Case {
    pub pattern: Pattern,
    pub body: Expression,
}

pub type Pattern = Item<BarePattern>;

#[derive(Clone, Debug)]
#[allow(clippy::box_collection)]
pub enum BarePattern {
    Number(Box<Number>),
    Text(Box<Text>),
    Binding(Box<Binding>),
    Binder(Box<Binder>),
    Application(Box<Application<Pattern>>),
    Error,
}

impl PossiblyErroneous for BarePattern {
    fn error() -> Self {
        Self::Error
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

impl From<SomeSequence> for BarePattern {
    fn from(sequence: SomeSequence) -> Self {
        match sequence {}
    }
}

impl From<Binding> for BarePattern {
    fn from(binding: Binding) -> Self {
        Self::Binding(Box::new(binding))
    }
}

#[derive(Clone, Debug)]
pub struct Binder(pub Identifier);

impl From<Binder> for BarePattern {
    fn from(binder: Binder) -> Self {
        Self::Binder(Box::new(binder))
    }
}

impl From<Application<Pattern>> for BarePattern {
    fn from(application: Application<Pattern>) -> Self {
        Self::Application(Box::new(application))
    }
}

#[derive(Clone, Debug)]
pub struct Binding(pub Identifier);

#[derive(Clone, Debug)]
pub struct Application<T> {
    pub callee: T,
    pub explicitness: Explicitness,
    pub argument: T,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Number {
    Nat(Nat),
    Nat32(u32),
    Nat64(u64),
    Int(Int),
    Int32(i32),
    Int64(i64),
}

impl Number {
    pub fn parse(source: &str, type_: intrinsic::NumericType) -> Result<Self, ()> {
        use intrinsic::NumericType::*;

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
    pub fn type_(&self) -> intrinsic::NumericType {
        use intrinsic::NumericType::*;

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

#[derive(Clone, PartialEq, Eq, Debug)]
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

// @Temporary placeholder until we can desugar sequence literals to
// concrete constructors of sequence-like data types
pub enum SomeSequence {}

#[derive(Clone, Default)]
pub struct Namespace {
    pub binders: Vec<DeclarationIndex>,
}

impl fmt::Debug for Namespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.binders
                .iter()
                .map(|binding| format!("{binding:?}"))
                .join_with(' ')
        )
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
            Self::Unrestricted => write!(f, "*"),
            Self::Restricted(reach) => write!(f, "{:?}", reach.lock().unwrap()),
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
