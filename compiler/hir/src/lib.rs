//! The high-level intermediate representation.
#![feature(
    decl_macro,
    default_free_fn,
    generic_associated_types,
    type_alias_impl_trait
)]

pub use format::{Display, DisplayContext};
pub use identifier::{DeBruijnIndex, DeclarationIndex, Identifier, Index, LocalDeclarationIndex};
use lushui_ast::Explicitness;
use std::sync::{Arc, Mutex};
// use lushui_component::Component;
use lushui_error::PossiblyErroneous;
pub use lushui_lowered_ast::Item;
use lushui_span::{SourceFileIndex, Span};
use lushui_utilities::{obtain, Int, Nat};

mod format;
pub(crate) mod identifier;
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

#[derive(Clone)]
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
    IO(Box<IO>),
    Error,
}

impl PossiblyErroneous for BareExpression {
    fn error() -> Self {
        Self::Error
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
pub struct Projection {
    // @Task
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

#[derive(Clone)]
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

#[derive(Clone)]
pub struct Binding(pub Identifier);

#[derive(Clone)]
pub struct Application<T> {
    pub callee: T,
    pub explicitness: Explicitness,
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

#[derive(Clone, PartialEq, Eq)]
pub enum Text {
    Text(String),
}

// @Temporary placeholder until we can desugar sequence literals to
// concrete constructors of sequence-like data types
pub enum SomeSequence {}

// @Beacon @Beacon @Beacon @Task move into separate mod (those bindings that used to be
// in resolver):

#[derive(Clone, Default)]
pub struct Namespace {
    pub binders: Vec<DeclarationIndex>,
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

/// How far up binding exposure _reaches_ in the tree of namespaces given by a path.
#[derive(Clone, PartialEq, Eq)]
pub enum ExposureReach {
    Resolved(LocalDeclarationIndex),
    PartiallyResolved(PartiallyResolvedPath),
}

#[derive(Clone, PartialEq, Eq)]
pub struct PartiallyResolvedPath {
    /// The resolved part.
    pub namespace: LocalDeclarationIndex,
    /// The unresolved part.
    pub path: lushui_ast::Path,
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
