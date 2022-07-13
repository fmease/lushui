//! The high-level intermediate representation.

pub(crate) use crate::syntax::lowered_ast::Item;
use crate::{
    component::Component,
    error::PossiblyErroneous,
    resolver::FunctionScope,
    session::IntrinsicNumericType,
    span::{SourceFileIndex, Span},
    syntax::ast::Explicitness,
    typer::interpreter,
    utility::obtain,
};
pub(crate) use identifier::{
    DeBruijnIndex, DeclarationIndex, Identifier, Index, LocalDeclarationIndex,
};

mod format;
pub(crate) mod identifier;

pub type Declaration = Item<BareDeclaration>;

impl Declaration {
    pub(crate) fn constructor(&self) -> Option<&Constructor> {
        obtain!(&self.bare, BareDeclaration::Constructor(constructor) => constructor)
    }
}

pub enum BareDeclaration {
    Function(Box<Function>),
    Data(Box<Data>),
    Constructor(Box<Constructor>),
    Module(Box<Module>),
    Use(Box<Use>),
    Error,
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
    Substitution(Box<Substitution>),
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
pub struct Substitution {
    pub substitution: interpreter::Substitution,
    pub expression: Expression,
}

impl From<Substitution> for BareExpression {
    fn from(substitution: Substitution) -> Self {
        Self::Substitution(Box::new(substitution))
    }
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
    pub(crate) index: usize, // @Task IOIndex
    pub(crate) arguments: Vec<Expression>,
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
    Nat(crate::utility::Nat),
    Nat32(u32),
    Nat64(u64),
    Int(crate::utility::Int),
    Int32(i32),
    Int64(i64),
}

impl Number {
    pub(crate) fn parse(source: &str, type_: IntrinsicNumericType) -> Result<Self, ()> {
        use IntrinsicNumericType::*;

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
    pub(crate) fn type_(&self) -> IntrinsicNumericType {
        use IntrinsicNumericType::*;

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
