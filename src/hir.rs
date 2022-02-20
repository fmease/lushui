//! The high-level intermediate representation.

pub(crate) use crate::syntax::lowered_ast::Item;
use crate::{
    error::PossiblyErroneous,
    package::session::IntrinsicNumericType,
    resolver::{Component, FunctionScope},
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

pub type Declaration = Item<DeclarationKind>;

impl Declaration {
    pub(crate) fn constructor(&self) -> Option<&Constructor> {
        obtain!(&self.value, DeclarationKind::Constructor(constructor) => constructor)
    }
}

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
    pub binder: Option<Identifier>,
    pub target: Identifier,
}

impl From<Use> for DeclarationKind {
    fn from(use_: Use) -> Self {
        Self::Use(Box::new(use_))
    }
}

pub type Expression = Item<ExpressionKind>;

#[derive(Clone)]
pub enum ExpressionKind {
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

impl PossiblyErroneous for ExpressionKind {
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

impl From<PiType> for ExpressionKind {
    fn from(pi: PiType) -> Self {
        Self::PiType(Box::new(pi))
    }
}

impl From<Application<Expression>> for ExpressionKind {
    fn from(application: Application<Expression>) -> Self {
        Self::Application(Box::new(application))
    }
}

impl From<Number> for ExpressionKind {
    fn from(number: Number) -> Self {
        Self::Number(Box::new(number))
    }
}

impl From<Text> for ExpressionKind {
    fn from(text: Text) -> Self {
        Self::Text(Box::new(text))
    }
}

impl From<SomeSequence> for ExpressionKind {
    fn from(sequence: SomeSequence) -> Self {
        match sequence {}
    }
}

impl From<Binding> for ExpressionKind {
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
pub struct Substitution {
    pub substitution: interpreter::Substitution,
    pub expression: Expression,
}

impl From<Substitution> for ExpressionKind {
    fn from(substitution: Substitution) -> Self {
        Self::Substitution(Box::new(substitution))
    }
}

#[derive(Clone)]
pub struct IntrinsicApplication {
    pub callee: Identifier,
    pub arguments: Vec<Expression>,
}

impl From<IntrinsicApplication> for ExpressionKind {
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

impl From<IO> for ExpressionKind {
    fn from(io: IO) -> Self {
        Self::IO(Box::new(io))
    }
}

#[derive(Clone)]
pub struct Case {
    pub pattern: Pattern,
    pub body: Expression,
}

pub type Pattern = Item<PatternKind>;

#[derive(Clone)]
#[allow(clippy::box_collection)]
pub enum PatternKind {
    Number(Box<Number>),
    Text(Box<Text>),
    Binding(Box<Binding>),
    Binder(Box<Binder>),
    Application(Box<Application<Pattern>>),
    Error,
}

impl PossiblyErroneous for PatternKind {
    fn error() -> Self {
        Self::Error
    }
}

impl From<Number> for PatternKind {
    fn from(number: Number) -> Self {
        Self::Number(Box::new(number))
    }
}

impl From<Text> for PatternKind {
    fn from(text: Text) -> Self {
        Self::Text(Box::new(text))
    }
}

impl From<SomeSequence> for PatternKind {
    fn from(sequence: SomeSequence) -> Self {
        match sequence {}
    }
}

impl From<Binding> for PatternKind {
    fn from(binding: Binding) -> Self {
        Self::Binding(Box::new(binding))
    }
}

#[derive(Clone)]
pub struct Binder(pub Identifier);

impl From<Binder> for PatternKind {
    fn from(binder: Binder) -> Self {
        Self::Binder(Box::new(binder))
    }
}

impl From<Application<Pattern>> for PatternKind {
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
