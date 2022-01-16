//! The high-level intermediate representation.

use crate::{
    error::PossiblyErroneous,
    resolver::{Capsule, FunctionScope},
    span::{SourceFileIndex, Span},
    syntax::{
        ast::Explicitness,
        lowered_ast::{Item, Number},
    },
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
#[allow(clippy::box_collection)]
pub enum ExpressionKind {
    PiType(Box<PiType>),
    Application(Box<Application<Expression>>),
    Type,
    Number(Box<Number>),
    Text(Box<String>),
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

#[derive(Clone)]
pub struct CaseAnalysis {
    pub scrutinee: Expression,
    pub cases: Vec<Case>,
}

#[derive(Clone)]
pub struct Substitution {
    pub substitution: interpreter::Substitution,
    pub expression: Expression,
}

#[derive(Clone)]
pub struct IntrinsicApplication {
    pub callee: Identifier,
    pub arguments: Vec<Expression>,
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
    Text(Box<String>),
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

pub(crate) macro expr($( $tree:tt )+) {
    crate::item::item!(crate::hir, ExpressionKind, Box; $( $tree )+)
}
