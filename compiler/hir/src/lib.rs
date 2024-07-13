//! The high-level intermediate representation.
#![feature(decl_macro)]

use diagnostics::{error::PossiblyErroneous, reporter::ErasedReportedError};
use joinery::JoinableIterator;
use span::{Spanned, SrcFileIdx};
use special::NumTy;
use std::{
    fmt,
    sync::{Arc, Mutex},
};
use utility::{obtain, Int, Nat};

pub use ast::ParamKind;
pub use entity::{Entity, EntityKind};
pub use ident::{DeBruijnIdx, DeclIdx, Ident, Index, LocalDeclIdx};
pub use lo_ast::{attr, Attr, AttrName, Attrs, BareAttr, Item};

mod entity;
mod ident;

// @Task get rid of this smh.
pub mod interfaceable;
pub mod special;

/// A declaration.
pub type Decl = Item<BareDecl>;

/// A location-less declaration.
pub enum BareDecl {
    Func(Box<Func>),
    DataTy(Box<DataTy>),
    Ctor(Box<Ctor>),
    Module(Box<Module>),
    Use(Box<Use>),
    Error(ErasedReportedError),
}

impl BareDecl {
    // @Task impl as TryFrom if possible
    pub fn ctor(&self) -> Option<&Ctor> {
        obtain!(self, BareDecl::Ctor(ctor) => ctor)
    }
}

impl PossiblyErroneous for BareDecl {
    fn error(error: ErasedReportedError) -> Self {
        Self::Error(error)
    }
}

/// A function.
pub struct Func {
    pub binder: Ident,
    pub ty: Expr,
    pub body: Option<Expr>,
}

impl From<Func> for BareDecl {
    fn from(func: Func) -> Self {
        Self::Func(Box::new(func))
    }
}

/// A data type
pub struct DataTy {
    pub binder: Ident,
    pub ty: Expr,
    pub ctors: Option<Vec<Decl>>,
}

impl From<DataTy> for BareDecl {
    fn from(ty: DataTy) -> Self {
        Self::DataTy(Box::new(ty))
    }
}

pub struct Ctor {
    pub binder: Ident,
    pub ty: Expr,
}

impl From<Ctor> for BareDecl {
    fn from(ctor: Ctor) -> Self {
        Self::Ctor(Box::new(ctor))
    }
}

pub struct Module {
    pub binder: Ident,
    pub file: SrcFileIdx,
    pub decls: Vec<Decl>,
}

impl From<Module> for BareDecl {
    fn from(module: Module) -> Self {
        Self::Module(Box::new(module))
    }
}

pub struct Use {
    pub binder: Option<Ident>,
    pub target: Ident,
}

impl From<Use> for BareDecl {
    fn from(use_: Use) -> Self {
        Self::Use(Box::new(use_))
    }
}

/// An expression.
pub type Expr = Item<BareExpr>;

/// A location-less expression.
#[derive(Clone)]
pub enum BareExpr {
    NumLit(Box<NumLit>),
    TextLit(Box<TextLit>),
    Binding(Box<Binding>),
    App(Box<App<Expr>>),
    IntrApp(Box<IntrApp>),
    RecLit(Box<RecLit>),
    Proj(Box<Proj>),
    PiTy(Box<PiTy>),
    LamLit(Box<LamLit>),
    IO(Box<IO>),
    CaseAnalysis(Box<CaseAnalysis>),
    Substed(Box<Substed>),
    Error(ErasedReportedError),
}

impl PossiblyErroneous for BareExpr {
    fn error(error: ErasedReportedError) -> Self {
        Self::Error(error)
    }
}

#[derive(Clone)]
pub struct PiTy {
    pub kind: ParamKind,
    pub binder: Option<Ident>,
    pub domain: Expr,
    pub codomain: Expr,
}

impl From<PiTy> for BareExpr {
    fn from(pi: PiTy) -> Self {
        Self::PiTy(Box::new(pi))
    }
}

impl From<App<Expr>> for BareExpr {
    fn from(app: App<Expr>) -> Self {
        Self::App(Box::new(app))
    }
}

impl From<NumLit> for BareExpr {
    fn from(num: NumLit) -> Self {
        Self::NumLit(Box::new(num))
    }
}

impl From<TextLit> for BareExpr {
    fn from(text: TextLit) -> Self {
        Self::TextLit(Box::new(text))
    }
}

impl From<Binding> for BareExpr {
    fn from(binding: Binding) -> Self {
        Self::Binding(Box::new(binding))
    }
}

/// A lambda literal.
#[derive(Clone)]
pub struct LamLit {
    pub kind: ParamKind,
    pub binder: Option<Ident>,
    pub domain: Option<Expr>,
    pub codomain: Option<Expr>,
    pub body: Expr,
}

impl From<LamLit> for BareExpr {
    fn from(lambda: LamLit) -> Self {
        Self::LamLit(Box::new(lambda))
    }
}

#[derive(Clone)]
pub struct CaseAnalysis {
    pub scrutinee: Expr,
    pub cases: Vec<Case>,
}

impl From<CaseAnalysis> for BareExpr {
    fn from(analysis: CaseAnalysis) -> Self {
        Self::CaseAnalysis(Box::new(analysis))
    }
}

/// A substituted expression.
#[derive(Clone)]
pub struct Substed {
    pub subst: Subst,
    pub expr: Expr,
}

impl From<Substed> for BareExpr {
    fn from(substed: Substed) -> Self {
        Self::Substed(Box::new(substed))
    }
}

/// A substitution.
#[derive(Clone)]
pub enum Subst {
    Shift(usize),
    Use(Box<Subst>, Expr),
}

/// An intrinstic application.
#[derive(Clone)]
pub struct IntrApp {
    pub callee: Ident,
    pub args: Vec<Expr>,
}

impl From<IntrApp> for BareExpr {
    fn from(app: IntrApp) -> Self {
        Self::IntrApp(Box::new(app))
    }
}

/// A record literal.
#[derive(Clone)]
pub struct RecLit {
    pub ty: Spanned<DeclIdx>,
    pub fields: Vec<Field>,
}

impl From<RecLit> for BareExpr {
    fn from(record: RecLit) -> Self {
        Self::RecLit(Box::new(record))
    }
}

#[derive(Clone)]
pub struct Field {
    pub binder: ast::Ident,
    pub body: Expr,
}

#[derive(Clone)]
/// A record field projection.
pub struct Proj {
    pub basis: Expr,
    pub field: ast::Ident,
}

impl From<Proj> for BareExpr {
    fn from(proj: Proj) -> Self {
        Self::Proj(Box::new(proj))
    }
}

#[derive(Clone)]
pub struct IO {
    pub index: usize, // @Task IOIndex
    pub args: Vec<Expr>,
    // @Task continuation: Option<Expr>
}

impl From<IO> for BareExpr {
    fn from(io: IO) -> Self {
        Self::IO(Box::new(io))
    }
}

#[derive(Clone)]
pub struct Case {
    pub pat: Pat,
    pub body: Expr,
}

/// A pattern.
pub type Pat = Item<BarePat>;

#[derive(Clone)]
#[allow(clippy::box_collection)]
pub enum BarePat {
    NumLit(Box<NumLit>),
    TextLit(Box<TextLit>),
    // @Task rename Binding & LetBinding in a way that makes it understandable which
    // one of the two *defines* and which one *references*.
    Binding(Binding),
    LetBinding(LocalBinder),
    App(Box<App<Pat>>),
    Error(ErasedReportedError),
}

impl PossiblyErroneous for BarePat {
    fn error(error: ErasedReportedError) -> Self {
        Self::Error(error)
    }
}

impl From<NumLit> for BarePat {
    fn from(num: NumLit) -> Self {
        Self::NumLit(Box::new(num))
    }
}

impl From<TextLit> for BarePat {
    fn from(text: TextLit) -> Self {
        Self::TextLit(Box::new(text))
    }
}

impl From<Binding> for BarePat {
    fn from(binding: Binding) -> Self {
        Self::Binding(binding)
    }
}

impl From<LocalBinder> for BarePat {
    fn from(binder: LocalBinder) -> Self {
        Self::LetBinding(binder)
    }
}

impl From<App<Pat>> for BarePat {
    fn from(app: App<Pat>) -> Self {
        Self::App(Box::new(app))
    }
}

#[derive(Clone)]
pub struct Binding(pub Ident); // @Task rename 0 -> binder again

/// A function application.
#[derive(Clone)]
pub struct App<T> {
    pub kind: ParamKind,
    pub callee: T,
    pub arg: T,
}

/// A number literal.
#[derive(Clone, PartialEq, Eq)]
pub enum NumLit {
    Nat(Nat),
    Nat32(u32),
    Nat64(u64),
    Int(Int),
    Int32(i32),
    Int64(i64),
}

impl NumLit {
    pub fn parse(source: &str, ty: NumTy) -> Result<Self, ()> {
        use special::NumTy::*;

        match ty {
            Nat => source.parse().map(Self::Nat).map_err(drop),
            Nat32 => source.parse().map(Self::Nat32).map_err(drop),
            Nat64 => source.parse().map(Self::Nat64).map_err(drop),
            Int => source.parse().map(Self::Int).map_err(drop),
            Int32 => source.parse().map(Self::Int32).map_err(drop),
            Int64 => source.parse().map(Self::Int64).map_err(drop),
        }
    }
}

impl NumLit {
    pub fn ty(&self) -> NumTy {
        use special::NumTy::*;

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

impl fmt::Display for NumLit {
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

/// A text literal.
#[derive(Clone, PartialEq, Eq)]
pub enum TextLit {
    Text(String),
}

impl fmt::Display for TextLit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // @Task don't use Rust's Debug impl for str!
            Self::Text(text) => write!(f, "{text:?}"),
        }
    }
}

pub type LocalBinder = span::binder::LocalBinder<Ident>;

#[derive(Clone, Default)]
pub struct Namespace {
    pub binders: Vec<DeclIdx>,
}

impl fmt::Debug for Namespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let binders = self
            .binders
            .iter()
            .map(|binder| format!("{binder:?}"))
            .join_with(' ');

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
    Resolved(LocalDeclIdx),
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
    pub namespace: LocalDeclIdx,
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
    Reducible(Expr),
    Neutral,
}

impl ValueView {
    pub fn is_neutral(&self) -> bool {
        matches!(self, Self::Neutral)
    }
}
