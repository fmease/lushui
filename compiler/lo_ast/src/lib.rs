//! The lowered abstract syntax tree (Lo-AST).
#![feature(adt_const_params, decl_macro)]
#![allow(incomplete_features)] // adt_const_params

use ast::{Ident, LocalBinder, NumLit, ParamKind, Path, SeqLit, TextLit, Wildcard};
use diagnostics::{error::PossiblyErroneous, reporter::ErasedReportedError};
use span::{Spanned, SrcFileIdx};

pub use attr::{Attr, AttrName, Attrs, BareAttr};
pub use format::Display;

mod format;

pub mod attr;

pub type Item<Bare> = span::item::Item<Bare, attr::Attrs>;

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

pub struct DataTy {
    pub binder: Ident,
    pub ty: Expr,
    pub decls: Option<Vec<Decl>>,
}

impl From<DataTy> for BareDecl {
    fn from(ty: DataTy) -> Self {
        Self::DataTy(Box::new(ty))
    }
}

/// A constructor.
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
    pub binder: Ident,
    pub target: Path,
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
    /// The type of types.
    ///
    /// This construct doesn't have corresponding syntax in the surface language.
    /// It's synthesized during lowering.
    Ty,
    /// A hygienic local binding.
    ///
    /// This construct doesn't have corresponding syntax in the surface language.
    /// It's synthesized during lowering.
    LocalBinding(DeBruijnLevel),
    Wildcard(Box<Wildcard>),
    NumLit(Box<NumLit>),
    TextLit(Box<TextLit>),
    Path(Box<Path>),
    App(Box<App<Expr>>),
    SeqLit(Box<SeqLit<Expr>>),
    RecLit(Box<RecLit<Expr>>),
    Proj(Box<Proj>),
    PiTy(Box<PiTy>),
    LamLit(Box<LamLit>),
    CaseAnalysis(Box<CaseAnalysis>),
    UseBinding,
    Error(ErasedReportedError),
}

impl PossiblyErroneous for BareExpr {
    fn error(error: ErasedReportedError) -> Self {
        Self::Error(error)
    }
}

impl From<DeBruijnLevel> for BareExpr {
    fn from(level: DeBruijnLevel) -> Self {
        Self::LocalBinding(level)
    }
}

impl From<Box<Wildcard>> for BareExpr {
    fn from(wildcard: Box<Wildcard>) -> Self {
        Self::Wildcard(wildcard)
    }
}

impl From<Wildcard> for BareExpr {
    fn from(wildcard: Wildcard) -> Self {
        Box::new(wildcard).into()
    }
}

impl From<Box<NumLit>> for BareExpr {
    fn from(num: Box<NumLit>) -> Self {
        Self::NumLit(num)
    }
}

impl From<Box<TextLit>> for BareExpr {
    fn from(text: Box<TextLit>) -> Self {
        Self::TextLit(text)
    }
}

impl From<Box<Path>> for BareExpr {
    fn from(path: Box<Path>) -> Self {
        Self::Path(path)
    }
}

impl From<Path> for BareExpr {
    fn from(path: Path) -> Self {
        Box::new(path).into()
    }
}

impl From<App<Expr>> for BareExpr {
    fn from(app: App<Expr>) -> Self {
        Self::App(Box::new(app))
    }
}

impl From<SeqLit<Expr>> for BareExpr {
    fn from(seq: SeqLit<Expr>) -> Self {
        Self::SeqLit(Box::new(seq))
    }
}

impl From<RecLit<Expr>> for BareExpr {
    fn from(rec: RecLit<Expr>) -> Self {
        Self::RecLit(Box::new(rec))
    }
}

/// A record field projection.
#[derive(Clone)]
pub struct Proj {
    pub basis: Expr,
    pub field: Ident,
}

impl From<Proj> for BareExpr {
    fn from(projection: Proj) -> Self {
        Self::Proj(Box::new(projection))
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

#[derive(Clone)]
pub struct Case {
    pub pattern: Pat,
    pub body: Expr,
}

/// A pattern.
pub type Pat = Item<BarePattern>;

#[derive(Clone)]
pub enum BarePattern {
    Wildcard(Box<Wildcard>),
    NumLit(Box<NumLit>),
    TextLit(Box<TextLit>),
    Path(Box<Path>),
    LetBinding(LocalBinder),
    App(Box<App<Pat>>),
    SeqLit(Box<SeqLit<Pat>>),
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

impl From<Box<NumLit>> for BarePattern {
    fn from(num: Box<NumLit>) -> Self {
        Self::NumLit(num)
    }
}

impl From<NumLit> for BarePattern {
    fn from(number: NumLit) -> Self {
        Box::new(number).into()
    }
}

impl From<Box<TextLit>> for BarePattern {
    fn from(text: Box<TextLit>) -> Self {
        Self::TextLit(text)
    }
}

impl From<TextLit> for BarePattern {
    fn from(text: TextLit) -> Self {
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

impl From<App<Pat>> for BarePattern {
    fn from(app: App<Pat>) -> Self {
        Self::App(Box::new(app))
    }
}

impl From<SeqLit<Pat>> for BarePattern {
    fn from(seq: SeqLit<Pat>) -> Self {
        Self::SeqLit(Box::new(seq))
    }
}

/// A function application.
#[derive(Clone)]
pub struct App<T> {
    pub kind: ParamKind,
    pub callee: T,
    pub arg: T,
}

/// A record literal.
#[derive(Clone)]
pub struct RecLit<T> {
    pub path: Option<Path>,
    pub fields: Spanned<Vec<Field<T>>>,
    pub base: Option<T>,
}

#[derive(Clone)]

pub struct Field<T> {
    pub binder: Ident,
    pub body: T,
}

#[derive(Clone, Copy)]
pub struct DeBruijnLevel(pub usize);
