use crate::{
    decl::{Use, UsePathTree},
    App, Ident, Item, LocalBinder, NumLit, Params, Pat, Path, RecLit, SeqLit, TextLit, Wildcard,
};
use diagnostics::{error::PossiblyErroneous, reporter::ErasedReportedError};

/// An expression.
pub type Expr = Item<BareExpr>;

/// A location-less expression.
#[derive(Clone, PartialEq, Eq)]
pub enum BareExpr {
    Wildcard(Box<Wildcard>),
    NumLit(Box<NumLit>),
    TextLit(Box<TextLit>),
    Path(Box<Path>),
    App(Box<App<Expr>>),
    SeqLit(Box<SeqLit<Expr>>),
    RecLit(Box<RecLit<Expr>>),
    Proj(Box<Proj>),
    QuantifiedTy(Box<QuantifiedTy>),
    LamLit(Box<LamLit>),
    CaseAnalysis(Box<CaseAnalysis>),
    LetBinding(Box<LetBinding>),
    UseBinding(Box<UseBinding>),
    DoBlock(Box<DoBlock>),
    Error(ErasedReportedError),
}

impl From<Wildcard> for BareExpr {
    fn from(wildcard: Wildcard) -> Self {
        Self::Wildcard(Box::new(wildcard))
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

impl From<Path> for BareExpr {
    fn from(path: Path) -> Self {
        Self::Path(Box::new(path))
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

impl From<Proj> for BareExpr {
    fn from(proj: Proj) -> Self {
        Self::Proj(Box::new(proj))
    }
}

impl From<QuantifiedTy> for BareExpr {
    fn from(ty: QuantifiedTy) -> Self {
        Self::QuantifiedTy(Box::new(ty))
    }
}

impl From<LamLit> for BareExpr {
    fn from(lambda: LamLit) -> Self {
        Self::LamLit(Box::new(lambda))
    }
}

impl From<CaseAnalysis> for BareExpr {
    fn from(analysis: CaseAnalysis) -> Self {
        Self::CaseAnalysis(Box::new(analysis))
    }
}

impl From<LetBinding> for BareExpr {
    fn from(binding: LetBinding) -> Self {
        Self::LetBinding(Box::new(binding))
    }
}

impl From<UseBinding> for BareExpr {
    fn from(binding: UseBinding) -> Self {
        Self::UseBinding(Box::new(binding))
    }
}

impl From<DoBlock> for BareExpr {
    fn from(do_: DoBlock) -> Self {
        Self::DoBlock(Box::new(do_))
    }
}

impl PossiblyErroneous for BareExpr {
    fn error(error: ErasedReportedError) -> Self {
        Self::Error(error)
    }
}

/// A record field projection: A projection from a record to one of its fields.
///
/// # Examples
///
/// ```lushui
/// main = (process via)::component
/// ```
///
/// * `(process via)` is the *basis*
/// * `component` is the *field*
///
/// ```lushui
/// main = compound::first::second
/// ```
///
/// * `compound::first` is the *basis*
/// * `second` is the *field*
#[derive(Clone, PartialEq, Eq)]
pub struct Proj {
    pub basis: Expr,
    pub field: Ident,
}

/// A quantified type.
///
/// It is either a
///
/// * Π-type (dependent function type) or a
/// * Σ-type (dependent pair type)
///
/// depending on the *quantifier*.
///
/// # Examples
///
/// ```lushui
/// Pi = Int -> Int -> Int
/// Sigma = Nat ** Nat ** Nat
/// ```
///
/// * inside of function `Pi`
///   * the *quantifier* is [Π]
///   * the very first `Int` is the *parameter*
///   * `Int -> Int` is the *codomain*
/// * inside of function `Sigma`
///   * the *quantifier* is [Σ]
///   * the very first `Nat` is the *parameter*
///   * `Nat ** Nat` is the *codomain*
///
/// ```lushui
/// Pi = For '(A: Type) '(n: Nat) (v: Vec A n) -> List A
/// Sigma = For '(n: Nat) (v: Vec E n) ** E
/// ```
///
/// * inside of function `Pi`
///   * the *quantifier* is [Π]
///   * `'(A: Type)`, `'(n: Nat)` and `(v: Vec A n)` are the *parameters*
///   * `List A` is the *codomain*
/// * inside of function `Sigma`
///   * the *quantifier* is [Σ]
///   * `'(n: Nat)` and `(v: Vec E n)` are the *parameters*
///   * `E` at the very end is the *codomain*
///
/// [Π]: Quantifier::Pi
/// [Σ]: Quantifier::Sigma
// @Task add context parameters to docs
#[derive(Clone, PartialEq, Eq)]
pub struct QuantifiedTy {
    pub quantifier: Quantifier,
    pub params: Params,
    pub codomain: Expr,
}

/// Infix quantifier used by [quantified types].
///
/// [quantified types]: QuantifiedType
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Quantifier {
    /// Used for Π-types. Denoted as `->` in the surface language.
    Pi,
    /// Used for Σ-types. Denoted as `**` in the surface language.
    Sigma,
}

/// A lambda literal.
///
/// # Examples
///
/// ```lushui
/// main = for 'A (a: A): A => identity a
/// ```
///
/// * `'A` and `(a: A)` are the *parameters*
/// * `A` right before `=>` is the *codomain*
/// * `identity a` is the *body*
#[derive(Clone, PartialEq, Eq)]
pub struct LamLit {
    pub params: Params,
    pub codomain: Option<Expr>,
    pub body: Expr,
}

/// A case-analysis expression.
///
/// # Examples
///
/// ```lushui
/// not (x: Bool): Bool =
///     case x of
///         false => true
///         true => false
/// ```
///
/// * `x` between `case` and `of` is the *scrutinee*
/// * the last two lines contain the *cases*
#[derive(Clone, PartialEq, Eq)]
pub struct CaseAnalysis {
    pub scrutinee: Expr,
    pub cases: Vec<Case>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Case {
    pub pattern: Pat,
    pub body: Expr,
}

/// A let-binding.
///
/// # Examples
///
/// ```lushui
/// function (n: Nat) =
///     let operation x y: Nat = Nat.+ x (Nat.* y 2) in
///     operation (operation n 90) 80
/// ```
///
/// * `operation` following the `let` is the *binder*
/// * `x` and `y` are the *parameters*
/// * `Nat` following the colon in line 2 is the *type*
/// * `Nat.+ x (Nat.* y 2)` is the *body*
/// * `operation (operation n 90) 80` is the *scope*
#[derive(Clone, PartialEq, Eq)]
pub struct LetBinding {
    pub binder: LocalBinder,
    pub params: Params,
    pub ty: Option<Expr>,
    pub body: Option<Expr>,
    pub scope: Expr,
}

/// A use-binding.
///
/// # Examples
///
/// ```lushui
/// main =
///     use extern.core.((nat.Nat as N) (int.Int as I)) in
///     f N.1 I.-1
/// ```
///
/// * between `use` and `in` are the *bindings* (the use-path tree)
/// * `f N.1 I.-1` is the *scope*
#[derive(Clone, PartialEq, Eq)]
pub struct UseBinding {
    pub bindings: UsePathTree,
    pub scope: Expr,
}

#[derive(Clone, PartialEq, Eq)]
pub struct DoBlock {
    pub statements: Vec<Statement>,
}

// @Task rename this to BareStatement and redefine Statement to be an Item<BareStatement>
#[derive(Clone, PartialEq, Eq)]
pub enum Statement {
    Let(LetStatement),
    Use(Use),
    Expr(Expr),
}

/// A let-statement.
///
/// # Examples
///
/// ```lushui
/// main = do
///     let process parameter: Result = compute (prepare parameter)
///     pure unit
/// ```
///
/// * `process` is the *binder*
/// * `parameter` is the *parameter*
/// * `Result` is the *type*
/// * `compute (prepare parameter)` is the *body* with mode [plain]
///
/// ```lushui
/// main = do
///     let result: Result <- compute flag
///     pure unit
/// ```
///
/// * `result` is the *binder*
/// * there are no *parameters*
/// * `Result` is the *type*
/// * `compute flag` is the *body* with mode [effectful]
///
/// [plain]: BindingMode::Plain
/// [effectful]: BindingMode::Effectful
#[derive(Clone, PartialEq, Eq)]
pub struct LetStatement {
    pub binder: LocalBinder,
    /// Providing parameters for [effectful] let-statements is only *syntactically* valid,
    /// not semantically. They are rejected in the lowerer.
    ///
    /// [effectful]: BindingMode::Effectful
    pub params: Params,
    pub ty: Option<Expr>,
    pub body: Option<(BindingMode, Expr)>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BindingMode {
    Plain,
    Effectful,
}
