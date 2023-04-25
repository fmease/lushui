use crate::{
    declaration::{Use, UsePathTree},
    Application, Identifier, Item, LocalBinder, NumberLiteral, Parameters, Path, Pattern,
    RecordLiteral, SequenceLiteral, TextLiteral, Wildcard,
};
use diagnostics::{error::PossiblyErroneous, reporter::ErasedReportedError};

/// An expression.
pub type Expression = Item<BareExpression>;

/// An expression without an enclosing [`span::Span`].
#[derive(Clone, PartialEq, Eq)]
pub enum BareExpression {
    Wildcard(Box<Wildcard>),
    NumberLiteral(Box<NumberLiteral>),
    TextLiteral(Box<TextLiteral>),
    Path(Box<Path>),
    Application(Box<Application<Expression>>),
    SequenceLiteral(Box<SequenceLiteral<Expression>>),
    RecordLiteral(Box<RecordLiteral<Expression>>),
    Projection(Box<Projection>),
    QuantifiedType(Box<QuantifiedType>),
    LambdaLiteral(Box<LambdaLiteral>),
    CaseAnalysis(Box<CaseAnalysis>),
    LetBinding(Box<LetBinding>),
    UseBinding(Box<UseBinding>),
    DoBlock(Box<DoBlock>),
    Error(ErasedReportedError),
}

impl From<Wildcard> for BareExpression {
    fn from(wildcard: Wildcard) -> Self {
        Self::Wildcard(Box::new(wildcard))
    }
}

impl From<NumberLiteral> for BareExpression {
    fn from(number: NumberLiteral) -> Self {
        Self::NumberLiteral(Box::new(number))
    }
}

impl From<TextLiteral> for BareExpression {
    fn from(text: TextLiteral) -> Self {
        Self::TextLiteral(Box::new(text))
    }
}

impl From<Path> for BareExpression {
    fn from(path: Path) -> Self {
        Self::Path(Box::new(path))
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

impl From<Projection> for BareExpression {
    fn from(projection: Projection) -> Self {
        Self::Projection(Box::new(projection))
    }
}

impl From<QuantifiedType> for BareExpression {
    fn from(type_: QuantifiedType) -> Self {
        Self::QuantifiedType(Box::new(type_))
    }
}

impl From<LambdaLiteral> for BareExpression {
    fn from(lambda: LambdaLiteral) -> Self {
        Self::LambdaLiteral(Box::new(lambda))
    }
}

impl From<CaseAnalysis> for BareExpression {
    fn from(analysis: CaseAnalysis) -> Self {
        Self::CaseAnalysis(Box::new(analysis))
    }
}

impl From<LetBinding> for BareExpression {
    fn from(binding: LetBinding) -> Self {
        Self::LetBinding(Box::new(binding))
    }
}

impl From<UseBinding> for BareExpression {
    fn from(binding: UseBinding) -> Self {
        Self::UseBinding(Box::new(binding))
    }
}

impl From<DoBlock> for BareExpression {
    fn from(do_: DoBlock) -> Self {
        Self::DoBlock(Box::new(do_))
    }
}

impl PossiblyErroneous for BareExpression {
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
pub struct Projection {
    pub basis: Expression,
    pub field: Identifier,
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
pub struct QuantifiedType {
    pub quantifier: Quantifier,
    pub parameters: Parameters,
    pub codomain: Expression,
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
pub struct LambdaLiteral {
    pub parameters: Parameters,
    pub codomain: Option<Expression>,
    pub body: Expression,
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
    pub scrutinee: Expression,
    pub cases: Vec<Case>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Case {
    pub pattern: Pattern,
    pub body: Expression,
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
    pub parameters: Parameters,
    pub type_: Option<Expression>,
    pub body: Option<Expression>,
    pub scope: Expression,
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
    pub scope: Expression,
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
    Expression(Expression),
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
    pub parameters: Parameters,
    pub type_: Option<Expression>,
    pub body: Option<(BindingMode, Expression)>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BindingMode {
    Plain,
    Effectful,
}
