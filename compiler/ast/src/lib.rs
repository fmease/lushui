//! The abstract syntax tree (AST).
//!
//! The most important definitions are [`Declaration`], [`Expression`] and [`Pattern`].
#![feature(default_free_fn, let_chains)]

use diagnostics::{error::PossiblyErroneous, reporter::ErasedReportedError};
pub use format::{Debug, Format};
use lexer::{token::BareToken, word::Word, CharExt};
use span::{SourceFileIndex, Span, Spanned, Spanning};
use std::{fmt, hash::Hash};
use utilities::{obtain, smallvec, Atom, SmallVec};

mod format;

pub type Item<Bare> = span::item::Item<Bare, Attributes>;

/// A declaration.
///
/// The syntactic category of module-level definitions like functions, data types and modules.
pub type Declaration = Item<BareDeclaration>;

/// A declaration without an enclosing [`Span`].
#[derive(PartialEq, Eq)]
pub enum BareDeclaration {
    Function(Box<Function>),
    Data(Box<Data>),
    Constructor(Box<Constructor>),
    Module(Box<Module>),
    ModuleHeader,
    Group(Box<Group>),
    Use(Box<Use>),
}

impl TryFrom<BareDeclaration> for Module {
    type Error = ();

    fn try_from(declaration: BareDeclaration) -> Result<Self, Self::Error> {
        obtain!(declaration, BareDeclaration::Module(module) => *module).ok_or(())
    }
}

/// A function declaration.
///
/// # Examples
///
/// ```lushui
/// identity 'A (a: A): A = a
/// ```
///
/// * `identity` is the *binder*
/// * `'A` and `(a: A)` are the *parameters*
/// * `A` following the colon is the *type*
/// * `a` at the end is the *body*
#[derive(Clone, PartialEq, Eq)]
pub struct Function {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_: Option<Expression>,
    pub body: Option<Expression>,
}

impl From<Function> for BareDeclaration {
    fn from(function: Function) -> Self {
        Self::Function(Box::new(function))
    }
}

/// A data type declaration.
///
/// # Examples
///
/// ```lushui
/// data Result (A: Type) (E: Type): Type of
///     failure 'A 'E: E -> Result A E
///     success 'A 'E: A -> Result A E
/// ```
///
/// * `Result` is the *binder*
/// * `(A: Type)` and `(E: Type)` are the *parameters*
/// * `Type` at the end of the first line is the *type*
/// * the last two lines contain the *constructors*
#[derive(PartialEq, Eq)]
pub struct Data {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_: Option<Expression>,
    pub constructors: Option<Vec<Declaration>>,
}

impl From<Data> for BareDeclaration {
    fn from(type_: Data) -> Self {
        Self::Data(Box::new(type_))
    }
}

/// A (term) constructor declaration.
///
/// # Examples
///
/// ```lushui
/// data U: Type of
///     type (A: Type): U = ?type
/// ```
///
/// * the first line is not part of the constructor
/// * `type` is the *binder*
/// * `(A: Type)` is the *parameter*
/// * `U` is the *type*
/// * `?type` is the *body*
#[derive(PartialEq, Eq)]
pub struct Constructor {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_: Option<Expression>,
    /// The body of a constructor.
    ///
    /// Explicitly provided constructor bodies are only *syntactically* valid,
    /// not semantically. They are rejected in the lowerer.
    pub body: Option<Expression>,
}

impl From<Constructor> for BareDeclaration {
    fn from(constructor: Constructor) -> Self {
        Self::Constructor(Box::new(constructor))
    }
}

/// A module declaration.
///
/// # Examples
///
/// `file.lushui`:
///
/// ```lushui
/// module inner of
///     use extern.core.type.Type
/// ```
///
/// * `inner` is the *binder*
/// * the index of `file.lushui` is the *file* (index)
/// * the last line contains the only *declaration* (or subdeclaration)
#[derive(PartialEq, Eq)]
pub struct Module {
    pub binder: Identifier,
    pub file: SourceFileIndex,
    pub declarations: Option<Vec<Declaration>>,
}

impl From<Module> for BareDeclaration {
    fn from(module: Module) -> Self {
        Self::Module(Box::new(module))
    }
}

/// An attribute group.
///
/// > **NOTE** Does not have a corresponding syntactic form in the surface language yet if ever.
#[derive(PartialEq, Eq)]
pub struct Group {
    pub declarations: Vec<Declaration>,
}

/// A use-declaration or a use-statement.
///
/// See [`BareDeclaration::Use`] and [`Statement::Use`].
#[derive(Clone, PartialEq, Eq)]
pub struct Use {
    pub bindings: UsePathTree,
}

impl From<Use> for BareDeclaration {
    fn from(use_: Use) -> Self {
        Self::Use(Box::new(use_))
    }
}

pub type UsePathTree = Spanned<BareUsePathTree>;

#[derive(Clone, PartialEq, Eq)]
pub enum BareUsePathTree {
    Single {
        target: Path,
        binder: Option<Identifier>,
    },
    Multiple {
        path: Path,
        bindings: Vec<UsePathTree>,
    },
}

pub type Attributes = Vec<Attribute>;
pub type Attribute = Spanned<BareAttribute>;

#[derive(Clone, PartialEq, Eq)]
pub enum BareAttribute {
    Regular {
        binder: Identifier,
        arguments: SmallVec<AttributeArgument, 1>,
    },
    Documentation,
}

pub type AttributeArgument = Spanned<BareAttributeArgument>;

#[derive(Clone, PartialEq, Eq)]
pub enum BareAttributeArgument {
    NumberLiteral(Atom),
    TextLiteral(Atom),
    Path(Box<Path>),
    Named(Box<NamedAttributeArgument>),
}

impl BareAttributeArgument {
    pub const fn name(&self) -> &'static str {
        match self {
            Self::NumberLiteral(_) => "number literal",
            Self::TextLiteral(_) => "text literal",
            Self::Path(_) => "path",
            Self::Named(_) => "named argument",
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct NamedAttributeArgument {
    pub binder: Identifier,
    pub value: AttributeArgument,
}

/// An expression.
pub type Expression = Item<BareExpression>;

/// An expression without an enclosing [`Span`].
#[derive(Clone, PartialEq, Eq)]
pub enum BareExpression {
    QuantifiedType(Box<QuantifiedType>),
    Application(Box<Application<Expression>>),
    Projection(Box<Projection>),
    Path(Box<Path>),
    NumberLiteral(Box<NumberLiteral>),
    TextLiteral(Box<TextLiteral>),
    TypedHole(Box<TypedHole>),
    LetBinding(Box<LetBinding>),
    UseBinding(Box<UseBinding>),
    LambdaLiteral(Box<LambdaLiteral>),
    CaseAnalysis(Box<CaseAnalysis>),
    DoBlock(Box<DoBlock>),
    SequenceLiteral(Box<SequenceLiteral<Expression>>),
    Error(ErasedReportedError),
}

impl PossiblyErroneous for BareExpression {
    fn error(error: ErasedReportedError) -> Self {
        Self::Error(error)
    }
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
#[derive(Clone, PartialEq, Eq)]
pub struct QuantifiedType {
    pub quantifier: Quantifier,
    pub parameters: Parameters,
    pub codomain: Expression,
}

impl From<QuantifiedType> for BareExpression {
    fn from(type_: QuantifiedType) -> Self {
        Self::QuantifiedType(Box::new(type_))
    }
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

impl From<Application<Expression>> for BareExpression {
    fn from(application: Application<Expression>) -> Self {
        Self::Application(Box::new(application))
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

impl From<Projection> for BareExpression {
    fn from(projection: Projection) -> Self {
        Self::Projection(Box::new(projection))
    }
}

impl From<Path> for BareExpression {
    fn from(path: Path) -> Self {
        Self::Path(Box::new(path))
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

/// A (typed) hole.
///
/// # Examples
///
/// ```lushui
/// main = ?impl
/// ```
///
/// * `?impl` is the hole
/// * `impl` is the *tag*
#[derive(Clone, PartialEq, Eq)]
pub struct TypedHole {
    /// The tag of the hole.
    ///
    /// It's *tag* not *binder* or *name* to emphasize that it's not unique inside of a program.
    pub tag: Identifier,
}

impl From<TypedHole> for BareExpression {
    fn from(hole: TypedHole) -> Self {
        Self::TypedHole(Box::new(hole))
    }
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
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_: Option<Expression>,
    pub body: Option<Expression>,
    pub scope: Expression,
}

impl From<LetBinding> for BareExpression {
    fn from(binding: LetBinding) -> Self {
        Self::LetBinding(Box::new(binding))
    }
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

impl From<UseBinding> for BareExpression {
    fn from(binding: UseBinding) -> Self {
        Self::UseBinding(Box::new(binding))
    }
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

impl From<LambdaLiteral> for BareExpression {
    fn from(lambda: LambdaLiteral) -> Self {
        Self::LambdaLiteral(Box::new(lambda))
    }
}

// @Beacon @Task parse *`()`, *`(x,)`, `(x, y)`, `(x, y, z)`, … as tuple (sigma) literals
// (*) semantically ill-formed

// pub struct TupleLiteral;

// impl From<TupleLiteral> for BareExpression {
//     fn from(tuple: TupleLiteral) -> Self {
//         Self::TupleLiteral(Box::new(tuple))
//     }
// }

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

impl From<CaseAnalysis> for BareExpression {
    fn from(analysis: CaseAnalysis) -> Self {
        Self::CaseAnalysis(Box::new(analysis))
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Case {
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Clone, PartialEq, Eq)]
pub struct DoBlock {
    pub statements: Vec<Statement>,
}

impl From<DoBlock> for BareExpression {
    fn from(do_: DoBlock) -> Self {
        Self::DoBlock(Box::new(do_))
    }
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
    pub binder: Identifier,
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

impl From<SequenceLiteral<Expression>> for BareExpression {
    fn from(sequence: SequenceLiteral<Expression>) -> Self {
        Self::SequenceLiteral(Box::new(sequence))
    }
}

pub type Parameters = SmallVec<Parameter, 1>;
// @Beacon @Beacon @Beacon @Task make this an Item<_> for attribute support on params
pub type Parameter = Spanned<BareParameter>;

#[derive(Clone, PartialEq, Eq)]
pub struct BareParameter {
    pub explicitness: Explicitness,
    pub binder: Identifier,
    pub type_: Option<Expression>,
}

pub type Pattern = Item<BarePattern>;

#[derive(Clone, PartialEq, Eq)]
pub enum BarePattern {
    NumberLiteral(Box<NumberLiteral>),
    TextLiteral(Box<TextLiteral>),
    SequenceLiteral(Box<SequenceLiteral<Pattern>>),
    Path(Box<Path>),
    Binder(Box<Identifier>),
    Application(Box<Application<Pattern>>),
}

impl From<NumberLiteral> for BarePattern {
    fn from(number: NumberLiteral) -> Self {
        Self::NumberLiteral(Box::new(number))
    }
}

impl From<TextLiteral> for BarePattern {
    fn from(text: TextLiteral) -> Self {
        Self::TextLiteral(Box::new(text))
    }
}

impl From<SequenceLiteral<Pattern>> for BarePattern {
    fn from(sequence: SequenceLiteral<Pattern>) -> Self {
        Self::SequenceLiteral(Box::new(sequence))
    }
}

impl From<Path> for BarePattern {
    fn from(path: Path) -> Self {
        Self::Path(Box::new(path))
    }
}

impl From<Identifier> for BarePattern {
    fn from(binder: Identifier) -> Self {
        Self::Binder(Box::new(binder))
    }
}

impl From<Application<Pattern>> for BarePattern {
    fn from(application: Application<Pattern>) -> Self {
        Self::Application(Box::new(application))
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct NumberLiteral {
    pub path: Option<Path>,
    pub literal: Spanned<Atom>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Application<T> {
    pub callee: T,
    pub explicitness: Explicitness,
    pub binder: Option<Identifier>,
    pub argument: T,
}

#[derive(Clone, PartialEq, Eq)]
pub struct SequenceLiteral<T> {
    pub path: Option<Path>,
    pub elements: Spanned<Vec<T>>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct TextLiteral {
    pub path: Option<Path>,
    pub literal: Spanned<Atom>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub hanger: Option<Hanger>,
    /// Has to be non-empty if the `hanger` is `None`.
    // @Task make it impossible to construct with an empty vector
    pub segments: SmallVec<Identifier, 1>,
}

impl Path {
    // @Task make it impossible to construct with an empty vector
    pub fn with_segments(segments: SmallVec<Identifier, 1>) -> Self {
        Self {
            hanger: None,
            segments,
        }
    }

    pub fn join(mut self, other: Self) -> Result<Self, Hanger> {
        if let Some(hanger) = other.hanger && !matches!(hanger.bare, BareHanger::Self_) {
            return Err(hanger);
        }
        self.segments.extend(other.segments);
        Ok(self)
    }

    pub fn is_bare_hanger(&self, hanger: BareHanger) -> bool {
        self.hanger
            .map_or(false, |some_hanger| some_hanger.bare == hanger)
            && self.segments.is_empty()
    }

    /// The path head if it is an identifier.
    pub fn identifier_head(&self) -> Option<Identifier> {
        if self.hanger.is_some() {
            return None;
        }

        Some(self.segments[0])
    }
}

// @Question bad idea?
impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl From<Identifier> for Path {
    fn from(identifier: Identifier) -> Self {
        Self {
            hanger: None,
            segments: smallvec![identifier],
        }
    }
}

impl From<Hanger> for Path {
    fn from(hanger: Hanger) -> Self {
        Self {
            hanger: Some(hanger),
            segments: SmallVec::new(),
        }
    }
}

impl Spanning for Path {
    fn span(&self) -> Span {
        // @Task improve
        if let Some(head) = &self.hanger {
            head.span().merge(self.segments.last())
        } else {
            self.segments
                .first()
                .unwrap()
                .span()
                .merge(self.segments.last().unwrap())
        }
    }
}

pub type Hanger = Spanned<BareHanger>;

/// The non-identifier head of a path.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum BareHanger {
    Extern,
    Topmost,
    Super,
    Self_,
}

impl BareHanger {
    pub const fn name(self) -> &'static str {
        match self {
            Self::Extern => "extern",
            Self::Topmost => "topmost",
            Self::Super => "super",
            Self::Self_ => "self",
        }
    }
}

impl TryFrom<BareToken> for BareHanger {
    type Error = ();

    fn try_from(token: BareToken) -> Result<Self, Self::Error> {
        Ok(match token {
            BareToken::Extern => Self::Extern,
            BareToken::Topmost => Self::Topmost,
            BareToken::Super => Self::Super,
            BareToken::Self_ => Self::Self_,
            _ => return Err(()),
        })
    }
}

/// Either a [word](Word) or a symbol.
#[derive(Clone, Copy, Debug)]
pub struct Identifier(Spanned<Atom>);

impl Identifier {
    /// Create a new identifier without checking if it is a valid word or symbol.
    // @Task swap args
    pub const fn new_unchecked(atom: Atom, span: Span) -> Self {
        Self(Spanned::new(span, atom))
    }

    pub fn bare(self) -> Atom {
        self.0.bare
    }

    pub fn to_str(self) -> &'static str {
        self.0.bare.to_str()
    }

    pub fn into_inner(self) -> Spanned<Atom> {
        self.0
    }

    pub fn is_symbol(self) -> bool {
        // either all characters are symbols or none
        self.to_str().chars().next().unwrap().is_symbol()
    }

    pub fn is_word(self) -> bool {
        // either all characters are letters or none
        !self.is_symbol()
    }
}

impl From<Identifier> for Expression {
    fn from(identifier: Identifier) -> Self {
        Expression::new(
            Attributes::new(),
            identifier.span(),
            Path::from(identifier).into(),
        )
    }
}

impl From<Spanned<Word>> for Identifier {
    fn from(name: Spanned<Word>) -> Self {
        Self::new_unchecked(name.bare.into_inner(), name.span)
    }
}

impl TryFrom<Identifier> for Spanned<Word> {
    type Error = ();

    fn try_from(identifier: Identifier) -> Result<Self, Self::Error> {
        identifier
            .is_word()
            .then(|| Self::new(identifier.span(), Word::new_unchecked(identifier.bare())))
            .ok_or(())
    }
}

impl Spanning for Identifier {
    fn span(&self) -> Span {
        self.0.span
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.bare() == other.bare()
    }
}

impl Eq for Identifier {}

impl Hash for Identifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.bare().hash(state);
    }
}

pub use Explicitness::*;

/// The explicitness of a parameter or argument.
///
/// In the context of parameters, this specifies whether in an application, the corresponding argument has
/// to be passed explicitly or should be inferred, i.e. the parameter is [Implicit].
///
/// In the context of applications, [Implicit] means that the argument is passed explicitly
/// even though the parameter is marked implicit.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum Explicitness {
    Implicit,
    #[default]
    Explicit,
}

impl From<Option<Span>> for Explicitness {
    fn from(span: Option<Span>) -> Self {
        match span {
            Some(_) => Implicit,
            None => Explicit,
        }
    }
}
