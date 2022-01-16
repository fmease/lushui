//! The abstract syntax tree (AST).
//!
//! The most important definitions are [`Declaration`], [`Expression`] and [`Pattern`].

mod format;

use super::{
    lexer::is_punctuation,
    token::{Token, TokenKind},
};
use crate::{
    diagnostics::{Code, Diagnostic},
    error::PossiblyErroneous,
    span::{PossiblySpanning, SourceFileIndex, Span, Spanned, Spanning},
    utility::{obtain, Atom, SmallVec},
};
pub use format::Format;
use smallvec::smallvec;
use std::{fmt, hash::Hash};

pub(crate) type Item<Kind> = crate::item::Item<Kind, Attributes>;

pub type Declaration = Item<DeclarationKind>;

/// The syntax node of a declaration.
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum DeclarationKind {
    Function(Box<Function>),
    Data(Box<Data>),
    Constructor(Box<Constructor>),
    Module(Box<Module>),
    ModuleHeader,
    Group(Box<Group>),
    Use(Box<Use>),
}

impl TryFrom<DeclarationKind> for Module {
    type Error = ();

    fn try_from(declaration: DeclarationKind) -> Result<Self, Self::Error> {
        obtain!(declaration, DeclarationKind::Module(module) => *module).ok_or(())
    }
}

/// The syntax node of a value declaration or a let statement.
///
/// See [`DeclarationKind::Function`] and [`Statement::Let`].
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Function {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_annotation: Option<Expression>,
    pub body: Option<Expression>,
}

impl From<Function> for DeclarationKind {
    fn from(function: Function) -> Self {
        Self::Function(Box::new(function))
    }
}

/// The syntax node of a data declaration.
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Data {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_annotation: Option<Expression>,
    pub constructors: Option<Vec<Declaration>>,
}

impl From<Data> for DeclarationKind {
    fn from(type_: Data) -> Self {
        Self::Data(Box::new(type_))
    }
}

/// The syntax node of a constructor.
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Constructor {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_annotation: Option<Expression>,
    /// Explicit bodies are only syntactically legal for constructors,
    /// not semantically.
    pub body: Option<Expression>,
}

impl From<Constructor> for DeclarationKind {
    fn from(constructor: Constructor) -> Self {
        Self::Constructor(Box::new(constructor))
    }
}

/// The syntax node of a module declaration.
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Module {
    pub binder: Identifier,
    pub file: SourceFileIndex,
    pub declarations: Option<Vec<Declaration>>,
}

impl From<Module> for DeclarationKind {
    fn from(module: Module) -> Self {
        Self::Module(Box::new(module))
    }
}

/// The syntax node of attribute groups.
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Group {
    pub declarations: Vec<Declaration>,
}

/// The syntax node of a use-declaration or statement.
///
/// See [`DeclarationKind::Use`] and [`Statement::Use`].
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Use {
    pub bindings: UsePathTree,
}

impl From<Use> for DeclarationKind {
    fn from(use_: Use) -> Self {
        Self::Use(Box::new(use_))
    }
}

pub type UsePathTree = Spanned<UsePathTreeKind>;

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum UsePathTreeKind {
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
pub type Attribute = Spanned<AttributeKind>;

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum AttributeKind {
    Regular {
        binder: Identifier,
        arguments: SmallVec<AttributeArgument, 1>,
    },
    Documentation,
}

// @Beacon @Task smh incorporate HashMap/BTreeMap<WeaklySpanned<Identifier>, Argument>
// to enable out of order named parameters
pub type AttributeArgument = Spanned<AttributeArgumentKind>;

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum AttributeArgumentKind {
    NumberLiteral(Atom),
    TextLiteral(Atom),
    Path(Box<Path>),
    Named(Box<NamedAttributeArgument>),
}

impl AttributeArgumentKind {
    pub(crate) const fn name(&self) -> &'static str {
        match self {
            Self::NumberLiteral(_) => "number literal",
            Self::TextLiteral(_) => "text literal",
            Self::Path(_) => "path",
            Self::Named(_) => "named argument",
        }
    }
}

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct NamedAttributeArgument {
    pub binder: Identifier,
    pub value: AttributeArgument,
}

pub type Expression = Item<ExpressionKind>;

/// The syntax node of an expression.
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum ExpressionKind {
    PiTypeLiteral(Box<PiTypeLiteral>),
    Application(Box<Application<Expression>>),
    Field(Box<Field>),
    Path(Box<Path>),
    TypeLiteral,
    NumberLiteral(Box<NumberLiteral>),
    TextLiteral(Box<TextLiteral>),
    TypedHole(Box<TypedHole>),
    LetIn(Box<LetIn>),
    UseIn(Box<UseIn>),
    LambdaLiteral(Box<LambdaLiteral>),
    CaseAnalysis(Box<CaseAnalysis>),
    DoBlock(Box<DoBlock>),
    SequenceLiteral(Box<SequenceLiteral<Expression>>),
    Error,
}

impl PossiblyErroneous for ExpressionKind {
    fn error() -> Self {
        Self::Error
    }
}

/// The syntax node of a pi type literal.
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct PiTypeLiteral {
    pub domain: Domain,
    pub codomain: Expression,
}

impl From<PiTypeLiteral> for ExpressionKind {
    fn from(pi: PiTypeLiteral) -> Self {
        Self::PiTypeLiteral(Box::new(pi))
    }
}

/// The domain of a [pi type literal](PiTypeLiteral).
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Domain {
    pub explicitness: Explicitness,
    pub laziness: Option<Span>,
    pub binder: Option<Identifier>,
    pub expression: Expression,
}

impl From<Application<Expression>> for ExpressionKind {
    fn from(application: Application<Expression>) -> Self {
        Self::Application(Box::new(application))
    }
}

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Field {
    pub base: Expression,
    pub member: Identifier,
}

impl From<Field> for ExpressionKind {
    fn from(field: Field) -> Self {
        Self::Field(Box::new(field))
    }
}

impl From<Path> for ExpressionKind {
    fn from(path: Path) -> Self {
        Self::Path(Box::new(path))
    }
}

impl From<NumberLiteral> for ExpressionKind {
    fn from(number: NumberLiteral) -> Self {
        Self::NumberLiteral(Box::new(number))
    }
}

impl From<TextLiteral> for ExpressionKind {
    fn from(text: TextLiteral) -> Self {
        Self::TextLiteral(Box::new(text))
    }
}

/// The syntax node of typed holes.
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct TypedHole {
    pub tag: Identifier,
}

impl From<TypedHole> for ExpressionKind {
    fn from(hole: TypedHole) -> Self {
        Self::TypedHole(Box::new(hole))
    }
}

/// The syntax-node of a let/in expression.
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct LetIn {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_annotation: Option<Expression>,
    pub expression: Option<Expression>,
    pub scope: Expression,
}

impl From<LetIn> for ExpressionKind {
    fn from(let_in: LetIn) -> Self {
        Self::LetIn(Box::new(let_in))
    }
}

/// The syntax node of a use/in expression.
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct UseIn {
    pub bindings: UsePathTree,
    pub scope: Expression,
}

impl From<UseIn> for ExpressionKind {
    fn from(use_in: UseIn) -> Self {
        Self::UseIn(Box::new(use_in))
    }
}

/// The syntax node of a lambda literal expression.
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct LambdaLiteral {
    pub parameters: Parameters,
    pub body_type_annotation: Option<Expression>,
    pub body: Expression,
}

impl From<LambdaLiteral> for ExpressionKind {
    fn from(lambda: LambdaLiteral) -> Self {
        Self::LambdaLiteral(Box::new(lambda))
    }
}

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
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
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Case {
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct DoBlock {
    pub statements: Vec<Statement>,
}

impl From<DoBlock> for ExpressionKind {
    fn from(do_: DoBlock) -> Self {
        Self::DoBlock(Box::new(do_))
    }
}

// @Note we probably gonna need to make this an item at some time for diagnostics
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum Statement {
    // @Note we could make the definition syntactically optional and provide a good error message
    // (missing definition) when lowering
    Let(LetStatement),
    Use(Use),
    // @Question should we rename this to Assign since we plan on not only lowering
    // to monads but also applicatives?
    Bind(BindStatement),
    Expression(Expression),
}

// @Note has a lot of overlap with [StatementKind::Let] (and a bit with [ExpressionKind::LetIn])
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct LetStatement {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_annotation: Option<Expression>,
    pub expression: Expression,
}

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct BindStatement {
    pub binder: Identifier,
    pub type_annotation: Option<Expression>,
    pub expression: Expression,
}

impl From<SequenceLiteral<Expression>> for ExpressionKind {
    fn from(sequence: SequenceLiteral<Expression>) -> Self {
        Self::SequenceLiteral(Box::new(sequence))
    }
}

pub type Parameters = Vec<Parameter>;
pub type Parameter = Spanned<ParameterKind>;

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct ParameterKind {
    pub explicitness: Explicitness,
    pub laziness: Option<Span>,
    pub binder: Identifier,
    pub type_annotation: Option<Expression>,
}

pub type Pattern = Item<PatternKind>;

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum PatternKind {
    NumberLiteral(Box<NumberLiteral>),
    TextLiteral(Box<TextLiteral>),
    SequenceLiteral(Box<SequenceLiteral<Pattern>>),
    Path(Box<Path>),
    Binder(Box<Identifier>),
    Application(Box<Application<Pattern>>),
}

impl From<NumberLiteral> for PatternKind {
    fn from(number: NumberLiteral) -> Self {
        Self::NumberLiteral(Box::new(number))
    }
}

impl From<TextLiteral> for PatternKind {
    fn from(text: TextLiteral) -> Self {
        Self::TextLiteral(Box::new(text))
    }
}

impl From<SequenceLiteral<Pattern>> for PatternKind {
    fn from(sequence: SequenceLiteral<Pattern>) -> Self {
        Self::SequenceLiteral(Box::new(sequence))
    }
}

impl From<Path> for PatternKind {
    fn from(path: Path) -> Self {
        Self::Path(Box::new(path))
    }
}

impl From<Identifier> for PatternKind {
    fn from(binder: Identifier) -> Self {
        Self::Binder(Box::new(binder))
    }
}

impl From<Application<Pattern>> for PatternKind {
    fn from(application: Application<Pattern>) -> Self {
        Self::Application(Box::new(application))
    }
}

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct NumberLiteral {
    pub path: Option<Path>,
    pub literal: Atom,
}

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Application<T> {
    pub callee: T,
    pub explicitness: Explicitness,
    pub binder: Option<Identifier>,
    pub argument: T,
}

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct SequenceLiteral<T> {
    pub path: Option<Path>,
    pub elements: Vec<T>,
}

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct TextLiteral {
    pub path: Option<Path>,
    pub literal: Atom,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub(crate) hanger: Option<Hanger>,
    // @Task better name: identifier_segments
    // @Note non-empty if hanger is None
    pub(crate) segments: SmallVec<Identifier, 1>,
}

impl Path {
    pub(crate) fn with_segments(segments: SmallVec<Identifier, 1>) -> Self {
        Self {
            hanger: None,
            segments,
        }
    }

    /// Construct a single identifier segment path.
    ///
    /// May panic.
    // @Note bad naming try_from_token (only for ids) <-> hanger only for hangers
    // unify?
    pub(crate) fn try_from_token(token: Token) -> Option<Self> {
        Some(Identifier::try_from(token).ok()?.into())
    }

    /// Construct a non-identifier-head-only path.
    pub(crate) fn hanger(token: Token) -> Self {
        Self {
            hanger: Some(Hanger::new(token.span, token.value.try_into().unwrap())),
            segments: SmallVec::new(),
        }
    }

    // @Task make this Option<Self> and move diagnostic construction into lowerer
    pub(crate) fn join(mut self, other: Self) -> Result<Self, Diagnostic> {
        if let Some(hanger) = other.hanger {
            if !matches!(hanger.value, HangerKind::Self_) {
                return Err(Diagnostic::error()
                    .code(Code::E026)
                    .message(format!("path `{}` not allowed in this position", hanger))
                    .primary_span(&hanger)
                    .help("consider moving this path to a separate use-declaration"));
            }
        }
        self.segments.extend(other.segments);
        Ok(self)
    }

    pub(crate) fn bare_hanger(&self, hanger: HangerKind) -> Option<Hanger> {
        self.hanger
            .filter(|some_hanger| some_hanger.value == hanger && self.segments.is_empty())
    }

    /// Return the path head if it is an identifier.
    pub(crate) fn identifier_head(&self) -> Option<&Identifier> {
        if self.hanger.is_some() {
            return None;
        }

        Some(&self.segments[0])
    }

    pub(crate) fn last_identifier(&self) -> Option<&Identifier> {
        self.segments.last()
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

impl Spanning for Path {
    fn span(&self) -> Span {
        if let Some(head) = &self.hanger {
            head.span()
                .merge(self.segments.first())
                .merge(self.segments.last())
        } else {
            self.segments
                .first()
                .unwrap()
                .span()
                .merge(self.segments.last().unwrap())
        }
    }
}

pub(crate) type Hanger = Spanned<HangerKind>;

/// The non-identifier head of a path.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum HangerKind {
    Extern,
    Capsule,
    Super,
    Self_,
}

impl HangerKind {
    pub(crate) const fn name(self) -> &'static str {
        match self {
            Self::Extern => "extern",
            Self::Capsule => "capsule",
            Self::Super => "super",
            Self::Self_ => "self",
        }
    }
}

impl TryFrom<TokenKind> for HangerKind {
    type Error = ();

    fn try_from(kind: TokenKind) -> Result<Self, Self::Error> {
        Ok(match kind {
            TokenKind::Extern => Self::Extern,
            TokenKind::Capsule => Self::Capsule,
            TokenKind::Super => Self::Super,
            TokenKind::Self_ => Self::Self_,
            _ => return Err(()),
        })
    }
}

/// Either a word or punctuation.
#[derive(Clone, Debug)]
pub struct Identifier(Spanned<Atom>);

impl Identifier {
    // @Beacon @Task docs why unchecked
    pub(crate) const fn new_unchecked(atom: Atom, span: Span) -> Self {
        Self(Spanned::new(span, atom))
    }

    pub(crate) fn as_atom(&self) -> &Atom {
        &self.0.value
    }

    pub(crate) fn into_atom(self) -> Atom {
        self.0.value
    }

    pub(crate) fn as_str(&self) -> &str {
        self.as_atom()
    }

    pub(crate) fn as_spanned_str(&self) -> Spanned<&str> {
        self.0.as_ref().map(|atom| &**atom)
    }

    pub(crate) fn is_punctuation(&self) -> bool {
        // either all characters are punctuation or none
        is_punctuation(self.as_atom().chars().next().unwrap())
    }

    pub(crate) fn is_word(&self) -> bool {
        // either all characters are letters or none
        !self.is_punctuation()
    }
}

impl TryFrom<Token> for Identifier {
    type Error = ();

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        Ok(Self(Spanned::new(
            token.span,
            token.into_identifier().ok_or(())?,
        )))
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

impl Spanning for Identifier {
    fn span(&self) -> Span {
        self.0.span
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.as_atom() == other.as_atom()
    }
}

impl Eq for Identifier {}

impl Hash for Identifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_atom().hash(state);
    }
}

pub(crate) use Explicitness::*;

/// The explicitness of a parameter or argument.
///
/// In the context of parameters, this specifies whether in an application, the corresponding argument has
/// to be passed explicitly or should be infered, i.e. the parameter is [Implicit].
///
/// In the context of applications, [Implicit] means that the argument is passed explicitly
/// even though the parameter is marked implicit.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum Explicitness {
    Implicit,
    #[default]
    Explicit,
}

#[derive(Clone, Copy)]
pub(super) enum SpannedExplicitness {
    Implicit { marker: Span },
    Explicit,
}

impl From<SpannedExplicitness> for Explicitness {
    fn from(explicitness: SpannedExplicitness) -> Self {
        match explicitness {
            SpannedExplicitness::Implicit { .. } => Implicit,
            SpannedExplicitness::Explicit => Explicit,
        }
    }
}

impl PossiblySpanning for SpannedExplicitness {
    fn possible_span(&self) -> Option<Span> {
        match self {
            &Self::Implicit { marker } => Some(marker),
            Self::Explicit => None,
        }
    }
}
