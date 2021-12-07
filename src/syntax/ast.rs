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
use smallvec::smallvec;
use std::{default::default, hash::Hash};

pub use format::Format;

pub type Item<Kind> = crate::item::Item<Kind, Attributes>;

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

/// The syntax node of a data declaration.
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Data {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_annotation: Option<Expression>,
    pub constructors: Option<Vec<Declaration>>,
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

/// The syntax node of a module declaration.
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Module {
    pub binder: Identifier,
    pub file: SourceFileIndex,
    pub declarations: Option<Vec<Declaration>>,
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

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Attribute {
    pub binder: Identifier,
    pub arguments: SmallVec<AttributeArgument, 1>,
    pub span: Span,
}

impl Spanning for Attribute {
    fn span(&self) -> Span {
        self.span
    }
}

pub type AttributeArgument = Spanned<AttributeArgumentKind>;

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum AttributeArgumentKind {
    NumberLiteral(Box<String>),
    TextLiteral(Box<String>),
    /// To be able to lower documentation comments without immense memory wastage.
    TextEncodedInSpan,
    Path(Box<Path>),
    Named(Box<NamedAttributeArgument>),
}

impl AttributeArgumentKind {
    pub const fn name(&self) -> &'static str {
        match self {
            Self::NumberLiteral(_) => "number literal",
            Self::TextLiteral(_) | Self::TextEncodedInSpan => "text literal",
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
    Application(Box<Application>),
    TypeLiteral,
    NumberLiteral(Box<String>),
    TextLiteral(Box<String>),
    TypedHole(Box<TypedHole>),
    Path(Box<Path>),
    Field(Box<Field>),
    LambdaLiteral(Box<LambdaLiteral>),
    LetIn(Box<LetIn>),
    UseIn(Box<UseIn>),
    CaseAnalysis(Box<CaseAnalysis>),
    DoBlock(Box<DoBlock>),
    SequenceLiteral(Box<SequenceLiteral>),
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

/// The domain of a [pi type literal](PiTypeLiteral).
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Domain {
    pub explicitness: Explicitness,
    pub aspect: ParameterAspect,
    pub binder: Option<Identifier>,
    pub expression: Expression,
}

/// The extra qualities a parameter of a pi type can posses.
// @Task maybe change types to Spanned<Laziness>, Spanned<Fieldness>
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct ParameterAspect {
    pub laziness: Option<Span>,
    pub fieldness: Option<Span>,
}

impl ParameterAspect {
    pub const fn is_lazy(self) -> bool {
        self.laziness.is_some()
    }

    pub const fn is_field(self) -> bool {
        self.fieldness.is_some()
    }
}

/// The syntax node of function application.
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Application {
    pub callee: Expression,
    pub explicitness: Explicitness,
    pub binder: Option<Identifier>,
    pub argument: Expression,
}

/// The syntax node of a typed hole.
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct TypedHole {
    pub tag: Identifier,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub hanger: Option<Hanger>,
    // @Task better name: identifier_segments
    // @Note non-empty if hanger is None
    pub segments: SmallVec<Identifier, 1>,
}

impl Path {
    pub fn with_segments(segments: SmallVec<Identifier, 1>) -> Self {
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
    pub fn try_from_token(token: Token) -> Option<Self> {
        Some(Identifier::try_from(token).ok()?.into())
    }

    /// Construct a non-identifier-head-only path.
    pub fn hanger(token: Token) -> Self {
        Self {
            hanger: Some(Hanger::new(token.span, token.value.try_into().unwrap())),
            segments: SmallVec::new(),
        }
    }

    // @Task make this Option<Self> and move diagnostic construction into lowerer
    pub fn join(mut self, other: Self) -> Result<Self, Diagnostic> {
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

    pub fn bare_hanger(&self, hanger: HangerKind) -> Option<Hanger> {
        self.hanger
            .filter(|some_hanger| some_hanger.value == hanger && self.segments.is_empty())
    }

    /// Return the path head if it is an identifier.
    pub fn identifier_head(&self) -> Option<&Identifier> {
        if self.hanger.is_some() {
            return None;
        }

        Some(&self.segments[0])
    }

    pub fn last_identifier(&self) -> Option<&Identifier> {
        self.segments.last()
    }

    /// Try to debase a path to a single identifier.
    ///
    /// A path is _simple_ iff it has a single segment being the head which is not a hanger.
    pub fn to_single_identifier(&self) -> Option<&Identifier> {
        if !self.is_single_identifier() {
            return None;
        }

        Some(&self.segments[0])
    }

    pub fn is_single_identifier(&self) -> bool {
        self.hanger.is_none() && self.segments.len() == 1
    }

    pub fn unhanged(self) -> Self {
        Self {
            hanger: None,
            segments: self.segments,
        }
    }

    // @Task replace thing function with something that does not allocate
    // @Note we'd like to have a PathView which uses slices of segments
    pub fn tail(&self) -> Option<Self> {
        let segments = if self.hanger.is_some() {
            self.segments.clone()
        } else {
            let segments: SmallVec<_, 1> = self.segments.iter().skip(1).cloned().collect();
            match segments.is_empty() {
                true => return None,
                false => segments,
            }
        };

        Some(Self {
            hanger: None,
            segments,
        })
    }

    // @Task avoid allocation, try to design it as a slice `&self.segments[..LEN - 1]`
    /// Path consisting of segments 0 to n-1
    // @Task verify
    pub fn prefix(&self) -> Self {
        Self {
            hanger: self.hanger,
            segments: self.segments.iter().rev().skip(1).rev().cloned().collect(),
        }
    }
}

use std::fmt;

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

/// The syntax node of a path expression.
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Field {
    pub base: Expression,
    pub member: Identifier,
}

/// The syntax node of a lambda literal expression.
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct LambdaLiteral {
    pub parameters: Parameters,
    pub body_type_annotation: Option<Expression>,
    pub body: Expression,
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

/// The syntax node of a use/in expression.
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct UseIn {
    pub bindings: UsePathTree,
    pub scope: Expression,
}

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct CaseAnalysis {
    pub scrutinee: Expression,
    pub cases: Vec<Case>,
}

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct DoBlock {
    pub statements: Vec<Statement>,
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

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct SequenceLiteral {
    pub elements: Vec<Expression>,
}

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Case {
    pub pattern: Pattern,
    pub body: Expression,
}

pub type Parameters = Vec<ParameterGroup>;

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct ParameterGroup {
    pub explicitness: Explicitness,
    pub aspect: ParameterAspect,
    /// non-empty
    pub parameters: SmallVec<Identifier, 1>,
    pub type_annotation: Option<Expression>,
    pub span: Span,
}

impl Spanning for ParameterGroup {
    fn span(&self) -> Span {
        self.span
    }
}

pub type Pattern = Item<PatternKind>;

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum PatternKind {
    NumberLiteral(Box<String>),
    TextLiteral(Box<String>),
    // @Note unfortunate naming @Update @Question can't we at least rename the variant?
    SequenceLiteralPattern(Box<SequenceLiteralPattern>),
    Path(Box<Path>),
    Binder(Box<Binder>),
    Deapplication(Box<Deapplication>),
}

/// A binder inside of a pattern.
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Binder {
    pub binder: Identifier,
}

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Deapplication {
    pub callee: Pattern,
    pub explicitness: Explicitness,
    pub binder: Option<Identifier>,
    pub argument: Pattern,
}

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct SequenceLiteralPattern {
    pub elements: Vec<Pattern>,
}

pub type Hanger = Spanned<HangerKind>;

/// The non-identifier head of a path.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum HangerKind {
    Extern,
    Crate,
    Super,
    Self_,
}

impl HangerKind {
    pub const fn name(self) -> &'static str {
        match self {
            Self::Extern => "extern",
            Self::Crate => "crate",
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
            TokenKind::Crate => Self::Crate,
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
    pub const fn new_unchecked(atom: Atom, span: Span) -> Self {
        Self(Spanned::new(span, atom))
    }

    pub fn as_atom(&self) -> &Atom {
        &self.0.value
    }

    pub fn into_atom(self) -> Atom {
        self.0.value
    }

    pub fn as_str(&self) -> &str {
        self.as_atom()
    }

    pub fn as_spanned_str(&self) -> Spanned<&str> {
        self.0.as_ref().map(|atom| &**atom)
    }

    pub fn stripped(self) -> Self {
        Self::new_unchecked(self.0.value, default())
    }

    pub fn is_punctuation(&self) -> bool {
        // either all characters are punctuation or none
        is_punctuation(self.as_atom().chars().next().unwrap())
    }

    pub fn is_word(&self) -> bool {
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
        expr! {
            Path(Attributes::new(), identifier.span(); Path::from(identifier))
        }
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

pub use Explicitness::*;

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

pub macro decl($( $tree:tt )+) {
    crate::item::item!(crate::syntax::ast, DeclarationKind, Box; $( $tree )+)
}

pub macro expr($( $tree:tt )+) {
    crate::item::item!(crate::syntax::ast, ExpressionKind, Box; $( $tree )+)
}

pub macro pat($( $tree:tt )+) {
    crate::item::item!(crate::syntax::ast, PatternKind, Box; $( $tree )+)
}

pub macro attrarg($kind:ident($span:expr; $value:expr $(,)?)) {
    crate::syntax::ast::AttributeArgument::new(
        $span,
        crate::syntax::ast::AttributeArgumentKind::$kind(Box::new($value)),
    )
}
