//! The abstract syntax tree (AST).
//!
//! The most important definitions are [`Declaration`], [`Expression`] and [`Pattern`].

// use lushui_lexer::is_punctuation;
pub use format::Format;
use lushui_diagnostics::{Diagnostic, ErrorCode};
use lushui_error::PossiblyErroneous;
use lushui_span::{PossiblySpanning, SourceFileIndex, Span, Spanned, Spanning};
use lushui_token::{BareToken, Token, TokenExt, Word};
use lushui_utilities::{obtain, Atom, SmallVec};
use smallvec::smallvec;
use std::{fmt, hash::Hash};

mod format;

pub type Item<Bare> = lushui_item::Item<Bare, Attributes>;

pub type Declaration = Item<BareDeclaration>;

/// The syntax node of a declaration.
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

/// The syntax node of a value declaration or a let statement.
///
/// See [`BareDeclaration::Function`] and [`Statement::Let`].
#[derive(Clone, PartialEq, Eq)]
pub struct Function {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_annotation: Option<Expression>,
    pub body: Option<Expression>,
}

impl From<Function> for BareDeclaration {
    fn from(function: Function) -> Self {
        Self::Function(Box::new(function))
    }
}

/// The syntax node of a data declaration.
#[derive(PartialEq, Eq)]
pub struct Data {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_annotation: Option<Expression>,
    pub constructors: Option<Vec<Declaration>>,
}

impl From<Data> for BareDeclaration {
    fn from(type_: Data) -> Self {
        Self::Data(Box::new(type_))
    }
}

/// The syntax node of a constructor.
#[derive(PartialEq, Eq)]
pub struct Constructor {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_annotation: Option<Expression>,
    /// Explicit bodies are only syntactically legal for constructors,
    /// not semantically.
    pub body: Option<Expression>,
}

impl From<Constructor> for BareDeclaration {
    fn from(constructor: Constructor) -> Self {
        Self::Constructor(Box::new(constructor))
    }
}

/// The syntax node of a module declaration.
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

/// The syntax node of attribute groups.
#[derive(PartialEq, Eq)]
pub struct Group {
    pub declarations: Vec<Declaration>,
}

/// The syntax node of a use-declaration or statement.
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

// @Beacon @Task smh incorporate HashMap/BTreeMap<WeaklySpanned<Identifier>, Argument>
// to enable out of order named parameters
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

pub type Expression = Item<BareExpression>;

/// The syntax node of an expression.
#[derive(Clone, PartialEq, Eq)]
pub enum BareExpression {
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

impl PossiblyErroneous for BareExpression {
    fn error() -> Self {
        Self::Error
    }
}

/// The syntax node of a pi type literal.
#[derive(Clone, PartialEq, Eq)]
pub struct PiTypeLiteral {
    pub domain: Domain,
    pub codomain: Expression,
}

impl From<PiTypeLiteral> for BareExpression {
    fn from(pi: PiTypeLiteral) -> Self {
        Self::PiTypeLiteral(Box::new(pi))
    }
}

/// The domain of a [pi type literal](PiTypeLiteral).
#[derive(Clone, PartialEq, Eq)]
pub struct Domain {
    pub explicitness: Explicitness,
    pub laziness: Option<Span>,
    pub binder: Option<Identifier>,
    pub expression: Expression,
}

impl From<Application<Expression>> for BareExpression {
    fn from(application: Application<Expression>) -> Self {
        Self::Application(Box::new(application))
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Field {
    pub base: Expression,
    pub member: Identifier,
}

impl From<Field> for BareExpression {
    fn from(field: Field) -> Self {
        Self::Field(Box::new(field))
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

/// The syntax node of typed holes.
#[derive(Clone, PartialEq, Eq)]
pub struct TypedHole {
    pub tag: Identifier,
}

impl From<TypedHole> for BareExpression {
    fn from(hole: TypedHole) -> Self {
        Self::TypedHole(Box::new(hole))
    }
}

/// The syntax-node of a let/in expression.
#[derive(Clone, PartialEq, Eq)]
pub struct LetIn {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_annotation: Option<Expression>,
    pub expression: Option<Expression>,
    pub scope: Expression,
}

impl From<LetIn> for BareExpression {
    fn from(let_in: LetIn) -> Self {
        Self::LetIn(Box::new(let_in))
    }
}

/// The syntax node of a use/in expression.
#[derive(Clone, PartialEq, Eq)]
pub struct UseIn {
    pub bindings: UsePathTree,
    pub scope: Expression,
}

impl From<UseIn> for BareExpression {
    fn from(use_in: UseIn) -> Self {
        Self::UseIn(Box::new(use_in))
    }
}

/// The syntax node of a lambda literal expression.
#[derive(Clone, PartialEq, Eq)]
pub struct LambdaLiteral {
    pub parameters: Parameters,
    pub body_type_annotation: Option<Expression>,
    pub body: Expression,
}

impl From<LambdaLiteral> for BareExpression {
    fn from(lambda: LambdaLiteral) -> Self {
        Self::LambdaLiteral(Box::new(lambda))
    }
}

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

// @Note we probably gonna need to make this an item at some time for diagnostics
#[derive(Clone, PartialEq, Eq)]
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

// @Note has a lot of overlap with [BareStatement::Let] (and a bit with [BareExpression::LetIn])
#[derive(Clone, PartialEq, Eq)]
pub struct LetStatement {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_annotation: Option<Expression>,
    pub expression: Expression,
}

#[derive(Clone, PartialEq, Eq)]
pub struct BindStatement {
    pub binder: Identifier,
    pub type_annotation: Option<Expression>,
    pub expression: Expression,
}

impl From<SequenceLiteral<Expression>> for BareExpression {
    fn from(sequence: SequenceLiteral<Expression>) -> Self {
        Self::SequenceLiteral(Box::new(sequence))
    }
}

pub type Parameters = Vec<Parameter>;
pub type Parameter = Spanned<BareParameter>;

#[derive(Clone, PartialEq, Eq)]
pub struct BareParameter {
    pub explicitness: Explicitness,
    pub laziness: Option<Span>,
    pub binder: Identifier,
    pub type_annotation: Option<Expression>,
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

    // @Task make this Option<Self> and move diagnostic construction into lowerer
    pub fn join(mut self, other: Self) -> Result<Self, Diagnostic> {
        if let Some(hanger) = other.hanger {
            if !matches!(hanger.bare, BareHanger::Self_) {
                return Err(Diagnostic::error()
                    .code(ErrorCode::E026)
                    .message(format!("path ‘{}’ not allowed in this position", hanger))
                    .primary_span(&hanger)
                    .help("consider moving this path to a separate use-declaration"));
            }
        }
        self.segments.extend(other.segments);
        Ok(self)
    }

    pub fn bare_hanger(&self, hanger: BareHanger) -> Option<Hanger> {
        self.hanger
            .filter(|some_hanger| some_hanger.bare == hanger && self.segments.is_empty())
    }

    /// The path head if it is an identifier.
    pub fn identifier_head(&self) -> Option<&Identifier> {
        if self.hanger.is_some() {
            return None;
        }

        Some(&self.segments[0])
    }

    pub fn last_identifier(&self) -> Option<&Identifier> {
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

/// Either a word or punctuation.
#[derive(Clone, Debug)]
pub struct Identifier(Spanned<Atom>);

impl Identifier {
    // @Beacon @Task docs why unchecked
    pub const fn new_unchecked(atom: Atom, span: Span) -> Self {
        Self(Spanned::new(span, atom))
    }

    pub fn as_atom(&self) -> &Atom {
        &self.0.bare
    }

    pub fn into_atom(self) -> Atom {
        self.0.bare
    }

    pub fn as_str(&self) -> &str {
        self.as_atom()
    }

    pub fn as_spanned_str(&self) -> Spanned<&str> {
        self.0.as_ref().map(|atom| &**atom)
    }

    pub fn is_punctuation(&self) -> bool {
        // either all characters are punctuation or none
        // is_punctuation(self.as_atom().chars().next().unwrap())
        todo!() // @Beacon @Beacon @Beacon @Task
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
        Expression::new(
            Attributes::new(),
            identifier.span(),
            Path::from(identifier).into(),
        )
    }
}

impl From<Spanned<Word>> for Identifier {
    fn from(name: Spanned<Word>) -> Self {
        // Self::new_unchecked(name.bare.0, name.span)
        todo!() // @Beacon @Beacon @Beacon @Task
    }
}

impl TryFrom<Identifier> for Spanned<Word> {
    type Error = ();

    fn try_from(identifier: Identifier) -> Result<Self, Self::Error> {
        // identifier
        //     .is_word()
        //     .then(|| Self::new(identifier.span(), Word(identifier.into_atom())))
        //     .ok_or(())
        todo!() // @Beacon @Beacon @Beacon @Task
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

#[derive(Clone, Copy)]
pub enum SpannedExplicitness {
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
