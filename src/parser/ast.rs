//! The abstract syntax tree (AST).
//!
//! The most important definitions are [Declaration], [Expression] and [Pattern].

mod format;

use crate::{
    diagnostics::{Code, Diagnostic, Result, Results},
    lexer::{Token, TokenKind},
    smallvec,
    span::{PossiblySpanning, SourceFile, Span, Spanned, Spanning},
    support::InvalidFallback,
    support::ManyErrExt,
    Atom, SmallVec,
};
use std::{convert::TryFrom, convert::TryInto, default::default, rc::Rc};

pub use format::Format;

pub type Item<Kind> = crate::item::Item<Kind, Attributes>;

pub type Declaration = Item<DeclarationKind>;

/// The syntax node of a declaration.
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum DeclarationKind {
    Value(Box<Value>),
    Data(Box<Data>),
    Constructor(Box<Constructor>),
    Module(Box<Module>),
    Crate(Box<Crate>),
    Header,
    Group(Box<Group>),
    Use(Box<Use>),
}

/// The syntax node of a value declaration or a let statement.
///
/// See [DeclarationKind::Value] and [Statement::Let].
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Value {
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
    pub file: Rc<SourceFile>,
    pub declarations: Option<Vec<Declaration>>,
}

/// The syntax node of a crate declaration.
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Crate {
    pub binder: Identifier,
}

/// The syntax node of attribute groups.
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Group {
    pub declarations: Vec<Declaration>,
}

/// The syntax node of a use declaration or statement.
///
/// See [DeclarationKind::Use] and [Statement::Use].
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
    Generated,
    Path(Box<Path>),
    Named(Box<NamedAttributeArgument>),
}

impl AttributeArgumentKind {
    pub const fn name(&self) -> &'static str {
        match self {
            Self::NumberLiteral(_) => "number literal",
            Self::TextLiteral(_) => "text literal",
            Self::Generated => "generated text",
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
    LambdaLiteral(Box<LambdaLiteral>),
    LetIn(Box<LetIn>),
    UseIn(Box<UseIn>),
    CaseAnalysis(Box<CaseAnalysis>),
    DoBlock(Box<DoBlock>),
    SequenceLiteral(Box<SequenceLiteral>),
    Invalid,
}

impl InvalidFallback for ExpressionKind {
    fn invalid() -> Self {
        Self::Invalid
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
    pub aspect: ParameterAspect,
    pub binder: Option<Identifier>,
    pub expression: Expression,
}

impl Domain {
    pub(super) fn simple(expression: Expression) -> Self {
        Self {
            aspect: default(),
            binder: None,
            expression,
        }
    }
}

/// The extra qualities a parameter of a pi type can posses.
// @Task change types to Spanned<Explicitness>, Spanned<Laziness>,
// Spanned<Fieldness>, @Update maybe??? whould the span be SHAM if ~None?
// hmm not that great
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct ParameterAspect {
    pub explicitness: Explicitness,
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

impl From<Explicitness> for ParameterAspect {
    fn from(explicitness: Explicitness) -> Self {
        Self {
            explicitness,
            ..default()
        }
    }
}

/// The syntax node of function application.
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Application {
    pub callee: Expression,
    pub argument: Expression,
    pub explicitness: Explicitness,
    pub binder: Option<Identifier>,
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
    pub segments: SmallVec<Identifier, 1>,
}

impl Path {
    /// Construct a single identifier segment path.
    ///
    /// May panic.
    pub fn try_from_token(token: Token) -> Option<Self> {
        Some(Identifier::try_from(token).ok()?.into())
    }

    /// Construct a non-identifier-head-only path.
    pub fn hanger(token: Token) -> Self {
        Self {
            hanger: Some(Hanger::new(token.span, token.kind.try_into().unwrap())),
            segments: SmallVec::new(),
        }
    }

    // @Task make this Option<Self> and move diagnostic construction into lowerer
    pub fn join(mut self, other: Self) -> Result<Self> {
        if let Some(hanger) = other.hanger {
            if !matches!(hanger.kind, HangerKind::Self_) {
                return Err(Diagnostic::error()
                    .with_code(Code::E026)
                    .with_message(format!(
                        "path hanger `{}` not allowed in this position",
                        hanger
                    ))
                    .with_primary_span(&hanger)
                    .with_help("consider moving this path to its own separate use declaration"));
            }
        }
        self.segments.extend(other.segments);
        Ok(self)
    }

    pub fn is_self(&self) -> bool {
        self.hanger
            .map_or(false, |hanger| hanger.kind == HangerKind::Self_)
            && self.segments.is_empty()
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
    pub fn to_simple(&self) -> Option<&Identifier> {
        if self.hanger.is_some() || self.segments.len() > 1 {
            return None;
        }

        Some(&self.segments[0])
    }

    pub fn tail(&self) -> Self {
        Self {
            hanger: None,
            // @Task avoid allocation, try to design it as a slice `&self.segments[1..]`
            segments: if self.hanger.is_some() {
                self.segments.clone()
            } else {
                self.segments.iter().skip(1).cloned().collect()
            },
        }
    }

    // @Task avoid allocation, try to design it as a slice `&self.segments[..LEN - 1]`
    /// Path consisting of segments 0 to n-1
    // @Task verify
    pub fn prefix(&self) -> Self {
        Self {
            hanger: self.hanger.clone(),
            segments: self.segments.iter().rev().skip(1).rev().cloned().collect(),
        }
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
                .merge(&self.segments.first())
                .merge(&self.segments.last())
        } else {
            self.segments
                .first()
                .unwrap()
                .span
                .merge(&self.segments.last().unwrap())
        }
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

/// The syntax-node of a let/in expression.
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct LetIn {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_annotation: Option<Expression>,
    // @Task improve upon naming
    // @Note we could make this syntactically optional and then prove a beatiful error message
    // in the lowerer (that's what we currently do for [DeclarationKind::Value] and plan to do
    // for [LetStatement])
    pub expression: Expression,
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

// @Note has a lot of overlap with [StatementKind::Value] (and a bit with [ExpressionKind::LetIn])
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
    pub aspect: ParameterAspect,
    /// non-empty
    // @Task make type-safe
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
    // @Note unfortunate naming
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
    pub argument: Pattern,
    pub explicitness: Explicitness,
    pub binder: Option<Identifier>,
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
    Crate,
    Super,
    Self_,
}

impl HangerKind {
    pub const fn name(self) -> &'static str {
        match self {
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
            TokenKind::Crate => Self::Crate,
            TokenKind::Super => Self::Super,
            TokenKind::Self_ => Self::Self_,
            _ => return Err(()),
        })
    }
}

#[derive(Clone, Eq)]
pub struct Identifier {
    atom: Atom,
    pub span: Span,
}

impl Identifier {
    pub fn new(atom: Atom, span: Span) -> Self {
        Self { atom, span }
    }

    pub fn is_punctuation(&self) -> bool {
        crate::lexer::is_punctuation(self.atom.chars().next().unwrap())
    }

    pub fn as_str(&self) -> &str {
        &self.atom
    }

    pub fn stripped(self) -> Self {
        Self {
            span: Span::SHAM,
            ..self
        }
    }
}

impl TryFrom<Token> for Identifier {
    type Error = ();

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        Ok(Self {
            span: token.span,
            atom: token.identifier().ok_or(())?,
        })
    }
}

impl From<Identifier> for Expression {
    fn from(identifier: Identifier) -> Self {
        expr! {
            Path(Attributes::default(), identifier.span; Path::from(identifier))
        }
    }
}

impl Spanning for Identifier {
    fn span(&self) -> Span {
        self.span
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.atom == other.atom
    }
}

impl std::hash::Hash for Identifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.atom.hash(state);
    }
}

/// Something attributes can be ascribed to.
///
/// This is the trait version of the struct [Item].
pub trait AttributeTarget: Spanning {
    /// Used in diagnostics.
    fn name(&self) -> &'static str;
    fn as_attribute_targets(&self) -> AttributeTargets;

    /// Target-specific attribute checks
    // @Note this is the wrong place, it should be in the lowerer, somehow
    fn check_attributes(&self, _attributes: &crate::lowered_ast::Attributes) -> Results<()> {
        Ok(())
    }
}

impl AttributeTarget for Declaration {
    fn name(&self) -> &'static str {
        use DeclarationKind::*;

        match self.kind {
            Value(_) => "a value declaration",
            Data(_) => "a data declaration",
            Constructor(_) => "a constructor declaration",
            Module(_) => "a module declaration",
            Header => "a module header declaraiton",
            Crate(_) => "a crate declaration",
            Group(_) => "an attribute group declaration",
            Use(_) => "a use declaration",
        }
    }

    fn as_attribute_targets(&self) -> AttributeTargets {
        use DeclarationKind::*;

        match self.kind {
            Value(_) => AttributeTargets::VALUE_DECLARATION,
            Data(_) => AttributeTargets::DATA_DECLARATION,
            Constructor(_) => AttributeTargets::CONSTRUCTOR_DECLARATION,
            Module(_) | Header => AttributeTargets::MODULE_DECLARATION,
            Crate(_) => AttributeTargets::CRATE_DECLARATION,
            Group(_) => AttributeTargets::all(),
            Use(_) => AttributeTargets::USE_DECLARATION,
        }
    }

    fn check_attributes(&self, attributes: &crate::lowered_ast::Attributes) -> Results<()> {
        use crate::lowered_ast::AttributeKeys;
        use DeclarationKind::*;

        let (body, binder) = match &self.kind {
            Value(value) => (
                value.body.as_ref().map(|expression| expression.span),
                &value.binder,
            ),
            // @Task instead of using the span of the whole data declaration for empty bodies
            // (`=` but nothing else), find a way to return the span of the `=`
            Data(data) => (
                data.constructors
                    .as_ref()
                    .map(|constructors| constructors.possible_span().unwrap_or(self.span)),
                &data.binder,
            ),
            _ => return Ok(()),
        };

        match (body, attributes.has(AttributeKeys::FOREIGN)) {
            (None, false) => {
                Err(Diagnostic::error()
                    .with_code(Code::E012)
                    .with_message(format!("declaration `{}` has no definition", binder))
                    .with_primary_span(self)
                    // @Task make this less awkward for data declarations
                    .with_help("provide a definition for the declaration with `= VALUE`"))
            }
            (Some(body), true) => Err(Diagnostic::error()
                .with_code(Code::E020)
                .with_message(format!(
                    "`{}` is defined multiple times in this scope",
                    binder
                ))
                .with_labeled_primary_span(&body, "conflicting definition")
                .with_labeled_secondary_span(
                    &attributes.get(AttributeKeys::FOREIGN).next().unwrap(),
                    "conflicting definition",
                )
                .with_note(
                    "declaration is marked `foreign` but it also has a body introduced by `=`",
                )),
            _ => Ok(()),
        }
        .many_err()
    }
}

impl AttributeTarget for Expression {
    fn name(&self) -> &'static str {
        use ExpressionKind::*;

        match self.kind {
            PiTypeLiteral(_) => "a pi type literal",
            Application(_) => "an application",
            TypeLiteral => "a type literal",
            NumberLiteral(_) => "a number literal expression",
            TextLiteral(_) => "a text literal expression",
            TypedHole(_) => "a typed hole",
            Path(_) => "a path expression",
            LambdaLiteral(_) => "a lambda literal",
            LetIn(_) => "a let/in expression",
            UseIn(_) => "a use/in expression",
            CaseAnalysis(_) => "a case analysis",
            DoBlock(_) => "a do block",
            SequenceLiteral(_) => "a sequence literal expression",
            Invalid => "an invalid expression",
        }
    }

    fn as_attribute_targets(&self) -> AttributeTargets {
        use ExpressionKind::*;

        match self.kind {
            PiTypeLiteral(_) => AttributeTargets::PI_TYPE_LITERAL_EXPRESSION,
            Application(_) => AttributeTargets::APPLICATION_EXPRESSION,
            TypeLiteral => AttributeTargets::TYPE_LITERAL_EXPRESSION,
            NumberLiteral(_) => AttributeTargets::NUMBER_LITERAL_EXPRESSION,
            TextLiteral(_) => AttributeTargets::TEXT_LITERAL_EXPRESSION,
            TypedHole(_) => AttributeTargets::TYPED_HOLE_EXPRESSION,
            Path(_) => AttributeTargets::PATH_EXPRESSION,
            LambdaLiteral(_) => AttributeTargets::LAMBDA_LITERAL_EXPRESSION,
            LetIn(_) => AttributeTargets::LET_IN_EXPRESSION,
            UseIn(_) => AttributeTargets::USE_IN_EXPRESSION,
            CaseAnalysis(_) => AttributeTargets::CASE_ANALYSIS_EXPRESSION,
            DoBlock(_) => AttributeTargets::DO_BLOCK_EXPRESSION,
            SequenceLiteral(_) => AttributeTargets::SEQUENCE_LITERAL_EXPRESSION,
            Invalid => AttributeTargets::empty(),
        }
    }
}

impl AttributeTarget for Pattern {
    fn name(&self) -> &'static str {
        use PatternKind::*;

        match self.kind {
            NumberLiteral(_) => "a number literal pattern",
            TextLiteral(_) => "a text literal pattern",
            SequenceLiteralPattern(_) => "a sequence literal pattern",
            Path(_) => "a path pattern",
            Binder(_) => "a binder pattern",
            Deapplication(_) => "a deapplication",
        }
    }

    fn as_attribute_targets(&self) -> AttributeTargets {
        use PatternKind::*;

        match self.kind {
            NumberLiteral(_) => AttributeTargets::NUMBER_LITERAL_PATTERN,
            TextLiteral(_) => AttributeTargets::TEXT_LITERAL_PATTERN,
            SequenceLiteralPattern(_) => AttributeTargets::SEQUENCE_LITERAL_PATTERN,
            Path(_) => AttributeTargets::PATH_PATTERN,
            Binder(_) => AttributeTargets::BINDER_PATTERN,
            Deapplication(_) => AttributeTargets::DEAPPLICATION_PATTERN,
        }
    }
}

// excluded: crate::ast::DeclarationKind::{Header, Group}
// @Task somehow generate the explicit bits
bitflags::bitflags! {
    /// Attribute targets.
    pub struct AttributeTargets: u32 {
        const VALUE_DECLARATION = 1 << 0;
        const DATA_DECLARATION = 1 << 1;
        const CONSTRUCTOR_DECLARATION = 1 << 2;
        const MODULE_DECLARATION = 1 << 3;
        const CRATE_DECLARATION = 1 << 4;
        const USE_DECLARATION = 1 << 5;

        const DECLARATION = Self::VALUE_DECLARATION.bits
            | Self::DATA_DECLARATION.bits
            | Self::CONSTRUCTOR_DECLARATION.bits
            | Self::MODULE_DECLARATION.bits
            | Self::CRATE_DECLARATION.bits
            | Self::USE_DECLARATION.bits;

        const PI_TYPE_LITERAL_EXPRESSION = 1 << 6;
        const APPLICATION_EXPRESSION = 1 << 7;
        const TYPE_LITERAL_EXPRESSION = 1 << 8;
        const NUMBER_LITERAL_EXPRESSION = 1 << 9;
        const TEXT_LITERAL_EXPRESSION = 1 << 10;
        const TYPED_HOLE_EXPRESSION = 1 << 11;
        const PATH_EXPRESSION = 1 << 12;
        const LAMBDA_LITERAL_EXPRESSION = 1 << 13;
        const LET_IN_EXPRESSION = 1 << 14;
        const USE_IN_EXPRESSION = 1 << 15;
        const CASE_ANALYSIS_EXPRESSION = 1 << 16;
        const DO_BLOCK_EXPRESSION = 1 << 17;
        const SEQUENCE_LITERAL_EXPRESSION = 1 << 18;

        const EXPRESSION = Self::PI_TYPE_LITERAL_EXPRESSION.bits
            | Self::APPLICATION_EXPRESSION.bits
            | Self::TYPE_LITERAL_EXPRESSION.bits
            | Self::NUMBER_LITERAL_EXPRESSION.bits
            | Self::TEXT_LITERAL_EXPRESSION.bits
            | Self::TYPED_HOLE_EXPRESSION.bits
            | Self::PATH_EXPRESSION.bits
            | Self::LAMBDA_LITERAL_EXPRESSION.bits
            | Self::LET_IN_EXPRESSION.bits
            | Self::USE_IN_EXPRESSION.bits
            | Self::CASE_ANALYSIS_EXPRESSION.bits
            | Self::DO_BLOCK_EXPRESSION.bits
            | Self::SEQUENCE_LITERAL_EXPRESSION.bits;

        const NUMBER_LITERAL_PATTERN = 1 << 19;
        const TEXT_LITERAL_PATTERN = 1 << 20;
        const SEQUENCE_LITERAL_PATTERN = 1 << 21;
        const PATH_PATTERN = 1 << 22;
        const BINDER_PATTERN = 1 << 23;
        const DEAPPLICATION_PATTERN = 1 << 24;

        const PATTERN = Self::NUMBER_LITERAL_PATTERN.bits
            | Self::TEXT_LITERAL_PATTERN.bits
            | Self::SEQUENCE_LITERAL_PATTERN.bits
            | Self::PATH_PATTERN.bits
            | Self::BINDER_PATTERN.bits
            | Self::DEAPPLICATION_PATTERN.bits;

        const NUMBER_LITERAL = Self::NUMBER_LITERAL_EXPRESSION.bits | Self::NUMBER_LITERAL_PATTERN.bits;
        const TEXT_LITERAL = Self::TEXT_LITERAL_EXPRESSION.bits | Self::TEXT_LITERAL_PATTERN.bits;
        const SEQUENCE_LITERAL = Self::SEQUENCE_LITERAL_EXPRESSION.bits | Self::SEQUENCE_LITERAL_PATTERN.bits;
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
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Explicitness {
    Implicit,
    Explicit,
}

impl Default for Explicitness {
    fn default() -> Self {
        Explicit
    }
}

pub macro decl($( $tree:tt )+) {
    crate::item::item!(crate::ast, DeclarationKind, Box; $( $tree )+)
}

pub macro expr($( $tree:tt )+) {
    crate::item::item!(crate::ast, ExpressionKind, Box; $( $tree )+)
}

pub macro pat($( $tree:tt )+) {
    crate::item::item!(crate::ast, PatternKind, Box; $( $tree )+)
}

pub macro attrarg($kind:ident($span:expr; $value:expr $(,)?)) {
    crate::ast::AttributeArgument::new(
        $span,
        crate::ast::AttributeArgumentKind::$kind(Box::new($value)),
    )
}
