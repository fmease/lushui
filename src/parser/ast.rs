use crate::{
    diagnostic::{Code, Diagnostic, Result, Results},
    lexer::{Number, Token, TokenKind},
    smallvec,
    span::{PossiblySpanning, SourceFile, Span, Spanned, Spanning},
    support::InvalidFallback,
    support::ManyErrExt,
    Atom, SmallVec,
};
use std::{convert::TryFrom, convert::TryInto, fmt, rc::Rc};

pub type Item<Kind> = crate::item::Item<Kind, Attributes>;

pub type Declaration = Item<DeclarationKind>;

/// The syntax node of a declaration.
#[derive(Debug)]
pub enum DeclarationKind {
    Value(Box<Value>),
    Data(Box<Data>),
    Constructor(Box<Constructor>),
    Module(Box<Module>),
    Crate(Box<Crate>),
    Header(Box<Header>),
    Group(Box<Group>),
    Use(Box<Use>),
}

/// The syntax node of a value declaration or a let statement.
///
/// See [DeclarationKind::Value] and [Statement::Let].
#[derive(Clone, Debug)]
pub struct Value {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_annotation: Option<Expression>,
    pub expression: Option<Expression>,
}

/// The syntax node of a data declaration.
#[derive(Debug)]
pub struct Data {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_annotation: Option<Expression>,
    pub constructors: Option<Vec<Declaration>>,
}

/// The syntax node of a constructor.
#[derive(Debug)]
pub struct Constructor {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_annotation: Option<Expression>,
    pub record: bool,
}

/// The syntax node of a module declaration.
#[derive(Debug)]
pub struct Module {
    pub binder: Identifier,
    pub file: Rc<SourceFile>,
    pub exposures: ExposureList,
    pub declarations: Option<Vec<Declaration>>,
}

/// The syntax node of a crate declaration.
#[derive(Debug)]
pub struct Crate {
    pub binder: Identifier,
}

/// The syntax node of a module header.
#[derive(Debug)]
pub struct Header {
    pub exposures: ExposureList,
}

/// The syntax node of attribute groups.
#[derive(Debug)]
pub struct Group {
    pub declarations: Vec<Declaration>,
}

/// The syntax node of a use declaration or statement.
///
/// See [DeclarationKind::Use] and [Statement::Use].
#[derive(Clone, Debug)]
pub struct Use {
    pub bindings: PathTree,
}

#[derive(Debug, Clone)]
pub enum PathTree {
    Single {
        target: Path,
        binder: Option<Identifier>,
    },
    Multiple {
        path: Path,
        bindings: Vec<Self>,
    },
}

impl Spanning for PathTree {
    fn span(&self) -> Span {
        match self {
            Self::Single { target, binder } => target.span().merge(binder),
            Self::Multiple { path, bindings } => {
                path.span().merge(&bindings.first()).merge(&bindings.last())
            }
        }
    }
}

// @Temporary
// @Task support for constructor (and field) exposures (paths + multipaths)
type ExposureList = Vec<Identifier>;

pub type Attributes = Vec<Attribute>;

#[derive(Debug, Clone)]
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

// @Task add span information
#[derive(Debug, Clone)]
pub enum AttributeArgument {
    NumberLiteral(Box<Number>),
    TextLiteral(Box<String>),
    /// To be able to lower documentation comments without immense memory wastage.
    Generated,
    Path(Box<Path>),
    Named(Box<NamedAttributeArgument>),
}

#[derive(Debug, Clone)]
pub struct NamedAttributeArgument {
    pub binder: Identifier,
    pub value: AttributeArgument,
}

pub type Expression = Item<ExpressionKind>;

/// The syntax node of an expression.
#[derive(Debug, Clone)]
pub enum ExpressionKind {
    PiTypeLiteral(Box<PiTypeLiteral>),
    Application(Box<Application>),
    TypeLiteral,
    NumberLiteral(Box<Number>),
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

/// The syntax node of pi-type literals.
#[derive(Debug, Clone)]
pub struct PiTypeLiteral {
    pub binder: Option<Identifier>,
    pub domain: Expression,
    pub codomain: Expression,
    pub explicitness: Explicitness,
}

/// The syntax node of function application.
#[derive(Debug, Clone)]
pub struct Application {
    pub callee: Expression,
    pub argument: Expression,
    pub explicitness: Explicitness,
    pub binder: Option<Identifier>,
}

/// The syntax node of a typed hole.
#[derive(Debug, Clone)]
pub struct TypedHole {
    pub tag: Identifier,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub hanger: Option<Hanger>,
    pub segments: SmallVec<Identifier, 1>,
}

/// The syntax node of a lambda literal expression.
#[derive(Debug, Clone)]
pub struct LambdaLiteral {
    pub parameters: Parameters,
    pub body_type_annotation: Option<Expression>,
    pub body: Expression,
}

/// The syntax-node of a let/in expression.
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub struct UseIn {
    pub bindings: PathTree,
    pub scope: Expression,
}

#[derive(Debug, Clone)]
pub struct CaseAnalysis {
    pub expression: Expression,
    pub cases: Vec<Case>,
}

#[derive(Debug, Clone)]
pub struct DoBlock {
    pub statements: Vec<Statement>,
}

// @Note we probably gonna need to make this spanning in the future (for diagnostics) just like
// Expression, Pattern, Declaration.
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub struct LetStatement {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_annotation: Option<Expression>,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct BindStatement {
    pub binder: Identifier,
    pub type_annotation: Option<Expression>,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct SequenceLiteral {
    pub elements: Vec<Expression>,
}

impl From<Path> for Expression {
    fn from(path: Path) -> Self {
        Expression {
            span: path.span(),
            kind: ExpressionKind::Path(Box::new(path)),
            attributes: Attributes::default(),
        }
    }
}

impl From<Path> for Pattern {
    fn from(path: Path) -> Self {
        Pattern {
            span: path.span(),
            kind: PatternKind::Path(Box::new(path)),
            attributes: Attributes::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Case {
    pub pattern: Pattern,
    pub expression: Expression,
}

pub type Parameters = Vec<ParameterGroup>;

#[derive(Debug, Clone)]
pub struct ParameterGroup {
    pub parameters: SmallVec<Identifier, 1>,
    pub type_annotation: Option<Expression>,
    pub explicitness: Explicitness,
    pub span: Span,
}

impl Spanning for ParameterGroup {
    fn span(&self) -> Span {
        self.span
    }
}

pub type Pattern = Item<PatternKind>;

#[derive(Debug, Clone)]
pub enum PatternKind {
    NumberLiteral(Box<Number>),
    TextLiteral(Box<String>),
    // @Note unfortunate naming
    SequenceLiteralPattern(Box<SequenceLiteralPattern>),
    Path(Box<Path>),
    Binder(Box<Binder>),
    Deapplication(Box<Deapplication>),
}

/// A binder inside of a pattern.
#[derive(Debug, Clone)]
pub struct Binder {
    pub binder: Identifier,
}

#[derive(Debug, Clone)]
pub struct Deapplication {
    pub callee: Pattern,
    pub argument: Pattern,
    pub explicitness: Explicitness,
    pub binder: Option<Identifier>,
}

#[derive(Debug, Clone)]
pub struct SequenceLiteralPattern {
    pub elements: Vec<Pattern>,
}

pub type Hanger = Spanned<HangerKind>;

impl fmt::Display for Hanger {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self.kind {
                HangerKind::Crate => "crate",
                HangerKind::Super => "super",
                HangerKind::Self_ => "self",
            }
        )
    }
}

/// The non-identifier head of a path.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum HangerKind {
    Crate,
    Super,
    Self_,
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

    /// Panics if the token is not an identifier.
    pub fn from_token(token: Token) -> Self {
        Self {
            span: token.span,
            atom: token.identifier(),
        }
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

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Identifier ({}) {:?}", self.atom, self.span)
    }
}

// @Temporary
impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:width$}",
            self.atom,
            width = f.width().unwrap_or_default()
        )
    }
}

impl Path {
    /// Construct a single identifier segment path.
    ///
    /// May panic.
    pub fn identifier(token: Token) -> Self {
        Self {
            hanger: None,
            segments: smallvec![Identifier::from_token(token).into()],
        }
    }

    /// Construct a non-identifier-head-only path.
    pub fn hanger(token: Token) -> Self {
        Self {
            hanger: Some(Hanger::new(token.span, token.kind.try_into().unwrap())),
            segments: SmallVec::new(),
        }
    }

    pub fn join(mut self, other: Self) -> Result<Self> {
        if let Some(head) = other.hanger {
            if !matches!(head.kind, HangerKind::Self_) {
                return Err(Diagnostic::error()
                    .with_code(Code::E026)
                    .with_message("`super` or `crate` not allowed in this position")
                    .with_span(&head));
            }
        }
        self.segments.extend(other.segments);
        Ok(self)
    }

    pub fn is_self(&self) -> bool {
        matches!(
            self.hanger,
            Some(Hanger {
                kind: HangerKind::Self_,
                ..
            })
        )
    }

    pub fn first_identifier(&self) -> Option<&Identifier> {
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

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn display_path(
            head: &Option<Hanger>,
            segments: &[Identifier],
            f: &mut fmt::Formatter<'_>,
        ) -> fmt::Result {
            let mut segments = segments.iter();

            match head {
                Some(head) => write!(f, "{}", head)?,
                None => write!(f, "{}", segments.next().unwrap())?,
            }

            write!(
                f,
                "{}",
                segments
                    .map(|segment| format!(".{}", segment))
                    .collect::<String>()
            )
        }
        display_path(&self.hanger, &self.segments, f)
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
            Header(_) => "a module header declaraiton",
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
            Module(_) | Header(_) => AttributeTargets::MODULE_DECLARATION,
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
                value.expression.as_ref().map(|expression| expression.span),
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
                    .with_span(self)
                    // @Task make this less awkward for data declarations
                    .with_help("provide a definition for the declaration with `= VALUE`"))
            }
            // @Task return two diagnostics
            (Some(body), true) => Err(Diagnostic::error()
                .with_code(Code::E020)
                .with_message(format!(
                    "`{}` is defined multiple times in this scope",
                    binder
                ))
                .with_labeled_span(&body, "conflicting definition")
                .with_labeled_span(
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

/// The explicitness of a parameter or argument.
///
/// In the context of parameters, this specifies whether in an application, the corresponding argument has
/// to be passed explicitly or should be infered, i.e. the parameter is [Implicit].
///
/// In the context of applications, [Implicit] means that the argument is passed explicitly
/// even though the parameter is marked implicit.
#[derive(Clone, Copy, Debug)]
pub enum Explicitness {
    Implicit,
    Explicit,
}

pub use Explicitness::*;

impl Explicitness {
    pub const fn is_implicit(self) -> bool {
        matches!(self, Implicit)
    }
}

impl fmt::Display for Explicitness {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Implicit => write!(f, ","),
            Explicit => write!(f, ""),
        }
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
