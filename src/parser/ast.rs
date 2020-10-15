use std::{
    fmt,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use crate::{
    diagnostic::{Code, Diagnostic, Result},
    lexer::{Number, Token},
    smallvec,
    span::{PossiblySpanning, SourceFile, Span, Spanned, Spanning},
    support::InvalidFallback,
    Atom, SmallVec,
};

#[derive(Debug)]
pub struct Declaration {
    pub kind: DeclarationKind,
    pub span: Span,
    pub attributes: Attributes,
}

impl Declaration {
    pub fn as_attribute_target(&self) -> Targets {
        use DeclarationKind::*;

        match &self.kind {
            Value(_) => Targets::VALUE,
            Data(_) => Targets::DATA,
            Constructor(_) => Targets::CONSTRUCTOR,
            Module(_) | Header(_) => Targets::MODULE,
            Crate(_) => Targets::CRATE,
            Group(_) => Targets::all(),
            Use(_) => Targets::USE,
        }
    }

    pub fn kind_as_str(&self) -> &'static str {
        use DeclarationKind::*;

        match &self.kind {
            Value(_) => "value",
            Data(_) => "data",
            Constructor(_) => "constructor",
            Module(_) => "module",
            Crate(_) => "crate",
            Header(_) => "module header",
            Group(_) => "attribute group",
            Use(_) => "use",
        }
    }
}

impl Spanning for Declaration {
    fn span(&self) -> Span {
        self.span
    }
}

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
    pub bindings: UseBindings,
}

// @Task documentation, span information
#[derive(Debug, Clone)]
pub enum UseBindings {
    Single {
        target: Path,
        binder: Option<Identifier>,
    },
    Multiple {
        path: Path,
        bindings: Vec<Self>,
    },
}

impl Spanning for UseBindings {
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

#[derive(Debug, Default, Clone)]
pub struct Attributes(Vec<Attribute>);

impl Attributes {
    pub fn has(&self, query: AttributeKind) -> bool {
        self.get(query).is_some()
    }

    pub fn get(&self, query: AttributeKind) -> Option<&Attribute> {
        self.iter().find(|attribute| attribute.kind == query)
    }

    pub fn filter(&self, predicate: impl Fn(&Attribute) -> bool) -> Vec<&Attribute> {
        self.iter()
            .filter(|attribute| predicate(attribute))
            .collect()
    }

    pub fn nonconforming(&self, targets: Targets) -> Vec<&Attribute> {
        self.filter(|attribute| !attribute.targets().contains(targets))
    }
}

impl Deref for Attributes {
    type Target = Vec<Attribute>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Attributes {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub type Attribute = Spanned<AttributeKind>;

impl Attribute {
    pub fn targets(&self) -> Targets {
        use AttributeKind::*;

        match self.kind {
            Documentation | Deprecated | Unstable | If | Allow | Warn | Deny | Forbid => {
                Targets::all()
            }
            Foreign => Targets::VALUE | Targets::DATA,
            Inherent | Moving | Shallow => Targets::DATA,
            Unsafe => Targets::VALUE | Targets::CONSTRUCTOR,
        }
    }

    // @Note in most cases (except documentation) because they have arguments/are parameterized
    pub fn unique(&self) -> bool {
        use AttributeKind::*;

        match self.kind {
            Foreign | Inherent | Moving | Deprecated | Unstable | Unsafe | Shallow => true,
            Documentation | If | Allow | Warn | Deny | Forbid => false,
        }
    }
}

// @Task add `include`, `provide`, … in the future
// @Task add arguments
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttributeKind {
    Documentation,
    Foreign,
    /// Make bindings available for FFI. Opposite of `foreign`.
    Inherent,
    Moving,
    Deprecated,
    Unstable,
    If,
    Allow,
    Warn,
    Deny,
    Forbid,
    Unsafe,
    Shallow,
}

impl AttributeKind {
    pub const fn name(self) -> &'static str {
        match self {
            Self::Documentation => "documentation comment",
            Self::Foreign => "foreign",
            Self::Inherent => "inherent",
            Self::Moving => "moving",
            Self::Deprecated => "deprecated",
            Self::Unstable => "unstable",
            Self::If => "if",
            Self::Allow => "allow",
            Self::Warn => "warn",
            Self::Deny => "deny",
            Self::Forbid => "forbid",
            Self::Unsafe => "unsafe",
            Self::Shallow => "shallow",
        }
    }
}

impl fmt::Display for AttributeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if *self == Self::Documentation {
            return write!(f, "a {}", self.name());
        }

        write!(f, "attribute `{}`", self.name())
    }
}

bitflags::bitflags! {
    pub struct Targets: u8 {
        const VALUE = 0b0000_0001;
        const DATA = 0b0000_0010;
        const CONSTRUCTOR = 0b0000_0100;
        const MODULE = 0b0000_1000;
        const CRATE = 0b0010_0000;
        const USE = 0b0001_0000;
    }
}

pub type Expression = Spanned<ExpressionKind>;

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
    /// See documentation on [crate::hir::Expression::Invalid].
    Invalid,
}

/// The syntax node of pi-type literals.
#[derive(Debug, Clone)]
pub struct PiTypeLiteral {
    pub binder: Option<Identifier>,
    pub parameter: Expression,
    pub expression: Expression,
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

#[derive(Debug, Clone)]
pub struct Path {
    pub head: Option<Head>,
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
    // in the desugarer (that's what we currently do for [DeclarationKind::Value] and plan to do
    // for [LetStatement])
    pub expression: Expression,
    pub scope: Expression,
}

/// The syntax node of a use/in expression.
#[derive(Debug, Clone)]
pub struct UseIn {
    pub bindings: UseBindings,
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
    // (missing definition) when desugaring
    Let(LetStatement),
    Use(Use),
    // @Question should we rename this to Assign since we plan on not only desugaring
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

impl InvalidFallback for Expression {
    fn invalid() -> Self {
        expr! {
            Invalid[Span::SHAM]
        }
    }
}

impl From<Path> for Expression {
    fn from(path: Path) -> Self {
        Expression {
            span: path.span(),
            kind: ExpressionKind::Path(Box::new(path)),
        }
    }
}

impl From<Path> for Pattern {
    fn from(path: Path) -> Self {
        Pattern {
            span: path.span(),
            kind: PatternKind::Path(Box::new(path)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Case {
    pub pattern: Pattern,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct Parameters {
    pub parameters: Vec<ParameterGroup>,
    pub span: Option<Span>,
}

impl PossiblySpanning for Parameters {
    fn possible_span(&self) -> Option<Span> {
        self.span
    }
}

impl Deref for Parameters {
    type Target = Vec<ParameterGroup>;

    fn deref(&self) -> &Self::Target {
        &self.parameters
    }
}

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

pub type Pattern = Spanned<PatternKind>;

#[derive(Debug, Clone)]
pub enum PatternKind {
    NumberLiteral(Box<Number>),
    TextLiteral(Box<String>),
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
}

pub type Head = Spanned<HeadKind>;

impl fmt::Display for Head {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self.kind {
                HeadKind::Crate => "crate",
                HeadKind::Super => "super",
                HeadKind::Self_ => "self",
            }
        )
    }
}

/// The head of a path.
#[derive(Clone, Debug)]
pub enum HeadKind {
    Crate,
    Super,
    Self_,
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
    pub fn new_identifier(token: Token) -> Self {
        Self {
            head: None,
            segments: smallvec![Identifier::from_token(token).into()],
        }
    }

    pub fn new_crate(token: Token) -> Self {
        Self {
            head: Some(Head::new(token.span, HeadKind::Crate)),
            segments: SmallVec::new(),
        }
    }

    pub fn new_super(token: Token) -> Self {
        Self {
            head: Some(Head::new(token.span, HeadKind::Super)),
            segments: SmallVec::new(),
        }
    }

    pub fn new_self(token: Token) -> Self {
        Self {
            head: Some(Head::new(token.span, HeadKind::Self_)),
            segments: SmallVec::new(),
        }
    }

    pub fn join(mut self, other: Self) -> Result<Self> {
        if let Some(head) = other.head {
            if !matches!(head.kind, HeadKind::Self_) {
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
            self.head,
            Some(Head {
                kind: HeadKind::Self_,
                ..
            })
        )
    }

    pub fn first_identifier(&self) -> Option<&Identifier> {
        if self.head.is_some() {
            return None;
        }

        Some(&self.segments[0])
    }

    pub fn last_identifier(&self) -> Option<&Identifier> {
        self.segments.last()
    }

    pub fn simple(&self) -> Option<&Identifier> {
        if self.head.is_some() || self.segments.len() > 1 {
            return None;
        }

        Some(&self.segments[0])
    }

    pub fn tail(&self) -> Self {
        Self {
            head: None,
            // @Task avoid allocation, try to design it as a slice `&self.segments[1..]`
            segments: if self.head.is_some() {
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
            head: self.head.clone(),
            segments: self.segments.iter().rev().skip(1).rev().cloned().collect(),
        }
    }
}

impl Spanning for Path {
    fn span(&self) -> Span {
        if let Some(head) = &self.head {
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
            head: &Option<Head>,
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
        display_path(&self.head, &self.segments, f)
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
    pub fn is_implicit(self) -> bool {
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

pub macro decl($kind:ident[$span:expr] { $( $body:tt )+ }) {
    Declaration {
        span: $span,
        attributes: Attributes::default(),
        kind: DeclarationKind::$kind(Box::new(self::$kind { $( $body )+ })),
    }
}

pub macro expr {
    ($kind:ident[$span:expr] { $( $body:tt )+ }) => {
        Expression::new(
            $span,
            ExpressionKind::$kind(Box::new(self::$kind { $( $body )+ })),
        )
    },
    ($kind:ident[$span:expr]) => {
        Expression::new(
            $span,
            ExpressionKind::$kind,
        )
    },
    ($kind:ident[$span:expr]($value:expr)) => {
        Expression::new(
            $span,
            ExpressionKind::$kind(Box::from($value)),
        )
    },
}

pub macro pat {
    ($kind:ident[$span:expr] { $( $body:tt )+ }) => {
        Pattern::new(
            $span,
            PatternKind::$kind(Box::new(self::$kind { $( $body )+ })),
        )
    },
    ($kind:ident[$span:expr]($value:expr)) => {
        Pattern::new(
            $span,
            PatternKind::$kind(Box::from($value)),
        )
    },
}
