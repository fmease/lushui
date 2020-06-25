use std::{
    fmt,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use crate::{
    lexer::Token,
    smallvec,
    span::{PossiblySpanning, SourceFile, Span, Spanned, Spanning},
    support::InvalidFallback,
    Atom, SmallVec,
};
use freestanding::freestanding;

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
            Module(_) => Targets::MODULE,
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
#[freestanding]
#[streamline(Box)]
#[derive(Debug)]
pub enum DeclarationKind {
    /// The syntax node of a value declaration.
    Value {
        binder: Identifier,
        parameters: Parameters,
        type_annotation: Option<Expression>,
        expression: Option<Expression>,
    },
    /// The syntax node of a data declaration.
    Data {
        binder: Identifier,
        parameters: Parameters,
        type_annotation: Option<Expression>,
        constructors: Option<Vec<Declaration>>,
    },
    /// The syntax node of a constructor.
    Constructor {
        binder: Identifier,
        parameters: Parameters,
        type_annotation: Option<Expression>,
    },
    /// The syntax node of a module declaration.
    Module {
        binder: Identifier,
        file: Rc<SourceFile>,
        // @Task support for constructor (and field) exposures (paths + multipaths)
        exposures: Vec<Identifier>,
        declarations: Option<Vec<Declaration>>,
    },
    /// The syntax node of a use declaration.
    Use { bindings: UseBindings },
}

// @Task documentation, span information
#[derive(Debug)]
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

#[derive(Debug, Default, Clone)]
pub struct Attributes {
    values: Vec<Attribute>,
}

impl Attributes {
    pub fn has(&self, query: AttributeKind) -> bool {
        self.get(query).is_some()
    }

    pub fn get(&self, query: AttributeKind) -> Option<&Attribute> {
        self.iter().find(|attribute| attribute.kind == query)
    }

    pub fn nonconforming(&self, targets: Targets) -> Vec<&Attribute> {
        self.iter()
            .filter(|attribute| !attribute.targets().contains(targets))
            .collect()
    }
}

impl Deref for Attributes {
    type Target = Vec<Attribute>;

    fn deref(&self) -> &Self::Target {
        &self.values
    }
}

impl DerefMut for Attributes {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.values
    }
}

pub type Attribute = Spanned<AttributeKind>;

impl Attribute {
    pub fn targets(&self) -> Targets {
        use AttributeKind::*;

        match self.kind {
            Documentation => Targets::all(),
            Foreign => Targets::VALUE | Targets::DATA,
            Inherent => Targets::DATA,
        }
    }

    pub fn unique(&self) -> bool {
        use AttributeKind::*;

        match self.kind {
            Documentation => false,
            Foreign | Inherent => true,
        }
    }
}

// @Task add `include`, `if`, `deprecated`, `unstable`, `unsafe`, `open`, … in the future
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttributeKind {
    Documentation,
    Foreign,
    /// Make bindings available for FFI. Opposite of `foreign`.
    Inherent,
}

impl fmt::Display for AttributeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Documentation => "a documentation comment",
            Self::Foreign => "attribute `foreign`",
            Self::Inherent => "attribute `inherent`",
        })
    }
}

bitflags::bitflags! {
    pub struct Targets: u8 {
        const VALUE = 0b0000_0001;
        const DATA = 0b0000_0010;
        const CONSTRUCTOR = 0b0000_0100;
        const MODULE = 0b0000_1000;
        const USE = 0b0001_0000;
    }
}

pub type Expression = Spanned<ExpressionKind>;

/// The syntax node of an expression.
#[freestanding]
#[streamline(Box)]
#[derive(Debug, Clone)]
pub enum ExpressionKind {
    /// The syntax node of pi-type literals.
    PiTypeLiteral {
        binder: Option<Identifier>,
        parameter: Expression,
        expression: Expression,
        explicitness: Explicitness,
    },
    /// The syntax node of function application.
    Application {
        callee: Expression,
        argument: Expression,
        explicitness: Explicitness,
    },
    TypeLiteral,
    NatLiteral {
        value: crate::Nat,
    },
    TextLiteral {
        value: String,
    },
    Path {
        head: Option<Head>,
        segments: SmallVec<[Identifier; 1]>,
    },
    /// The syntax node of a lambda literal expression.
    LambdaLiteral {
        parameters: Parameters,
        body_type_annotation: Option<Expression>,
        body: Expression,
    },
    /// The syntax-node of a let-in expression.
    LetIn {
        binder: Identifier,
        parameters: Parameters,
        type_annotation: Option<Expression>,
        // @Task improve upon naming
        expression: Expression,
        scope: Expression,
    },
    UseIn,
    CaseAnalysis {
        expression: Expression,
        cases: Vec<Case>,
    },
    /// See documentation on [crate::hir::Expression::Invalid].
    Invalid,
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
    pub parameters: crate::SmallVec<[Identifier; 1]>,
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

#[freestanding]
#[streamline(Box)]
#[derive(Debug, Clone)]
pub enum PatternKind {
    #[skip]
    NatLiteral(NatLiteral),
    #[skip]
    TextLiteral(TextLiteral),
    #[skip]
    Path(Path),
    Binder {
        binder: Identifier,
    },
    Deapplication {
        callee: Pattern,
        argument: Pattern,
    },
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

#[derive(Debug, Clone, Eq)]
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

    // @Beacon @Beacon @Task proper error handling, disallow super and crate here
    pub fn join(mut self, other: Self) -> Self {
        debug_assert!(other.head.is_none() || other.is_self());
        self.segments.extend(other.segments);
        self
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
    }
}

pub macro pat {
    ($kind:ident[$span:expr] { $( $body:tt )+ }) => {
        Pattern::new(
            $span,
            PatternKind::$kind(Box::new(self::$kind { $( $body )+ })),
        )
    }
}
