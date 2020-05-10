use std::{
    fmt,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use crate::span::{SourceFile, Span, Spanned};
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

/// The syntax node of a declaration.
#[freestanding]
#[streamline(Box)]
#[derive(Debug)]
pub enum DeclarationKind {
    /// The syntax node of a value declaration.
    Value {
        binder: Identifier,
        parameters: AnnotatedParameters,
        type_annotation: Expression,
        expression: Option<Expression>,
    },
    /// The syntax node of a data declaration.
    Data {
        binder: Identifier,
        parameters: AnnotatedParameters,
        type_annotation: Expression,
        constructors: Option<Vec<Declaration>>,
    },
    /// The syntax node of a constructor.
    Constructor {
        binder: Identifier,
        parameters: AnnotatedParameters,
        type_annotation: Expression,
    },
    /// The syntax node of a module declaration.
    Module {
        binder: Identifier,
        file: Rc<SourceFile>,
        declarations: Option<Vec<Declaration>>,
    },
    /// The syntax node of a use declaration.
    // @Task
    Use { path: Path, bindings: () },
}

pub type AnnotatedParameters = Vec<AnnotatedParameterGroup>;

#[derive(Debug)]
pub struct AnnotatedParameterGroup {
    pub parameters: crate::SmallVec<[Identifier; 1]>,
    pub type_annotation: Expression,
    pub explicitness: Explicitness,
}

#[derive(Debug, Default)]
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
        head: Option<PathHead>,
        segments: Vec<Identifier>,
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
        cases: Vec<CaseAnalysisCaseGroup>,
    },
}

/// Expression where ExpressionKind is unboxed.
pub type RawExpression<K> = Spanned<K>;

#[derive(Debug, Clone)]
pub struct CaseAnalysisCaseGroup {
    pub patterns: Vec<Pattern>,
    pub expression: Expression,
}

pub type Parameters = Vec<ParameterGroup>;

#[derive(Debug, Clone)]
pub struct ParameterGroup {
    pub parameters: crate::SmallVec<[Identifier; 1]>,
    pub type_annotation: Option<Expression>,
    pub explicitness: Explicitness,
}

pub type Pattern = Spanned<PatternKind>;

// @Task add text literal pattern
// @Note unfortunately, there is much duplication going on of things from Expression
// currently, the separation is used for type safety and documentation as we theoretically
// could use Expression::{NatLiteral, Path, Application}. In fact, there are no pattern-
// exclusive constructs right now. that might all change, especially if we syntactically
// differenciate "l-values" from "r-values" in patterns (well, ... type annotation's are
// exclusive)
#[freestanding]
#[streamline(Box)]
#[derive(Debug, Clone)]
pub enum PatternKind {
    NatLiteralPattern {
        value: crate::Nat,
    },
    PathPattern {
        head: Option<PathHead>,
        segments: Vec<Identifier>,
        type_annotation: Option<Expression>,
    },
    ApplicationPattern {
        callee: Pattern,
        argument: Pattern,
    },
}

pub type PathHead = Spanned<PathHeadKind>;

// @Note this is not well designed at all:
// so much rigmarole. if we were to use identifiers plus string comparison
// with `"super"` etc, life would be simpler (but not type-safe i know!!)
#[derive(Debug, Clone)]
pub enum PathHeadKind {
    Crate,
    Super,
}

#[derive(Debug, Clone, Eq)]
pub struct Identifier {
    atom: crate::Atom,
    pub span: Span,
}

impl Identifier {
    pub fn new(atom: crate::Atom, span: Span) -> Self {
        Self { atom, span }
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

// @Note the separation between Head and Segments is soo annoying
// and leads to convoluted code which would actually be really simple
// in a different format
impl Path {
    pub fn identifier_head(&self) -> Option<&Identifier> {
        if self.head.is_some() {
            return None;
        }

        Some(&self.segments[0])
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

// @Note we are required to implement `Display` for `Path` because
// the `hir::Binder` trait requires it. And we won't change that for now
// because it makes sense.
// Nonetheless, I think that we will never print paths via the parser's Path,
// only ever through ModuleScope methods in crate::resolver
// Solution: narrow the bound `Display` to functions processing hir::Expression<resolver::Identifier>
// @Temporary
impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_path(&self.head, &self.segments, f)
    }
}

impl fmt::Display for PathPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_path(&self.head, &self.segments, f)
    }
}

// @Temporary sorry. not in the mood for programming right now
// I will be happier once I figure out how to abstract over all of this
// nicely
fn display_path(
    head: &Option<PathHead>,
    segments: &[Identifier],
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    let mut segments = segments.iter();

    match head {
        Some(PathHead {
            kind: PathHeadKind::Crate,
            ..
        }) => f.write_str("crate")?,
        Some(PathHead {
            kind: PathHeadKind::Super,
            ..
        }) => f.write_str("super")?,
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

/// The explicitness of a parameter or argument.
///
/// In the context of parameters, this specifies whether in an application, the corresponding argument has
/// to be passed explicitly or should be infered, i.e. the parameter is [Explicitness::Implicit].
///
/// In the context of applications, [Explicitness::Implicit] means that the argument is passed explicitly
/// even though the parameter is marked implicit.
#[derive(Clone, Copy, Debug)]
pub enum Explicitness {
    Implicit,
    Explicit,
}

impl fmt::Display for Explicitness {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Implicit => f.write_str("|"),
            Self::Explicit => f.write_str(""),
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
        Expression {
            span: $span,
            kind: ExpressionKind::$kind(Box::new(self::$kind { $( $body )+ })),
        }
    },
    ($kind:ident[$span:expr]) => {
        Expression {
            span: $span,
            kind: ExpressionKind::$kind,
        }
    }
}

pub macro pat {
    ($kind:ident[$span:expr] { $( $body:tt )+ }) => {
        Pattern {
            span: $span,
            kind: PatternKind::$kind(Box::new(self::$kind { $( $body )+ })),
        }
    }
}
