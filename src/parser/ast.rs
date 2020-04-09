use std::ops::{Deref, DerefMut};

use super::*;

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
            Use => Targets::USE,
        }
    }

    pub fn kind_as_str(&self) -> &'static str {
        use DeclarationKind::*;

        match &self.kind {
            Value(_) => "value",
            Data(_) => "data",
            Constructor(_) => "constructor",
            Module(_) => "module",
            Use => "use",
        }
    }
}

/// The syntax node of a declaration.
#[freestanding]
#[streamline(Box)]
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
    Module { declarations: Vec<Declaration> },
    /// The syntax node of a use declaration.
    Use,
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
    // @Task make it able to parse complex paths
    Path {
        // @Task in the future: segments: Vec<Identifier>,
        segments: Identifier,
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
#[freestanding]
#[streamline(Box)]
#[derive(Debug, Clone)]
pub enum PatternKind {
    NatLiteralPattern {
        value: crate::Nat,
    },
    PathPattern {
        segments: Identifier,
        type_annotation: Option<Expression>,
    },
    ApplicationPattern {
        callee: Pattern,
        argument: Pattern,
    },
}

#[derive(Debug, Clone, Eq)]
pub struct Identifier {
    pub atom: crate::Atom,
    pub span: Span,
}

impl Identifier {
    pub fn new(atom: crate::Atom, span: Span) -> Self {
        Self { atom, span }
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
        write!(f, "{}", self.atom)
    }
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
