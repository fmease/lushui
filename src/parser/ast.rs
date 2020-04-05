use std::ops::{Deref, DerefMut};

use super::*;

pub struct Declaration {
    pub kind: DeclarationKind,
    pub span: Span,
    pub attributes: Attributes,
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
        constructors: Option<Vec<Constructor>>,
    },
    /// The syntax node of a module declaration.
    Module { declarations: Vec<Declaration> },
    /// The syntax node of a use declaration.
    Use,
}

/// The syntax node of a constructor.
#[derive(Debug)]
pub struct Constructor {
    pub binder: Identifier,
    pub parameters: AnnotatedParameters,
    pub type_annotation: Expression,
    pub span: Span,
    pub attributes: Attributes,
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
        self.iter()
            .find(|attribute| attribute.kind == query)
            .is_some()
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

// @Task move attribute logic into its own module Further, create a more principled approach
// using a `AttributeKind::target`-method and a function which validates against
// `DeclarationKind` and the like
pub type Attribute = Spanned<AttributeKind>;

// @Task add `include`, `if`, `deprecated`, `unstable`, `unsafe`, `open`, … in the future
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttributeKind {
    Documentation,
    Foreign,
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
