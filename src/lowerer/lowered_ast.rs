//! The lowered AST.

use crate::{
    ast::{self, Explicitness, Identifier, Path},
    diagnostic::{Diagnostic, Results},
    lexer::Number,
    span::{SourceFile, Spanned},
    support::{InvalidFallback, ManyErrExt},
};
use std::{convert::TryInto, fmt, rc::Rc};

pub type Item<Kind> = crate::item::Item<Kind, Attributes>;

pub type Declaration = Item<DeclarationKind>;

pub enum DeclarationKind {
    Value(Box<Value>),
    Data(Box<Data>),
    Constructor(Box<Constructor>),
    Module(Box<Module>),
    Use(Box<Use>),
    Invalid,
}

impl InvalidFallback for DeclarationKind {
    fn invalid() -> Self {
        Self::Invalid
    }
}

pub struct Value {
    pub binder: Identifier,
    pub type_annotation: Expression,
    pub expression: Option<Expression>,
}

pub struct Data {
    pub binder: Identifier,
    pub type_annotation: Expression,
    pub constructors: Option<Vec<Declaration>>,
}

pub struct Constructor {
    pub binder: Identifier,
    pub type_annotation: Expression,
}

pub struct Module {
    pub binder: Identifier,
    pub file: Rc<SourceFile>,
    pub declarations: Vec<Declaration>,
}

pub struct Use {
    pub binder: Option<Identifier>,
    pub target: Path,
}

pub type Expression = Item<ExpressionKind>;

#[derive(Clone)]
pub enum ExpressionKind {
    PiType(Rc<PiType>),
    Application(Rc<Application>),
    Type,
    Number(Rc<Number>),
    Text(Rc<String>),
    Binding(Rc<Binding>),
    Lambda(Rc<Lambda>),
    UseIn,
    CaseAnalysis(Rc<CaseAnalysis>),
    Invalid,
}

impl InvalidFallback for ExpressionKind {
    fn invalid() -> Self {
        Self::Invalid
    }
}

#[derive(Clone)]
pub struct PiType {
    pub parameter: Option<Identifier>,
    pub domain: Expression,
    pub codomain: Expression,
    pub explicitness: Explicitness,
}

#[derive(Clone)]
pub struct Application {
    pub callee: Expression,
    pub argument: Expression,
    pub explicitness: Explicitness,
}

#[derive(Clone)]
pub struct Binding {
    pub binder: Path,
}

#[derive(Clone)]
pub struct Lambda {
    pub parameter: Identifier,
    pub parameter_type_annotation: Option<Expression>,
    pub explicitness: Explicitness,
    pub body_type_annotation: Option<Expression>,
    pub body: Expression,
}

#[derive(Clone)]
pub struct CaseAnalysis {
    pub subject: Expression,
    pub cases: Vec<Case>,
}

#[derive(Clone)]
pub struct Case {
    pub pattern: Pattern,
    pub body: Expression,
}

pub type Pattern = Item<PatternKind>;

#[derive(Clone)]
pub enum PatternKind {
    Number(Rc<Number>),
    Text(Rc<String>),
    Binding(Rc<Binding>),
    Binder(Rc<Binder>),
    Deapplication(Rc<Deapplication>),
    Invalid,
}

impl InvalidFallback for PatternKind {
    fn invalid() -> Self {
        Self::Invalid
    }
}

/// A binder inside of a pattern.
#[derive(Clone)]
pub struct Binder {
    pub binder: Identifier,
}

#[derive(Clone)]
pub struct Deapplication {
    pub callee: Pattern,
    pub argument: Pattern,
}

#[derive(Clone, Default)]
pub struct Attributes {
    pub(super) data: Box<[Attribute]>,
    pub(super) keys: AttributeKeys,
}

impl Attributes {
    pub fn has(&self, keys: AttributeKeys) -> bool {
        self.keys.contains(keys)
    }

    pub fn within(&self, keys: AttributeKeys) -> bool {
        keys.contains(self.keys)
    }

    pub fn get(&self, keys: AttributeKeys) -> impl DoubleEndedIterator<Item = &Attribute> + Clone {
        self.data
            .iter()
            .filter(move |attribute| attribute.matches(keys))
    }
}

impl InvalidFallback for Attributes {
    fn invalid() -> Self {
        Self::default()
    }
}

pub type Attribute = Spanned<AttributeKind>;

impl Attribute {
    pub fn parse(attribute: &ast::Attribute) -> Results<Self> {
        Ok(Attribute::new(
            attribute.span,
            AttributeKind::parse(attribute)
                .map_err(Into::into) // @Temporary cuz the Error ty
                .many_err()?,
        ))
    }

    pub fn matches(&self, keys: AttributeKeys) -> bool {
        keys.contains(self.kind.key())
    }
}

// @Question should we provide a bitset of nullary attributes, next to a Vec<Attribute>?
// (or instead of)
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum AttributeKind {
    /// Form: `allow <0:lint:Path>`
    Allow {
        lint: Lint,
    },
    /// Form: `deny <0:lint:Path>`
    Deny {
        lint: Lint,
    },
    /// Form: `deprecated <0:reason:Text-Literal> [<since:Version>] [<replacement:String>]`
    Deprecated {
        reason: String,
        since: Option<Version>,
        replacement: Option<String>,
    },
    // @Note only maybe String
    Documentation {
        content: String,
    },
    /// Form: `forbid <0:lint:Path>`
    Forbid {
        lint: Lint,
    },
    Foreign,
    If {
        condition: Condition,
    },
    Ignore,
    Include,
    Inherent,
    Int,
    Int32,
    Int64,
    List,
    Moving,
    Nat,
    Nat32,
    Nat64,
    Opaque,
    /// Form: `public [<0:scope:Path>]`
    // @Question rename `scope` to `up-to`?
    Public {
        scope: Option<ast::Path>,
    },
    /// Form: `recursion-limit <0:depth:Number-Literal>`
    RecursionLimit {
        depth: usize,
    },
    Rune,
    Shallow,
    Test,
    Text,
    Unsafe,
    /// Form: `unstable <feature:Path> <reason:Text-Literal>`
    Unstable {
        feature: Feature,
        reason: String,
    },
    Vector,
    /// Form: `warn <0:lint:Path>`
    Warn {
        lint: Lint,
    },
}

impl AttributeKind {
    // @Question move to lowerer?
    pub fn parse(attribute: &ast::Attribute) -> Result<Self, Error> {
        Ok(match attribute.binder.as_str() {
            "allow" => Self::Allow {
                lint: Lint::parse(&attribute.arguments)?,
            },
            "deny" => Self::Deny {
                lint: Lint::parse(&attribute.arguments)?,
            },
            // @Bug @Temporary
            "deprecated" => Self::Deprecated {
                reason: String::from("xxx"),
                since: None,
                replacement: None,
            },
            "documentation" => Self::Documentation {
                // @Beacon @Temporary @Bug
                content: String::new(),
            },
            "forbid" => Self::Forbid {
                lint: Lint::parse(&attribute.arguments)?,
            },
            "foreign" => Self::Foreign,
            "if" => todo!(),
            "ignore" => Self::Ignore,
            "include" => Self::Include,
            "inherent" => Self::Inherent,
            "Int" => Self::Int,
            "Int32" => Self::Int32,
            "Int64" => Self::Int64,
            "List" => Self::List,
            "moving" => Self::Moving,
            "Nat" => Self::Nat,
            "Nat32" => Self::Nat32,
            "Nat64" => Self::Nat64,
            "opaque" => Self::Opaque,
            "public" => Self::Public {
                scope: match &*attribute.arguments {
                    [] => None,
                    [ast::AttributeArgument::Path(path)] => Some(path.as_ref().clone()),
                    [ast::AttributeArgument::Named(named)] => match &**named {
                        ast::NamedAttributeArgument { binder, value }
                            if binder.as_str() == "scope" =>
                        {
                            match value {
                                ast::AttributeArgument::Path(path) => Some(path.as_ref().clone()),
                                _ => return Err(Error::InvalidArgument),
                            }
                        }
                        _ => return Err(Error::InvalidArgument),
                    },
                    [_] => return Err(Error::InvalidArgument),
                    _ => return Err(Error::TooManyArguments),
                },
            },
            // @Task create a high-level API for this
            "recursion-limit" => {
                #[allow(unreachable_code)]
                Self::RecursionLimit {
                    depth: match &*attribute.arguments {
                        // @Task say which ones are missing
                        [] => return Err(Error::TooFewArguments),
                        [ast::AttributeArgument::NumberLiteral(_number)] => todo!(),
                        [ast::AttributeArgument::Named(named)] => match &**named {
                            ast::NamedAttributeArgument { binder, value }
                                if binder.as_str() == "depth" =>
                            {
                                match value {
                                    ast::AttributeArgument::NumberLiteral(number) => {
                                        match &**number {
                                            Number::Nat(number) => number
                                                .try_into()
                                                .map_err(|_| Error::InvalidArgument)?,
                                            _ => return Err(Error::InvalidArgument),
                                        }
                                    }
                                    _ => return Err(Error::InvalidArgument),
                                }
                            }
                            _ => return Err(Error::InvalidArgument),
                        },
                        [_] => return Err(Error::InvalidArgument),
                        _ => return Err(Error::TooManyArguments),
                    },
                }
            }
            "Rune" => Self::Rune,
            "shallow" => Self::Shallow,
            "test" => Self::Test,
            "Text" => Self::Text,
            "unsafe" => Self::Unsafe,
            // @Bug @Temporary
            "unstable" => Self::Unstable {
                feature: Feature::Dummy,
                reason: String::new(),
            },
            "Vector" => Self::Vector,
            "warn" => Self::Warn {
                lint: Lint::parse(&attribute.arguments)?,
            },
            _binder => return Err(Error::UndefinedAttribute), // @Temporary
        })
    }

    // @Question is outputting "attribute `documentation`" bad output for documentation comments?
    pub const fn quoted_name(&self) -> &'static str {
        macro quoted($name:literal) {
            concat!("`", $name, "`")
        }

        match self {
            Self::Allow { .. } => quoted!("allow"),
            Self::Deny { .. } => quoted!("deny"),
            Self::Deprecated { .. } => quoted!("deprecated"),
            Self::Documentation { .. } => quoted!("documentation"),
            Self::Forbid { .. } => quoted!("forbid"),
            Self::Foreign => quoted!("foreign"),
            Self::If { .. } => quoted!("if"),
            Self::Ignore => quoted!("ignore"),
            Self::Include => quoted!("include"),
            Self::Inherent => quoted!("inherent"),
            Self::Int => quoted!("Int"),
            Self::Int32 => quoted!("Int32"),
            Self::Int64 => quoted!("Int64"),
            Self::List => quoted!("List"),
            Self::Moving => quoted!("moving"),
            Self::Nat => quoted!("Nat"),
            Self::Nat32 => quoted!("Nat32"),
            Self::Nat64 => quoted!("Nat64"),
            Self::Opaque => quoted!("opaque"),
            Self::Public { .. } => quoted!("public"),
            Self::RecursionLimit { .. } => quoted!("recursion-limit"),
            Self::Rune => quoted!("Rune"),
            Self::Shallow => quoted!("shallow"),
            Self::Test => quoted!("test"),
            Self::Text => quoted!("Text"),
            Self::Unsafe => quoted!("unsafe"),
            Self::Unstable { .. } => quoted!("unstable"),
            Self::Vector => quoted!("Vector"),
            Self::Warn { .. } => quoted!("warn"),
        }
    }

    // keep this in sync with Self::targets_as_str
    pub fn targets(&self) -> ast::AttributeTargets {
        use ast::AttributeTargets as Targets;
        use AttributeKind::*;

        match self {
            Allow { .. } | Deny { .. } | Forbid { .. } | Warn { .. } => Targets::all(),
            Deprecated { .. } | Documentation { .. } | If { .. } | Ignore | Unstable { .. } => {
                Targets::DECLARATION
            }
            Foreign => Targets::VALUE_DECLARATION | Targets::DATA_DECLARATION,
            Include | Rune | Text => Targets::TEXT_LITERAL,
            Inherent | Moving | Shallow | Opaque => Targets::DATA_DECLARATION,
            Int | Int32 | Int64 | Nat | Nat32 | Nat64 => Targets::NUMBER_LITERAL,
            List | Vector => Targets::SEQUENCE_LITERAL,
            // @Task but smh add extra diagnostic note saying they are public automatically
            Public { .. } => Targets::DECLARATION - Targets::CONSTRUCTOR_DECLARATION,
            RecursionLimit { .. } => Targets::MODULE_DECLARATION,
            Test => Targets::VALUE_DECLARATION | Targets::MODULE_DECLARATION,
            Unsafe => {
                Targets::VALUE_DECLARATION
                    | Targets::CONSTRUCTOR_DECLARATION
                    | Targets::EXPRESSION
                    | Targets::PATTERN
            }
        }
    }

    // keep this in sync with Self::targets!
    // @Task find a way to get around this manual work
    pub fn targets_as_str(&self) -> &'static str {
        use AttributeKind::*;

        match self {
            Allow { .. } | Deny { .. } | Forbid { .. } | Warn { .. } => "anything",
            Deprecated { .. } | Documentation { .. } | If { .. } | Ignore | Unstable { .. } => {
                "declarations"
            }
            Foreign => "value or data declarations",
            Include | Rune | Text => "text literals",
            Inherent | Moving | Shallow | Opaque => "data declarations",
            Int | Int32 | Int64 | Nat | Nat32 | Nat64 => "number literals",
            List | Vector => "sequence literals",
            Public { .. } => "any declaration except constructor ones",
            RecursionLimit { .. } => "module declarations",
            Test => "value or module declarations",
            Unsafe => "value or constructor declarations, expressions or patterns",
        }
    }

    pub fn key(&self) -> AttributeKeys {
        match self {
            Self::Allow { .. } => AttributeKeys::ALLOW,
            Self::Deny { .. } => AttributeKeys::DENY,
            Self::Deprecated { .. } => AttributeKeys::DEPRECATED,
            Self::Documentation { .. } => AttributeKeys::DOCUMENTATION,
            Self::Forbid { .. } => AttributeKeys::FORBID,
            Self::Foreign => AttributeKeys::FOREIGN,
            Self::If { .. } => AttributeKeys::IF,
            Self::Ignore => AttributeKeys::IGNORE,
            Self::Include => AttributeKeys::INCLUDE,
            Self::Inherent => AttributeKeys::INHERENT,
            Self::Int => AttributeKeys::INT,
            Self::Int32 => AttributeKeys::INT32,
            Self::Int64 => AttributeKeys::INT64,
            Self::List => AttributeKeys::LIST,
            Self::Moving => AttributeKeys::MOVING,
            Self::Nat => AttributeKeys::NAT,
            Self::Nat32 => AttributeKeys::NAT32,
            Self::Nat64 => AttributeKeys::NAT64,
            Self::Opaque => AttributeKeys::OPAQUE,
            Self::Public { .. } => AttributeKeys::PUBLIC,
            Self::RecursionLimit { .. } => AttributeKeys::RECURSION_LIMIT,
            Self::Rune => AttributeKeys::RUNE,
            Self::Shallow => AttributeKeys::SHALLOW,
            Self::Test => AttributeKeys::TEST,
            Self::Text => AttributeKeys::TEXT,
            Self::Unsafe => AttributeKeys::UNSAFE,
            Self::Unstable { .. } => AttributeKeys::UNSTABLE,
            Self::Vector => AttributeKeys::VECTOR,
            Self::Warn { .. } => AttributeKeys::WARN,
        }
    }
}

bitflags::bitflags! {
    pub struct AttributeKeys: u64 {
        const ALLOW = 1 << 0;
        const DENY = 1 << 1;
        const DEPRECATED = 1 << 2;
        const DOCUMENTATION = 1 << 3;
        const FORBID = 1 << 4;
        const FOREIGN = 1 << 5;
        const IF = 1 << 6;
        const IGNORE = 1 << 7;
        const INCLUDE = 1 << 8;
        const INHERENT = 1 << 9;
        const INT = 1 << 10;
        const INT32 = 1 << 11;
        const INT64 = 1 << 12;
        const LIST = 1 << 13;
        const MOVING = 1 << 14;
        const NAT = 1 << 15;
        const NAT32 = 1 << 16;
        const NAT64 = 1 << 17;
        const OPAQUE = 1 << 18;
        const PUBLIC = 1 << 19;
        const RECURSION_LIMIT = 1 << 20;
        const RUNE = 1 << 21;
        const SHALLOW = 1 << 22;
        const TEST = 1 << 23;
        const TEXT = 1 << 24;
        const UNSAFE = 1 << 25;
        const UNSTABLE = 1 << 26;
        const VECTOR = 1 << 27;
        const WARN = 1 << 28;

        const COEXISTABLE = Self::ALLOW.bits
            | Self::DENY.bits
            | Self::DOCUMENTATION.bits
            | Self::FORBID.bits
            | Self::WARN.bits;
    }
}

impl Default for AttributeKeys {
    fn default() -> Self {
        Self::empty()
    }
}

// @Task payloads
pub enum Error {
    UndefinedAttribute,
    UndefinedLint,
    TooFewArguments,
    InvalidArgument,
    TooManyArguments,
}

// @Beacon @Task more information, esp. span information
impl From<Error> for Diagnostic {
    fn from(error: Error) -> Self {
        let message = match error {
            Error::UndefinedAttribute => "undefined attribute",
            Error::UndefinedLint => "undefined lint",
            Error::TooFewArguments => "too few arguments in attribute",
            Error::InvalidArgument => "invalid arguments in attribute",
            Error::TooManyArguments => "too many arguments in attribute",
        };
        Diagnostic::error().with_message(message)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Lint {}

impl Lint {
    fn parse(_arguments: &[ast::AttributeArgument]) -> Result<Self, Error> {
        Err(Error::UndefinedLint)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Version;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Condition;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Feature {
    Dummy,
}

// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled
// @Task display attributes
impl Declaration {
    fn format_with_depth(&self, depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use crate::INDENTATION_IN_SPACES;
        use DeclarationKind::*;

        match &self.kind {
            Value(declaration) => {
                write!(f, "{}: {}", declaration.binder, declaration.type_annotation)?;
                if let Some(expression) = &declaration.expression {
                    write!(f, " = {}", expression)?;
                }
                writeln!(f)
            }
            Data(declaration) => match &declaration.constructors {
                Some(constructors) => {
                    writeln!(
                        f,
                        "data {}: {} =",
                        declaration.binder, declaration.type_annotation
                    )?;
                    for constructor in constructors {
                        let depth = depth + 1;
                        write!(
                            f,
                            "{}{}",
                            " ".repeat(depth * INDENTATION_IN_SPACES),
                            constructor
                        )?;
                    }
                    Ok(())
                }
                None => writeln!(
                    f,
                    "data {}: {}",
                    declaration.binder, declaration.type_annotation
                ),
            },
            Constructor(constructor) => {
                writeln!(f, "{}: {}", constructor.binder, constructor.type_annotation)
            }
            Module(declaration) => {
                writeln!(f, "module {}: =", declaration.binder)?;
                for declaration in &declaration.declarations {
                    let depth = depth + 1;
                    write!(f, "{}", " ".repeat(depth * INDENTATION_IN_SPACES))?;
                    declaration.format_with_depth(depth, f)?;
                }
                Ok(())
            }
            Use(declaration) => match &declaration.binder {
                Some(binder) => writeln!(f, "use {} as {}", declaration.target, binder),
                None => writeln!(f, "use {}", declaration.target),
            },
            Invalid => writeln!(f, "<invalid>"),
        }
    }
}

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.format_with_depth(0, f)
    }
}

// @Task display fewer round brackets by making use of precedence
// @Note many wasted allocations (intermediate Strings)
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ExpressionKind::*;

        match &self.kind {
            PiType(literal) => write!(f, "{}", literal),
            Application(application) => {
                write!(f, "{} ", application.callee.wrap())?;
                if application.explicitness.is_implicit() {
                    write!(f, "({}{})", application.explicitness, application.argument)
                } else {
                    write!(f, "{}", application.argument.wrap())
                }
            }
            Type => write!(f, "Type"),
            Number(literal) => write!(f, "{}", literal),
            Text(literal) => write!(f, "{:?}", literal),
            Binding(binding) => write!(f, "{}", binding.binder),
            Lambda(lambda) => write!(f, "{}", lambda),
            UseIn => todo!(),
            // @Task fix indentation
            CaseAnalysis(analysis) => {
                writeln!(f, "case {} of", analysis.subject)?;
                for case in &analysis.cases {
                    write!(f, "{}", case)?;
                }
                Ok(())
            }
            Invalid => write!(f, "<invalid>"),
        }
    }
}

impl fmt::Display for PiType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(parameter) = &self.parameter {
            write!(f, "({}{}: {})", self.explicitness, parameter, self.domain)?;
        } else if self.explicitness.is_implicit() {
            write!(f, "({}{})", self.explicitness, self.domain)?;
        } else {
            write!(f, "{}", self.domain.wrap())?;
        }
        write!(f, " -> {}", self.codomain.wrap())
    }
}

impl fmt::Display for Lambda {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\\({}{}", self.explicitness, self.parameter)?;
        if let Some(annotation) = &self.parameter_type_annotation {
            write!(f, ": {}", annotation.wrap())?;
        }
        write!(f, ")")?;
        if let Some(annotation) = &self.body_type_annotation {
            write!(f, ": {}", annotation.wrap())?;
        }
        write!(f, " => {}", self.body.wrap())
    }
}

impl fmt::Display for Case {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{} => {}", self.pattern, self.body)
    }
}

// @Task update bracket business
impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use PatternKind::*;

        match &self.kind {
            Number(number) => write!(f, "{}", number),
            Text(text) => write!(f, "{:?}", text),
            Binding(binding) => write!(f, "{}", binding.binder),
            Binder(binder) => write!(f, "\\{}", binder.binder),
            Deapplication(application) => {
                write!(f, "({}) ({})", application.callee, application.argument)
            }
            Invalid => write!(f, "<invalid>"),
        }
    }
}

trait WrapExpression {
    fn wrap<'a>(&'a self) -> PossiblyWrapped<'a>;
}

impl WrapExpression for Expression {
    fn wrap<'a>(&'a self) -> PossiblyWrapped<'a> {
        PossiblyWrapped(self)
    }
}

struct PossiblyWrapped<'a>(&'a Expression);

impl PossiblyWrapped<'_> {
    fn needs_brackets_conservative(&self) -> bool {
        use ExpressionKind::*;

        !matches!(&self.0.kind, Type | Number(_) | Text(_) | Binding(_) | Invalid)
    }
}

impl fmt::Display for PossiblyWrapped<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.needs_brackets_conservative() {
            write!(f, "({})", self.0)
        } else {
            write!(f, "{}", self.0)
        }
    }
}

pub macro decl($( $tree:tt )+) {
    crate::item::item!(crate::lowered_ast, DeclarationKind, Box; $( $tree )+)
}

pub macro expr($( $tree:tt )+) {
    crate::item::item!(crate::lowered_ast, ExpressionKind, Rc; $( $tree )+)
}

pub macro pat($( $tree:tt )+) {
    crate::item::item!(crate::lowered_ast, PatternKind, Rc; $( $tree )+)
}
