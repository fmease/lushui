//! The lowered AST.

use crate::{
    ast::{
        self,
        Explicitness::{self, *},
        Identifier, Path,
    },
    diagnostics::Results,
    span::{SourceFile, Spanned},
    support::InvalidFallback,
};
use std::{fmt, rc::Rc};

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
    pub binder: Identifier,
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
    pub is_field: bool,
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
    pub fn new(attributes: Vec<Attribute>) -> Self {
        let mut keys = AttributeKeys::empty();

        for attribute in &attributes {
            keys |= attribute.kind.key();
        }

        Self {
            data: attributes.into_boxed_slice(),
            keys,
        }
    }

    pub fn has(&self, keys: AttributeKeys) -> bool {
        self.keys.contains(keys)
    }

    pub fn within(&self, keys: AttributeKeys) -> bool {
        keys.contains(self.keys)
    }

    // @Note @Beacon this API is flawed for attributes with arguments since we *redundantly* need to
    // match on `AttributeKind` again after already having specified `AttributeKeys`
    pub fn get(&self, keys: AttributeKeys) -> impl DoubleEndedIterator<Item = &Attribute> + Clone {
        self.data
            .iter()
            .filter(move |attribute| attribute.matches(keys))
    }

    pub fn iter(&self) -> impl Iterator<Item = &Attribute> {
        self.data.iter()
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
            AttributeKind::parse(attribute)?,
        ))
    }

    pub fn matches(&self, keys: AttributeKeys) -> bool {
        keys.contains(self.kind.key())
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum AttributeKind {
    /// Allow a [lint](Lint).
    ///
    /// ## Form
    ///
    /// ```text
    /// allow <0:lint:Path>
    /// ```
    Allow { lint: Lint },
    /// Deny a [lint](Lint).
    ///
    /// ## Form
    ///
    /// ```text
    /// deny <0:lint:Path>
    /// ```
    Deny { lint: Lint },
    /// Deprecate a binding providing a reason.
    ///
    /// … and optionally a [version](Version) marking the start of the deprecation,
    /// one to mark the (future) version where the binding is no longer going to be present
    /// in the public API.
    /// Lastly, a description on how one can replace the deprecated binding. Ideally, this
    /// should not be a text literal but something structured which IDEs etc. can read.
    ///
    /// ## Form
    ///
    /// ```text
    /// deprecated <0:reason:Text-Literal> [<since:Version>] [<replacement:Text-Literal>]
    /// ```
    Deprecated {
        reason: String,
        since: Option<Version>,
        until: Option<Version>,
        replacement: Option<String>,
    },
    /// Documentation of a binding.
    ///
    /// The format of text is not decided yet. I'd like to have something better than
    /// markdown. There are plenty of options.
    ///
    /// ## Form
    ///
    /// ```text
    /// documentation <0:content:Text-Literal>
    /// ```
    // @Task change to enum { (String), (Span) }
    Documentation { content: String },
    /// Forbid a [lint](Lint).
    ///
    /// ## Form
    ///
    /// ```text
    /// forbid <0:lint:Path>
    /// ```
    Forbid { lint: Lint },
    /// Mark a binding as having a definition outside of this language.
    Foreign,
    /// Make the inclusion of the attribute target dependent on a [condition](Condition).
    ///
    /// Aka conditional compilation.
    ///
    /// ## Form
    ///
    /// ```text
    /// if <0:condition:Condition>
    /// ```
    If { condition: Condition },
    /// Exclude the attribute target from further processing.
    ///
    /// Basically `@(if false)` but `if` won't be implemented anytime soon.
    Ignore,
    /// Statically include the contents of file given by path.
    Include,
    /// Declare a binding Rust FFI compatible for the TWI.
    ///
    /// This is an implementation detail right now and will likely removed
    /// once our FFI story grows stronger.
    Inherent,
    /// Specify the concrete type of a number literal to be `Int`.
    Int,
    /// Specify the concrete type of a number literal to be `Int32`.
    Int32,
    /// Specify the concrete type of a number literal to be `Int64`.
    Int64,
    /// Specify the concrete type of a sequence literal to be `List`.
    List,
    /// Change the path where the external module resides.
    ///
    /// ## Form
    ///
    /// ```text
    /// location <0:path:Text-Literal>
    /// ```
    Location { path: String },
    /// Mark a data type binding to be likely expanded in the number of constructors.
    Moving,
    /// Specify the concrete type of a number literal to be `Nat`.
    Nat,
    /// Specify the concrete type of a number literal to be `Nat32`.
    Nat32,
    /// Specify the concrete type of a number literal to be `Nat64`.
    Nat64,
    /// Hide the constructors/implementation details of a (public) data type binding.
    Opaque,
    /// Make the binding part of the public API or at least visible in modules higher up.
    ///
    /// If no `scope` is given, the binding is marked as exposed to any other crates.
    ///
    /// ## Form
    ///
    /// ```text
    /// public [<0:scope:Path>]
    ///
    // @Question rename `scope` to `up-to`?
    Public { scope: Option<ast::Path> },
    /// Define the recursion limit of the TWI.
    ///
    /// ## Form
    ///
    /// ```text
    /// recursion-limit <0:depth:Number-Literal>
    /// ```
    // @Task define allowed range
    RecursionLimit { depth: u32 },
    /// Specify the concrete type of a text literal to be `Rune`.
    Rune,
    /// Force an expression to be evaluated at compile-time.
    Static,
    /// Mark a function as a unit test.
    Test,
    /// Specify the concrete type of a text literal to be `Text`.
    Text,
    /// Mark a binding or expression as "unsafe".
    Unsafe,
    /// Mark a binding as an unstable part of the public API.
    ///
    /// ## Form
    ///
    /// ```text
    /// unstable <feature:Path> <reason:Text-Literal>
    /// ```
    Unstable { feature: Feature, reason: String },
    /// Specify the concrete type of a sequence literal to be `Vector`.
    Vector,
    /// Warn on a [lint](Lint).
    ///
    /// ## Form
    ///
    /// ```text
    /// warn <0:lint:Path>
    /// ```
    Warn { lint: Lint },
}

impl AttributeKind {
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
            Self::Location { .. } => quoted!("location"),
            Self::Moving => quoted!("moving"),
            Self::Nat => quoted!("Nat"),
            Self::Nat32 => quoted!("Nat32"),
            Self::Nat64 => quoted!("Nat64"),
            Self::Opaque => quoted!("opaque"),
            Self::Public { .. } => quoted!("public"),
            Self::RecursionLimit { .. } => quoted!("recursion-limit"),
            Self::Rune => quoted!("Rune"),
            Self::Static => quoted!("static"),
            Self::Test => quoted!("test"),
            Self::Text => quoted!("Text"),
            Self::Unsafe => quoted!("unsafe"),
            Self::Unstable { .. } => quoted!("unstable"),
            Self::Vector => quoted!("Vector"),
            Self::Warn { .. } => quoted!("warn"),
        }
    }

    // keep this in sync with Self::target_names
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
            Inherent | Moving | Opaque => Targets::DATA_DECLARATION,
            Int | Int32 | Int64 | Nat | Nat32 | Nat64 => Targets::NUMBER_LITERAL,
            List | Vector => Targets::SEQUENCE_LITERAL,
            // @Task but smh add extra diagnostic note saying they are public automatically
            Public { .. } => Targets::DECLARATION - Targets::CONSTRUCTOR_DECLARATION,
            Location { .. } | RecursionLimit { .. } => Targets::MODULE_DECLARATION,
            Test => Targets::VALUE_DECLARATION | Targets::MODULE_DECLARATION,
            Static => Targets::EXPRESSION,
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
    pub fn target_names(&self) -> &'static str {
        use AttributeKind::*;

        match self {
            Allow { .. } | Deny { .. } | Forbid { .. } | Warn { .. } => "anything",
            Deprecated { .. } | Documentation { .. } | If { .. } | Ignore | Unstable { .. } => {
                "declarations"
            }
            Foreign => "value or data declarations",
            Include | Rune | Text => "text literals",
            Inherent | Moving | Opaque => "data declarations",
            Int | Int32 | Int64 | Nat | Nat32 | Nat64 => "number literals",
            List | Vector => "sequence literals",
            Public { .. } => "declarations except constructors",
            Location { .. } | RecursionLimit { .. } => "module declarations",
            Test => "value or module declarations",
            Static => "expressions",
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
            Self::Location { .. } => AttributeKeys::LOCATION,
            Self::Moving => AttributeKeys::MOVING,
            Self::Nat => AttributeKeys::NAT,
            Self::Nat32 => AttributeKeys::NAT32,
            Self::Nat64 => AttributeKeys::NAT64,
            Self::Opaque => AttributeKeys::OPAQUE,
            Self::Public { .. } => AttributeKeys::PUBLIC,
            Self::RecursionLimit { .. } => AttributeKeys::RECURSION_LIMIT,
            Self::Rune => AttributeKeys::RUNE,
            Self::Static => AttributeKeys::STATIC,
            Self::Test => AttributeKeys::TEST,
            Self::Text => AttributeKeys::TEXT,
            Self::Unsafe => AttributeKeys::UNSAFE,
            Self::Unstable { .. } => AttributeKeys::UNSTABLE,
            Self::Vector => AttributeKeys::VECTOR,
            Self::Warn { .. } => AttributeKeys::WARN,
        }
    }
}

impl fmt::Display for AttributeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@")?;

        match self {
            Self::Allow { lint } => write!(f, "(allow {})", lint),
            Self::Deny { lint } => write!(f, "(deny {})", lint),
            Self::Deprecated {
                reason,
                since,
                until,
                replacement,
            } => write!(
                f,
                "(deprecated (reason {:?}) (since {:?}) (until {:?}) (replacement {:?}))",
                reason, since, until, replacement
            ),
            // Self::Documentation { content } => writeln!(f, ";{:?}", content),
            Self::Documentation { content } => write!(f, "(documentation {:?})", content),
            Self::Forbid { lint } => write!(f, "(forbid {})", lint),
            Self::Foreign => write!(f, "foreign"),
            Self::If { condition } => write!(f, "(if {})", condition),
            Self::Ignore => write!(f, "ignore"),
            Self::Include => write!(f, "include"),
            Self::Inherent => write!(f, "inherent"),
            Self::Int => write!(f, "Int"),
            Self::Int32 => write!(f, "Int32"),
            Self::Int64 => write!(f, "Int64"),
            Self::List => write!(f, "List"),
            Self::Location { path } => write!(f, "(location {})", path),
            Self::Moving => write!(f, "moving"),
            Self::Nat => write!(f, "Nat"),
            Self::Nat32 => write!(f, "Nat32"),
            Self::Nat64 => write!(f, "Nat64"),
            Self::Opaque => write!(f, "opaque"),
            Self::Public { scope } => match scope {
                Some(scope) => write!(f, "(scope {})", scope),
                None => write!(f, "public"),
            },
            Self::RecursionLimit { depth } => write!(f, "(recursion-limit {})", depth),
            Self::Rune => write!(f, "Rune"),
            Self::Static => write!(f, "static"),
            Self::Test => write!(f, "test"),
            Self::Text => write!(f, "Text"),
            Self::Unsafe => write!(f, "unsafe"),
            Self::Unstable { feature, reason } => write!(f, "(feature {} {:?})", feature, reason),
            Self::Vector => write!(f, "Vector"),
            Self::Warn { lint } => write!(f, "(warn {})", lint),
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
        const LOCATION = 1 << 14;
        const MOVING = 1 << 15;
        const NAT = 1 << 16;
        const NAT32 = 1 << 17;
        const NAT64 = 1 << 18;
        const OPAQUE = 1 << 19;
        const PUBLIC = 1 << 20;
        const RECURSION_LIMIT = 1 << 21;
        const RUNE = 1 << 22;
        const STATIC = 1 << 23;
        const TEST = 1 << 24;
        const TEXT = 1 << 25;
        const UNSAFE = 1 << 26;
        const UNSTABLE = 1 << 27;
        const VECTOR = 1 << 28;
        const WARN = 1 << 29;

        const COEXISTABLE = Self::ALLOW.bits
            | Self::DENY.bits
            | Self::DOCUMENTATION.bits
            | Self::FORBID.bits
            | Self::WARN.bits;

        const SUPPORTED = Self::DOCUMENTATION.bits
            | Self::FOREIGN.bits
            | Self::INHERENT.bits
            | Self::INT.bits
            | Self::INT32.bits
            | Self::INT64.bits
            | Self::LOCATION.bits
            | Self::NAT.bits
            | Self::NAT32.bits
            | Self::NAT64.bits;
        const UNSUPPORTED = !Self::SUPPORTED.bits;
    }
}

impl Default for AttributeKeys {
    fn default() -> Self {
        Self::empty()
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Lint {}

impl fmt::Display for Lint {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {}
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Version {}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Condition {}

impl fmt::Display for Condition {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {}
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Feature {}

impl fmt::Display for Feature {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {}
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Number {
    Nat(crate::Nat),
    Nat32(u32),
    Nat64(u64),
    Int(crate::Int),
    Int32(i32),
    Int64(i64),
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nat(value) => write!(f, "{}", value),
            Self::Nat32(value) => write!(f, "{}", value),
            Self::Nat64(value) => write!(f, "{}", value),
            Self::Int(value) => write!(f, "{}", value),
            Self::Int32(value) => write!(f, "{}", value),
            Self::Int64(value) => write!(f, "{}", value),
        }
    }
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
            Use(declaration) => writeln!(f, "use {} as {}", declaration.target, declaration.binder),
            Invalid => write!(f, "?(invalid)"),
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
                match application.explicitness {
                    Explicit => write!(f, "{}", application.argument.wrap()),
                    Implicit => write!(f, "({}{})", application.explicitness, application.argument),
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
            Invalid => write!(f, "?(invalid)"),
        }
    }
}

impl fmt::Display for PiType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let domain_needs_brackets =
            self.parameter.is_some() || self.explicitness == Implicit || self.is_field;

        if domain_needs_brackets {
            write!(f, "(")?;
        }

        write!(f, "{}", self.explicitness)?;

        if self.is_field {
            write!(f, "field ")?;
        }

        if let Some(parameter) = &self.parameter {
            write!(f, "{}: ", parameter)?;
        }

        if domain_needs_brackets {
            write!(f, "{}", self.domain)?;
        } else {
            write!(f, "{}", self.domain.wrap())?;
        }

        if domain_needs_brackets {
            write!(f, ")")?;
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
            Invalid => write!(f, "?(invalid)"),
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
