//! The lowered abstract syntax tree (lowered AST).

// @Task rename a bunch of syntax node to make them more like the ones of the AST
// i.e. not Number but NumberLiteral etc

mod format;

use super::ast::{self, Explicitness, Identifier, ParameterAspect, Path};
use crate::{
    diagnostics::{Code, Diagnostic, Reporter},
    error::{PossiblyErroneous, Result},
    span::{PossiblySpanning, SourceFileIndex, Span, Spanned, Spanning},
};
use std::rc::Rc;

pub type Item<Kind> = crate::item::Item<Kind, Attributes>;

pub type Declaration = Item<DeclarationKind>;

pub enum DeclarationKind {
    Value(Box<Value>),
    Data(Box<Data>),
    Constructor(Box<Constructor>),
    Module(Box<Module>),
    Use(Box<Use>),
    Error,
}

impl PossiblyErroneous for DeclarationKind {
    fn error() -> Self {
        Self::Error
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
    pub file: SourceFileIndex,
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
    Error,
}

impl PossiblyErroneous for ExpressionKind {
    fn error() -> Self {
        Self::Error
    }
}

#[derive(Clone)]
pub struct PiType {
    // @Question should we move `aspect`, `parameter` and `domain` to
    // separate `Domain` type like in `crate::syntax::ast` or should we keep it
    // flat and also inline `Domain` in the non-lowered AST?
    pub explicitness: Explicitness,
    pub aspect: ParameterAspect,
    pub parameter: Option<Identifier>,
    pub domain: Expression,
    pub codomain: Expression,
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
    // @Temporary
    pub laziness: Option<Span>,
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
    Error,
}

impl PossiblyErroneous for PatternKind {
    fn error() -> Self {
        Self::Error
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

/// Something attributes can be ascribed to.
///
/// This is the trait version of the struct [Item].
pub trait AttributeTarget: Spanning {
    /// Used in diagnostics.
    fn name(&self) -> &'static str;

    fn as_attribute_targets(&self) -> AttributeTargets;

    /// Target-specific attribute checks
    // @Note weird API
    fn check_attributes(&self, _attributes: &Attributes, _reporter: &Reporter) -> Result {
        Ok(())
    }
}

impl AttributeTarget for ast::Declaration {
    fn name(&self) -> &'static str {
        use ast::DeclarationKind::*;

        match self.data {
            Value(_) => "a value declaration",
            Data(_) => "a data declaration",
            Constructor(_) => "a constructor declaration",
            Module(_) => "a module declaration",
            Header => "a module header declaraiton",
            Group(_) => "an attribute group declaration",
            Use(_) => "a use-declaration",
        }
    }

    fn as_attribute_targets(&self) -> AttributeTargets {
        use ast::DeclarationKind::*;

        match self.data {
            Value(_) => AttributeTargets::VALUE_DECLARATION,
            Data(_) => AttributeTargets::DATA_DECLARATION,
            Constructor(_) => AttributeTargets::CONSTRUCTOR_DECLARATION,
            Module(_) | Header => AttributeTargets::MODULE_DECLARATION,
            Group(_) => AttributeTargets::all(),
            Use(_) => AttributeTargets::USE_DECLARATION,
        }
    }

    fn check_attributes(&self, attributes: &Attributes, reporter: &Reporter) -> Result {
        use ast::DeclarationKind::*;

        let (body, binder, definition_marker) = match &self.data {
            Value(value) => (
                value.body.as_ref().map(|expression| expression.span),
                &value.binder,
                "=",
            ),
            Data(data) => (
                data.constructors
                    .as_ref()
                    .map(|constructors| constructors.possible_span().unwrap_or(self.span)),
                &data.binder,
                "of",
            ),
            _ => return Ok(()),
        };

        match (body, attributes.has(AttributeKeys::FOREIGN)) {
            (None, false) => {
                Diagnostic::error()
                    .code(Code::E012)
                    .message(format!("declaration `{}` has no definition", binder))
                    .primary_span(self)
                    .help(format!("provide a definition with `{definition_marker}`"))
                    .report(reporter);
                Err(())
            }
            (Some(body), true) => {
                Diagnostic::error()
                .code(Code::E020)
                .message(format!(
                    "`{}` is defined multiple times in this scope",
                    binder
                ))
                .labeled_primary_span(&body, "conflicting definition")
                .labeled_secondary_span(
                    attributes.filter(AttributeKeys::FOREIGN).next().unwrap(),
                    "conflicting definition",
                )
                .note(format!("declaration is marked `foreign` but it also has a body introduced by `{definition_marker}`"))
                .report(reporter);
                Err(())
            }
            _ => Ok(()),
        }
    }
}

impl AttributeTarget for ast::Expression {
    fn name(&self) -> &'static str {
        use ast::ExpressionKind::*;

        match self.data {
            PiTypeLiteral(_) => "a pi type literal",
            Application(_) => "an application",
            TypeLiteral => "a type literal",
            NumberLiteral(_) => "a number literal expression",
            TextLiteral(_) => "a text literal expression",
            TypedHole(_) => "a typed hole",
            Path(_) => "a path expression",
            Field(_) => "a field",
            LambdaLiteral(_) => "a lambda literal",
            LetIn(_) => "a let/in-expression",
            UseIn(_) => "a use/in-expression",
            CaseAnalysis(_) => "a case analysis",
            DoBlock(_) => "a do block",
            SequenceLiteral(_) => "a sequence literal expression",
            Error => "an erroneous expression",
        }
    }

    fn as_attribute_targets(&self) -> AttributeTargets {
        use ast::ExpressionKind::*;

        match self.data {
            PiTypeLiteral(_) => AttributeTargets::PI_TYPE_LITERAL_EXPRESSION,
            Application(_) => AttributeTargets::APPLICATION_EXPRESSION,
            TypeLiteral => AttributeTargets::TYPE_LITERAL_EXPRESSION,
            NumberLiteral(_) => AttributeTargets::NUMBER_LITERAL_EXPRESSION,
            TextLiteral(_) => AttributeTargets::TEXT_LITERAL_EXPRESSION,
            TypedHole(_) => AttributeTargets::TYPED_HOLE_EXPRESSION,
            Path(_) => AttributeTargets::PATH_EXPRESSION,
            Field(_) => AttributeTargets::FIELD_EXPRESSION,
            LambdaLiteral(_) => AttributeTargets::LAMBDA_LITERAL_EXPRESSION,
            LetIn(_) => AttributeTargets::LET_IN_EXPRESSION,
            UseIn(_) => AttributeTargets::USE_IN_EXPRESSION,
            CaseAnalysis(_) => AttributeTargets::CASE_ANALYSIS_EXPRESSION,
            DoBlock(_) => AttributeTargets::DO_BLOCK_EXPRESSION,
            SequenceLiteral(_) => AttributeTargets::SEQUENCE_LITERAL_EXPRESSION,
            Error => AttributeTargets::empty(),
        }
    }
}

impl AttributeTarget for ast::Pattern {
    fn name(&self) -> &'static str {
        use ast::PatternKind::*;

        match self.data {
            NumberLiteral(_) => "a number literal pattern",
            TextLiteral(_) => "a text literal pattern",
            SequenceLiteralPattern(_) => "a sequence literal pattern",
            Path(_) => "a path pattern",
            Binder(_) => "a binder pattern",
            Deapplication(_) => "a deapplication",
        }
    }

    fn as_attribute_targets(&self) -> AttributeTargets {
        use ast::PatternKind::*;

        match self.data {
            NumberLiteral(_) => AttributeTargets::NUMBER_LITERAL_PATTERN,
            TextLiteral(_) => AttributeTargets::TEXT_LITERAL_PATTERN,
            SequenceLiteralPattern(_) => AttributeTargets::SEQUENCE_LITERAL_PATTERN,
            Path(_) => AttributeTargets::PATH_PATTERN,
            Binder(_) => AttributeTargets::BINDER_PATTERN,
            Deapplication(_) => AttributeTargets::DEAPPLICATION_PATTERN,
        }
    }
}

// excluded: crate::syntax::ast::DeclarationKind::{Header, Group}
// @Task somehow generate the explicit bits
bitflags::bitflags! {
    /// Attribute targets.
    pub struct AttributeTargets: u32 {
        const VALUE_DECLARATION = 1 << 0;
        const DATA_DECLARATION = 1 << 1;
        const CONSTRUCTOR_DECLARATION = 1 << 2;
        const MODULE_DECLARATION = 1 << 3;
        const USE_DECLARATION = 1 << 5;

        const DECLARATION = Self::VALUE_DECLARATION.bits
            | Self::DATA_DECLARATION.bits
            | Self::CONSTRUCTOR_DECLARATION.bits
            | Self::MODULE_DECLARATION.bits
            | Self::USE_DECLARATION.bits;

        const PI_TYPE_LITERAL_EXPRESSION = 1 << 6;
        const APPLICATION_EXPRESSION = 1 << 7;
        const TYPE_LITERAL_EXPRESSION = 1 << 8;
        const NUMBER_LITERAL_EXPRESSION = 1 << 9;
        const TEXT_LITERAL_EXPRESSION = 1 << 10;
        const TYPED_HOLE_EXPRESSION = 1 << 11;
        const PATH_EXPRESSION = 1 << 12;
        const FIELD_EXPRESSION = 1 << 13;
        const LAMBDA_LITERAL_EXPRESSION = 1 << 14;
        const LET_IN_EXPRESSION = 1 << 15;
        const USE_IN_EXPRESSION = 1 << 16;
        const CASE_ANALYSIS_EXPRESSION = 1 << 17;
        const DO_BLOCK_EXPRESSION = 1 << 18;
        const SEQUENCE_LITERAL_EXPRESSION = 1 << 19;

        const EXPRESSION = Self::PI_TYPE_LITERAL_EXPRESSION.bits
            | Self::APPLICATION_EXPRESSION.bits
            | Self::TYPE_LITERAL_EXPRESSION.bits
            | Self::NUMBER_LITERAL_EXPRESSION.bits
            | Self::TEXT_LITERAL_EXPRESSION.bits
            | Self::TYPED_HOLE_EXPRESSION.bits
            | Self::PATH_EXPRESSION.bits
            | Self::FIELD_EXPRESSION.bits
            | Self::LAMBDA_LITERAL_EXPRESSION.bits
            | Self::LET_IN_EXPRESSION.bits
            | Self::USE_IN_EXPRESSION.bits
            | Self::CASE_ANALYSIS_EXPRESSION.bits
            | Self::DO_BLOCK_EXPRESSION.bits
            | Self::SEQUENCE_LITERAL_EXPRESSION.bits;

        const NUMBER_LITERAL_PATTERN = 1 << 20;
        const TEXT_LITERAL_PATTERN = 1 << 21;
        const SEQUENCE_LITERAL_PATTERN = 1 << 22;
        const PATH_PATTERN = 1 << 23;
        const BINDER_PATTERN = 1 << 24;
        const DEAPPLICATION_PATTERN = 1 << 25;

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

#[derive(Clone, Default)]
pub struct Attributes {
    pub(super) data: Box<[Attribute]>,
    pub(super) keys: AttributeKeys,
}

impl Attributes {
    pub fn new(attributes: Vec<Attribute>) -> Self {
        let mut keys = AttributeKeys::empty();

        for attribute in &attributes {
            keys |= attribute.data.key();
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

    pub fn filter(
        &self,
        keys: AttributeKeys,
    ) -> impl DoubleEndedIterator<Item = &Attribute> + Clone {
        self.data
            .iter()
            .filter(move |attribute| attribute.matches(keys))
    }

    pub fn get<'a, R: ?Sized>(
        &'a self,
        mut predicate: impl FnMut(&'a AttributeKind) -> Option<&'a R>,
    ) -> &'a R {
        self.data
            .iter()
            .find_map(|attribute| predicate(&attribute.data))
            .unwrap()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Attribute> {
        self.data.iter()
    }
}

impl PossiblyErroneous for Attributes {
    fn error() -> Self {
        Self::default()
    }
}

pub type Attribute = Spanned<AttributeKind>;

impl Attribute {
    pub fn parse(attribute: &ast::Attribute, reporter: &Reporter) -> Result<Self> {
        Ok(Attribute::new(
            attribute.span,
            AttributeKind::parse(attribute, reporter)?,
        ))
    }

    pub fn matches(&self, keys: AttributeKeys) -> bool {
        keys.contains(self.data.key())
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum AttributeKind {
    /// Allow a [lint](Lint).
    ///
    /// # Form
    ///
    /// ```text
    /// allow <0:lint:Path>
    /// ```
    Allow { lint: Lint },
    /// Deny a [lint](Lint).
    ///
    /// # Form
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
    /// # Form
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
    /// # Form
    ///
    /// ```text
    /// documentation <0:content:Text-Literal>
    /// ```
    // @Task change to enum { (String), (Span) }
    Documentation { content: String },
    /// Forbid a [lint](Lint).
    ///
    /// # Form
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
    /// # Form
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
    /// # Form
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
    /// If no `reach` is given, the binding is exposed to other crates.
    ///
    /// # Form
    ///
    /// ```text
    /// public [<0:reach:Path>]
    ///
    Public { reach: Option<ast::Path> },
    /// Define the recursion limit of the TWI.
    ///
    /// # Form
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
    /// # Form
    ///
    /// ```text
    /// unstable <feature:Path> <reason:Text-Literal>
    /// ```
    Unstable { feature: Feature, reason: String },
    /// Specify the concrete type of a sequence literal to be `Vector`.
    Vector,
    /// Warn on a [lint](Lint).
    ///
    /// # Form
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
    pub fn targets(&self) -> AttributeTargets {
        use AttributeKind::*;
        use AttributeTargets as Targets;

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
            | Self::NAT64.bits
            | Self::OPAQUE.bits
            | Self::PUBLIC.bits;
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

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Version {}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Condition {}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Feature {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Number {
    Nat(crate::utility::Nat),
    Nat32(u32),
    Nat64(u64),
    Int(crate::utility::Int),
    Int32(i32),
    Int64(i64),
}

pub macro decl($( $tree:tt )+) {
    crate::item::item!(crate::syntax::lowered_ast, DeclarationKind, Box; $( $tree )+)
}

pub macro expr($( $tree:tt )+) {
    crate::item::item!(crate::syntax::lowered_ast, ExpressionKind, Rc; $( $tree )+)
}

pub macro pat($( $tree:tt )+) {
    crate::item::item!(crate::syntax::lowered_ast, PatternKind, Rc; $( $tree )+)
}
