use derivation::{Discriminant, Elements, FromStr, Str};
use span::{Span, Spanned, Spanning};
use std::fmt;
use utilities::{condition, obtain, Atom};

/// Something attributes can be ascribed to.
///
/// This is the trait version of the struct [`super::Item`].
pub trait Target: Spanning {
    /// Used in diagnostics.
    fn name(&self) -> &'static str;

    fn as_targets(&self) -> Targets;
}

impl Target for ast::Declaration {
    fn name(&self) -> &'static str {
        use ast::BareDeclaration::*;

        match &self.bare {
            Function(_) => "a function declaration",
            Data(_) => "a data declaration",
            Constructor(_) => "a constructor declaration",
            // @Task it would be better (= less confusing) if we didn't mention "inline" and "out-of-line"
            // unconditionally, only for relevant target mismatches
            Module(module) => {
                if module.declarations.is_some() {
                    "an inline module declaration"
                } else {
                    "an out-of-line module declaration"
                }
            }
            ModuleHeader => "a module header declaration",
            Group(_) => "an attribute group declaration",
            Use(_) => "a use-declaration",
        }
    }

    fn as_targets(&self) -> Targets {
        use ast::BareDeclaration::*;

        match &self.bare {
            Function(_) => Targets::FUNCTION_DECLARATION,
            Data(_) => Targets::DATA_DECLARATION,
            Constructor(_) => Targets::CONSTRUCTOR_DECLARATION,
            Module(module) => {
                if module.declarations.is_some() {
                    Targets::INLINE_MODULE_DECLARATION
                } else {
                    Targets::OUT_OF_LINE_MODULE_DECLARATION
                }
            }
            ModuleHeader => Targets::MODULE_HEADER_DECLARATION,
            Group(_) => Targets::all(),
            Use(_) => Targets::USE_DECLARATION,
        }
    }
}

impl Target for ast::Expression {
    fn name(&self) -> &'static str {
        use ast::BareExpression::*;

        match self.bare {
            PiTypeLiteral(_) => "a pi type literal",
            Application(_) => "an application",
            NumberLiteral(_) => "a number literal expression",
            TextLiteral(_) => "a text literal expression",
            TypedHole(_) => "a typed hole",
            Path(_) => "a path expression",
            Field(_) => "a field",
            LambdaLiteral(_) => "a lambda literal",
            LetBinding(_) => "a let-binding",
            UseBinding(_) => "a use-binding",
            CaseAnalysis(_) => "a case analysis",
            DoBlock(_) => "a do block",
            SequenceLiteral(_) => "a sequence literal expression",
            Error(_) => "an erroneous expression",
        }
    }

    fn as_targets(&self) -> Targets {
        use ast::BareExpression::*;

        match self.bare {
            PiTypeLiteral(_) => Targets::PI_TYPE_LITERAL_EXPRESSION,
            Application(_) => Targets::APPLICATION_EXPRESSION,
            NumberLiteral(_) => Targets::NUMBER_LITERAL_EXPRESSION,
            TextLiteral(_) => Targets::TEXT_LITERAL_EXPRESSION,
            TypedHole(_) => Targets::TYPED_HOLE_EXPRESSION,
            Path(_) => Targets::PATH_EXPRESSION,
            Field(_) => Targets::FIELD_EXPRESSION,
            LambdaLiteral(_) => Targets::LAMBDA_LITERAL_EXPRESSION,
            LetBinding(_) => Targets::LET_IN_EXPRESSION,
            UseBinding(_) => Targets::USE_IN_EXPRESSION,
            CaseAnalysis(_) => Targets::CASE_ANALYSIS_EXPRESSION,
            DoBlock(_) => Targets::DO_BLOCK_EXPRESSION,
            SequenceLiteral(_) => Targets::SEQUENCE_LITERAL_EXPRESSION,
            Error(_) => Targets::empty(),
        }
    }
}

impl Target for ast::Pattern {
    fn name(&self) -> &'static str {
        use ast::BarePattern::*;

        match self.bare {
            NumberLiteral(_) => "a number literal pattern",
            TextLiteral(_) => "a text literal pattern",
            SequenceLiteral(_) => "a sequence literal pattern",
            Path(_) => "a path pattern",
            Binder(_) => "a binder pattern",
            Application(_) => "an application pattern",
        }
    }

    fn as_targets(&self) -> Targets {
        use ast::BarePattern::*;

        match self.bare {
            NumberLiteral(_) => Targets::NUMBER_LITERAL_PATTERN,
            TextLiteral(_) => Targets::TEXT_LITERAL_PATTERN,
            SequenceLiteral(_) => Targets::SEQUENCE_LITERAL_PATTERN,
            Path(_) => Targets::PATH_PATTERN,
            Binder(_) => Targets::BINDER_PATTERN,
            Application(_) => Targets::APPLICATION_PATTERN,
        }
    }
}

// excluded: crate::syntax::ast::DeclarationKind::{Header, Group}
// @Task somehow generate the explicit bits
// @Beacon @Task replace this!
bitflags::bitflags! {
    /// Attribute targets.
    pub struct Targets: u32 {
        const FUNCTION_DECLARATION = 1 << 0;
        const DATA_DECLARATION = 1 << 1;
        const CONSTRUCTOR_DECLARATION = 1 << 2;
        const INLINE_MODULE_DECLARATION = 1 << 3;
        const OUT_OF_LINE_MODULE_DECLARATION = 1 << 26;
        const MODULE_HEADER_DECLARATION = 1 << 4;
        const USE_DECLARATION = 1 << 5;

        const MODULE_DECLARATION = Self::INLINE_MODULE_DECLARATION.bits
            | Self::OUT_OF_LINE_MODULE_DECLARATION.bits;

        const DECLARATION = Self::FUNCTION_DECLARATION.bits
            | Self::DATA_DECLARATION.bits
            | Self::CONSTRUCTOR_DECLARATION.bits
            | Self::MODULE_DECLARATION.bits
            | Self::MODULE_HEADER_DECLARATION.bits
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
        const APPLICATION_PATTERN = 1 << 25;

        const PATTERN = Self::NUMBER_LITERAL_PATTERN.bits
            | Self::TEXT_LITERAL_PATTERN.bits
            | Self::SEQUENCE_LITERAL_PATTERN.bits
            | Self::PATH_PATTERN.bits
            | Self::BINDER_PATTERN.bits
            | Self::APPLICATION_PATTERN.bits;

        const NUMBER_LITERAL = Self::NUMBER_LITERAL_EXPRESSION.bits
            | Self::NUMBER_LITERAL_PATTERN.bits;
        const TEXT_LITERAL = Self::TEXT_LITERAL_EXPRESSION.bits
            | Self::TEXT_LITERAL_PATTERN.bits;
        const SEQUENCE_LITERAL = Self::SEQUENCE_LITERAL_EXPRESSION.bits
            | Self::SEQUENCE_LITERAL_PATTERN.bits;
    }
}

impl Targets {
    // @Beacon @Bug this is soo fragile!
    pub fn description(self) -> &'static str {
        // @Task use match + inline consts once derived operations are const
        condition! {
            self == Self::all() => "anything",
            self == Self::DECLARATION => "declarations",
            self == Self::FUNCTION_DECLARATION | Self::DATA_DECLARATION => "function or data declarations",
            self == Self::TEXT_LITERAL => "text literals",
            self == Self::DATA_DECLARATION => "data declarations",
            self == Self::NUMBER_LITERAL => "number literals",
            self == Self::SEQUENCE_LITERAL => "sequence literals",
            // @Note hideous description!
            self == Self::DECLARATION - Self::CONSTRUCTOR_DECLARATION - Self::MODULE_HEADER_DECLARATION => "declarations except constructors and module headers",
            self == Self::MODULE_DECLARATION => "module declarations",
            self == Self::MODULE_DECLARATION | Self::MODULE_HEADER_DECLARATION => "module (header) declarations",
            self == Self::OUT_OF_LINE_MODULE_DECLARATION => "out-of-line module declarations",
            self == Self::FUNCTION_DECLARATION | Self::MODULE_DECLARATION | Self::MODULE_HEADER_DECLARATION => "function or module (header) declarations",
            self == Self::EXPRESSION => "expressions",
            self == Self::FUNCTION_DECLARATION
                | Self::CONSTRUCTOR_DECLARATION
                | Self::EXPRESSION
                | Self::PATTERN => "function or constructor declarations, expressions or patterns",
            // the particular description is not needed for `AttributeKinds::target` and thus it is not defined
            else => unreachable!(),
        }
    }
}

#[derive(Clone, Default)]
pub struct Attributes(pub Vec<Attribute>);

impl Attributes {
    pub fn has<Q: Query>(&self, query: Q) -> bool {
        self.0
            .iter()
            .any(move |attribute| query.matches(&attribute.bare))
    }

    pub fn filter<Q: Query>(&self, query: Q) -> impl Iterator<Item = &Attribute> {
        self.0
            .iter()
            .filter(move |attribute| query.matches(&attribute.bare))
    }

    pub fn get<const NAME: AttributeName>(&self) -> Option<Spanned<&DataQueryOutput<NAME>>>
    where
        NameQuery<NAME>: DataQuery,
    {
        self.0.iter().find_map(move |attribute| {
            Some(Spanned::new(
                attribute.span,
                NameQuery::<NAME>::obtain(&attribute.bare)?,
            ))
        })
    }

    pub fn select<const NAME: AttributeName>(
        &self,
    ) -> impl Iterator<Item = &DataQueryOutput<NAME>> + Clone
    where
        NameQuery<NAME>: DataQuery,
    {
        self.0
            .iter()
            .filter_map(move |attribute| NameQuery::<NAME>::obtain(&attribute.bare))
    }

    pub fn span<Q: Query>(&self, query: Q) -> Option<Span> {
        self.0
            .iter()
            .find(move |attribute| query.matches(&attribute.bare))
            .map(Attribute::span)
    }
}

pub type Attribute = Spanned<BareAttribute>;

#[derive(Clone, PartialEq, Eq, Hash, Discriminant)]
#[discriminant(
    name:
        #[derive(Elements, Str, FromStr)]
        #[format(dash_case)]
        #[str(to_str)]
        AttributeName
)]
pub enum BareAttribute {
    /// Hide the constructors of a (public) data type.
    Abstract,
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
    /// deprecated <0:reason:Text-Literal> [<since:Version>] [<removal:Version>] [<replacement:Text-Literal>]
    /// ```
    Deprecated(Deprecated),
    /// Documentation of a binding.
    ///
    /// The format of text is not decided yet. I'd like to have something better than
    /// markdown. There are plenty of options.
    ///
    /// # Form
    ///
    /// ```text
    /// doc <0:content:Text-Literal>
    /// ```
    Doc { content: Atom },
    /// Forbid a [lint](Lint).
    ///
    /// # Form
    ///
    /// ```text
    /// forbid <0:lint:Path>
    /// ```
    Forbid { lint: Lint },
    /// Make the inclusion of the attribute target dependent on a [condition](Condition).
    ///
    /// Aka conditional compilation.
    ///
    /// # Form
    ///
    /// ```text
    /// if <0:condition:Condition>
    /// ```
    #[allow(dead_code)]
    If { condition: Condition },
    /// Identify a binding as being intrinsic to the language.
    ///
    /// # Form
    ///
    /// ```text
    /// intrinsic [<0:name:Path>]
    /// ```
    Intrinsic(Special),
    /// Exclude the attribute target from further processing.
    ///
    /// Basically `@(if false)` but `if` won't be implemented anytime soon.
    Ignore,
    /// Statically include the contents of file given by path.
    Include,
    /// Identify a binding as being known to the compiler.
    ///
    /// # Form
    ///
    /// ```text
    /// known [<0:name:Path>]
    /// ```
    Known(Special),
    /// Change the path where the out-of-line module resides.
    ///
    /// # Form
    ///
    /// ```text
    /// location <0:path:Text-Literal>
    /// ```
    Location { path: Atom },
    /// Mark a data type binding to be likely expanded in the number of constructors.
    Moving,
    /// Make the binding part of the public API or at least visible in modules higher up.
    ///
    /// If no `reach` is given, the binding is exposed to other components.
    ///
    /// # Form
    ///
    /// ```text
    /// public [<0:reach:Path>]
    ///
    Public(Public),
    /// Define the recursion limit of the TWI.
    ///
    /// # Form
    ///
    /// ```text
    /// recursion-limit <0:depth:Number-Literal>
    /// ```
    // @Task define allowed range
    RecursionLimit { depth: u32 },
    /// Force an expression to be evaluated at compile-time.
    Static,
    /// Output statistics about a declaration.
    Statistics,
    /// Mark a function as a unit test.
    Test,
    /// Mark a binding or expression as "unsafe".
    Unsafe,
    /// Mark a binding as an unstable part of the public API.
    ///
    /// # Form
    ///
    /// ```text
    /// unstable <feature:Path> <reason:Text-Literal>
    /// ```
    #[allow(dead_code)]
    Unstable(Unstable),
    /// Warn on a [lint](Lint).
    ///
    /// # Form
    ///
    /// ```text
    /// warn <0:lint:Path>
    /// ```
    Warn { lint: Lint },
}

impl BareAttribute {
    pub fn targets(&self) -> Targets {
        use BareAttribute::*;

        // when updating, update `AttributeTargets::description` accordingly
        match self {
            Allow { .. } | Deny { .. } | Forbid { .. } | Warn { .. } => Targets::all(),
            Deprecated { .. } | Doc { .. } | If { .. } | Ignore | Statistics | Unstable { .. } => {
                Targets::DECLARATION
            }
            Intrinsic { .. } => Targets::FUNCTION_DECLARATION | Targets::DATA_DECLARATION,
            Include => Targets::TEXT_LITERAL,
            Known { .. } | Moving | Abstract => Targets::DATA_DECLARATION,
            // @Task for constructors, smh add extra diagnostic note saying they are public automatically
            // @Update with `@transparent` implemented, suggest `@transparent` on the data decl
            Public { .. } => {
                Targets::DECLARATION
                    - Targets::CONSTRUCTOR_DECLARATION
                    - Targets::MODULE_HEADER_DECLARATION
            }
            RecursionLimit { .. } => {
                Targets::MODULE_DECLARATION | Targets::MODULE_HEADER_DECLARATION
            }
            Location { .. } => Targets::OUT_OF_LINE_MODULE_DECLARATION,
            Test => {
                Targets::FUNCTION_DECLARATION
                    | Targets::MODULE_DECLARATION
                    | Targets::MODULE_HEADER_DECLARATION
            }
            Static => Targets::EXPRESSION,
            Unsafe => {
                Targets::FUNCTION_DECLARATION
                    | Targets::CONSTRUCTOR_DECLARATION
                    | Targets::EXPRESSION
                    | Targets::PATTERN
            }
        }
    }

    pub const fn is_implemented(&self) -> bool {
        self.name().is_implemented()
    }

    pub const fn is_internal(&self) -> bool {
        self.name().is_internal()
    }

    pub const fn can_be_applied_multiple_times(&self) -> bool {
        matches!(
            self,
            Self::Allow { .. }
                | Self::Deny { .. }
                | Self::Doc { .. }
                | Self::Forbid { .. }
                | Self::Warn { .. }
        )
    }
}

impl fmt::Display for BareAttribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@")?;

        let name = self.name().to_str();

        match self {
            Self::Abstract
            | Self::Ignore
            | Self::Include
            | Self::Moving
            | Self::Static
            | Self::Statistics
            | Self::Test
            | Self::Unsafe => write!(f, "{name}"),

            Self::Allow { lint }
            | Self::Deny { lint }
            | Self::Forbid { lint }
            | Self::Warn { lint } => write!(f, "({name} {lint})"),

            Self::Intrinsic(special) | Self::Known(special) => match &special.name {
                Some(path) => write!(f, "({name} {path})"),
                None => write!(f, "{name}"),
            },

            Self::Deprecated(deprecated) => {
                if deprecated.reason.is_none()
                    && deprecated.since.is_none()
                    && deprecated.removal.is_none()
                    && deprecated.replacement.is_none()
                {
                    write!(f, "{name}")
                } else {
                    write!(f, "({name}")?;
                    if let Some(reason) = &deprecated.reason {
                        write!(f, " (reason {reason:?})")?;
                    }
                    if let Some(since) = &deprecated.since {
                        write!(f, " (since {since:?})")?;
                    }
                    if let Some(removal) = &deprecated.removal {
                        write!(f, " (removal {removal:?})")?;
                    }
                    if let Some(replacement) = &deprecated.replacement {
                        write!(f, " (replacement {replacement:?})")?;
                    }
                    write!(f, ")")
                }
            }
            Self::Doc { content } => write!(f, "({name} {content:?})"),
            Self::If { condition } => write!(f, "({name} {condition})"),
            Self::Location { path } => write!(f, "({name} {path})"),
            Self::Public(public) => match &public.reach {
                Some(reach) => write!(f, "({name} {reach})"),
                None => write!(f, "{name}"),
            },
            Self::RecursionLimit { depth } => write!(f, "({name} {depth})"),
            Self::Unstable(unstable) => {
                write!(f, "({name} {} {:?})", unstable.feature, unstable.reason)
            }
        }
    }
}

impl AttributeName {
    pub const fn is_implemented(self) -> bool {
        matches!(
            self,
            Self::Deprecated { .. }
                | Self::Doc { .. }
                | Self::Location { .. }
                | Self::Abstract
                | Self::Public { .. }
        ) || self.is_internal()
    }

    pub const fn is_internal(self) -> bool {
        matches!(self, Self::Intrinsic | Self::Known | Self::Statistics)
    }
}

impl fmt::Display for AttributeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.to_str())
    }
}

pub trait Query: Copy {
    fn matches(self, attribute: &BareAttribute) -> bool;

    fn or<Q: Query>(self, other: Q) -> Or<Self, Q> {
        Or {
            left: self,
            right: other,
        }
    }
}

impl Query for AttributeName {
    fn matches(self, attribute: &BareAttribute) -> bool {
        attribute.name() == self
    }
}

#[derive(Clone, Copy)]
pub struct Or<L: Query, R: Query> {
    left: L,
    right: R,
}

impl<L: Query, R: Query> Query for Or<L, R> {
    fn matches(self, attribute: &BareAttribute) -> bool {
        self.left.matches(attribute) || self.right.matches(attribute)
    }
}

impl<F: Fn(&BareAttribute) -> bool + Copy> Query for Predicate<F> {
    fn matches(self, attribute: &BareAttribute) -> bool {
        self.0(attribute)
    }
}

#[derive(Clone, Copy)]
pub struct Predicate<F: Fn(&BareAttribute) -> bool>(pub F);

pub trait DataQuery: Copy {
    type Output: ?Sized;

    fn obtain(attribute: &BareAttribute) -> Option<&Self::Output>;
}

#[derive(Clone, Copy)]
pub struct NameQuery<const NAME: AttributeName>;

pub type DataQueryOutput<const NAME: AttributeName> = <NameQuery<NAME> as DataQuery>::Output;

// @Task use proc macro to derive non-unit getters
macro data_queries($( $name:ident: $Output:ty = $pat:pat => $expr:expr ),+ $(,)?) {
    $(
        impl DataQuery for NameQuery<{ AttributeName::$name }> {
            type Output = $Output;

            fn obtain(attribute: &BareAttribute) -> Option<&Self::Output> {
                obtain!(attribute, $pat => $expr)
            }
        }
    )+
}

data_queries! {
    Deprecated: Deprecated = BareAttribute::Deprecated(deprecated) => deprecated,
    Doc: str = BareAttribute::Doc { content } => content,
    Intrinsic: Special = BareAttribute::Intrinsic(special) => special,
    Known: Special = BareAttribute::Known(special) => special,
    Location: str = BareAttribute::Location { path } => path,
    Public: Public = BareAttribute::Public(reach) => reach,
}

#[derive(Clone, PartialEq, Eq, Hash)]

pub struct Deprecated {
    pub reason: Option<Atom>,
    pub since: Option<Version>,
    pub removal: Option<Version>,
    pub replacement: Option<Atom>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Public {
    pub reach: Option<ast::Path>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Unstable {
    pub feature: Feature,
    pub reason: String,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Special {
    pub name: Option<ast::Path>,
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
