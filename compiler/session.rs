use crate::{
    component::{Component, ComponentIndex},
    diagnostics::{Diagnostic, ErrorCode, Reporter},
    entity::{Entity, EntityKind},
    error::Result,
    hir::{self, DeclarationIndex, Expression, ExpressionKind, Identifier},
    package::{Package, PackageIndex},
    span::{SourceMap, Span},
    syntax::ast::Explicitness,
    utility::{condition, HashMap, Int, Nat},
};
use derivation::{FromStr, Str};
use index_map::IndexMap;
use num_traits::{CheckedDiv, CheckedSub};
use std::{
    default::default,
    fmt,
    ops::Index,
    path::PathBuf,
    str::FromStr,
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

const BUILD_FOLDER_NAME: &str = "build";

pub struct BuildSession {
    /// The components which have already been built in this session.
    components: HashMap<ComponentIndex, Component>,
    /// The packages whose components have not necessarily been built yet in this session but are about to.
    packages: IndexMap<PackageIndex, Package>,
    goal_component: ComponentIndex,
    goal_package: PackageIndex,
    // @Task Identifier -> DeclarationIndex
    known_bindings: HashMap<KnownBinding, Identifier>,
    intrinsic_types: HashMap<IntrinsicType, Identifier>,
    intrinsic_functions: HashMap<IntrinsicFunction, IntrinsicFunctionValue>,
    map: Arc<RwLock<SourceMap>>,
    reporter: Reporter,
}

impl BuildSession {
    /// Create a new build session with all intrinsic functions defined.
    pub(crate) fn new(
        packages: IndexMap<PackageIndex, Package>,
        goal_component: ComponentIndex,
        goal_package: PackageIndex,
        map: &Arc<RwLock<SourceMap>>,
        reporter: Reporter,
    ) -> Self {
        Self {
            components: default(),
            packages,
            goal_component,
            goal_package,
            known_bindings: default(),
            intrinsic_types: default(),
            intrinsic_functions: intrinsic_functions(),
            map: map.clone(),
            reporter,
        }
    }

    #[cfg(test)]
    pub(crate) fn test() -> Self {
        let map: Arc<RwLock<SourceMap>> = default();

        Self {
            components: default(),
            packages: default(),
            goal_component: ComponentIndex(0),
            goal_package: PackageIndex::new_unchecked(0),
            known_bindings: default(),
            intrinsic_types: default(),
            intrinsic_functions: default(),
            map: map.clone(),
            reporter: Reporter::stderr().with_map(map),
        }
    }

    pub fn goal_package(&self) -> PackageIndex {
        self.goal_package
    }

    pub fn goal_component(&self) -> ComponentIndex {
        self.goal_component
    }

    /// The path to the folder containing the build artifacts.
    pub(crate) fn build_folder(&self) -> PathBuf {
        // @Beacon @Beacon @Bug does not work with single-file packages!
        self[self.goal_package].path.join(BUILD_FOLDER_NAME)
    }

    pub fn add(&mut self, component: Component) {
        self.components.insert(component.index(), component);
    }

    #[allow(dead_code)]
    pub(crate) fn known_bindings(&self) -> impl Iterator<Item = (KnownBinding, &Identifier)> {
        self.known_bindings
            .iter()
            .map(|(&known, identifier)| (known, identifier))
    }

    pub(crate) fn known_binding(&self, known: KnownBinding) -> Option<&Identifier> {
        self.known_bindings.get(&known)
    }

    pub(crate) fn look_up_known_binding(&self, known: KnownBinding) -> Result<Expression> {
        Ok(self
            .known_binding(known)
            .cloned()
            .ok_or_else(|| Diagnostic::missing_known_binding(known).report(self.reporter()))?
            .into_expression())
    }

    pub(crate) fn look_up_known_type(
        &self,
        known: KnownBinding,
        expression: Span,
    ) -> Result<Expression> {
        Ok(self
            .known_binding(known)
            .cloned()
            .ok_or_else(|| {
                Diagnostic::missing_known_type(known, expression).report(self.reporter())
            })?
            .into_expression())
    }

    pub(crate) fn define_known_binding(
        &mut self,
        binder: &Identifier,
        namespace: Option<&str>,
        attribute: Span,
    ) -> Result {
        let Some(binding) = KnownBinding::parse(namespace, binder.as_str()) else {
            return Err(Diagnostic::error()
                .code(ErrorCode::E063)
                .message(format!(
                    "‘{}{binder}’ is not a known binding",
                    namespace.map(|namespace| format!(".{namespace}"))
                        .unwrap_or_default()
                ))
                .primary_span(binder)
                .secondary_span(attribute)
                .report(self.reporter()));
        };

        if let Some(previous) = self.known_bindings.get(&binding) {
            return Err(Diagnostic::error()
                .code(ErrorCode::E039)
                .message(format!(
                    "the known binding ‘{}’ is defined multiple times",
                    binding.path()
                ))
                .labeled_primary_span(binder, "redefinition")
                .labeled_secondary_span(previous, "previous definition")
                .report(self.reporter()));
        }

        self.known_bindings.insert(binding, binder.clone());

        Ok(())
    }

    pub(crate) fn intrinsic_types(&self) -> impl Iterator<Item = (IntrinsicType, &Identifier)> {
        self.intrinsic_types
            .iter()
            .map(|(&intrinsic, identifier)| (intrinsic, identifier))
    }

    pub(crate) fn intrinsic_type(&self, intrinsic: IntrinsicType) -> Option<&Identifier> {
        self.intrinsic_types.get(&intrinsic)
    }

    // @Beacon @Task support paths!
    pub(crate) fn define_intrinsic_type(&mut self, binder: Identifier, attribute: Span) -> Result {
        let Ok(intrinsic) = binder.as_str().parse::<IntrinsicType>() else {
            return Err(Diagnostic::unrecognized_intrinsic_binding(binder.as_str(), IntrinsicKind::Type)
                .primary_span(&binder)
                .secondary_span(attribute)
                .report(self.reporter()));
        };

        if let Some(previous) = self.intrinsic_type(intrinsic) {
            return Err(Diagnostic::error()
                .code(ErrorCode::E040)
                .message(format!(
                    "the intrinsic type ‘{intrinsic}’ is defined multiple times",
                ))
                .labeled_primary_span(&binder, "redefinition")
                .labeled_secondary_span(previous as &_, "previous definition")
                .report(self.reporter()));
        }

        self.intrinsic_types.insert(intrinsic, binder);

        Ok(())
    }

    pub(crate) fn define_intrinsic_function(
        &mut self,
        binder: Identifier,
        type_: Expression,
        attribute: Span,
    ) -> Result<EntityKind> {
        let Ok(intrinsic) = binder.as_str().parse() else {
            return Err(Diagnostic::unrecognized_intrinsic_binding(binder.as_str(), IntrinsicKind::Function)
                .primary_span(&binder)
                .secondary_span(attribute)
                .report(self.reporter()));
        };

        // @Task explain why we remove here
        // @Task explain why unwrap
        let function = self.intrinsic_functions.remove(&intrinsic).unwrap();

        Ok(EntityKind::IntrinsicFunction {
            type_,
            arity: function.arity,
            function: function.function,
        })
    }

    pub(crate) fn look_up_intrinsic_type(
        &self,
        intrinsic: IntrinsicType,
        expression: Option<Span>,
    ) -> Result<Expression> {
        if let Some(intrinsic) = self.intrinsic_type(intrinsic) {
            return Ok(intrinsic.clone().into_expression());
        }

        Err(
            Diagnostic::missing_intrinsic_binding(intrinsic, IntrinsicKind::Type)
                .with(|error| match expression {
                    Some(expression) => {
                        error.labeled_primary_span(expression, "the type of this expression")
                    }
                    None => error,
                })
                .report(self.reporter()),
        )
    }

    pub fn shared_map(&self) -> RwLockReadGuard<'_, SourceMap> {
        self.map.read().unwrap()
    }

    pub fn map(&self) -> RwLockWriteGuard<'_, SourceMap> {
        self.map.write().unwrap()
    }

    pub fn reporter(&self) -> &Reporter {
        &self.reporter
    }
}

impl Index<DeclarationIndex> for BuildSession {
    type Output = Entity;

    fn index(&self, index: DeclarationIndex) -> &Self::Output {
        &self[index.component()][index.local_unchecked()]
    }
}

impl Index<ComponentIndex> for BuildSession {
    type Output = Component;

    fn index(&self, index: ComponentIndex) -> &Self::Output {
        self.components.get(&index).unwrap_or_else(|| {
            panic!(
                "attempt to look up unbuilt or unknown component ‘{index:?}’ in the build session"
            )
        })
    }
}

impl Index<PackageIndex> for BuildSession {
    type Output = Package;

    fn index(&self, index: PackageIndex) -> &Self::Output {
        &self.packages[index]
    }
}

impl Diagnostic {
    fn missing_known_binding(binding: KnownBinding) -> Self {
        Self::error().code(ErrorCode::E062).message(format!(
            "the known binding ‘{}’ is not defined",
            binding.path()
        ))
    }

    fn missing_known_type(binding: KnownBinding, expression: Span) -> Self {
        Self::missing_known_binding(binding)
            .labeled_primary_span(expression, "the type of this expression")
    }

    fn unrecognized_intrinsic_binding(name: &str, kind: IntrinsicKind) -> Self {
        Self::error()
            .code(ErrorCode::E061)
            .message(format!("‘{name}’ is not an intrinsic {kind}"))
    }

    fn missing_intrinsic_binding(intrinsic: IntrinsicType, kind: IntrinsicKind) -> Self {
        Self::error()
            .code(ErrorCode::E060)
            .message(format!("the intrinsic {kind} ‘{intrinsic}’ is not defined"))
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) enum KnownBinding {
    /// The type `Unit`.
    Unit,
    /// The value `Unit.unit`.
    UnitUnit,
    /// The type `Bool`.
    Bool,
    /// The value `Bool.false`.
    BoolFalse,
    /// The value `Bool.true`.
    BoolTrue,
    /// The type `Option`.
    Option,
    /// The value `Option.none`.
    OptionNone,
    /// The value `Option.some`.
    OptionSome,
}

impl KnownBinding {
    #[must_use]
    pub(crate) fn parse(namespace: Option<&str>, binder: &str) -> Option<Self> {
        Some(match (namespace, binder) {
            (None, "Unit") => Self::Unit,
            (Some("Unit"), "unit") => Self::UnitUnit,
            (None, "Bool") => Self::Bool,
            (Some("Bool"), "false") => Self::BoolFalse,
            (Some("Bool"), "true") => Self::BoolTrue,
            (None, "Option") => Self::Option,
            (Some("Option"), "none") => Self::OptionNone,
            (Some("Option"), "some") => Self::OptionSome,
            _ => return None,
        })
    }

    pub(crate) const fn path(self) -> &'static str {
        match self {
            Self::Unit => "Unit",
            Self::UnitUnit => "Unit.unit",
            Self::Bool => "Bool",
            Self::BoolFalse => "Bool.false",
            Self::BoolTrue => "Bool.true",
            Self::Option => "Option",
            Self::OptionNone => "Option.none",
            Self::OptionSome => "Option.some",
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) enum IntrinsicType {
    Numeric(IntrinsicNumericType),
    Text,
    IO,
}

impl From<IntrinsicNumericType> for IntrinsicType {
    fn from(type_: IntrinsicNumericType) -> Self {
        Self::Numeric(type_)
    }
}

impl FromStr for IntrinsicType {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(match input {
            "Nat" => IntrinsicNumericType::Nat.into(),
            "Nat32" => IntrinsicNumericType::Nat32.into(),
            "Nat64" => IntrinsicNumericType::Nat64.into(),
            "Int" => IntrinsicNumericType::Int.into(),
            "Int32" => IntrinsicNumericType::Int32.into(),
            "Int64" => IntrinsicNumericType::Int64.into(),
            "Text" => Self::Text,
            "IO" => Self::IO,
            _ => return Err(()),
        })
    }
}

impl fmt::Display for IntrinsicType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Numeric(type_) => write!(f, "{type_}"),
            Self::Text => write!(f, "Text"),
            Self::IO => write!(f, "IO"),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) enum IntrinsicNumericType {
    Nat,
    Nat32,
    Nat64,
    Int,
    Int32,
    Int64,
}

impl IntrinsicNumericType {
    pub(crate) const fn interval(self) -> &'static str {
        // @Question use `∞`?
        match self {
            Self::Nat => "[0, infinity)",
            Self::Nat32 => "[0, 2^32-1]",
            Self::Nat64 => "[0, 2^64-1]",
            Self::Int => "(-infinity, infinity)",
            Self::Int32 => "[-2^31, 2^31-1]",
            Self::Int64 => "[-2^63, 2^63-1]",
        }
    }
}

// @Task derive this with `#[format(upper_dash_case)]`
impl fmt::Display for IntrinsicNumericType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Nat => "Nat",
            Self::Nat32 => "Nat32",
            Self::Nat64 => "Nat64",
            Self::Int => "Int",
            Self::Int32 => "Int32",
            Self::Int64 => "Int64",
        })
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, FromStr, Str)]
#[format(dash_case)]
pub(crate) enum IntrinsicFunction {
    Add,
    Subtract,
    // @Temporary
    PanickingSubtract,
    Multiply,
    Divide,
    Equal,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Display,
    Concat,
    AddNat32,
    Print,
}

impl fmt::Display for IntrinsicFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

pub(crate) type BareIntrinsicFunctionValue = fn(arguments: Vec<Value>) -> Value;

pub(crate) struct IntrinsicFunctionValue {
    pub(crate) arity: usize,
    pub(crate) function: BareIntrinsicFunctionValue,
}

// @Task improve name
pub(crate) enum IntrinsicKind {
    Type,
    Function,
}

impl fmt::Display for IntrinsicKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Type => "type",
            Self::Function => "function",
        })
    }
}

// @Task replace this HashMap business with a match and interpret at call-site!
fn intrinsic_functions() -> HashMap<IntrinsicFunction, IntrinsicFunctionValue> {
    use IntrinsicFunction::*;

    let mut intrinsics = HashMap::default();

    intrinsics.insert(Add, pure!(|x: Nat, y: Nat| x + y));
    intrinsics.insert(Subtract, pure!(|x: Nat, y: Nat| x.checked_sub(&y)));
    intrinsics.insert(PanickingSubtract, pure!(|x: Nat, y: Nat| x - y));
    intrinsics.insert(Multiply, pure!(|x: Nat, y: Nat| x * y));
    intrinsics.insert(Divide, pure!(|x: Nat, y: Nat| x.checked_div(&y)));
    intrinsics.insert(Equal, pure!(|x: Nat, y: Nat| x == y));
    intrinsics.insert(Less, pure!(|x: Nat, y: Nat| x < y));
    intrinsics.insert(LessEqual, pure!(|x: Nat, y: Nat| x <= y));
    intrinsics.insert(Greater, pure!(|x: Nat, y: Nat| x > y));
    intrinsics.insert(GreaterEqual, pure!(|x: Nat, y: Nat| x >= y));
    intrinsics.insert(Display, pure!(|x: Nat| x.to_string()));
    intrinsics.insert(Concat, pure!(|a: Text, b: Text| a + &b));
    // @Temporary until we can target specific modules
    intrinsics.insert(AddNat32, pure!(|a: Nat32, b: Nat32| a + b));
    // @Temporary
    intrinsics.insert(
        Print,
        pure!(|message: Text| Value::IO {
            index: 0,
            arguments: vec![Value::Text(message)],
        }),
    );

    intrinsics
}

/// An FFI-compatible type.
///
/// Except `Type` and `->`.
pub(crate) enum Type {
    Unit,
    Bool,
    Nat,
    Nat32,
    Nat64,
    Int,
    Int32,
    Int64,
    Text,
    Option(Box<Type>),
}

impl Type {
    // @Task improve this code with the new enum logic
    fn from_expression(expression: &Expression, session: &BuildSession) -> Option<Self> {
        use IntrinsicNumericType::*;
        use IntrinsicType::*;
        use KnownBinding::*;

        let known = |binding: &hir::Binding, known: KnownBinding| {
            session
                .known_binding(known)
                .map_or(false, |known| &binding.0 == known)
        };
        let intrinsic = |binding: &hir::Binding, intrinsic: IntrinsicType| {
            session
                .intrinsic_type(intrinsic)
                .map_or(false, |intrinsic| &binding.0 == intrinsic)
        };

        Some(match &expression.value {
            // @Note this lookup looks incredibly inefficient
            ExpressionKind::Binding(binding) => condition! {
                known(binding, Unit) => Self::Unit,
                known(binding, Bool) => Self::Bool,
                intrinsic(binding, Nat.into()) => Self::Nat,
                intrinsic(binding, Nat32.into()) => Self::Nat32,
                intrinsic(binding, Nat64.into()) => Self::Nat64,
                intrinsic(binding, Int.into()) => Self::Int,
                intrinsic(binding, Int32.into()) => Self::Int32,
                intrinsic(binding, Int64.into()) => Self::Int64,
                intrinsic(binding, Text) => Self::Text,
                else => return None,
            },
            ExpressionKind::Application(application) => match &application.callee.value {
                ExpressionKind::Binding(binding) if known(binding, Option) => Self::Option(
                    Box::new(Self::from_expression(&application.argument, session)?),
                ),
                _ => return None,
            },
            _ => return None,
        })
    }

    fn into_expression(self, component: &Component, session: &BuildSession) -> Result<Expression> {
        use IntrinsicNumericType::*;
        use IntrinsicType::*;
        use KnownBinding::*;

        let intrinsic = |binding| session.look_up_intrinsic_type(binding, None);
        let known = |binding| session.look_up_known_binding(binding);

        match self {
            Self::Unit => known(Unit),
            Self::Bool => known(Bool),
            Self::Nat => intrinsic(Nat.into()),
            Self::Nat32 => intrinsic(Nat32.into()),
            Self::Nat64 => intrinsic(Nat64.into()),
            Self::Int => intrinsic(Int.into()),
            Self::Int32 => intrinsic(Int32.into()),
            Self::Int64 => intrinsic(Int64.into()),
            Self::Text => intrinsic(Text),
            Self::Option(type_) => Ok(application(
                known(Option)?,
                type_.into_expression(component, session)?,
            )),
        }
    }
}

/// An FFI-compatible value.
///
/// Except `Type` and `->`.
pub(crate) enum Value {
    Unit,
    Bool(bool),
    Text(String),
    Nat(Nat),
    Nat32(u32),
    Nat64(u64),
    Int(Int),
    Int32(i32),
    Int64(i64),
    Option {
        type_: Type,
        value: Option<Box<Value>>,
    },
    IO {
        index: usize,
        arguments: Vec<Value>,
    },
}

impl Value {
    pub(crate) fn from_expression(expression: &Expression, session: &BuildSession) -> Option<Self> {
        use ExpressionKind::*;
        use KnownBinding::*;

        let known = |binding: &hir::Binding, known: KnownBinding| {
            session
                .known_binding(known)
                .map_or(false, |known| &binding.0 == known)
        };

        Some(match &expression.value {
            Text(text) => {
                use hir::Text::*;

                match &**text {
                    // @Note not great
                    Text(text) => Self::Text(text.clone()),
                }
            }
            Number(number) => {
                use hir::Number::*;

                match &**number {
                    Nat(nat) => Self::Nat(nat.clone()),
                    Nat32(nat) => Self::Nat32(*nat),
                    Nat64(nat) => Self::Nat64(*nat),
                    Int(int) => Self::Int(int.clone()),
                    Int32(int) => Self::Int32(*int),
                    Int64(int) => Self::Int64(*int),
                }
            }
            Binding(binding) => condition! {
                known(binding, UnitUnit) => Value::Unit,
                known(binding, BoolFalse) => Value::Bool(false),
                known(binding, BoolTrue) => Value::Bool(true),
                else => return None,
            },

            Application(application0) => match &application0.callee.value {
                Binding(binding) if known(binding, OptionNone) => Value::Option {
                    value: None,
                    type_: self::Type::from_expression(&application0.argument, session)?,
                },
                Application(application1) => match &application1.callee.value {
                    Binding(binding) if known(binding, OptionSome) => Value::Option {
                        value: Some(Box::new(Self::from_expression(
                            &application0.argument,
                            session,
                        )?)),
                        type_: self::Type::from_expression(&application1.argument, session)?,
                    },
                    _ => return None,
                },
                _ => return None,
            },
            _ => return None,
        })
    }

    pub(crate) fn into_expression(
        self,
        component: &Component,
        session: &BuildSession,
    ) -> Result<Expression> {
        use hir::{Number::*, Text::*};

        Ok(match self {
            Self::Unit => session.look_up_known_binding(KnownBinding::Unit)?,
            Self::Bool(value) => session.look_up_known_binding(if value {
                KnownBinding::BoolTrue
            } else {
                KnownBinding::BoolFalse
            })?,
            Self::Text(value) => Expression::new(default(), default(), Text(value).into()),
            Self::Nat(value) => Expression::new(default(), default(), Nat(value).into()),
            Self::Nat32(value) => Expression::new(default(), default(), Nat32(value).into()),
            Self::Nat64(value) => Expression::new(default(), default(), Nat64(value).into()),
            Self::Int(value) => Expression::new(default(), default(), Int(value).into()),
            Self::Int32(value) => Expression::new(default(), default(), Int32(value).into()),
            Self::Int64(value) => Expression::new(default(), default(), Int64(value).into()),
            Self::Option { type_, value } => match value {
                Some(value) => application(
                    application(
                        session.look_up_known_binding(KnownBinding::OptionSome)?,
                        type_.into_expression(component, session)?,
                    ),
                    value.into_expression(component, session)?,
                ),
                None => application(
                    session.look_up_known_binding(KnownBinding::OptionNone)?,
                    type_.into_expression(component, session)?,
                ),
            },
            Self::IO { index, arguments } => Expression::new(
                default(),
                default(),
                hir::IO {
                    index,
                    arguments: arguments
                        .into_iter()
                        .map(|argument| argument.into_expression(component, session))
                        .collect::<Result<Vec<_>>>()?,
                }
                .into(),
            ),
        })
    }
}

/// Rust types that can be mapped to FFI-compatible lushui types.
///
/// This trait is not strictly necessary but it makes defining intrinsic functions on
/// the Rust side much more ergonomic!
pub(crate) trait IntoValue {
    fn into_type() -> Type;
    fn into_value(self) -> Value;
}

impl<V: IntoValue> From<V> for Value {
    fn from(value: V) -> Self {
        value.into_value()
    }
}

macro simple_value_correspondence($( $rust_type:ty => $lushui_type:ident ),+ $(,)?) {
    $(
        impl IntoValue for $rust_type {
            fn into_type() -> Type {
                Type::$lushui_type
            }

            fn into_value(self) -> Value {
                Value::$lushui_type(self)
            }
        }
    )+
}

simple_value_correspondence! {
    bool => Bool,
    String => Text,
    Nat => Nat,
    u32 => Nat32,
    u64 => Nat64,
    Int => Int,
    i32 => Int32,
    i64 => Int64,
}

impl IntoValue for () {
    fn into_type() -> Type {
        Type::Unit
    }

    fn into_value(self) -> Value {
        Value::Unit
    }
}

impl<V: IntoValue> IntoValue for Option<V> {
    fn into_type() -> Type {
        Type::Option(Box::new(V::into_type()))
    }

    fn into_value(self) -> Value {
        Value::Option {
            type_: V::into_type(),
            value: self.map(|value| Box::new(value.into())),
        }
    }
}

fn application(callee: Expression, argument: Expression) -> Expression {
    Expression::new(
        default(),
        default(),
        hir::Application {
            callee,
            argument,
            explicitness: Explicitness::Explicit,
        }
        .into(),
    )
}

macro pure(|$( $var:ident: $variant:ident ),*| $body:expr ) {
    IntrinsicFunctionValue {
        arity: count!($( $var )*),
        function: |arguments| {
            let mut arguments = arguments.into_iter();

            $(
                let $var = match arguments.next() {
                    Some(Value::$variant(value)) => value,
                    _ => unreachable!(),
                };
            )+

            $body.into()
        }
    }
}

macro count {
    () => { 0 },
    ($var:ident $( $rest:tt )*) => { 1 + count!($( $rest )*) },
}
