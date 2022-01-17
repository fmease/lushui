use super::{CapsuleIndex, Package, PackageIndex};
use crate::{
    diagnostics::{Code, Diagnostic, Reporter},
    entity::{Entity, EntityKind},
    error::Result,
    hir::{self, DeclarationIndex, Expression, ExpressionKind, Identifier, Number},
    resolver::Capsule,
    span::Span,
    syntax::ast::Explicitness,
    utility::{condition, HashMap, Int, Nat},
};
use index_map::IndexMap;
use num_traits::{CheckedDiv, CheckedSub};
use std::{
    default::default,
    fmt,
    ops::{Index, IndexMut},
    path::PathBuf,
    str::FromStr,
};

const BUILD_FOLDER_NAME: &str = "build";

pub struct BuildSession {
    /// The capsules which have already been built.
    capsules: HashMap<CapsuleIndex, Capsule>,
    packages: IndexMap<PackageIndex, Package>,
    goal_capsule: CapsuleIndex,
    goal_package: PackageIndex,
    known_bindings: HashMap<KnownBinding, Identifier>,
    intrinsic_types: HashMap<IntrinsicType, Identifier>,
    intrinsic_functions: HashMap<IntrinsicFunction, IntrinsicFunctionValue>,
}

impl BuildSession {
    /// Create a new build session with intrinsic functions registered.
    pub(crate) fn new(
        packages: IndexMap<PackageIndex, Package>,
        goal_capsule: CapsuleIndex,
        goal_package: PackageIndex,
    ) -> Self {
        Self {
            capsules: default(),
            packages,
            goal_capsule,
            goal_package,
            known_bindings: default(),
            intrinsic_types: default(),
            intrinsic_functions: intrinsic_functions(),
        }
    }

    #[cfg(test)]
    pub(crate) fn empty(goal_capsule: CapsuleIndex, goal_package: PackageIndex) -> Self {
        Self {
            capsules: default(),
            packages: default(),
            goal_capsule,
            goal_package,
            known_bindings: default(),
            intrinsic_types: default(),
            intrinsic_functions: default(),
        }
    }

    pub fn goal_package(&self) -> PackageIndex {
        self.goal_package
    }

    pub fn goal_capsule(&self) -> CapsuleIndex {
        self.goal_capsule
    }

    /// The path to the folder containing the build artifacts and the documentation.
    pub(crate) fn build_folder(&self) -> PathBuf {
        // @Beacon @Beacon @Bug does not work with single-file packages!
        self[self.goal_package].path.join(BUILD_FOLDER_NAME)
    }

    pub fn add(&mut self, capsule: Capsule) {
        self.capsules.insert(capsule.index(), capsule);
    }

    pub(crate) fn known_binding(&self, known: KnownBinding) -> Option<&Identifier> {
        self.known_bindings.get(&known)
    }

    pub(crate) fn look_up_known_binding(
        &self,
        known: KnownBinding,
        reporter: &Reporter,
    ) -> Result<Expression> {
        Ok(self
            .known_binding(known)
            .cloned()
            .ok_or_else(|| Diagnostic::missing_known_binding(known).report(reporter))?
            .into_expression())
    }

    pub(crate) fn look_up_known_type(
        &self,
        known: KnownBinding,
        expression: Span,
        reporter: &Reporter,
    ) -> Result<Expression> {
        Ok(self
            .known_binding(known)
            .cloned()
            .ok_or_else(|| {
                Diagnostic::missing_known_type(known, expression).report(reporter);
            })?
            .into_expression())
    }

    pub(crate) fn register_known_type(
        &mut self,
        binder: &Identifier,
        constructors: &[&Identifier],
        attribute: Span,
        reporter: &Reporter,
    ) -> Result {
        use KnownBinding::*;

        let duplicate = |previous: &Identifier| {
            Diagnostic::error()
                .code(Code::E039)
                .message(format!(
                    "the known binding `{}` is defined multiple times",
                    binder
                ))
                .labeled_primary_span(binder, "conflicting definition")
                .labeled_secondary_span(previous, "previous definition")
        };

        let Ok(binding) = binder.as_str().parse() else {
            Diagnostic::error()
                .code(Code::E063)
                .message(format!("`{}` is not a known binding", binder))
                .primary_span(binder)
                .secondary_span(attribute)
                .report(reporter);
            return Err(());
        };

        if let Some(previous) = self.known_bindings.get(&binding) {
            duplicate(previous).report(reporter);
            return Err(());
        }

        self.known_bindings.insert(binding, binder.clone());

        let mut constructor = |known: KnownBinding| {
            if let Some(&constructor) = constructors
                .into_iter()
                .find(|constructor| constructor.as_str() == known.name())
            {
                self.known_bindings.insert(known, constructor.clone());
            }
        };

        match binding {
            Unit => {
                constructor(UnitUnit);
            }
            Bool => {
                constructor(BoolFalse);
                constructor(BoolTrue);
            }
            Option => {
                constructor(OptionNone);
                constructor(OptionSome);
            }
            _ => {}
        }

        Ok(())
    }

    pub(crate) fn intrinsic_type(&self, intrinsic: IntrinsicType) -> Option<&Identifier> {
        self.intrinsic_types.get(&intrinsic)
    }

    // @Beacon @Task support paths!
    pub(crate) fn register_intrinsic_type(
        &mut self,
        binder: Identifier,
        attribute: Span,
        reporter: &Reporter,
    ) -> Result {
        let Ok(intrinsic) = binder.as_str().parse::<IntrinsicType>() else {
            Diagnostic::unrecognized_intrinsic_binding(binder.as_str(), IntrinsicKind::Type)
                .primary_span(&binder)
                .secondary_span(attribute)
                .report(reporter);
            return Err(());
        };

        if let Some(previous) = self.intrinsic_type(intrinsic) {
            Diagnostic::error()
                .code(Code::E040)
                .message(format!(
                    "the intrinsic type `{intrinsic}` is defined multiple times",
                ))
                .labeled_primary_span(&binder, "conflicting definition")
                .labeled_secondary_span(previous as &_, "previous definition")
                .report(reporter);
            return Err(());
        }

        self.intrinsic_types.insert(intrinsic, binder);

        Ok(())
    }

    pub(crate) fn register_intrinsic_function(
        &mut self,
        binder: Identifier,
        type_: Expression,
        attribute: Span,
        reporter: &Reporter,
    ) -> Result<EntityKind, ()> {
        let Ok(intrinsic) = binder.as_str().parse() else {
            Diagnostic::unrecognized_intrinsic_binding(binder.as_str(), IntrinsicKind::Function)
                .primary_span(&binder)
                .secondary_span(attribute)
                .report(reporter);
            return Err(());
        };

        // @Task explain why we remove here
        // @Task explain why unwrap
        let function = self.intrinsic_functions.remove(&intrinsic).unwrap();

        Ok(EntityKind::Intrinsic {
            type_,
            arity: function.arity,
            function: function.function,
        })
    }

    pub(crate) fn look_up_intrinsic_type(
        &self,
        intrinsic: IntrinsicType,
        expression: Option<Span>,
        reporter: &Reporter,
    ) -> Result<Expression> {
        if let Some(intrinsic) = self.intrinsic_type(intrinsic) {
            return Ok(intrinsic.clone().into_expression());
        }

        Diagnostic::missing_intrinsic_binding(intrinsic, IntrinsicKind::Type)
            .if_present(expression, |diagnostic, span| {
                diagnostic.labeled_primary_span(span, "the type of this expression")
            })
            .report(reporter);

        Err(())
    }
}

impl Index<DeclarationIndex> for BuildSession {
    type Output = Entity;

    fn index(&self, index: DeclarationIndex) -> &Self::Output {
        &self[index.capsule()][index.local_unchecked()]
    }
}

impl Index<CapsuleIndex> for BuildSession {
    type Output = Capsule;

    fn index(&self, index: CapsuleIndex) -> &Self::Output {
        self.capsules.get(&index).unwrap_or_else(|| {
            panic!("attempt to look up unbuilt or unknown capsule `{index:?}` in the build session")
        })
    }
}

impl Index<PackageIndex> for BuildSession {
    type Output = Package;

    fn index(&self, index: PackageIndex) -> &Self::Output {
        &self.packages[index]
    }
}

impl IndexMut<PackageIndex> for BuildSession {
    fn index_mut(&mut self, index: PackageIndex) -> &mut Self::Output {
        &mut self.packages[index]
    }
}

impl Diagnostic {
    fn missing_known_binding(binding: KnownBinding) -> Self {
        Self::error()
            .code(Code::E062)
            .message(format!("the known binding `{}` is missing", binding.path()))
    }

    fn missing_known_type(binding: KnownBinding, expression: Span) -> Self {
        Self::missing_known_binding(binding)
            .labeled_primary_span(expression, "the type of this expression")
    }

    fn unrecognized_intrinsic_binding(name: &str, kind: IntrinsicKind) -> Self {
        Self::error()
            .code(Code::E061)
            .message(format!("`{name}` is not an intrinsic {kind}"))
    }

    fn missing_intrinsic_binding(intrinsic: IntrinsicType, kind: IntrinsicKind) -> Self {
        Self::error()
            .code(Code::E060)
            .message(format!("the intrinsic {kind} `{intrinsic}` is missing"))
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
    // @Task derive this
    pub(crate) const fn name(self) -> &'static str {
        match self {
            Self::Unit => "Unit",
            Self::UnitUnit => "unit",
            Self::Bool => "Bool",
            Self::BoolFalse => "false",
            Self::BoolTrue => "true",
            Self::Option => "Option",
            Self::OptionNone => "none",
            Self::OptionSome => "some",
        }
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

impl FromStr for KnownBinding {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(match input {
            "Unit" => Self::Unit,
            "unit" => Self::UnitUnit,
            "Bool" => Self::Bool,
            "false" => Self::BoolFalse,
            "true" => Self::BoolTrue,
            "Option" => Self::Option,
            "none" => Self::OptionNone,
            "some" => Self::OptionSome,
            _ => return Err(()),
        })
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) enum IntrinsicType {
    Nat,
    Nat32,
    Nat64,
    Int,
    Int32,
    Int64,
    Text,
    IO,
}

impl IntrinsicType {
    pub(crate) fn numeric(number: &Number) -> Self {
        match number {
            Number::Nat(_) => Self::Nat,
            Number::Nat32(_) => Self::Nat32,
            Number::Nat64(_) => Self::Nat64,
            Number::Int(_) => Self::Int,
            Number::Int32(_) => Self::Int32,
            Number::Int64(_) => Self::Int64,
        }
    }
}

impl FromStr for IntrinsicType {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(match input {
            "Nat" => Self::Nat,
            "Nat32" => Self::Nat32,
            "Nat64" => Self::Nat64,
            "Int" => Self::Int,
            "Int32" => Self::Int32,
            "Int64" => Self::Int64,
            "Text" => Self::Text,
            "IO" => Self::IO,
            _ => return Err(()),
        })
    }
}

// @Task derive this
impl fmt::Display for IntrinsicType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Nat => "Nat",
                Self::Nat32 => "Nat32",
                Self::Nat64 => "Nat64",
                Self::Int => "Int",
                Self::Int32 => "Int32",
                Self::Int64 => "Int64",
                Self::Text => "Text",
                Self::IO => "IO",
            }
        )
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) enum IntrinsicFunction {
    Add,
    Subtract,
    // @Temporary existence
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

// @Beacon @Beacon @Beacon @Temporary
// @Task derive this
impl FromStr for IntrinsicFunction {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(match input {
            "add" => Self::Add,
            "subtract" => Self::Subtract,
            "panicking-subtract" => Self::PanickingSubtract,
            "multiply" => Self::Multiply,
            "divide" => Self::Divide,
            "equal" => Self::Equal,
            "less" => Self::Less,
            "less-equal" => Self::LessEqual,
            "greater" => Self::Greater,
            "greater-equal" => Self::GreaterEqual,
            "display" => Self::Display,
            "concat" => Self::Concat,
            "add-nat32" => Self::AddNat32,
            "print" => Self::Print,
            _ => return Err(()),
        })
    }
}

// @Task derive this
impl fmt::Display for IntrinsicFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Add => "add",
                Self::Subtract => "subtract",
                Self::PanickingSubtract => "panicking-subtract",
                Self::Multiply => "multiply",
                Self::Divide => "divide",
                Self::Equal => "equal",
                Self::Less => "less",
                Self::LessEqual => "less-equal",
                Self::Greater => "greater",
                Self::GreaterEqual => "greater-equal",
                Self::Display => "display",
                Self::Concat => "concat",
                Self::AddNat32 => "add-nat32",
                Self::Print => "print",
            }
        )
    }
}

pub(crate) type BareIntrinsicFunctionValue = fn(arguments: Vec<Value>) -> Value;

pub(crate) struct IntrinsicFunctionValue {
    pub(crate) arity: usize,
    pub(crate) function: BareIntrinsicFunctionValue,
}

// @Question naming etc
pub(crate) enum IntrinsicKind {
    Type,
    Function,
}

impl fmt::Display for IntrinsicKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Type => "type",
                Self::Function => "function",
            }
        )
    }
}

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
                intrinsic(binding, Nat) => Self::Nat,
                intrinsic(binding, Nat32) => Self::Nat32,
                intrinsic(binding, Nat64) => Self::Nat64,
                intrinsic(binding, Int) => Self::Int,
                intrinsic(binding, Int32) => Self::Int32,
                intrinsic(binding, Int64) => Self::Int64,
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

    fn into_expression(
        self,
        capsule: &Capsule,
        session: &BuildSession,
        reporter: &Reporter,
    ) -> Result<Expression> {
        use IntrinsicType::*;
        use KnownBinding::*;

        let intrinsic = |binding| session.look_up_intrinsic_type(binding, None, reporter);
        let known = |binding| session.look_up_known_binding(binding, reporter);

        match self {
            Self::Unit => known(Unit),
            Self::Bool => known(Bool),
            Self::Nat => intrinsic(Nat),
            Self::Nat32 => intrinsic(Nat32),
            Self::Nat64 => intrinsic(Nat64),
            Self::Int => intrinsic(Int),
            Self::Int32 => intrinsic(Int32),
            Self::Int64 => intrinsic(Int64),
            Self::Text => intrinsic(Text),
            Self::Option(type_) => Ok(application(
                known(Option)?,
                type_.into_expression(capsule, session, reporter)?,
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
        capsule: &Capsule,
        session: &BuildSession,
        reporter: &Reporter,
    ) -> Result<Expression> {
        use hir::{Number::*, Text::*};

        Ok(match self {
            Self::Unit => session.look_up_known_binding(KnownBinding::Unit, reporter)?,
            Self::Bool(value) => session.look_up_known_binding(
                if value {
                    KnownBinding::BoolTrue
                } else {
                    KnownBinding::BoolFalse
                },
                reporter,
            )?,
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
                        session.look_up_known_binding(KnownBinding::OptionSome, reporter)?,
                        type_.into_expression(capsule, session, reporter)?,
                    ),
                    value.into_expression(capsule, session, reporter)?,
                ),
                None => application(
                    session.look_up_known_binding(KnownBinding::OptionNone, reporter)?,
                    type_.into_expression(capsule, session, reporter)?,
                ),
            },
            Self::IO { index, arguments } => Expression::new(
                default(),
                default(),
                hir::IO {
                    index,
                    arguments: arguments
                        .into_iter()
                        .map(|argument| argument.into_expression(capsule, session, reporter))
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
