use super::{CrateIndex, Package, PackageIndex};
use crate::{
    diagnostics::{Code, Diagnostic, Reporter},
    entity::Entity,
    error::Result,
    hir::{expr, Constructor, Expression, ExpressionKind},
    resolver::{Crate, DeclarationIndex, Identifier},
    span::{Span, Spanning},
    syntax::{
        ast::{Explicitness, Path},
        lowered_ast::{Attribute, Attributes, Number},
    },
    utility::{condition, HashMap, Int, Nat},
};
use index_map::IndexMap;
use num_traits::{CheckedDiv, CheckedSub};
use std::{
    borrow::Cow,
    default::default,
    fmt,
    ops::{Index, IndexMut},
    str::FromStr,
};

#[derive(Default)]
pub struct BuildSession {
    built_crates: HashMap<CrateIndex, Crate>,
    built_packages: IndexMap<PackageIndex, Package>,
    known_bindings: HashMap<KnownBinding, Identifier>,
    intrinsic_types: HashMap<IntrinsicType, Identifier>,
    // @Temporary type
    intrinsic_functions: HashMap<IntrinsicFunction, (Option<Identifier>, IntrinsicFunctionValue)>,
}

impl BuildSession {
    pub fn with_packages(packages: IndexMap<PackageIndex, Package>) -> Self {
        Self {
            built_packages: packages,
            intrinsic_functions: intrinsic_functions(),
            ..default()
        }
    }

    pub fn add(&mut self, crate_: Crate) {
        self.built_crates.insert(crate_.index, crate_);
    }

    pub fn known_binding(&self, known: KnownBinding) -> Option<&Identifier> {
        self.known_bindings.get(&known)
    }

    pub fn look_up_known_binding(
        &self,
        known: KnownBinding,
        reporter: &Reporter,
    ) -> Result<Expression> {
        Ok(self
            .known_binding(known)
            .cloned()
            .ok_or_else(|| Diagnostic::missing_known_binding(known).report(reporter))?
            .to_expression())
    }

    pub fn look_up_known_type(
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
            .to_expression())
    }

    pub fn register_known_type<'a>(
        &mut self,
        binder: &Identifier,
        constructors: Vec<&'a Constructor>,
        attribute: &Attribute,
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
                .labeled_secondary_span(attribute, "marks the binding as known to the compiler")
                .report(reporter);
            return Err(());
        };

        if let Some(previous) = self.known_bindings.get(&binding) {
            duplicate(previous).report(reporter);
            return Err(());
        }

        self.known_bindings.insert(binding, binder.clone());

        let mut constructor = |known: KnownBinding| {
            if let Some(constructor) = constructors
                .iter()
                .find(|constructor| constructor.binder.as_str() == known.name())
            {
                self.known_bindings
                    .insert(known, constructor.binder.clone());
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

    pub fn intrinsic_type(&self, intrinsic: IntrinsicType) -> Option<&Identifier> {
        self.intrinsic_types.get(&intrinsic)
    }

    // @Beacon @Task support paths!
    pub fn register_intrinsic_type(
        &mut self,
        binder: Identifier,
        attribute: &Attribute,
        reporter: &Reporter,
    ) -> Result {
        let Ok(intrinsic) = binder.as_str().parse::<IntrinsicType>() else {
            Diagnostic::unrecognized_intrinsic_binding(&binder.source.clone().into(), IntrinsicKind::Type)
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

    pub fn intrinsic_function(
        &self,
        intrinsic: IntrinsicFunction,
    ) -> &(Option<Identifier>, IntrinsicFunctionValue) {
        &self.intrinsic_functions[&intrinsic]
    }

    pub fn register_intrinsic_function(
        &mut self,
        name: Cow<'_, Path>,
        style: IntrinsicNameStyle<'_>,
        binder: Identifier,
        reporter: &Reporter,
    ) -> Result<IntrinsicFunction, ()> {
        let Ok(intrinsic) = IntrinsicFunction::parse(&name) else {
            let diagnostic = Diagnostic::unrecognized_intrinsic_binding(&name, IntrinsicKind::Function);

            match style {
                // @Task use diagnostic suggestion API once available
                IntrinsicNameStyle::Implicit { attribute } => diagnostic
                    .labeled_primary_span(&binder, "unrecognized intrinsic")
                    .labeled_secondary_span(attribute, "marks the binding as intrinsic to the language")
                    .note(format!("the name of the alleged intrinsic, `{name}`, is derived from the absolute path of `{binder}`"))
                    .help("consider adding an explicit name to the attribute `diagnostic` to overwrite"),
                IntrinsicNameStyle::Explicit => diagnostic
                    .primary_span(name.span()),
            }
            .report(reporter);

            return Err(());
        };

        let function = self.intrinsic_function(intrinsic);

        if let Some(previous) = &function.0 {
            Diagnostic::error()
                .code(Code::E040)
                .message(format!(
                    "the intrinsic function `{intrinsic}` is defined multiple times",
                ))
                .labeled_primary_span(&binder, "conflicting definition")
                .labeled_secondary_span(previous, "previous definition")
                .report(reporter);
            return Err(());
        };

        // @Beacon @Beacon @Beacon
        // @Beacon @Beacon @Beacon @Task dont just store the binder at 0 but dep on the style the path or the binder
        self.intrinsic_functions.get_mut(&intrinsic).unwrap().0 = Some(binder);

        Ok(intrinsic)
    }

    pub fn look_up_intrinsic_type(
        &self,
        intrinsic: IntrinsicType,
        expression: Option<Span>,
        reporter: &Reporter,
    ) -> Result<Expression> {
        if let Some(intrinsic) = self.intrinsic_type(intrinsic) {
            return Ok(intrinsic.clone().to_expression());
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
        &self.built_crates[&index.crate_index()][index.local_index()]
    }
}

impl Index<CrateIndex> for BuildSession {
    type Output = Crate;

    fn index(&self, index: CrateIndex) -> &Self::Output {
        &self.built_crates[&index]
    }
}

impl Index<PackageIndex> for BuildSession {
    type Output = Package;

    fn index(&self, index: PackageIndex) -> &Self::Output {
        &self.built_packages[index]
    }
}

impl IndexMut<PackageIndex> for BuildSession {
    fn index_mut(&mut self, index: PackageIndex) -> &mut Self::Output {
        &mut self.built_packages[index]
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

    fn unrecognized_intrinsic_binding(name: &Path, kind: IntrinsicKind) -> Self {
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
pub enum KnownBinding {
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
    pub const fn name(&self) -> &'static str {
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

    pub const fn path(&self) -> &'static str {
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

pub enum IntrinsicNameStyle<'a> {
    /// Intrinsic binding is marked `@intrinsic`.
    Implicit { attribute: &'a Attribute },
    /// Intrinsic binding is marked `@(intrinsic qualified.name)`.
    Explicit,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum IntrinsicType {
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
    pub fn numeric(number: &Number) -> Self {
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
pub enum IntrinsicFunction {
    NatAdd,
    NatSubtract,
    NatUncheckedSubtract,
    NatMultiply,
    NatDivide,
    NatEqual,
    NatLess,
    NatLessEqual,
    NatGreater,
    NatGreaterEqual,
    NatDisplay,
    TextConcat,
    Nat32Add,
}

impl IntrinsicFunction {
    fn parse(name: &Path) -> Result<Self, ()> {
        if name.hanger.is_some() {
            return Err(());
        }

        Ok(
            match &*name
                .segments
                .iter()
                .map(|identifier| identifier.as_str())
                .collect::<Vec<_>>()
            {
                ["nat", "add"] => Self::NatAdd,
                ["nat", "subtract"] => Self::NatSubtract,
                ["nat", "unchecked-subtract"] => Self::NatUncheckedSubtract,
                ["nat", "multiply"] => Self::NatMultiply,
                ["nat", "divide"] => Self::NatDivide,
                ["nat", "equal"] => Self::NatEqual,
                ["nat", "less"] => Self::NatLess,
                ["nat", "less-equal"] => Self::NatLessEqual,
                ["nat", "greater"] => Self::NatGreater,
                ["nat", "greater-equal"] => Self::NatGreaterEqual,
                ["nat", "display"] => Self::NatDisplay,
                ["text", "concat"] => Self::TextConcat,
                ["nat32", "add"] => Self::Nat32Add,
                _ => return Err(()),
            },
        )
    }
}

// @Task derive this smh
impl fmt::Display for IntrinsicFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::NatAdd => "nat.add",
                Self::NatSubtract => "nat.subtract",
                Self::NatUncheckedSubtract => "nat.unchecked-subtract",
                Self::NatMultiply => "nat.multiply",
                Self::NatDivide => "nat.divide",
                Self::NatEqual => "nat.equal",
                Self::NatLess => "nat.less",
                Self::NatLessEqual => "nat.less-equal",
                Self::NatGreater => "nat.greater",
                Self::NatGreaterEqual => "nat.greater-equal",
                Self::NatDisplay => "nat.display",
                Self::TextConcat => "text.concat",
                Self::Nat32Add => "nat32.add",
            }
        )
    }
}

pub type IntrinsicFunctionPointer = fn(arguments: Vec<Value>) -> Value;

#[derive(Clone, Copy)]
pub struct IntrinsicFunctionValue {
    pub arity: usize,
    pub function: IntrinsicFunctionPointer,
}

// @Question naming etc
pub enum IntrinsicKind {
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

// @Temporary ret ty
fn intrinsic_functions() -> HashMap<IntrinsicFunction, (Option<Identifier>, IntrinsicFunctionValue)>
{
    use IntrinsicFunction::*;

    let mut intrinsics = HashMap::default();

    intrinsics.insert(NatAdd, (None, pure!(|x: Nat, y: Nat| x + y)));
    intrinsics.insert(
        NatSubtract,
        (None, pure!(|x: Nat, y: Nat| x.checked_sub(&y))),
    );
    intrinsics.insert(NatUncheckedSubtract, (None, pure!(|x: Nat, y: Nat| x - y)));
    intrinsics.insert(NatMultiply, (None, pure!(|x: Nat, y: Nat| x * y)));
    intrinsics.insert(NatDivide, (None, pure!(|x: Nat, y: Nat| x.checked_div(&y))));
    intrinsics.insert(NatEqual, (None, pure!(|x: Nat, y: Nat| x == y)));
    intrinsics.insert(NatLess, (None, pure!(|x: Nat, y: Nat| x < y)));
    intrinsics.insert(NatLessEqual, (None, pure!(|x: Nat, y: Nat| x <= y)));
    intrinsics.insert(NatGreater, (None, pure!(|x: Nat, y: Nat| x > y)));
    intrinsics.insert(NatGreaterEqual, (None, pure!(|x: Nat, y: Nat| x >= y)));
    intrinsics.insert(NatDisplay, (None, pure!(|x: Nat| x.to_string())));
    intrinsics.insert(TextConcat, (None, pure!(|a: Text, b: Text| a + &b)));
    intrinsics.insert(Nat32Add, (None, pure!(|a: Nat32, b: Nat32| a + b)));

    intrinsics
}

/// An FFI-compatible type.
///
/// Except `Type` and `->`.
pub enum Type {
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

        let known = |binding: &crate::hir::Binding, known: KnownBinding| {
            session
                .known_binding(known)
                .map_or(false, |known| &binding.binder == known)
        };
        let intrinsic = |binding: &crate::hir::Binding, intrinsic: IntrinsicType| {
            session
                .intrinsic_type(intrinsic)
                .map_or(false, |intrinsic| &binding.binder == intrinsic)
        };

        Some(match &expression.data {
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
            ExpressionKind::Application(application) => match &application.callee.data {
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
        crate_: &Crate,
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
                type_.into_expression(crate_, session, reporter)?,
            )),
        }
    }
}

/// An FFI-compatible value.
///
/// Except `Type` and `->`.
pub enum Value {
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
    pub fn from_expression(expression: &Expression, session: &BuildSession) -> Option<Self> {
        use ExpressionKind::*;
        use KnownBinding::*;

        let known = |binding: &crate::hir::Binding, known: KnownBinding| {
            session
                .known_binding(known)
                .map_or(false, |known| &binding.binder == known)
        };

        Some(match &expression.data {
            Text(text) => Self::Text(text.as_ref().clone()),
            Number(number) => {
                use crate::syntax::lowered_ast::Number::*;

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

            Application(application0) => match &application0.callee.data {
                Binding(binding) if known(binding, OptionNone) => Value::Option {
                    value: None,
                    type_: self::Type::from_expression(&application0.argument, session)?,
                },
                Application(application1) => match &application1.callee.data {
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

    pub fn into_expression(
        self,
        crate_: &Crate,
        session: &BuildSession,
        reporter: &Reporter,
    ) -> Result<Expression> {
        use crate::syntax::lowered_ast::Number::*;

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
            Self::Text(value) => expr! { Text(Attributes::default(), Span::default(); value) },
            Self::Nat(value) => expr! {
                Number(Attributes::default(), Span::default();
                Nat(value))
            },
            Self::Nat32(value) => expr! {
                Number(Attributes::default(), Span::default();
                Nat32(value))
            },
            Self::Nat64(value) => expr! {
                Number(Attributes::default(), Span::default();
                Nat64(value))
            },
            Self::Int(value) => expr! {
                Number(Attributes::default(), Span::default();
                Int(value))
            },
            Self::Int32(value) => expr! {
                Number(Attributes::default(), Span::default();
                Int32(value))
            },
            Self::Int64(value) => expr! {
                Number(Attributes::default(), Span::default();
                Int64(value))
            },
            Self::Option { type_, value } => match value {
                Some(value) => application(
                    application(
                        session.look_up_known_binding(KnownBinding::OptionSome, reporter)?,
                        type_.into_expression(crate_, session, reporter)?,
                    ),
                    value.into_expression(crate_, session, reporter)?,
                ),
                None => application(
                    session.look_up_known_binding(KnownBinding::OptionNone, reporter)?,
                    type_.into_expression(crate_, session, reporter)?,
                ),
            },
            Self::IO { index, arguments } => expr! {
                IO {
                    Attributes::default(), Span::default();
                    index,
                    arguments: arguments.into_iter()
                        .map(|argument| argument.into_expression(crate_, session, reporter))
                        .collect::<Result<Vec<_>>>()?,
                }
            },
        })
    }
}

/// Rust types that can be mapped to FFI-compatible lushui types.
///
/// This trait is not strictly necessary but it makes defining intrinsic functions on
/// the Rust side much more ergonomic!
pub trait IntoValue {
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
    expr! {
        Application {
            Attributes::default(), Span::default();
            callee,
            argument,
            explicitness: Explicitness::Explicit
        }
    }
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
