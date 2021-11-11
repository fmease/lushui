// @Note all of this is ugly as hell!

use super::{Crate, Expression};
use crate::{
    diagnostics::{Code, Diagnostic, Reporter},
    error::Result,
    hir::{expr, Constructor, ExpressionKind},
    package::BuildSession,
    resolver::Identifier,
    span::Span,
    syntax::{
        ast::Explicit,
        lowered_ast::{Attribute, Attributes},
    },
    typer::Declaration,
    utility::{HashMap, Int, Nat},
};

// @Note ugly data structures in use
#[derive(Default)]
pub struct Scope {
    /// Foreign types (`@foreign data`).
    pub foreign_types: HashMap<&'static str, Option<Identifier>>,
    /// Foreign bindings (`@foreign`).
    pub foreign_bindings: HashMap<&'static str, ForeignFunction>,
    /// Inherent values (`@inherent`)
    pub(in crate::typer) inherent_values: InherentValueMap,
    /// Inherent types (`@inherent`)
    pub(in crate::typer) inherent_types: InherentTypeMap,
    pub(super) _runners: IndexMap<IOIndex, IORunner>,
}

impl Scope {
    pub(in crate::typer) fn register_inherent_bindings<'a>(
        &mut self,
        binder: &Identifier,
        mut constructors: impl Iterator<Item = &'a Constructor>,
        declaration: &Declaration,
        attribute: &Attribute,
        reporter: &Reporter,
    ) -> Result {
        // @Task link to previous definition
        let duplicate = || {
            Diagnostic::error()
                .code(Code::E020)
                .message(format!(
                    "`{}` is defined multiple times as inherent",
                    binder
                ))
                .primary_span(declaration)
        };

        let mut find = |value_name, inherent: &mut Option<_>| {
            if let Some(constructor) =
                constructors.find(|constructor| constructor.binder.as_str() == value_name)
            {
                *inherent = Some(constructor.binder.clone().stripped());
            }
        };

        match binder.as_str() {
            Type::UNIT => {
                if self.inherent_types.unit.is_some() {
                    duplicate().report(reporter);
                    return Err(());
                }

                self.inherent_types.unit = Some(binder.clone().stripped());
                find(Value::UNIT, &mut self.inherent_values.unit);
            }
            Type::BOOL => {
                if self.inherent_types.bool.is_some() {
                    duplicate().report(reporter);
                    return Err(());
                }

                self.inherent_types.bool = Some(binder.clone().stripped());
                find(Value::FALSE, &mut self.inherent_values.false_);
                find(Value::TRUE, &mut self.inherent_values.true_);
            }
            Type::OPTION => {
                if self.inherent_types.option.is_some() {
                    duplicate().report(reporter);
                    return Err(());
                }

                self.inherent_types.option = Some(binder.clone().stripped());
                find(Value::NONE, &mut self.inherent_values.none);
                find(Value::SOME, &mut self.inherent_values.some);
            }
            _ => {
                Diagnostic::error()
                    .code(Code::E062)
                    .message(format!("`{}` is not an inherent type", binder))
                    .primary_span(attribute)
                    .labeled_secondary_span(declaration, "ascribed to this declaration")
                    .report(reporter);
                return Err(());
            }
        }

        Ok(())
    }
}

pub type NakedForeignFunction = fn(arguments: Vec<Value>) -> Value;

pub struct ForeignFunction {
    pub(super) arity: usize,
    pub(super) function: NakedForeignFunction,
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

names! {
    Type::{
        UNIT = "Unit",
        BOOL = "Bool",
        NAT = "Nat",
        NAT32 = "Nat32",
        NAT64 = "Nat64",
        INT = "Int",
        INT32 = "Int32",
        INT64 = "Int64",
        TEXT = "Text",
        OPTION = "Option",
        IO = "IO",
    }
}

impl Type {
    fn from_expression(expression: &Expression, scope: &Scope) -> Option<Self> {
        let types = &scope.inherent_types;

        use ExpressionKind::*;

        Some(match &expression.data {
            Binding(binding) if matches(&binding.binder, &types.unit)? => Self::Unit,
            Binding(binding) if matches(&binding.binder, &types.bool)? => Self::Bool,
            Binding(binding)
                if matches(&binding.binder, &scope.foreign_types[self::Type::NAT])? =>
            {
                Self::Nat
            }
            Binding(binding)
                if matches(&binding.binder, &scope.foreign_types[self::Type::NAT32])? =>
            {
                Self::Nat32
            }
            Binding(binding)
                if matches(&binding.binder, &scope.foreign_types[self::Type::NAT64])? =>
            {
                Self::Nat64
            }
            Binding(binding)
                if matches(&binding.binder, &scope.foreign_types[self::Type::INT])? =>
            {
                Self::Int
            }
            Binding(binding)
                if matches(&binding.binder, &scope.foreign_types[self::Type::INT32])? =>
            {
                Self::Int32
            }
            Binding(binding)
                if matches(&binding.binder, &scope.foreign_types[self::Type::INT64])? =>
            {
                Self::Int64
            }
            Binding(binding)
                if matches(&binding.binder, &scope.foreign_types[self::Type::TEXT])? =>
            {
                Self::Text
            }
            Application(application) => match &application.callee.data {
                Binding(binding) if matches(&binding.binder, &types.option)? => Self::Option(
                    Box::new(self::Type::from_expression(&application.argument, scope)?),
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
        match self {
            Self::Unit => crate_.look_up_unit_type(None, reporter),
            Self::Bool => crate_.look_up_bool_type(None, reporter),
            Self::Nat => crate_.look_up_foreign_type(Type::NAT, None, session, reporter),
            Self::Nat32 => crate_.look_up_foreign_type(Type::NAT32, None, session, reporter),
            Self::Nat64 => crate_.look_up_foreign_type(Type::NAT64, None, session, reporter),
            Self::Int => crate_.look_up_foreign_type(Type::INT, None, session, reporter),
            Self::Int32 => crate_.look_up_foreign_type(Type::INT32, None, session, reporter),
            Self::Int64 => crate_.look_up_foreign_type(Type::INT64, None, session, reporter),
            Self::Text => crate_.look_up_foreign_type(Type::TEXT, None, session, reporter),
            Self::Option(type_) => Ok(application(
                crate_.look_up_option_type(None, reporter)?,
                type_.into_expression(crate_, session, reporter)?,
            )),
        }
    }
}

// @Task smh merge InherentTypeMap and InherentValueMap without creating too much boilerplate
#[derive(Default, Debug)]
pub struct InherentTypeMap {
    pub unit: Option<Identifier>,
    pub bool: Option<Identifier>,
    pub option: Option<Identifier>,
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

names! {
    Value::{
        UNIT = "unit",
        FALSE = "false",
        TRUE = "true",
        NONE = "none",
        SOME = "some",
    }
}

impl Value {
    pub fn from_expression(expression: &Expression, scope: &Scope) -> Option<Self> {
        let values = &scope.inherent_values;

        use ExpressionKind::*;

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
            Binding(binding) if matches(&binding.binder, &values.unit)? => Value::Unit,
            Binding(binding) if matches(&binding.binder, &values.false_)? => Value::Bool(false),
            Binding(binding) if matches(&binding.binder, &values.true_)? => Value::Bool(true),
            Application(application0) => match &application0.callee.data {
                Binding(binding) if matches(&binding.binder, &values.none)? => Value::Option {
                    value: None,
                    type_: self::Type::from_expression(&application0.argument, scope)?,
                },
                Application(application1) => match &application1.callee.data {
                    Binding(binding) if matches(&binding.binder, &values.some)? => Value::Option {
                        value: Some(Box::new(Self::from_expression(
                            &application0.argument,
                            scope,
                        )?)),
                        type_: self::Type::from_expression(&application1.argument, scope)?,
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
        let values = &crate_.ffi.inherent_values;

        fn missing_inherent() -> Diagnostic {
            // @Task message, span
            Diagnostic::error().code(Code::E063)
        }

        use crate::syntax::lowered_ast::Number::*;

        Ok(match self {
            Self::Unit => values
                .unit
                .clone()
                .ok_or_else(|| missing_inherent().report(reporter))?
                .to_expression(),
            Self::Bool(value) => (if value { &values.true_ } else { &values.false_ }
                .clone()
                .ok_or_else(|| missing_inherent().report(reporter))?)
            .to_expression(),
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
                        values
                            .some
                            .clone()
                            .ok_or_else(|| missing_inherent().report(reporter))?
                            .to_expression(),
                        type_.into_expression(crate_, session, reporter)?,
                    ),
                    value.into_expression(crate_, session, reporter)?,
                ),
                None => application(
                    values
                        .none
                        .clone()
                        .ok_or_else(|| missing_inherent().report(reporter))?
                        .to_expression(),
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

#[derive(Default)]
pub struct InherentValueMap {
    pub unit: Option<Identifier>,
    pub false_: Option<Identifier>,
    pub true_: Option<Identifier>,
    pub none: Option<Identifier>,
    pub some: Option<Identifier>,
}

macro names($context:ident::{ $( $name:ident = $repr:literal ),+ $(,)? }) {
    impl $context {
        $( pub const $name: &'static str = $repr; )+
    }
}

/// Rust types that can be mapped to FFI-compatible lushui types.
///
/// This trait is not strictly necessary but it makes defining foreign functions on
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, index_map::Index)]
pub struct IOIndex(usize);

pub type IORunner = fn(Vec<Value>) -> Value;

use index_map::IndexMap;
use num_traits::ops::checked::{CheckedDiv, CheckedSub};

pub fn register_foreign_bindings(crate_: &mut Crate) {
    crate_.register_foreign_type(Type::NAT);
    crate_.register_foreign_type(Type::NAT32);
    crate_.register_foreign_type(Type::NAT64);
    crate_.register_foreign_type(Type::INT);
    crate_.register_foreign_type(Type::INT32);
    crate_.register_foreign_type(Type::INT64);
    crate_.register_foreign_type(Type::TEXT);
    crate_.register_foreign_type(Type::IO); // @Temporary

    // @Task make this module aware

    pure!(crate_, "add", |x: Nat, y: Nat| x + y);
    pure!(crate_, "subtract", |x: Nat, y: Nat| x.checked_sub(&y));
    // @Temporary until we get option.unwrap
    pure!(crate_, "panicking-subtract", |x: Nat, y: Nat| x - y);
    pure!(crate_, "multiply", |x: Nat, y: Nat| x * y);
    pure!(crate_, "divide", |x: Nat, y: Nat| x.checked_div(&y));
    pure!(crate_, "equal", |x: Nat, y: Nat| x == y);
    pure!(crate_, "less", |x: Nat, y: Nat| x < y);
    pure!(crate_, "less-equal", |x: Nat, y: Nat| x <= y);
    pure!(crate_, "greater", |x: Nat, y: Nat| x > y);
    pure!(crate_, "greater-equal", |x: Nat, y: Nat| x >= y);
    pure!(crate_, "display", |x: Nat| x.to_string());
    pure!(crate_, "concat", |a: Text, b: Text| a + &b);
    // @Temporary until we can target specific modules
    pure!(crate_, "add-nat32", |a: Nat32, b: Nat32| a + b);
    // @Temporary
    pure!(crate_, "print", |message: Text| Value::IO {
        index: 0,
        arguments: vec![Value::Text(message)]
    });

    // @Beacon @Task write impure!/register_impure: add field to Crate: IndexVec<IOIndex, IO> with function (IO performer/runner)
    // meaninh `IO` does not mean hir::IO here but IO { runner: IORunner } (...)
    // then do the Value::IO { index: MOST_RECENT_INDEX, args } automatically
    // @Note this function won't rely on a Display/Show trait for now
    #[allow(unreachable_code)]
    let _ = |_message: Value| print!("{}", todo!());

    // scope.insert_untyped_foreign_binding("panic", 2, |arguments| {
    //     let message = assume!(Text(&arguments[1]));

    //     panic!("lushui panicked with argument {:?}", message.value);
    // });
}

macro pure($scope:ident, $binder:literal, |$( $var:ident: $variant:ident ),*| $body:expr ) {
    $scope.register_pure_foreign_binding($binder, count!($( $var )*), |arguments| {
        let mut arguments = arguments.into_iter();

        $(
            let $var = match arguments.next() {
                Some(Value::$variant(value)) => value,
                _ => unreachable!(),
            };
        )+

        $body.into()
    });
}

macro count {
    () => { 0 },
    ($var:ident $( $rest:tt )*) => { 1 + count!($( $rest )*) },
}

fn application(callee: Expression, argument: Expression) -> Expression {
    expr! {
        Application {
            Attributes::default(), Span::default();
            callee,
            argument,
            explicitness: Explicit
        }
    }
}

// @Question @Bug unsound?? does this take the identifier index into account?
fn matches(binder: &Identifier, inherent: &Option<Identifier>) -> Option<bool> {
    inherent.as_ref().map(|inherent| binder == inherent)
}
