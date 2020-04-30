use super::{Expression, ModuleScope};
use crate::{
    diagnostic::*,
    hir::{expr, Constructor, ExpressionKind},
    parser::Explicitness,
    resolver::Identifier,
    span::Span,
    Nat,
};

pub type ForeignFunction = fn(arguments: Vec<Value>) -> Value;

pub fn register_inherent_bindings<'a>(
    binder: &Identifier,
    mut constructors: impl Iterator<Item = &'a Constructor<Identifier>>,
    declaration: Span,
    attribute: Span,
    scope: &mut ModuleScope,
) -> Result<()> {
    // @Task link to previous definition
    let duplicate = || {
        Diagnostic::new(
            Level::Fatal,
            Code::E020,
            format!("`{}` is defined multiple times as inherent", binder),
        )
        .with_span(declaration)
    };

    let mut find = |value_name, inherent: &mut Option<_>| {
        if let Some(constructor) =
            constructors.find(|constructor| &constructor.binder.source.atom == value_name)
        {
            *inherent = Some(constructor.binder.clone().dummified());
        }
    };

    match &*binder.source.atom {
        Type::UNIT => {
            if scope.inherent_types.unit.is_some() {
                return Err(duplicate());
            }

            scope.inherent_types.unit = Some(binder.clone().dummified());
            find(Value::UNIT, &mut scope.inherent_values.unit);
        }
        Type::BOOL => {
            if scope.inherent_types.bool.is_some() {
                return Err(duplicate());
            }

            scope.inherent_types.bool = Some(binder.clone().dummified());
            find(Value::FALSE, &mut scope.inherent_values.r#false);
            find(Value::TRUE, &mut scope.inherent_values.r#true);
        }
        Type::OPTION => {
            if scope.inherent_types.option.is_some() {
                return Err(duplicate());
            }

            scope.inherent_types.option = Some(binder.clone().dummified());
            find(Value::NONE, &mut scope.inherent_values.none);
            find(Value::SOME, &mut scope.inherent_values.some);
        }
        _ => {
            return Err(Diagnostic::new(
                Level::Fatal,
                Code::E062,
                format!("`{}` is not an inherent type", binder),
            )
            .with_span(attribute)
            .with_labeled_span(declaration, "ascribed to this declaration"))
        }
    }

    Ok(())
}

/// An FFI-compatible type.
///
/// Except `Type` and `->`.
pub enum Type {
    Unit,
    Bool,
    Nat,
    Text,
    Option(Box<Type>),
}

impl Type {
    // @Task rename so they have "name" in their names, or make them
    // resolver::Identifiers (dummified)
    pub const UNIT: &'static str = "Unit";
    pub const BOOL: &'static str = "Bool";
    pub const NAT: &'static str = "Nat";
    pub const TEXT: &'static str = "Text";
    pub const OPTION: &'static str = "Option";

    fn from_expression(expression: &Expression, scope: &super::ModuleScope) -> Option<Self> {
        let types = &scope.inherent_types;

        use ExpressionKind::*;

        Some(match &expression.kind {
            Binding(binding) if matches(&binding.binder, &types.unit)? => Self::Unit,
            Binding(binding) if matches(&binding.binder, &types.bool)? => Self::Bool,
            Binding(binding)
                if matches(&binding.binder, &scope.foreign_types[self::Type::NAT])? =>
            {
                Self::Nat
            }
            Binding(binding)
                if matches(&binding.binder, &scope.foreign_types[self::Type::TEXT])? =>
            {
                Self::Text
            }
            Application(application) => match &application.callee.kind {
                Binding(binding) if matches(&binding.binder, &types.option)? => Self::Option(
                    Box::new(self::Type::from_expression(&application.argument, scope)?),
                ),
                _ => return None,
            },
            _ => return None,
        })
    }

    fn into_expression(self, scope: &super::ModuleScope) -> Result<Expression> {
        let types = &scope.inherent_types;

        // @Task message
        fn missing_inherent() -> Diagnostic {
            Diagnostic::new(Level::Fatal, Code::E063, "XXX XXX")
        }

        Ok(match self {
            Self::Unit => binding(types.unit.clone().ok_or_else(missing_inherent)?),
            Self::Bool => binding(types.bool.clone().ok_or_else(missing_inherent)?),
            Self::Nat => scope.lookup_foreign_type(Type::NAT, None)?,
            Self::Text => scope.lookup_foreign_type(Type::TEXT, None)?,
            Self::Option(r#type) => application(
                binding(types.option.clone().ok_or_else(missing_inherent)?),
                r#type.into_expression(scope)?,
            ),
        })
    }
}
// @Task smh merge InherentTypeMap and InherentValueMap without creating too much boilerplate
#[derive(Default)]
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
    Option {
        r#type: Type,
        value: Option<Box<Value>>,
    },
}

impl Value {
    // @Task rename so they have "name" in their names, or make them
    // resolver::Identifiers (dummified)
    pub const UNIT: &'static str = "unit";
    pub const FALSE: &'static str = "false";
    pub const TRUE: &'static str = "true";
    pub const NONE: &'static str = "none";
    pub const SOME: &'static str = "some";

    pub fn from_expression(expression: &Expression, scope: &super::ModuleScope) -> Option<Self> {
        let values = &scope.inherent_values;

        use ExpressionKind::*;

        Some(match &expression.kind {
            Text(text) => Self::Text(text.value.clone()),
            Nat(nat) => Self::Nat(nat.value.clone()),
            Binding(binding) if matches(&binding.binder, &values.unit)? => Value::Unit,
            Binding(binding) if matches(&binding.binder, &values.r#false)? => Value::Bool(false),
            Binding(binding) if matches(&binding.binder, &values.r#true)? => Value::Bool(true),
            Application(application0) => match &application0.callee.kind {
                Binding(binding) if matches(&binding.binder, &values.none)? => Value::Option {
                    value: None,
                    r#type: self::Type::from_expression(&application0.argument, scope)?,
                },
                Application(application1) => match &application1.callee.kind {
                    Binding(binding) if matches(&binding.binder, &values.some)? => Value::Option {
                        value: Some(Box::new(Self::from_expression(
                            &application0.argument,
                            scope,
                        )?)),
                        r#type: self::Type::from_expression(&application1.argument, scope)?,
                    },
                    _ => return None,
                },
                _ => return None,
            },
            _ => return None,
        })
    }

    pub fn into_expression(self, scope: &super::ModuleScope) -> Result<Expression> {
        let values = &scope.inherent_values;

        // @Task message
        fn missing_inherent() -> Diagnostic {
            Diagnostic::new(Level::Fatal, Code::E063, "XXX XXX")
        }

        Ok(match self {
            Self::Unit => binding(values.unit.clone().ok_or_else(missing_inherent)?),
            Self::Bool(value) => binding(
                if value {
                    &values.r#false
                } else {
                    &values.r#true
                }
                .clone()
                .ok_or_else(missing_inherent)?,
            ),
            Self::Text(value) => expr! { Text[] { value } },
            Self::Nat(value) => expr! { Nat[] { value } },
            Self::Option { r#type, value } => match value {
                Some(value) => application(
                    application(
                        binding(values.some.clone().ok_or_else(missing_inherent)?),
                        r#type.into_expression(scope)?,
                    ),
                    value.into_expression(scope)?,
                ),
                None => application(
                    binding(values.none.clone().ok_or_else(missing_inherent)?),
                    r#type.into_expression(scope)?,
                ),
            },
        })
    }
}

#[derive(Default)]
pub struct InherentValueMap {
    pub unit: Option<Identifier>,
    pub r#false: Option<Identifier>,
    pub r#true: Option<Identifier>,
    pub none: Option<Identifier>,
    pub some: Option<Identifier>,
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

impl IntoValue for () {
    fn into_type() -> Type {
        Type::Unit
    }

    fn into_value(self) -> Value {
        Value::Unit
    }
}

impl IntoValue for bool {
    fn into_type() -> Type {
        Type::Bool
    }

    fn into_value(self) -> Value {
        Value::Bool(self)
    }
}

impl IntoValue for String {
    fn into_type() -> Type {
        Type::Text
    }

    fn into_value(self) -> Value {
        Value::Text(self)
    }
}

impl IntoValue for Nat {
    fn into_type() -> Type {
        Type::Nat
    }

    fn into_value(self) -> Value {
        Value::Nat(self)
    }
}

impl<V: IntoValue> IntoValue for Option<V> {
    fn into_type() -> Type {
        Type::Option(Box::new(V::into_type()))
    }

    fn into_value(self) -> Value {
        Value::Option {
            r#type: V::into_type(),
            value: self.map(|value| Box::new(value.into())),
        }
    }
}

use num_traits::ops::checked::{CheckedDiv, CheckedSub};

pub fn register_foreign_bindings(scope: &mut ModuleScope) {
    scope.register_foreign_type(Type::NAT);
    scope.register_foreign_type(Type::TEXT);

    // @Task make this module aware

    pure!(scope, "add", |x: Nat, y: Nat| x + y);
    pure!(scope, "subtract", |x: Nat, y: Nat| x.checked_sub(&y));
    pure!(scope, "multiply", |x: Nat, y: Nat| x * y);
    pure!(scope, "divide", |x: Nat, y: Nat| x.checked_div(&y));
    pure!(scope, "equal", |x: Nat, y: Nat| x == y);
    pure!(scope, "less", |x: Nat, y: Nat| x < y);
    pure!(scope, "less-equal", |x: Nat, y: Nat| x <= y);
    pure!(scope, "greater", |x: Nat, y: Nat| x > y);
    pure!(scope, "greater-equal", |x: Nat, y: Nat| x >= y);
    pure!(scope, "display", |x: Nat| x.to_string());
    pure!(scope, "concat", |a: Text, b: Text| a + &b);

    // scope.insert_untyped_foreign_binding("panic", 2, |arguments| {
    //     let message = assume!(Text(&arguments[1]));

    //     panic!("lushui panicked with argument {:?}", message.value);
    // });
}

macro pure($scope:ident, $binder:literal, |$( $var:ident: $variant:ident ),*| $( $body:tt )+) {
    $scope.register_pure_foreign_binding($binder, count!($( $var )*), |arguments| {
        let mut arguments = arguments.into_iter();

        $(
            let $var = match arguments.next() {
                Some(Value::$variant(value)) => value,
                _ => unreachable!(),
            };
        )+

        (|| $( $body )+)().into()
    });
}

macro count {
    () => { 0 },
    ($var:ident $( $rest:tt )*) => { 1 + count!($( $rest )*) },
}

fn binding(binder: Identifier) -> Expression {
    expr! { Binding[] { binder } }
}

fn application(callee: Expression, argument: Expression) -> Expression {
    expr! {
        Application[] { callee, argument, explicitness: Explicitness::Explicit }
    }
}

fn matches(binder: &Identifier, inherent: &Option<Identifier>) -> Option<bool> {
    inherent.as_ref().map(|inherent| binder == inherent)
}
