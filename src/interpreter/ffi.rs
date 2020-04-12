use super::{scope::InherentMap, type_names, Expression, ModuleScope};
use crate::{
    diagnostic::*,
    hir::{expr, ExpressionKind},
    resolver::Identifier,
};

pub type ForeignFunction = fn(arguments: Vec<Value>) -> Value;

// @Question reference-counting?
pub enum Value {
    Unit,
    Bool(bool),
    // @Question which inner type?
    Text(String),
    Nat(crate::Nat),
    Option(Option<Box<Value>>),
}

impl Value {
    // @Bug even if we don't need `Unit`, this throws...
    // in that case, let's just transform the inherent map into
    // an Option<InhererentMap2> which does not contain Options
    pub fn try_from_expression(expression: Expression, map: &InherentMap) -> Result<Option<Self>> {
        fn matches(
            binder: &Identifier,
            inherent: &Option<Identifier>,
            name: &str,
        ) -> Result<bool, Diagnostic> {
            if let Some(inherent) = inherent.as_ref() {
                Ok(binder == inherent)
            } else {
                // @Task span
                Err(Diagnostic::new(
                    Level::Fatal,
                    Code::E063,
                    format!("inherent type `{}` not registered", name),
                ))
            }
        }

        use ExpressionKind::*;

        // @Task
        Ok(Some(match expression.kind {
            Text(text) => Self::Text(text.value.clone()),
            Nat(nat) => Self::Nat(nat.value.clone()),
            Binding(binding) if matches(&binding.binder, &map.unit, type_names::UNIT)? => {
                Value::Unit
            }
            Binding(binding) if matches(&binding.binder, &map.r#false, type_names::BOOL)? => {
                Value::Unit
            }
            Binding(binding) if matches(&binding.binder, &map.r#true, type_names::BOOL)? => {
                Value::Unit
            }
            // @Task Application (for Option)
            _ => return Ok(None),
        }))
    }

    pub fn try_into_expression(self, map: &InherentMap) -> Result<Expression> {
        (|| {
            Some(match self {
                Self::Unit => expr! {
                    Binding[] {
                        binder: map.unit.clone()?
                    }
                },
                Self::Bool(bool) => expr! {
                    Binding[] {
                        binder: if bool { &map.r#false } else { &map.r#true }.clone()?
                    }
                },
                Self::Text(text) => expr! { Text[] { value: text } },
                Self::Nat(nat) => expr! { Nat[] { value: nat } },
                // @Task return an application!
                // @Note it first needs to be applied to a type which
                // is again a `Value` (we need to infer_type on it)
                Self::Option(_) => todo!(),
            })
        })()
        // @Task message
        .ok_or_else(|| Diagnostic::new(Level::Fatal, Code::E063, "XXX XXX"))
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Self::Unit
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::Text(value)
    }
}

impl From<crate::Nat> for Value {
    fn from(value: crate::Nat) -> Value {
        Self::Nat(value)
    }
}

pub fn register_foreign_bindings(scope: &mut ModuleScope) {
    scope.register_foreign_type(type_names::NAT);
    scope.register_foreign_type(type_names::TEXT);

    scope.register_pure_foreign_binding("add", 2, |arguments| {
        assume! { let [x: Nat, y: Nat] = arguments }
        (x + y).into()
    });
    scope.register_pure_foreign_binding("multiply", 2, |arguments| {
        assume! { let [x: Nat, y: Nat] = arguments }
        (x * y).into()
    });
    scope.register_pure_foreign_binding("equal", 2, |arguments| {
        assume! { let [x: Nat, y: Nat] = arguments }
        (x == y).into()
    });
    scope.register_pure_foreign_binding("concat", 2, |arguments| {
        assume! { let [a: Text, b: Text] = arguments }
        (a + &b).into()
    });

    // scope.insert_untyped_foreign_binding("panic", 2, |arguments| {
    //     let message = assume!(Text(&arguments[1]));

    //     panic!("lushui panicked with argument {:?}", message.value);
    // });
}

macro assume(let [$( $var:ident: $variant:ident ),+] = $arguments:ident) {
    // @Note later, the iterator will be passed directly as argument
    let mut arguments = $arguments.into_iter();

    $(
        let $var = match arguments.next() {
            Some(Value::$variant(value)) => value,
            _ => unreachable!(),
        };
    )+
}
