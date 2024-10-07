use crate::Session;
use diagnostics::error::Result;
use hir::{Expression, ParameterKind::Explicit, interfaceable, special};
use utility::condition;

pub trait InterfaceableBindingExt: Sized {
    fn from_expression(expression: &Expression, session: &Session<'_>) -> Option<Self>;
    fn into_expression(self, session: &Session<'_>) -> Result<Expression>;
}

impl InterfaceableBindingExt for interfaceable::Type {
    // @Task improve this code with the new enum logic
    fn from_expression(expression: &Expression, session: &Session<'_>) -> Option<Self> {
        use special::NumericType::*;
        use special::Type::*;

        macro is_special($binding:ident, $name:ident) {
            session.specials().is($binding.0, $name)
        }

        Some(match &expression.bare {
            // @Note this lookup looks incredibly inefficient
            hir::BareExpression::Binding(binding) => condition! {
                is_special!(binding, Unit) => Self::Unit,
                is_special!(binding, Bool) => Self::Bool,
                is_special!(binding, Nat) => Self::Nat,
                is_special!(binding, Nat32) => Self::Nat32,
                is_special!(binding, Nat64) => Self::Nat64,
                is_special!(binding, Int) => Self::Int,
                is_special!(binding, Int32) => Self::Int32,
                is_special!(binding, Int64) => Self::Int64,
                is_special!(binding, Text) => Self::Text,
                else => return None,
            },
            hir::BareExpression::Application(application) => match &application.callee.bare {
                hir::BareExpression::Binding(binding) if is_special!(binding, Option) => {
                    Self::Option(Box::new(Self::from_expression(&application.argument, session)?))
                }
                _ => return None,
            },
            _ => return None,
        })
    }

    fn into_expression(self, session: &Session<'_>) -> Result<Expression> {
        use special::{NumericType::*, Type::*};

        macro special($name:ident) {
            session.require_special($name, None).map(hir::Identifier::to_item)
        }

        match self {
            Self::Unit => special!(Unit),
            Self::Bool => special!(Bool),
            Self::Nat => special!(Nat),
            Self::Nat32 => special!(Nat32),
            Self::Nat64 => special!(Nat64),
            Self::Int => special!(Int),
            Self::Int32 => special!(Int32),
            Self::Int64 => special!(Int64),
            Self::Text => special!(Text),
            Self::Option(type_) => {
                Ok(application(special!(Option)?, type_.into_expression(session)?))
            }
        }
    }
}

impl InterfaceableBindingExt for interfaceable::Value {
    fn from_expression(expression: &Expression, session: &Session<'_>) -> Option<Self> {
        use hir::BareExpression::*;
        use special::Constructor::*;

        macro is_special($binding:ident, $name:ident) {
            session.specials().is($binding.0, $name)
        }

        Some(match &expression.bare {
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
                is_special!(binding, UnitUnit) => Self::Unit,
                is_special!(binding, BoolFalse) => Self::Bool(false),
                is_special!(binding, BoolTrue) => Self::Bool(true),
                else => return None,
            },

            Application(application0) => match &application0.callee.bare {
                Binding(binding) if is_special!(binding, OptionNone) => Self::Option {
                    value: None,
                    type_: interfaceable::Type::from_expression(&application0.argument, session)?,
                },
                Application(application1) => match &application1.callee.bare {
                    Binding(binding) if is_special!(binding, OptionSome) => Self::Option {
                        value: Some(Box::new(Self::from_expression(
                            &application0.argument,
                            session,
                        )?)),
                        type_: interfaceable::Type::from_expression(
                            &application1.argument,
                            session,
                        )?,
                    },
                    _ => return None,
                },
                _ => return None,
            },
            _ => return None,
        })
    }

    fn into_expression(self, session: &Session<'_>) -> Result<Expression> {
        use hir::Number::*;
        use special::{Constructor::*, Type::*};

        Ok(match self {
            Self::Unit => session.require_special(Unit, None)?.to_item(),
            Self::Bool(value) => {
                session.require_special(if value { BoolTrue } else { BoolFalse }, None)?.to_item()
            }
            Self::Text(value) => Expression::bare(hir::Text::Text(value).into()),
            Self::Nat(value) => Expression::bare(Nat(value).into()),
            Self::Nat32(value) => Expression::bare(Nat32(value).into()),
            Self::Nat64(value) => Expression::bare(Nat64(value).into()),
            Self::Int(value) => Expression::bare(Int(value).into()),
            Self::Int32(value) => Expression::bare(Int32(value).into()),
            Self::Int64(value) => Expression::bare(Int64(value).into()),
            Self::Option { type_, value } => match value {
                Some(value) => application(
                    application(
                        session.require_special(OptionSome, None)?.to_item(),
                        type_.into_expression(session)?,
                    ),
                    value.into_expression(session)?,
                ),
                None => application(
                    session.require_special(OptionNone, None)?.to_item(),
                    type_.into_expression(session)?,
                ),
            },
            Self::IO { index, arguments } => Expression::bare(
                hir::IO {
                    index,
                    arguments: arguments
                        .into_iter()
                        .map(|argument| argument.into_expression(session))
                        .collect::<Result<Vec<_>>>()?,
                }
                .into(),
            ),
        })
    }
}

fn application(callee: Expression, argument: Expression) -> Expression {
    Expression::bare(hir::Application { callee, argument, kind: Explicit }.into())
}
