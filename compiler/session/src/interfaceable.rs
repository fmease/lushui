use crate::Session;
use diagnostics::error::Result;
use hir::{interfaceable, special, Expr, ParamKind::Explicit};
use utility::condition;

pub trait InterfaceableBindingExt: Sized {
    fn from_expr(expr: &Expr, sess: &Session<'_>) -> Option<Self>;
    fn into_expr(self, sess: &Session<'_>) -> Result<Expr>;
}

impl InterfaceableBindingExt for interfaceable::Type {
    // @Task improve this code with the new enum logic
    fn from_expr(expr: &Expr, sess: &Session<'_>) -> Option<Self> {
        use special::NumTy::*;
        use special::Ty::*;

        macro is_special($binding:ident, $name:ident) {
            sess.specials().is($binding.0, $name)
        }

        Some(match &expr.bare {
            // @Note this lookup looks incredibly inefficient
            hir::BareExpr::Binding(binding) => condition! {
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
            hir::BareExpr::App(app) => match &app.callee.bare {
                hir::BareExpr::Binding(binding) if is_special!(binding, Option) => {
                    Self::Option(Box::new(Self::from_expr(&app.arg, sess)?))
                }
                _ => return None,
            },
            _ => return None,
        })
    }

    fn into_expr(self, sess: &Session<'_>) -> Result<Expr> {
        use special::{NumTy::*, Ty::*};

        macro special($name:ident) {
            sess.require_special($name, None).map(hir::Ident::to_item)
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
            Self::Option(ty) => Ok(app(special!(Option)?, ty.into_expr(sess)?)),
        }
    }
}

impl InterfaceableBindingExt for interfaceable::Value {
    fn from_expr(expr: &Expr, sess: &Session<'_>) -> Option<Self> {
        use hir::BareExpr::*;
        use special::Ctor::*;

        macro is_special($binding:ident, $name:ident) {
            sess.specials().is($binding.0, $name)
        }

        Some(match &expr.bare {
            TextLit(text) => {
                use hir::TextLit::*;

                match &**text {
                    // @Note not great
                    Text(text) => Self::Text(text.clone()),
                }
            }
            NumLit(num) => {
                use hir::NumLit::*;

                match &**num {
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

            App(app0) => match &app0.callee.bare {
                Binding(binding) if is_special!(binding, OptionNone) => Self::Option {
                    value: None,
                    ty: interfaceable::Type::from_expr(&app0.arg, sess)?,
                },
                App(app1) => match &app1.callee.bare {
                    Binding(binding) if is_special!(binding, OptionSome) => Self::Option {
                        value: Some(Box::new(Self::from_expr(&app0.arg, sess)?)),
                        ty: interfaceable::Type::from_expr(&app1.arg, sess)?,
                    },
                    _ => return None,
                },
                _ => return None,
            },
            _ => return None,
        })
    }

    fn into_expr(self, sess: &Session<'_>) -> Result<Expr> {
        use hir::NumLit::*;
        use special::{Ctor::*, Ty::*};

        Ok(match self {
            Self::Unit => sess.require_special(Unit, None)?.to_item(),
            Self::Bool(value) => sess
                .require_special(if value { BoolTrue } else { BoolFalse }, None)?
                .to_item(),
            Self::Text(value) => Expr::bare(hir::TextLit::Text(value).into()),
            Self::Nat(value) => Expr::bare(Nat(value).into()),
            Self::Nat32(value) => Expr::bare(Nat32(value).into()),
            Self::Nat64(value) => Expr::bare(Nat64(value).into()),
            Self::Int(value) => Expr::bare(Int(value).into()),
            Self::Int32(value) => Expr::bare(Int32(value).into()),
            Self::Int64(value) => Expr::bare(Int64(value).into()),
            Self::Option { ty, value } => match value {
                Some(value) => app(
                    app(
                        sess.require_special(OptionSome, None)?.to_item(),
                        ty.into_expr(sess)?,
                    ),
                    value.into_expr(sess)?,
                ),
                None => app(
                    sess.require_special(OptionNone, None)?.to_item(),
                    ty.into_expr(sess)?,
                ),
            },
            Self::IO {
                index,
                args: arguments,
            } => Expr::bare(
                hir::IO {
                    index,
                    args: arguments
                        .into_iter()
                        .map(|argument| argument.into_expr(sess))
                        .collect::<Result<Vec<_>>>()?,
                }
                .into(),
            ),
        })
    }
}

fn app(callee: Expr, arg: Expr) -> Expr {
    Expr::bare(
        hir::App {
            callee,
            arg,
            kind: Explicit,
        }
        .into(),
    )
}
