use crate::{DeclarationIndex, Identifier};
use diagnostics::{error::Result, Diagnostic, ErrorCode};
use num_traits::{CheckedDiv, CheckedSub};
use span::{Span, Spanning};
use std::fmt;
use utilities::HashMap;

/// A special binding.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Binding {
    Type(Type),
    Constructor(Constructor),
    Function(Function),
}

impl Binding {
    pub fn parse(namespace: Option<&str>, name: &str) -> Option<Self> {
        Some(match (namespace, name) {
            (None, "Type") => Type::Type.into(),
            (None, "Unit") => Type::Unit.into(),
            (None, "Bool") => Type::Bool.into(),
            (None, "Text") => Type::Text.into(),
            (None, "Option") => Type::Option.into(),
            (None, "List") => SequentialType::List.into(),
            (None, "Vector") => SequentialType::Vector.into(),
            (None, "Tuple") => SequentialType::Tuple.into(),
            (None, "Nat") => NumericType::Nat.into(),
            (None, "Nat32") => NumericType::Nat32.into(),
            (None, "Nat64") => NumericType::Nat64.into(),
            (None, "Int") => NumericType::Int.into(),
            (None, "Int32") => NumericType::Int32.into(),
            (None, "Int64") => NumericType::Int64.into(),
            (None, "IO") => Type::IO.into(),
            (Some("Unit"), "unit") => Constructor::UnitUnit.into(),
            (Some("Bool"), "false") => Constructor::BoolFalse.into(),
            (Some("Bool"), "true") => Constructor::BoolTrue.into(),
            (Some("Option"), "none") => Constructor::OptionNone.into(),
            (Some("Option"), "some") => Constructor::OptionSome.into(),
            (Some("List"), "empty") => Constructor::ListEmpty.into(),
            (Some("List"), "prepend") => Constructor::ListPrepend.into(),
            (Some("Vector"), "empty") => Constructor::VectorEmpty.into(),
            (Some("Vector"), "prepend") => Constructor::VectorPrepend.into(),
            (Some("Tuple"), "empty") => Constructor::TupleEmpty.into(),
            (Some("Tuple"), "prepend") => Constructor::TuplePrepend.into(),
            (Some("nat"), "add") => Function::NatAdd.into(),
            (Some("nat"), "subtract") => Function::NatSubtract.into(),
            (Some("nat"), "unchecked-subtract") => Function::NatUncheckedSubtract.into(),
            (Some("nat"), "multiply") => Function::NatMultiply.into(),
            (Some("nat"), "divide") => Function::NatDivide.into(),
            (Some("nat"), "equal") => Function::NatEqual.into(),
            (Some("nat"), "less") => Function::NatLess.into(),
            (Some("nat"), "less-equal") => Function::NatLessEqual.into(),
            (Some("nat"), "greater") => Function::NatGreater.into(),
            (Some("nat"), "greater-equal") => Function::NatGreaterEqual.into(),
            (Some("nat"), "display") => Function::NatDisplay.into(),
            (Some("text"), "concat") => Function::TextConcat.into(),
            (Some("nat32"), "add") => Function::Nat32Add.into(),
            (Some("nat32"), "successor") => Function::Nat32Successor.into(),
            (Some("io"), "print") => Function::IoPrint.into(),
            _ => return None,
        })
    }

    fn kind(self) -> Kind {
        match self {
            Self::Type(binding) => binding.kind(),
            Self::Constructor(_) => Kind::Known,
            Self::Function(binding) => binding.kind(),
        }
    }
}

impl fmt::Display for Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Type(binding) => binding.fmt(f),
            Self::Constructor(binding) => binding.fmt(f),
            Self::Function(binding) => binding.fmt(f),
        }
    }
}

/// A special type (constructor).
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Type {
    /// The intrinsic type `Type`.
    Type,
    /// The known type `Unit`.
    Unit,
    /// The known type `Bool`.
    Bool,
    /// The intrinsic type `Text`.
    Text,
    /// The known type constructor `Option`.
    Option,
    /// A known sequential type.
    Sequential(SequentialType),
    /// An intrinsic numeric type.
    Numeric(NumericType),
    /// The intrinsic type constructor `IO`.
    IO,
}

impl Type {
    fn kind(self) -> Kind {
        match self {
            Self::Type | Self::Text | Self::Numeric(_) | Self::IO => Kind::Intrinsic,
            Self::Unit | Self::Bool | Self::Option | Self::Sequential(_) => Kind::Known,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Type => "Type",
            Self::Unit => "Unit",
            Self::Bool => "Bool",
            Self::Text => "Text",
            Self::Option => "Option",
            Self::Sequential(type_) => return type_.fmt(f),
            Self::Numeric(type_) => return type_.fmt(f),
            Self::IO => "IO",
        })
    }
}

impl From<Type> for Binding {
    fn from(type_: Type) -> Self {
        Self::Type(type_)
    }
}

/// A known sequential type.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum SequentialType {
    /// The known type constructor `List`.
    List,
    /// The known type constructor `Vector`.
    Vector,
    /// The known type constructor `Tuple`.
    Tuple,
}

// @Task derive this with `#[format(upper_dash_case)]`
impl fmt::Display for SequentialType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::List => "List",
            Self::Vector => "Vector",
            Self::Tuple => "Tuple",
        })
    }
}

impl From<SequentialType> for Type {
    fn from(type_: SequentialType) -> Self {
        Self::Sequential(type_)
    }
}

impl From<SequentialType> for Binding {
    fn from(type_: SequentialType) -> Self {
        Type::from(type_).into()
    }
}

/// An intrinsic numeric type.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum NumericType {
    /// The intrinsic type `Nat`.
    Nat,
    /// The intrinsic type `Nat32`.
    Nat32,
    /// The intrinsic type `Nat64`.
    Nat64,
    /// The intrinsic type `Int`.
    Int,
    /// The intrinsic type `Int32`.
    Int32,
    /// The intrinsic type `Int64`.
    Int64,
}

impl NumericType {
    pub const fn interval(self) -> &'static str {
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
impl fmt::Display for NumericType {
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

impl From<NumericType> for Type {
    fn from(type_: NumericType) -> Self {
        Self::Numeric(type_)
    }
}

impl From<NumericType> for Binding {
    fn from(type_: NumericType) -> Self {
        Type::from(type_).into()
    }
}

/// A known constructor.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Constructor {
    /// The known constructor `Unit.unit`.
    UnitUnit,
    /// The known constructor `Bool.false`.
    BoolFalse,
    /// The known constructor `Bool.true`.
    BoolTrue,
    /// The known constructor `Option.none`.
    OptionNone,
    /// The known constructor `Option.some`.
    OptionSome,
    /// The known constructor `List.empty`.
    ListEmpty,
    /// The known constructor `List.prepend`.
    ListPrepend,
    /// The known constructor `Vector.empty`.
    VectorEmpty,
    /// The known constructor `Vector.prepend`.
    VectorPrepend,
    /// The known constructor `Tuple.empty`.
    TupleEmpty,
    /// The known constructor `Tuple.prepend`.
    TuplePrepend,
}

impl fmt::Display for Constructor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Constructor::UnitUnit => "Unit.unit",
            Constructor::BoolFalse => "Bool.false",
            Constructor::BoolTrue => "Bool.true",
            Constructor::OptionNone => "Option.none",
            Constructor::OptionSome => "Option.some",
            Constructor::ListEmpty => "List.empty",
            Constructor::ListPrepend => "List.prepend",
            Constructor::VectorEmpty => "Vector.empty",
            Constructor::VectorPrepend => "Vector.prepend",
            Constructor::TupleEmpty => "Tuple.empty",
            Constructor::TuplePrepend => "Tuple.prepend",
        })
    }
}

impl From<Constructor> for Binding {
    fn from(constructor: Constructor) -> Self {
        Self::Constructor(constructor)
    }
}

/// An intrinsic function.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Function {
    /// The intrinsic function `nat.add`.
    NatAdd,
    /// The intrinsic function `nat.subtract`.
    NatSubtract,
    /// The intrinsic function `nat.unchecked-subtract`.
    NatUncheckedSubtract,
    /// The intrinsic function `nat.multiply`.
    NatMultiply,
    /// The intrinsic function `nat.divide`.
    NatDivide,
    /// The intrinsic function `nat.equal`.
    NatEqual,
    /// The intrinsic function `nat.less`.
    NatLess,
    /// The intrinsic function `nat.less-equal`.
    NatLessEqual,
    /// The intrinsic function `nat.greater`.
    NatGreater,
    /// The intrinsic function `nat.greater-equal`.
    NatGreaterEqual,
    /// The intrinsic function `nat.display`.
    NatDisplay,
    /// The intrinsic function `text.concat`.
    TextConcat,
    /// The intrinsic function `nat32.add`.
    Nat32Add,
    /// The intrinsic function `nat32.successor`.
    Nat32Successor,
    /// The intrinsic function `io.print`.
    IoPrint,
}

// @Task find a better system than this arity/evaluate split
impl Function {
    pub const fn name(self) -> &'static str {
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
            Self::Nat32Successor => "nat32.successor",
            Self::IoPrint => "io.print",
        }
    }

    pub const fn arity(self) -> usize {
        match self {
            Self::Nat32Successor | Self::NatDisplay | Self::IoPrint => 1,
            Self::NatAdd
            | Self::NatSubtract
            | Self::NatUncheckedSubtract
            | Self::NatMultiply
            | Self::NatDivide
            | Self::NatEqual
            | Self::NatLess
            | Self::NatLessEqual
            | Self::NatGreater
            | Self::NatGreaterEqual
            | Self::TextConcat
            | Self::Nat32Add => 2,
        }
    }

    // @Task don't use interfaceable values
    pub fn evaluate(
        self,
        arguments: Vec<crate::interfaceable::Value>,
    ) -> crate::interfaceable::Value {
        use crate::interfaceable::Value::*;

        let mut arguments = arguments.into_iter();
        match self {
            Self::NatAdd => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                (x + y).into()
            }
            Self::NatSubtract => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                x.checked_sub(&y).into()
            }
            Self::NatUncheckedSubtract => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                (x - y).into()
            }
            Self::NatMultiply => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                (x * y).into()
            }
            Self::NatDivide => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                x.checked_div(&y).into()
            }
            Self::NatEqual => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                (x == y).into()
            }
            Self::NatLess => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                (x < y).into()
            }
            Self::NatLessEqual => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                (x <= y).into()
            }
            Self::NatGreater => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                (x > y).into()
            }
            Self::NatGreaterEqual => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                let Some(Nat(y)) = arguments.next() else { unreachable!() };
                (x >= y).into()
            }
            Self::NatDisplay => {
                let Some(Nat(x)) = arguments.next() else { unreachable!() };
                x.to_string().into()
            }
            Self::TextConcat => {
                let Some(Text(x)) = arguments.next() else { unreachable!() };
                let Some(Text(y)) = arguments.next() else { unreachable!() };
                (x + &y).into()
            }
            Self::Nat32Add => {
                let Some(Nat32(x)) = arguments.next() else { unreachable!() };
                let Some(Nat32(y)) = arguments.next() else { unreachable!() };
                (x + y).into()
            }
            Self::Nat32Successor => {
                let Some(Nat32(x)) = arguments.next() else { unreachable!() };
                (x + 1).into()
            }
            Self::IoPrint => {
                let Some(Text(message)) = arguments.next() else { unreachable!() };
                crate::interfaceable::Value::IO {
                    index: 0,
                    arguments: vec![message.into()],
                }
            }
        }
    }

    #[allow(clippy::unused_self)] // forward compatibility
    fn kind(self) -> Kind {
        Kind::Intrinsic
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

impl From<Function> for Binding {
    fn from(function: Function) -> Self {
        Self::Function(function)
    }
}

/// The kind of special binding.
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Kind {
    Intrinsic,
    Known,
}

impl Kind {
    const fn article(self) -> &'static str {
        match self {
            Self::Intrinsic => "an",
            Self::Known => "a",
        }
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Intrinsic => "intrinsic",
            Self::Known => "known",
        })
    }
}

/// A bidirectional map for special bindings.
#[derive(Default)]
pub struct Bindings {
    from: HashMap<Binding, Identifier>,
    to: HashMap<DeclarationIndex, Binding>,
}

impl Bindings {
    pub fn require<T: From<crate::Binding>>(
        &self,
        special: impl Into<Binding>,
        user: Option<Span>,
    ) -> Result<crate::Item<T>, Diagnostic> {
        let special = special.into();

        self.get(special)
            .map(|identifier| identifier.clone().into_item())
            .ok_or_else(|| missing_binding_error(special, user))
    }

    // @Beacon @Note kinda weird having both name & binder, can we move one of em into DefStyle?
    pub fn define(
        &mut self,
        kind: Kind,
        binder: Identifier,
        style: DefinitionStyle<'_>,
        attribute: Span,
    ) -> Result<Binding, Diagnostic> {
        let special = style
            .as_name(&binder)
            .and_then(|(namespace, name)| Binding::parse(namespace, name));
        let Some(special) = special.filter(|special| special.kind() == kind) else {
            return Err(Diagnostic::error()
                .code(match kind {
                    Kind::Intrinsic => ErrorCode::E061,
                    Kind::Known => ErrorCode::E063,
                })
                .message(format!(
                    "‘{}’ is not {} {kind} binding",
                    style.as_path(&binder),
                    kind.article()
                ))
                .primary_span(match style {
                    DefinitionStyle::Implicit { .. } => binder.span(),
                    DefinitionStyle::Explicit { name } => name.span(),
                })
                .with(|error| match style {
                    DefinitionStyle::Implicit { .. } => error
                        .labeled_secondary_span(
                            attribute,
                            match kind {
                                Kind::Intrinsic => {
                                    "claims the binding is intrinsic to the language"
                                }
                                Kind::Known => "claims the binding is known to the compiler",
                            },
                        )
                        .help(format!("consider adding an explicit name to the attribute to overwrite the derived one:\n‘@({kind} name)’")),
                    DefinitionStyle::Explicit { .. } => error,
                })
                // @Task add a UI test for this case
                .with(|error| match special {
                    Some(special) => {
                        let kind = special.kind();
                        error.note(format!("it is {} {kind} binding", kind.article()))
                    }
                    None => error,
                }));
        };

        if let Some(previous) = self.get(special) {
            return Err(Diagnostic::error()
                .code(match kind {
                    Kind::Intrinsic => ErrorCode::E040,
                    Kind::Known => ErrorCode::E039,
                })
                .message(format!(
                    "the {kind} binding ‘{special}’ is defined multiple times"
                ))
                .labeled_primary_span(binder, "redefinition")
                .labeled_secondary_span(previous, "previous definition"));
        }

        self.insert_unchecked(special, binder);

        Ok(special)
    }

    pub fn insert_unchecked(&mut self, special: Binding, binder: Identifier) {
        let index = binder.declaration_index().unwrap();
        self.to.insert(index, special);
        self.from.insert(special, binder);
    }

    pub fn get<K: Key>(&self, key: K) -> Option<<K as Key>::Output<'_>> {
        key.index(self)
    }

    pub fn is(&self, binder: &Identifier, special: impl Into<Binding>) -> bool {
        self.get(special).map_or(false, |special| special == binder)
    }
}

pub enum DefinitionStyle<'a, T = &'a str> {
    /// The name of the special binding is implied by the binder & the namespace of the declaration (e.g. in `@known`).
    Implicit { namespace: Option<T> },
    /// The name of the special binding is given explicitly (e.g. in `@(intrinsic qualified.name)`).
    Explicit { name: &'a ast::Path },
}

impl<'a> DefinitionStyle<'a> {
    fn as_path(&self, binder: &Identifier) -> String {
        match self {
            Self::Implicit { namespace } => namespace.map_or(binder.to_string(), |namespace| {
                format!("{namespace}.{binder}")
            }),
            Self::Explicit { name } => name.to_string(),
        }
    }

    fn as_name(&'a self, binder: &'a Identifier) -> Option<(Option<&'a str>, &'a str)> {
        match self {
            &Self::Implicit { namespace } => Some((namespace, binder.as_str())),
            Self::Explicit { name } => {
                if name.hanger.is_some() {
                    return None;
                }

                Some(match &*name.segments {
                    [namespace, name] => (Some(namespace.as_str()), name.as_str()),
                    [name] => (None, name.as_str()),
                    _ => return None,
                })
            }
        }
    }
}

pub trait Key: Sized {
    type Output<'a>;

    fn index(self, bindings: &Bindings) -> Option<Self::Output<'_>>;
}

impl<B: Into<Binding>> Key for B {
    type Output<'a> = &'a Identifier;

    fn index(self, bindings: &Bindings) -> Option<Self::Output<'_>> {
        bindings.from.get(&self.into())
    }
}

impl Key for DeclarationIndex {
    type Output<'a> = Binding;

    fn index(self, bindings: &Bindings) -> Option<Self::Output<'_>> {
        bindings.to.get(&self).copied()
    }
}

fn missing_binding_error(special: Binding, user: Option<Span>) -> Diagnostic {
    let kind = special.kind();

    Diagnostic::error()
        .code(match kind {
            Kind::Known => ErrorCode::E062,
            Kind::Intrinsic => ErrorCode::E060,
        })
        .message(format!("the {kind} binding ‘{special}’ is not defined"))
        .with(|error| match user {
            // @Task label
            Some(user) => error.labeled_primary_span(user, "the type of this expression"),
            None => error,
        })
}
