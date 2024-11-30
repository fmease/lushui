use crate::{DeclarationIndex, Identifier};
use diagnostics::{Diagnostic, ErrorCode, Substitution, error::Result};
use num_traits::{CheckedDiv, CheckedSub};
use span::{Span, Spanning};
use std::fmt;
use utility::{Atom, HashMap};

/// A special binding.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Binding {
    Type(Type),
    Constructor(Constructor),
    Function(Function),
}

impl Binding {
    pub fn parse(namespace: Option<Atom>, name: Atom) -> Option<Self> {
        Some(match (namespace, name) {
            (None, Atom::TYPE) => Type::Type.into(),
            (None, Atom::UNIT_UPPER) => Type::Unit.into(),
            (None, Atom::BOOL) => Type::Bool.into(),
            (None, Atom::TEXT_UPPER) => Type::Text.into(),
            (None, Atom::OPTION) => Type::Option.into(),
            (None, Atom::LIST) => SequentialType::List.into(),
            (None, Atom::VECTOR) => SequentialType::Vector.into(),
            (None, Atom::TUPLE) => SequentialType::Tuple.into(),
            (None, Atom::NAT_UPPER) => NumericType::Nat.into(),
            (None, Atom::NAT32_UPPER) => NumericType::Nat32.into(),
            (None, Atom::NAT64) => NumericType::Nat64.into(),
            (None, Atom::INT) => NumericType::Int.into(),
            (None, Atom::INT32) => NumericType::Int32.into(),
            (None, Atom::INT64) => NumericType::Int64.into(),
            (None, Atom::IO_UPPER) => Type::IO.into(),
            (Some(Atom::UNIT_UPPER), Atom::UNIT_LOWER) => Constructor::UnitUnit.into(),
            (Some(Atom::BOOL), Atom::FALSE) => Constructor::BoolFalse.into(),
            (Some(Atom::BOOL), Atom::TRUE) => Constructor::BoolTrue.into(),
            (Some(Atom::OPTION), Atom::NONE) => Constructor::OptionNone.into(),
            (Some(Atom::OPTION), Atom::SOME) => Constructor::OptionSome.into(),
            (Some(Atom::LIST), Atom::EMPTY) => Constructor::ListEmpty.into(),
            (Some(Atom::LIST), Atom::PREPEND) => Constructor::ListPrepend.into(),
            (Some(Atom::VECTOR), Atom::EMPTY) => Constructor::VectorEmpty.into(),
            (Some(Atom::VECTOR), Atom::PREPEND) => Constructor::VectorPrepend.into(),
            (Some(Atom::TUPLE), Atom::EMPTY) => Constructor::TupleEmpty.into(),
            (Some(Atom::TUPLE), Atom::PREPEND) => Constructor::TuplePrepend.into(),
            (Some(Atom::NAT_LOWER), Atom::ADD) => Function::NatAdd.into(),
            (Some(Atom::NAT_LOWER), Atom::SUBTRACT) => Function::NatSubtract.into(),
            (Some(Atom::NAT_LOWER), Atom::UNCHECKED_SUBTRACT) => {
                Function::NatUncheckedSubtract.into()
            }
            (Some(Atom::NAT_LOWER), Atom::MULTIPLY) => Function::NatMultiply.into(),
            (Some(Atom::NAT_LOWER), Atom::DIVIDE) => Function::NatDivide.into(),
            (Some(Atom::NAT_LOWER), Atom::EQUAL) => Function::NatEqual.into(),
            (Some(Atom::NAT_LOWER), Atom::LESS) => Function::NatLess.into(),
            (Some(Atom::NAT_LOWER), Atom::LESS_EQUAL) => Function::NatLessEqual.into(),
            (Some(Atom::NAT_LOWER), Atom::GREATER) => Function::NatGreater.into(),
            (Some(Atom::NAT_LOWER), Atom::GREATER_EQUAL) => Function::NatGreaterEqual.into(),
            (Some(Atom::NAT_LOWER), Atom::DISPLAY) => Function::NatDisplay.into(),
            (Some(Atom::TEXT_LOWER), Atom::CONCAT) => Function::TextConcat.into(),
            (Some(Atom::NAT32_LOWER), Atom::ADD) => Function::Nat32Add.into(),
            (Some(Atom::NAT32_LOWER), Atom::SUCCESSOR) => Function::Nat32Successor.into(),
            (Some(Atom::IO_LOWER), Atom::PRINT) => Function::IoPrint.into(),
            _ => return None,
        })
    }

    pub fn kind(self) -> Kind {
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
            Self::UnitUnit => "Unit.unit",
            Self::BoolFalse => "Bool.false",
            Self::BoolTrue => "Bool.true",
            Self::OptionNone => "Option.none",
            Self::OptionSome => "Option.some",
            Self::ListEmpty => "List.empty",
            Self::ListPrepend => "List.prepend",
            Self::VectorEmpty => "Vector.empty",
            Self::VectorPrepend => "Vector.prepend",
            Self::TupleEmpty => "Tuple.empty",
            Self::TuplePrepend => "Tuple.prepend",
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
                crate::interfaceable::Value::IO { index: 0, arguments: vec![message.into()] }
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
    // @Beacon @Note kinda weird having both name & binder, can we move one of em into DefStyle?
    pub fn define(
        &mut self,
        kind: Kind,
        binder: Identifier,
        style: DefinitionStyle<'_>,
        attribute: Span,
    ) -> Result<Binding, Diagnostic> {
        let special =
            style.as_name(binder).and_then(|(namespace, name)| Binding::parse(namespace, name));
        let Some(special) = special.filter(|special| special.kind() == kind) else {
            return Err(Diagnostic::error()
                .code(match kind {
                    Kind::Intrinsic => ErrorCode::E061,
                    Kind::Known => ErrorCode::E063,
                })
                .message(format!(
                    "‘{}’ is not {} {kind} binding",
                    style.as_path(binder),
                    kind.article()
                ))
                .unlabeled_span(match style {
                    DefinitionStyle::Implicit { .. } => binder.span(),
                    DefinitionStyle::Explicit { name } => name.span(),
                })
                .with(|it| match style {
                    DefinitionStyle::Implicit { .. } => it
                        .label(
                            attribute,
                            match kind {
                                Kind::Intrinsic => {
                                    "claims the binding is intrinsic to the language"
                                }
                                Kind::Known => "claims the binding is known to the compiler",
                            },
                        )
                        .suggest(
                            attribute,
                            "consider adding an explicit name to the attribute to overwrite the derived one",
                            Substitution::from(format!("@({kind} "))
                                .placeholder("name")
                                .str(")"),
                        ),
                    DefinitionStyle::Explicit { .. } => it,
                })
                // @Task add a UI test for this case
                .with(|it| match special {
                    Some(special) => {
                        let kind = special.kind();
                        it.note(format!("it is {} {kind} binding", kind.article()))
                    }
                    None => it,
                }));
        };

        if let Some(previous) = self.get(special) {
            return Err(Diagnostic::error()
                .code(match kind {
                    Kind::Intrinsic => ErrorCode::E040,
                    Kind::Known => ErrorCode::E039,
                })
                .message(format!("the {kind} binding ‘{special}’ is defined multiple times"))
                .span(binder, "redefinition")
                .label(previous, "previous definition"));
        }

        self.insert_unchecked(special, binder);

        Ok(special)
    }

    pub fn insert_unchecked(&mut self, special: Binding, binder: Identifier) {
        let index = binder.declaration_index().unwrap();
        self.to.insert(index, special);
        self.from.insert(special, binder);
    }

    pub fn get<K: Key>(&self, key: K) -> Option<<K as Key>::Output> {
        key.index(self)
    }

    pub fn is(&self, binder: Identifier, special: impl Into<Binding>) -> bool {
        self.get(special).is_some_and(|special| special == binder)
    }
}

#[derive(Clone, Copy)]
pub enum DefinitionStyle<'a, T = Atom> {
    /// The name of the special binding is implied by the binder & the namespace of the declaration (e.g. in `@known`).
    Implicit { namespace: Option<T> },
    /// The name of the special binding is given explicitly (e.g. in `@(intrinsic qualified.name)`).
    Explicit { name: &'a ast::Path },
}

impl<'a> DefinitionStyle<'a> {
    fn as_path(&self, binder: Identifier) -> String {
        match self {
            Self::Implicit { namespace } => {
                namespace.map_or(binder.to_string(), |namespace| format!("{namespace}.{binder}"))
            }
            Self::Explicit { name } => name.to_string(),
        }
    }

    fn as_name(&'a self, binder: Identifier) -> Option<(Option<Atom>, Atom)> {
        match self {
            &Self::Implicit { namespace } => Some((namespace, binder.bare())),
            Self::Explicit { name } => {
                if name.hanger.is_some() {
                    return None;
                }

                Some(match &*name.segments {
                    [namespace, name] => (Some(namespace.bare()), name.bare()),
                    [name] => (None, name.bare()),
                    _ => return None,
                })
            }
        }
    }
}

pub trait Key: Sized {
    type Output;

    fn index(self, bindings: &Bindings) -> Option<Self::Output>;
}

impl<B: Into<Binding>> Key for B {
    type Output = Identifier;

    fn index(self, bindings: &Bindings) -> Option<Self::Output> {
        bindings.from.get(&self.into()).copied()
    }
}

impl Key for DeclarationIndex {
    type Output = Binding;

    fn index(self, bindings: &Bindings) -> Option<Self::Output> {
        bindings.to.get(&self).copied()
    }
}
