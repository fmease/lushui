use std::fmt;

use crate::{
    interpreter::ffi::ForeignFunction,
    interpreter::scope::ValueView,
    parser,
    resolver::{CrateIndex, Identifier, Namespace},
    typer::Expression,
};

#[derive(Clone)]
pub struct Entity {
    /// Source information of the definition site.
    pub source: parser::Identifier,
    pub kind: EntityKind,
}

impl Entity {
    pub fn is_untyped(&self) -> bool {
        matches!(self.kind, EntityKind::UntypedValue | EntityKind::UntypedDataType(_))
    }

    pub fn type_(&self) -> Option<Expression> {
        use EntityKind::*;

        Some(
            match &self.kind {
                Value { type_, .. } => type_,
                DataType { type_, .. } => type_,
                Constructor { type_, .. } => type_,
                Foreign { type_, .. } => type_,
                UntypedValue | UntypedDataType(_) => return None,
                _ => unreachable!(),
            }
            .clone(),
        )
    }

    /// Retrieve the value of an entity
    pub fn value(&self) -> ValueView {
        match &self.kind {
            EntityKind::Value {
                expression: Some(expression),
                ..
            } => ValueView::Reducible(expression.clone()),
            EntityKind::Value {
                expression: None, ..
            } => ValueView::Neutral,
            kind if kind.is_resolver_specific() => unreachable!(),
            _ => ValueView::Neutral,
        }
    }
}

impl fmt::Debug for Entity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:20} |-> {:?}", self.source, self.kind)
    }
}

#[derive(Clone)]
pub enum EntityKind {
    UntypedValue,
    Module(Namespace),
    UntypedDataType(Namespace),
    /// A use bindings means extra indirection. We don't just "clone" the value it gets
    /// "assigned" to. We merely reference it. This way we don't need to reference-count
    /// module scopes (to avoid deep copies). Also, once we merge this data structure with
    /// the one from the interpreter, we can successfully alias constructors and still
    /// pattern match on them!
    /// Invariant: The "target" is never a Use itself. There are no nested aliases
    Use(CrateIndex),
    UnresolvedUse,

    Value {
        type_: Expression,
        expression: Option<Expression>,
    },
    // @Question should we store the constructors?
    DataType {
        type_: Expression,
        constructors: Vec<Identifier>,
    },
    Constructor {
        type_: Expression,
    },
    Foreign {
        type_: Expression,
        arity: usize,
        function: ForeignFunction,
    },
}

impl EntityKind {
    fn is_resolver_specific(&self) -> bool {
        use EntityKind::*;

        matches!(self, UntypedValue | Module(_) | Use(_) | UnresolvedUse)
    }
}

impl fmt::Debug for EntityKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use EntityKind::*;

        match self {
            UntypedValue => write!(f, "untyped value"),
            Module(scope) => write!(f, "module, {:?}", scope),
            UntypedDataType(scope) => write!(f, "untyped data type, {:?}", scope),
            Use(index) => write!(f, "use {:?}", index),
            UnresolvedUse => write!(f, "unresolved use"),
            Value { type_, expression } => match expression {
                Some(expression) => write!(f, "{}: {}", expression, type_),
                None => write!(f, ": {}", type_),
            },
            DataType {
                type_,
                constructors,
            } => write!(
                f,
                "data: {} = {}",
                type_,
                constructors
                    .iter()
                    .map(|constructor| format!("{} ", constructor))
                    .collect::<String>()
            ),
            Constructor { type_ } => write!(f, "constructor: {}", type_),
            Foreign { type_, .. } => write!(f, "foreign: {}", type_),
        }
    }
}
