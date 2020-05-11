use std::fmt;

use crate::{
    interpreter::ffi::ForeignFunction,
    interpreter::scope::ValueView,
    parser,
    resolver::{CrateIndex, Identifier, ModuleScope},
    typer::Expression,
};

#[derive(Clone)]
pub struct Entity {
    /// Source information of the definition site.
    pub source: parser::Identifier,
    pub kind: EntityKind,
}

impl Entity {
    pub fn is_untyped_value(&self) -> bool {
        matches!(self.kind, EntityKind::UntypedValue)
    }

    pub fn r#type(&self) -> Option<Expression> {
        use EntityKind::*;

        Some(
            match &self.kind {
                Value { r#type, .. } => r#type,
                DataType { r#type, .. } => r#type,
                Constructor { r#type, .. } => r#type,
                Foreign { r#type, .. } => r#type,
                UntypedValue => return None,
                _ => unreachable!(),
            }
            .clone(),
        )
    }

    /// Retrieve the value of an entity
    pub fn value(&self) -> ValueView {
        match &self.kind {
            EntityKind::Value { expression, .. } => ValueView::Reducible(expression.clone()),
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
    Module(ModuleScope),
    /// A use bindings means extra indirection. We don't just "clone" the value it gets
    /// "assigned" to. We merely reference it. This way we don't need to reference-count
    /// module scopes (to avoid deep copies). Also, once we merge this data structure with
    /// the one from the interpreter, we can successfully alias constructors and still
    /// pattern match on them!
    /// Invariant: The "target" is never a Use itself. There are no nested aliases
    Use(CrateIndex),
    UnresolvedUse,

    Value {
        r#type: Expression,
        expression: Expression,
    },
    // @Question should we store the constructors?
    DataType {
        r#type: Expression,
        constructors: Vec<Identifier>,
    },
    Constructor {
        r#type: Expression,
    },
    Foreign {
        r#type: Expression,
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
            UntypedValue => f.write_str("untyped value"),
            Module(scope) => write!(f, "module, {:?}", scope),
            Use(index) => write!(f, "use {:?}", index),
            UnresolvedUse => write!(f, "unresolved use"),
            Value { r#type, expression } => write!(f, "{}: {}", expression, r#type),
            DataType {
                r#type,
                constructors,
            } => write!(
                f,
                "data: {} = {}",
                r#type,
                constructors
                    .iter()
                    .map(|constructor| format!("{} ", constructor))
                    .collect::<String>()
            ),
            Constructor { r#type } => write!(f, "constructor: {}", r#type),
            Foreign { r#type, .. } => write!(f, "foreign: {}", r#type),
        }
    }
}
