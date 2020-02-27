use std::{
    fmt,
    hash::{Hash, Hasher},
    sync::atomic::{AtomicU64, Ordering as AtomicOrdering},
};

use crate::{parser, span::Span};

static LAST_GENERATED_NUMERIC_IDENTIFIER: AtomicU64 = AtomicU64::new(0);

/// Either an identifier found in the source program or a synthetic one.
///
/// Sourced identifiers don't compare by their span only their (interned) contents.
#[derive(Clone, Debug, Eq)]
pub enum Identifier {
    Sourced(parser::Identifier),
    Synthetic(parser::Identifier, u64),
}

impl Identifier {
    pub fn from(identifier: &str) -> Self {
        Identifier::Sourced(Self::sourced(identifier))
    }

    fn sourced(identifier: &str) -> parser::Identifier {
        parser::Identifier {
            atom: crate::Atom::from(identifier),
            // @Bug ugly, error-prone
            span: Span::dummy(),
        }
    }

    pub fn refresh(&self) -> Self {
        Self::Synthetic(
            match self {
                Self::Sourced(identifier) => identifier.clone(),
                Self::Synthetic(identifier, _) => identifier.clone(),
            },
            LAST_GENERATED_NUMERIC_IDENTIFIER.fetch_add(1, AtomicOrdering::SeqCst),
        )
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Sourced(left), Self::Sourced(right)) => left.atom == right.atom,
            (
                Self::Synthetic(left_identifier, left_version),
                Self::Synthetic(right_identifier, right_version),
            ) => left_identifier.atom == right_identifier.atom && left_version == right_version,
            _ => false,
        }
    }
}

impl Hash for Identifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Sourced(identifier) => {
                1u64.hash(state);
                identifier.atom.hash(state);
            }
            Self::Synthetic(identifier, version) => {
                2u64.hash(state);
                identifier.atom.hash(state);
                version.hash(state);
            }
        }
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Sourced(identifier) => write!(f, "{}", identifier),
            // @Temporary
            Self::Synthetic(identifier, version) => write!(f, "{}${}", identifier, version),
        }
    }
}
