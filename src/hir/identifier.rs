use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicU64, Ordering as AtomicOrdering};

use crate::parser;
use crate::span::Span;

static LAST_GENERATED_NUMERIC_IDENTIFIER: AtomicU64 = AtomicU64::new(0);

// @Task update docs
/// Either an identifier found in the source program or a synthetic one.
///
/// Plain identifiers don't compare by their span only their (interned) contents.
#[derive(Clone, Debug, Eq)]
pub enum Identifier {
    Stub,
    Plain(parser::Identifier),
    Generated(parser::Identifier, u64),
}

impl Identifier {
    pub fn from(identifier: &str) -> Self {
        Identifier::Plain(Self::plain(identifier))
    }

    fn plain(identifier: &str) -> parser::Identifier {
        parser::Identifier {
            atom: crate::lexer::Atom::from(identifier),
            // @Bug ugly, error-prone
            span: Span::dummy(),
        }
    }

    pub fn refresh(&self) -> Self {
        Self::Generated(
            match self {
                Self::Stub => Identifier::plain(""),
                Self::Plain(identifier) => identifier.clone(),
                Self::Generated(identifier, _) => identifier.clone(),
            },
            LAST_GENERATED_NUMERIC_IDENTIFIER.fetch_add(1, AtomicOrdering::SeqCst),
        )
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // @Question are two stubs equal?
            (Self::Stub, Self::Stub) => true,
            (Self::Plain(left), Self::Plain(right)) => left.atom == right.atom,
            (
                Self::Generated(left_identifier, left_version),
                Self::Generated(right_identifier, right_version),
            ) => left_identifier.atom == right_identifier.atom && left_version == right_version,
            _ => false,
        }
    }
}

impl Hash for Identifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Stub => 0u64.hash(state),
            Self::Plain(identifier) => {
                1u64.hash(state);
                identifier.atom.hash(state);
            }
            Self::Generated(identifier, version) => {
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
            Self::Plain(identifier) => write!(f, "{}", identifier),
            // @Temporary
            Self::Stub => f.write_str("$"),
            Self::Generated(identifier, version) => write!(f, "{}${}", identifier, version),
        }
    }
}
