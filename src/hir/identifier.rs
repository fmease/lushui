use std::fmt;
use std::hash::{Hash, Hasher};

pub type RefreshState<'a> = &'a mut u64;

/// Either an identifier found in the source program or a synthetic one.
///
/// Plain identifiers don't compare by their span only their (interned) contents.
#[derive(Clone, Debug, Eq)]
pub enum Identifier {
    Plain(crate::parser::Identifier),
    // @Question should we keep a base identifier for pretty-printing?
    // like Generated(parser::Identifier, u64) preserving the source
    // of the synthetic identifier
    Generated(u64),
}

impl Identifier {
    pub fn refresh(&self, state: RefreshState<'_>) -> Self {
        *state += 1;
        Self::Generated(*state)
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Plain(left), Self::Plain(right)) => left.atom == right.atom,
            (Self::Generated(left), Self::Generated(right)) => left == right,
            _ => false,
        }
    }
}

impl Hash for Identifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Plain(identifier) => {
                1u64.hash(state);
                identifier.atom.hash(state);
            }
            Self::Generated(identifier) => {
                2u64.hash(state);
                identifier.hash(state);
            }
        }
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Plain(identifier) => write!(f, "{}", identifier),
            // @Temporary
            Self::Generated(identifier) => write!(f, "${}", identifier),
        }
    }
}
