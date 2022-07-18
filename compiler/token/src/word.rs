use std::fmt;
use utilities::Atom;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Word(Atom);

impl Word {
    pub fn new_unchecked(name: Atom) -> Self {
        Self(name)
    }

    pub fn into_inner(self) -> Atom {
        self.0
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Debug for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}w", self.as_str())
    }
}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
