use std::fmt;
use utilities::Atom;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Word(Atom);

impl Word {
    pub fn new_unchecked(name: Atom) -> Self {
        Self(name)
    }

    pub fn into_inner(self) -> Atom {
        self.0
    }

    pub fn to_str(self) -> &'static str {
        self.0.to_str()
    }
}

impl fmt::Debug for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}w", self.to_str())
    }
}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}
