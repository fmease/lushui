use std::fmt;

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct ComponentIndex(pub u16);

impl ComponentIndex {}

impl fmt::Debug for ComponentIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}c", self.0)
    }
}

impl index_map::Index for ComponentIndex {
    fn new(index: usize) -> Self {
        Self(index.try_into().unwrap())
    }

    fn value(self) -> usize {
        self.0 as _
    }
}
