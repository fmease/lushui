use crate::{Span, Spanning};
use std::fmt;
use utility::{Atom, obtain};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum LocalBinder<T> {
    Named(T),
    Discarded(Span),
}

impl<T: Copy> LocalBinder<T> {
    pub fn name(self) -> Option<T> {
        obtain!(self, Self::Named(binder) => binder)
    }

    pub fn map<U: Copy>(self, mapper: impl FnOnce(T) -> U) -> LocalBinder<U> {
        match self {
            Self::Named(binder) => LocalBinder::Named(mapper(binder)),
            Self::Discarded(span) => LocalBinder::Discarded(span),
        }
    }
}

impl<T: Binder> LocalBinder<T> {
    pub fn to_str(self) -> &'static str {
        match self {
            LocalBinder::Named(binder) => binder.to_str(),
            LocalBinder::Discarded(_) => Atom::UNDERSCORE.to_str(),
        }
    }
}

impl<T: Spanning> Spanning for LocalBinder<T> {
    fn span(&self) -> Span {
        match self {
            Self::Named(binder) => binder.span(),
            &Self::Discarded(span) => span,
        }
    }
}

impl<T: Binder> fmt::Display for LocalBinder<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.to_str())
    }
}

impl<T> From<T> for LocalBinder<T> {
    fn from(binder: T) -> Self {
        Self::Named(binder)
    }
}

pub trait Binder: Copy {
    fn to_str(self) -> &'static str;
}
