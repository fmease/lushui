#![feature(type_alias_impl_trait)]

use std::{
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
};

pub use derive::Index;

pub struct IndexMap<I, T> {
    values: Vec<T>,
    _marker: PhantomData<fn(&I)>,
}

impl<I, T> IndexMap<I, T> {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self::bare(Vec::with_capacity(capacity))
    }

    pub fn bare(values: Vec<T>) -> Self {
        Self { values, _marker: PhantomData }
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.values.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    #[must_use]
    pub fn last(&self) -> Option<&T> {
        self.values.last()
    }

    pub fn last_mut(&mut self) -> Option<&mut T> {
        self.values.last_mut()
    }

    pub fn pop(&mut self) -> Option<T> {
        self.values.pop()
    }

    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.values.iter()
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.values.iter_mut()
    }

    pub fn into_values(self) -> impl Iterator<Item = T> {
        self.values.into_iter()
    }

    #[must_use]
    pub fn into_vec(self) -> Vec<T> {
        self.values
    }
}

impl<I: Index, T> IndexMap<I, T> {
    #[must_use]
    pub fn next_index(&self) -> I {
        wrap(self.values.len())
    }

    #[must_use]
    pub fn last_index(&self) -> Option<I> {
        Some(wrap(self.values.len().checked_sub(1)?))
    }

    pub fn insert(&mut self, value: T) -> I {
        let index = self.next_index();
        self.values.push(value);
        index
    }

    pub fn insert_with(&mut self, constructor: impl FnOnce(I) -> T) -> I {
        let index = self.next_index();
        self.values.push(constructor(self.next_index()));
        index
    }

    pub fn get(&self, index: I) -> Option<&T> {
        self.values.get(unwrap(index))
    }

    pub fn get_mut(&mut self, index: I) -> Option<&mut T> {
        self.values.get_mut(unwrap(index))
    }

    pub fn indices(&self) -> impl Iterator<Item = I> {
        (0..self.len()).map(wrap)
    }
}

use iter::{IntoIter, Iter, IterMut};

// FIXME: Inline this module once `type_alias_impl_trait` supports `#[define]` (..).
mod iter {
    use super::{Index, IndexMap, map};

    // FIXME: Shouldn't rustc be smart enough to imply `T: 'a`?

    pub type IntoIter<I: Index, T> = impl Iterator<Item = (I, T)>;
    pub type Iter<'a, I: Index, T: 'a> = impl Iterator<Item = (I, &'a T)>;
    pub type IterMut<'a, I: Index, T: 'a> = impl Iterator<Item = (I, &'a mut T)>;

    impl<I: Index, T> IndexMap<I, T> {
        pub(super) fn into_iter(self) -> IntoIter<I, T> {
            self.values.into_iter().enumerate().map(map)
        }

        pub fn iter(&self) -> Iter<'_, I, T> {
            self.values.iter().enumerate().map(map)
        }

        pub fn iter_mut(&mut self) -> IterMut<'_, I, T> {
            self.values.iter_mut().enumerate().map(map)
        }
    }
}

impl<I, T> Default for IndexMap<I, T> {
    fn default() -> Self {
        Self::bare(Vec::new())
    }
}

impl<I, T: Clone> Clone for IndexMap<I, T> {
    fn clone(&self) -> Self {
        Self { values: self.values.clone(), _marker: PhantomData }
    }
}

impl<I, T: PartialEq> PartialEq for IndexMap<I, T> {
    fn eq(&self, other: &Self) -> bool {
        self.values == other.values
    }
}

impl<I, T: Eq> Eq for IndexMap<I, T> {}

impl<I, T: PartialOrd> PartialOrd for IndexMap<I, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.values.partial_cmp(&other.values)
    }
}

impl<I, T: Ord> Ord for IndexMap<I, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.values.cmp(&other.values)
    }
}

impl<I, T: Hash> Hash for IndexMap<I, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.values.hash(state);
    }
}

impl<I: Index, T> std::ops::Index<I> for IndexMap<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        &self.values[unwrap(index)]
    }
}

impl<I: Index, T> std::ops::IndexMut<I> for IndexMap<I, T> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.values[unwrap(index)]
    }
}

impl<I: Index + fmt::Debug, T: fmt::Debug> fmt::Debug for IndexMap<I, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self).finish()
    }
}

impl<I: Index, T> IntoIterator for IndexMap<I, T> {
    type Item = (I, T);
    type IntoIter = IntoIter<I, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.into_iter()
    }
}

impl<'a, I: Index, T> IntoIterator for &'a IndexMap<I, T> {
    type Item = (I, &'a T);
    type IntoIter = Iter<'a, I, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, I: Index, T> IntoIterator for &'a mut IndexMap<I, T> {
    type Item = (I, &'a mut T);
    type IntoIter = IterMut<'a, I, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

fn map<I: Index, T>((index, value): (usize, T)) -> (I, T) {
    (wrap(index), value)
}

fn wrap<I: Index>(index: usize) -> I {
    I::new(index.try_into().unwrap(), Guard::THE)
}

fn unwrap<I: Index>(index: I) -> usize {
    index.into_inner(Guard::THE).try_into().unwrap()
}

pub trait Index {
    type Representation: TryFrom<usize, Error: fmt::Debug> + TryInto<usize, Error: fmt::Debug>;

    /// This function is only meant to be called by the `index_map` crate (hence the guard).
    fn new(index: Self::Representation, _: Guard) -> Self;

    /// This function is only meant to be called by the `index_map` crate (hence the guard).
    fn into_inner(self, _: Guard) -> Self::Representation;
}

// Of course, this guard is not foolproof[^1] but that's fine. It's only meant to be a reminder.
// [^1]: An adversarial impl may "steal" the guard, store it somewhere (via a global variable)
//       and subsequently retrieve it from almost anywhere.
pub struct Guard(());

impl Guard {
    const THE: Self = Self(());
}
