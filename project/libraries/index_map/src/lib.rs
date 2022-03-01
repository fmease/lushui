#![deny(rust_2018_idioms, unused_must_use)]

pub use derive::Index;
use std::{
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
};

// @Task impl IntoIterator, Extend, docs

pub struct IndexMap<I, T> {
    values: Vec<T>,
    _marker: PhantomData<fn(&I)>,
}

impl<I, T> IndexMap<I, T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_vec(values: Vec<T>) -> Self {
        Self {
            values,
            _marker: PhantomData,
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self::from_vec(Vec::with_capacity(capacity))
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

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

    pub fn into_vec(self) -> Vec<T> {
        self.values
    }
}

impl<I: Index, T> IndexMap<I, T> {
    pub fn next_index(&self) -> I {
        I::new(self.values.len())
    }

    pub fn last_index(&self) -> Option<I> {
        Some(I::new(self.values.len().checked_sub(1)?))
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
        self.values.get(index.value())
    }

    pub fn get_mut(&mut self, index: I) -> Option<&mut T> {
        self.values.get_mut(index.value())
    }

    pub fn iter(&self) -> impl Iterator<Item = (I, &T)> {
        self.values.iter().enumerate().map(map_entry)
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (I, &mut T)> {
        self.values.iter_mut().enumerate().map(map_entry)
    }

    pub fn into_iter(self) -> impl Iterator<Item = (I, T)> {
        self.values.into_iter().enumerate().map(map_entry)
    }

    pub fn indices(&self) -> impl Iterator<Item = I> {
        (0..self.len()).map(I::new)
    }
}

impl<I, T> Default for IndexMap<I, T> {
    fn default() -> Self {
        Self::from_vec(Vec::new())
    }
}

impl<I, T: Clone> Clone for IndexMap<I, T> {
    fn clone(&self) -> Self {
        Self {
            values: self.values.clone(),
            _marker: PhantomData,
        }
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
        &self.values[index.value()]
    }
}

impl<I: Index, T> std::ops::IndexMut<I> for IndexMap<I, T> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.values[index.value()]
    }
}

impl<I: Index + fmt::Debug, T: fmt::Debug> fmt::Debug for IndexMap<I, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

fn map_entry<I: Index, T>((index, value): (usize, T)) -> (I, T) {
    (I::new(index), value)
}

pub trait Index {
    fn new(index: usize) -> Self;

    fn value(self) -> usize;
}
