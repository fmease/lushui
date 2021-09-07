//! Spanned-key map.

use crate::{
    span::{Span, Spanned},
    util::HashMap,
};
use std::{hash::Hash, iter::FromIterator};

#[derive(Debug)]
pub struct SpannedKeyMap<K, V>(pub(crate) HashMap<K, Store<V>>);

impl<K, V> SpannedKeyMap<K, V> {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn insert(&mut self, key: Spanned<K>, value: V)
    where
        K: Eq + Hash,
    {
        self.0.insert(
            key.kind,
            Store {
                key: key.span,
                value,
            },
        );
    }

    // @Task impl IntoIterator
    pub fn into_iter(self) -> impl Iterator<Item = ExternalEntry<K, V>> {
        self.0
            .into_iter()
            .map(|(key, store)| (Spanned::new(store.key, key), store.value))
    }

    pub fn into_keys(self) -> impl Iterator<Item = Spanned<K>> {
        self.0
            .into_iter()
            .map(|(key, store)| Spanned::new(store.key, key))
    }

    pub fn iter(&self) -> impl Iterator<Item = (Spanned<&K>, &V)> {
        self.0
            .iter()
            .map(|(key, store)| (Spanned::new(store.key, key), &store.value))
    }
}

impl<K, V> Default for SpannedKeyMap<K, V> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<K: Eq + Hash, V> Extend<ExternalEntry<K, V>> for SpannedKeyMap<K, V> {
    fn extend<T: IntoIterator<Item = ExternalEntry<K, V>>>(&mut self, iterator: T) {
        self.0.extend(iterator.into_iter().map(internal_entry))
    }
}

impl<K: Eq + Hash, V> FromIterator<ExternalEntry<K, V>> for SpannedKeyMap<K, V> {
    fn from_iter<T: IntoIterator<Item = ExternalEntry<K, V>>>(iterator: T) -> Self {
        Self(iterator.into_iter().map(internal_entry).collect())
    }
}

fn internal_entry<K, V>((key, value): ExternalEntry<K, V>) -> InternalEntry<K, V> {
    (
        key.kind,
        Store {
            key: key.span,
            value,
        },
    )
}

type ExternalEntry<K, V> = (Spanned<K>, V);
type InternalEntry<K, V> = (K, Store<V>);

#[derive(Debug)]
pub(crate) struct Store<V> {
    pub(crate) value: V,
    pub(crate) key: Span,
}
