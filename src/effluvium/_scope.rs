use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;
use std::ops::{Deref, DerefMut};

#[derive(Debug)]
pub struct Scope<'p, K: Eq + Hash, V> {
    parent: Option<&'p Scope<'p, K, V>>,
    bindings: HashMap<K, V>,
}

impl<'p, K: Eq + Hash, V> Scope<'p, K, V> {
    pub fn with_parent(scope: &'p Scope<'p, K, V>) -> Self {
        Self {
            parent: Some(scope),
            bindings: HashMap::new(),
        }
    }

    pub fn lookup<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Eq + Hash + ?Sized,
    {
        self.bindings
            .get(key)
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.lookup(key)))
    }
}

impl<'p, K: Eq + Hash, V> Default for Scope<'p, K, V> {
    fn default() -> Self {
        Self {
            parent: None,
            bindings: HashMap::new(),
        }
    }
}

impl<'p, K: Eq + Hash, V> Deref for Scope<'p, K, V> {
    type Target = HashMap<K, V>;

    fn deref(&self) -> &Self::Target {
        &self.bindings
    }
}

impl<'p, K: Eq + Hash, V> DerefMut for Scope<'p, K, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.bindings
    }
}
