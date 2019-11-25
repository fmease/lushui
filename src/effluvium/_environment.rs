use std::borrow::Borrow;

#[derive(Debug)]
pub struct EnvironmentOwner<K, V> {
    bindings: Vec<(K, V)>,
}

impl<K, V> EnvironmentOwner<K, V> {
    pub fn new() -> Self {
        EnvironmentOwner {
            bindings: Vec::new(),
        }
    }
    pub fn with_capacity(capacity: usize) -> Self {
        EnvironmentOwner {
            bindings: Vec::with_capacity(capacity),
        }
    }
}

// @Temporary design

// @Note this should be something we own
pub struct Environment<'a, K, V> {
    owner: &'a mut EnvironmentOwner<K, V>,
}

impl<'a, K, V> Environment<'a, K, V> {
    pub fn new(owner: &'a mut EnvironmentOwner<K, V>) -> Self {
        Self { owner }
    }

    pub fn insert(self, key: K, value: V) {
        self.owner.bindings.push((key, value));
        
    }

    pub fn lookup<Q>(&self, key: &Q) -> Option<&V>
    where
        Q: ?Sized + Eq + Borrow<K>,
        K: Eq,
    {
        for binding in self.owner.bindings.iter().rev() {
            if &binding.0 == key.borrow() {
                return Some(&binding.1);
            }
        }

        None
    }
}

// @Bug if insert isn't called, this will pop "foreign" bindings
impl<'a, K, V> Drop for Environment<'a, K, V> {
    fn drop(&mut self) {
        self.owner.bindings.pop();
    }
}

fn test() {
    let mut owner = EnvironmentOwner::new();
    let global = Environment::new(&mut owner);

    global.insert("foo", 34);

    foo(Environment::new(&mut owner));
    foo(Environment::new(&mut owner));
}

fn foo(env: Environment<'_, &str, i32>) {}
