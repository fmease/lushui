use crate::HashMap;
use index_map::{Index, IndexMap};
use std::{
    fmt,
    iter::zip,
    sync::{LazyLock, Mutex},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Atom(u32);

impl Atom {
    pub fn new(value: &str) -> Self {
        Interner::the().lock().unwrap().intern(value)
    }

    pub fn from_owned(value: String) -> Self {
        Interner::the().lock().unwrap().intern_owned(value)
    }

    pub fn to_str(self) -> &'static str {
        Interner::the().lock().unwrap().get(self)
    }
}

impl Index for Atom {
    fn new(index: usize) -> Self {
        Self(index.try_into().unwrap())
    }

    fn value(self) -> usize {
        self.0 as _
    }
}

impl fmt::Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.to_str())
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.to_str())
    }
}

impl From<&str> for Atom {
    fn from(value: &str) -> Self {
        Self::new(value)
    }
}

impl From<String> for Atom {
    fn from(value: String) -> Self {
        Self::from_owned(value)
    }
}

// Positive ordering impls would be a pitfall:
//
// * Comparison by index would be fast but highly unintuitive and in most cases not what
//   the user wants.
// * Comparison by string would be rather slow and the user should explicitly opt-in
//   via `Atom::to_str`.
impl !PartialOrd for Atom {}
impl !Ord for Atom {}

struct Interner {
    atoms: HashMap<&'static str, Atom>,
    strings: IndexMap<Atom, &'static str>,
}

impl Interner {
    fn the() -> &'static Mutex<Self> {
        static SELF: LazyLock<Mutex<Interner>> = LazyLock::new(|| Mutex::new(Interner::new()));

        &SELF
    }

    fn with(values: Vec<&'static str>) -> Self {
        Self {
            atoms: zip(&values, 0..)
                .map(|(&string, atom)| (string, Atom(atom)))
                .collect(),
            strings: IndexMap::bare(values),
        }
    }

    fn intern(&mut self, value: &str) -> Atom {
        if let Some(&atom) = self.atoms.get(value) {
            return atom;
        }

        self.insert(Box::leak(Box::from(value)))
    }

    fn intern_owned(&mut self, value: String) -> Atom {
        if let Some(&atom) = self.atoms.get(&*value) {
            return atom;
        }

        self.insert(String::leak(value))
    }

    fn insert(&mut self, value: &'static str) -> Atom {
        let atom = self.strings.insert(value);
        self.atoms.insert(value, atom);
        atom
    }

    fn get(&self, atom: Atom) -> &'static str {
        self.strings[atom]
    }
}

macro_rules! atoms {
    ($( $atom:ident $( => $string:literal )? ),* $(,)?) => {
        #[allow(non_upper_case_globals)]
        impl Atom {
            $(
                pub const $atom: Self = Self(${ index() });
            )*
        }

        impl Interner {
            fn new() -> Self {
                Self::with(vec![
                    $( atoms!(@str $atom $( $string )?) ),*
                ])
            }
        }
    };
    (@str $atom:ident $string:literal) => { $string };
    (@str $atom:ident) => { stringify!($atom) };
}

atoms! {
    abstract_ => "abstract",
    add,
    allow,
    Bool,
    concat,
    deny,
    deprecated,
    depth,
    display,
    divide,
    doc,
    empty,
    equal,
    false_ => "false",
    forbid,
    greater_equal => "greater-equal",
    greater,
    if_ => "if",
    ignore,
    include,
    Int,
    Int32,
    Int64,
    intrinsic,
    io,
    IO,
    known,
    less_equal => "less-equal",
    less,
    lint,
    List,
    location,
    main,
    moving,
    multiply,
    name,
    nat,
    Nat,
    nat32,
    Nat32,
    Nat64,
    none,
    Option,
    path,
    prepend,
    print,
    public,
    reach,
    reason,
    recursion_limit => "recursion-limit",
    replacement,
    some,
    static_ => "static",
    statistics,
    subtract,
    successor,
    test,
    text,
    Text,
    true_ => "true",
    Tuple,
    Type,
    unchecked_subtract => "unchecked-subtract",
    underscore => "_",
    unit,
    Unit,
    unsafe_ => "unsafe",
    unstable,
    Vector,
    warn,
}
