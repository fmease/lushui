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
    pub fn to_str(self) -> &'static str {
        Interner::the().lock().unwrap().get(self)
    }
}

impl From<&str> for Atom {
    fn from(value: &str) -> Self {
        Interner::the().lock().unwrap().intern_borrowed(value)
    }
}

impl From<String> for Atom {
    fn from(value: String) -> Self {
        Interner::the().lock().unwrap().intern_owned(value)
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

// Ordering impls for Atom would be a pitfall:
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

    fn intern_borrowed(&mut self, value: &str) -> Atom {
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
    ($( $atom:ident => $string:literal ),* $(,)?) => {
        impl Atom {
            $(
                pub const $atom: Self = Self(${ index() });
            )*
        }

        impl Interner {
            fn new() -> Self {
                Self::with(vec![
                    $( $string ),*
                ])
            }
        }
    };
    (@str $atom:ident $string:literal) => { $string };
    (@str $atom:ident) => { stringify!($atom) };
}

atoms! {
    ABSTRACT => "abstract",
    ADD => "add",
    ALLOW => "allow",
    AS => "as",
    BOOL => "Bool",
    CASE => "case",
    CONCAT => "concat",
    DATA => "data",
    DENY => "deny",
    DEPRECATED => "deprecated",
    DEPTH => "depth",
    DISPLAY => "display",
    DIVIDE => "divide",
    DO => "do",
    DOC => "doc",
    EMPTY => "empty",
    EQUAL => "equal",
    EXTERN => "extern",
    FALSE => "false",
    FOR_LOWER => "for",
    FOR_UPPER => "For",
    FORBID => "forbid",
    GIVEN => "given",
    GREATER => "greater",
    GREATER_EQUAL => "greater-equal",
    IF => "if",
    IGNORE => "ignore",
    IN => "in",
    INCLUDE => "include",
    INT => "Int",
    INT32 => "Int32",
    INT64 => "Int64",
    INTRINSIC => "intrinsic",
    IO_LOWER => "io",
    IO_UPPER => "IO",
    KNOWN => "known",
    LESS => "less",
    LESS_EQUAL => "less-equal",
    LET => "let",
    LINT => "lint",
    LIST => "List",
    LOCATION => "location",
    MAIN => "main",
    MODULE => "module",
    MOVING => "moving",
    MULTIPLY => "multiply",
    NAME => "name",
    NAT_LOWER => "nat",
    NAT_UPPER => "Nat",
    NAT32_LOWER => "nat32",
    NAT32_UPPER => "Nat32",
    NAT64 => "Nat64",
    NONE => "none",
    OF => "of",
    OPTION => "Option",
    PATH => "path",
    PREPEND => "prepend",
    PRINT => "print",
    PUBLIC => "public",
    REACH => "reach",
    REASON => "reason",
    RECORD => "record",
    RECURSION_LIMIT => "recursion-limit",
    REPLACEMENT => "replacement",
    SELF => "self",
    SOME => "some",
    STATIC => "static",
    STATISTICS => "statistics",
    SUBTRACT => "subtract",
    SUCCESSOR => "successor",
    SUPER => "super",
    TEST => "test",
    TEXT_LOWER => "text",
    TEXT_UPPER => "Text",
    TOPMOST => "topmost",
    TRAIT => "trait",
    TRUE => "true",
    TUPLE => "Tuple",
    TYPE => "Type",
    UNCHECKED_SUBTRACT => "unchecked-subtract",
    UNDERSCORE => "_",
    UNIT_LOWER => "unit",
    UNIT_UPPER => "Unit",
    UNSAFE => "unsafe",
    UNSTABLE => "unstable",
    USE => "use",
    VECTOR => "Vector",
    WARN => "warn",
}
