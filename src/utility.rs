//! Utility functionality and definitions.

pub(crate) mod lexer;

pub(crate) use num_bigint::{BigInt as Int, BigUint as Nat};
pub(crate) use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::{ffi::OsStr, path::Path};
pub(crate) use string_cache::DefaultAtom as Atom;

pub(crate) type Str = std::borrow::Cow<'static, str>;

pub(crate) type SmallVec<T, const N: usize> = smallvec::SmallVec<[T; N]>;

pub(crate) fn has_file_extension(path: &Path, required_extension: &str) -> bool {
    path.extension().and_then(OsStr::to_str) == Some(required_extension)
}

// @Task replace `$( $pat:pat_param )|+` with `$pat:pat` once rust-analyzer understands
// 2021 patterns
pub(crate) macro obtain($expr:expr, $( $pat:pat_param )|+ $( if $guard:expr )? $(,)? => $mapping:expr $(,)?) {
    match $expr {
        $( $pat )|+ $( if $guard )? => Some($mapping),
        _ => None
    }
}

pub(crate) trait GetFromEndExt {
    type Item;

    fn get_from_end(&self, index: usize) -> Option<&Self::Item>;
}

impl<T> GetFromEndExt for [T] {
    type Item = T;

    fn get_from_end(&self, index: usize) -> Option<&Self::Item> {
        let index = self.len().checked_sub(index.checked_add(1)?)?;
        Some(unsafe { self.get_unchecked(index) })
    }
}

pub(crate) macro try_all {
    ($( $binder:ident ),* ; $( $continuation:stmt);+ $(;)?) => {
        #[allow(redundant_semicolons, unused_variables)]
        let ($( $binder, )*) = match ($( ::std::ops::Try::branch($binder), )*) {
            ($( ::std::ops::ControlFlow::Continue($binder), )*) => ($( $binder, )*),
            _ => { $( $continuation );+ }
        };
    },
}

pub(crate) macro unrc($compound:ident.$projection:ident) {
    ::std::rc::Rc::try_unwrap($compound)
        .map(|compound| compound.$projection)
        .unwrap_or_else(|compound| compound.$projection.clone())
}

pub(crate) macro condition($( $condition:expr => $consequence:expr ),+ $(, else => $alternative:expr )? $(,)?) {
    match () {
        $( _ if $condition => $consequence, )+
        $( _ => $alternative )?
    }
}
