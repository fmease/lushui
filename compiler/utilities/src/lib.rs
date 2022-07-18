//! Utility functionality and definitions.
#![feature(associated_type_bounds, decl_macro, never_type_fallback, never_type)]

use colored::Colorize;
use difference::{Changeset, Difference};
pub use num_bigint::{BigInt as Int, BigUint as Nat};
pub use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::{cell::Cell, ffi::OsStr, fmt, path::Path};
pub use string_cache::DefaultAtom as Atom;

pub mod cycle;

// @Question should this reside somewhere else?
pub const FILE_EXTENSION: &str = "lushui";

pub type Str = std::borrow::Cow<'static, str>;

pub type SmallVec<T, const N: usize> = smallvec::SmallVec<[T; N]>;

pub fn has_file_extension(path: &Path, required_extension: &str) -> bool {
    path.extension().and_then(OsStr::to_str) == Some(required_extension)
}

// @Task replace `$( $pat:pat_param )|+` with `$pat:pat` once rust-analyzer understands
// 2021 patterns
pub macro obtain($expr:expr, $( $pat:pat_param )|+ $( if $guard:expr )? $(,)? => $mapping:expr $(,)?) {
    match $expr {
        $( $pat )|+ $( if $guard )? => Some($mapping),
        _ => None
    }
}

pub trait GetFromEndExt {
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

pub macro try_all {
    ($( $binder:ident ),* ; $( $continuation:stmt);+ $(;)?) => {
        #[allow(redundant_semicolons, unused_variables)]
        let ($( $binder, )*) = match ($( ::std::ops::Try::branch($binder), )*) {
            ($( ::std::ops::ControlFlow::Continue($binder), )*) => ($( $binder, )*),
            _ => { $( $continuation );+ }
        };
    },
}

pub macro condition($( $condition:expr => $consequence:expr ),+ $(, else => $alternative:expr )? $(,)?) {
    match () {
        $( _ if $condition => $consequence, )+
        $( _ => $alternative )?
    }
}

#[allow(unused_macros)]
pub macro no_std_assert($( $anything:tt )*) {
    compile_error!("use the function `assert_eq` instead of macro `assert_eq` and similar")
}

pub fn difference(original: &str, edit: &str, split: &str) -> String {
    use std::io::Write;

    let mut buffer = Vec::new();

    // the provided Display implementation for Changesets is unreadable when whitespace differs
    for difference in Changeset::new(original, edit, split).diffs {
        match difference {
            Difference::Same(lines) => {
                for line in lines.lines() {
                    writeln!(buffer, "{} {line}", " ".on_bright_white()).unwrap();
                }
            }
            Difference::Add(lines) => {
                for line in lines.lines().chain(lines.is_empty().then(|| "")) {
                    writeln!(buffer, "{} {}", "+".black().on_green(), line.green()).unwrap();
                }
            }
            Difference::Rem(lines) => {
                for line in lines.lines().chain(lines.is_empty().then(|| "")) {
                    writeln!(buffer, "{} {}", "-".black().on_red(), line.red()).unwrap();
                }
            }
        }
    }

    String::from_utf8(buffer).unwrap()
}

pub trait ListingExt {
    fn list(self, conjunction: Conjunction) -> String;
}

impl<I> ListingExt for I
where
    I: Iterator<Item: Clone + fmt::Display> + Clone,
{
    fn list(self, conjunction: Conjunction) -> String {
        let mut this = self.peekable();
        let mut first = true;
        let mut result = String::new();

        while let Some(item) = this.next() {
            if !first {
                if this.peek().is_some() {
                    result += ", ";
                } else {
                    use std::fmt::Write;
                    write!(result, " {conjunction} ").unwrap();
                }
            }

            result += &item.to_string();
            first = false;
        }

        result
    }
}

#[derive(Clone, Copy)]
pub enum Conjunction {
    And,
    Or,
}

impl fmt::Display for Conjunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::And => "and",
            Self::Or => "or",
        })
    }
}

/// Use the singular or form the plural of the given word depending on the given amount.
///
/// # Examples
///
/// ```
/// # use utilities::pluralize;
/// # fn main() {
/// assert_eq!(pluralize!(1, "factor"), "factor");
/// assert_eq!(pluralize!(15, "factor"), "factors");
/// assert_eq!(pluralize!(0, "person", "people"), "people");
/// # }
/// ```
pub macro pluralize {
    ($amount:expr, $singular:expr, $plural:expr $(,)?) => {
        match $amount {
            1 => std::borrow::Cow::<'_, str>::from($singular),
            _ => $plural.into(),
        }
    },
    ($amount:expr, $singular:literal $(,)?) => {
        match $amount {
            1 => $singular,
            _ => concat!($singular, "s"),
        }
    }
}

pub trait QuoteExt {
    fn quote(self) -> String;
}

impl<D: fmt::Display> QuoteExt for D {
    fn quote(self) -> String {
        format!("‘{self}’")
    }
}

pub macro quoted($code:expr) {
    concat!("‘", $code, "’")
}

// @Task replace this whole business with a `write_*` function
pub struct AutoColoredChangeset<'a>(pub &'a Changeset);

impl fmt::Display for AutoColoredChangeset<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let split = &self.0.split;

        for difference in &self.0.diffs {
            match difference {
                Difference::Same(snippet) => {
                    write!(f, "{snippet}{split}")?;
                }
                Difference::Add(snippet) => {
                    // @Task get rid of wasteful allocation
                    write!(f, "{}{split}", snippet.green())?;
                }
                Difference::Rem(snippet) => {
                    // @Task get rid of wasteful allocation
                    write!(f, "{}{split}", snippet.red())?;
                }
            }
        }

        Ok(())
    }
}

pub trait AsAutoColoredChangeset {
    fn auto_colored(&self) -> AutoColoredChangeset<'_>;
}

impl AsAutoColoredChangeset for Changeset {
    fn auto_colored(&self) -> AutoColoredChangeset<'_> {
        AutoColoredChangeset(self)
    }
}

pub struct IOError<'a>(pub std::io::Error, pub &'a Path);

impl fmt::Display for IOError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // @Question custom messages?
        write!(f, "‘{}’: {}", self.1.to_string_lossy(), self.0)
    }
}

pub fn debugged<'f>(
    formatter: impl FnOnce(&mut fmt::Formatter<'_>) -> fmt::Result + 'f,
) -> impl fmt::Debug + 'f {
    Formatted(Cell::new(Some(formatter)))
}

pub fn displayed<'f>(
    formatter: impl FnOnce(&mut fmt::Formatter<'_>) -> fmt::Result + 'f,
) -> impl fmt::Display + 'f {
    Formatted(Cell::new(Some(formatter)))
}

struct Formatted<F>(Cell<Option<F>>);

impl<F: FnOnce(&mut fmt::Formatter<'_>) -> fmt::Result> fmt::Debug for Formatted<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.take().unwrap()(f)
    }
}

impl<F: FnOnce(&mut fmt::Formatter<'_>) -> fmt::Result> fmt::Display for Formatted<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.take().unwrap()(f)
    }
}

// @Note this has to reside in this crate since crate `span` depends on this definition
#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct ComponentIndex(pub u16);

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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn listing_no_elements() {
        assert_eq!(std::iter::empty::<!>().list(Conjunction::And), "");
    }

    #[test]
    fn listing_one_element() {
        assert_eq!(std::iter::once(1).list(Conjunction::Or), "1");
    }

    #[test]
    fn listing_two_elements() {
        assert_eq!(
            [false, true].into_iter().list(Conjunction::And),
            "false and true"
        );
    }

    #[test]
    fn listing_three_elements() {
        assert_eq!([1, 2, 3].into_iter().list(Conjunction::Or), "1, 2 or 3");
    }

    #[test]
    fn listing_many_elements() {
        assert_eq!(
            ["a", "b", "c", "d", "e", "f", "g"]
                .into_iter()
                .list(Conjunction::And),
            "a, b, c, d, e, f and g"
        );
    }
}