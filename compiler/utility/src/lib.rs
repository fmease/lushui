//! Utility functionality and definitions.
#![feature(
    decl_macro,
    macro_metavar_expr,
    negative_impls,
    never_type_fallback,
    never_type,
    os_string_pathbuf_leak
)]

use difference::Difference;
use paint::{AnsiColor, ColorExt, Painter};
use std::{
    cell::Cell,
    ffi::OsStr,
    fmt,
    io::{self, Write},
    path::Path,
};

pub use atom::Atom;
pub use difference::Changeset;
pub use num_bigint::{BigInt as Int, BigUint as Nat};
pub use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
pub use smallvec::smallvec;

pub mod atom;
pub mod cycle;
pub mod paint;
pub mod path;

pub const FILE_EXTENSION: &str = "lushui"; // @Question worth to be an Atom?
pub const PROGRAM_ENTRY: Atom = Atom::MAIN;

pub type Str = std::borrow::Cow<'static, str>;

pub type SmallVec<T, const N: usize> = smallvec::SmallVec<[T; N]>;

pub fn has_file_extension(path: &Path, required_extension: &str) -> bool {
    path.extension().and_then(OsStr::to_str) == Some(required_extension)
}

pub macro obtain($expr:expr, $pat:pat $( if $guard:expr )? $(,)? => $mapping:expr $(,)?) {
    match $expr {
        $pat $( if $guard )? => Some($mapping),
        _ => None
    }
}

pub fn default<T: Default>() -> T {
    T::default()
}

// This can theoretically be generalized using GATs to support the implementors
// R<O<T>, E> and O<R<T, E>>. However, rustc's type inference engine sucks and
// cannot handle the use sites in the simplest of cases 😭.
pub trait AndThenMapExt<T, E> {
    fn and_then_map<U, F: FnOnce(T) -> Result<U, E>>(self, mapper: F) -> Result<Option<U>, E>;
}

impl<T, E> AndThenMapExt<T, E> for Result<Option<T>, E> {
    fn and_then_map<U, F: FnOnce(T) -> Result<U, E>>(self, mapper: F) -> Result<Option<U>, E> {
        self.and_then(|value| value.map(mapper).transpose())
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
        $( () if $condition => $consequence, )+
        $( () => $alternative )?
    }
}

#[allow(unused_macros)]
pub macro no_std_assert($( $anything:tt )*) {
    compile_error!("use the function `assert_eq` instead of macro `assert_eq` and similar")
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum OwnedOrBorrowed<'a, T> {
    Owned(T),
    Borrowed(&'a T),
}

impl<'a, T> OwnedOrBorrowed<'a, T> {
    #[allow(clippy::should_implement_trait)] // ambiguity with `std::conver::AsRef` is fine
    pub fn as_ref(&'a self) -> &'a T {
        match self {
            Self::Owned(value) => value,
            Self::Borrowed(value) => value,
        }
    }
}

pub trait ChangesetExt {
    fn render(&self, painter: &mut Painter) -> io::Result<()>;
    fn render_with_ledge(&self, painter: &mut Painter) -> io::Result<()>;
}

impl ChangesetExt for Changeset {
    fn render(&self, painter: &mut Painter) -> io::Result<()> {
        for difference in &self.diffs {
            match difference {
                Difference::Same(snippet) => {
                    write!(painter, "{snippet}{}", self.split)?;
                }
                Difference::Add(snippet) => {
                    painter.set(AnsiColor::Green)?;
                    write!(painter, "{snippet}{}", self.split)?;
                    painter.unset()?;
                }
                Difference::Rem(snippet) => {
                    painter.set(AnsiColor::Red)?;
                    write!(painter, "{snippet}{}", self.split)?;
                    painter.unset()?;
                }
            }
        }

        Ok(())
    }

    fn render_with_ledge(&self, painter: &mut Painter) -> io::Result<()> {
        for difference in &self.diffs {
            match difference {
                Difference::Same(lines) => {
                    for line in lines.lines() {
                        painter.set(AnsiColor::BrightWhite.to_bg())?;
                        write!(painter, " ")?;
                        painter.unset()?;
                        write!(painter, " {line}{}", self.split)?;
                    }
                }
                Difference::Add(lines) => {
                    for line in lines.lines().chain(lines.is_empty().then_some("")) {
                        painter.set(AnsiColor::Black.on(AnsiColor::Green))?;
                        write!(painter, "+")?;
                        painter.unset()?;
                        painter.set(AnsiColor::Green)?;
                        write!(painter, " {line}")?;
                        painter.unset()?;
                        write!(painter, "{}", self.split)?;
                    }
                }
                Difference::Rem(lines) => {
                    for line in lines.lines().chain(lines.is_empty().then_some("")) {
                        painter.set(AnsiColor::Black.on(AnsiColor::Red))?;
                        write!(painter, "-")?;
                        painter.unset()?;
                        painter.set(AnsiColor::Red)?;
                        write!(painter, " {line}")?;
                        painter.unset()?;
                        write!(painter, "{}", self.split)?;
                    }
                }
            }
        }

        Ok(())
    }
}

pub trait ListingExt {
    fn list(self, conjunction: Conjunction) -> String;
}

impl<I> ListingExt for I
where
    I: Iterator<Item: Clone + fmt::Display>,
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
                    result += " ";
                    result += conjunction.to_str();
                    result += " ";
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

impl Conjunction {
    const fn to_str(self) -> &'static str {
        match self {
            Self::And => "and",
            Self::Or => "or",
        }
    }
}

/// Use the singular or the plural form of the given word depending on the given amount.
///
/// # Examples
///
/// ```
/// # use utility::pluralize;
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

pub trait FormatError {
    fn format(self) -> String;
}

impl FormatError for io::Error {
    fn format(self) -> String {
        // @Task custom output
        self.to_string()
    }
}

// @Task rename to debug / display

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
#[derive(PartialEq, Eq, Clone, Copy, Hash, index_map::Index)]
pub struct ComponentIndex(u16);

// FIXME: Remove these associated functions once we've moved `ComponentIndex` back to
//        the HIR crate (or something similar). Only `DeclarationIndex` should have
//        access to the underlying numeric index.
impl ComponentIndex {
    #[cfg(feature = "test")]
    pub fn mock() -> Self {
        Self(0)
    }

    pub fn new_unchecked(index: u16) -> Self {
        Self(index)
    }

    pub fn into_inner(self) -> u16 {
        self.0
    }
}

impl fmt::Debug for ComponentIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}c", self.0)
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
        assert_eq!([false, true].into_iter().list(Conjunction::And), "false and true");
    }

    #[test]
    fn listing_three_elements() {
        assert_eq!([1, 2, 3].into_iter().list(Conjunction::Or), "1, 2 or 3");
    }

    #[test]
    fn listing_many_elements() {
        assert_eq!(
            ["a", "b", "c", "d", "e", "f", "g"].into_iter().list(Conjunction::And),
            "a, b, c, d, e, f and g"
        );
    }
}

pub fn width(str: &str) -> usize {
    let width = |char| match char {
        '\n' => 0,
        _ => unicode_width::UnicodeWidthChar::width(char).unwrap_or(1),
    };
    str.chars().map(width).sum()
}
