//! Utility functionality and definitions.

use colored::Colorize;
use difference::{Changeset, Difference};
use joinery::JoinableIterator;
pub(crate) use num_bigint::{BigInt as Int, BigUint as Nat};
pub(crate) use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::{ffi::OsStr, fmt, path::Path};
pub(crate) use string_cache::DefaultAtom as Atom;

pub(crate) mod lexer;

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

pub(crate) macro condition($( $condition:expr => $consequence:expr ),+ $(, else => $alternative:expr )? $(,)?) {
    match () {
        $( _ if $condition => $consequence, )+
        $( _ => $alternative )?
    }
}

#[allow(unused_macros)]
pub(crate) macro no_std_assert($( $anything:tt )*) {
    compile_error!("use the function `assert_eq` instead of macro `assert_eq` and similar")
}

#[cfg(test)]
pub(crate) fn difference(original: &str, edit: &str, split: &str) -> String {
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

pub trait DisplayWith: Sized {
    type Context<'a>: Copy;

    fn format(&self, context: Self::Context<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result;

    fn with<'a>(&'a self, context: Self::Context<'a>) -> WithContext<'a, Self> {
        WithContext {
            subject: self,
            context,
        }
    }
}

pub struct WithContext<'a, T: DisplayWith> {
    subject: &'a T,
    context: T::Context<'a>,
}

impl<T: DisplayWith> fmt::Display for WithContext<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.subject.format(self.context, f)
    }
}

impl<T: DisplayWith> fmt::Debug for WithContext<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

pub trait UnorderedListingExt {
    fn list(self, conjunction: Conjunction) -> String;
}

impl<I> UnorderedListingExt for I
where
    I: Iterator<Item: Clone + fmt::Display> + Clone,
{
    fn list(mut self, conjunction: Conjunction) -> String {
        use std::iter::once;

        let last = self.next().unwrap();

        match self.next() {
            Some(item) => {
                let body = once(item).chain(self).join_with(", ");
                format!("{body} {conjunction} {last}")
            }
            None => last.to_string(),
        }
    }
}

pub(crate) trait OrderedListingExt {
    fn list_in_order(self, conjunction: Conjunction) -> String;
}

impl<I> OrderedListingExt for I
where
    I: DoubleEndedIterator<Item: fmt::Display + Clone> + Clone,
{
    fn list_in_order(mut self, conjunction: Conjunction) -> String {
        use std::iter::once;

        let last = self.next_back().unwrap();

        match self.next() {
            Some(item) => {
                let body = once(item).chain(self).join_with(", ");
                format!("{body} {conjunction} {last}")
            }
            None => last.to_string(),
        }
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
/// # use lushui::utility::pluralize;
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
        // @Task optimize
        format!("`{self}`")
    }
}

pub(crate) macro quoted($code:expr) {
    concat!("`", $code, "`")
}

pub(crate) trait AsDebug: fmt::Display + Sized {
    fn as_debug(&self) -> DisplayIsDebug<'_, Self> {
        DisplayIsDebug(self)
    }
}

impl<T: fmt::Display> AsDebug for T {}

pub(crate) struct DisplayIsDebug<'a, T: fmt::Display>(&'a T);

impl<T: fmt::Display> fmt::Debug for DisplayIsDebug<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

pub(crate) trait AsDisplay: fmt::Debug + Sized {
    fn as_display(&self) -> DebugIsDisplay<'_, Self> {
        DebugIsDisplay(self)
    }
}

impl<T: fmt::Debug> AsDisplay for T {}

pub(crate) struct DebugIsDisplay<'a, T: fmt::Debug>(&'a T);

impl<T: fmt::Debug> fmt::Display for DebugIsDisplay<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

// @Task replace this whole business with a `write_*` function
pub(crate) struct AutoColoredChangeset<'a>(pub(crate) &'a Changeset);

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

pub(crate) trait AsAutoColoredChangeset {
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
        write!(f, "`{}`: {}", self.1.to_string_lossy(), self.0)
    }
}
