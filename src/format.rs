//! Formatting support functionality.

use colored::Colorize;
use difference::{Changeset, Difference};
use joinery::JoinableIterator;

use std::{fmt, io::Write};

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
    pub subject: &'a T,
    pub context: T::Context<'a>,
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

pub fn ordered_listing<I>(mut items: I, conjunction: Conjunction) -> String
where
    I: DoubleEndedIterator<Item: fmt::Display + Clone> + Clone,
{
    use std::iter::once;

    let last = items.next_back().unwrap();

    match items.next() {
        Some(item) => {
            let body = once(item).chain(items).join_with(", ");
            format!("{body} {conjunction} {last}")
        }
        None => last.to_string(),
    }
}

pub fn unordered_listing<I>(mut items: I, conjunction: Conjunction) -> String
where
    I: Iterator<Item: Clone + fmt::Display> + Clone,
{
    use std::iter::once;

    let last = items.next().unwrap();

    match items.next() {
        Some(item) => {
            let body = once(item).chain(items).join_with(", ");
            format!("{body} {conjunction} {last}")
        }
        None => last.to_string(),
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

pub macro pluralize {
    ($amount:expr, $singular:expr, $plural:expr $(,)?) => {
        match $amount {
            1 => $singular.into(),
            _ => $plural.into(),
        }: ::std::borrow::Cow<'_, str>
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
        format!("`{}`", self)
    }
}

pub macro quoted($code:expr) {
    concat!("`", $code, "`")
}

pub trait AsDebug: fmt::Display + Sized {
    fn as_debug(&self) -> DisplayIsDebug<'_, Self> {
        DisplayIsDebug(self)
    }
}

impl<T: fmt::Display> AsDebug for T {}

pub struct DisplayIsDebug<'a, T: fmt::Display>(&'a T);

impl<T: fmt::Display> fmt::Debug for DisplayIsDebug<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

pub trait AsDisplay: fmt::Debug + Sized {
    fn as_display(&self) -> DebugIsDisplay<'_, Self> {
        DebugIsDisplay(self)
    }
}

impl<T: fmt::Debug> AsDisplay for T {}

pub struct DebugIsDisplay<'a, T: fmt::Debug>(&'a T);

impl<T: fmt::Debug> fmt::Display for DebugIsDisplay<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
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

// the provided Display implementation for Changesets is problematic when whitespace differs
pub fn differences_with_ledge(differences: &[Difference]) -> String {
    let mut buffer = Vec::new();

    for difference in differences {
        match difference {
            Difference::Same(lines) => {
                for line in lines.lines() {
                    writeln!(buffer, "{} {}", " ".on_bright_white(), line).unwrap();
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
