//! Formatting support functionality.
//!
// @Task rename this module in utilities and move formatting (`WithDisplay`, pluralize)
// into separate module
// @Task move error handling APIs to separate module `errors`

use joinery::JoinableIterator;

use std::fmt;

pub trait DisplayWith: Sized {
    type Linchpin;

    fn format(&self, linchpin: &Self::Linchpin, f: &mut fmt::Formatter<'_>) -> fmt::Result;

    fn with<'a>(&'a self, linchpin: &'a Self::Linchpin) -> WithLinchpin<'a, Self> {
        WithLinchpin {
            subject: self,
            linchpin,
        }
    }
}

pub struct WithLinchpin<'a, T: DisplayWith> {
    pub subject: &'a T,
    pub linchpin: &'a T::Linchpin,
}

impl<T: DisplayWith> fmt::Display for WithLinchpin<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.subject.format(self.linchpin, f)
    }
}

impl<T: DisplayWith> fmt::Debug for WithLinchpin<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

pub fn ordered_listing<I>(items: I, conjunction: Conjunction) -> String
where
    I: DoubleEndedIterator<Item: fmt::Display + Clone> + Clone,
{
    use std::iter::once;

    let mut items = items.rev();
    let last = items.next().unwrap();
    let mut items = items.rev();

    match items.next() {
        Some(item) => {
            let body = once(item).chain(items).join_with(", ");
            let conjunction = conjunction.name();
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
            let conjunction = conjunction.name();
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

impl Conjunction {
    const fn name(self) -> &'static str {
        match self {
            Self::And => "and",
            Self::Or => "or",
        }
    }
}

use std::borrow::Cow;

pub macro s_pluralize($amount:expr, $singular:literal) {
    match $amount {
        1 => $singular,
        _ => concat!($singular, "s"),
    }
}

pub fn pluralize<'a, S: Into<Cow<'a, str>>>(
    amount: usize,
    singular: &'a str,
    plural: impl FnOnce() -> S,
) -> Cow<'a, str> {
    match amount {
        1 => singular.into(),
        _ => plural().into(),
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
