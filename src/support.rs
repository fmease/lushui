//! Helper "support" bindings for every module.
//!
//! All of those items are about error handling. They want a home.
//! `crate::diagnostics` might not be it. It's still some way to go
//! until I figure out how to handle errors best.
//!
//! Read documention of [crate::diagnostics] for more information on
//! the error handling APIs found in this module
// @Task rename this module in utilities and move formatting (`WithDisplay`, pluralize)
// into separate module
// @Task move error handling APIs to separate module `errors`

use joinery::JoinableIterator;

use crate::{
    diagnostics::{Diagnostic, Diagnostics, Result, Results},
    SmallVec,
};

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

/// Join results accumulating errors.
pub macro accumulate_errors {
    ($result0:expr, $result1:expr $(,)?) => { accumulate_errors2($result0, $result1) },
    ($result0:expr, $result1:expr, $result2:expr $(,)?) => { accumulate_errors3($result0, $result1, $result2) },
    ($result0:expr, $result1:expr, $result2:expr, $result3:expr, $(,)?) => { accumulate_errors4($result0, $result1, $result2, $result3) },
}

pub fn accumulate_errors2<A, B>(result0: Results<A>, result1: Results<B>) -> Results<(A, B)> {
    match (result0, result1) {
        (Ok(okay0), Ok(okay1)) => Ok((okay0, okay1)),
        (Err(error), Ok(_)) | (Ok(_), Err(error)) => Err(error),
        (Err(error0), Err(error1)) => {
            let mut error = error0;
            error.extend(error1);
            Err(error)
        }
    }
}

pub fn accumulate_errors3<A, B, C>(
    result0: Results<A>,
    result1: Results<B>,
    result2: Results<C>,
) -> Results<(A, B, C)> {
    let result = accumulate_errors2(result0, result1);
    let ((okay0, okay1), okay2) = accumulate_errors2(result, result2)?;
    Ok((okay0, okay1, okay2))
}

pub fn accumulate_errors4<A, B, C, D>(
    result0: Results<A>,
    result1: Results<B>,
    result2: Results<C>,
    result3: Results<D>,
) -> Results<(A, B, C, D)> {
    let result = accumulate_errors3(result0, result1, result2);
    let ((okay0, okay1, okay2), okay3) = accumulate_errors2(result, result3)?;
    Ok((okay0, okay1, okay2, okay3))
}

// @Beacon @Task documentation
pub trait InvalidFallback {
    fn invalid() -> Self;
}

impl<T, const N: usize> InvalidFallback for SmallVec<T, N> {
    fn invalid() -> Self {
        Self::default()
    }
}

impl<T> InvalidFallback for Vec<T> {
    fn invalid() -> Self {
        Self::default()
    }
}

impl InvalidFallback for () {
    fn invalid() -> Self {}
}

/// Try to get a value falling back to something invalid logging errors.
pub trait TryIn<T: InvalidFallback> {
    fn try_in(self, bag: &mut Diagnostics) -> T;
}

impl<T: InvalidFallback> TryIn<T> for Results<T> {
    fn try_in(self, bag: &mut Diagnostics) -> T {
        match self {
            Ok(okay) => okay,
            Err(errors) => {
                bag.extend(errors);
                T::invalid()
            }
        }
    }
}

impl<T: InvalidFallback> TryIn<T> for Result<T> {
    fn try_in(self, diagnostics: &mut Diagnostics) -> T {
        match self {
            Ok(okay) => okay,
            Err(error) => {
                diagnostics.insert(error);
                T::invalid()
            }
        }
    }
}

// @Task documentation
pub trait TransposeExt<T, E> {
    fn transpose(self) -> Result<Vec<T>, E>;
}

impl<I, T, E> TransposeExt<T, E> for I
where
    I: Iterator<Item = Result<T, E>>,
    E: Extend<Diagnostic> + IntoIterator<Item = Diagnostic>,
{
    fn transpose(self) -> Result<Vec<T>, E> {
        let mut final_result = Ok(Vec::new());
        for result in self {
            match final_result {
                Ok(ref mut okays) => match result {
                    Ok(okay) => okays.push(okay),
                    Err(errors) => final_result = Err(errors),
                },
                Err(ref mut previous_errors) => match result {
                    Ok(_) => (),
                    Err(errors) => previous_errors.extend(errors),
                },
            }
        }
        final_result
    }
}

pub trait ManyErrExt<T> {
    fn many_err(self) -> Results<T>;
}

impl<T> ManyErrExt<T> for Result<T, Diagnostic> {
    fn many_err(self) -> Results<T> {
        self.map_err(|error| Some(error).into_iter().collect())
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

use std::fmt;

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

pub macro try_or($subject:expr, $continuation:expr, buffer = $buffer:expr) {
    match $subject {
        Ok(subject) => subject,
        Err(error) => {
            $buffer.insert(error);
            $continuation
        }
    }
}

pub macro obtain($expr:expr, $( $pat:pat )|+ $( if $guard:expr )? $(,)? => $mapping:expr) {
    match $expr {
        $( $pat )|+ $( if $guard )? => Some($mapping),
        _ => None
    }
}
