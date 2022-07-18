//! Error handling mechanisms.
#![feature(decl_macro)]

use diagnostics::{reporter::ErasedReportedError, Diagnostic, Reporter};
use std::fmt;
use utilities::SmallVec;

pub type Result<T = (), E = ErasedReportedError> = std::result::Result<T, E>;

#[derive(Debug)]
#[must_use]
pub struct Outcome<T> {
    pub bare: T,
    pub health: Health,
}

impl<T> Outcome<T> {
    pub const fn new(bare: T, health: Health) -> Self {
        Self { bare, health }
    }

    pub const fn untainted(bare: T) -> Self {
        Self::new(bare, Health::Untainted)
    }

    pub const fn tainted(bare: T) -> Self {
        Self::new(bare, Health::Tainted)
    }

    pub fn map<U>(self, mapper: impl FnOnce(T) -> U) -> Outcome<U> {
        Outcome::new(mapper(self.bare), self.health)
    }
}

pub trait Stain<T> {
    fn stain(self, health: &mut Health) -> T;
}

impl<T> Stain<T> for Outcome<T> {
    fn stain(self, health: &mut Health) -> T {
        *health = health.and(self.health);
        self.bare
    }
}

impl<T: PossiblyErroneous> Stain<T> for Result<T> {
    fn stain(self, health: &mut Health) -> T {
        match self {
            Ok(value) => value,
            Err(_) => {
                health.taint();
                T::error()
            }
        }
    }
}

pub trait OkIfUntaintedExt<T> {
    fn ok_if_untainted(value: T, health: Health) -> Self;
}

impl<T> OkIfUntaintedExt<T> for Result<T> {
    fn ok_if_untainted(value: T, health: Health) -> Self {
        match health {
            Health::Untainted => Ok(value),
            // @Beacon @Task don't use this unchecked call,
            // use the ErasedReportedError inside of Tainted (once available)
            Health::Tainted => Err(ErasedReportedError::new_unchecked()),
        }
    }
}

pub macro Outcome($bare:pat, $health:pat) {
    Outcome {
        bare: $bare,
        health: $health,
    }
}

impl<T: PossiblyErroneous> From<Option<T>> for Outcome<T> {
    fn from(option: Option<T>) -> Self {
        match option {
            Some(value) => Outcome::untainted(value),
            None => Outcome::tainted(T::error()),
        }
    }
}

impl<T: PossiblyErroneous> From<Result<T>> for Outcome<T> {
    fn from(option: Result<T>) -> Self {
        match option {
            Ok(value) => Outcome::untainted(value),
            Err(_error) => Outcome::tainted(T::error()),
        }
    }
}

impl<T> From<Outcome<T>> for Result<T> {
    fn from(outcome: Outcome<T>) -> Self {
        match outcome.health {
            Health::Untainted => Ok(outcome.bare),
            // @Beacon @Task don't use this unchecked call,
            // use the ErasedReportedError inside of Tainted (once available)
            Health::Tainted => Err(ErasedReportedError::new_unchecked()),
        }
    }
}

impl<T: PossiblyErroneous> PossiblyErroneous for Outcome<T> {
    fn error() -> Self {
        Self::tainted(T::error())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
#[must_use]
pub enum Health {
    #[default]
    Untainted,
    /// Marks non-fatal failures.
    // @Beacon @Task make this take an ErasedReportedError enabling us to
    // remove the unchecked creation calls in the combinators below
    Tainted,
}

impl Health {
    pub const fn is_tainted(self) -> bool {
        matches!(self, Self::Tainted)
    }

    pub const fn is_untainted(self) -> bool {
        !self.is_tainted()
    }

    fn and(self, other: Self) -> Self {
        match (self, other) {
            (Self::Untainted, Self::Untainted) => Self::Untainted,
            (_, _) => Self::Tainted,
        }
    }

    pub fn taint(&mut self) {
        if *self == Self::Untainted {
            *self = Self::Tainted;
        }
    }
}

impl From<Result> for Health {
    fn from(result: Result) -> Self {
        match result {
            Ok(()) => Self::Untainted,
            Err(_) => Self::Tainted,
        }
    }
}

impl From<Health> for Result {
    fn from(health: Health) -> Self {
        match health {
            Health::Untainted => Ok(()),
            // @Beacon @Task don't use this unchecked call,
            // use the ErasedReportedError inside of Tainted (once available)
            Health::Tainted => Err(ErasedReportedError::new_unchecked()),
        }
    }
}

impl fmt::Display for Health {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Untainted => write!(f, "untainted"),
            Self::Tainted => write!(f, "tainted"),
        }
    }
}

pub trait PossiblyErroneous {
    fn error() -> Self;
}

impl<T, const N: usize> PossiblyErroneous for SmallVec<T, N> {
    fn error() -> Self {
        Self::default()
    }
}

impl<T> PossiblyErroneous for Vec<T> {
    fn error() -> Self {
        Self::default()
    }
}

// @Note very weird impl...it does not really corresp. to our
// notion of possibly errorneous but still this impl is very useful
// as it allows us to call try_in on functions that merely check (Result<(), Error>)
// @Beacon @Question can we replace () here with ErasedReportedError?
impl PossiblyErroneous for () {
    fn error() -> Self {}
}

pub trait ReportedExt {
    type Output;

    fn reported(self, reporter: &Reporter) -> Self::Output;
}

impl<T, E> ReportedExt for Result<T, E>
where
    Diagnostic: From<E>,
{
    type Output = Result<T>;

    fn reported(self, reporter: &Reporter) -> Self::Output {
        self.map_err(|error| Diagnostic::from(error).report(reporter))
    }
}

// This can theoretically be generalized using GATs to support the implementors
// R<O<T>, E> and O<R<T, E>>. However, rustc's type inference engine sucks and
// cannot handle the use sites in the simplest of cases 😭.
pub trait AndThenMapExt<T> {
    fn and_then_map<U, F: FnOnce(T) -> Result<U>>(self, mapper: F) -> Result<Option<U>>;
}

impl<T> AndThenMapExt<T> for Result<Option<T>> {
    fn and_then_map<U, F: FnOnce(T) -> Result<U>>(self, mapper: F) -> Result<Option<U>> {
        self.and_then(|value| value.map(mapper).transpose())
    }
}