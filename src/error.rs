//! Different error handling mechanisms.

use std::fmt;

use crate::{
    diagnostics::{reporter::ErrorReported, Diagnostic, Reporter},
    utility::SmallVec,
};

pub type Result<T = (), E = ErrorReported> = std::result::Result<T, E>;

#[derive(Debug)]
#[must_use]
pub struct Outcome<T> {
    pub value: T,
    pub health: Health,
}

impl<T> Outcome<T> {
    pub const fn new(value: T, health: Health) -> Self {
        Self { value, health }
    }

    pub const fn untainted(value: T) -> Self {
        Self::new(value, Health::Untainted)
    }

    pub const fn tainted(value: T) -> Self {
        Self::new(value, Health::Tainted)
    }

    pub fn map<U>(self, mapper: impl FnOnce(T) -> U) -> Outcome<U> {
        Outcome::new(mapper(self.value), self.health)
    }
}

pub(crate) trait Stain<T> {
    fn stain(self, health: &mut Health) -> T;
}

impl<T> Stain<T> for Outcome<T> {
    fn stain(self, health: &mut Health) -> T {
        *health = health.and(self.health);
        self.value
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

pub(crate) trait OkIfUntaintedExt<T> {
    fn ok_if_untainted(value: T, health: Health) -> Self;
}

impl<T> OkIfUntaintedExt<T> for Result<T> {
    fn ok_if_untainted(value: T, health: Health) -> Self {
        match health {
            Health::Untainted => Ok(value),
            // @Beacon @Beacon @Beacon @Task don't use this unchecked call, use the ErrorReported inside of Tainted (once available)
            Health::Tainted => Err(ErrorReported::error_will_be_reported_unchecked()),
        }
    }
}

pub macro outcome($value:pat, $health:pat) {
    $crate::error::Outcome {
        value: $value,
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
            Err(_token) => Outcome::tainted(T::error()),
        }
    }
}

impl<T> From<Outcome<T>> for Result<T> {
    fn from(outcome: Outcome<T>) -> Self {
        match outcome.health {
            Health::Untainted => Ok(outcome.value),
            // @Beacon @Beacon @Beacon @Task don't use this unchecked call, use the ErrorReported inside of Tainted (once available)
            Health::Tainted => Err(ErrorReported::error_will_be_reported_unchecked()),
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
    // @Beacon @Beacon @Beacon @Task make this take an ErrorReported enabling us to
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

    pub(crate) fn taint(&mut self) {
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
            // @Beacon @Beacon @Beacon @Task don't use this unchecked call, use the ErrorReported inside of Tainted (once available)
            Health::Tainted => Err(ErrorReported::error_will_be_reported_unchecked()),
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

// @Task make this pub(crate) once the type-leak rules are relaxed
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
// @Beacon @Beacon @Beacon @Question can we replace () here with ErrorReported?
impl PossiblyErroneous for () {
    fn error() -> Self {}
}

pub(crate) trait ReportedExt {
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
