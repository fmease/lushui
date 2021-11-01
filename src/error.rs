//! Different error handling mechanisms.

use std::fmt;

use crate::{
    diagnostics::{Diagnostic, Reporter},
    utility::SmallVec,
};

pub type Result<T = (), E = ()> = std::result::Result<T, E>;

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

    // @Question good API?
    #[must_use = "use `&=` if you want to ignore the result"]
    pub fn unwrap(self, health: &mut Health) -> T {
        *health &= self.health;
        self.value
    }

    pub fn map<U>(self, mapper: impl FnOnce(T) -> U) -> Outcome<U> {
        Outcome::new(mapper(self.value), self.health)
    }
}

pub macro outcome($value:pat, $health:pat) {
    crate::error::Outcome {
        value: $value,
        health: $health,
    }
}

// @Temporary API
pub fn map_outcome_from_result<T, U: PossiblyErroneous>(
    result: Result<T>,
    mapper: impl FnOnce(T) -> U,
) -> Outcome<U> {
    match result {
        Ok(value) => Outcome::untainted(mapper(value)),
        Err(()) => Outcome::tainted(U::error()),
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
            Err(()) => Outcome::tainted(T::error()),
        }
    }
}

impl<T> From<Outcome<T>> for Result<T> {
    fn from(outcome: Outcome<T>) -> Self {
        match outcome.health {
            Health::Untainted => Ok(outcome.value),
            Health::Tainted => Err(()),
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
    Tainted,
}

impl Health {
    pub const fn of<T>(self, value: T) -> Outcome<T> {
        Outcome::new(value, self)
    }

    pub const fn is_tainted(self) -> bool {
        matches!(self, Self::Tainted)
    }

    pub const fn is_untainted(self) -> bool {
        !self.is_tainted()
    }

    pub fn taint(&mut self) {
        if *self == Self::Untainted {
            *self = Self::Tainted;
        }
    }
}

impl<H: Into<Health>> std::ops::BitAnd<H> for Health {
    type Output = Self;

    fn bitand(self, other: H) -> Self::Output {
        match (self, other.into()) {
            (Self::Untainted, Self::Untainted) => Self::Untainted,
            (_, _) => Self::Tainted,
        }
    }
}

impl<H: Into<Health>> std::ops::BitAndAssign<H> for Health {
    fn bitand_assign(&mut self, other: H) {
        *self = *self & other;
    }
}

impl From<Result> for Health {
    fn from(result: Result) -> Self {
        match result {
            Ok(()) => Self::Untainted,
            Err(()) => Self::Tainted,
        }
    }
}

impl From<Health> for Result {
    fn from(health: Health) -> Self {
        match health {
            Health::Untainted => Ok(()),
            Health::Tainted => Err(()),
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

// @Beacon @Task documentation
// @Task move (maybe)
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
