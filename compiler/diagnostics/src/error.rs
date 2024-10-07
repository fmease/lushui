//! Error handling mechanisms.

use crate::{Diagnostic, reporter::ErasedReportedError};
use utility::default;

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

    pub const fn tainted(bare: T, error: ErasedReportedError) -> Self {
        Self::new(bare, Health::Tainted(error))
    }
}

// @Task get rid of this
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
            Err(error) => {
                health.taint(error);
                T::error(error)
            }
        }
    }
}

impl Stain<()> for Result {
    fn stain(self, health: &mut Health) {
        match self {
            Ok(value) => value,
            Err(error) => health.taint(error),
        }
    }
}

impl<T: PossiblyErroneous> From<Result<T>> for Outcome<T> {
    fn from(option: Result<T>) -> Self {
        match option {
            Ok(value) => Outcome::untainted(value),
            Err(error) => Outcome::tainted(T::error(error), error),
        }
    }
}

impl<T> From<Outcome<T>> for Result<T> {
    fn from(outcome: Outcome<T>) -> Self {
        match outcome.health {
            Health::Untainted => Ok(outcome.bare),
            Health::Tainted(error) => Err(error),
        }
    }
}

impl<T: PossiblyErroneous> PossiblyErroneous for Outcome<T> {
    fn error(error: ErasedReportedError) -> Self {
        Self::tainted(T::error(error), error)
    }
}

// @Task replace this type with existing `Result` (`Result<(), ErasedReportedError>`)
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
#[must_use]
pub enum Health {
    #[default]
    Untainted,
    Tainted(ErasedReportedError),
}

impl Health {
    fn and(self, other: Self) -> Self {
        match (self, other) {
            (Self::Untainted, Self::Untainted) => Self::Untainted,
            (Self::Tainted(error), _) | (_, Self::Tainted(error)) => Self::Tainted(error),
        }
    }

    pub fn taint(&mut self, error: ErasedReportedError) {
        if *self == Self::Untainted {
            *self = Self::Tainted(error);
        }
    }
}

impl From<Result> for Health {
    fn from(result: Result) -> Self {
        match result {
            Ok(()) => Self::Untainted,
            Err(error) => Self::Tainted(error),
        }
    }
}

impl From<Health> for Result {
    fn from(health: Health) -> Self {
        match health {
            Health::Untainted => Ok(()),
            Health::Tainted(error) => Err(error),
        }
    }
}

pub trait PossiblyErroneous {
    fn error(error: ErasedReportedError) -> Self;
}

impl PossiblyErroneous for ErasedReportedError {
    fn error(error: ErasedReportedError) -> Self {
        error
    }
}

impl<Bare: PossiblyErroneous, Attributes: Default> PossiblyErroneous
    for span::item::Item<Bare, Attributes>
{
    fn error(error: ErasedReportedError) -> Self {
        Self { bare: Bare::error(error), span: default(), attributes: default() }
    }
}

/// An error handler.
pub trait Handler: Sized {
    #[must_use]
    fn embed<T: PossiblyErroneous>(self, diagnostic: Diagnostic) -> T;
}
