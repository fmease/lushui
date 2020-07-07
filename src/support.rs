//! Helper "support" bindings for every module.
//!
//! All of those items are about error handling. They want a home.
//! `crate::diagnostic` might not be it. It's still some way to go
//! until I figure out how to handle errors best.
// @Question should we move the Result helpers to crate::diagnostic?

use crate::diagnostic::{Diagnostic, Diagnostics, Results};

// @Note the `accumulate_error` methods ignore whether an error is fatal or not, this should be changed @Task

pub mod accumulate_errors {
    use super::*;

    pub trait Accumulate2Errors<A, B> {
        fn accumulate_err(self) -> Results<(A, B)>;
    }

    pub trait Accumulate3Errors<A, B, C> {
        fn accumulate_err(self) -> Results<(A, B, C)>;
    }

    impl<A, B> Accumulate2Errors<A, B> for (Results<A>, Results<B>) {
        fn accumulate_err(self) -> Results<(A, B)> {
            match (self.0, self.1) {
                (Ok(okay0), Ok(okay1)) => Ok((okay0, okay1)),
                (Err(error), Ok(_)) | (Ok(_), Err(error)) => Err(error),
                (Err(error0), Err(error1)) => {
                    let mut error = error0;
                    error.extend(error1);
                    Err(error)
                }
            }
        }
    }

    impl<A, B, C> Accumulate3Errors<A, B, C> for (Results<A>, Results<B>, Results<C>) {
        fn accumulate_err(self) -> Results<(A, B, C)> {
            let result = (self.0, self.1).accumulate_err();
            let ((okay0, okay1), okay2) = (result, self.2).accumulate_err()?;
            Ok((okay0, okay1, okay2))
        }
    }
}

pub trait InvalidFallback {
    fn invalid() -> Self;
}

/// Try to get a value falling back to something invalid logging errors.
pub trait TrySoftly<T: InvalidFallback> {
    fn try_softly(self, bag: &mut Diagnostics) -> T;
}

impl<T: InvalidFallback> TrySoftly<T> for Results<T> {
    fn try_softly(self, bag: &mut Diagnostics) -> T {
        match self {
            Ok(okay) => okay,
            Err(errors) => {
                bag.extend(errors);
                T::invalid()
            }
        }
    }
}

impl<T: InvalidFallback> TrySoftly<T> for Result<T, Diagnostic> {
    fn try_softly(self, diagnostics: &mut Diagnostics) -> T {
        match self {
            Ok(okay) => okay,
            Err(error) => {
                diagnostics.insert(error);
                T::invalid()
            }
        }
    }
}

pub macro release($errors:expr) {{
    let errors = $errors;
    if !errors.is_empty() {
        return Err(errors);
    }
}}

pub trait TransposeExt<T> {
    fn transpose(self) -> Results<Vec<T>>;
}

impl<T> TransposeExt<T> for Vec<Results<T>> {
    fn transpose(self) -> Results<Vec<T>> {
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

pub enum Error<E> {
    Unrecoverable(Diagnostic),
    Recoverable(E),
}

use std::convert::{TryFrom, TryInto};

impl<E: TryInto<Diagnostic>> TryFrom<Error<E>> for Diagnostic {
    type Error = E::Error;

    fn try_from(error: Error<E>) -> Result<Self, Self::Error> {
        match error {
            Error::Unrecoverable(diagnostic) => Ok(diagnostic),
            Error::Recoverable(error) => error.try_into(),
        }
    }
}

impl<E> From<Diagnostic> for Error<E> {
    fn from(diagnostic: Diagnostic) -> Self {
        Self::Unrecoverable(diagnostic)
    }
}

use std::borrow::Cow;

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

use std::fmt;

pub struct DisplayIsDebug<'a, T: fmt::Display>(pub &'a T);

impl<T: fmt::Display> fmt::Debug for DisplayIsDebug<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

pub struct DebugIsDisplay<'a, T: fmt::Debug>(pub &'a T);

impl<T: fmt::Debug> fmt::Display for DebugIsDisplay<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
