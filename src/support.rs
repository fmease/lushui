//! Helper "support" bindings for every module.
//!
//! All of those items are about error handling. They want a home.
//! `crate::diagnostic` might not be it. It's still some way to go
//! until I figure out how to handle errors best.
// @Question should we move the Result helpers to crate::diagnostic?

use crate::diagnostic::{Diagnostic, Diagnostics};

type Result<T, E = Diagnostics> = std::result::Result<T, E>;

// @Note the `accumulate_error` methods ignore whether an error is fatal or not, this should be changed @Task

pub mod accumulate_errors {
    use super::*;

    pub trait Accumulate2Errors<A, B> {
        fn accumulate_err(self) -> Result<(A, B)>;
    }

    pub trait Accumulate3Errors<A, B, C> {
        fn accumulate_err(self) -> Result<(A, B, C)>;
    }

    impl<A, B> Accumulate2Errors<A, B> for (Result<A>, Result<B>) {
        fn accumulate_err(self) -> Result<(A, B)> {
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

    impl<A, B, C> Accumulate3Errors<A, B, C> for (Result<A>, Result<B>, Result<C>) {
        fn accumulate_err(self) -> Result<(A, B, C)> {
            let result = (self.0, self.1).accumulate_err();
            let ((okay0, okay1), okay2) = (result, self.2).accumulate_err()?;
            Ok((okay0, okay1, okay2))
        }
    }
}

pub trait MayBeInvalid {
    fn invalid() -> Self;
}

pub trait TryNonFatallyExt<T: MayBeInvalid> {
    fn try_non_fatally(self, bag: &mut Diagnostics) -> T;
}

impl<T: MayBeInvalid> TryNonFatallyExt<T> for Result<T> {
    fn try_non_fatally(self, bag: &mut Diagnostics) -> T {
        match self {
            Ok(okay) => okay,
            Err(errors) => {
                bag.extend(errors);
                T::invalid()
            }
        }
    }
}

impl<T: MayBeInvalid> TryNonFatallyExt<T> for Result<T, Diagnostic> {
    fn try_non_fatally(self, bag: &mut Diagnostics) -> T {
        match self {
            Ok(okay) => okay,
            Err(error) => {
                bag.insert(error);
                T::invalid()
            }
        }
    }
}

pub macro release_errors($errors:expr) {{
    let errors = $errors;
    if !errors.is_empty() {
        return Err(errors);
    }
}}

pub trait TransposeExt<T> {
    fn transpose(self) -> Result<Vec<T>>;
}

impl<T> TransposeExt<T> for Vec<Result<T>> {
    fn transpose(self) -> Result<Vec<T>> {
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
    fn many_err(self) -> Result<T>;
}

impl<T> ManyErrExt<T> for Result<T, Diagnostic> {
    fn many_err(self) -> Result<T> {
        self.map_err(|error| Some(error).into_iter().collect())
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
