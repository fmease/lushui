//! Helper "support" bindings for every module.
//!
//! All of those items are about error handling. They want a home.
//! `crate::diagnostic` might not be it. It's still some way to go
//! until I figure out how to handle errors best.
// @Question should we move the Result helpers to crate::diagnostic?

use crate::diagnostic::{Diagnostic, Diagnostics};

type Result<T> = std::result::Result<T, Diagnostics>;

// @Task better names
pub mod handle {
    use super::*;

    /// "Handle" 2 results mapping okays and merging errors which are `Vec`s
    pub trait HandleTwo<A, B> {
        fn handle<O>(self, map: impl FnOnce(A, B) -> O) -> Result<O>;
    }

    pub trait HandleThree<A, B, C> {
        fn handle<O>(self, map: impl FnOnce(A, B, C) -> O) -> Result<O>;
    }

    impl<A, B> HandleTwo<A, B> for (Result<A>, Result<B>) {
        fn handle<O>(self, map: impl FnOnce(A, B) -> O) -> Result<O> {
            match (self.0, self.1) {
                (Ok(okay0), Ok(okay1)) => Ok(map(okay0, okay1)),
                (Err(error), Ok(_)) | (Ok(_), Err(error)) => Err(error),
                (Err(error0), Err(mut error1)) => {
                    let mut error = error0;
                    error.append(&mut error1);
                    Err(error)
                }
            }
        }
    }

    impl<A, B, C> HandleThree<A, B, C> for (Result<A>, Result<B>, Result<C>) {
        fn handle<O>(self, map: impl FnOnce(A, B, C) -> O) -> Result<O> {
            (
                (self.0, self.1).handle(|okay0, okay1| (okay0, okay1)),
                self.2,
            )
                .handle(|(okay0, okay1), okay2| map(okay0, okay1, okay2))
        }
    }
}

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
                    Err(mut errors) => previous_errors.append(&mut errors),
                },
            }
        }
        final_result
    }
}

pub trait ManyErrExt<T> {
    fn many_err(self) -> Result<T>;
}

impl<T> ManyErrExt<T> for std::result::Result<T, Diagnostic> {
    fn many_err(self) -> Result<T> {
        self.map_err(|error| vec![error])
    }
}
