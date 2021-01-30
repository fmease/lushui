//! Different error handling mechanisms.
//! Read documention of [crate::diagnostics] for more information on
//! the error handling APIs found in this module
// @Task move documentation to here

use crate::{
    diagnostics::{Diagnostic, Diagnostics, Result, Results},
    SmallVec,
};

/// Join results accumulating errors.
// @Note I don't like this accumulation API at all, it is cumbersome to define and cumbersome
// to use! @Task Try to replace it!
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

/// Try to get a value falling back to something invalid logging errors.
pub trait TryIn<T: PossiblyErroneous> {
    fn try_in(self, bag: &mut Diagnostics) -> T;
}

impl<T: PossiblyErroneous> TryIn<T> for Results<T> {
    fn try_in(self, bag: &mut Diagnostics) -> T {
        match self {
            Ok(okay) => okay,
            Err(errors) => {
                bag.extend(errors);
                T::error()
            }
        }
    }
}

impl<T: PossiblyErroneous> TryIn<T> for Result<T> {
    fn try_in(self, diagnostics: &mut Diagnostics) -> T {
        match self {
            Ok(okay) => okay,
            Err(error) => {
                diagnostics.insert(error);
                T::error()
            }
        }
    }
}

// @Task documentation
// @Task rename this to something more descriptive relating to the concept of errors for one!
// @Note this can go once we commit to mutable error buffers and throw away
// the accumulation API
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

// @Note this can go once we commit to mutable error buffers and throw away
// the accumulation API
pub trait ManyErrExt<T> {
    fn many_err(self) -> Results<T>;
}

impl<T> ManyErrExt<T> for Result<T, Diagnostic> {
    fn many_err(self) -> Results<T> {
        self.map_err(|error| Some(error).into_iter().collect())
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

// @Note does not fit this module
pub macro obtain($expr:expr, $( $pat:pat )|+ $( if $guard:expr )? $(,)? => $mapping:expr) {
    match $expr {
        $( $pat )|+ $( if $guard )? => Some($mapping),
        _ => None
    }
}
