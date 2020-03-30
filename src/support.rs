pub mod handle {
    /// "Handle" 2 results mapping okays and merging errors which are `Vec`s
    pub trait Two<E> {
        type A;
        type B;

        fn handle<O>(self, map: impl FnOnce(Self::A, Self::B) -> O) -> Result<O, Vec<E>>;
    }

    pub trait Three<E> {
        type A;
        type B;
        type C;

        fn handle<O>(self, map: impl FnOnce(Self::A, Self::B, Self::C) -> O) -> Result<O, Vec<E>>;
    }

    impl<A, B, E> Two<E> for (Result<A, Vec<E>>, Result<B, Vec<E>>) {
        type A = A;
        type B = B;

        fn handle<O>(self, map: impl FnOnce(Self::A, Self::B) -> O) -> Result<O, Vec<E>> {
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

    impl<A, B, C, E> Three<E> for (Result<A, Vec<E>>, Result<B, Vec<E>>, Result<C, Vec<E>>) {
        type A = A;
        type B = B;
        type C = C;

        fn handle<O>(self, map: impl FnOnce(Self::A, Self::B, Self::C) -> O) -> Result<O, Vec<E>> {
            (
                (self.0, self.1).handle(|okay0, okay1| (okay0, okay1)),
                self.2,
            )
                .handle(|(okay0, okay1), okay2| map(okay0, okay1, okay2))
        }
    }
}

pub trait TransposeExt<A, E> {
    fn transpose(self) -> Result<Vec<A>, Vec<E>>;
}

impl<A, E> TransposeExt<A, E> for Vec<Result<A, Vec<E>>> {
    fn transpose(self) -> Result<Vec<A>, Vec<E>> {
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
