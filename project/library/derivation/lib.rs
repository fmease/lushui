pub use macros::{Discriminant, Elements, FromStr, Str};

pub trait Elements: Sized {
    type Iter: Iterator<Item = Self>;

    // @Task use "impl in return position" once in nightly
    fn elements() -> Self::Iter;
}
