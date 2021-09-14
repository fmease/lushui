pub use derive::Discriminant;

pub trait Discriminant {
    type Discriminant;

    // @Beacon @Task make this a const fn
    fn discriminant(&self) -> Self::Discriminant;
}
