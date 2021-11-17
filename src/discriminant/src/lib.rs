pub use derive::Discriminant;
pub use derive::Elements;

pub trait Discriminant {
    type Discriminant;

    // @Beacon @Task make this a const fn
    fn discriminant(&self) -> Self::Discriminant;
}

// @Task use this trait once impl_trait_in_trait is implemented
// pub trait Elements {
//     fn elements() -> impl Iterator<Item = Self>;
// }
