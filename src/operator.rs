pub mod complement;
pub mod transpose;

pub use complement::Complement;
pub use transpose::Transpose;

pub trait OpMut<G, C = ()> {
    fn apply_mut(self, result: &mut G);
}

pub trait OpOwned<G, C = ()> {
    fn apply(self) -> G;
}
