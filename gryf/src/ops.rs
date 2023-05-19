pub mod complement;
pub mod transpose;
pub mod undirect;

pub use complement::Complement;
pub use transpose::Transpose;
pub use undirect::Undirect;

pub trait OpMut<G, C = ()> {
    fn apply_mut(self, result: &mut G);
}

pub trait OpOwned<G, C = ()> {
    fn apply(self) -> G;
}
