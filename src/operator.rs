pub mod complement;

pub use complement::Complement;

pub trait OpMut<G> {
    fn apply_mut(self, result: &mut G);
}

pub trait OpOwned<G> {
    fn apply(self) -> G;
}
