use std::hash::Hash;
use std::marker::PhantomData;

pub trait IndexType:
    Clone + Copy + PartialEq + Eq + PartialOrd + Ord + Hash + private::Sealed
{
    fn from_bits(bits: u64) -> Self;
    fn to_bits(self) -> u64;

    fn new(index: usize) -> Self {
        Self::from_bits(index as u64)
    }

    fn to_usize(self) -> usize {
        self.to_bits() as usize
    }

    fn null() -> Self {
        Self::new(usize::MAX)
    }

    fn is_null(&self) -> bool {
        *self == Self::null()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VertexIndex(u64);

impl IndexType for VertexIndex {
    fn from_bits(bits: u64) -> Self {
        Self(bits)
    }

    fn to_bits(self) -> u64 {
        self.0
    }
}

impl Default for VertexIndex {
    fn default() -> Self {
        Self::null()
    }
}

impl From<usize> for VertexIndex {
    fn from(index: usize) -> Self {
        Self::new(index)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EdgeIndex(u64);

impl IndexType for EdgeIndex {
    fn from_bits(bits: u64) -> Self {
        Self(bits)
    }

    fn to_bits(self) -> u64 {
        self.0
    }
}

impl Default for EdgeIndex {
    fn default() -> Self {
        Self::null()
    }
}

impl From<usize> for EdgeIndex {
    fn from(index: usize) -> Self {
        Self::new(index)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Virtual<I>(u64, PhantomData<I>);

impl<I: IndexType> IndexType for Virtual<I> {
    fn from_bits(bits: u64) -> Self {
        Self(bits, PhantomData)
    }

    fn to_bits(self) -> u64 {
        self.0
    }
}

impl<I: IndexType + Default> Default for Virtual<I> {
    fn default() -> Self {
        Self::new(I::default().to_usize())
    }
}

impl<I: IndexType> From<usize> for Virtual<I> {
    fn from(index: usize) -> Self {
        Self::new(index)
    }
}

mod private {
    use super::*;

    pub trait Sealed {}

    impl Sealed for VertexIndex {}
    impl Sealed for EdgeIndex {}
    impl<I> Sealed for Virtual<I> {}
}
