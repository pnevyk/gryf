use std::hash::Hash;
use std::marker::PhantomData;

pub trait IndexType:
    Clone + Copy + PartialEq + Eq + PartialOrd + Ord + Hash + private::Sealed
{
    fn new(index: usize) -> Self;
    fn to_usize(self) -> usize;

    fn null() -> Self {
        Self::new(usize::MAX)
    }

    fn is_null(&self) -> bool {
        *self == Self::null()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VertexIndex(usize);

impl IndexType for VertexIndex {
    fn new(index: usize) -> Self {
        Self(index)
    }

    fn to_usize(self) -> usize {
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
pub struct EdgeIndex(usize);

impl IndexType for EdgeIndex {
    fn new(index: usize) -> Self {
        Self(index)
    }

    fn to_usize(self) -> usize {
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
pub struct Virtual<I>(usize, PhantomData<I>);

impl<I: IndexType> IndexType for Virtual<I> {
    fn new(index: usize) -> Self {
        Self(index, PhantomData)
    }

    fn to_usize(self) -> usize {
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
