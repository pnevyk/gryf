use std::{hash::Hash, marker::PhantomData};

pub trait IndexType: Clone + PartialEq + PartialOrd + Ord + Hash {}

pub trait NumIndexType: IndexType + Copy + From<u64> + Into<u64> {
    fn to_bits(self) -> u64 {
        self.into()
    }

    fn from_bits(bits: u64) -> Self {
        bits.into()
    }

    fn as_usize(self) -> usize {
        self.to_bits().try_into().expect("index type overflow")
    }

    fn from_usize(index: usize) -> Self {
        (index as u64).into()
    }

    fn null() -> Self {
        u64::MAX.into()
    }

    fn is_null(&self) -> bool {
        *self == Self::null()
    }
}

// For edge indices that are represented as a pair of vertex indices.
impl<T: IndexType, U: IndexType> IndexType for (T, U) {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VertexIndex<Ix = u64>(pub Ix);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EdgeIndex<Ix = u64>(pub Ix);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Virtual<I>(u64, PhantomData<I>);

impl<I> Virtual<I> {
    pub fn new(index: u64) -> Self {
        Self(index, PhantomData)
    }
}

impl<I: IndexType> IndexType for Virtual<I> {}

impl<I: NumIndexType> From<usize> for Virtual<I> {
    fn from(index: usize) -> Self {
        Self::new(index.try_into().expect("index type overflow"))
    }
}

impl<I: NumIndexType> From<Virtual<I>> for usize {
    fn from(index: Virtual<I>) -> Self {
        index.0.try_into().expect("index type overflow")
    }
}

impl<I: NumIndexType> From<u64> for Virtual<I> {
    fn from(index: u64) -> Self {
        Self::new(index)
    }
}

impl<I: NumIndexType> From<Virtual<I>> for u64 {
    fn from(index: Virtual<I>) -> Self {
        index.0
    }
}

impl<I: NumIndexType> NumIndexType for Virtual<I> {}

pub trait Indexing {
    type VertexIndex: IndexType;
    type EdgeIndex: IndexType;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DefaultIndexing {}

impl Indexing for DefaultIndexing {
    type VertexIndex = VertexIndex;
    type EdgeIndex = EdgeIndex;
}

pub struct CustomIndexing<VIx, EIx> {
    ty: PhantomData<fn() -> (VIx, EIx)>,
}

impl<VIx: IndexType, EIx: IndexType> Indexing for CustomIndexing<VIx, EIx> {
    type VertexIndex = VIx;
    type EdgeIndex = EIx;
}

pub trait UseIndex<Ix: Indexing> {
    type Index: IndexType;
}

pub enum UseVertexIndex {}

impl<Ix: Indexing> UseIndex<Ix> for UseVertexIndex {
    type Index = Ix::VertexIndex;
}

pub enum UseEdgeIndex {}

impl<Ix: Indexing> UseIndex<Ix> for UseEdgeIndex {
    type Index = Ix::EdgeIndex;
}

mod imp {
    use super::*;

    macro_rules! impl_num_index {
        ($index_ty:ident, $int_ty:ty) => {
            impl IndexType for $index_ty<$int_ty> {}

            impl From<usize> for $index_ty<$int_ty> {
                fn from(index: usize) -> Self {
                    Self(index.try_into().expect("index type overflow"))
                }
            }

            impl From<$index_ty<$int_ty>> for usize {
                fn from(index: $index_ty<$int_ty>) -> Self {
                    index.0.try_into().expect("index type overflow")
                }
            }

            impl From<u64> for $index_ty<$int_ty> {
                fn from(index: u64) -> Self {
                    Self(index.try_into().expect("index type overflow"))
                }
            }

            impl From<$index_ty<$int_ty>> for u64 {
                fn from(index: $index_ty<$int_ty>) -> Self {
                    index.0.try_into().expect("index type overflow")
                }
            }

            impl NumIndexType for $index_ty<$int_ty> {}
        };
    }

    impl_num_index!(VertexIndex, usize);
    impl_num_index!(VertexIndex, u64);
    impl_num_index!(VertexIndex, u32);
    impl_num_index!(VertexIndex, u16);
    impl_num_index!(VertexIndex, u8);

    impl_num_index!(EdgeIndex, usize);
    impl_num_index!(EdgeIndex, u64);
    impl_num_index!(EdgeIndex, u32);
    impl_num_index!(EdgeIndex, u16);
    impl_num_index!(EdgeIndex, u8);

    impl IndexType for () {}
}
