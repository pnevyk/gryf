use std::{hash::Hash, marker::PhantomData};

pub trait IdType: Clone + Ord + Hash + core::fmt::Debug {}

pub trait NumIdType: IdType + Copy + From<usize> + Into<usize> {
    fn to_bits(self) -> u64;
    fn from_bits(bits: u64) -> Self;
    fn null() -> Self;

    fn to_usize(self) -> usize {
        self.into()
    }

    fn from_usize(id: usize) -> Self {
        Self::from(id)
    }

    fn is_null(&self) -> bool {
        *self == Self::null()
    }
}

// For edge ids that are represented as a pair of vertex ids.
impl<T: IdType, U: IdType> IdType for (T, U) {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VertexId<Id = u64>(pub Id);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EdgeId<Id = u64>(pub Id);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Virtual<I>(u64, PhantomData<I>);

impl<I> Virtual<I> {
    pub fn new(id: u64) -> Self {
        Self(id, PhantomData)
    }
}

impl<I: IdType> IdType for Virtual<I> {}

impl<I: NumIdType> From<usize> for Virtual<I> {
    fn from(id: usize) -> Self {
        Self::new(id.try_into().expect("id type overflow"))
    }
}

impl<I: NumIdType> From<Virtual<I>> for usize {
    fn from(id: Virtual<I>) -> Self {
        id.0.try_into().expect("id type overflow")
    }
}

impl<I: NumIdType> From<u64> for Virtual<I> {
    fn from(id: u64) -> Self {
        Self::new(id)
    }
}

impl<I: NumIdType> From<Virtual<I>> for u64 {
    fn from(id: Virtual<I>) -> Self {
        id.0
    }
}

impl<I: NumIdType> NumIdType for Virtual<I> {
    fn to_bits(self) -> u64 {
        self.0
    }

    fn from_bits(bits: u64) -> Self {
        Self::new(bits)
    }

    fn null() -> Self {
        Self::new(u64::MAX)
    }
}

pub trait GraphIdTypes {
    type VertexId: IdType;
    type EdgeId: IdType;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DefaultId {}

impl GraphIdTypes for DefaultId {
    type VertexId = VertexId;
    type EdgeId = EdgeId;
}

pub struct CustomId<VIdd, EIdd> {
    ty: PhantomData<fn() -> (VIdd, EIdd)>,
}

impl<VIdd: IdType, EIdd: IdType> GraphIdTypes for CustomId<VIdd, EIdd> {
    type VertexId = VIdd;
    type EdgeId = EIdd;
}

pub(crate) trait UseId<Id: GraphIdTypes> {
    type Id: IdType;
}

pub(crate) enum UseVertexId {}

impl<Id: GraphIdTypes> UseId<Id> for UseVertexId {
    type Id = Id::VertexId;
}

#[allow(unused)]
pub(crate) enum UseEdgeId {}

impl<Id: GraphIdTypes> UseId<Id> for UseEdgeId {
    type Id = Id::EdgeId;
}

macro_rules! impl_num_id {
    ($id_ty:ident, $int_ty:ty) => {
        impl IdType for $id_ty<$int_ty> {}

        impl From<usize> for $id_ty<$int_ty> {
            fn from(id: usize) -> Self {
                Self(id.try_into().expect("id type overflow"))
            }
        }

        impl From<$id_ty<$int_ty>> for usize {
            fn from(id: $id_ty<$int_ty>) -> Self {
                id.0.try_into().expect("id type overflow")
            }
        }

        impl NumIdType for $id_ty<$int_ty> {
            fn to_bits(self) -> u64 {
                self.0 as u64
            }

            fn from_bits(bits: u64) -> Self {
                Self(bits as $int_ty)
            }

            fn null() -> Self {
                Self(<$int_ty>::MAX)
            }
        }
    };
}

impl_num_id!(VertexId, usize);
impl_num_id!(VertexId, u64);
impl_num_id!(VertexId, u32);
impl_num_id!(VertexId, u16);
impl_num_id!(VertexId, u8);

impl_num_id!(EdgeId, usize);
impl_num_id!(EdgeId, u64);
impl_num_id!(EdgeId, u32);
impl_num_id!(EdgeId, u16);
impl_num_id!(EdgeId, u8);

impl IdType for () {}
