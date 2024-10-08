mod compact_id_map;

pub use compact_id_map::CompactIdMap;

use std::{fmt::Debug, hash::Hash, marker::PhantomData};

use super::borrow::OwnableRef;

/// A unique identification of a vertex or edge in a graph.
///
/// In standard graph representations, the id type is an integer. Conceptually,
/// such an integer id is of type `usize`, but one can choose a smaller integer
/// type (such as u8 or u16) to lower the memory footprint. In these cases, the
/// algorithms can treat the id as usize with all the benefits (e.g., indexing
/// to a contiguous array).
///
/// For implicit graphs, an id can be of any form as long as it implements
/// required interface and super traits. In general, such ids can't be treated
/// as integers and require a different handling, usually with overhead.
///
/// Any id must also have a representation for a "sentinel" value. For integers,
/// we use the maximum value of the corresponding type for the sentinel, so we
/// don't introduce the overhead of using `Option<int>` and can use 0 as the
/// first index (as is natural).
pub trait IdType: Clone + Ord + Hash + Debug {
    /// Conceptually `None` in `Option<Id>`, but without using `Option`.
    fn sentinel() -> Self;

    /// Determines if the id type is `usize`-compatible.
    ///
    /// Types that are not `usize`-compatible require a special, often less
    /// efficient handling.
    fn is_integer() -> bool;

    /// Converts an id into the corresponding `u64`.
    ///
    /// Types for which `is_integer() == false` should panic in this function.
    fn as_bits(&self) -> u64;

    /// Converts an `u64` into the corresponding id.
    ///
    /// Types for which `is_integer() == false` should panic in this function.
    fn from_bits(bits: u64) -> Self;

    /// Converts an id into the corresponding `usize`.
    ///
    /// Types for which `is_integer() == false` should panic in this function.
    fn as_usize(&self) -> usize {
        self.as_bits() as usize
    }

    /// Converts an `usize` into the corresponding id.
    ///
    /// Types for which `is_integer() == false` should panic in this function.
    fn from_usize(index: usize) -> Self {
        Self::from_bits(index as u64)
    }

    fn is_sentinel(&self) -> bool {
        self == &Self::sentinel()
    }
}

/// Type-level specification that an id type is integer-like.
///
/// All types that implement this trait must return `true` in
/// `IdType::is_integer`.
pub trait IntegerIdType: IdType + Copy + From<usize> + Into<usize> {}

// For edge ids that are represented as a pair of vertex ids.
impl<T: IdType, U: IdType> IdType for (T, U) {
    fn sentinel() -> Self {
        (T::sentinel(), U::sentinel())
    }

    fn is_integer() -> bool {
        false
    }

    fn as_bits(&self) -> u64 {
        panic!("unsupported")
    }

    fn from_bits(_: u64) -> Self {
        panic!("unsupported")
    }
}

pub trait AsIdRef<I: IdType> {
    fn as_id(&self) -> OwnableRef<'_, I>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VertexId<N = u64>(N);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EdgeId<N = u64>(N);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Virtual<I>(u64, PhantomData<I>);

impl<I: IdType> IdType for Virtual<I> {
    fn sentinel() -> Self {
        Self::from_bits(u64::MAX)
    }

    fn is_integer() -> bool {
        true
    }

    fn as_bits(&self) -> u64 {
        self.0
    }

    fn from_bits(bits: u64) -> Self {
        Self(bits, PhantomData)
    }
}

impl<I: IntegerIdType> From<usize> for Virtual<I> {
    fn from(index: usize) -> Self {
        Self::from_usize(index)
    }
}

impl<I: IntegerIdType> From<Virtual<I>> for usize {
    fn from(id: Virtual<I>) -> Self {
        id.as_usize()
    }
}

impl<I: IntegerIdType> From<u64> for Virtual<I> {
    fn from(bits: u64) -> Self {
        Self::from_bits(bits)
    }
}

impl<I: IntegerIdType> From<Virtual<I>> for u64 {
    fn from(id: Virtual<I>) -> Self {
        id.as_bits()
    }
}

impl<I: IntegerIdType> IntegerIdType for Virtual<I> {}

pub trait IdPair {
    type VertexId: IdType;
    type EdgeId: IdType;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DefaultId {}

impl IdPair for DefaultId {
    type VertexId = VertexId;
    type EdgeId = EdgeId;
}

pub struct CustomId<VI, EI> {
    ty: PhantomData<fn() -> (VI, EI)>,
}

impl<VI: IdType, EI: IdType> IdPair for CustomId<VI, EI> {
    type VertexId = VI;
    type EdgeId = EI;
}

pub(crate) trait UseId<Id: IdPair> {
    type Id: IdType;
}

pub(crate) enum UseVertexId {}

impl<Id: IdPair> UseId<Id> for UseVertexId {
    type Id = Id::VertexId;
}

#[allow(unused)]
pub(crate) enum UseEdgeId {}

impl<Id: IdPair> UseId<Id> for UseEdgeId {
    type Id = Id::EdgeId;
}

macro_rules! impl_int_id {
    ($id_ty:ident, $int_ty:ty) => {
        impl IdType for $id_ty<$int_ty> {
            fn sentinel() -> Self {
                Self(<$int_ty>::MAX)
            }

            fn is_integer() -> bool {
                true
            }

            fn as_bits(&self) -> u64 {
                self.0 as u64
            }

            fn from_bits(bits: u64) -> Self {
                Self(bits as $int_ty)
            }

            fn as_usize(&self) -> usize {
                self.0.try_into().expect("id type overflow")
            }

            fn from_usize(index: usize) -> Self {
                Self(index.try_into().expect("id type overflow"))
            }
        }

        impl From<usize> for $id_ty<$int_ty> {
            fn from(index: usize) -> Self {
                Self::from_usize(index)
            }
        }

        impl From<$id_ty<$int_ty>> for usize {
            fn from(id: $id_ty<$int_ty>) -> Self {
                id.as_usize()
            }
        }

        impl IntegerIdType for $id_ty<$int_ty> {}
    };
}

impl_int_id!(VertexId, usize);
impl_int_id!(VertexId, u64);
impl_int_id!(VertexId, u32);
impl_int_id!(VertexId, u16);
impl_int_id!(VertexId, u8);

impl_int_id!(EdgeId, usize);
impl_int_id!(EdgeId, u64);
impl_int_id!(EdgeId, u32);
impl_int_id!(EdgeId, u16);
impl_int_id!(EdgeId, u8);

impl IdType for () {
    #[allow(clippy::unused_unit)]
    fn sentinel() -> Self {
        ()
    }

    fn is_integer() -> bool {
        false
    }

    fn as_bits(&self) -> u64 {
        panic!("unsupported")
    }

    fn from_bits(_: u64) -> Self {
        panic!("unsupported")
    }
}

impl<I> AsIdRef<I> for I
where
    I: IdType,
{
    fn as_id(&self) -> OwnableRef<'_, I> {
        OwnableRef::Borrowed(self)
    }
}

impl<I> AsIdRef<I> for &I
where
    I: IdType,
{
    fn as_id(&self) -> OwnableRef<'_, I> {
        OwnableRef::Borrowed(self)
    }
}

impl<I> AsIdRef<I> for usize
where
    I: IntegerIdType,
{
    fn as_id(&self) -> OwnableRef<'_, I> {
        OwnableRef::Owned(I::from(*self))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn supports_different_id_variants() {
        fn inner(_id: &VertexId) {}

        fn outer<I>(id: I)
        where
            I: AsIdRef<VertexId>,
        {
            inner(id.as_id().as_ref())
        }

        outer(VertexId::from_usize(3));
        #[allow(clippy::needless_borrows_for_generic_args)]
        outer(&VertexId::from_usize(3));
        outer(3);
    }
}
