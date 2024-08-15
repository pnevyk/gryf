//! Traits and types used for identifying vertices and edges in graphs.
//!
//! All types that are supposed to be used as vertex/edge identifiers must
//! implement [`IdType`] trait. For better performance and more functionality,
//! they should also implement [`IntegerIdType`] if possible.
//!
//! The default ID types are [`VertexId`] and [`EdgeId`]. They are of size `u64`
//! by default, but this can be changed via their generic parameter `N`.

mod compact_id_map;

pub use compact_id_map::CompactIdMap;

use std::{fmt::Debug, hash::Hash, marker::PhantomData};

use super::borrow::OwnableRef;

/// A unique identification of a vertex or edge in a graph.
///
/// In standard graph representations, the ID type is an integer. Conceptually,
/// such an integer ID is of type `usize`, but one can choose a smaller integer
/// type (such as u8 or u16) to lower the memory footprint. In the cases of
/// integer ID, the algorithms can treat the ID as usize with all the benefits
/// (e.g., indexing to a contiguous array).
///
/// For implicit graphs, an ID can be of any form as long as it implements
/// required interface and super traits. In general, such IDs can't be treated
/// as integers and require a different handling, usually with overhead.
///
/// Any ID must also have a representation for a
/// "[sentinel](https://en.wikipedia.org/wiki/Sentinel_value)" value. For
/// integers, we use the maximum value of the corresponding type for the
/// sentinel, so we don't introduce the overhead of using `Option<int>` and can
/// use 0 as the first index as is natural.
pub trait IdType: Clone + Ord + Hash + Debug {
    /// Conceptually `None` in `Option<ID>`, but without using `Option`.
    fn sentinel() -> Self;

    /// Determines if the ID type is representable by an integer. See
    /// [IntegerIdType] for more details.
    ///
    /// Types that are not integers require a special, often less efficient
    /// handling.
    fn is_integer() -> bool;

    /// Converts an ID into the corresponding `u64`.
    ///
    /// # Panics
    ///
    /// Types for which [`Self::is_integer`](IdType::is_integer) returns `false`
    /// should panic.
    fn as_bits(&self) -> u64;

    /// Converts an `u64` into the corresponding ID.
    ///
    /// # Panics
    ///
    /// Types for which [`Self::is_integer`](IdType::is_integer) returns `false`
    /// should panic.
    fn from_bits(bits: u64) -> Self;

    /// Converts an ID into the corresponding `usize`.
    ///
    /// # Panics
    ///
    /// Types for which [`Self::is_integer`](IdType::is_integer) returns `false`
    /// should panic.
    fn as_usize(&self) -> usize {
        self.as_bits() as usize
    }

    /// Converts an `usize` into the corresponding ID.
    ///
    /// # Panics
    ///
    /// Types for which [`Self::is_integer`](IdType::is_integer) returns `false`
    /// should panic.
    fn from_usize(id: usize) -> Self {
        Self::from_bits(id as u64)
    }

    /// Returns `true` if the value represents the sentinel value.
    fn is_sentinel(&self) -> bool {
        self == &Self::sentinel()
    }
}

/// Type-level specification that an ID type is representable by integer.
///
/// Types that implement this trait must return `true` in [`IdType::is_integer`]
/// and support all integer-related conversions.
///
/// All integer values up to some upper bound should be valid IDs and there
/// should be no discontinuity. For example, check the two integer conversions
/// of a chess square below, where one is correct and one is wrong.
///
/// ```
/// struct ChessSquare {
///     file: u8,
///     rank: u8
/// }
///
/// impl ChessSquare {
///     fn as_bits_good(&self) -> u64 {
///         // Continuous space up to 63
///         (self.rank * 8 + self.file) as u64
///     }
///
///     fn as_bits_bad(&self) -> u64 {
///         // Discontinuity between (7, 0) == 7 and (0, 1) == 256.
///         (self.rank as u64) << 8 | (self.file as u64)
///     }
/// }
/// ```
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

/// Used to support `I`, `&I` and `usize` as vertex/edge IDs in function
/// arguments.
///
/// For integer ID types, passing the ID by value feels more natural and can
/// possibly lead to a more efficient generated code. For non-integer ID types,
/// cloning is an unnecessary and potentially costly operation and passing by
/// reference is preferred.
///
/// Using `usize` as the ID value is supported mainly for convenience during
/// graph building, but should be avoided after that.
///
/// Types in [`domain`](crate::domain) module use this trait for the ID
/// parameters so that the following works:
///
/// ```
/// use gryf::Graph;
///
/// let mut graph = Graph::<_, (), _>::new_undirected();
///
/// let v = graph.add_vertex(42);
///
/// graph.vertex(v); // by value
/// graph.vertex(&v); // by reference
/// graph.vertex(0); // usize
/// ```
pub trait AsIdRef<I: IdType> {
    /// Converts itself to the ID type `I`, either as owned value or a
    /// reference.
    fn as_id(&self) -> OwnableRef<'_, I>;
}

/// The default representation of an integer index for vertices. Generic type
/// `N` can be used to control the byte size of the backing integer (`u64` by
/// default).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VertexId<N = u64>(N);

/// The default representation of an integer index for edges. Generic type `N`
/// can be used to control the byte size of the backing integer (`u64` by
/// default).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EdgeId<N = u64>(N);

/// An alternative representation of an real ID in specific contexts. It is
/// always an integer type, even if the real ID is not.
///
/// In [`CompactIdMap`], it is used to represent a contiguous array of IDs of a
/// graph, even if the numbering of vertices or edges in the graph has "holes".
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

/// Specification of vertex and edge ID types pair.
///
/// The main purpose is a reduction of the number of generic parameters from two
/// to one (accepting the increase of associated types).
pub trait IdPair {
    /// ID type for vertices.
    type VertexId: IdType;

    /// ID type for edges.
    type EdgeId: IdType;
}

/// Default indexing using [`VertexId`] and [`EdgeId`] as the ID pair.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DefaultId {}

impl IdPair for DefaultId {
    type VertexId = VertexId;
    type EdgeId = EdgeId;
}

/// Custom indexing using `VI` and `EI` generic types as the ID pair.
///
/// # Examples
///
/// ```
/// use gryf::{
///     core::id::{CustomId, EdgeId, VertexId},
///     storage::AdjList,
///     Graph,
/// };
///
/// let mut graph =
///     Graph::new_directed_in(AdjList::with_id::<CustomId<VertexId<u8>, EdgeId<u16>>>());
///
/// let hello = graph.add_vertex("hello");
/// let world = graph.add_vertex("world");
///
/// graph.add_edge(hello, world, ());
/// ```
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
