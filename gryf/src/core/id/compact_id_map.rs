use std::cmp::min;

use indexmap::IndexMap;
use rustc_hash::FxBuildHasher;

use crate::core::id::{IdType, IntegerIdType, Virtual};

/// Mapping from graph IDs to a contiguous sequence of [virtual](Virtual) IDs
/// that can be used in algorithms.
///
/// For compact storages (e.g., [`AdjList`](crate::storage::AdjList)) that do
/// not have any holes in ID sequences even after removing, the mapping is a
/// noop and doesn't take any memory. Use [`CompactIdMap::isomorphic`]
/// constructor in these cases.
///
/// For storages with holes and _N_ vertices or edges, the time and space
/// properties are:
///
/// * memory used: _O(N)_
/// * virtual to real mapping: _O(1)_
/// * real to virtual mapping: _O(log(N))_
///
/// For non-integer IDs, the time and space properties are unspecified, but they
/// are expected to be slower than integer IDs.
#[derive(Debug)]
pub struct CompactIdMap<I> {
    backend: Backend<I>,
}

#[derive(Debug)]
enum Backend<I> {
    Isomorphic {
        len: usize,
    },
    Vec {
        map: Vec<I>,
    },
    Map {
        map: IndexMap<I, usize, FxBuildHasher>,
    },
}

impl<I: IdType> CompactIdMap<I> {
    /// Constructs the map from the iterator of IDs.
    pub fn new<A>(iter: A) -> Self
    where
        A: Iterator<Item = I>,
    {
        let backend = if I::is_integer() {
            // Integer IDs can be handled by Vec.
            let mut map = Vec::with_capacity(iter.size_hint().1.unwrap_or(32));
            let mut hole = false;

            for (i, id) in iter.enumerate() {
                // If the ID doesn't corresponding to the enumeration index,
                // then there is a hole.
                hole |= id != I::from_usize(i);
                map.push(id);
            }

            if hole {
                // Make sure that the IDs are sorted, which is later assumed.
                map.sort_unstable();
                Backend::Vec { map }
            } else {
                // There is no hole, isomorphic backend with no overhead can be
                // used.
                Backend::Isomorphic { len: map.len() }
            }
        } else {
            // Otherwise, we need to fall back to a real lookup table
            // implementation.
            Backend::Map {
                map: iter.enumerate().map(|(i, id)| (id, i)).collect(),
            }
        };

        Self { backend }
    }

    /// Constructs a noop map where real IDs are already in contiguous sequence.
    pub fn isomorphic(len: usize) -> Self
    where
        // Isomorphic map makes only sense for integer IDs.
        I: IntegerIdType,
    {
        Self {
            backend: Backend::Isomorphic { len },
        }
    }

    /// Returns the number of IDs in the map.
    pub fn len(&self) -> usize {
        match self.backend {
            Backend::Isomorphic { len } => len,
            Backend::Vec { ref map } => map.len(),
            Backend::Map { ref map } => map.len(),
        }
    }

    /// Returns `true` if the map contains no IDs.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns `true` if the mapping is noop.
    pub fn is_isomorphic(&self) -> bool {
        matches!(self.backend, Backend::Isomorphic { .. })
    }

    /// Maps given virtual ID to the corresponding real ID in the original graph.
    ///
    /// `Into<Virtual<I>>` is used instead of `Virtual<I>` because the
    /// algorithms often work with `usize` and that can be used instead of
    /// explicitly constructing the `Virtual` type.
    pub fn to_real<V: Into<Virtual<I>>>(&self, id: V) -> Option<I> {
        let id: Virtual<I> = id.into();

        match self.backend {
            Backend::Isomorphic { len } => {
                (id.as_usize() < len).then(|| I::from_bits(id.as_bits()))
            }
            Backend::Vec { ref map } => map.get(id.as_usize()).cloned(),
            Backend::Map { ref map } => map.get_index(id.as_usize()).map(|(id, _)| id.clone()),
        }
    }

    /// Maps given real ID from the original graph to a virtual ID in the
    /// contiguous sequence.
    pub fn to_virt(&self, id: I) -> Option<Virtual<I>> {
        match self.backend {
            Backend::Isomorphic { len } => {
                (id.as_usize() < len).then(|| Virtual::from_bits(id.as_bits()))
            }
            Backend::Vec { ref map } => {
                // Using `wrapping_sub` not to panic on overflow.
                let direct = min(id.as_usize(), self.len().wrapping_sub(1));
                let direct_elem = map.get(direct).cloned()?;

                // This is possible when the first hole in the mapping is after
                // the given ID.
                if direct_elem == id {
                    return Some(Virtual::from_usize(direct));
                }

                // TODO: Employ heuristics that would identify that the real id
                // is actually not far from the position in the mapping and so it
                // can be found faster than by binary search over the whole map.

                // Fallback to binary search otherwise.
                map.binary_search(&id).ok().map(Virtual::from_usize)
            }
            Backend::Map { ref map } => map.get(&id).copied().map(Virtual::from_usize),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use proptest::prelude::*;

    use super::*;

    use crate::core::id::VertexId;

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct NonIntegerId<'a>(&'a str);

    impl IdType for NonIntegerId<'_> {
        fn sentinel() -> Self {
            NonIntegerId("")
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

    #[test]
    fn no_holes() {
        let map = CompactIdMap::new((0..10u64).map(VertexId));

        assert_eq!(map.to_virt(3.into()), Some(Virtual::from_usize(3)));
        assert_eq!(map.to_real(3u64), Some(VertexId(3)));
    }

    #[test]
    fn no_holes_means_isomorphic() {
        let map = CompactIdMap::new((0..10u64).map(VertexId));

        assert!(map.is_isomorphic());
    }

    #[test]
    fn holes_matching() {
        let map = CompactIdMap::new((0..10u64).filter(|&i| i != 5).map(VertexId));

        assert_eq!(map.to_virt(3.into()), Some(Virtual::from_usize(3)));
        assert_eq!(map.to_real(3u64), Some(VertexId(3)));
    }

    #[test]
    fn holes_close() {
        let map = CompactIdMap::new((0..20u64).filter(|&i| i != 15).map(VertexId));

        assert_eq!(map.to_virt(18.into()), Some(Virtual::from_usize(17)));
        assert_eq!(map.to_real(17u64), Some(VertexId(18)));
    }

    #[test]
    fn holes_binary_search() {
        let map = CompactIdMap::new((0..20u64).filter(|i| !(5..15).contains(i)).map(VertexId));

        assert_eq!(map.to_virt(15.into()), Some(Virtual::from_usize(5)));
        assert_eq!(map.to_real(5u64), Some(VertexId(15)));
    }

    #[test]
    fn isomorphic() {
        let map = CompactIdMap::<VertexId>::isomorphic(10);

        assert_eq!(map.to_virt(3.into()), Some(Virtual::from_usize(3)));
        assert_eq!(map.to_real(3u64), Some(VertexId(3)));
    }

    #[test]
    fn sorted_vec() {
        let map = CompactIdMap::new([3usize, 5, 1].into_iter().map(VertexId));

        assert_eq!(map.to_real(0u64), Some(VertexId(1)));
        assert_eq!(map.to_real(1u64), Some(VertexId(3)));
        assert_eq!(map.to_real(2u64), Some(VertexId(5)));
    }

    #[test]
    fn non_integer() {
        let map = CompactIdMap::<NonIntegerId>::new(
            [NonIntegerId("foo"), NonIntegerId("bar")].into_iter(),
        );

        assert_eq!(
            map.to_virt(NonIntegerId("bar")),
            Some(Virtual::from_usize(1))
        );
        assert_eq!(
            map.to_real(Virtual::from_usize(0)),
            Some(NonIntegerId("foo"))
        );
    }

    fn arb_unique_vec<T: Debug + Ord + Arbitrary>() -> impl Strategy<Value = Vec<T>> {
        any::<Vec<T>>().prop_map(|mut values| {
            values.sort_unstable();
            values.dedup();
            values
        })
    }

    proptest! {
        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_random_int_ids(ids in arb_unique_vec::<u16>()) {
            let map = CompactIdMap::new(ids.iter().cloned().map(VertexId::<u16>));

            for id in ids {
                let id = VertexId(id);
                prop_assert_eq!(id, map.to_real(map.to_virt(id).unwrap()).unwrap());
            }
        }

        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_random_non_int_ids(ids in arb_unique_vec::<String>()) {
            let cloned = ids.clone();
            let map = CompactIdMap::new(cloned.iter().map(|id| NonIntegerId(id.as_str())));

            for id in ids {
                let id = NonIntegerId(id.as_str());
                prop_assert_eq!(id.clone(), map.to_real(map.to_virt(id).unwrap()).unwrap());
            }
        }
    }
}
