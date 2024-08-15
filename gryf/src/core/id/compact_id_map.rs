use std::cmp::min;

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
#[derive(Debug)]
pub struct CompactIdMap<I> {
    map: Vec<I>,
    len: usize,
}

impl<I: IntegerIdType> CompactIdMap<I> {
    /// Constructs the map from the iterator of IDs.
    pub fn new<A>(iter: A) -> Self
    where
        A: Iterator<Item = I>,
    {
        let mut map = iter.collect::<Vec<_>>();
        map.sort_unstable_by_key(|id| id.as_bits());
        let len = map.len();

        Self { map, len }
    }

    /// Constructs a noop map where real IDs are already in contiguous sequence.
    pub fn isomorphic(len: usize) -> Self {
        Self {
            map: Vec::new(),
            len,
        }
    }

    /// Returns the number of IDs in the map.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns `true` if the map contains no IDs.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns `true` if the mapping is noop.
    pub fn is_isomorphic(&self) -> bool {
        self.map.len() != self.len
    }

    /// Maps given virtual ID to the corresponding real ID in the original graph.
    ///
    /// `Into<Virtual<I>>` is used instead of `Virtual<I>` because the
    /// algorithms often work with `usize` and that can be used instead of
    /// explicitly constructing the `Virtual` type.
    pub fn to_real<V: Into<Virtual<I>>>(&self, id: V) -> Option<I> {
        let id: Virtual<I> = id.into();

        if self.is_isomorphic() {
            (id.as_usize() < self.len()).then(|| I::from_bits(id.as_bits()))
        } else {
            self.map.get(id.as_usize()).cloned()
        }
    }

    /// Maps given real ID from the original graph to a virtual ID in the
    /// contiguous sequence.
    pub fn to_virt(&self, id: I) -> Option<Virtual<I>> {
        if self.is_isomorphic() {
            (id.as_usize() < self.len()).then(|| Virtual::from_bits(id.as_bits()))
        } else {
            // Using `wrapping_sub` not to panic on overflow.
            let direct = min(id.as_usize(), self.len().wrapping_sub(1));
            let direct_elem = self.map.get(direct).cloned()?;

            // This will always be true for storages without holes, and sometimes
            // for storages with holes.
            if direct_elem == id {
                return Some(Virtual::from_usize(direct));
            }

            // TODO: Employ heuristics that would identify that the real id
            // is actually not far from the position in the mapping and so it
            // can be found faster than by binary search over the whole map.

            // Fallback to binary search otherwise.
            self.map.binary_search(&id).ok().map(Virtual::from_usize)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::core::id::VertexId;

    #[test]
    fn no_holes() {
        let map = CompactIdMap::new((0..10u64).map(VertexId));

        assert_eq!(map.to_virt(3.into()), Some(Virtual::from_usize(3)));
        assert_eq!(map.to_real(3u64), Some(VertexId(3)));
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

    // TODO: proptest, generate a random Vec of VertexId and create the mapping,
    // then for every v from the array, map.real(map.virt(v)) == v.
}
