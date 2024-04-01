use std::cmp::min;

use crate::core::id::{IdType, IntegerIdType, Virtual};

// For compact storages, the space and time for both directions is constant (use
// `isomorphic`). For storages with holes, the space is O(|V|), virtual to real
// is O(1) and real to virtual is O(log(|V|)) with heuristics that make it O(1)
// in many cases.
#[derive(Debug)]
pub struct CompactIdMap<I> {
    map: Vec<I>,
    len: usize,
}

impl<I: IntegerIdType> CompactIdMap<I> {
    pub fn new<A>(iter: A) -> Self
    where
        A: Iterator<Item = I>,
    {
        let mut map = iter.collect::<Vec<_>>();
        map.sort_unstable_by_key(|id| id.as_bits());
        let len = map.len();

        Self { map, len }
    }

    pub fn isomorphic(len: usize) -> Self {
        Self {
            map: Vec::new(),
            len,
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_isomorphic(&self) -> bool {
        self.map.len() != self.len
    }

    pub fn real<V: Into<Virtual<I>>>(&self, id: V) -> Option<I> {
        // Into<Virtual<I>> is used instead of Virtual<I> because the algorithms
        // will usually work with numeric ids with their data structures and so
        // it is more convenient to use this.
        let id: Virtual<I> = id.into();

        if self.is_isomorphic() {
            (id.as_usize() < self.len()).then(|| I::from_bits(id.as_bits()))
        } else {
            self.map.get(id.as_usize()).cloned()
        }
    }

    pub fn virt(&self, id: I) -> Option<Virtual<I>> {
        if self.is_isomorphic() {
            (id.as_usize() < self.len()).then(|| Virtual::new(id.as_bits()))
        } else {
            // Using `wrapping_sub` not to panic on overflow.
            let direct = min(id.as_usize(), self.len().wrapping_sub(1));
            let direct_elem = self.map.get(direct).cloned()?;

            // This will always be true for storages without holes, and sometimes
            // for storages with holes.
            if direct_elem == id {
                return Some(Virtual::new(direct as u64));
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

        assert_eq!(map.virt(3.into()), Some(Virtual::new(3)));
        assert_eq!(map.real(3u64), Some(VertexId(3)));
    }

    #[test]
    fn holes_matching() {
        let map = CompactIdMap::new((0..10u64).filter(|&i| i != 5).map(VertexId));

        assert_eq!(map.virt(3.into()), Some(Virtual::new(3)));
        assert_eq!(map.real(3u64), Some(VertexId(3)));
    }

    #[test]
    fn holes_close() {
        let map = CompactIdMap::new((0..20u64).filter(|&i| i != 15).map(VertexId));

        assert_eq!(map.virt(18.into()), Some(Virtual::new(17)));
        assert_eq!(map.real(17u64), Some(VertexId(18)));
    }

    #[test]
    fn holes_binary_search() {
        let map = CompactIdMap::new((0..20u64).filter(|i| !(5..15).contains(i)).map(VertexId));

        assert_eq!(map.virt(15.into()), Some(Virtual::new(5)));
        assert_eq!(map.real(5u64), Some(VertexId(15)));
    }

    #[test]
    fn isomorphic() {
        let map = CompactIdMap::<VertexId>::isomorphic(10);

        assert_eq!(map.virt(3.into()), Some(Virtual::new(3)));
        assert_eq!(map.real(3u64), Some(VertexId(3)));
    }

    // TODO: proptest, generate a random Vec of VertexId and create the mapping,
    // then for every v from the array, map.real(map.virt(v)) == v.
}
