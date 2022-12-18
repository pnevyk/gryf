use std::cmp::min;

use crate::index::{NumIndexType, Virtual};

// For compact storages, the space and time for both directions is constant (use
// `isomorphic`). For storages with holes, the space is O(|V|), virtual to real
// is O(1) and real to virtual is O(log(|V|)) with heuristics that make it O(1)
// in many cases.
#[derive(Debug)]
pub struct CompactIndexMap<I> {
    map: Vec<I>,
    len: usize,
}

impl<I: NumIndexType> CompactIndexMap<I> {
    pub fn new<A>(iter: A) -> Self
    where
        A: Iterator<Item = I>,
    {
        let mut map = iter.collect::<Vec<_>>();
        map.sort_unstable_by_key(|index| index.to_bits());
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

    pub fn real<V: Into<Virtual<I>>>(&self, index: V) -> Option<I> {
        // Into<Virtual<I>> is used instead of Virtual<I> because the algorithms
        // will usually work with numeric indices with their data structures and
        // so it is more convenient to use this.
        let index: Virtual<I> = index.into();

        if self.is_isomorphic() {
            (index.to_usize() < self.len()).then(|| I::from_bits(index.to_bits()))
        } else {
            self.map.get(index.to_usize()).copied()
        }
    }

    pub fn virt(&self, index: I) -> Option<Virtual<I>> {
        if self.is_isomorphic() {
            (index.to_usize() < self.len()).then(|| Virtual::new(index.to_bits()))
        } else {
            // Using `wrapping_sub` not to panic on overflow.
            let direct = min(index.to_usize(), self.len().wrapping_sub(1));
            let direct_elem = self.map.get(direct).copied()?;

            // This will always be true for storages without holes, and sometimes
            // for storages with holes.
            if direct_elem == index {
                return Some(Virtual::new(direct as u64));
            }

            // TODO: Employ heuristics that would identify that the real index
            // is actually not far from the position in the mapping and so it
            // can be found faster than by binary search over the whole map.

            // Fallback to binary search otherwise.
            self.map.binary_search(&index).ok().map(Virtual::from_usize)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::index::VertexIndex;

    #[test]
    fn no_holes() {
        let map = CompactIndexMap::new((0..10u64).map(VertexIndex));

        assert_eq!(map.virt(3.into()), Some(Virtual::new(3)));
        assert_eq!(map.real(3u64), Some(VertexIndex(3)));
    }

    #[test]
    fn holes_matching() {
        let map = CompactIndexMap::new((0..10u64).filter(|&i| i != 5).map(VertexIndex));

        assert_eq!(map.virt(3.into()), Some(Virtual::new(3)));
        assert_eq!(map.real(3u64), Some(VertexIndex(3)));
    }

    #[test]
    fn holes_close() {
        let map = CompactIndexMap::new((0..20u64).filter(|&i| i != 15).map(VertexIndex));

        assert_eq!(map.virt(18.into()), Some(Virtual::new(17)));
        assert_eq!(map.real(17u64), Some(VertexIndex(18)));
    }

    #[test]
    fn holes_binary_search() {
        let map =
            CompactIndexMap::new((0..20u64).filter(|i| !(5..15).contains(i)).map(VertexIndex));

        assert_eq!(map.virt(15.into()), Some(Virtual::new(5)));
        assert_eq!(map.real(5u64), Some(VertexIndex(15)));
    }

    #[test]
    fn isomorphic() {
        let map = CompactIndexMap::<VertexIndex>::isomorphic(10);

        assert_eq!(map.virt(3.into()), Some(Virtual::new(3)));
        assert_eq!(map.real(3u64), Some(VertexIndex(3)));
    }

    // TODO: proptest, generate a random Vec of VertexIndex and create the
    // mapping, then for every v from the array, map.real(map.virt(v)) == v.
}
