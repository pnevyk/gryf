use std::cmp::min;

use crate::index::{IndexType, Virtual};

// For compact storages, the space and time for both directions is constant (use
// `isomorphic`). For storages with holes, the space is O(|V|), virtual to real
// is O(1) and real to virtual is O(log(|V|)) with heuristics that make it O(1)
// in many cases.
#[derive(Debug)]
pub struct CompactIndexMap<I> {
    map: Vec<I>,
    len: usize,
}

impl<I: IndexType> CompactIndexMap<I> {
    pub fn new<A>(iter: A) -> Self
    where
        A: Iterator<Item = I>,
    {
        let mut map = iter.collect::<Vec<_>>();
        map.sort_unstable_by_key(|index| index.to_usize());
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

    pub fn real<V: Into<Virtual<I>>>(&self, index: V) -> I {
        // Into<Virtual<I>> is used instead of Virtual<I> because the algorithms
        // will usually work with numeric indices with their data structures and
        // so it is more convenient to use this.
        let index = index.into();

        if self.is_isomorphic() {
            if index.to_usize() < self.len() {
                I::new(index.to_usize())
            } else {
                panic!("virtual vertex does not exist");
            }
        } else {
            self.map
                .get(index.to_usize())
                .copied()
                .expect("virtual vertex does not exist")
        }
    }

    pub fn virt(&self, index: I) -> Virtual<I> {
        if self.is_isomorphic() {
            if index.to_usize() < self.len() {
                Virtual::new(index.to_usize())
            } else {
                panic!("vertex does not exist");
            }
        } else {
            // Using `wrapping_sub` not to panic on overflow, but on indexing below
            // with custom panic message.
            let direct = min(index.to_usize(), self.len().wrapping_sub(1));
            let direct_elem = self
                .map
                .get(direct)
                .copied()
                .expect("vertex does not exist");

            // This will always be true for storages without holes, and sometimes
            // for storages with holes.
            if direct_elem == index {
                return Virtual::new(direct);
            }

            // TODO: Employ heuristics that would identify that the real index
            // is actually not far from the position in the mapping and so it
            // can be found faster than by binary search over the whole map.

            // Fallback to binary search otherwise.
            Virtual::new(
                self.map
                    .binary_search(&index)
                    .expect("vertex does not exist"),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::index::VertexIndex;

    #[test]
    fn no_holes() {
        let map = CompactIndexMap::new((0..10).map(VertexIndex::new));

        assert_eq!(map.virt(3.into()), Virtual::new(3));
        assert_eq!(map.real(3), VertexIndex::new(3));
    }

    #[test]
    fn holes_matching() {
        let map = CompactIndexMap::new((0..10).filter(|&i| i != 5).map(VertexIndex::new));

        assert_eq!(map.virt(3.into()), Virtual::new(3));
        assert_eq!(map.real(3), VertexIndex::new(3));
    }

    #[test]
    fn holes_close() {
        let map = CompactIndexMap::new((0..20).filter(|&i| i != 15).map(VertexIndex::new));

        assert_eq!(map.virt(18.into()), Virtual::new(17));
        assert_eq!(map.real(17), VertexIndex::new(18));
    }

    #[test]
    fn holes_binary_search() {
        let map = CompactIndexMap::new(
            (0..20)
                .filter(|i| !(5..15).contains(i))
                .map(VertexIndex::new),
        );

        assert_eq!(map.virt(15.into()), Virtual::new(5));
        assert_eq!(map.real(5), VertexIndex::new(15));
    }

    #[test]
    fn isomorphic() {
        let map = CompactIndexMap::<VertexIndex>::isomorphic(10);

        assert_eq!(map.virt(3.into()), Virtual::new(3));
        assert_eq!(map.real(3), VertexIndex::new(3));
    }

    // TODO: proptest, generate a random Vec of VertexIndex and create the
    // mapping, then for every v from the array, map.real(map.virt(v)) == v.
}
