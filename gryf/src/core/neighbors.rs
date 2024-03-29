use super::{
    base::{GraphBase, WeakRef},
    index::IndexType,
    marker::{Direction, EdgeType},
};

pub trait NeighborRef<VI: IndexType, EI: IndexType> {
    fn index(&self) -> WeakRef<'_, VI>;
    fn edge(&self) -> WeakRef<'_, EI>;
    fn src(&self) -> WeakRef<'_, VI>;
    fn dir(&self) -> Direction;
}

pub trait Neighbors: GraphBase {
    type NeighborRef<'a>: NeighborRef<Self::VertexIndex, Self::EdgeIndex>
    where
        Self: 'a;

    type NeighborsIter<'a>: Iterator<Item = Self::NeighborRef<'a>>
    where
        Self: 'a;

    fn neighbors(&self, src: &Self::VertexIndex) -> Self::NeighborsIter<'_>;
    fn neighbors_directed(
        &self,
        src: &Self::VertexIndex,
        dir: Direction,
    ) -> Self::NeighborsIter<'_>;

    fn degree(&self, index: &Self::VertexIndex) -> usize {
        if Self::EdgeType::is_directed() {
            self.degree_directed(index, Direction::Outgoing)
                + self.degree_directed(index, Direction::Incoming)
        } else {
            self.degree_directed(index, Direction::Outgoing)
        }
    }

    fn degree_directed(&self, index: &Self::VertexIndex, dir: Direction) -> usize {
        if Self::EdgeType::is_directed() {
            self.neighbors_directed(index, dir).count()
        } else {
            // In undirected graphs, we need to handle self-loops.
            self.neighbors_directed(index, dir)
                .map(|neighbor| {
                    // If this is a self-loop, we need to count it twice.
                    // Storages are required to yield a self-loop just once. If
                    // this requirement is satisfied, then this implementation
                    // of degree is correct.
                    if neighbor.index().as_ref() == index {
                        2
                    } else {
                        1
                    }
                })
                .sum()
        }
    }
}

impl<VI: IndexType, EI: IndexType> NeighborRef<VI, EI> for (VI, EI, VI, Direction) {
    fn index(&self) -> WeakRef<'_, VI> {
        WeakRef::Borrowed(&self.0)
    }

    fn edge(&self) -> WeakRef<'_, EI> {
        WeakRef::Borrowed(&self.1)
    }

    fn src(&self) -> WeakRef<'_, VI> {
        WeakRef::Borrowed(&self.2)
    }

    fn dir(&self) -> Direction {
        self.3
    }
}

macro_rules! deref_neighbors {
    ($($ref_kind:tt)*) => {
        impl<G> Neighbors for $($ref_kind)* G
        where
            G: Neighbors,
        {
            type NeighborRef<'a> = G::NeighborRef<'a>
            where
                Self: 'a;

            type NeighborsIter<'a> = G::NeighborsIter<'a>
            where
                Self: 'a;

            fn neighbors(&self, src: &Self::VertexIndex) -> Self::NeighborsIter<'_> {
                (**self).neighbors(src)
            }

            fn neighbors_directed(&self, src: &Self::VertexIndex, dir: Direction) -> Self::NeighborsIter<'_> {
                (**self).neighbors_directed(src, dir)
            }

            fn degree(&self, index: &Self::VertexIndex) -> usize {
                (**self).degree(index)
            }

            fn degree_directed(&self, index: &Self::VertexIndex, dir: Direction) -> usize {
                (**self).degree_directed(index, dir)
            }
        }
    }
}

deref_neighbors!(&);
deref_neighbors!(&mut);
