use super::{
    base::{GraphBase, WeakRef},
    id::IdType,
    marker::{Direction, EdgeType},
};

pub trait NeighborRef<VId: IdType, EId: IdType> {
    fn id(&self) -> WeakRef<'_, VId>;
    fn edge(&self) -> WeakRef<'_, EId>;
    fn src(&self) -> WeakRef<'_, VId>;
    fn dir(&self) -> Direction;
}

pub trait Neighbors: GraphBase {
    type NeighborRef<'a>: NeighborRef<Self::VertexId, Self::EdgeId>
    where
        Self: 'a;

    type NeighborsIter<'a>: Iterator<Item = Self::NeighborRef<'a>>
    where
        Self: 'a;

    fn neighbors(&self, src: &Self::VertexId) -> Self::NeighborsIter<'_>;
    fn neighbors_directed(&self, src: &Self::VertexId, dir: Direction) -> Self::NeighborsIter<'_>;

    fn degree(&self, id: &Self::VertexId) -> usize {
        if Self::EdgeType::is_directed() {
            self.degree_directed(id, Direction::Outgoing)
                + self.degree_directed(id, Direction::Incoming)
        } else {
            self.degree_directed(id, Direction::Outgoing)
        }
    }

    fn degree_directed(&self, id: &Self::VertexId, dir: Direction) -> usize {
        if Self::EdgeType::is_directed() {
            self.neighbors_directed(id, dir).count()
        } else {
            // In undirected graphs, we need to handle self-loops.
            self.neighbors_directed(id, dir)
                .map(|neighbor| {
                    // If this is a self-loop, we need to count it twice.
                    // Storages are required to yield a self-loop just once. If
                    // this requirement is satisfied, then this implementation
                    // of degree is correct.
                    if neighbor.id().as_ref() == id {
                        2
                    } else {
                        1
                    }
                })
                .sum()
        }
    }
}

impl<VId: IdType, EId: IdType> NeighborRef<VId, EId> for (VId, EId, VId, Direction) {
    fn id(&self) -> WeakRef<'_, VId> {
        WeakRef::Borrowed(&self.0)
    }

    fn edge(&self) -> WeakRef<'_, EId> {
        WeakRef::Borrowed(&self.1)
    }

    fn src(&self) -> WeakRef<'_, VId> {
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

            fn neighbors(&self, src: &Self::VertexId) -> Self::NeighborsIter<'_> {
                (**self).neighbors(src)
            }

            fn neighbors_directed(&self, src: &Self::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
                (**self).neighbors_directed(src, dir)
            }

            fn degree(&self, id: &Self::VertexId) -> usize {
                (**self).degree(id)
            }

            fn degree_directed(&self, id: &Self::VertexId, dir: Direction) -> usize {
                (**self).degree_directed(id, dir)
            }
        }
    }
}

deref_neighbors!(&);
deref_neighbors!(&mut);
