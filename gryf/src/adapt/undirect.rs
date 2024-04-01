use std::marker::PhantomData;

use crate::{
    common::CompactIdMap,
    core::{id::NumIdType, marker::Direction, Neighbors, VerticesBase, WeakRef},
};

use gryf_derive::{
    Edges, EdgesBase, EdgesBaseWeak, EdgesMut, EdgesWeak, GraphBase, Guarantee, Vertices,
    VerticesBase, VerticesBaseWeak, VerticesMut, VerticesWeak,
};

// TODO: Remove these imports once hygiene of procedural macros is fixed.
use crate::core::{
    marker::EdgeType, AddEdgeError, AddVertexError, Edges, EdgesBase, EdgesBaseWeak, EdgesMut,
    EdgesWeak, GraphBase, Guarantee, Vertices, VerticesBaseWeak, VerticesMut, VerticesWeak,
};

#[derive(
    Debug,
    GraphBase,
    VerticesBase,
    Vertices,
    VerticesMut,
    EdgesBase,
    Edges,
    EdgesMut,
    VerticesBaseWeak,
    VerticesWeak,
    EdgesBaseWeak,
    EdgesWeak,
    Guarantee,
)]
pub struct Undirect<Ty: EdgeType, G> {
    #[graph]
    graph: G,
    ty: PhantomData<Ty>,
}

impl<Ty: EdgeType, G> Undirect<Ty, G>
where
    G: EdgesBaseWeak<Ty>,
{
    pub fn new(graph: G) -> Self {
        Self {
            graph,
            ty: PhantomData,
        }
    }

    pub fn into_inner(self) -> G {
        self.graph
    }
}

impl<Ty: EdgeType, G> Neighbors for Undirect<Ty, G>
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
        self.graph.neighbors(src)
    }

    fn neighbors_directed(&self, src: &Self::VertexId, _dir: Direction) -> Self::NeighborsIter<'_> {
        self.neighbors(src)
    }

    fn degree(&self, index: &Self::VertexId) -> usize {
        self.graph.degree(index)
    }

    fn degree_directed(&self, index: &Self::VertexId, _dir: Direction) -> usize {
        self.graph.degree(index)
    }
}

#[cfg(test)]
mod tests {
    use crate::{core::marker::Directed, storage::AdjList};

    use super::*;

    #[test]
    fn neighbors() {
        let mut graph = AdjList::<_, _, Directed, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v2, &v0, ());

        let graph = Undirect::new(graph);

        assert_eq!(
            graph.neighbors_directed(&v0, Direction::Outgoing).count(),
            2
        );
        assert_eq!(
            graph.neighbors_directed(&v0, Direction::Incoming).count(),
            2
        );
        assert_eq!(graph.degree_directed(&v0, Direction::Outgoing), 2);
        assert_eq!(graph.degree_directed(&v0, Direction::Incoming), 2);
    }
}
