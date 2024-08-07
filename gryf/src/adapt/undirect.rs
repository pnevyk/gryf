use crate::core::{marker::Direction, Neighbors};

use gryf_derive::{EdgeSet, GraphBase, Guarantee, VertexSet};

#[derive(Debug, GraphBase, VertexSet, EdgeSet, Guarantee)]
#[gryf_crate]
pub struct Undirect<G> {
    #[graph]
    graph: G,
}

impl<G> Undirect<G> {
    pub fn new(graph: G) -> Self {
        Self { graph }
    }

    pub fn into_inner(self) -> G {
        self.graph
    }
}

impl<G> Neighbors for Undirect<G>
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

    fn degree(&self, id: &Self::VertexId) -> usize {
        self.graph.degree(id)
    }

    fn degree_directed(&self, id: &Self::VertexId, _dir: Direction) -> usize {
        self.graph.degree(id)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        core::{marker::Directed, GraphAdd},
        storage::AdjList,
    };

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
