use macros::VerticesMut;

use crate::index::{EdgeIndex, VertexIndex};
use crate::infra::CompactIndexMap;
use crate::marker::{Directed, Direction, EdgeType};
use crate::traits::*;
use crate::{EdgesWeak, Guarantee, Vertices, VerticesWeak};

#[derive(Debug, Vertices, VerticesMut, Guarantee, VerticesWeak, EdgesWeak)]
pub struct Reversed<G> {
    #[graph]
    inner: G,
}

impl<G> Reversed<G> {
    pub fn new(graph: G) -> Self {
        Self { inner: graph }
    }

    pub fn into_unmodified(self) -> G {
        self.inner
    }

    pub fn apply<E>(self) -> G
    where
        G: EdgesMut<E, Directed> + StableIndices,
    {
        let mut inner = self.inner;

        let edges = inner
            .edges()
            .map(|edge| (edge.src(), edge.index(), edge.dst()))
            .collect::<Vec<_>>();

        for (src, index, dst) in edges {
            let data = inner.remove_edge(index).unwrap();
            inner.add_edge(dst, src, data);
        }

        inner
    }
}

impl<E, G> Edges<E, Directed> for Reversed<G>
where
    G: Edges<E, Directed>,
{
    type EdgeRef<'a, T: 'a> = ReversedRef<G::EdgeRef<'a, T>>;

    type EdgeIndicesIter<'a>
    where
        Self: 'a,
    = G::EdgeIndicesIter<'a>;

    type EdgesIter<'a, T: 'a>
    where
        Self: 'a,
    = Iter<G::EdgesIter<'a, T>>;

    fn edge_count(&self) -> usize {
        self.inner.edge_count()
    }

    fn edge_bound(&self) -> usize {
        self.inner.edge_bound()
    }

    fn edge(&self, index: EdgeIndex) -> Option<&E> {
        self.inner.edge(index)
    }

    fn endpoints(&self, index: EdgeIndex) -> Option<(VertexIndex, VertexIndex)> {
        self.inner.endpoints(index).map(|(src, dst)| (dst, src))
    }

    fn edge_index(&self, src: VertexIndex, dst: VertexIndex) -> Option<EdgeIndex> {
        self.inner.edge_index(dst, src)
    }

    fn edge_indices(&self) -> Self::EdgeIndicesIter<'_> {
        self.inner.edge_indices()
    }

    fn edges(&self) -> Self::EdgesIter<'_, E> {
        Iter(self.inner.edges())
    }

    fn contains_edge(&self, index: EdgeIndex) -> bool {
        self.inner.contains_edge(index)
    }

    fn edge_index_map(&self) -> CompactIndexMap<EdgeIndex> {
        self.inner.edge_index_map()
    }
}

impl<G> Neighbors for Reversed<G>
where
    G: Neighbors,
{
    type NeighborRef<'a> = ReversedRef<G::NeighborRef<'a>>;

    type NeighborsIter<'a>
    where
        Self: 'a,
    = Iter<G::NeighborsIter<'a>>;

    fn neighbors(&self, src: VertexIndex) -> Self::NeighborsIter<'_> {
        Iter(self.inner.neighbors(src))
    }

    fn neighbors_directed(&self, src: VertexIndex, dir: Direction) -> Self::NeighborsIter<'_> {
        Iter(self.inner.neighbors_directed(src, dir.opposite()))
    }

    fn degree(&self, index: VertexIndex) -> usize {
        self.inner.degree(index)
    }

    fn degree_directed(&self, index: VertexIndex, dir: Direction) -> usize {
        self.inner.degree_directed(index, dir.opposite())
    }
}

pub struct ReversedRef<R>(R);

impl<E, R> EdgeRef<E, Directed> for ReversedRef<R>
where
    R: EdgeRef<E, Directed>,
{
    fn index(&self) -> EdgeIndex {
        self.0.index()
    }

    fn data(&self) -> &E {
        self.0.data()
    }

    fn src(&self) -> VertexIndex {
        self.0.dst()
    }

    fn dst(&self) -> VertexIndex {
        self.0.src()
    }
}

impl<R> NeighborRef for ReversedRef<R>
where
    R: NeighborRef,
{
    fn index(&self) -> VertexIndex {
        self.0.index()
    }

    fn edge(&self) -> EdgeIndex {
        self.0.edge()
    }

    fn src(&self) -> VertexIndex {
        self.0.src()
    }

    fn dir(&self) -> Direction {
        self.0.dir().opposite()
    }
}

pub struct Iter<I>(I);

impl<I> Iterator for Iter<I>
where
    I: Iterator,
{
    type Item = ReversedRef<I::Item>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(ReversedRef)
    }
}

#[cfg(test)]
mod tests {
    use crate::marker::{Incoming, Outgoing};
    use crate::storage::AdjList;

    use super::*;

    fn create_graph() -> AdjList<(), i32, Directed> {
        let mut graph = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(v0, v1, 0);
        graph.add_edge(v1, v2, 1);
        graph.add_edge(v2, v0, 2);
        graph.add_edge(v2, v1, 3);

        graph
    }

    #[test]
    fn endpoints() {
        let graph = Reversed::new(create_graph());

        assert_eq!(graph.endpoints(1.into()), Some((2.into(), 1.into())));
        assert_eq!(graph.endpoints(3.into()), Some((1.into(), 2.into())));
    }

    #[test]
    fn edge_index() {
        let graph = Reversed::new(create_graph());

        assert_eq!(graph.edge_index(2.into(), 1.into()), Some(1.into()));
        assert_eq!(graph.edge_index(1.into(), 2.into()), Some(3.into()));
    }

    #[test]
    fn edges() {
        let graph = Reversed::new(create_graph());
        let mut edges = graph
            .edges()
            .map(|edge| (edge.src(), edge.dst(), *edge.data()));

        assert_eq!(edges.next(), Some((1.into(), 0.into(), 0)));
        assert_eq!(edges.next(), Some((2.into(), 1.into(), 1)));
        assert_eq!(edges.next(), Some((0.into(), 2.into(), 2)));
        assert_eq!(edges.next(), Some((1.into(), 2.into(), 3)));
    }

    #[test]
    fn neighbors() {
        let graph = Reversed::new(create_graph());
        let mut neighbors = graph
            .neighbors(1.into())
            .map(|neighbor| (neighbor.index(), neighbor.src(), neighbor.dir()));

        assert_eq!(neighbors.next(), Some((2.into(), 1.into(), Incoming)));
        assert_eq!(neighbors.next(), Some((0.into(), 1.into(), Outgoing)));
        assert_eq!(neighbors.next(), Some((2.into(), 1.into(), Outgoing)));
    }

    #[test]
    fn neighbors_directed() {
        let graph = Reversed::new(create_graph());
        let mut neighbors = graph
            .neighbors_directed(1.into(), Outgoing)
            .map(|neighbor| (neighbor.index(), neighbor.src(), neighbor.dir()));

        assert_eq!(neighbors.next(), Some((0.into(), 1.into(), Outgoing)));
        assert_eq!(neighbors.next(), Some((2.into(), 1.into(), Outgoing)));

        let mut neighbors = graph
            .neighbors_directed(1.into(), Incoming)
            .map(|neighbor| (neighbor.index(), neighbor.src(), neighbor.dir()));

        assert_eq!(neighbors.next(), Some((2.into(), 1.into(), Incoming)));
    }
}
