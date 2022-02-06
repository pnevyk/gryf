use super::OpOwned;
use crate::index::{EdgeIndex, VertexIndex};
use crate::infra::CompactIndexMap;
use crate::marker::{Directed, Direction, EdgeType};
use crate::traits::*;
use crate::{
    EdgesBaseWeak, EdgesWeak, Guarantee, Vertices, VerticesBase, VerticesBaseWeak, VerticesMut,
    VerticesWeak,
};

#[derive(
    Debug,
    VerticesBase,
    Vertices,
    VerticesMut,
    VerticesBaseWeak,
    VerticesWeak,
    EdgesBaseWeak,
    EdgesWeak,
    Guarantee,
)]
pub struct Transpose<G> {
    #[graph]
    graph: G,
}

impl<G> Transpose<G>
where
    G: VerticesBase + EdgesBase<Directed>,
{
    pub fn new(graph: G) -> Self {
        Self { graph }
    }

    pub fn into_unmodified(self) -> G {
        self.graph
    }
}

impl<E, G> OpOwned<G, E> for Transpose<G>
where
    G: EdgesMut<E, Directed> + StableIndices,
{
    fn apply(self) -> G {
        let mut graph = self.graph;

        let edges = graph
            .edges()
            .map(|edge| (edge.src(), edge.index(), edge.dst()))
            .collect::<Vec<_>>();

        for (src, index, dst) in edges {
            let data = graph.remove_edge(index).unwrap();
            graph.add_edge(dst, src, data);
        }

        graph
    }
}

impl<G> EdgesBase<Directed> for Transpose<G>
where
    G: EdgesBase<Directed>,
{
    type EdgeIndicesIter<'a>
    where
        Self: 'a,
    = G::EdgeIndicesIter<'a>;

    fn edge_count(&self) -> usize {
        self.graph.edge_count()
    }

    fn edge_bound(&self) -> usize {
        self.graph.edge_bound()
    }

    fn endpoints(&self, index: EdgeIndex) -> Option<(VertexIndex, VertexIndex)> {
        self.graph.endpoints(index).map(|(src, dst)| (dst, src))
    }

    fn edge_index(&self, src: VertexIndex, dst: VertexIndex) -> Option<EdgeIndex> {
        self.graph.edge_index(dst, src)
    }

    fn edge_indices(&self) -> Self::EdgeIndicesIter<'_> {
        self.graph.edge_indices()
    }

    fn contains_edge(&self, index: EdgeIndex) -> bool {
        self.graph.contains_edge(index)
    }

    fn edge_index_map(&self) -> CompactIndexMap<EdgeIndex> {
        self.graph.edge_index_map()
    }
}

impl<E, G> Edges<E, Directed> for Transpose<G>
where
    G: Edges<E, Directed>,
{
    type EdgeRef<'a, T: 'a> = TransposeRef<G::EdgeRef<'a, T>>;

    type EdgesIter<'a, T: 'a>
    where
        Self: 'a,
    = Iter<G::EdgesIter<'a, T>>;

    fn edge(&self, index: EdgeIndex) -> Option<&E> {
        self.graph.edge(index)
    }

    fn edges(&self) -> Self::EdgesIter<'_, E> {
        Iter(self.graph.edges())
    }
}

impl<G> Neighbors for Transpose<G>
where
    G: Neighbors,
{
    type NeighborRef<'a> = TransposeRef<G::NeighborRef<'a>>;

    type NeighborsIter<'a>
    where
        Self: 'a,
    = Iter<G::NeighborsIter<'a>>;

    fn neighbors(&self, src: VertexIndex) -> Self::NeighborsIter<'_> {
        Iter(self.graph.neighbors(src))
    }

    fn neighbors_directed(&self, src: VertexIndex, dir: Direction) -> Self::NeighborsIter<'_> {
        Iter(self.graph.neighbors_directed(src, dir.opposite()))
    }

    fn degree(&self, index: VertexIndex) -> usize {
        self.graph.degree(index)
    }

    fn degree_directed(&self, index: VertexIndex, dir: Direction) -> usize {
        self.graph.degree_directed(index, dir.opposite())
    }
}

pub struct TransposeRef<R>(R);

impl<E, R> EdgeRef<E> for TransposeRef<R>
where
    R: EdgeRef<E>,
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

impl<R> NeighborRef for TransposeRef<R>
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
    type Item = TransposeRef<I::Item>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(TransposeRef)
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
        let graph = Transpose::new(create_graph());

        assert_eq!(graph.endpoints(1.into()), Some((2.into(), 1.into())));
        assert_eq!(graph.endpoints(3.into()), Some((1.into(), 2.into())));
    }

    #[test]
    fn edge_index() {
        let graph = Transpose::new(create_graph());

        assert_eq!(graph.edge_index(2.into(), 1.into()), Some(1.into()));
        assert_eq!(graph.edge_index(1.into(), 2.into()), Some(3.into()));
    }

    #[test]
    fn edges() {
        let graph = Transpose::new(create_graph());
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
        let graph = Transpose::new(create_graph());
        let mut neighbors = graph
            .neighbors(1.into())
            .map(|neighbor| (neighbor.index(), neighbor.src(), neighbor.dir()));

        assert_eq!(neighbors.next(), Some((2.into(), 1.into(), Incoming)));
        assert_eq!(neighbors.next(), Some((0.into(), 1.into(), Outgoing)));
        assert_eq!(neighbors.next(), Some((2.into(), 1.into(), Outgoing)));
    }

    #[test]
    fn neighbors_directed() {
        let graph = Transpose::new(create_graph());
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
