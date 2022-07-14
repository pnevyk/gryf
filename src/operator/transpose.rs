use super::OpOwned;
use crate::index::{Indexing, NumIndexType};
use crate::infra::CompactIndexMap;
use crate::marker::{Directed, Direction, EdgeType};
use crate::traits::*;
use crate::{
    EdgesBaseWeak, EdgesWeak, GraphBase, Guarantee, Vertices, VerticesBase, VerticesBaseWeak,
    VerticesMut, VerticesWeak,
};

#[derive(
    Debug,
    GraphBase,
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

impl<E, G, S: Stability> OpOwned<G, (E, S)> for Transpose<G>
where
    G: EdgesMut<E, Directed> + StableIndices<G::EdgeIndex, S>,
{
    fn apply(self) -> G {
        let mut graph = self.graph;

        let edges = graph
            .edges()
            .map(|edge| (edge.src().clone(), edge.index().clone(), edge.dst().clone()))
            .collect::<Vec<_>>();

        for (src, index, dst) in edges {
            let data = graph.remove_edge(&index).unwrap();
            graph.add_edge(&dst, &src, data);
        }

        graph
    }
}

impl<G> EdgesBase<Directed> for Transpose<G>
where
    G: EdgesBase<Directed>,
{
    type EdgeIndicesIter<'a> = G::EdgeIndicesIter<'a>
    where
        Self: 'a;

    fn edge_count(&self) -> usize {
        self.graph.edge_count()
    }

    fn edge_bound(&self) -> usize {
        self.graph.edge_bound()
    }

    fn endpoints(&self, index: &Self::EdgeIndex) -> Option<(Self::VertexIndex, Self::VertexIndex)> {
        self.graph.endpoints(index).map(|(src, dst)| (dst, src))
    }

    fn edge_index(
        &self,
        src: &Self::VertexIndex,
        dst: &Self::VertexIndex,
    ) -> Option<Self::EdgeIndex> {
        self.graph.edge_index(dst, src)
    }

    fn edge_indices(&self) -> Self::EdgeIndicesIter<'_> {
        self.graph.edge_indices()
    }

    fn contains_edge(&self, index: &Self::EdgeIndex) -> bool {
        self.graph.contains_edge(index)
    }

    fn edge_index_map(&self) -> CompactIndexMap<G::EdgeIndex>
    where
        Self::EdgeIndex: NumIndexType,
    {
        self.graph.edge_index_map()
    }
}

impl<E, G> Edges<E, Directed> for Transpose<G>
where
    G: Edges<E, Directed>,
{
    type EdgeRef<'a> = TransposeRef<G::EdgeRef<'a>>
    where
        Self: 'a,
        E: 'a;

    type EdgesIter<'a> = Iter<G::EdgesIter<'a>>
    where
        Self: 'a,
        E: 'a;

    fn edge(&self, index: &Self::EdgeIndex) -> Option<&E> {
        self.graph.edge(index)
    }

    fn edges(&self) -> Self::EdgesIter<'_> {
        Iter(self.graph.edges())
    }
}

impl<G> Neighbors for Transpose<G>
where
    G: Neighbors,
{
    type NeighborRef<'a> = TransposeRef<G::NeighborRef<'a>>
    where
        Self: 'a;

    type NeighborsIter<'a> = Iter<G::NeighborsIter<'a>>
    where
        Self: 'a;

    fn neighbors(&self, src: &Self::VertexIndex) -> Self::NeighborsIter<'_> {
        Iter(self.graph.neighbors(src))
    }

    fn neighbors_directed(
        &self,
        src: &Self::VertexIndex,
        dir: Direction,
    ) -> Self::NeighborsIter<'_> {
        Iter(self.graph.neighbors_directed(src, dir.opposite()))
    }

    fn degree(&self, index: &Self::VertexIndex) -> usize {
        self.graph.degree(index)
    }

    fn degree_directed(&self, index: &Self::VertexIndex, dir: Direction) -> usize {
        self.graph.degree_directed(index, dir.opposite())
    }
}

pub struct TransposeRef<R>(R);

impl<Ix, E, R> EdgeRef<Ix, E> for TransposeRef<R>
where
    Ix: Indexing,
    R: EdgeRef<Ix, E>,
{
    fn index(&self) -> &Ix::EdgeIndex {
        self.0.index()
    }

    fn data(&self) -> &E {
        self.0.data()
    }

    fn src(&self) -> &Ix::VertexIndex {
        self.0.dst()
    }

    fn dst(&self) -> &Ix::VertexIndex {
        self.0.src()
    }
}

impl<Ix, R> NeighborRef<Ix> for TransposeRef<R>
where
    Ix: Indexing,
    R: NeighborRef<Ix>,
{
    fn index(&self) -> WeakRef<'_, Ix::VertexIndex> {
        self.0.index()
    }

    fn edge(&self) -> WeakRef<'_, Ix::EdgeIndex> {
        self.0.edge()
    }

    fn src(&self) -> WeakRef<'_, Ix::VertexIndex> {
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
    use crate::index::DefaultIndexing;
    use crate::marker::{Incoming, Outgoing};
    use crate::storage::AdjList;

    use super::*;

    fn create_graph() -> AdjList<(), i32, Directed, DefaultIndexing> {
        let mut graph = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, 0);
        graph.add_edge(&v1, &v2, 1);
        graph.add_edge(&v2, &v0, 2);
        graph.add_edge(&v2, &v1, 3);

        graph
    }

    #[test]
    fn endpoints() {
        let graph = Transpose::new(create_graph());

        assert_eq!(
            graph.endpoints(&1usize.into()),
            Some((2usize.into(), 1usize.into()))
        );
        assert_eq!(
            graph.endpoints(&3usize.into()),
            Some((1usize.into(), 2usize.into()))
        );
    }

    #[test]
    fn edge_index() {
        let graph = Transpose::new(create_graph());

        assert_eq!(
            graph.edge_index(&2usize.into(), &1usize.into()),
            Some(1usize.into())
        );
        assert_eq!(
            graph.edge_index(&1usize.into(), &2usize.into()),
            Some(3usize.into())
        );
    }

    #[test]
    fn edges() {
        let graph = Transpose::new(create_graph());
        let mut edges = graph.edges().map(|edge| {
            (
                *EdgeRef::<DefaultIndexing, _>::src(&edge),
                *EdgeRef::<DefaultIndexing, _>::dst(&edge),
                *EdgeRef::<DefaultIndexing, _>::data(&edge),
            )
        });

        assert_eq!(edges.next(), Some((1usize.into(), 0usize.into(), 0)));
        assert_eq!(edges.next(), Some((2usize.into(), 1usize.into(), 1)));
        assert_eq!(edges.next(), Some((0usize.into(), 2usize.into(), 2)));
        assert_eq!(edges.next(), Some((1usize.into(), 2usize.into(), 3)));
    }

    #[test]
    fn neighbors() {
        let graph = Transpose::new(create_graph());
        let mut neighbors = graph.neighbors(&1usize.into()).map(|neighbor| {
            (
                NeighborRef::<DefaultIndexing>::index(&neighbor).into_owned(),
                NeighborRef::<DefaultIndexing>::src(&neighbor).into_owned(),
                NeighborRef::<DefaultIndexing>::dir(&neighbor),
            )
        });

        assert_eq!(
            neighbors.next(),
            Some((2usize.into(), 1usize.into(), Incoming))
        );
        assert_eq!(
            neighbors.next(),
            Some((0usize.into(), 1usize.into(), Outgoing))
        );
        assert_eq!(
            neighbors.next(),
            Some((2usize.into(), 1usize.into(), Outgoing))
        );
    }

    #[test]
    fn neighbors_directed() {
        let graph = Transpose::new(create_graph());
        let mut neighbors = graph
            .neighbors_directed(&1usize.into(), Outgoing)
            .map(|neighbor| {
                (
                    NeighborRef::<DefaultIndexing>::index(&neighbor).into_owned(),
                    NeighborRef::<DefaultIndexing>::src(&neighbor).into_owned(),
                    NeighborRef::<DefaultIndexing>::dir(&neighbor),
                )
            });

        assert_eq!(
            neighbors.next(),
            Some((0usize.into(), 1usize.into(), Outgoing))
        );
        assert_eq!(
            neighbors.next(),
            Some((2usize.into(), 1usize.into(), Outgoing))
        );

        let mut neighbors = graph
            .neighbors_directed(&1usize.into(), Incoming)
            .map(|neighbor| {
                (
                    NeighborRef::<DefaultIndexing>::index(&neighbor).into_owned(),
                    NeighborRef::<DefaultIndexing>::src(&neighbor).into_owned(),
                    NeighborRef::<DefaultIndexing>::dir(&neighbor),
                )
            });

        assert_eq!(
            neighbors.next(),
            Some((2usize.into(), 1usize.into(), Incoming))
        );
    }
}
