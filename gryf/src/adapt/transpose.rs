use crate::core::{
    base::{EdgeRef, NeighborRef},
    id::IdType,
    marker::{Directed, Direction},
    props::{Stability, StableId},
    weak::WeakRef,
    EdgeSet, GraphBase, GraphFull, GraphRef, Neighbors,
};

use gryf_derive::{GraphBase, GraphMut, Guarantee, VertexSet};

#[derive(Debug, GraphBase, VertexSet, GraphMut, Guarantee)]
#[gryf_crate]
pub struct Transpose<G> {
    #[graph]
    graph: G,
}

impl<G> Transpose<G>
where
    G: GraphBase<EdgeType = Directed>,
{
    pub fn new(graph: G) -> Self {
        Self { graph }
    }

    pub fn into_inner(self) -> G {
        self.graph
    }

    pub fn apply<V, E, S: Stability>(self) -> G
    where
        G: GraphFull<V, E> + StableId<G::EdgeId, S>,
    {
        let mut graph = self.graph;

        let edges = graph
            .edges()
            .map(|edge| (edge.src().clone(), edge.id().clone(), edge.dst().clone()))
            .collect::<Vec<_>>();

        for (src, id, dst) in edges {
            let attr = graph.remove_edge(&id).unwrap();
            graph.add_edge(&dst, &src, attr);
        }

        graph
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

    fn neighbors(&self, src: &Self::VertexId) -> Self::NeighborsIter<'_> {
        Iter(self.graph.neighbors(src))
    }

    fn neighbors_directed(&self, src: &Self::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
        Iter(self.graph.neighbors_directed(src, dir.opposite()))
    }

    fn degree(&self, id: &Self::VertexId) -> usize {
        self.graph.degree(id)
    }

    fn degree_directed(&self, id: &Self::VertexId, dir: Direction) -> usize {
        self.graph.degree_directed(id, dir.opposite())
    }
}

impl<G> EdgeSet for Transpose<G>
where
    G: EdgeSet,
{
    type EdgeIdsIter<'a> = G::EdgeIdsIter<'a>
    where
        Self: 'a;

    type EdgeIdIter<'a> = G::EdgeIdIter<'a>
    where
        Self: 'a;

    fn edge_ids(&self) -> Self::EdgeIdsIter<'_> {
        self.graph.edge_ids()
    }

    fn edge_id(&self, src: &Self::VertexId, dst: &Self::VertexId) -> Self::EdgeIdIter<'_> {
        self.graph.edge_id(dst, src)
    }

    fn endpoints(&self, id: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)> {
        self.graph.endpoints(id).map(|(src, dst)| (dst, src))
    }

    fn edge_id_any(&self, src: &Self::VertexId, dst: &Self::VertexId) -> Option<Self::EdgeId> {
        self.graph.edge_id_any(dst, src)
    }
}

impl<V, E, G> GraphRef<V, E> for Transpose<G>
where
    G: GraphRef<V, E>,
{
    type VertexRef<'a> = G::VertexRef<'a>
    where
        Self: 'a,
        V: 'a;

    type VerticesIter<'a> = G::VerticesIter<'a>
    where
        Self: 'a,
        V: 'a;

    type EdgeRef<'a> = TransposeRef<G::EdgeRef<'a>>
    where
        Self: 'a,
        E: 'a;

    type EdgesIter<'a> = Iter<G::EdgesIter<'a>>
    where
        Self: 'a,
        E: 'a;

    fn vertices(&self) -> Self::VerticesIter<'_> {
        self.graph.vertices()
    }

    fn edges(&self) -> Self::EdgesIter<'_> {
        Iter(self.graph.edges())
    }

    fn vertex(&self, id: &Self::VertexId) -> Option<&V> {
        self.graph.vertex(id)
    }

    fn edge(&self, id: &Self::EdgeId) -> Option<&E> {
        self.graph.edge(id)
    }
}

pub struct TransposeRef<R>(R);

impl<R> TransposeRef<R> {
    pub fn new(inner: R) -> Self {
        Self(inner)
    }
}

impl<VId, EId, E, R> EdgeRef<VId, EId, E> for TransposeRef<R>
where
    VId: IdType,
    EId: IdType,
    R: EdgeRef<VId, EId, E>,
{
    fn id(&self) -> &EId {
        self.0.id()
    }

    fn attr(&self) -> &E {
        self.0.attr()
    }

    fn src(&self) -> &VId {
        self.0.dst()
    }

    fn dst(&self) -> &VId {
        self.0.src()
    }
}

impl<VId, EId, R> NeighborRef<VId, EId> for TransposeRef<R>
where
    VId: IdType,
    EId: IdType,
    R: NeighborRef<VId, EId>,
{
    fn id(&self) -> WeakRef<'_, VId> {
        self.0.id()
    }

    fn edge(&self) -> WeakRef<'_, EId> {
        self.0.edge()
    }

    fn src(&self) -> WeakRef<'_, VId> {
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
    use super::*;

    use crate::{
        core::{id::DefaultId, GraphAdd},
        storage::AdjList,
    };

    fn create_graph() -> AdjList<(), i32, Directed, DefaultId> {
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

        assert_eq!(graph.endpoints(&1.into()), Some((2.into(), 1.into())));
        assert_eq!(graph.endpoints(&3.into()), Some((1.into(), 2.into())));
    }

    #[test]
    fn edge_id() {
        let graph = Transpose::new(create_graph());

        assert_eq!(graph.edge_id_any(&2.into(), &1.into()), Some(1.into()));
        assert_eq!(graph.edge_id_any(&1.into(), &2.into()), Some(3.into()));
    }

    #[test]
    fn edges() {
        let graph = Transpose::new(create_graph());
        let mut edges = graph
            .edges()
            .map(|edge| (*edge.src(), *edge.dst(), *edge.attr()));

        assert_eq!(edges.next(), Some((1.into(), 0.into(), 0)));
        assert_eq!(edges.next(), Some((2.into(), 1.into(), 1)));
        assert_eq!(edges.next(), Some((0.into(), 2.into(), 2)));
        assert_eq!(edges.next(), Some((1.into(), 2.into(), 3)));
    }

    #[test]
    fn neighbors() {
        let graph = Transpose::new(create_graph());
        let mut neighbors = graph.neighbors(&1.into()).map(|neighbor| {
            (
                neighbor.id().into_owned(),
                neighbor.src().into_owned(),
                neighbor.dir(),
            )
        });

        assert_eq!(
            neighbors.next(),
            Some((2.into(), 1.into(), Direction::Incoming))
        );
        assert_eq!(
            neighbors.next(),
            Some((0.into(), 1.into(), Direction::Outgoing))
        );
        assert_eq!(
            neighbors.next(),
            Some((2.into(), 1.into(), Direction::Outgoing))
        );
    }

    #[test]
    fn neighbors_directed() {
        let graph = Transpose::new(create_graph());
        let mut neighbors = graph
            .neighbors_directed(&1.into(), Direction::Outgoing)
            .map(|neighbor| {
                (
                    neighbor.id().into_owned(),
                    neighbor.src().into_owned(),
                    neighbor.dir(),
                )
            });

        assert_eq!(
            neighbors.next(),
            Some((0.into(), 1.into(), Direction::Outgoing))
        );
        assert_eq!(
            neighbors.next(),
            Some((2.into(), 1.into(), Direction::Outgoing))
        );

        let mut neighbors = graph
            .neighbors_directed(&1.into(), Direction::Incoming)
            .map(|neighbor| {
                (
                    neighbor.id().into_owned(),
                    neighbor.src().into_owned(),
                    neighbor.dir(),
                )
            });

        assert_eq!(
            neighbors.next(),
            Some((2.into(), 1.into(), Direction::Incoming))
        );
    }
}
