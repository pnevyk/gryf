use crate::core::{
    base::{EdgeReference, NeighborReference, VertexReference},
    id::IntegerIdType,
    marker::Direction,
    EdgeSet, GraphBase, GraphRef, Neighbors, VertexSet,
};

use gryf_derive::GraphBase;

#[derive(GraphBase)]
#[gryf_crate]
pub struct Subgraph<G, S = ()>
where
    G: GraphBase,
{
    #[graph]
    graph: G,
    state: S,
    #[allow(clippy::type_complexity)]
    filter_vertex: Box<dyn Fn(&G::VertexId, &G, &S) -> bool>,
    #[allow(clippy::type_complexity)]
    filter_edge: Box<dyn Fn(&G::EdgeId, &G, &S) -> bool>,
}

impl<G> Subgraph<G>
where
    G: GraphBase,
{
    pub fn new(graph: G) -> Self {
        Self::with_state(graph, ())
    }
}

impl<G, S> Subgraph<G, S>
where
    G: GraphBase,
{
    pub fn with_state(graph: G, state: S) -> Self {
        Self {
            graph,
            state,
            filter_vertex: Box::new(|_, _, _| true),
            filter_edge: Box::new(|_, _, _| true),
        }
    }

    pub fn into_inner(self) -> G {
        self.graph
    }

    pub fn filter_vertex<F>(self, predicate: F) -> Self
    where
        F: Fn(&G::VertexId, &G, &S) -> bool + 'static,
    {
        Self {
            filter_vertex: Box::new(predicate),
            ..self
        }
    }

    pub fn filter_edge<F>(self, predicate: F) -> Self
    where
        F: Fn(&G::EdgeId, &G, &S) -> bool + 'static,
    {
        Self {
            filter_edge: Box::new(predicate),
            ..self
        }
    }

    fn check_vertex(&self, id: &G::VertexId) -> bool {
        (self.filter_vertex)(id, &self.graph, &self.state)
    }

    fn check_edge(&self, id: &G::EdgeId) -> bool {
        (self.filter_edge)(id, &self.graph, &self.state)
    }
}

impl<G, S> Neighbors for Subgraph<G, S>
where
    G: Neighbors,
{
    type NeighborRef<'a> = G::NeighborRef<'a>
    where
        Self: 'a;

    type NeighborsIter<'a> = SubgraphIter<'a, G::NeighborsIter<'a>, G::NeighborRef<'a>>
    where
        Self: 'a;

    fn neighbors_undirected(&self, from: &G::VertexId) -> Self::NeighborsIter<'_> {
        if !self.check_vertex(from) {
            panic!("vertex does not exist");
        }

        SubgraphIter::<_, G::NeighborRef<'_>>::new(
            self.graph.neighbors_undirected(from),
            |neighbor| self.check_vertex(&neighbor.id()) && self.check_edge(&neighbor.edge()),
            true,
        )
    }

    fn neighbors_directed(&self, from: &G::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
        if !self.check_vertex(from) {
            panic!("vertex does not exist");
        }

        SubgraphIter::<_, G::NeighborRef<'_>>::new(
            self.graph.neighbors_directed(from, dir),
            |neighbor| self.check_vertex(&neighbor.id()) && self.check_edge(&neighbor.edge()),
            true,
        )
    }
}

impl<G, S> VertexSet for Subgraph<G, S>
where
    G: VertexSet,
{
    type VerticesByIdIter<'a> = SubgraphIter<'a, G::VerticesByIdIter<'a>, G::VertexId>
    where
        Self: 'a;

    fn vertices_by_id(&self) -> Self::VerticesByIdIter<'_> {
        SubgraphIter::new(
            self.graph.vertices_by_id(),
            |id| self.check_vertex(id),
            true,
        )
    }

    fn vertex_bound(&self) -> usize
    where
        Self::VertexId: IntegerIdType,
    {
        self.graph.vertex_bound()
    }

    fn contains_vertex(&self, id: &Self::VertexId) -> bool {
        self.check_vertex(id) && self.graph.contains_vertex(id)
    }
}

impl<G, S> EdgeSet for Subgraph<G, S>
where
    G: EdgeSet,
{
    type EdgesByIdIter<'a> = SubgraphIter<'a, G::EdgesByIdIter<'a>, G::EdgeId>
    where
        Self: 'a;

    type EdgeIdIter<'a> = SubgraphIter<'a, G::EdgeIdIter<'a>, G::EdgeId>
    where
        Self: 'a;

    fn edges_by_id(&self) -> Self::EdgesByIdIter<'_> {
        SubgraphIter::new(self.graph.edges_by_id(), |id| self.contains_edge(id), true)
    }

    fn edge_id(&self, from: &Self::VertexId, to: &Self::VertexId) -> Self::EdgeIdIter<'_> {
        let endpoints_exist = self.check_vertex(from) && self.check_vertex(to);

        SubgraphIter::new(
            self.graph.edge_id(from, to),
            |id| self.contains_edge(id),
            endpoints_exist,
        )
    }

    fn endpoints(&self, id: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)> {
        if self.check_edge(id) {
            match self.graph.endpoints(id) {
                Some((from, to)) if self.check_vertex(&from) && self.check_vertex(&to) => {
                    Some((from, to))
                }
                _ => None,
            }
        } else {
            None
        }
    }

    fn edge_bound(&self) -> usize
    where
        Self::EdgeId: IntegerIdType,
    {
        self.graph.edge_bound()
    }

    fn contains_edge(&self, id: &Self::EdgeId) -> bool {
        self.endpoints(id).is_some()
    }
}

impl<V, E, G, S> GraphRef<V, E> for Subgraph<G, S>
where
    G: GraphRef<V, E>,
{
    type VertexRef<'a> = G::VertexRef<'a>
    where
        Self: 'a,
        V: 'a;

    type VerticesIter<'a> = SubgraphIter<'a, G::VerticesIter<'a>, G::VertexRef<'a>>
    where
        Self: 'a,
        V: 'a;

    type EdgeRef<'a> = G::EdgeRef<'a>
        where
            Self: 'a,
            E: 'a;

    type EdgesIter<'a> = SubgraphIter<'a, G::EdgesIter<'a>, G::EdgeRef<'a>>
        where
            Self: 'a,
            E: 'a;

    fn vertices(&self) -> Self::VerticesIter<'_> {
        SubgraphIter::<_, G::VertexRef<'_>>::new(
            self.graph.vertices(),
            |vertex| self.check_vertex(vertex.id()),
            true,
        )
    }

    fn edges(&self) -> Self::EdgesIter<'_> {
        SubgraphIter::<_, G::EdgeRef<'_>>::new(
            self.graph.edges(),
            |edge| self.contains_edge(edge.id()),
            true,
        )
    }

    fn vertex(&self, id: &Self::VertexId) -> Option<&V> {
        if self.check_vertex(id) {
            self.graph.vertex(id)
        } else {
            None
        }
    }

    fn edge(&self, id: &Self::EdgeId) -> Option<&E> {
        if self.contains_edge(id) {
            self.graph.edge(id)
        } else {
            None
        }
    }
}

pub struct SubgraphIter<'a, I, T> {
    inner: I,
    #[allow(clippy::type_complexity)]
    filter: Box<dyn Fn(&T) -> bool + 'a>,
    non_empty: bool,
}

impl<'a, I, T> SubgraphIter<'a, I, T> {
    fn new<F>(inner: I, filter: F, non_empty: bool) -> Self
    where
        F: Fn(&T) -> bool + 'a,
    {
        Self {
            inner,
            filter: Box::new(filter),
            non_empty,
        }
    }
}

impl<'a, I, T> Iterator for SubgraphIter<'a, I, T>
where
    I: Iterator<Item = T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.non_empty {
            self.inner.find(&self.filter)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        core::{
            id::{DefaultId, EdgeId, IdType, VertexId},
            marker::Directed,
            GraphAdd,
        },
        storage::AdjList,
    };

    use super::*;

    fn create_subgraph() -> Subgraph<AdjList<i32, i32, Directed, DefaultId>> {
        // digraph G {
        //     v0 [label="0"];
        //     v1 [label="1"];
        //     v2 [label="2"];
        //     v3 [label="3"];
        //     v4 [label="4", color="red"];
        //     v5 [label="5", color="red"];
        //     v0 -> v1 [label="0"];
        //     v0 -> v2 [label="1"];
        //     v0 -> v3 [label="2"];
        //     v1 -> v2 [label="3"];
        //     v1 -> v3 [label="4", color="red"];
        //     v1 -> v4 [label="5", color="red"];
        //     v1 -> v5 [label="6"];
        //     v2 -> v4 [label="7", color="red"];
        //     v2 -> v5 [label="8"];
        //     v3 -> v4 [label="9", color="red"];
        //     v4 -> v5 [label="10"];
        // }

        let mut graph = AdjList::new();

        let v0 = graph.add_vertex(0);
        let v1 = graph.add_vertex(1);
        let v2 = graph.add_vertex(2);
        let v3 = graph.add_vertex(3);
        let v4 = graph.add_vertex(4);
        let v5 = graph.add_vertex(5);

        graph.add_edge(&v0, &v1, 0);
        graph.add_edge(&v0, &v2, 1);
        graph.add_edge(&v0, &v3, 2);
        graph.add_edge(&v1, &v2, 3);
        graph.add_edge(&v1, &v3, 4);
        graph.add_edge(&v1, &v4, 5);
        graph.add_edge(&v1, &v5, 6);
        graph.add_edge(&v2, &v4, 7);
        graph.add_edge(&v2, &v5, 8);
        graph.add_edge(&v3, &v4, 9);
        graph.add_edge(&v4, &v5, 10);

        Subgraph::new(graph)
            .filter_vertex(|v: &VertexId, _, _| v.as_usize() < 4)
            .filter_edge(|e, g, _| {
                let (from, to): (VertexId, VertexId) = g.endpoints(e).unwrap();
                from.as_usize() + to.as_usize() < 4 || to.as_usize() == 5
            })
    }

    fn v(id: usize) -> VertexId {
        id.into()
    }

    fn e(id: usize) -> EdgeId {
        id.into()
    }

    #[test]
    fn subgraph_vertex_count() {
        let subgraph = create_subgraph();
        assert_eq!(subgraph.vertex_count(), 4);
    }

    #[test]
    fn subgraph_vertex_bound() {
        let subgraph = create_subgraph();
        assert_eq!(subgraph.vertex_bound(), 6);
    }

    #[test]
    fn subgraph_vertices_by_id() {
        let subgraph = create_subgraph();

        let mut vertices_by_id = subgraph.vertices_by_id().collect::<Vec<_>>();
        vertices_by_id.sort_unstable();
        assert_eq!(&vertices_by_id, &[v(0), v(1), v(2), v(3)]);
    }

    #[test]
    fn subgraph_contains_vertex() {
        let subgraph = create_subgraph();

        assert!(subgraph.contains_vertex(&v(0)));
        assert!(!subgraph.contains_vertex(&v(5)));
    }

    #[test]
    fn subgraph_vertex() {
        let subgraph = create_subgraph();

        assert!(subgraph.vertex(&v(0)).is_some());
        assert!(subgraph.vertex(&v(5)).is_none());
    }

    #[test]
    fn subgraph_vertices() {
        let subgraph = create_subgraph();

        let mut vertices_by_id = subgraph.vertices().map(|v| *v.id()).collect::<Vec<_>>();
        vertices_by_id.sort_unstable();
        assert_eq!(&vertices_by_id, &[v(0), v(1), v(2), v(3)]);
    }

    #[test]
    fn subgraph_find_vertex() {
        let subgraph = create_subgraph();

        assert!(subgraph.find_vertex(&0).is_some());
        assert!(subgraph.find_vertex(&5).is_none());
    }

    #[test]
    fn subgraph_edge_count() {
        let subgraph = create_subgraph();
        assert_eq!(subgraph.edge_count(), 4);
    }

    #[test]
    fn subgraph_edge_bound() {
        let subgraph = create_subgraph();
        assert_eq!(subgraph.edge_bound(), 11);
    }

    #[test]
    fn subgraph_endpoints() {
        let subgraph = create_subgraph();

        assert!(subgraph.endpoints(&e(0)).is_some());
        assert!(subgraph.endpoints(&e(4)).is_none());
        assert!(subgraph.endpoints(&e(10)).is_none());
    }

    #[test]
    fn subgraph_edge_id_any() {
        let subgraph = create_subgraph();

        assert!(subgraph.edge_id_any(&v(1), &v(2)).is_some());
        assert!(subgraph.edge_id_any(&v(1), &v(3)).is_none());
        assert!(subgraph.edge_id_any(&v(1), &v(5)).is_none());
    }

    #[test]
    fn subgraph_edges_by_id() {
        let subgraph = create_subgraph();

        let mut edges_by_id = subgraph.edges_by_id().collect::<Vec<_>>();
        edges_by_id.sort_unstable();
        assert_eq!(&edges_by_id, &[e(0), e(1), e(2), e(3)]);
    }

    #[test]
    fn subgraph_contains_edge() {
        let subgraph = create_subgraph();

        assert!(subgraph.contains_edge(&e(0)));
        assert!(!subgraph.contains_edge(&e(4)));
        assert!(!subgraph.contains_edge(&e(10)));
    }

    #[test]
    fn subgraph_edge() {
        let subgraph = create_subgraph();

        assert!(subgraph.edge(&e(0)).is_some());
        assert!(subgraph.edge(&e(4)).is_none());
        assert!(subgraph.edge(&e(10)).is_none());
    }

    #[test]
    fn subgraph_edges() {
        let subgraph = create_subgraph();

        let mut edges_by_id = subgraph.edges().map(|e| *e.id()).collect::<Vec<_>>();
        edges_by_id.sort_unstable();
        assert_eq!(&edges_by_id, &[e(0), e(1), e(2), e(3)]);
    }

    #[test]
    fn subgraph_neighbors() {
        let subgraph = create_subgraph();

        let mut edges_by_id = subgraph
            .neighbors_undirected(&v(1))
            .map(|e| e.edge().into_owned())
            .collect::<Vec<_>>();
        edges_by_id.sort_unstable();
        assert_eq!(&edges_by_id, &[e(0), e(3)]);
    }

    #[test]
    fn subgraph_neighbors_directed() {
        let subgraph = create_subgraph();

        let mut edges_by_id = subgraph
            .neighbors_directed(&v(3), Direction::Incoming)
            .map(|e| e.edge().into_owned())
            .collect::<Vec<_>>();
        edges_by_id.sort_unstable();
        assert_eq!(&edges_by_id, &[e(2)]);

        let mut edges_by_id = subgraph
            .neighbors_directed(&v(1), Direction::Outgoing)
            .map(|e| e.edge().into_owned())
            .collect::<Vec<_>>();
        edges_by_id.sort_unstable();
        assert_eq!(&edges_by_id, &[e(3)]);
    }
}
