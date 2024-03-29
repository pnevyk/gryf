use crate::core::{
    marker::{Direction, EdgeType},
    EdgeRef, Edges, EdgesBase, GraphBase, NeighborRef, Neighbors, VertexRef, Vertices,
    VerticesBase,
};

use gryf_derive::{EdgesBaseWeak, EdgesWeak, GraphBase, VerticesBaseWeak, VerticesWeak};

// TODO: Remove these imports once hygiene of procedural macros is fixed.
use crate::core::{EdgesBaseWeak, EdgesWeak, VerticesBaseWeak, VerticesWeak, WeakRef};

#[derive(GraphBase, VerticesBaseWeak, VerticesWeak, EdgesBaseWeak, EdgesWeak)]
pub struct Subgraph<G, S = ()>
where
    G: GraphBase,
{
    #[graph]
    graph: G,
    state: S,
    #[allow(clippy::type_complexity)]
    filter_vertex: Box<dyn Fn(&G::VertexIndex, &G, &S) -> bool>,
    #[allow(clippy::type_complexity)]
    filter_edge: Box<dyn Fn(&G::EdgeIndex, &G, &S) -> bool>,
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
        F: Fn(&G::VertexIndex, &G, &S) -> bool + 'static,
    {
        Self {
            filter_vertex: Box::new(predicate),
            ..self
        }
    }

    pub fn filter_edge<F>(self, predicate: F) -> Self
    where
        F: Fn(&G::EdgeIndex, &G, &S) -> bool + 'static,
    {
        Self {
            filter_edge: Box::new(predicate),
            ..self
        }
    }

    fn check_vertex(&self, index: &G::VertexIndex) -> bool {
        (self.filter_vertex)(index, &self.graph, &self.state)
    }

    fn check_edge(&self, index: &G::EdgeIndex) -> bool {
        (self.filter_edge)(index, &self.graph, &self.state)
    }
}

impl<G, S> VerticesBase for Subgraph<G, S>
where
    G: VerticesBase,
{
    type VertexIndicesIter<'a> = SubsetIter<'a, G::VertexIndicesIter<'a>, G::VertexIndex>
    where
        Self: 'a;

    fn vertex_count(&self) -> usize {
        self.vertex_indices().count()
    }

    fn vertex_bound(&self) -> usize {
        self.graph.vertex_bound()
    }

    fn vertex_indices(&self) -> Self::VertexIndicesIter<'_> {
        SubsetIter::new(
            self.graph.vertex_indices(),
            |index| self.check_vertex(index),
            true,
        )
    }

    fn contains_vertex(&self, index: &Self::VertexIndex) -> bool {
        self.check_vertex(index) && self.graph.contains_vertex(index)
    }
}

impl<V, G, S> Vertices<V> for Subgraph<G, S>
where
    G: Vertices<V>,
{
    type VertexRef<'a> = G::VertexRef<'a>
    where
        Self: 'a,
        V: 'a;

    type VerticesIter<'a> = SubsetIter<'a, G::VerticesIter<'a>, G::VertexRef<'a>>
    where
        Self: 'a,
        V: 'a;

    fn vertex(&self, index: &G::VertexIndex) -> Option<&V> {
        if self.check_vertex(index) {
            self.graph.vertex(index)
        } else {
            None
        }
    }

    fn vertices(&self) -> Self::VerticesIter<'_> {
        SubsetIter::<_, G::VertexRef<'_>>::new(
            self.graph.vertices(),
            |vertex| self.check_vertex(vertex.index()),
            true,
        )
    }

    fn find_vertex(&self, vertex: &V) -> Option<Self::VertexIndex>
    where
        V: Eq,
    {
        match self.graph.find_vertex(vertex) {
            Some(index) if self.check_vertex(&index) => Some(index),
            _ => None,
        }
    }
}

impl<Ty: EdgeType, G, S> EdgesBase<Ty> for Subgraph<G, S>
where
    G: EdgesBase<Ty>,
{
    type EdgeIndicesIter<'a> = SubsetIter<'a, G::EdgeIndicesIter<'a>, G::EdgeIndex>
    where
        Self: 'a;
    type EdgeIndexIter<'a> = SubsetIter<'a, G::EdgeIndexIter<'a>, G::EdgeIndex>
    where
        Self: 'a;

    fn edge_count(&self) -> usize {
        self.edge_indices().count()
    }

    fn edge_bound(&self) -> usize {
        self.graph.edge_bound()
    }

    fn endpoints(&self, index: &G::EdgeIndex) -> Option<(G::VertexIndex, G::VertexIndex)> {
        if self.check_edge(index) {
            match self.graph.endpoints(index) {
                Some((src, dst)) if self.check_vertex(&src) && self.check_vertex(&dst) => {
                    Some((src, dst))
                }
                _ => None,
            }
        } else {
            None
        }
    }

    fn edge_index(&self, src: &G::VertexIndex, dst: &G::VertexIndex) -> Self::EdgeIndexIter<'_> {
        let endpoints_exist = self.check_vertex(src) && self.check_vertex(dst);

        SubsetIter::new(
            self.graph.edge_index(src, dst),
            |index| self.contains_edge(index),
            endpoints_exist,
        )
    }

    fn edge_index_any(
        &self,
        src: &Self::VertexIndex,
        dst: &Self::VertexIndex,
    ) -> Option<Self::EdgeIndex> {
        self.edge_index(src, dst).next()
    }

    fn edge_indices(&self) -> Self::EdgeIndicesIter<'_> {
        SubsetIter::new(
            self.graph.edge_indices(),
            |index| self.contains_edge(index),
            true,
        )
    }

    fn contains_edge(&self, index: &G::EdgeIndex) -> bool {
        self.endpoints(index).is_some()
    }
}

impl<E, Ty: EdgeType, G, S> Edges<E, Ty> for Subgraph<G, S>
where
    G: Edges<E, Ty>,
{
    type EdgeRef<'a> = G::EdgeRef<'a>
    where
        Self: 'a,
        E: 'a;

    type EdgesIter<'a> = SubsetIter<'a, G::EdgesIter<'a>, G::EdgeRef<'a>>
    where
        Self: 'a,
        E: 'a;

    fn edge(&self, index: &G::EdgeIndex) -> Option<&E> {
        if self.contains_edge(index) {
            self.graph.edge(index)
        } else {
            None
        }
    }

    fn edges(&self) -> Self::EdgesIter<'_> {
        SubsetIter::<_, G::EdgeRef<'_>>::new(
            self.graph.edges(),
            |edge| self.contains_edge(edge.index()),
            true,
        )
    }
}

impl<G, S> Neighbors for Subgraph<G, S>
where
    G: Neighbors,
{
    type NeighborRef<'a> = G::NeighborRef<'a>
    where
        Self: 'a;

    type NeighborsIter<'a> = SubsetIter<'a, G::NeighborsIter<'a>, G::NeighborRef<'a>>
    where
        Self: 'a;

    fn neighbors(&self, src: &G::VertexIndex) -> Self::NeighborsIter<'_> {
        if !self.check_vertex(src) {
            panic!("vertex does not exist");
        }

        SubsetIter::<_, G::NeighborRef<'_>>::new(
            self.graph.neighbors(src),
            |neighbor| self.check_vertex(&neighbor.index()) && self.check_edge(&neighbor.edge()),
            true,
        )
    }

    fn neighbors_directed(&self, src: &G::VertexIndex, dir: Direction) -> Self::NeighborsIter<'_> {
        if !self.check_vertex(src) {
            panic!("vertex does not exist");
        }

        SubsetIter::<_, G::NeighborRef<'_>>::new(
            self.graph.neighbors_directed(src, dir),
            |neighbor| self.check_vertex(&neighbor.index()) && self.check_edge(&neighbor.edge()),
            true,
        )
    }
}

pub struct SubsetIter<'a, I, T> {
    inner: I,
    #[allow(clippy::type_complexity)]
    filter: Box<dyn Fn(&T) -> bool + 'a>,
    non_empty: bool,
}

impl<'a, I, T> SubsetIter<'a, I, T> {
    pub fn new<F>(inner: I, filter: F, non_empty: bool) -> Self
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

impl<'a, I, T> Iterator for SubsetIter<'a, I, T>
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
            index::{DefaultIndexing, EdgeIndex, NumIndexType, VertexIndex},
            marker::Directed,
            EdgesMut, VerticesMut,
        },
        storage::AdjList,
    };

    use super::*;

    fn create_subgraph() -> Subgraph<AdjList<i32, i32, Directed, DefaultIndexing>> {
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
            .filter_vertex(|v: &VertexIndex, _, _| v.to_usize() < 4)
            .filter_edge(|e, g, _| {
                let (src, dst): (VertexIndex, VertexIndex) = g.endpoints(e).unwrap();
                src.to_usize() + dst.to_usize() < 4 || dst.to_usize() == 5
            })
    }

    fn v(index: usize) -> VertexIndex {
        index.into()
    }

    fn e(index: usize) -> EdgeIndex {
        index.into()
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
    fn subgraph_vertex_indices() {
        let subgraph = create_subgraph();

        let mut vertex_indices = subgraph.vertex_indices().collect::<Vec<_>>();
        vertex_indices.sort_unstable();
        assert_eq!(&vertex_indices, &[v(0), v(1), v(2), v(3)]);
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

        let mut vertex_indices = subgraph.vertices().map(|v| *v.index()).collect::<Vec<_>>();
        vertex_indices.sort_unstable();
        assert_eq!(&vertex_indices, &[v(0), v(1), v(2), v(3)]);
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
    fn subgraph_edge_index_any() {
        let subgraph = create_subgraph();

        assert!(subgraph.edge_index_any(&v(1), &v(2)).is_some());
        assert!(subgraph.edge_index_any(&v(1), &v(3)).is_none());
        assert!(subgraph.edge_index_any(&v(1), &v(5)).is_none());
    }

    #[test]
    fn subgraph_edge_indices() {
        let subgraph = create_subgraph();

        let mut edge_indices = subgraph.edge_indices().collect::<Vec<_>>();
        edge_indices.sort_unstable();
        assert_eq!(&edge_indices, &[e(0), e(1), e(2), e(3)]);
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

        let mut edge_indices = subgraph.edges().map(|e| *e.index()).collect::<Vec<_>>();
        edge_indices.sort_unstable();
        assert_eq!(&edge_indices, &[e(0), e(1), e(2), e(3)]);
    }

    #[test]
    fn subgraph_neighbors() {
        let subgraph = create_subgraph();

        let mut edge_indices = subgraph
            .neighbors(&v(1))
            .map(|e| e.edge().into_owned())
            .collect::<Vec<_>>();
        edge_indices.sort_unstable();
        assert_eq!(&edge_indices, &[e(0), e(3)]);
    }

    #[test]
    fn subgraph_neighbors_directed() {
        let subgraph = create_subgraph();

        let mut edge_indices = subgraph
            .neighbors_directed(&v(3), Direction::Incoming)
            .map(|e| e.edge().into_owned())
            .collect::<Vec<_>>();
        edge_indices.sort_unstable();
        assert_eq!(&edge_indices, &[e(2)]);

        let mut edge_indices = subgraph
            .neighbors_directed(&v(1), Direction::Outgoing)
            .map(|e| e.edge().into_owned())
            .collect::<Vec<_>>();
        edge_indices.sort_unstable();
        assert_eq!(&edge_indices, &[e(3)]);
    }
}
