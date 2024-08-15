use std::collections::BTreeSet;

use crate::core::{
    base::{EdgeReference, NeighborReference, VertexReference},
    connect::ConnectVertices,
    create::Create,
    error::{AddEdgeError, AddEdgeErrorKind, AddVertexError},
    id::{CompactIdMap, EdgeId, IntegerIdType, VertexId},
    marker::Direction,
    props::{NoReplace, StableId},
    EdgeSet, GraphAdd, GraphBase, GraphFull, GraphMut, GraphRef, Neighbors, VertexSet,
};

use gryf_derive::{GraphBase, Guarantee};

#[derive(Debug, GraphBase, Guarantee)]
#[gryf_crate]
pub struct Stable<G: GraphBase> {
    #[graph]
    inner: G,
    removed_vertices: BTreeSet<G::VertexId>,
    removed_edges: BTreeSet<G::EdgeId>,
}

impl<G> Stable<G>
where
    G: GraphBase,
{
    pub fn new(inner: G) -> Self {
        Self {
            inner,
            removed_vertices: BTreeSet::default(),
            removed_edges: BTreeSet::default(),
        }
    }

    pub fn apply<V, E>(self) -> G
    where
        G: GraphFull<V, E>,
    {
        let mut inner = self.inner;

        // Propagate the removals. The descending order of ids is important for
        // not invalidating the stored ids when creating "holes" in the
        // underlying graph.
        for edge in self.removed_edges.iter().rev() {
            let removed = inner.remove_edge(edge);
            debug_assert!(removed.is_some());
        }

        for vertex in self.removed_vertices.iter().rev() {
            let removed = inner.remove_vertex(vertex);
            debug_assert!(removed.is_some());
        }

        inner
    }
}

impl<G> Default for Stable<G>
where
    G: Default,
    G: GraphBase,
{
    fn default() -> Self {
        Self::new(G::default())
    }
}

impl<G> From<G> for Stable<G>
where
    G: GraphBase,
{
    fn from(inner: G) -> Self {
        Self::new(inner)
    }
}

impl<G> Neighbors for Stable<G>
where
    G: Neighbors,
{
    type NeighborRef<'a> = G::NeighborRef<'a>
    where
        Self: 'a;

    type NeighborsIter<'a> = NeighborsIter<'a, G>
    where
        Self: 'a;

    fn neighbors_undirected(&self, from: &G::VertexId) -> Self::NeighborsIter<'_> {
        if self.removed_vertices.contains(from) {
            panic!("vertex does not exist");
        }

        NeighborsIter {
            inner: self.inner.neighbors_undirected(from),
            removed_vertices: &self.removed_vertices,
            removed_edges: &self.removed_edges,
        }
    }

    fn neighbors_directed(&self, from: &G::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
        if self.removed_vertices.contains(from) {
            panic!("vertex does not exist");
        }

        NeighborsIter {
            inner: self.inner.neighbors_directed(from, dir),
            removed_vertices: &self.removed_vertices,
            removed_edges: &self.removed_edges,
        }
    }
}

impl<G> VertexSet for Stable<G>
where
    G: VertexSet,
{
    type VerticesByIdIter<'a>= VertexIds<'a, G>
    where
        Self: 'a;

    fn vertices_by_id(&self) -> Self::VerticesByIdIter<'_> {
        VertexIds {
            inner: self.inner.vertices_by_id(),
            removed_vertices: &self.removed_vertices,
        }
    }

    fn vertex_count(&self) -> usize {
        self.inner.vertex_count() - self.removed_vertices.len()
    }

    fn vertex_bound(&self) -> usize
    where
        Self::VertexId: IntegerIdType,
    {
        self.inner.vertex_bound()
    }

    fn contains_vertex(&self, id: &Self::VertexId) -> bool {
        !self.removed_vertices.contains(id) && self.inner.contains_vertex(id)
    }

    fn vertex_id_map(&self) -> CompactIdMap<Self::VertexId>
    where
        Self::VertexId: IntegerIdType,
    {
        if self.removed_vertices.is_empty() {
            self.inner.vertex_id_map()
        } else {
            CompactIdMap::new(self.vertices_by_id())
        }
    }
}

impl<G> EdgeSet for Stable<G>
where
    G: EdgeSet,
{
    type EdgesByIdIter<'a> = EdgesByIdIter<'a, G>
    where
        Self: 'a;

    type EdgeIdIter<'a> = EdgeIdIter<'a, G>
    where
        Self: 'a;

    fn edges_by_id(&self) -> Self::EdgesByIdIter<'_> {
        EdgesByIdIter {
            inner: self.inner.edges_by_id(),
            removed_edges: &self.removed_edges,
        }
    }

    fn edge_id(&self, from: &Self::VertexId, to: &Self::VertexId) -> Self::EdgeIdIter<'_> {
        let endpoints_exist =
            !self.removed_vertices.contains(from) && !self.removed_vertices.contains(to);
        EdgeIdIter {
            inner: self.inner.edge_id(from, to),
            removed_edges: &self.removed_edges,
            endpoints_exist,
        }
    }

    fn endpoints(&self, id: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)> {
        if self.removed_edges.contains(id) {
            None
        } else {
            self.inner.endpoints(id)
        }
    }

    fn edge_count(&self) -> usize {
        self.inner.edge_count() - self.removed_edges.len()
    }

    fn edge_bound(&self) -> usize
    where
        Self::EdgeId: IntegerIdType,
    {
        self.inner.edge_bound()
    }

    fn contains_edge(&self, id: &Self::EdgeId) -> bool {
        !self.removed_edges.contains(id) && self.inner.contains_edge(id)
    }

    fn edge_id_map(&self) -> CompactIdMap<Self::EdgeId>
    where
        Self::EdgeId: IntegerIdType,
    {
        if self.removed_edges.is_empty() {
            self.inner.edge_id_map()
        } else {
            CompactIdMap::new(self.edges_by_id())
        }
    }
}

impl<V, E, G> GraphRef<V, E> for Stable<G>
where
    G: GraphRef<V, E>,
{
    type VertexRef<'a> = G::VertexRef<'a>
    where
        Self: 'a,
        V: 'a;

    type VerticesIter<'a> = VerticesIter<'a, V, E, G>
    where
        Self: 'a,
        V: 'a;

    type EdgeRef<'a> = G::EdgeRef<'a>
    where
        Self: 'a,
        E: 'a;

    type EdgesIter<'a> = EdgesIter<'a, V, E, G>
    where
        Self: 'a,
        E: 'a;

    fn vertices(&self) -> Self::VerticesIter<'_> {
        VerticesIter {
            inner: self.inner.vertices(),
            removed_vertices: &self.removed_vertices,
        }
    }

    fn edges(&self) -> Self::EdgesIter<'_> {
        EdgesIter {
            inner: self.inner.edges(),
            removed_edges: &self.removed_edges,
        }
    }

    fn vertex(&self, id: &Self::VertexId) -> Option<&V> {
        if self.removed_vertices.contains(id) {
            None
        } else {
            self.inner.vertex(id)
        }
    }

    fn edge(&self, id: &Self::EdgeId) -> Option<&E> {
        if self.removed_edges.contains(id) {
            None
        } else {
            self.inner.edge(id)
        }
    }
}

impl<V, E, G> GraphMut<V, E> for Stable<G>
where
    G: GraphMut<V, E>,
{
    fn vertex_mut(&mut self, id: &Self::VertexId) -> Option<&mut V> {
        if self.removed_vertices.contains(id) {
            None
        } else {
            self.inner.vertex_mut(id)
        }
    }

    fn edge_mut(&mut self, id: &Self::EdgeId) -> Option<&mut E> {
        if self.removed_edges.contains(id) {
            None
        } else {
            self.inner.edge_mut(id)
        }
    }
}

impl<V, E, G> GraphAdd<V, E> for Stable<G>
where
    G: GraphAdd<V, E>,
{
    fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>> {
        self.inner.try_add_vertex(vertex)
    }

    fn try_add_edge(
        &mut self,
        from: &Self::VertexId,
        to: &Self::VertexId,
        edge: E,
    ) -> Result<Self::EdgeId, AddEdgeError<E>> {
        if self.removed_vertices.contains(from) {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::TailAbsent));
        }

        if self.removed_vertices.contains(to) {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::HeadAbsent));
        }

        self.inner.try_add_edge(from, to, edge)
    }
}

impl<V, E, G> GraphFull<V, E> for Stable<G>
where
    G: GraphFull<V, E> + Neighbors,
    V: Clone,
    E: Clone,
{
    fn remove_vertex(&mut self, id: &Self::VertexId) -> Option<V> {
        if let Some(attr) = self.vertex(id) {
            let attr = attr.clone();

            // Iterate over remaining neighbors only to get edges to be marked
            // as removed. An alternative could be to iterate over all neighbors
            // in the inner graph and let HashMap to handle duplicates, but that
            // may cause unnecessary overhead if a lot of edges incident to the
            // vertex has been removed.
            let mut removed_edges = BTreeSet::default();
            for neighbor in self.neighbors_undirected(id) {
                removed_edges.insert(neighbor.edge().into_owned());
            }

            self.removed_vertices.insert(id.clone());

            for edge in removed_edges {
                self.removed_edges.insert(edge);
            }

            Some(attr)
        } else {
            None
        }
    }

    fn remove_edge(&mut self, id: &Self::EdgeId) -> Option<E> {
        if let Some(attr) = self.edge(id) {
            let attr = attr.clone();
            self.removed_edges.insert(id.clone());
            Some(attr)
        } else {
            None
        }
    }

    fn clear(&mut self) {
        for vertex in self.inner.vertices_by_id() {
            for neighbor in self.inner.neighbors_undirected(&vertex) {
                self.removed_edges.insert(neighbor.edge().into_owned());
            }

            self.removed_vertices.insert(vertex);
        }
    }

    fn clear_edges(&mut self) {
        for edge in self.inner.edges_by_id() {
            self.removed_edges.insert(edge);
        }
    }
}

impl<V: Clone, E: Clone, G> Create<V, E> for Stable<G>
where
    G: Create<V, E> + Neighbors,
{
    fn with_capacity(vertex_capacity: usize, edge_capacity: usize) -> Self {
        Self::new(G::with_capacity(vertex_capacity, edge_capacity))
    }
}

impl<V, E, G> ConnectVertices<V, E> for Stable<G>
where
    G: ConnectVertices<V, E> + Neighbors,
    V: Clone,
    E: Clone,
{
    fn connect_vertices<F>(&mut self, connect: F)
    where
        F: FnMut(&V, &V) -> Option<E>,
    {
        self.inner.connect_vertices(connect);
    }
}

impl<G: GraphBase> StableId<VertexId, NoReplace> for Stable<G> {}
impl<G: GraphBase> StableId<EdgeId, NoReplace> for Stable<G> {}

pub struct VertexIds<'a, G: VertexSet + 'a> {
    inner: G::VerticesByIdIter<'a>,
    removed_vertices: &'a BTreeSet<G::VertexId>,
}

impl<'a, G: VertexSet> Iterator for VertexIds<'a, G> {
    type Item = G::VertexId;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .by_ref()
            .find(|id| !self.removed_vertices.contains(id))
    }
}

pub struct VerticesIter<'a, V: 'a, E, G: GraphRef<V, E> + 'a> {
    inner: G::VerticesIter<'a>,
    removed_vertices: &'a BTreeSet<G::VertexId>,
}

impl<'a, V, E, G: GraphRef<V, E>> Iterator for VerticesIter<'a, V, E, G> {
    type Item = G::VertexRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .by_ref()
            .find(|vertex| !self.removed_vertices.contains(vertex.id()))
    }
}

pub struct EdgesByIdIter<'a, G: EdgeSet + 'a> {
    inner: G::EdgesByIdIter<'a>,
    removed_edges: &'a BTreeSet<G::EdgeId>,
}

impl<'a, G: EdgeSet> Iterator for EdgesByIdIter<'a, G> {
    type Item = G::EdgeId;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .by_ref()
            .find(|id| !self.removed_edges.contains(id))
    }
}

pub struct EdgesIter<'a, V, E: 'a, G: GraphRef<V, E> + 'a> {
    inner: G::EdgesIter<'a>,
    removed_edges: &'a BTreeSet<G::EdgeId>,
}

impl<'a, V, E, G: GraphRef<V, E>> Iterator for EdgesIter<'a, V, E, G> {
    type Item = G::EdgeRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .by_ref()
            .find(|edge| !self.removed_edges.contains(edge.id()))
    }
}

pub struct EdgeIdIter<'a, G: EdgeSet + 'a> {
    inner: G::EdgeIdIter<'a>,
    removed_edges: &'a BTreeSet<G::EdgeId>,
    endpoints_exist: bool,
}

impl<'a, G: EdgeSet> Iterator for EdgeIdIter<'a, G> {
    type Item = G::EdgeId;

    fn next(&mut self) -> Option<Self::Item> {
        if self.endpoints_exist {
            self.inner
                .by_ref()
                .find(|id| !self.removed_edges.contains(id))
        } else {
            None
        }
    }
}

pub struct NeighborsIter<'a, G: Neighbors + 'a> {
    inner: G::NeighborsIter<'a>,
    removed_vertices: &'a BTreeSet<G::VertexId>,
    removed_edges: &'a BTreeSet<G::EdgeId>,
}

impl<'a, G> Iterator for NeighborsIter<'a, G>
where
    G: Neighbors,
{
    type Item = G::NeighborRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.by_ref().find(|neighbor| {
            !self.removed_edges.contains(&neighbor.edge())
                && !self.removed_vertices.contains(&neighbor.id())
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        core::{
            id::DefaultId,
            marker::{Directed, Undirected},
        },
        storage::{tests::*, AdjList},
    };

    use std::collections::HashSet;

    #[test]
    fn basic_undirected() {
        test_basic::<Stable<AdjList<_, _, Undirected, DefaultId>>>();
    }

    #[test]
    fn basic_directed() {
        test_basic::<Stable<AdjList<_, _, Directed, DefaultId>>>();
    }

    #[test]
    fn apply() {
        let mut graph: Stable<AdjList<_, _, Undirected, DefaultId>> = Stable::new(AdjList::new());

        let u = graph.add_vertex("u");
        let v = graph.add_vertex("v");
        let w = graph.add_vertex("w");
        let x = graph.add_vertex("x");
        let y = graph.add_vertex("y");
        let z = graph.add_vertex("z");

        let e = graph.add_edge(&u, &v, "e");
        graph.add_edge(&w, &x, "f");
        let g = graph.add_edge(&y, &z, "g");

        // Testing if the apply operation successfully removes all vertices and
        // edges, even if the underlying graph does not have stable ids.

        graph.remove_edge(&e);
        graph.remove_edge(&g);

        graph.remove_vertex(&u);
        graph.remove_vertex(&z);

        let graph = graph.apply();

        assert_eq!(graph.vertex_count(), 4);
        assert_eq!(graph.edge_count(), 1);

        let vertices = graph
            .vertices()
            .map(|v| v.attr().to_string())
            .collect::<HashSet<_>>();

        assert!(vertices.contains("v"));
        assert!(vertices.contains("w"));
        assert!(vertices.contains("x"));
        assert!(vertices.contains("y"));

        let edges = graph
            .edges()
            .map(|e| e.attr().to_string())
            .collect::<HashSet<_>>();

        assert!(edges.contains("f"));
    }

    #[test]
    fn contains_vertex() {
        let mut graph: Stable<AdjList<_, (), Undirected, DefaultId>> = Stable::new(AdjList::new());

        let v = graph.add_vertex(());
        graph.remove_vertex(&v);

        assert!(!graph.contains_vertex(&v));
    }

    #[test]
    fn contains_edge() {
        let mut graph: Stable<AdjList<_, _, Undirected, DefaultId>> = Stable::new(AdjList::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let e = graph.add_edge(&v0, &v1, ());

        graph.remove_edge(&e);

        assert!(!graph.contains_edge(&e));
    }

    #[test]
    fn edge_id_any() {
        let mut graph: Stable<AdjList<_, _, Undirected, DefaultId>> = Stable::new(AdjList::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let e = graph.add_edge(&v0, &v1, ());

        graph.remove_edge(&e);

        assert!(graph.edge_id_any(&v0, &v1).is_none());
    }

    #[test]
    #[should_panic]
    fn replace_vertex() {
        let mut graph: Stable<AdjList<_, (), Undirected, DefaultId>> = Stable::new(AdjList::new());

        let v = graph.add_vertex(());
        graph.remove_vertex(&v);
        graph.replace_vertex(&v, ());
    }

    #[test]
    #[should_panic]
    fn replace_edge() {
        let mut graph: Stable<AdjList<_, _, Undirected, DefaultId>> = Stable::new(AdjList::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let e = graph.add_edge(&v0, &v1, ());

        graph.remove_edge(&e);
        graph.replace_edge(&e, ());
    }
}
