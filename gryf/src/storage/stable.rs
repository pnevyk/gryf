use std::collections::BTreeSet;

use crate::{
    common::CompactIdMap,
    core::{
        id::{EdgeId, IntegerIdType, VertexId},
        marker::{Direction, EdgeType},
        AddEdgeError, AddEdgeErrorKind, AddVertexError, ConnectVertices, Create, EdgeRef, Edges,
        EdgesBase, EdgesMut, GraphBase, NeighborRef, Neighbors, NoReplace, StableId, VertexRef,
        Vertices, VerticesBase, VerticesMut,
    },
};

use gryf_derive::{EdgesBaseWeak, EdgesWeak, GraphBase, Guarantee, VerticesBaseWeak, VerticesWeak};

// TODO: Remove these imports once hygiene of procedural macros is fixed.
use crate::core::{EdgesBaseWeak, EdgesWeak, Guarantee, VerticesBaseWeak, VerticesWeak, WeakRef};

#[derive(Debug, GraphBase, VerticesBaseWeak, VerticesWeak, EdgesBaseWeak, EdgesWeak, Guarantee)]
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

    pub fn apply<V, E, Ty: EdgeType>(self) -> G
    where
        G: VerticesMut<V> + EdgesMut<E, Ty>,
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

impl<G> VerticesBase for Stable<G>
where
    G: VerticesBase,
{
    type VertexIdsIter<'a> = VertexIds<'a, G>
    where
        Self: 'a;

    fn vertex_count(&self) -> usize {
        self.inner.vertex_count() - self.removed_vertices.len()
    }

    fn vertex_bound(&self) -> usize {
        self.inner.vertex_bound()
    }

    fn vertex_ids(&self) -> Self::VertexIdsIter<'_> {
        VertexIds {
            inner: self.inner.vertex_ids(),
            removed_vertices: &self.removed_vertices,
        }
    }

    fn contains_vertex(&self, id: &G::VertexId) -> bool {
        if self.removed_vertices.contains(id) {
            false
        } else {
            self.inner.contains_vertex(id)
        }
    }

    fn vertex_id_map(&self) -> CompactIdMap<G::VertexId>
    where
        Self::VertexId: IntegerIdType,
    {
        if self.removed_vertices.is_empty() {
            self.inner.vertex_id_map()
        } else {
            CompactIdMap::new(self.vertex_ids())
        }
    }
}

impl<V, G> Vertices<V> for Stable<G>
where
    G: Vertices<V>,
{
    type VertexRef<'a> = G::VertexRef<'a>
    where
        Self: 'a,
        V: 'a;

    type VerticesIter<'a> = VerticesIter<'a, V, G>
    where
        Self: 'a,
        V: 'a;

    fn vertex(&self, id: &G::VertexId) -> Option<&V> {
        if self.removed_vertices.contains(id) {
            None
        } else {
            self.inner.vertex(id)
        }
    }

    fn vertices(&self) -> Self::VerticesIter<'_> {
        VerticesIter {
            inner: self.inner.vertices(),
            removed_vertices: &self.removed_vertices,
        }
    }
}

impl<V, G> VerticesMut<V> for Stable<G>
where
    G: VerticesMut<V> + Neighbors,
    V: Clone,
{
    fn vertex_mut(&mut self, id: &G::VertexId) -> Option<&mut V> {
        if self.removed_vertices.contains(id) {
            None
        } else {
            self.inner.vertex_mut(id)
        }
    }

    fn try_add_vertex(&mut self, vertex: V) -> Result<G::VertexId, AddVertexError<V>> {
        self.inner.try_add_vertex(vertex)
    }

    fn remove_vertex(&mut self, id: &G::VertexId) -> Option<V> {
        if let Some(data) = self.vertex(id) {
            let data = data.clone();

            // Iterate over remaining neighbors only to get edges to be marked
            // as removed. An alternative could be to iterate over all neighbors
            // in the inner graph and let HashMap to handle duplicates, but that
            // may cause unnecessary overhead if a lot of edges incident to the
            // vertex has been removed.
            let mut removed_edges = BTreeSet::default();
            for neighbor in self.neighbors(id) {
                removed_edges.insert(neighbor.edge().into_owned());
            }

            self.removed_vertices.insert(id.clone());

            for edge in removed_edges {
                self.removed_edges.insert(edge);
            }

            Some(data)
        } else {
            None
        }
    }

    fn clear(&mut self) {
        for vertex in self.inner.vertex_ids() {
            for neighbor in self.inner.neighbors(&vertex) {
                self.removed_edges.insert(neighbor.edge().into_owned());
            }

            self.removed_vertices.insert(vertex);
        }
    }
}

impl<Ty: EdgeType, G> EdgesBase<Ty> for Stable<G>
where
    G: EdgesBase<Ty>,
{
    type EdgeIdsIter<'a> = EdgeIdsIter<'a, Ty, G>
    where
        Self: 'a;
    type EdgeIdIter<'a> = EdgeIdIter<'a, Ty, G>
    where
        Self: 'a;

    fn edge_count(&self) -> usize {
        self.inner.edge_count() - self.removed_edges.len()
    }

    fn edge_bound(&self) -> usize {
        self.inner.edge_bound()
    }

    fn endpoints(&self, id: &G::EdgeId) -> Option<(G::VertexId, G::VertexId)> {
        if self.removed_edges.contains(id) {
            None
        } else {
            self.inner.endpoints(id)
        }
    }

    fn edge_id(&self, src: &G::VertexId, dst: &G::VertexId) -> Self::EdgeIdIter<'_> {
        let endpoints_exist =
            !self.removed_vertices.contains(src) && !self.removed_vertices.contains(dst);
        EdgeIdIter {
            inner: self.inner.edge_id(src, dst),
            removed_edges: &self.removed_edges,
            endpoints_exist,
        }
    }

    fn edge_ids(&self) -> Self::EdgeIdsIter<'_> {
        EdgeIdsIter {
            inner: self.inner.edge_ids(),
            removed_edges: &self.removed_edges,
        }
    }

    fn contains_edge(&self, id: &G::EdgeId) -> bool {
        if self.removed_edges.contains(id) {
            false
        } else {
            self.inner.contains_edge(id)
        }
    }

    fn edge_id_map(&self) -> CompactIdMap<G::EdgeId>
    where
        Self::EdgeId: IntegerIdType,
    {
        if self.removed_edges.is_empty() {
            self.inner.edge_id_map()
        } else {
            CompactIdMap::new(self.edge_ids())
        }
    }
}

impl<E, Ty: EdgeType, G> Edges<E, Ty> for Stable<G>
where
    G: Edges<E, Ty>,
{
    type EdgeRef<'a> = G::EdgeRef<'a>
    where
        Self: 'a,
        E: 'a;

    type EdgesIter<'a> = EdgesIter<'a, E, Ty, G>
    where
        Self: 'a,
        E: 'a;

    fn edge(&self, id: &G::EdgeId) -> Option<&E> {
        if self.removed_edges.contains(id) {
            None
        } else {
            self.inner.edge(id)
        }
    }

    fn edges(&self) -> Self::EdgesIter<'_> {
        EdgesIter {
            inner: self.inner.edges(),
            removed_edges: &self.removed_edges,
        }
    }
}

impl<E, Ty: EdgeType, G> EdgesMut<E, Ty> for Stable<G>
where
    G: EdgesMut<E, Ty>,
    E: Clone,
{
    fn edge_mut(&mut self, id: &G::EdgeId) -> Option<&mut E> {
        if self.removed_edges.contains(id) {
            None
        } else {
            self.inner.edge_mut(id)
        }
    }

    fn try_add_edge(
        &mut self,
        src: &G::VertexId,
        dst: &G::VertexId,
        edge: E,
    ) -> Result<G::EdgeId, AddEdgeError<E>> {
        if self.removed_vertices.contains(src) {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::SourceAbsent));
        }

        if self.removed_vertices.contains(dst) {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::DestinationAbsent));
        }

        self.inner.try_add_edge(src, dst, edge)
    }

    fn remove_edge(&mut self, id: &G::EdgeId) -> Option<E> {
        if let Some(data) = self.edge(id) {
            let data = data.clone();
            self.removed_edges.insert(id.clone());
            Some(data)
        } else {
            None
        }
    }

    fn clear_edges(&mut self) {
        for edge in self.inner.edge_ids() {
            self.removed_edges.insert(edge);
        }
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

    fn neighbors(&self, src: &G::VertexId) -> Self::NeighborsIter<'_> {
        if self.removed_vertices.contains(src) {
            panic!("vertex does not exist");
        }

        NeighborsIter {
            inner: self.inner.neighbors(src),
            removed_vertices: &self.removed_vertices,
            removed_edges: &self.removed_edges,
        }
    }

    fn neighbors_directed(&self, src: &G::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
        if self.removed_vertices.contains(src) {
            panic!("vertex does not exist");
        }

        NeighborsIter {
            inner: self.inner.neighbors_directed(src, dir),
            removed_vertices: &self.removed_vertices,
            removed_edges: &self.removed_edges,
        }
    }
}

impl<V: Clone, E: Clone, Ty: EdgeType, G> Create<V, E, Ty> for Stable<G>
where
    G: Create<V, E, Ty> + Neighbors,
{
    fn with_capacity(vertex_count: usize, edge_count: usize) -> Self {
        Self::new(G::with_capacity(vertex_count, edge_count))
    }
}

impl<V, E, Ty: EdgeType, G> ConnectVertices<V, E, Ty> for Stable<G>
where
    G: ConnectVertices<V, E, Ty> + Neighbors,
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

pub trait Stabilize {
    fn stabilize(self) -> Stable<Self>
    where
        Self: Sized + GraphBase;
}

pub struct VertexIds<'a, G: VerticesBase + 'a> {
    inner: G::VertexIdsIter<'a>,
    removed_vertices: &'a BTreeSet<G::VertexId>,
}

impl<'a, G: VerticesBase> Iterator for VertexIds<'a, G> {
    type Item = G::VertexId;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .by_ref()
            .find(|id| !self.removed_vertices.contains(id))
    }
}

pub struct VerticesIter<'a, V: 'a, G: Vertices<V> + 'a> {
    inner: G::VerticesIter<'a>,
    removed_vertices: &'a BTreeSet<G::VertexId>,
}

impl<'a, V, G: Vertices<V>> Iterator for VerticesIter<'a, V, G> {
    type Item = G::VertexRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .by_ref()
            .find(|vertex| !self.removed_vertices.contains(vertex.id()))
    }
}

pub struct EdgeIdsIter<'a, Ty: EdgeType, G: EdgesBase<Ty> + 'a> {
    inner: G::EdgeIdsIter<'a>,
    removed_edges: &'a BTreeSet<G::EdgeId>,
}

impl<'a, Ty: EdgeType, G: EdgesBase<Ty>> Iterator for EdgeIdsIter<'a, Ty, G> {
    type Item = G::EdgeId;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .by_ref()
            .find(|id| !self.removed_edges.contains(id))
    }
}

pub struct EdgesIter<'a, E: 'a, Ty: EdgeType, G: Edges<E, Ty> + 'a> {
    inner: G::EdgesIter<'a>,
    removed_edges: &'a BTreeSet<G::EdgeId>,
}

impl<'a, E, Ty: EdgeType, G: Edges<E, Ty>> Iterator for EdgesIter<'a, E, Ty, G> {
    type Item = G::EdgeRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .by_ref()
            .find(|edge| !self.removed_edges.contains(edge.id()))
    }
}

pub struct EdgeIdIter<'a, Ty: EdgeType, G: EdgesBase<Ty> + 'a> {
    inner: G::EdgeIdIter<'a>,
    removed_edges: &'a BTreeSet<G::EdgeId>,
    endpoints_exist: bool,
}

impl<'a, Ty: EdgeType, G: EdgesBase<Ty>> Iterator for EdgeIdIter<'a, Ty, G> {
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
        test_basic::<Undirected, Stable<AdjList<_, _, _, DefaultId>>>();
    }

    #[test]
    fn basic_directed() {
        test_basic::<Directed, Stable<AdjList<_, _, _, DefaultId>>>();
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
            .map(|v| v.data().to_string())
            .collect::<HashSet<_>>();

        assert!(vertices.contains("v"));
        assert!(vertices.contains("w"));
        assert!(vertices.contains("x"));
        assert!(vertices.contains("y"));

        let edges = graph
            .edges()
            .map(|e| e.data().to_string())
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
