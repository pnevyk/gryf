use std::collections::BTreeSet;

use crate::{
    common::CompactIndexMap,
    core::{
        index::{EdgeIndex, NumIndexType, VertexIndex},
        marker::{Direction, EdgeType},
        Create, EdgeRef, Edges, EdgesBase, EdgesMut, GraphBase, NeighborRef, Neighbors, NoReplace,
        StableIndices, VertexRef, Vertices, VerticesBase, VerticesMut,
    },
};

use crate::derive::{
    EdgesBaseWeak, EdgesWeak, GraphBase, Guarantee, VerticesBaseWeak, VerticesWeak,
};

// TODO: Remove these imports once hygiene of procedural macros is fixed.
use crate::core::{EdgesBaseWeak, EdgesWeak, Guarantee, VerticesBaseWeak, VerticesWeak, WeakRef};

#[derive(Debug, GraphBase, VerticesBaseWeak, VerticesWeak, EdgesBaseWeak, EdgesWeak, Guarantee)]
pub struct Stable<G: GraphBase> {
    #[graph]
    inner: G,
    removed_vertices: BTreeSet<G::VertexIndex>,
    removed_edges: BTreeSet<G::EdgeIndex>,
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

        // Propagate the removals. The descending order of indices is important
        // for not invalidating the stored indices when creating "holes" in the
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
    type VertexIndicesIter<'a> = VertexIndices<'a, G>
    where
        Self: 'a;

    fn vertex_count(&self) -> usize {
        self.inner.vertex_count() - self.removed_vertices.len()
    }

    fn vertex_bound(&self) -> usize {
        self.inner.vertex_bound()
    }

    fn vertex_indices(&self) -> Self::VertexIndicesIter<'_> {
        VertexIndices {
            inner: self.inner.vertex_indices(),
            removed_vertices: &self.removed_vertices,
        }
    }

    fn contains_vertex(&self, index: &G::VertexIndex) -> bool {
        if self.removed_vertices.contains(index) {
            false
        } else {
            self.inner.contains_vertex(index)
        }
    }

    fn vertex_index_map(&self) -> CompactIndexMap<G::VertexIndex>
    where
        Self::VertexIndex: NumIndexType,
    {
        if self.removed_vertices.is_empty() {
            self.inner.vertex_index_map()
        } else {
            CompactIndexMap::new(self.vertex_indices())
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

    fn vertex(&self, index: &G::VertexIndex) -> Option<&V> {
        if self.removed_vertices.contains(index) {
            None
        } else {
            self.inner.vertex(index)
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
    fn vertex_mut(&mut self, index: &G::VertexIndex) -> Option<&mut V> {
        if self.removed_vertices.contains(index) {
            None
        } else {
            self.inner.vertex_mut(index)
        }
    }

    fn add_vertex(&mut self, vertex: V) -> G::VertexIndex {
        self.inner.add_vertex(vertex)
    }

    fn remove_vertex(&mut self, index: &G::VertexIndex) -> Option<V> {
        if let Some(data) = self.vertex(index) {
            let data = data.clone();
            self.removed_vertices.insert(index.clone());

            // Iterate over remaining neighbors only to get edges to be marked
            // as removed. An alternative could be to iterate over all neighbors
            // in the inner graph and let HashMap to handle duplicates, but that
            // may cause unnecessary overhead if a lot of edges incident to the
            // vertex has been removed.
            let mut removed_edges = BTreeSet::default();
            for neighbor in self.neighbors(index) {
                removed_edges.insert(neighbor.edge().into_owned());
            }

            for edge in removed_edges {
                self.removed_edges.insert(edge);
            }

            Some(data)
        } else {
            None
        }
    }

    fn clear(&mut self) {
        for vertex in self.inner.vertex_indices() {
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
    type EdgeIndicesIter<'a> = EdgeIndices<'a, Ty, G>
    where
        Self: 'a;

    fn edge_count(&self) -> usize {
        self.inner.edge_count() - self.removed_edges.len()
    }

    fn edge_bound(&self) -> usize {
        self.inner.edge_bound()
    }

    fn endpoints(&self, index: &G::EdgeIndex) -> Option<(G::VertexIndex, G::VertexIndex)> {
        if self.removed_edges.contains(index) {
            None
        } else {
            self.inner.endpoints(index)
        }
    }

    fn edge_index(&self, src: &G::VertexIndex, dst: &G::VertexIndex) -> Option<G::EdgeIndex> {
        match (
            self.removed_vertices.contains(src),
            self.removed_vertices.contains(dst),
        ) {
            (false, false) => self.inner.edge_index(src, dst).and_then(|index| {
                if self.removed_edges.contains(&index) {
                    None
                } else {
                    Some(index)
                }
            }),
            _ => None,
        }
    }

    fn edge_indices(&self) -> Self::EdgeIndicesIter<'_> {
        EdgeIndices {
            inner: self.inner.edge_indices(),
            removed_edges: &self.removed_edges,
        }
    }

    fn contains_edge(&self, index: &G::EdgeIndex) -> bool {
        if self.removed_edges.contains(index) {
            false
        } else {
            self.inner.contains_edge(index)
        }
    }

    fn edge_index_map(&self) -> CompactIndexMap<G::EdgeIndex>
    where
        Self::EdgeIndex: NumIndexType,
    {
        if self.removed_edges.is_empty() {
            self.inner.edge_index_map()
        } else {
            CompactIndexMap::new(self.edge_indices())
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

    fn edge(&self, index: &G::EdgeIndex) -> Option<&E> {
        if self.removed_edges.contains(index) {
            None
        } else {
            self.inner.edge(index)
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
    fn edge_mut(&mut self, index: &G::EdgeIndex) -> Option<&mut E> {
        if self.removed_edges.contains(index) {
            None
        } else {
            self.inner.edge_mut(index)
        }
    }

    fn add_edge(&mut self, src: &G::VertexIndex, dst: &G::VertexIndex, edge: E) -> G::EdgeIndex {
        self.inner.add_edge(src, dst, edge)
    }

    fn remove_edge(&mut self, index: &G::EdgeIndex) -> Option<E> {
        if let Some(data) = self.edge(index) {
            let data = data.clone();
            self.removed_edges.insert(index.clone());
            Some(data)
        } else {
            None
        }
    }

    fn clear_edges(&mut self) {
        for edge in self.inner.edge_indices() {
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

    fn neighbors(&self, src: &G::VertexIndex) -> Self::NeighborsIter<'_> {
        NeighborsIter {
            inner: self.inner.neighbors(src),
            removed_vertices: &self.removed_vertices,
            removed_edges: &self.removed_edges,
        }
    }

    fn neighbors_directed(&self, src: &G::VertexIndex, dir: Direction) -> Self::NeighborsIter<'_> {
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

impl<G: GraphBase> StableIndices<VertexIndex, NoReplace> for Stable<G> {}
impl<G: GraphBase> StableIndices<EdgeIndex, NoReplace> for Stable<G> {}

pub trait Stabilize {
    fn stabilize(self) -> Stable<Self>
    where
        Self: Sized + GraphBase;
}

pub struct VertexIndices<'a, G: VerticesBase + 'a> {
    inner: G::VertexIndicesIter<'a>,
    removed_vertices: &'a BTreeSet<G::VertexIndex>,
}

impl<'a, G: VerticesBase> Iterator for VertexIndices<'a, G> {
    type Item = G::VertexIndex;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .by_ref()
            .find(|index| !self.removed_vertices.contains(index))
    }
}

pub struct VerticesIter<'a, V: 'a, G: Vertices<V> + 'a> {
    inner: G::VerticesIter<'a>,
    removed_vertices: &'a BTreeSet<G::VertexIndex>,
}

impl<'a, V, G: Vertices<V>> Iterator for VerticesIter<'a, V, G> {
    type Item = G::VertexRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .by_ref()
            .find(|vertex| !self.removed_vertices.contains(vertex.index()))
    }
}

pub struct EdgeIndices<'a, Ty: EdgeType, G: EdgesBase<Ty> + 'a> {
    inner: G::EdgeIndicesIter<'a>,
    removed_edges: &'a BTreeSet<G::EdgeIndex>,
}

impl<'a, Ty: EdgeType, G: EdgesBase<Ty>> Iterator for EdgeIndices<'a, Ty, G> {
    type Item = G::EdgeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .by_ref()
            .find(|index| !self.removed_edges.contains(index))
    }
}

pub struct EdgesIter<'a, E: 'a, Ty: EdgeType, G: Edges<E, Ty> + 'a> {
    inner: G::EdgesIter<'a>,
    removed_edges: &'a BTreeSet<G::EdgeIndex>,
}

impl<'a, E, Ty: EdgeType, G: Edges<E, Ty>> Iterator for EdgesIter<'a, E, Ty, G> {
    type Item = G::EdgeRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .by_ref()
            .find(|edge| !self.removed_edges.contains(edge.index()))
    }
}

pub struct NeighborsIter<'a, G: Neighbors + 'a> {
    inner: G::NeighborsIter<'a>,
    removed_vertices: &'a BTreeSet<G::VertexIndex>,
    removed_edges: &'a BTreeSet<G::EdgeIndex>,
}

impl<'a, G> Iterator for NeighborsIter<'a, G>
where
    G: Neighbors,
{
    type Item = G::NeighborRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.by_ref().find(|neighbor| {
            !self.removed_edges.contains(&neighbor.edge())
                && !self.removed_vertices.contains(&neighbor.index())
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        core::{
            index::DefaultIndexing,
            marker::{Directed, Undirected},
        },
        storage::{tests::*, AdjList},
    };

    use std::collections::HashSet;

    #[test]
    fn basic_undirected() {
        test_basic::<Undirected, Stable<AdjList<_, _, _, DefaultIndexing>>>();
    }

    #[test]
    fn basic_directed() {
        test_basic::<Directed, Stable<AdjList<_, _, _, DefaultIndexing>>>();
    }

    #[test]
    fn apply() {
        let mut graph: Stable<AdjList<_, _, Undirected, DefaultIndexing>> =
            Stable::new(AdjList::new());

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
        // edges, even if the underlying graph does not have stable indices.

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
        let mut graph: Stable<AdjList<_, (), Undirected, DefaultIndexing>> =
            Stable::new(AdjList::new());

        let v = graph.add_vertex(());
        graph.remove_vertex(&v);

        assert!(!graph.contains_vertex(&v));
    }

    #[test]
    fn contains_edge() {
        let mut graph: Stable<AdjList<_, _, Undirected, DefaultIndexing>> =
            Stable::new(AdjList::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let e = graph.add_edge(&v0, &v1, ());

        graph.remove_edge(&e);

        assert!(!graph.contains_edge(&e));
    }

    #[test]
    fn edge_index() {
        let mut graph: Stable<AdjList<_, _, Undirected, DefaultIndexing>> =
            Stable::new(AdjList::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let e = graph.add_edge(&v0, &v1, ());

        graph.remove_edge(&e);

        assert!(graph.edge_index(&v0, &v1).is_none());
    }

    #[test]
    #[should_panic]
    fn replace_vertex() {
        let mut graph: Stable<AdjList<_, (), Undirected, DefaultIndexing>> =
            Stable::new(AdjList::new());

        let v = graph.add_vertex(());
        graph.remove_vertex(&v);
        graph.replace_vertex(&v, ());
    }

    #[test]
    #[should_panic]
    fn replace_edge() {
        let mut graph: Stable<AdjList<_, _, Undirected, DefaultIndexing>> =
            Stable::new(AdjList::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let e = graph.add_edge(&v0, &v1, ());

        graph.remove_edge(&e);
        graph.replace_edge(&e, ());
    }
}
