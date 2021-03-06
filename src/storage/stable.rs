use std::collections::BTreeSet;
use std::marker::PhantomData;

use crate::index::{EdgeIndex, VertexIndex};
use crate::infra::CompactIndexMap;
use crate::marker::{Direction, EdgeType};
use crate::traits::*;
use crate::{EdgesBaseWeak, EdgesWeak, Guarantee, VerticesBaseWeak, VerticesWeak};

#[derive(Debug, VerticesBaseWeak, VerticesWeak, EdgesBaseWeak, EdgesWeak, Guarantee)]
pub struct Stable<G> {
    #[graph]
    inner: G,
    removed_vertices: BTreeSet<VertexIndex>,
    removed_edges: BTreeSet<EdgeIndex>,
}

impl<G> Stable<G> {
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
        for edge in self.removed_edges.iter().rev().copied() {
            let removed = inner.remove_edge(edge);
            debug_assert!(removed.is_some());
        }

        for vertex in self.removed_vertices.iter().rev().copied() {
            let removed = inner.remove_vertex(vertex);
            debug_assert!(removed.is_some());
        }

        inner
    }
}

impl<G> Default for Stable<G>
where
    G: Default,
{
    fn default() -> Self {
        Self::new(G::default())
    }
}

impl<G> From<G> for Stable<G> {
    fn from(inner: G) -> Self {
        Self::new(inner)
    }
}

impl<G> VerticesBase for Stable<G>
where
    G: VerticesBase,
{
    type VertexIndicesIter<'a> = VertexIndices<'a, G::VertexIndicesIter<'a>>
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

    fn contains_vertex(&self, index: VertexIndex) -> bool {
        if self.removed_vertices.contains(&index) {
            false
        } else {
            self.inner.contains_vertex(index)
        }
    }

    fn vertex_index_map(&self) -> CompactIndexMap<VertexIndex> {
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
    type VertexRef<'a, T: 'a> = G::VertexRef<'a, T>
    where
        Self: 'a;

    type VerticesIter<'a, T: 'a> = VerticesIter<'a, T, Self::VertexRef<'a, T>, G::VerticesIter<'a, T>>
    where
        Self: 'a;

    fn vertex(&self, index: VertexIndex) -> Option<&V> {
        if self.removed_vertices.contains(&index) {
            None
        } else {
            self.inner.vertex(index)
        }
    }

    fn vertices(&self) -> Self::VerticesIter<'_, V> {
        VerticesIter {
            inner: self.inner.vertices(),
            removed_vertices: &self.removed_vertices,
            ty: PhantomData,
        }
    }
}

impl<V, G> VerticesMut<V> for Stable<G>
where
    G: VerticesMut<V> + Neighbors,
    V: Clone,
{
    fn vertex_mut(&mut self, index: VertexIndex) -> Option<&mut V> {
        if self.removed_vertices.contains(&index) {
            None
        } else {
            self.inner.vertex_mut(index)
        }
    }

    fn add_vertex(&mut self, vertex: V) -> VertexIndex {
        self.inner.add_vertex(vertex)
    }

    fn remove_vertex(&mut self, index: VertexIndex) -> Option<V> {
        if let Some(data) = self.vertex(index) {
            let data = data.clone();
            self.removed_vertices.insert(index);

            // Iterate over remaining neighbors only to get edges to be marked
            // as removed. An alternative could be to iterate over all neighbors
            // in the inner graph and let HashMap to handle duplicates, but that
            // may cause unnecessary overhead if a lot of edges incident to the
            // vertex has been removed.
            let mut removed_edges = BTreeSet::default();
            for neighbor in self.neighbors(index) {
                removed_edges.insert(neighbor.edge());
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
            self.removed_vertices.insert(vertex);

            for neighbor in self.inner.neighbors(vertex) {
                self.removed_edges.insert(neighbor.edge());
            }
        }
    }
}

impl<Ty: EdgeType, G> EdgesBase<Ty> for Stable<G>
where
    G: EdgesBase<Ty>,
{
    type EdgeIndicesIter<'a> = EdgeIndices<'a, G::EdgeIndicesIter<'a>>
    where
        Self: 'a;

    fn edge_count(&self) -> usize {
        self.inner.edge_count() - self.removed_edges.len()
    }

    fn edge_bound(&self) -> usize {
        self.inner.edge_bound()
    }

    fn endpoints(&self, index: EdgeIndex) -> Option<(VertexIndex, VertexIndex)> {
        if self.removed_edges.contains(&index) {
            None
        } else {
            self.inner.endpoints(index)
        }
    }

    fn edge_index(&self, src: VertexIndex, dst: VertexIndex) -> Option<EdgeIndex> {
        match (
            self.removed_vertices.contains(&src),
            self.removed_vertices.contains(&dst),
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

    fn contains_edge(&self, index: EdgeIndex) -> bool {
        if self.removed_edges.contains(&index) {
            false
        } else {
            self.inner.contains_edge(index)
        }
    }

    fn edge_index_map(&self) -> CompactIndexMap<EdgeIndex> {
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
    type EdgeRef<'a, T: 'a> = G::EdgeRef<'a, T>
    where
        Self: 'a;

    type EdgesIter<'a, T: 'a> = EdgesIter<'a, T, Self::EdgeRef<'a, T>, G::EdgesIter<'a, T>>
    where
        Self: 'a;

    fn edge(&self, index: EdgeIndex) -> Option<&E> {
        if self.removed_edges.contains(&index) {
            None
        } else {
            self.inner.edge(index)
        }
    }

    fn edges(&self) -> Self::EdgesIter<'_, E> {
        EdgesIter {
            inner: self.inner.edges(),
            removed_edges: &self.removed_edges,
            ty: PhantomData,
        }
    }
}

impl<E, Ty: EdgeType, G> EdgesMut<E, Ty> for Stable<G>
where
    G: EdgesMut<E, Ty>,
    E: Clone,
{
    fn edge_mut(&mut self, index: EdgeIndex) -> Option<&mut E> {
        if self.removed_edges.contains(&index) {
            None
        } else {
            self.inner.edge_mut(index)
        }
    }

    fn add_edge(&mut self, src: VertexIndex, dst: VertexIndex, edge: E) -> EdgeIndex {
        self.inner.add_edge(src, dst, edge)
    }

    fn remove_edge(&mut self, index: EdgeIndex) -> Option<E> {
        if let Some(data) = self.edge(index) {
            let data = data.clone();
            self.removed_edges.insert(index);
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

    fn neighbors(&self, src: VertexIndex) -> Self::NeighborsIter<'_> {
        NeighborsIter {
            inner: self.inner.neighbors(src),
            removed_vertices: &self.removed_vertices,
            removed_edges: &self.removed_edges,
        }
    }

    fn neighbors_directed(&self, src: VertexIndex, dir: Direction) -> Self::NeighborsIter<'_> {
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

impl<G> StableIndices<VertexIndex, NoReplace> for Stable<G> {}
impl<G> StableIndices<EdgeIndex, NoReplace> for Stable<G> {}

pub trait Stabilize {
    fn stabilize(self) -> Stable<Self>
    where
        Self: Sized;
}

pub struct VertexIndices<'a, I> {
    inner: I,
    removed_vertices: &'a BTreeSet<VertexIndex>,
}

impl<'a, I> Iterator for VertexIndices<'a, I>
where
    I: Iterator<Item = VertexIndex>,
{
    type Item = VertexIndex;

    fn next(&mut self) -> Option<Self::Item> {
        for index in self.inner.by_ref() {
            if !self.removed_vertices.contains(&index) {
                return Some(index);
            }
        }

        None
    }
}

pub struct VerticesIter<'a, V: 'a, R, I> {
    inner: I,
    removed_vertices: &'a BTreeSet<VertexIndex>,
    ty: PhantomData<(V, R)>,
}

impl<'a, V: 'a, R, I> Iterator for VerticesIter<'a, V, R, I>
where
    R: VertexRef<V>,
    I: Iterator<Item = R>,
{
    type Item = R;

    fn next(&mut self) -> Option<Self::Item> {
        for vertex in self.inner.by_ref() {
            if !self.removed_vertices.contains(&vertex.index()) {
                return Some(vertex);
            }
        }

        None
    }
}

pub struct EdgeIndices<'a, I> {
    inner: I,
    removed_edges: &'a BTreeSet<EdgeIndex>,
}

impl<'a, I> Iterator for EdgeIndices<'a, I>
where
    I: Iterator<Item = EdgeIndex>,
{
    type Item = EdgeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        for index in self.inner.by_ref() {
            if !self.removed_edges.contains(&index) {
                return Some(index);
            }
        }

        None
    }
}

pub struct EdgesIter<'a, E: 'a, R: EdgeRef<E>, I> {
    inner: I,
    removed_edges: &'a BTreeSet<EdgeIndex>,
    ty: PhantomData<(E, R)>,
}

impl<'a, E: 'a, R, I> Iterator for EdgesIter<'a, E, R, I>
where
    R: EdgeRef<E>,
    I: Iterator<Item = R>,
{
    type Item = R;

    fn next(&mut self) -> Option<Self::Item> {
        for edge in self.inner.by_ref() {
            if !self.removed_edges.contains(&edge.index()) {
                return Some(edge);
            }
        }

        None
    }
}

pub struct NeighborsIter<'a, S: Neighbors + 'a> {
    inner: S::NeighborsIter<'a>,
    removed_vertices: &'a BTreeSet<VertexIndex>,
    removed_edges: &'a BTreeSet<EdgeIndex>,
}

impl<'a, S> Iterator for NeighborsIter<'a, S>
where
    S: Neighbors,
{
    type Item = S::NeighborRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        for neighbor in self.inner.by_ref() {
            if !self.removed_edges.contains(&neighbor.edge())
                && !self.removed_vertices.contains(&neighbor.index())
            {
                return Some(neighbor);
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::marker::{Directed, Undirected};
    use crate::storage::tests::*;
    use crate::storage::AdjList;

    use std::collections::HashSet;

    #[test]
    fn basic_undirected() {
        test_basic::<Undirected, Stable<AdjList<_, _, _>>>();
    }

    #[test]
    fn basic_directed() {
        test_basic::<Directed, Stable<AdjList<_, _, _>>>();
    }

    #[test]
    fn apply() {
        let mut graph: Stable<AdjList<_, _, Undirected>> = Stable::new(AdjList::new());

        let u = graph.add_vertex("u");
        let v = graph.add_vertex("v");
        let w = graph.add_vertex("w");
        let x = graph.add_vertex("x");
        let y = graph.add_vertex("y");
        let z = graph.add_vertex("z");

        let e = graph.add_edge(u, v, "e");
        graph.add_edge(w, x, "f");
        let g = graph.add_edge(y, z, "g");

        // Testing if the apply operation successfully removes all vertices and
        // edges, even if the underlying graph does not have stable indices.

        graph.remove_edge(e);
        graph.remove_edge(g);

        graph.remove_vertex(u);
        graph.remove_vertex(z);

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
        let mut graph: Stable<AdjList<_, (), Undirected>> = Stable::new(AdjList::new());

        let v = graph.add_vertex(());
        graph.remove_vertex(v);

        assert!(!graph.contains_vertex(v));
    }

    #[test]
    fn contains_edge() {
        let mut graph: Stable<AdjList<_, _, Undirected>> = Stable::new(AdjList::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let e = graph.add_edge(v0, v1, ());

        graph.remove_edge(e);

        assert!(!graph.contains_edge(e));
    }

    #[test]
    fn edge_index() {
        let mut graph: Stable<AdjList<_, _, Undirected>> = Stable::new(AdjList::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let e = graph.add_edge(v0, v1, ());

        graph.remove_edge(e);

        assert!(graph.edge_index(v0, v1).is_none());
    }

    #[test]
    #[should_panic]
    fn replace_vertex() {
        let mut graph: Stable<AdjList<_, (), Undirected>> = Stable::new(AdjList::new());

        let v = graph.add_vertex(());
        graph.remove_vertex(v);
        graph.replace_vertex(v, ());
    }

    #[test]
    #[should_panic]
    fn replace_edge() {
        let mut graph: Stable<AdjList<_, _, Undirected>> = Stable::new(AdjList::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let e = graph.add_edge(v0, v1, ());

        graph.remove_edge(e);
        graph.replace_edge(e, ());
    }
}
