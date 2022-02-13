use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

use crate::index::{EdgeIndex, VertexIndex};
use crate::infra::CompactIndexMap;
use crate::marker::{Directed, Direction, EdgeType, Undirected};
use crate::storage::{AdjList, Frozen, Stable};
use crate::traits::*;
use crate::{
    Edges, EdgesBase, EdgesBaseWeak, EdgesMut, EdgesWeak, Guarantee, MultiEdges, Neighbors,
    Vertices, VerticesBase, VerticesBaseWeak, VerticesMut, VerticesWeak,
};

#[derive(
    Debug,
    VerticesBase,
    Vertices,
    VerticesMut,
    EdgesBase,
    Edges,
    EdgesMut,
    MultiEdges,
    Neighbors,
    VerticesBaseWeak,
    VerticesWeak,
    EdgesBaseWeak,
    EdgesWeak,
    Guarantee,
)]
pub struct Graph<V, E, Ty: EdgeType, G> {
    #[graph]
    graph: G,
    ty: PhantomData<(V, E, Ty)>,
}

impl<V, E, Ty: EdgeType> Graph<V, E, Ty, AdjList<V, E, Ty>> {
    pub fn new() -> Self {
        Self::with_storage(AdjList::new())
    }

    pub fn with_capacity(vertex_count: usize, edge_count: usize) -> Self {
        Self::with_storage(AdjList::with_capacity(vertex_count, edge_count))
    }
}

impl<V, E> Graph<V, E, Undirected, AdjList<V, E, Undirected>> {
    pub fn new_undirected() -> Self {
        Self::new()
    }
}

impl<V, E> Graph<V, E, Directed, AdjList<V, E, Directed>> {
    pub fn new_directed() -> Self {
        Self::new()
    }
}

impl<V, E, Ty: EdgeType, G> Default for Graph<V, E, Ty, G>
where
    G: Default,
{
    fn default() -> Self {
        Self::with_storage(G::default())
    }
}

impl<V, E, Ty: EdgeType, G> From<G> for Graph<V, E, Ty, G> {
    fn from(graph: G) -> Self {
        Self::with_storage(graph)
    }
}

impl<V, E, Ty: EdgeType, G> Graph<V, E, Ty, G> {
    pub fn with_storage(graph: G) -> Self {
        Self {
            graph,
            ty: PhantomData,
        }
    }

    pub fn extend_with_edges<T, I>(&mut self, iter: I)
    where
        T: IntoEdge<E, Ty>,
        I: IntoIterator<Item = T>,
        V: Default,
        G: ExtendWithEdges<T, V, E, Ty>,
    {
        self.graph.extend_with_edges(iter)
    }

    pub fn from_edges<T, I>(iter: I) -> Self
    where
        T: IntoEdge<E, Ty>,
        I: IntoIterator<Item = T>,
        V: Default,
        G: ExtendWithEdges<T, V, E, Ty>,
    {
        Self::with_storage(G::from_edges(iter))
    }

    pub fn extend_with_vertices<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = V>,
        G: ExtendWithVertices<V, E, Ty>,
    {
        self.graph.extend_with_vertices(iter)
    }

    pub fn from_vertices<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = V>,
        G: ExtendWithVertices<V, E, Ty>,
    {
        Self::with_storage(G::from_vertices(iter))
    }

    pub fn vertex_count(&self) -> usize
    where
        G: VerticesBase,
    {
        self.graph.vertex_count()
    }

    pub fn vertex_bound(&self) -> usize
    where
        G: VerticesBase,
    {
        self.graph.vertex_bound()
    }

    pub fn vertex_indices(&self) -> G::VertexIndicesIter<'_>
    where
        G: VerticesBase,
    {
        self.graph.vertex_indices()
    }

    pub fn vertex(&self, index: VertexIndex) -> Option<&V>
    where
        G: Vertices<V>,
    {
        self.graph.vertex(index)
    }

    pub fn vertices(&self) -> G::VerticesIter<'_, V>
    where
        G: Vertices<V>,
    {
        self.graph.vertices()
    }

    pub fn vertex_mut(&mut self, index: VertexIndex) -> Option<&mut V>
    where
        G: VerticesMut<V>,
    {
        self.graph.vertex_mut(index)
    }

    pub fn add_vertex(&mut self, vertex: V) -> VertexIndex
    where
        G: VerticesMut<V>,
    {
        self.graph.add_vertex(vertex)
    }

    pub fn remove_vertex(&mut self, index: VertexIndex) -> Option<V>
    where
        G: VerticesMut<V>,
    {
        self.graph.remove_vertex(index)
    }

    pub fn replace_vertex(&mut self, index: VertexIndex, vertex: V) -> V
    where
        G: VerticesMut<V>,
    {
        self.graph.replace_vertex(index, vertex)
    }

    pub fn clear(&mut self)
    where
        G: VerticesMut<V>,
    {
        self.graph.clear()
    }

    pub fn edge_count(&self) -> usize
    where
        G: EdgesBase<Ty>,
    {
        self.graph.edge_count()
    }

    pub fn edge_bound(&self) -> usize
    where
        G: EdgesBase<Ty>,
    {
        self.graph.edge_bound()
    }

    pub fn endpoints(&self, index: EdgeIndex) -> Option<(VertexIndex, VertexIndex)>
    where
        G: EdgesBase<Ty>,
    {
        self.graph.endpoints(index)
    }

    pub fn edge_index(&self, src: VertexIndex, dst: VertexIndex) -> Option<EdgeIndex>
    where
        G: EdgesBase<Ty>,
    {
        self.graph.edge_index(src, dst)
    }

    pub fn edge_indices(&self) -> G::EdgeIndicesIter<'_>
    where
        G: EdgesBase<Ty>,
    {
        self.graph.edge_indices()
    }

    pub fn contains_edge(&self, index: EdgeIndex) -> bool
    where
        G: EdgesBase<Ty>,
    {
        self.graph.contains_edge(index)
    }

    pub fn is_directed(&self) -> bool
    where
        G: EdgesBase<Ty>,
    {
        self.graph.is_directed()
    }

    pub fn edge(&self, index: EdgeIndex) -> Option<&E>
    where
        G: Edges<E, Ty>,
    {
        self.graph.edge(index)
    }

    pub fn edges(&self) -> G::EdgesIter<'_, E>
    where
        G: Edges<E, Ty>,
    {
        self.graph.edges()
    }

    pub fn edge_mut(&mut self, index: EdgeIndex) -> Option<&mut E>
    where
        G: EdgesMut<E, Ty>,
    {
        self.graph.edge_mut(index)
    }

    pub fn add_edge(&mut self, src: VertexIndex, dst: VertexIndex, edge: E) -> EdgeIndex
    where
        G: EdgesMut<E, Ty>,
    {
        self.graph.add_edge(src, dst, edge)
    }

    pub fn remove_edge(&mut self, index: EdgeIndex) -> Option<E>
    where
        G: EdgesMut<E, Ty>,
    {
        self.graph.remove_edge(index)
    }

    pub fn replace_edge(&mut self, index: EdgeIndex, edge: E) -> E
    where
        G: EdgesMut<E, Ty>,
    {
        self.graph.replace_edge(index, edge)
    }

    pub fn clear_edges(&mut self)
    where
        G: EdgesMut<E, Ty>,
    {
        self.graph.clear_edges()
    }

    pub fn multi_edge_index(
        &self,
        src: VertexIndex,
        dst: VertexIndex,
    ) -> G::MultiEdgeIndicesIter<'_>
    where
        G: MultiEdges<E, Ty>,
    {
        self.graph.multi_edge_index(src, dst)
    }

    pub fn neighbors(&self, src: VertexIndex) -> G::NeighborsIter<'_>
    where
        G: Neighbors,
    {
        self.graph.neighbors(src)
    }

    pub fn neighbors_directed(&self, src: VertexIndex, dir: Direction) -> G::NeighborsIter<'_>
    where
        G: Neighbors,
    {
        self.graph.neighbors_directed(src, dir)
    }

    pub fn degree(&self, src: VertexIndex) -> usize
    where
        G: Neighbors,
    {
        self.graph.degree(src)
    }

    pub fn degree_directed(&self, src: VertexIndex, dir: Direction) -> usize
    where
        G: Neighbors,
    {
        self.graph.degree_directed(src, dir)
    }

    pub fn stabilize(self) -> Graph<V, E, Ty, Stable<G>> {
        Graph::with_storage(Stable::new(self.graph))
    }

    pub fn freeze(self) -> Graph<V, E, Ty, Frozen<G>> {
        Graph::with_storage(Frozen::new(self.graph))
    }
}

impl<V, E, Ty: EdgeType, G> Deref for Graph<V, E, Ty, G> {
    type Target = G;

    fn deref(&self) -> &Self::Target {
        &self.graph
    }
}

impl<V, E, Ty: EdgeType, G> DerefMut for Graph<V, E, Ty, G> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.graph
    }
}

impl<V, E, Ty: EdgeType, G> Create<V, E, Ty> for Graph<V, E, Ty, G>
where
    G: Create<V, E, Ty>,
{
    fn with_capacity(vertex_count: usize, edge_count: usize) -> Self {
        Self::with_storage(G::with_capacity(vertex_count, edge_count))
    }
}
