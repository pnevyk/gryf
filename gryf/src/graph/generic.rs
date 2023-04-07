use std::{
    borrow::Borrow,
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use crate::core::{marker::Direction, ConnectVertices, ReplaceEdgeError, ReplaceVertexError};
use crate::core::{
    Create, Edges, EdgesBase, EdgesMut, GraphBase, MultiEdges, Neighbors, Vertices, VerticesBase,
    VerticesMut,
};
use crate::storage::{Frozen, Stable};
use crate::{
    core::{
        index::DefaultIndexing,
        marker::{Directed, EdgeType, Undirected},
        AddEdgeError, AddVertexError, ExtendWithEdges, ExtendWithVertices, IntoEdge,
    },
    storage::AdjList,
};

use gryf_derive::{
    Edges, EdgesBase, EdgesBaseWeak, EdgesMut, EdgesWeak, GraphBase, Guarantee, MultiEdges,
    Neighbors, Vertices, VerticesBase, VerticesBaseWeak, VerticesMut, VerticesWeak,
};

// TODO: Remove these imports once hygiene of procedural macros is fixed.
use crate::common::CompactIndexMap;
use crate::core::{
    index::NumIndexType, EdgesBaseWeak, EdgesWeak, Guarantee, VerticesBaseWeak, VerticesWeak,
    WeakRef,
};

#[derive(
    Debug,
    Clone,
    GraphBase,
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
pub struct Graph<V, E, Ty: EdgeType, G = AdjList<V, E, Ty, DefaultIndexing>> {
    #[graph]
    graph: G,
    ty: PhantomData<(V, E, Ty)>,
}

impl<V, E, Ty: EdgeType> Graph<V, E, Ty, AdjList<V, E, Ty, DefaultIndexing>> {
    pub fn new() -> Self {
        Self::with_storage(AdjList::new())
    }

    pub fn with_capacity(vertex_count: usize, edge_count: usize) -> Self {
        Self::with_storage(AdjList::with_capacity(vertex_count, edge_count))
    }
}

impl<V, E> Graph<V, E, Undirected, AdjList<V, E, Undirected, DefaultIndexing>> {
    pub fn new_undirected() -> Self {
        Self::new()
    }
}

impl<V, E> Graph<V, E, Directed, AdjList<V, E, Directed, DefaultIndexing>> {
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
        T: IntoEdge<G, E, Ty>,
        I: IntoIterator<Item = T>,
        V: Default,
        G: ExtendWithEdges<T, V, E, Ty>,
    {
        self.graph.extend_with_edges(iter)
    }

    pub fn from_edges<T, I>(iter: I) -> Self
    where
        T: IntoEdge<G, E, Ty>,
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

    pub fn vertex<VI>(&self, index: VI) -> Option<&V>
    where
        G: Vertices<V>,
        VI: Borrow<G::VertexIndex>,
    {
        self.graph.vertex(index.borrow())
    }

    pub fn vertices(&self) -> G::VerticesIter<'_>
    where
        G: Vertices<V>,
    {
        self.graph.vertices()
    }

    pub fn vertex_mut<VI>(&mut self, index: VI) -> Option<&mut V>
    where
        G: VerticesMut<V>,
        VI: Borrow<G::VertexIndex>,
    {
        self.graph.vertex_mut(index.borrow())
    }

    pub fn add_vertex(&mut self, vertex: V) -> G::VertexIndex
    where
        G: VerticesMut<V>,
    {
        self.graph.add_vertex(vertex)
    }

    pub fn try_add_vertex(&mut self, vertex: V) -> Result<G::VertexIndex, AddVertexError<V>>
    where
        G: VerticesMut<V>,
    {
        self.graph.try_add_vertex(vertex)
    }

    pub fn remove_vertex<VI>(&mut self, index: VI) -> Option<V>
    where
        G: VerticesMut<V>,
        VI: Borrow<G::VertexIndex>,
    {
        self.graph.remove_vertex(index.borrow())
    }

    pub fn replace_vertex<VI>(&mut self, index: VI, vertex: V) -> V
    where
        G: VerticesMut<V>,
        VI: Borrow<G::VertexIndex>,
    {
        self.graph.replace_vertex(index.borrow(), vertex)
    }

    pub fn try_replace_vertex<VI>(
        &mut self,
        index: VI,
        vertex: V,
    ) -> Result<V, ReplaceVertexError<V>>
    where
        G: VerticesMut<V>,
        VI: Borrow<G::VertexIndex>,
    {
        self.graph.try_replace_vertex(index.borrow(), vertex)
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

    pub fn endpoints<EI>(&self, index: EI) -> Option<(G::VertexIndex, G::VertexIndex)>
    where
        G: EdgesBase<Ty>,
        EI: Borrow<G::EdgeIndex>,
    {
        self.graph.endpoints(index.borrow())
    }

    pub fn edge_index<VI>(&self, src: VI, dst: VI) -> G::EdgeIndexIter<'_>
    where
        G: EdgesBase<Ty>,
        VI: Borrow<G::VertexIndex>,
    {
        self.graph.edge_index(src.borrow(), dst.borrow())
    }

    pub fn edge_index_any<VI>(&self, src: VI, dst: VI) -> Option<G::EdgeIndex>
    where
        G: EdgesBase<Ty>,
        VI: Borrow<G::VertexIndex>,
    {
        self.graph.edge_index_any(src.borrow(), dst.borrow())
    }

    pub fn edge_indices(&self) -> G::EdgeIndicesIter<'_>
    where
        G: EdgesBase<Ty>,
    {
        self.graph.edge_indices()
    }

    pub fn contains_edge<EI>(&self, index: EI) -> bool
    where
        G: EdgesBase<Ty>,
        EI: Borrow<G::EdgeIndex>,
    {
        self.graph.contains_edge(index.borrow())
    }

    pub fn is_directed(&self) -> bool
    where
        G: EdgesBase<Ty>,
    {
        self.graph.is_directed()
    }

    pub fn edge<EI>(&self, index: EI) -> Option<&E>
    where
        G: Edges<E, Ty>,
        EI: Borrow<G::EdgeIndex>,
    {
        self.graph.edge(index.borrow())
    }

    pub fn edges(&self) -> G::EdgesIter<'_>
    where
        G: Edges<E, Ty>,
    {
        self.graph.edges()
    }

    pub fn edge_mut<EI>(&mut self, index: EI) -> Option<&mut E>
    where
        G: EdgesMut<E, Ty>,
        EI: Borrow<G::EdgeIndex>,
    {
        self.graph.edge_mut(index.borrow())
    }

    pub fn add_edge<VI>(&mut self, src: VI, dst: VI, edge: E) -> G::EdgeIndex
    where
        G: EdgesMut<E, Ty>,
        VI: Borrow<G::VertexIndex>,
    {
        self.graph.add_edge(src.borrow(), dst.borrow(), edge)
    }

    pub fn try_add_edge<VI>(
        &mut self,
        src: VI,
        dst: VI,
        edge: E,
    ) -> Result<G::EdgeIndex, AddEdgeError<E>>
    where
        G: EdgesMut<E, Ty>,
        VI: Borrow<G::VertexIndex>,
    {
        self.graph.try_add_edge(src.borrow(), dst.borrow(), edge)
    }

    pub fn remove_edge<EI>(&mut self, index: EI) -> Option<E>
    where
        G: EdgesMut<E, Ty>,
        EI: Borrow<G::EdgeIndex>,
    {
        self.graph.remove_edge(index.borrow())
    }

    pub fn remove_edge_between<VI>(&mut self, src: VI, dst: VI) -> Option<E>
    where
        G: EdgesMut<E, Ty>,
        VI: Borrow<G::VertexIndex>,
    {
        self.graph.remove_edge_between(src.borrow(), dst.borrow())
    }

    pub fn replace_edge<EI>(&mut self, index: EI, edge: E) -> E
    where
        G: EdgesMut<E, Ty>,
        EI: Borrow<G::EdgeIndex>,
    {
        self.graph.replace_edge(index.borrow(), edge)
    }

    pub fn try_replace_edge<EI>(&mut self, index: EI, edge: E) -> Result<E, ReplaceEdgeError<E>>
    where
        G: EdgesMut<E, Ty>,
        EI: Borrow<G::EdgeIndex>,
    {
        self.graph.try_replace_edge(index.borrow(), edge)
    }

    pub fn clear_edges(&mut self)
    where
        G: EdgesMut<E, Ty>,
    {
        self.graph.clear_edges()
    }

    pub fn neighbors<VI>(&self, src: VI) -> G::NeighborsIter<'_>
    where
        G: Neighbors,
        VI: Borrow<G::VertexIndex>,
    {
        self.graph.neighbors(src.borrow())
    }

    pub fn neighbors_directed<VI>(&self, src: VI, dir: Direction) -> G::NeighborsIter<'_>
    where
        G: Neighbors,
        VI: Borrow<G::VertexIndex>,
    {
        self.graph.neighbors_directed(src.borrow(), dir)
    }

    pub fn degree<VI>(&self, src: VI) -> usize
    where
        G: Neighbors,
        VI: Borrow<G::VertexIndex>,
    {
        self.graph.degree(src.borrow())
    }

    pub fn degree_directed<VI>(&self, src: VI, dir: Direction) -> usize
    where
        G: Neighbors,
        VI: Borrow<G::VertexIndex>,
    {
        self.graph.degree_directed(src.borrow(), dir)
    }

    pub fn stabilize(self) -> Graph<V, E, Ty, Stable<G>>
    where
        G: GraphBase,
    {
        Graph::with_storage(Stable::new(self.graph))
    }

    pub fn freeze(self) -> Graph<V, E, Ty, Frozen<G>> {
        Graph::with_storage(Frozen::new(self.graph))
    }

    pub fn connect_vertices<F>(&mut self, connect: F)
    where
        G: ConnectVertices<V, E, Ty>,
        F: FnMut(&V, &V) -> Option<E>,
    {
        self.graph.connect_vertices(connect);
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
