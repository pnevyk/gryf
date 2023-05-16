use std::{
    borrow::Borrow,
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use crate::{
    core::{
        index::DefaultIndexing,
        marker::{Directed, Direction, EdgeType, Undirected},
        AddEdgeError, AddVertexError, ConnectVertices, Create, Edges, EdgesBase, EdgesMut,
        ExtendWithEdges, ExtendWithVertices, GraphBase, IntoEdge, MultiEdges, Neighbors,
        ReplaceEdgeError, ReplaceVertexError, Vertices, VerticesBase, VerticesMut,
    },
    storage::{AdjList, Frozen, Stable},
};

use super::AddEdgeConnectingError;

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
    storage: G,
    ty: PhantomData<(V, E, Ty)>,
}

impl<V, E, Ty: EdgeType> Graph<V, E, Ty> {
    pub fn new() -> Self {
        Self::new_in(AdjList::new())
    }

    pub fn with_capacity(vertex_count: usize, edge_count: usize) -> Self {
        Self::new_in(AdjList::with_capacity(vertex_count, edge_count))
    }
}

impl<V, E> Graph<V, E, Undirected> {
    pub fn new_undirected() -> Self {
        Self::new()
    }
}

impl<V, E, G> Graph<V, E, Undirected, G> {
    pub fn new_undirected_in(storage: G) -> Self {
        Self::new_in(storage)
    }
}

impl<V, E> Graph<V, E, Directed> {
    pub fn new_directed() -> Self {
        Self::new()
    }
}

impl<V, E, G> Graph<V, E, Directed, G> {
    pub fn new_directed_in(storage: G) -> Self {
        Self::new_in(storage)
    }
}

impl<V, E, Ty: EdgeType, G> Default for Graph<V, E, Ty, G>
where
    G: Default,
{
    fn default() -> Self {
        Self::new_in(G::default())
    }
}

impl<V, E, Ty: EdgeType, G> From<G> for Graph<V, E, Ty, G> {
    fn from(storage: G) -> Self {
        Self::new_in(storage)
    }
}

impl<V, E, Ty: EdgeType, G> Graph<V, E, Ty, G> {
    pub fn new_in(storage: G) -> Self {
        Self {
            storage,
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
        self.storage.extend_with_edges(iter)
    }

    pub fn from_edges<T, I>(iter: I) -> Self
    where
        T: IntoEdge<G, E, Ty>,
        I: IntoIterator<Item = T>,
        V: Default,
        G: ExtendWithEdges<T, V, E, Ty>,
    {
        Self::new_in(G::from_edges(iter))
    }

    pub fn extend_with_vertices<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = V>,
        G: ExtendWithVertices<V, E, Ty>,
    {
        self.storage.extend_with_vertices(iter)
    }

    pub fn from_vertices<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = V>,
        G: ExtendWithVertices<V, E, Ty>,
    {
        Self::new_in(G::from_vertices(iter))
    }

    pub fn vertex_count(&self) -> usize
    where
        G: VerticesBase,
    {
        self.storage.vertex_count()
    }

    pub fn vertex_bound(&self) -> usize
    where
        G: VerticesBase,
    {
        self.storage.vertex_bound()
    }

    pub fn vertex_indices(&self) -> G::VertexIndicesIter<'_>
    where
        G: VerticesBase,
    {
        self.storage.vertex_indices()
    }

    pub fn vertex<VI>(&self, index: VI) -> Option<&V>
    where
        G: Vertices<V>,
        VI: Borrow<G::VertexIndex>,
    {
        self.storage.vertex(index.borrow())
    }

    pub fn vertices(&self) -> G::VerticesIter<'_>
    where
        G: Vertices<V>,
    {
        self.storage.vertices()
    }

    pub fn find_vertex(&self, vertex: &V) -> Option<G::VertexIndex>
    where
        G: Vertices<V>,
        V: Eq,
    {
        self.storage.find_vertex(vertex)
    }

    pub fn vertex_mut<VI>(&mut self, index: VI) -> Option<&mut V>
    where
        G: VerticesMut<V>,
        VI: Borrow<G::VertexIndex>,
    {
        self.storage.vertex_mut(index.borrow())
    }

    pub fn add_vertex(&mut self, vertex: V) -> G::VertexIndex
    where
        G: VerticesMut<V>,
    {
        self.storage.add_vertex(vertex)
    }

    pub fn try_add_vertex(&mut self, vertex: V) -> Result<G::VertexIndex, AddVertexError<V>>
    where
        G: VerticesMut<V>,
    {
        self.storage.try_add_vertex(vertex)
    }

    pub fn remove_vertex<VI>(&mut self, index: VI) -> Option<V>
    where
        G: VerticesMut<V>,
        VI: Borrow<G::VertexIndex>,
    {
        self.storage.remove_vertex(index.borrow())
    }

    pub fn replace_vertex<VI>(&mut self, index: VI, vertex: V) -> V
    where
        G: VerticesMut<V>,
        VI: Borrow<G::VertexIndex>,
    {
        self.storage.replace_vertex(index.borrow(), vertex)
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
        self.storage.try_replace_vertex(index.borrow(), vertex)
    }

    pub fn clear(&mut self)
    where
        G: VerticesMut<V>,
    {
        self.storage.clear()
    }

    pub fn try_get_or_add_vertex(&mut self, vertex: V) -> Result<G::VertexIndex, AddVertexError<V>>
    where
        G: VerticesMut<V>,
        V: Eq,
    {
        self.storage.try_get_or_add_vertex(vertex)
    }

    pub fn get_or_add_vertex(&mut self, vertex: V) -> G::VertexIndex
    where
        G: VerticesMut<V>,
        V: Eq,
    {
        self.storage.get_or_add_vertex(vertex)
    }

    pub fn edge_count(&self) -> usize
    where
        G: EdgesBase<Ty>,
    {
        self.storage.edge_count()
    }

    pub fn edge_bound(&self) -> usize
    where
        G: EdgesBase<Ty>,
    {
        self.storage.edge_bound()
    }

    pub fn endpoints<EI>(&self, index: EI) -> Option<(G::VertexIndex, G::VertexIndex)>
    where
        G: EdgesBase<Ty>,
        EI: Borrow<G::EdgeIndex>,
    {
        self.storage.endpoints(index.borrow())
    }

    pub fn edge_index<VI>(&self, src: VI, dst: VI) -> G::EdgeIndexIter<'_>
    where
        G: EdgesBase<Ty>,
        VI: Borrow<G::VertexIndex>,
    {
        self.storage.edge_index(src.borrow(), dst.borrow())
    }

    pub fn edge_index_any<VI>(&self, src: VI, dst: VI) -> Option<G::EdgeIndex>
    where
        G: EdgesBase<Ty>,
        VI: Borrow<G::VertexIndex>,
    {
        self.storage.edge_index_any(src.borrow(), dst.borrow())
    }

    pub fn edge_indices(&self) -> G::EdgeIndicesIter<'_>
    where
        G: EdgesBase<Ty>,
    {
        self.storage.edge_indices()
    }

    pub fn contains_edge<EI>(&self, index: EI) -> bool
    where
        G: EdgesBase<Ty>,
        EI: Borrow<G::EdgeIndex>,
    {
        self.storage.contains_edge(index.borrow())
    }

    pub fn is_directed(&self) -> bool
    where
        G: EdgesBase<Ty>,
    {
        self.storage.is_directed()
    }

    pub fn edge<EI>(&self, index: EI) -> Option<&E>
    where
        G: Edges<E, Ty>,
        EI: Borrow<G::EdgeIndex>,
    {
        self.storage.edge(index.borrow())
    }

    pub fn edges(&self) -> G::EdgesIter<'_>
    where
        G: Edges<E, Ty>,
    {
        self.storage.edges()
    }

    pub fn edge_mut<EI>(&mut self, index: EI) -> Option<&mut E>
    where
        G: EdgesMut<E, Ty>,
        EI: Borrow<G::EdgeIndex>,
    {
        self.storage.edge_mut(index.borrow())
    }

    pub fn add_edge<VI>(&mut self, src: VI, dst: VI, edge: E) -> G::EdgeIndex
    where
        G: EdgesMut<E, Ty>,
        VI: Borrow<G::VertexIndex>,
    {
        self.storage.add_edge(src.borrow(), dst.borrow(), edge)
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
        self.storage.try_add_edge(src.borrow(), dst.borrow(), edge)
    }

    pub fn try_add_edge_connecting(
        &mut self,
        src: V,
        dst: V,
        edge: E,
    ) -> Result<G::EdgeIndex, AddEdgeConnectingError<V, E>>
    where
        G: VerticesMut<V> + EdgesMut<E, Ty>,
        V: Eq,
    {
        let src = self.storage.try_get_or_add_vertex(src)?;
        let dst = self.storage.try_get_or_add_vertex(dst)?;
        let edge = self.storage.try_add_edge(&src, &dst, edge)?;
        Ok(edge)
    }

    pub fn add_edge_connecting(&mut self, src: V, dst: V, edge: E) -> G::EdgeIndex
    where
        G: VerticesMut<V> + EdgesMut<E, Ty>,
        V: Eq,
    {
        match self.try_add_edge_connecting(src, dst, edge) {
            Ok(index) => index,
            Err(error) => panic!("{error}"),
        }
    }

    pub fn remove_edge<EI>(&mut self, index: EI) -> Option<E>
    where
        G: EdgesMut<E, Ty>,
        EI: Borrow<G::EdgeIndex>,
    {
        self.storage.remove_edge(index.borrow())
    }

    pub fn remove_edge_between<VI>(&mut self, src: VI, dst: VI) -> Option<E>
    where
        G: EdgesMut<E, Ty>,
        VI: Borrow<G::VertexIndex>,
    {
        self.storage.remove_edge_between(src.borrow(), dst.borrow())
    }

    pub fn replace_edge<EI>(&mut self, index: EI, edge: E) -> E
    where
        G: EdgesMut<E, Ty>,
        EI: Borrow<G::EdgeIndex>,
    {
        self.storage.replace_edge(index.borrow(), edge)
    }

    pub fn try_replace_edge<EI>(&mut self, index: EI, edge: E) -> Result<E, ReplaceEdgeError<E>>
    where
        G: EdgesMut<E, Ty>,
        EI: Borrow<G::EdgeIndex>,
    {
        self.storage.try_replace_edge(index.borrow(), edge)
    }

    pub fn clear_edges(&mut self)
    where
        G: EdgesMut<E, Ty>,
    {
        self.storage.clear_edges()
    }

    pub fn neighbors<VI>(&self, src: VI) -> G::NeighborsIter<'_>
    where
        G: Neighbors,
        VI: Borrow<G::VertexIndex>,
    {
        self.storage.neighbors(src.borrow())
    }

    pub fn neighbors_directed<VI>(&self, src: VI, dir: Direction) -> G::NeighborsIter<'_>
    where
        G: Neighbors,
        VI: Borrow<G::VertexIndex>,
    {
        self.storage.neighbors_directed(src.borrow(), dir)
    }

    pub fn degree<VI>(&self, src: VI) -> usize
    where
        G: Neighbors,
        VI: Borrow<G::VertexIndex>,
    {
        self.storage.degree(src.borrow())
    }

    pub fn degree_directed<VI>(&self, src: VI, dir: Direction) -> usize
    where
        G: Neighbors,
        VI: Borrow<G::VertexIndex>,
    {
        self.storage.degree_directed(src.borrow(), dir)
    }

    pub fn stabilize(self) -> Graph<V, E, Ty, Stable<G>>
    where
        G: GraphBase,
    {
        Graph::new_in(Stable::new(self.storage))
    }

    pub fn freeze(self) -> Graph<V, E, Ty, Frozen<G>> {
        Graph::new_in(Frozen::new(self.storage))
    }

    pub fn connect_vertices<F>(&mut self, connect: F)
    where
        G: ConnectVertices<V, E, Ty>,
        F: FnMut(&V, &V) -> Option<E>,
    {
        self.storage.connect_vertices(connect);
    }
}

impl<V, E, Ty: EdgeType, G> Deref for Graph<V, E, Ty, G> {
    type Target = G;

    fn deref(&self) -> &Self::Target {
        &self.storage
    }
}

impl<V, E, Ty: EdgeType, G> DerefMut for Graph<V, E, Ty, G> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.storage
    }
}

impl<V, E, Ty: EdgeType, G> Create<V, E, Ty> for Graph<V, E, Ty, G>
where
    G: Create<V, E, Ty>,
{
    fn with_capacity(vertex_count: usize, edge_count: usize) -> Self {
        Self::new_in(G::with_capacity(vertex_count, edge_count))
    }
}
