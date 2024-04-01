use std::{
    borrow::Borrow,
    marker::PhantomData,
    ops::{Deref, DerefMut, Index, IndexMut},
};

use crate::{
    core::{
        id::DefaultId,
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
use crate::common::CompactIdMap;
use crate::core::{
    id::NumIdType, EdgesBaseWeak, EdgesWeak, Guarantee, VerticesBaseWeak, VerticesWeak, WeakRef,
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
pub struct Graph<V, E, Ty: EdgeType, G = AdjList<V, E, Ty, DefaultId>> {
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

    pub fn vertex_ids(&self) -> G::VertexIdsIter<'_>
    where
        G: VerticesBase,
    {
        self.storage.vertex_ids()
    }

    pub fn vertex<VId>(&self, index: VId) -> Option<&V>
    where
        G: Vertices<V>,
        VId: Borrow<G::VertexId>,
    {
        self.storage.vertex(index.borrow())
    }

    pub fn vertices(&self) -> G::VerticesIter<'_>
    where
        G: Vertices<V>,
    {
        self.storage.vertices()
    }

    pub fn find_vertex(&self, vertex: &V) -> Option<G::VertexId>
    where
        G: Vertices<V>,
        V: Eq,
    {
        self.storage.find_vertex(vertex)
    }

    pub fn vertex_mut<VId>(&mut self, index: VId) -> Option<&mut V>
    where
        G: VerticesMut<V>,
        VId: Borrow<G::VertexId>,
    {
        self.storage.vertex_mut(index.borrow())
    }

    pub fn add_vertex(&mut self, vertex: V) -> G::VertexId
    where
        G: VerticesMut<V>,
    {
        self.storage.add_vertex(vertex)
    }

    pub fn try_add_vertex(&mut self, vertex: V) -> Result<G::VertexId, AddVertexError<V>>
    where
        G: VerticesMut<V>,
    {
        self.storage.try_add_vertex(vertex)
    }

    pub fn remove_vertex<VId>(&mut self, index: VId) -> Option<V>
    where
        G: VerticesMut<V>,
        VId: Borrow<G::VertexId>,
    {
        self.storage.remove_vertex(index.borrow())
    }

    pub fn replace_vertex<VId>(&mut self, index: VId, vertex: V) -> V
    where
        G: VerticesMut<V>,
        VId: Borrow<G::VertexId>,
    {
        self.storage.replace_vertex(index.borrow(), vertex)
    }

    pub fn try_replace_vertex<VId>(
        &mut self,
        index: VId,
        vertex: V,
    ) -> Result<V, ReplaceVertexError<V>>
    where
        G: VerticesMut<V>,
        VId: Borrow<G::VertexId>,
    {
        self.storage.try_replace_vertex(index.borrow(), vertex)
    }

    pub fn clear(&mut self)
    where
        G: VerticesMut<V>,
    {
        self.storage.clear()
    }

    pub fn try_get_or_add_vertex(&mut self, vertex: V) -> Result<G::VertexId, AddVertexError<V>>
    where
        G: VerticesMut<V>,
        V: Eq,
    {
        self.storage.try_get_or_add_vertex(vertex)
    }

    pub fn get_or_add_vertex(&mut self, vertex: V) -> G::VertexId
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

    pub fn endpoints<EId>(&self, index: EId) -> Option<(G::VertexId, G::VertexId)>
    where
        G: EdgesBase<Ty>,
        EId: Borrow<G::EdgeId>,
    {
        self.storage.endpoints(index.borrow())
    }

    pub fn edge_id<VId>(&self, src: VId, dst: VId) -> G::EdgeIdIter<'_>
    where
        G: EdgesBase<Ty>,
        VId: Borrow<G::VertexId>,
    {
        self.storage.edge_id(src.borrow(), dst.borrow())
    }

    pub fn edge_id_any<VId>(&self, src: VId, dst: VId) -> Option<G::EdgeId>
    where
        G: EdgesBase<Ty>,
        VId: Borrow<G::VertexId>,
    {
        self.storage.edge_id_any(src.borrow(), dst.borrow())
    }

    pub fn edge_ids(&self) -> G::EdgeIdsIter<'_>
    where
        G: EdgesBase<Ty>,
    {
        self.storage.edge_ids()
    }

    pub fn contains_edge<EId>(&self, index: EId) -> bool
    where
        G: EdgesBase<Ty>,
        EId: Borrow<G::EdgeId>,
    {
        self.storage.contains_edge(index.borrow())
    }

    pub fn is_directed(&self) -> bool
    where
        G: EdgesBase<Ty>,
    {
        self.storage.is_directed()
    }

    pub fn edge<EId>(&self, index: EId) -> Option<&E>
    where
        G: Edges<E, Ty>,
        EId: Borrow<G::EdgeId>,
    {
        self.storage.edge(index.borrow())
    }

    pub fn edges(&self) -> G::EdgesIter<'_>
    where
        G: Edges<E, Ty>,
    {
        self.storage.edges()
    }

    pub fn edge_mut<EId>(&mut self, index: EId) -> Option<&mut E>
    where
        G: EdgesMut<E, Ty>,
        EId: Borrow<G::EdgeId>,
    {
        self.storage.edge_mut(index.borrow())
    }

    pub fn add_edge<VId>(&mut self, src: VId, dst: VId, edge: E) -> G::EdgeId
    where
        G: EdgesMut<E, Ty>,
        VId: Borrow<G::VertexId>,
    {
        self.storage.add_edge(src.borrow(), dst.borrow(), edge)
    }

    pub fn try_add_edge<VId>(
        &mut self,
        src: VId,
        dst: VId,
        edge: E,
    ) -> Result<G::EdgeId, AddEdgeError<E>>
    where
        G: EdgesMut<E, Ty>,
        VId: Borrow<G::VertexId>,
    {
        self.storage.try_add_edge(src.borrow(), dst.borrow(), edge)
    }

    pub fn try_add_edge_connecting(
        &mut self,
        src: V,
        dst: V,
        edge: E,
    ) -> Result<G::EdgeId, AddEdgeConnectingError<V, E>>
    where
        G: VerticesMut<V> + EdgesMut<E, Ty>,
        V: Eq,
    {
        let src = self.storage.try_get_or_add_vertex(src)?;
        let dst = self.storage.try_get_or_add_vertex(dst)?;
        let edge = self.storage.try_add_edge(&src, &dst, edge)?;
        Ok(edge)
    }

    pub fn add_edge_connecting(&mut self, src: V, dst: V, edge: E) -> G::EdgeId
    where
        G: VerticesMut<V> + EdgesMut<E, Ty>,
        V: Eq,
    {
        match self.try_add_edge_connecting(src, dst, edge) {
            Ok(index) => index,
            Err(error) => panic!("{error}"),
        }
    }

    pub fn remove_edge<EId>(&mut self, index: EId) -> Option<E>
    where
        G: EdgesMut<E, Ty>,
        EId: Borrow<G::EdgeId>,
    {
        self.storage.remove_edge(index.borrow())
    }

    pub fn remove_edge_between<VId>(&mut self, src: VId, dst: VId) -> Option<E>
    where
        G: EdgesMut<E, Ty>,
        VId: Borrow<G::VertexId>,
    {
        self.storage.remove_edge_between(src.borrow(), dst.borrow())
    }

    pub fn replace_edge<EId>(&mut self, index: EId, edge: E) -> E
    where
        G: EdgesMut<E, Ty>,
        EId: Borrow<G::EdgeId>,
    {
        self.storage.replace_edge(index.borrow(), edge)
    }

    pub fn try_replace_edge<EId>(&mut self, index: EId, edge: E) -> Result<E, ReplaceEdgeError<E>>
    where
        G: EdgesMut<E, Ty>,
        EId: Borrow<G::EdgeId>,
    {
        self.storage.try_replace_edge(index.borrow(), edge)
    }

    pub fn clear_edges(&mut self)
    where
        G: EdgesMut<E, Ty>,
    {
        self.storage.clear_edges()
    }

    pub fn neighbors<VId>(&self, src: VId) -> G::NeighborsIter<'_>
    where
        G: Neighbors,
        VId: Borrow<G::VertexId>,
    {
        self.storage.neighbors(src.borrow())
    }

    pub fn neighbors_directed<VId>(&self, src: VId, dir: Direction) -> G::NeighborsIter<'_>
    where
        G: Neighbors,
        VId: Borrow<G::VertexId>,
    {
        self.storage.neighbors_directed(src.borrow(), dir)
    }

    pub fn degree<VId>(&self, src: VId) -> usize
    where
        G: Neighbors,
        VId: Borrow<G::VertexId>,
    {
        self.storage.degree(src.borrow())
    }

    pub fn degree_directed<VId>(&self, src: VId, dir: Direction) -> usize
    where
        G: Neighbors,
        VId: Borrow<G::VertexId>,
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

impl<V, E, Ty: EdgeType, G, VId> Index<VId> for Graph<V, E, Ty, G>
where
    G: Vertices<V>,
    VId: Borrow<G::VertexId>,
{
    type Output = V;

    fn index(&self, index: VId) -> &Self::Output {
        self.vertex(index).expect("vertex does not exist")
    }
}

impl<V, E, Ty: EdgeType, G, VId> IndexMut<VId> for Graph<V, E, Ty, G>
where
    G: VerticesMut<V>,
    VId: Borrow<G::VertexId>,
{
    fn index_mut(&mut self, index: VId) -> &mut Self::Output {
        self.vertex_mut(index).expect("vertex does not exist")
    }
}
