use std::{
    borrow::Borrow,
    marker::PhantomData,
    ops::{Deref, DerefMut, Index, IndexMut},
};

use crate::{
    core::{
        base::IntoEdge,
        connect::ConnectVertices,
        create::{Create, ExtendWithEdges, ExtendWithVertices},
        error::{
            AddEdgeConnectingError, AddEdgeError, AddVertexError, ReplaceEdgeError,
            ReplaceVertexError,
        },
        id::{DefaultId, IntegerIdType},
        marker::{Directed, Direction, EdgeType, Undirected},
        EdgeSet, GraphAdd, GraphBase, GraphFull, GraphMut, GraphRef, Neighbors, VertexSet,
    },
    storage::{AdjList, Frozen, Stable},
};

use gryf_derive::{
    EdgeSet, GraphAdd, GraphBase, GraphFull, GraphMut, GraphRef, Guarantee, MultiEdge, Neighbors,
    VertexSet,
};

#[derive(
    Debug,
    Clone,
    GraphBase,
    Neighbors,
    VertexSet,
    EdgeSet,
    GraphRef,
    GraphMut,
    GraphAdd,
    GraphFull,
    MultiEdge,
    Guarantee,
)]
#[gryf_crate]
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

impl<V, E, G> Graph<V, E, Undirected, G>
where
    G: GraphBase<EdgeType = Undirected>,
{
    pub fn new_undirected_in(storage: G) -> Self {
        Self::new_in(storage)
    }
}

impl<V, E> Graph<V, E, Directed> {
    pub fn new_directed() -> Self {
        Self::new()
    }
}

impl<V, E, G> Graph<V, E, Directed, G>
where
    G: GraphBase<EdgeType = Directed>,
{
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
        T: IntoEdge<G, E>,
        I: IntoIterator<Item = T>,
        G: ExtendWithEdges<T, V, E>,
    {
        self.storage.extend_with_edges(iter)
    }

    pub fn extend_with_vertices<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = V>,
        G: ExtendWithVertices<V, E>,
    {
        self.storage.extend_with_vertices(iter)
    }

    pub fn from_vertices<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = V>,
        G: ExtendWithVertices<V, E>,
    {
        Self::new_in(G::from_vertices(iter))
    }

    pub fn vertex_count(&self) -> usize
    where
        G: VertexSet,
    {
        self.storage.vertex_count()
    }

    pub fn vertex_bound(&self) -> usize
    where
        G: VertexSet,
        G::VertexId: IntegerIdType,
    {
        self.storage.vertex_bound()
    }

    pub fn vertices_by_id(&self) -> G::VerticesByIdIter<'_>
    where
        G: VertexSet,
    {
        self.storage.vertices_by_id()
    }

    pub fn vertex<VId>(&self, id: VId) -> Option<&V>
    where
        G: GraphRef<V, E>,
        VId: Borrow<G::VertexId>,
    {
        self.storage.vertex(id.borrow())
    }

    pub fn vertices(&self) -> G::VerticesIter<'_>
    where
        G: GraphRef<V, E>,
    {
        self.storage.vertices()
    }

    pub fn find_vertex(&self, vertex: &V) -> Option<G::VertexId>
    where
        G: GraphRef<V, E>,
        V: Eq,
    {
        self.storage.find_vertex(vertex)
    }

    pub fn vertex_mut<VId>(&mut self, id: VId) -> Option<&mut V>
    where
        G: GraphMut<V, E>,
        VId: Borrow<G::VertexId>,
    {
        self.storage.vertex_mut(id.borrow())
    }

    pub fn add_vertex(&mut self, vertex: V) -> G::VertexId
    where
        G: GraphAdd<V, E>,
    {
        self.storage.add_vertex(vertex)
    }

    pub fn try_add_vertex(&mut self, vertex: V) -> Result<G::VertexId, AddVertexError<V>>
    where
        G: GraphAdd<V, E>,
    {
        self.storage.try_add_vertex(vertex)
    }

    pub fn remove_vertex<VId>(&mut self, id: VId) -> Option<V>
    where
        G: GraphFull<V, E>,
        VId: Borrow<G::VertexId>,
    {
        self.storage.remove_vertex(id.borrow())
    }

    pub fn replace_vertex<VId>(&mut self, id: VId, vertex: V) -> V
    where
        G: GraphMut<V, E>,
        VId: Borrow<G::VertexId>,
    {
        self.storage.replace_vertex(id.borrow(), vertex)
    }

    pub fn try_replace_vertex<VId>(
        &mut self,
        id: VId,
        vertex: V,
    ) -> Result<V, ReplaceVertexError<V>>
    where
        G: GraphMut<V, E>,
        VId: Borrow<G::VertexId>,
    {
        self.storage.try_replace_vertex(id.borrow(), vertex)
    }

    pub fn clear(&mut self)
    where
        G: GraphFull<V, E>,
    {
        self.storage.clear()
    }

    pub fn try_get_or_add_vertex(&mut self, vertex: V) -> Result<G::VertexId, AddVertexError<V>>
    where
        G: GraphAdd<V, E>,
        V: Eq,
    {
        self.storage.try_get_or_add_vertex(vertex)
    }

    pub fn get_or_add_vertex(&mut self, vertex: V) -> G::VertexId
    where
        G: GraphAdd<V, E>,
        V: Eq,
    {
        self.storage.get_or_add_vertex(vertex)
    }

    pub fn edge_count(&self) -> usize
    where
        G: EdgeSet,
    {
        self.storage.edge_count()
    }

    pub fn edge_bound(&self) -> usize
    where
        G: EdgeSet,
        G::EdgeId: IntegerIdType,
    {
        self.storage.edge_bound()
    }

    pub fn endpoints<EId>(&self, id: EId) -> Option<(G::VertexId, G::VertexId)>
    where
        G: EdgeSet,
        EId: Borrow<G::EdgeId>,
    {
        self.storage.endpoints(id.borrow())
    }

    pub fn edge_id<VId>(&self, src: VId, dst: VId) -> G::EdgeIdIter<'_>
    where
        G: EdgeSet,
        VId: Borrow<G::VertexId>,
    {
        self.storage.edge_id(src.borrow(), dst.borrow())
    }

    pub fn edge_id_any<VId>(&self, src: VId, dst: VId) -> Option<G::EdgeId>
    where
        G: EdgeSet,
        VId: Borrow<G::VertexId>,
    {
        self.storage.edge_id_any(src.borrow(), dst.borrow())
    }

    pub fn edges_by_id(&self) -> G::EdgesByIdIter<'_>
    where
        G: EdgeSet,
    {
        self.storage.edges_by_id()
    }

    pub fn contains_edge<EId>(&self, id: EId) -> bool
    where
        G: EdgeSet,
        EId: Borrow<G::EdgeId>,
    {
        self.storage.contains_edge(id.borrow())
    }

    pub fn is_directed(&self) -> bool
    where
        G: GraphBase,
    {
        self.storage.is_directed()
    }

    pub fn edge<EId>(&self, id: EId) -> Option<&E>
    where
        G: GraphRef<V, E>,
        EId: Borrow<G::EdgeId>,
    {
        self.storage.edge(id.borrow())
    }

    pub fn edges(&self) -> G::EdgesIter<'_>
    where
        G: GraphRef<V, E>,
    {
        self.storage.edges()
    }

    pub fn edge_mut<EId>(&mut self, id: EId) -> Option<&mut E>
    where
        G: GraphMut<V, E>,
        EId: Borrow<G::EdgeId>,
    {
        self.storage.edge_mut(id.borrow())
    }

    pub fn add_edge<VId>(&mut self, src: VId, dst: VId, edge: E) -> G::EdgeId
    where
        G: GraphAdd<V, E>,
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
        G: GraphAdd<V, E>,
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
        G: GraphAdd<V, E>,
        V: Eq,
    {
        self.storage.try_add_edge_connecting(src, dst, edge)
    }

    pub fn add_edge_connecting(&mut self, src: V, dst: V, edge: E) -> G::EdgeId
    where
        G: GraphAdd<V, E>,
        V: Eq,
    {
        self.storage.add_edge_connecting(src, dst, edge)
    }

    pub fn remove_edge<EId>(&mut self, id: EId) -> Option<E>
    where
        G: GraphFull<V, E>,
        EId: Borrow<G::EdgeId>,
    {
        self.storage.remove_edge(id.borrow())
    }

    pub fn remove_edges_between<VId>(&mut self, src: VId, dst: VId)
    where
        G: GraphFull<V, E>,
        VId: Borrow<G::VertexId>,
    {
        self.storage
            .remove_edges_between(src.borrow(), dst.borrow())
    }

    pub fn remove_edge_any_between<VId>(&mut self, src: VId, dst: VId) -> Option<E>
    where
        G: GraphFull<V, E>,
        VId: Borrow<G::VertexId>,
    {
        self.storage
            .remove_edge_any_between(src.borrow(), dst.borrow())
    }

    pub fn replace_edge<EId>(&mut self, id: EId, edge: E) -> E
    where
        G: GraphMut<V, E>,
        EId: Borrow<G::EdgeId>,
    {
        self.storage.replace_edge(id.borrow(), edge)
    }

    pub fn try_replace_edge<EId>(&mut self, id: EId, edge: E) -> Result<E, ReplaceEdgeError<E>>
    where
        G: GraphMut<V, E>,
        EId: Borrow<G::EdgeId>,
    {
        self.storage.try_replace_edge(id.borrow(), edge)
    }

    pub fn clear_edges(&mut self)
    where
        G: GraphFull<V, E>,
    {
        self.storage.clear_edges()
    }

    pub fn neighbors_undirected<VId>(&self, src: VId) -> G::NeighborsIter<'_>
    where
        G: Neighbors,
        VId: Borrow<G::VertexId>,
    {
        self.storage.neighbors_undirected(src.borrow())
    }

    pub fn neighbors_directed<VId>(&self, src: VId, dir: Direction) -> G::NeighborsIter<'_>
    where
        G: Neighbors,
        VId: Borrow<G::VertexId>,
    {
        self.storage.neighbors_directed(src.borrow(), dir)
    }

    pub fn degree_undirected<VId>(&self, src: VId) -> usize
    where
        G: Neighbors,
        VId: Borrow<G::VertexId>,
    {
        self.storage.degree_undirected(src.borrow())
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

    #[doc(hidden)]
    pub fn freeze(self) -> Graph<V, E, Ty, Frozen<G>> {
        Graph::new_in(Frozen::new(self.storage))
    }

    pub fn connect_vertices<F>(&mut self, connect: F)
    where
        G: ConnectVertices<V, E>,
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

impl<V, E, Ty: EdgeType, G> Create<V, E> for Graph<V, E, Ty, G>
where
    G: Create<V, E>,
{
    fn with_capacity(vertex_count: usize, edge_count: usize) -> Self {
        Self::new_in(G::with_capacity(vertex_count, edge_count))
    }
}

impl<V, E, Ty: EdgeType, G, VId> Index<VId> for Graph<V, E, Ty, G>
where
    G: GraphRef<V, E>,
    VId: Borrow<G::VertexId>,
{
    type Output = V;

    fn index(&self, id: VId) -> &Self::Output {
        self.vertex(id).expect("vertex does not exist")
    }
}

impl<V, E, Ty: EdgeType, G, VId> IndexMut<VId> for Graph<V, E, Ty, G>
where
    G: GraphMut<V, E>,
    VId: Borrow<G::VertexId>,
{
    fn index_mut(&mut self, id: VId) -> &mut Self::Output {
        self.vertex_mut(id).expect("vertex does not exist")
    }
}
