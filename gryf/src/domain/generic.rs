use std::{
    borrow::Borrow,
    marker::PhantomData,
    ops::{Deref, DerefMut, Index, IndexMut},
};

use crate::{
    core::{
        EdgeSet, GraphAdd, GraphBase, GraphFull, GraphMut, GraphRef, Neighbors, VertexSet,
        base::IntoEdge,
        connect::ConnectVertices,
        create::{Create, ExtendWithEdges, ExtendWithVertices},
        error::{
            AddEdgeConnectingError, AddEdgeError, AddVertexError, ReplaceEdgeError,
            ReplaceVertexError,
        },
        id::{AsIdRef, DefaultId, IntegerIdType},
        marker::{Directed, Direction, EdgeType, Undirected},
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

    pub fn with_capacity(vertex_capacity: usize, edge_capacity: usize) -> Self {
        Self::new_in(AdjList::with_capacity(vertex_capacity, edge_capacity))
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

impl<V, E, G> Default for Graph<V, E, G::EdgeType, G>
where
    G: GraphBase + Default,
{
    fn default() -> Self {
        Self::new_in(G::default())
    }
}

impl<V, E, G> From<G> for Graph<V, E, G::EdgeType, G>
where
    G: GraphBase,
{
    fn from(storage: G) -> Self {
        Self::new_in(storage)
    }
}

impl<V, E, G> Graph<V, E, G::EdgeType, G>
where
    G: GraphBase,
{
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

    pub fn vertex_count_hint(&self) -> Option<usize>
    where
        G: GraphBase,
    {
        self.storage.vertex_count_hint()
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

    pub fn contains_vertex<VI>(&self, id: VI) -> bool
    where
        G: GraphRef<V, E>,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.contains_vertex(id.as_id().as_ref())
    }

    pub fn vertex<VI>(&self, id: VI) -> Option<&V>
    where
        G: GraphRef<V, E>,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.vertex(id.as_id().as_ref())
    }

    pub fn vertices(&self) -> G::VerticesIter<'_>
    where
        G: GraphRef<V, E>,
    {
        self.storage.vertices()
    }

    pub fn find_vertex<Q>(&self, vertex: Q) -> Option<G::VertexId>
    where
        G: GraphRef<V, E>,
        V: Eq,
        Q: Borrow<V>,
    {
        self.storage.find_vertex(vertex.borrow())
    }

    pub fn vertex_mut<VI>(&mut self, id: VI) -> Option<&mut V>
    where
        G: GraphMut<V, E>,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.vertex_mut(id.as_id().as_ref())
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

    pub fn remove_vertex<VI>(&mut self, id: VI) -> Option<V>
    where
        G: GraphFull<V, E>,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.remove_vertex(id.as_id().as_ref())
    }

    pub fn replace_vertex<VI>(&mut self, id: VI, vertex: V) -> V
    where
        G: GraphMut<V, E>,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.replace_vertex(id.as_id().as_ref(), vertex)
    }

    pub fn try_replace_vertex<VI>(&mut self, id: VI, vertex: V) -> Result<V, ReplaceVertexError<V>>
    where
        G: GraphMut<V, E>,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.try_replace_vertex(id.as_id().as_ref(), vertex)
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

    pub fn edge_count_hint(&self) -> Option<usize>
    where
        G: GraphBase,
    {
        self.storage.edge_count_hint()
    }

    pub fn edge_bound(&self) -> usize
    where
        G: EdgeSet,
        G::EdgeId: IntegerIdType,
    {
        self.storage.edge_bound()
    }

    pub fn endpoints<EI>(&self, id: EI) -> Option<(G::VertexId, G::VertexId)>
    where
        G: EdgeSet,
        EI: AsIdRef<G::EdgeId>,
    {
        self.storage.endpoints(id.as_id().as_ref())
    }

    pub fn edge_id<VI>(&self, from: VI, to: VI) -> G::EdgeIdIter<'_>
    where
        G: EdgeSet,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage
            .edge_id(from.as_id().as_ref(), to.as_id().as_ref())
    }

    pub fn edge_id_any<VI>(&self, from: VI, to: VI) -> Option<G::EdgeId>
    where
        G: EdgeSet,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage
            .edge_id_any(from.as_id().as_ref(), to.as_id().as_ref())
    }

    pub fn edges_by_id(&self) -> G::EdgesByIdIter<'_>
    where
        G: EdgeSet,
    {
        self.storage.edges_by_id()
    }

    pub fn contains_edge<EI>(&self, id: EI) -> bool
    where
        G: EdgeSet,
        EI: AsIdRef<G::EdgeId>,
    {
        self.storage.contains_edge(id.as_id().as_ref())
    }

    pub fn contains_edge_between<VI>(&self, from: VI, to: VI) -> bool
    where
        G: EdgeSet,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage
            .contains_edge_between(from.as_id().as_ref(), to.as_id().as_ref())
    }

    pub fn is_directed(&self) -> bool
    where
        G: GraphBase,
    {
        self.storage.is_directed()
    }

    pub fn edge<EI>(&self, id: EI) -> Option<&E>
    where
        G: GraphRef<V, E>,
        EI: AsIdRef<G::EdgeId>,
    {
        self.storage.edge(id.as_id().as_ref())
    }

    pub fn edges(&self) -> G::EdgesIter<'_>
    where
        G: GraphRef<V, E>,
    {
        self.storage.edges()
    }

    pub fn edge_mut<EI>(&mut self, id: EI) -> Option<&mut E>
    where
        G: GraphMut<V, E>,
        EI: AsIdRef<G::EdgeId>,
    {
        self.storage.edge_mut(id.as_id().as_ref())
    }

    pub fn add_edge<VI>(&mut self, from: VI, to: VI, edge: E) -> G::EdgeId
    where
        G: GraphAdd<V, E>,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage
            .add_edge(from.as_id().as_ref(), to.as_id().as_ref(), edge)
    }

    pub fn try_add_edge<VI>(
        &mut self,
        from: VI,
        to: VI,
        edge: E,
    ) -> Result<G::EdgeId, AddEdgeError<E>>
    where
        G: GraphAdd<V, E>,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage
            .try_add_edge(from.as_id().as_ref(), to.as_id().as_ref(), edge)
    }

    pub fn try_add_edge_connecting(
        &mut self,
        from: V,
        to: V,
        edge: E,
    ) -> Result<G::EdgeId, AddEdgeConnectingError<V, E>>
    where
        G: GraphAdd<V, E>,
        V: Eq,
    {
        self.storage.try_add_edge_connecting(from, to, edge)
    }

    pub fn add_edge_connecting(&mut self, from: V, to: V, edge: E) -> G::EdgeId
    where
        G: GraphAdd<V, E>,
        V: Eq,
    {
        self.storage.add_edge_connecting(from, to, edge)
    }

    pub fn remove_edge<EI>(&mut self, id: EI) -> Option<E>
    where
        G: GraphFull<V, E>,
        EI: AsIdRef<G::EdgeId>,
    {
        self.storage.remove_edge(id.as_id().as_ref())
    }

    pub fn remove_edges_between<VI>(&mut self, from: VI, to: VI)
    where
        G: GraphFull<V, E>,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage
            .remove_edges_between(from.as_id().as_ref(), to.as_id().as_ref())
    }

    pub fn remove_edge_any_between<VI>(&mut self, from: VI, to: VI) -> Option<E>
    where
        G: GraphFull<V, E>,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage
            .remove_edge_any_between(from.as_id().as_ref(), to.as_id().as_ref())
    }

    pub fn replace_edge<EI>(&mut self, id: EI, edge: E) -> E
    where
        G: GraphMut<V, E>,
        EI: AsIdRef<G::EdgeId>,
    {
        self.storage.replace_edge(id.as_id().as_ref(), edge)
    }

    pub fn try_replace_edge<EI>(&mut self, id: EI, edge: E) -> Result<E, ReplaceEdgeError<E>>
    where
        G: GraphMut<V, E>,
        EI: AsIdRef<G::EdgeId>,
    {
        self.storage.try_replace_edge(id.as_id().as_ref(), edge)
    }

    pub fn clear_edges(&mut self)
    where
        G: GraphFull<V, E>,
    {
        self.storage.clear_edges()
    }

    pub fn neighbors_undirected<VI>(&self, from: VI) -> G::NeighborsIter<'_>
    where
        G: Neighbors,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.neighbors_undirected(from.as_id().as_ref())
    }

    pub fn neighbors_directed<VI>(&self, from: VI, dir: Direction) -> G::NeighborsIter<'_>
    where
        G: Neighbors,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.neighbors_directed(from.as_id().as_ref(), dir)
    }

    pub fn degree_undirected<VI>(&self, from: VI) -> usize
    where
        G: Neighbors,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.degree_undirected(from.as_id().as_ref())
    }

    pub fn degree_directed<VI>(&self, from: VI, dir: Direction) -> usize
    where
        G: Neighbors,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.degree_directed(from.as_id().as_ref(), dir)
    }

    pub fn stabilize(self) -> Graph<V, E, G::EdgeType, Stable<G>>
    where
        G: GraphBase,
    {
        Graph::new_in(Stable::new(self.storage))
    }

    #[doc(hidden)]
    pub fn freeze(self) -> Graph<V, E, G::EdgeType, Frozen<G>> {
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

impl<V, E, G> Deref for Graph<V, E, G::EdgeType, G>
where
    G: GraphBase,
{
    type Target = G;

    fn deref(&self) -> &Self::Target {
        &self.storage
    }
}

impl<V, E, G> DerefMut for Graph<V, E, G::EdgeType, G>
where
    G: GraphBase,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.storage
    }
}

impl<V, E, G> Create<V, E> for Graph<V, E, G::EdgeType, G>
where
    G: Create<V, E>,
{
    fn with_capacity(vertex_capacity: usize, edge_capacity: usize) -> Self {
        Self::new_in(G::with_capacity(vertex_capacity, edge_capacity))
    }
}

impl<V, E, G, VI> Index<VI> for Graph<V, E, G::EdgeType, G>
where
    G: GraphRef<V, E>,
    VI: AsIdRef<G::VertexId>,
{
    type Output = V;

    fn index(&self, id: VI) -> &Self::Output {
        self.vertex(id).expect("vertex does not exist")
    }
}

impl<V, E, G, VI> IndexMut<VI> for Graph<V, E, G::EdgeType, G>
where
    G: GraphMut<V, E>,
    VI: AsIdRef<G::VertexId>,
{
    fn index_mut(&mut self, id: VI) -> &mut Self::Output {
        self.vertex_mut(id).expect("vertex does not exist")
    }
}
