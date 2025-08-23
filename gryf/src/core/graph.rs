use std::mem;

use super::{
    base::{EdgeReference, NeighborReference, VertexReference},
    borrow::OwnableRef,
    error::{
        AddEdgeConnectingError, AddEdgeError, AddVertexError, ReplaceEdgeError,
        ReplaceEdgeErrorKind, ReplaceVertexError, ReplaceVertexErrorKind,
    },
    id::{CompactIdMap, IdType, IntegerIdType},
    marker::{Direction, EdgeType},
};

///
/// Base trait for all graphs that provides core properties and functionality.
///
/// # Examples
///
/// ```
/// use std::collections::HashSet;
///
/// use gryf::core::GraphBase;
///
/// fn algorithm<G: GraphBase>(graph: &G) {
///     let visited =
///         HashSet::<G::VertexId>::with_capacity(graph.vertex_count_hint().unwrap_or(32));
/// }
/// ```
///
/// # Implementation notes
pub trait GraphBase {
    /// Vertex ID type of the graph.
    type VertexId: IdType;

    /// Edge ID type of the graph.
    type EdgeId: IdType;

    /// Directionality of the graph.
    type EdgeType: EdgeType;

    #[doc = include_str!("../../docs/include/graph_base.is_directed.md")]
    fn is_directed(&self) -> bool {
        Self::EdgeType::is_directed()
    }

    #[doc = include_str!("../../docs/include/graph_base.vertex_count_hint.md")]
    fn vertex_count_hint(&self) -> Option<usize> {
        None
    }

    #[doc = include_str!("../../docs/include/graph_base.edge_count_hint.md")]
    fn edge_count_hint(&self) -> Option<usize> {
        None
    }
}

/// Trait for traversing vertex neighbors in a graph.
///
/// # Implementation notes
pub trait Neighbors: GraphBase {
    /// Reference to a neighbor.
    type NeighborRef<'a>: NeighborReference<Self::VertexId, Self::EdgeId>
    where
        Self: 'a;

    /// Iterator over neighbors of a vertex.
    type NeighborsIter<'a>: Iterator<Item = Self::NeighborRef<'a>>
    where
        Self: 'a;

    #[doc = include_str!("../../docs/include/neighbors.neighbors_undirected.md")]
    fn neighbors_undirected(&self, from: &Self::VertexId) -> Self::NeighborsIter<'_>;

    #[doc = include_str!("../../docs/include/neighbors.neighbors_directed.md")]
    fn neighbors_directed(&self, from: &Self::VertexId, dir: Direction) -> Self::NeighborsIter<'_>;

    #[doc = include_str!("../../docs/include/neighbors.degree_undirected.md")]
    fn degree_undirected(&self, id: &Self::VertexId) -> usize {
        if Self::EdgeType::is_directed() {
            self.degree_directed(id, Direction::Outgoing)
                + self.degree_directed(id, Direction::Incoming)
        } else {
            self.degree_directed(id, Direction::Outgoing)
        }
    }

    #[doc = include_str!("../../docs/include/neighbors.degree_directed.md")]
    fn degree_directed(&self, id: &Self::VertexId, dir: Direction) -> usize {
        if Self::EdgeType::is_directed() {
            self.neighbors_directed(id, dir).count()
        } else {
            // In undirected graphs, we need to handle self-loops.
            self.neighbors_directed(id, dir)
                .map(|neighbor| {
                    // If this is a self-loop, we need to count it twice.
                    // Storages are required to yield a self-loop just once. If
                    // this requirement is satisfied, then this implementation
                    // of degree is correct.
                    if neighbor.id().as_ref() == id { 2 } else { 1 }
                })
                .sum()
        }
    }
}

/// Trait representing a finite set of vertices.
///
/// # Implementation notes
pub trait VertexSet: GraphBase {
    /// Iterator over vertex IDs.
    type VerticesByIdIter<'a>: Iterator<Item = Self::VertexId>
    where
        Self: 'a;

    #[doc = include_str!("../../docs/include/vertex_set.vertices_by_id.md")]
    fn vertices_by_id(&self) -> Self::VerticesByIdIter<'_>;

    #[doc = include_str!("../../docs/include/vertex_set.vertex_count.md")]
    fn vertex_count(&self) -> usize {
        self.vertices_by_id().count()
    }

    #[allow(rustdoc::redundant_explicit_links)]
    #[doc = include_str!("../../docs/include/vertex_set.vertex_bound.md")]
    fn vertex_bound(&self) -> usize
    where
        Self::VertexId: IntegerIdType,
    {
        self.vertices_by_id()
            .map(|v| v.as_usize())
            .max()
            .unwrap_or_default()
    }

    #[doc = include_str!("../../docs/include/vertex_set.contains_vertex.md")]
    fn contains_vertex(&self, id: &Self::VertexId) -> bool {
        self.vertices_by_id().any(|v| &v == id)
    }

    #[doc = include_str!("../../docs/include/vertex_set.vertex_id_map.md")]
    fn vertex_id_map(&self) -> CompactIdMap<Self::VertexId> {
        // Should be overridden to use `isomorphic` whenever possible.
        CompactIdMap::new(self.vertices_by_id())
    }
}

/// Trait representing a finite set of edges and the graph structure.
///
/// # Implementation notes
pub trait EdgeSet: GraphBase {
    /// Iterator over edge IDs.
    type EdgesByIdIter<'a>: Iterator<Item = Self::EdgeId>
    where
        Self: 'a;

    /// Iterator over edge IDs between two vertices.
    type EdgeIdIter<'a>: Iterator<Item = Self::EdgeId>
    where
        Self: 'a;

    #[doc = include_str!("../../docs/include/edge_set.edges_by_id.md")]
    fn edges_by_id(&self) -> Self::EdgesByIdIter<'_>;

    #[doc = include_str!("../../docs/include/edge_set.edge_id.md")]
    fn edge_id(&self, from: &Self::VertexId, to: &Self::VertexId) -> Self::EdgeIdIter<'_>;

    #[doc = include_str!("../../docs/include/edge_set.endpoints.md")]
    fn endpoints(&self, id: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)>;

    #[doc = include_str!("../../docs/include/edge_set.edge_count.md")]
    fn edge_count(&self) -> usize {
        self.edges_by_id().count()
    }

    #[allow(rustdoc::redundant_explicit_links)]
    #[doc = include_str!("../../docs/include/edge_set.edge_bound.md")]
    fn edge_bound(&self) -> usize
    where
        Self::EdgeId: IntegerIdType,
    {
        self.edges_by_id()
            .map(|e| e.as_usize())
            .max()
            .unwrap_or_default()
    }

    #[doc = include_str!("../../docs/include/edge_set.contains_edge.md")]
    fn contains_edge(&self, id: &Self::EdgeId) -> bool {
        self.edges_by_id().any(|e| &e == id)
    }

    #[doc = include_str!("../../docs/include/edge_set.contains_edge_between.md")]
    fn contains_edge_between(&self, from: &Self::VertexId, to: &Self::VertexId) -> bool {
        self.edge_id_any(from, to).is_some()
    }

    #[doc = include_str!("../../docs/include/edge_set.edge_id_any.md")]
    fn edge_id_any(&self, from: &Self::VertexId, to: &Self::VertexId) -> Option<Self::EdgeId> {
        self.edge_id(from, to).next()
    }

    #[doc = include_str!("../../docs/include/edge_set.edge_id_map.md")]
    fn edge_id_map(&self) -> CompactIdMap<Self::EdgeId> {
        // Should be overridden to use `isomorphic` whenever possible.
        CompactIdMap::new(self.edges_by_id())
    }
}

/// Trait for read-only access to graph attributes.
///
/// # Implementation notes
pub trait GraphRef<V, E>: VertexSet + EdgeSet {
    /// Reference to a vertex.
    type VertexRef<'a>: VertexReference<Self::VertexId, V>
    where
        Self: 'a,
        V: 'a;

    /// Iterator over vertices.
    type VerticesIter<'a>: Iterator<Item = Self::VertexRef<'a>>
    where
        Self: 'a,
        V: 'a;

    /// Reference to an edge.
    type EdgeRef<'a>: EdgeReference<Self::VertexId, Self::EdgeId, E>
    where
        Self: 'a,
        E: 'a;

    /// Iterator over edges.
    type EdgesIter<'a>: Iterator<Item = Self::EdgeRef<'a>>
    where
        Self: 'a,
        E: 'a;

    #[doc = include_str!("../../docs/include/graph_ref.vertices.md")]
    fn vertices(&self) -> Self::VerticesIter<'_>;

    #[doc = include_str!("../../docs/include/graph_ref.edges.md")]
    fn edges(&self) -> Self::EdgesIter<'_>;

    #[doc = include_str!("../../docs/include/graph_ref.vertex.md")]
    fn vertex(&self, id: &Self::VertexId) -> Option<&V>;

    #[doc = include_str!("../../docs/include/graph_ref.edge.md")]
    fn edge(&self, id: &Self::EdgeId) -> Option<&E>;

    #[doc = include_str!("../../docs/include/graph_ref.find_vertex.md")]
    fn find_vertex(&self, vertex: &V) -> Option<Self::VertexId>
    where
        V: Eq,
    {
        self.vertices().find_map(|v| {
            if v.attr() == vertex {
                Some(v.id().clone())
            } else {
                None
            }
        })
    }
}

/// Trait for read-only access to graph attributes of potentially [implicit]
/// graphs.
///
/// This trait should be preferred over [`GraphRef`] whenever possible, because
/// it allows accepting a wider spectrum of graphs, including implicit ones.
///
/// [implicit]: https://en.wikipedia.org/wiki/Implicit_graph
///
/// # Implementation notes
pub trait GraphWeak<V, E>: GraphBase {
    #[doc = include_str!("../../docs/include/graph_weak.vertex_weak.md")]
    fn vertex_weak(&self, id: &Self::VertexId) -> Option<OwnableRef<'_, V>>;

    #[doc = include_str!("../../docs/include/graph_weak.edge_weak.md")]
    fn edge_weak(&self, id: &Self::EdgeId) -> Option<OwnableRef<'_, E>>;
}

/// Trait for mutable access to graph attributes.
///
/// # Implementation notes
pub trait GraphMut<V, E>: GraphRef<V, E> {
    #[doc = include_str!("../../docs/include/graph_mut.vertex_mut.md")]
    fn vertex_mut(&mut self, id: &Self::VertexId) -> Option<&mut V>;

    #[doc = include_str!("../../docs/include/graph_mut.edge_mut.md")]
    fn edge_mut(&mut self, id: &Self::EdgeId) -> Option<&mut E>;

    #[doc = include_str!("../../docs/include/graph_mut.try_replace_vertex.md")]
    fn try_replace_vertex(
        &mut self,
        id: &Self::VertexId,
        vertex: V,
    ) -> Result<V, ReplaceVertexError<V>> {
        match self.vertex_mut(id) {
            Some(slot) => Ok(mem::replace(slot, vertex)),
            None => Err(ReplaceVertexError::new(
                vertex,
                ReplaceVertexErrorKind::VertexAbsent,
            )),
        }
    }

    #[doc = include_str!("../../docs/include/graph_mut.replace_vertex.md")]
    fn replace_vertex(&mut self, id: &Self::VertexId, vertex: V) -> V {
        match self.try_replace_vertex(id, vertex) {
            Ok(original) => original,
            Err(error) => panic!("{error}"),
        }
    }

    #[doc = include_str!("../../docs/include/graph_mut.try_replace_edge.md")]
    fn try_replace_edge(&mut self, id: &Self::EdgeId, edge: E) -> Result<E, ReplaceEdgeError<E>> {
        match self.edge_mut(id) {
            Some(slot) => Ok(mem::replace(slot, edge)),
            None => Err(ReplaceEdgeError::new(
                edge,
                ReplaceEdgeErrorKind::EdgeAbsent,
            )),
        }
    }

    #[doc = include_str!("../../docs/include/graph_mut.replace_edge.md")]
    fn replace_edge(&mut self, id: &Self::EdgeId, edge: E) -> E {
        match self.try_replace_edge(id, edge) {
            Ok(original) => original,
            Err(error) => panic!("{error}"),
        }
    }
}

/// Trait for adding new vertices and edges to a graph.
///
/// # Implementation notes
pub trait GraphAdd<V, E>: GraphMut<V, E> {
    #[doc = include_str!("../../docs/include/graph_add.try_add_vertex.md")]
    fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>>;

    #[doc = include_str!("../../docs/include/graph_add.try_add_edge.md")]
    fn try_add_edge(
        &mut self,
        from: &Self::VertexId,
        to: &Self::VertexId,
        edge: E,
    ) -> Result<Self::EdgeId, AddEdgeError<E>>;

    #[doc = include_str!("../../docs/include/graph_add.add_vertex.md")]
    fn add_vertex(&mut self, vertex: V) -> Self::VertexId {
        match self.try_add_vertex(vertex) {
            Ok(id) => id,
            Err(error) => panic!("{error}"),
        }
    }

    #[doc = include_str!("../../docs/include/graph_add.try_get_or_add_vertex.md")]
    fn try_get_or_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>>
    where
        V: Eq,
    {
        match self.find_vertex(&vertex) {
            Some(v) => Ok(v),
            None => self.try_add_vertex(vertex),
        }
    }

    #[doc = include_str!("../../docs/include/graph_add.get_or_add_vertex.md")]
    fn get_or_add_vertex(&mut self, vertex: V) -> Self::VertexId
    where
        V: Eq,
    {
        match self.try_get_or_add_vertex(vertex) {
            Ok(id) => id,
            Err(error) => panic!("{error}"),
        }
    }

    #[doc = include_str!("../../docs/include/graph_add.add_edge.md")]
    fn add_edge(&mut self, from: &Self::VertexId, to: &Self::VertexId, edge: E) -> Self::EdgeId {
        match self.try_add_edge(from, to, edge) {
            Ok(id) => id,
            Err(error) => panic!("{error}"),
        }
    }

    #[doc = include_str!("../../docs/include/graph_add.try_add_edge_connecting.md")]
    fn try_add_edge_connecting(
        &mut self,
        from: V,
        to: V,
        edge: E,
    ) -> Result<Self::EdgeId, AddEdgeConnectingError<V, E>>
    where
        V: Eq,
    {
        let from = self.try_get_or_add_vertex(from)?;
        let to = self.try_get_or_add_vertex(to)?;
        let edge = self.try_add_edge(&from, &to, edge)?;
        Ok(edge)
    }

    #[doc = include_str!("../../docs/include/graph_add.add_edge_connecting.md")]
    fn add_edge_connecting(&mut self, from: V, to: V, edge: E) -> Self::EdgeId
    where
        V: Eq,
    {
        match self.try_add_edge_connecting(from, to, edge) {
            Ok(id) => id,
            Err(error) => panic!("{error}"),
        }
    }
}

/// Trait for removing vertices and edges from a graph.
///
/// # Implementation notes
pub trait GraphFull<V, E>: GraphAdd<V, E> {
    #[doc = include_str!("../../docs/include/graph_full.remove_vertex.md")]
    fn remove_vertex(&mut self, id: &Self::VertexId) -> Option<V>;

    #[doc = include_str!("../../docs/include/graph_full.remove_edge.md")]
    fn remove_edge(&mut self, id: &Self::EdgeId) -> Option<E>;

    #[doc = include_str!("../../docs/include/graph_full.clear.md")]
    fn clear(&mut self) {
        let mut vertices = self.vertices_by_id().collect::<Vec<_>>();
        vertices.reverse();

        for v in vertices {
            self.remove_vertex(&v);
        }
    }

    #[doc = include_str!("../../docs/include/graph_full.remove_edges_between.md")]
    fn remove_edges_between(&mut self, from: &Self::VertexId, to: &Self::VertexId) {
        while self.remove_edge_any_between(from, to).is_some() {}
    }

    #[doc = include_str!("../../docs/include/graph_full.remove_edge_any_between.md")]
    fn remove_edge_any_between(&mut self, from: &Self::VertexId, to: &Self::VertexId) -> Option<E> {
        let id = self.edge_id_any(from, to)?;
        self.remove_edge(&id)
    }

    #[doc = include_str!("../../docs/include/graph_full.clear_edges.md")]
    fn clear_edges(&mut self) {
        let mut edges = self.edges_by_id().collect::<Vec<_>>();
        edges.reverse();

        for e in edges {
            self.remove_edge(&e);
        }
    }
}

mod imp {
    use crate::core::id::IdPair;

    use super::*;

    impl<G> IdPair for G
    where
        G: GraphBase,
    {
        type VertexId = G::VertexId;
        type EdgeId = G::EdgeId;
    }

    impl<V, E, G> GraphWeak<V, E> for G
    where
        G: GraphRef<V, E>,
    {
        fn vertex_weak(&self, id: &Self::VertexId) -> Option<OwnableRef<'_, V>> {
            self.vertex(id).map(OwnableRef::Borrowed)
        }

        fn edge_weak(&self, id: &Self::EdgeId) -> Option<OwnableRef<'_, E>> {
            self.edge(id).map(OwnableRef::Borrowed)
        }
    }

    macro_rules! deref_graph_base {
        ($($ref_kind:tt)*) => {
            impl<G> GraphBase for $($ref_kind)* G
            where
                G: GraphBase + ?Sized,
            {
                type VertexId = G::VertexId;
                type EdgeId = G::EdgeId;
                type EdgeType = G::EdgeType;

                fn vertex_count_hint(&self) -> Option<usize> {
                    (**self).vertex_count_hint()
                }

                fn edge_count_hint(&self) -> Option<usize> {
                    (**self).edge_count_hint()
                }
            }
        }
    }

    deref_graph_base!(&);
    deref_graph_base!(&mut);

    macro_rules! deref_neighbors {
        ($($ref_kind:tt)*) => {
            impl<G> Neighbors for $($ref_kind)* G
            where
                G: Neighbors,
            {
                type NeighborRef<'a> = G::NeighborRef<'a>
                where
                    Self: 'a;

                type NeighborsIter<'a> = G::NeighborsIter<'a>
                where
                    Self: 'a;

                fn neighbors_undirected(&self, from: &Self::VertexId) -> Self::NeighborsIter<'_> {
                    (**self).neighbors_undirected(from)
                }

                fn neighbors_directed(&self, from: &Self::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
                    (**self).neighbors_directed(from, dir)
                }

                fn degree_undirected(&self, id: &Self::VertexId) -> usize {
                    (**self).degree_undirected(id)
                }

                fn degree_directed(&self, id: &Self::VertexId, dir: Direction) -> usize {
                    (**self).degree_directed(id, dir)
                }
            }
        }
    }

    deref_neighbors!(&);
    deref_neighbors!(&mut);

    macro_rules! deref_vertex_set {
        ($($ref_kind:tt)*) => {
            impl<G> VertexSet for $($ref_kind)* G
            where
                G: VertexSet,
            {
                type VerticesByIdIter<'a> = G::VerticesByIdIter<'a>
                where
                    Self: 'a;

                fn vertices_by_id(&self) -> Self::VerticesByIdIter<'_> {
                    (**self).vertices_by_id()
                }

                fn vertex_count(&self) -> usize {
                    (**self).vertex_count()
                }

                fn vertex_bound(&self) -> usize
                where
                    Self::VertexId: IntegerIdType,
                {
                    (**self).vertex_bound()
                }

                fn contains_vertex(&self, id: &Self::VertexId) -> bool {
                    (**self).contains_vertex(id)
                }

                fn vertex_id_map(&self) -> CompactIdMap<Self::VertexId> {
                    (**self).vertex_id_map()
                }
            }
        }
    }

    deref_vertex_set!(&);
    deref_vertex_set!(&mut);

    macro_rules! deref_edge_set {
        ($($ref_kind:tt)*) => {
            impl<G> EdgeSet for $($ref_kind)* G
            where
                G: EdgeSet,
            {
                type EdgesByIdIter<'a> = G::EdgesByIdIter<'a>
                where
                    Self: 'a;

                type EdgeIdIter<'a> = G::EdgeIdIter<'a>
                where
                    Self: 'a;

                fn edges_by_id(&self) -> Self::EdgesByIdIter<'_> {
                    (**self).edges_by_id()
                }

                fn edge_id(&self, from: &Self::VertexId, to: &Self::VertexId) -> Self::EdgeIdIter<'_> {
                    (**self).edge_id(from, to)
                }

                fn endpoints(&self, id: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)> {
                    (**self).endpoints(id)
                }

                fn edge_count(&self) -> usize {
                    (**self).edge_count()
                }

                fn edge_bound(&self) -> usize
                where
                    Self::EdgeId: IntegerIdType,
                {
                    (**self).edge_bound()
                }

                fn contains_edge(&self, id: &Self::EdgeId) -> bool {
                    (**self).contains_edge(id)
                }

                fn contains_edge_between(&self, from: &Self::VertexId, to: &Self::VertexId) -> bool {
                    (**self).contains_edge_between(from, to)
                }

                fn edge_id_any(&self, from: &Self::VertexId, to: &Self::VertexId) -> Option<Self::EdgeId> {
                    (**self).edge_id_any(from, to)
                }

                fn edge_id_map(&self) -> CompactIdMap<Self::EdgeId> {
                    (**self).edge_id_map()
                }
            }
        }
    }

    deref_edge_set!(&);
    deref_edge_set!(&mut);

    macro_rules! deref_graph_ref {
        ($($ref_kind:tt)*) => {
            impl<V, E, G> GraphRef<V, E> for $($ref_kind)* G
            where
                G: GraphRef<V, E>,
            {
                type VertexRef<'a> = G::VertexRef<'a>
                where
                    Self: 'a,
                    V: 'a;

                type VerticesIter<'a> = G::VerticesIter<'a>
                where
                    Self: 'a,
                    V: 'a;

                type EdgeRef<'a> = G::EdgeRef<'a>
                where
                    Self: 'a,
                    E: 'a;

                type EdgesIter<'a> = G::EdgesIter<'a>
                where
                    Self: 'a,
                    E: 'a;

                fn vertices(&self) -> Self::VerticesIter<'_> {
                    (**self).vertices()
                }

                fn edges(&self) -> Self::EdgesIter<'_> {
                    (**self).edges()
                }

                fn vertex(&self, id: &Self::VertexId) -> Option<&V> {
                    (**self).vertex(id)
                }

                fn edge(&self, id: &Self::EdgeId) -> Option<&E> {
                    (**self).edge(id)
                }

                fn find_vertex(&self, vertex: &V) -> Option<Self::VertexId>
                where
                    V: Eq,
                {
                    (**self).find_vertex(vertex)
                }
            }
        }
    }

    deref_graph_ref!(&);
    deref_graph_ref!(&mut);

    impl<V, E, G> GraphMut<V, E> for &mut G
    where
        G: GraphMut<V, E>,
    {
        fn vertex_mut(&mut self, id: &Self::VertexId) -> Option<&mut V> {
            (**self).vertex_mut(id)
        }

        fn edge_mut(&mut self, id: &Self::EdgeId) -> Option<&mut E> {
            (**self).edge_mut(id)
        }

        fn try_replace_vertex(
            &mut self,
            id: &Self::VertexId,
            vertex: V,
        ) -> Result<V, ReplaceVertexError<V>> {
            (**self).try_replace_vertex(id, vertex)
        }

        fn replace_vertex(&mut self, id: &Self::VertexId, vertex: V) -> V {
            (**self).replace_vertex(id, vertex)
        }

        fn try_replace_edge(
            &mut self,
            id: &Self::EdgeId,
            edge: E,
        ) -> Result<E, ReplaceEdgeError<E>> {
            (**self).try_replace_edge(id, edge)
        }

        fn replace_edge(&mut self, id: &Self::EdgeId, edge: E) -> E {
            (**self).replace_edge(id, edge)
        }
    }

    impl<V, E, G> GraphAdd<V, E> for &mut G
    where
        G: GraphAdd<V, E>,
    {
        fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>> {
            (**self).try_add_vertex(vertex)
        }

        fn try_add_edge(
            &mut self,
            from: &Self::VertexId,
            to: &Self::VertexId,
            edge: E,
        ) -> Result<Self::EdgeId, AddEdgeError<E>> {
            (**self).try_add_edge(from, to, edge)
        }

        fn add_vertex(&mut self, vertex: V) -> Self::VertexId {
            (**self).add_vertex(vertex)
        }

        fn try_get_or_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>>
        where
            V: Eq,
        {
            (**self).try_get_or_add_vertex(vertex)
        }

        fn get_or_add_vertex(&mut self, vertex: V) -> Self::VertexId
        where
            V: Eq,
        {
            (**self).get_or_add_vertex(vertex)
        }

        fn add_edge(
            &mut self,
            from: &Self::VertexId,
            to: &Self::VertexId,
            edge: E,
        ) -> Self::EdgeId {
            (**self).add_edge(from, to, edge)
        }

        fn try_add_edge_connecting(
            &mut self,
            from: V,
            to: V,
            edge: E,
        ) -> Result<Self::EdgeId, AddEdgeConnectingError<V, E>>
        where
            V: Eq,
        {
            (**self).try_add_edge_connecting(from, to, edge)
        }

        fn add_edge_connecting(&mut self, from: V, to: V, edge: E) -> Self::EdgeId
        where
            V: Eq,
        {
            (**self).add_edge_connecting(from, to, edge)
        }
    }

    impl<V, E, G> GraphFull<V, E> for &mut G
    where
        G: GraphFull<V, E>,
    {
        fn remove_vertex(&mut self, id: &Self::VertexId) -> Option<V> {
            (**self).remove_vertex(id)
        }

        fn remove_edge(&mut self, id: &Self::EdgeId) -> Option<E> {
            (**self).remove_edge(id)
        }

        fn clear(&mut self) {
            (**self).clear()
        }

        fn remove_edges_between(&mut self, from: &Self::VertexId, to: &Self::VertexId) {
            (**self).remove_edges_between(from, to)
        }

        fn remove_edge_any_between(
            &mut self,
            from: &Self::VertexId,
            to: &Self::VertexId,
        ) -> Option<E> {
            (**self).remove_edge_any_between(from, to)
        }

        fn clear_edges(&mut self) {
            (**self).clear_edges()
        }
    }
}
