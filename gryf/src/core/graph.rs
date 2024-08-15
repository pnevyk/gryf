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

pub trait GraphBase {
    type VertexId: IdType;
    type EdgeId: IdType;
    type EdgeType: EdgeType;

    fn is_directed(&self) -> bool {
        Self::EdgeType::is_directed()
    }

    // Upper bound, if known.
    fn vertex_count_hint(&self) -> Option<usize> {
        None
    }

    // Upper bound, if known.
    fn edge_count_hint(&self) -> Option<usize> {
        None
    }
}

pub trait Neighbors: GraphBase {
    type NeighborRef<'a>: NeighborReference<Self::VertexId, Self::EdgeId>
    where
        Self: 'a;

    type NeighborsIter<'a>: Iterator<Item = Self::NeighborRef<'a>>
    where
        Self: 'a;

    fn neighbors_undirected(&self, from: &Self::VertexId) -> Self::NeighborsIter<'_>;
    fn neighbors_directed(&self, from: &Self::VertexId, dir: Direction) -> Self::NeighborsIter<'_>;

    fn degree_undirected(&self, id: &Self::VertexId) -> usize {
        if Self::EdgeType::is_directed() {
            self.degree_directed(id, Direction::Outgoing)
                + self.degree_directed(id, Direction::Incoming)
        } else {
            self.degree_directed(id, Direction::Outgoing)
        }
    }

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
                    if neighbor.id().as_ref() == id {
                        2
                    } else {
                        1
                    }
                })
                .sum()
        }
    }
}

pub trait VertexSet: GraphBase {
    type VerticesByIdIter<'a>: Iterator<Item = Self::VertexId>
    where
        Self: 'a;

    fn vertices_by_id(&self) -> Self::VerticesByIdIter<'_>;

    fn vertex_count(&self) -> usize {
        self.vertices_by_id().count()
    }

    fn vertex_bound(&self) -> usize
    where
        Self::VertexId: IntegerIdType,
    {
        self.vertices_by_id()
            .map(|v| v.as_usize())
            .max()
            .unwrap_or_default()
    }

    fn contains_vertex(&self, id: &Self::VertexId) -> bool {
        self.vertices_by_id().any(|v| &v == id)
    }

    fn vertex_id_map(&self) -> CompactIdMap<Self::VertexId>
    where
        Self::VertexId: IntegerIdType,
    {
        // Should be overridden to use `isomorphic` whenever possible.
        CompactIdMap::new(self.vertices_by_id())
    }
}

pub trait EdgeSet: GraphBase {
    type EdgesByIdIter<'a>: Iterator<Item = Self::EdgeId>
    where
        Self: 'a;

    type EdgeIdIter<'a>: Iterator<Item = Self::EdgeId>
    where
        Self: 'a;

    fn edges_by_id(&self) -> Self::EdgesByIdIter<'_>;

    fn edge_id(&self, from: &Self::VertexId, to: &Self::VertexId) -> Self::EdgeIdIter<'_>;

    fn endpoints(&self, id: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)>;

    fn edge_count(&self) -> usize {
        self.edges_by_id().count()
    }

    fn edge_bound(&self) -> usize
    where
        Self::EdgeId: IntegerIdType,
    {
        self.edges_by_id()
            .map(|e| e.as_usize())
            .max()
            .unwrap_or_default()
    }

    fn contains_edge(&self, id: &Self::EdgeId) -> bool {
        self.edges_by_id().any(|e| &e == id)
    }

    fn contains_edge_between(&self, from: &Self::VertexId, to: &Self::VertexId) -> bool {
        self.edge_id_any(from, to).is_some()
    }

    fn edge_id_any(&self, from: &Self::VertexId, to: &Self::VertexId) -> Option<Self::EdgeId> {
        self.edge_id(from, to).next()
    }

    fn edge_id_map(&self) -> CompactIdMap<Self::EdgeId>
    where
        Self::EdgeId: IntegerIdType,
    {
        // Should be overridden to use `isomorphic` whenever possible.
        CompactIdMap::new(self.edges_by_id())
    }
}

pub trait GraphRef<V, E>: VertexSet + EdgeSet {
    type VertexRef<'a>: VertexReference<Self::VertexId, V>
    where
        Self: 'a,
        V: 'a;

    type VerticesIter<'a>: Iterator<Item = Self::VertexRef<'a>>
    where
        Self: 'a,
        V: 'a;

    type EdgeRef<'a>: EdgeReference<Self::VertexId, Self::EdgeId, E>
    where
        Self: 'a,
        E: 'a;

    type EdgesIter<'a>: Iterator<Item = Self::EdgeRef<'a>>
    where
        Self: 'a,
        E: 'a;

    fn vertices(&self) -> Self::VerticesIter<'_>;
    fn edges(&self) -> Self::EdgesIter<'_>;

    fn vertex(&self, id: &Self::VertexId) -> Option<&V>;
    fn edge(&self, id: &Self::EdgeId) -> Option<&E>;

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

pub trait GraphWeak<V, E>: GraphBase {
    fn vertex_weak(&self, id: &Self::VertexId) -> Option<OwnableRef<'_, V>>;
    fn edge_weak(&self, id: &Self::EdgeId) -> Option<OwnableRef<'_, E>>;
}

pub trait GraphMut<V, E>: GraphRef<V, E> {
    fn vertex_mut(&mut self, id: &Self::VertexId) -> Option<&mut V>;
    fn edge_mut(&mut self, id: &Self::EdgeId) -> Option<&mut E>;

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

    fn replace_vertex(&mut self, id: &Self::VertexId, vertex: V) -> V {
        match self.try_replace_vertex(id, vertex) {
            Ok(original) => original,
            Err(error) => panic!("{error}"),
        }
    }

    fn try_replace_edge(&mut self, id: &Self::EdgeId, edge: E) -> Result<E, ReplaceEdgeError<E>> {
        match self.edge_mut(id) {
            Some(slot) => Ok(mem::replace(slot, edge)),
            None => Err(ReplaceEdgeError::new(
                edge,
                ReplaceEdgeErrorKind::EdgeAbsent,
            )),
        }
    }

    fn replace_edge(&mut self, id: &Self::EdgeId, edge: E) -> E {
        match self.try_replace_edge(id, edge) {
            Ok(original) => original,
            Err(error) => panic!("{error}"),
        }
    }
}

pub trait GraphAdd<V, E>: GraphMut<V, E> {
    fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>>;
    fn try_add_edge(
        &mut self,
        from: &Self::VertexId,
        to: &Self::VertexId,
        edge: E,
    ) -> Result<Self::EdgeId, AddEdgeError<E>>;

    fn add_vertex(&mut self, vertex: V) -> Self::VertexId {
        match self.try_add_vertex(vertex) {
            Ok(id) => id,
            Err(error) => panic!("{error}"),
        }
    }

    fn try_get_or_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>>
    where
        V: Eq,
    {
        match self.find_vertex(&vertex) {
            Some(v) => Ok(v),
            None => self.try_add_vertex(vertex),
        }
    }

    fn get_or_add_vertex(&mut self, vertex: V) -> Self::VertexId
    where
        V: Eq,
    {
        match self.try_get_or_add_vertex(vertex) {
            Ok(id) => id,
            Err(error) => panic!("{error}"),
        }
    }

    fn add_edge(&mut self, from: &Self::VertexId, to: &Self::VertexId, edge: E) -> Self::EdgeId {
        match self.try_add_edge(from, to, edge) {
            Ok(id) => id,
            Err(error) => panic!("{error}"),
        }
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
        let from = self.try_get_or_add_vertex(from)?;
        let to = self.try_get_or_add_vertex(to)?;
        let edge = self.try_add_edge(&from, &to, edge)?;
        Ok(edge)
    }

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

pub trait GraphFull<V, E>: GraphAdd<V, E> {
    fn remove_vertex(&mut self, id: &Self::VertexId) -> Option<V>;
    fn remove_edge(&mut self, id: &Self::EdgeId) -> Option<E>;

    fn clear(&mut self) {
        let mut vertices = self.vertices_by_id().collect::<Vec<_>>();
        vertices.reverse();

        for v in vertices {
            self.remove_vertex(&v);
        }
    }

    fn remove_edges_between(&mut self, from: &Self::VertexId, to: &Self::VertexId) {
        while self.remove_edge_any_between(from, to).is_some() {}
    }

    fn remove_edge_any_between(&mut self, from: &Self::VertexId, to: &Self::VertexId) -> Option<E> {
        let id = self.edge_id_any(from, to)?;
        self.remove_edge(&id)
    }

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

                fn vertex_id_map(&self) -> CompactIdMap<Self::VertexId>
                where
                    Self::VertexId: IntegerIdType,
                {
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

                fn edge_id_map(&self) -> CompactIdMap<Self::EdgeId>
                where
                    Self::EdgeId: IntegerIdType,
                {
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
