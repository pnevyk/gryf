use std::mem;

use crate::common::CompactIdMap;

use super::{
    base::{EdgeRef, NeighborRef, VertexRef},
    error::{AddEdgeError, AddVertexError, ReplaceEdgeError, ReplaceVertexError},
    id::{IdType, IntegerIdType},
    marker::{Direction, EdgeType},
    weak::WeakRef,
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
    type NeighborRef<'a>: NeighborRef<Self::VertexId, Self::EdgeId>
    where
        Self: 'a;

    type NeighborsIter<'a>: Iterator<Item = Self::NeighborRef<'a>>
    where
        Self: 'a;

    fn neighbors(&self, src: &Self::VertexId) -> Self::NeighborsIter<'_>;
    fn neighbors_directed(&self, src: &Self::VertexId, dir: Direction) -> Self::NeighborsIter<'_>;

    fn degree(&self, id: &Self::VertexId) -> usize {
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
    type VertexIdsIter<'a>: Iterator<Item = Self::VertexId>
    where
        Self: 'a;

    fn vertex_ids(&self) -> Self::VertexIdsIter<'_>;

    fn vertex_count(&self) -> usize {
        self.vertex_ids().count()
    }

    fn vertex_bound(&self) -> usize
    where
        Self::VertexId: IntegerIdType,
    {
        self.vertex_ids()
            .map(|v| v.as_usize())
            .max()
            .unwrap_or_default()
    }

    fn contains_vertex(&self, id: &Self::VertexId) -> bool {
        self.vertex_ids().any(|v| &v == id)
    }

    fn vertex_id_map(&self) -> CompactIdMap<Self::VertexId>
    where
        Self::VertexId: IntegerIdType,
    {
        // Should be overridden to use `isomorphic` whenever possible.
        CompactIdMap::new(self.vertex_ids())
    }
}

pub trait EdgeSet: GraphBase {
    type EdgeIdsIter<'a>: Iterator<Item = Self::EdgeId>
    where
        Self: 'a;

    type EdgeIdIter<'a>: Iterator<Item = Self::EdgeId>
    where
        Self: 'a;

    fn edge_ids(&self) -> Self::EdgeIdsIter<'_>;

    fn edge_id(&self, src: &Self::VertexId, dst: &Self::VertexId) -> Self::EdgeIdIter<'_>;

    fn endpoints(&self, id: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)>;

    fn edge_count(&self) -> usize {
        self.edge_ids().count()
    }

    fn edge_bound(&self) -> usize
    where
        Self::EdgeId: IntegerIdType,
    {
        self.edge_ids()
            .map(|e| e.as_usize())
            .max()
            .unwrap_or_default()
    }

    fn contains_edge(&self, id: &Self::EdgeId) -> bool {
        self.edge_ids().any(|e| &e == id)
    }

    fn edge_id_any(&self, src: &Self::VertexId, dst: &Self::VertexId) -> Option<Self::EdgeId> {
        self.edge_id(src, dst).next()
    }

    fn edge_id_map(&self) -> CompactIdMap<Self::EdgeId>
    where
        Self::EdgeId: IntegerIdType,
    {
        // Should be overridden to use `isomorphic` whenever possible.
        CompactIdMap::new(self.edge_ids())
    }
}

pub trait GraphRef<V, E>: VertexSet + EdgeSet {
    type VertexRef<'a>: VertexRef<Self::VertexId, V>
    where
        Self: 'a,
        V: 'a;

    type VerticesIter<'a>: Iterator<Item = Self::VertexRef<'a>>
    where
        Self: 'a,
        V: 'a;

    type EdgeRef<'a>: EdgeRef<Self::VertexId, Self::EdgeId, E>
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
    fn vertex_weak(&self, id: &Self::VertexId) -> Option<WeakRef<'_, V>>;
    fn edge_weak(&self, id: &Self::EdgeId) -> Option<WeakRef<'_, E>>;
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
            None => Err(ReplaceVertexError(vertex)),
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
            None => Err(ReplaceEdgeError(edge)),
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
        src: &Self::VertexId,
        dst: &Self::VertexId,
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

    fn add_edge(&mut self, src: &Self::VertexId, dst: &Self::VertexId, edge: E) -> Self::EdgeId {
        match self.try_add_edge(src, dst, edge) {
            Ok(id) => id,
            Err(error) => panic!("{error}"),
        }
    }
}

pub trait GraphFull<V, E>: GraphAdd<V, E> {
    fn remove_vertex(&mut self, id: &Self::VertexId) -> Option<V>;
    fn remove_edge(&mut self, id: &Self::EdgeId) -> Option<E>;

    fn clear(&mut self) {
        let mut vertices = self.vertex_ids().collect::<Vec<_>>();
        vertices.reverse();

        for v in vertices {
            self.remove_vertex(&v);
        }
    }

    fn remove_edge_between(&mut self, src: &Self::VertexId, dst: &Self::VertexId) -> Option<E> {
        let id = self.edge_id_any(src, dst)?;
        self.remove_edge(&id)
    }

    fn clear_edges(&mut self) {
        let mut edges = self.edge_ids().collect::<Vec<_>>();
        edges.reverse();

        for e in edges {
            self.remove_edge(&e);
        }
    }
}

mod imp {
    use crate::core::id::GraphIdTypes;

    use super::*;

    impl<G> GraphBase for &G
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

    impl<G> GraphBase for &mut G
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

    impl<G> GraphIdTypes for G
    where
        G: GraphBase,
    {
        type VertexId = G::VertexId;
        type EdgeId = G::EdgeId;
    }

    impl<V, E, G: GraphRef<V, E>> GraphWeak<V, E> for G {
        fn vertex_weak(&self, id: &Self::VertexId) -> Option<WeakRef<'_, V>> {
            self.vertex(id).map(WeakRef::Borrowed)
        }

        fn edge_weak(&self, id: &Self::EdgeId) -> Option<WeakRef<'_, E>> {
            self.edge(id).map(WeakRef::Borrowed)
        }
    }

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

                fn neighbors(&self, src: &Self::VertexId) -> Self::NeighborsIter<'_> {
                    (**self).neighbors(src)
                }

                fn neighbors_directed(&self, src: &Self::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
                    (**self).neighbors_directed(src, dir)
                }

                fn degree(&self, id: &Self::VertexId) -> usize {
                    (**self).degree(id)
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
                type VertexIdsIter<'a> = G::VertexIdsIter<'a>
                where
                    Self: 'a;

                fn vertex_ids(&self) -> Self::VertexIdsIter<'_> {
                    (**self).vertex_ids()
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
                type EdgeIdsIter<'a> = G::EdgeIdsIter<'a>
                where
                    Self: 'a;

                type EdgeIdIter<'a> = G::EdgeIdIter<'a>
                where
                    Self: 'a;

                fn edge_ids(&self) -> Self::EdgeIdsIter<'_> {
                    (**self).edge_ids()
                }

                fn edge_id(&self, src: &Self::VertexId, dst: &Self::VertexId) -> Self::EdgeIdIter<'_> {
                    (**self).edge_id(src, dst)
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

                fn edge_id_any(&self, src: &Self::VertexId, dst: &Self::VertexId) -> Option<Self::EdgeId> {
                    (**self).edge_id_any(src, dst)
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
}
