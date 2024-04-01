use std::{fmt, mem};

use thiserror::Error;

use crate::common::CompactIdMap;

use super::{
    base::{EdgeRef, GraphBase},
    id::IntegerIdType,
    marker::EdgeType,
    weak::WeakRef,
};

pub trait EdgesBase<Ty: EdgeType>: GraphBase {
    type EdgeIdsIter<'a>: Iterator<Item = Self::EdgeId>
    where
        Self: 'a;

    type EdgeIdIter<'a>: Iterator<Item = Self::EdgeId>
    where
        Self: 'a;

    fn edge_count(&self) -> usize;
    fn edge_bound(&self) -> usize;
    fn endpoints(&self, id: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)>;
    fn edge_id(&self, src: &Self::VertexId, dst: &Self::VertexId) -> Self::EdgeIdIter<'_>;
    fn edge_ids(&self) -> Self::EdgeIdsIter<'_>;

    fn contains_edge(&self, id: &Self::EdgeId) -> bool {
        self.endpoints(id).is_some()
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

    fn is_directed(&self) -> bool {
        Ty::is_directed()
    }
}

pub trait Edges<E, Ty: EdgeType>: EdgesBase<Ty> {
    type EdgeRef<'a>: EdgeRef<Self::VertexId, Self::EdgeId, E>
    where
        Self: 'a,
        E: 'a;

    type EdgesIter<'a>: Iterator<Item = Self::EdgeRef<'a>>
    where
        Self: 'a,
        E: 'a;

    fn edge(&self, id: &Self::EdgeId) -> Option<&E>;
    fn edges(&self) -> Self::EdgesIter<'_>;
}

#[derive(Debug, Error, PartialEq)]
#[error("adding edge failed: {kind}")]
pub struct AddEdgeError<E> {
    pub data: E,
    pub kind: AddEdgeErrorKind,
}

impl<E> AddEdgeError<E> {
    pub fn new(data: E, kind: AddEdgeErrorKind) -> Self {
        Self { data, kind }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddEdgeErrorKind {
    SourceAbsent,
    DestinationAbsent,
    MultiEdge,
    CapacityOverflow,
}

impl fmt::Display for AddEdgeErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let reason = match self {
            AddEdgeErrorKind::SourceAbsent => "source does not exist",
            AddEdgeErrorKind::DestinationAbsent => "destination does not exist",
            AddEdgeErrorKind::MultiEdge => {
                "an edge already exists and the graph does not allow multi edges"
            }
            AddEdgeErrorKind::CapacityOverflow => "the graph has exhausted its capacity",
        };
        f.write_str(reason)
    }
}

#[derive(Debug, Error, PartialEq)]
#[error("edge does not exist")]
pub struct ReplaceEdgeError<E>(pub E);

pub trait EdgesMut<E, Ty: EdgeType>: Edges<E, Ty> {
    fn edge_mut(&mut self, id: &Self::EdgeId) -> Option<&mut E>;
    fn try_add_edge(
        &mut self,
        src: &Self::VertexId,
        dst: &Self::VertexId,
        edge: E,
    ) -> Result<Self::EdgeId, AddEdgeError<E>>;
    fn remove_edge(&mut self, id: &Self::EdgeId) -> Option<E>;

    fn add_edge(&mut self, src: &Self::VertexId, dst: &Self::VertexId, edge: E) -> Self::EdgeId {
        match self.try_add_edge(src, dst, edge) {
            Ok(id) => id,
            Err(error) => panic!("{error}"),
        }
    }

    fn remove_edge_between(&mut self, src: &Self::VertexId, dst: &Self::VertexId) -> Option<E> {
        let id = self.edge_id_any(src, dst)?;
        self.remove_edge(&id)
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

    fn clear_edges(&mut self) {
        // Should be overridden by an efficient implementation whenever
        // possible.
        let mut edges = self.edge_ids().collect::<Vec<_>>();
        edges.reverse();

        for e in edges {
            self.remove_edge(&e);
        }
    }
}

pub trait EdgesBaseWeak<Ty: EdgeType>: GraphBase {
    fn endpoints_weak(&self, id: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)>;
    fn edge_id_weak(&self, src: &Self::VertexId, dst: &Self::VertexId) -> Option<Self::EdgeId>;

    fn edge_count_hint(&self) -> Option<usize> {
        None
    }

    fn edge_bound_hint(&self) -> Option<usize> {
        None
    }

    fn is_directed_weak(&self) -> bool {
        Ty::is_directed()
    }
}

pub trait EdgesWeak<E, Ty: EdgeType>: EdgesBaseWeak<Ty> {
    fn edge_weak(&self, id: &Self::EdgeId) -> Option<WeakRef<'_, E>>;
}

pub trait MultiEdges<Ty: EdgeType>: EdgesBase<Ty> {}

macro_rules! deref_edges_base {
    ($($ref_kind:tt)*) => {
        impl<Ty: EdgeType, G> EdgesBase<Ty> for $($ref_kind)* G
        where
            G: EdgesBase<Ty>,
        {
            type EdgeIdsIter<'a> = G::EdgeIdsIter<'a>
            where
                Self: 'a;
            type EdgeIdIter<'a> = G::EdgeIdIter<'a>
            where
                Self: 'a;

            fn edge_count(&self) -> usize {
                (**self).edge_count()
            }

            fn edge_bound(&self) -> usize {
                (**self).edge_bound()
            }

            fn endpoints(&self, id: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)> {
                (**self).endpoints(id)
            }

            fn edge_id(&self, src: &Self::VertexId, dst: &Self::VertexId) -> Self::EdgeIdIter<'_> {
                (**self).edge_id(src, dst)
            }

            fn edge_id_any(&self, src: &Self::VertexId, dst: &Self::VertexId) -> Option<Self::EdgeId> {
                (**self).edge_id_any(src, dst)
            }

            fn edge_ids(&self) -> Self::EdgeIdsIter<'_> {
                (**self).edge_ids()
            }

            fn contains_edge(&self, id: &Self::EdgeId) -> bool {
                (**self).contains_edge(id)
            }

            fn edge_id_map(&self) -> CompactIdMap<Self::EdgeId>
            where
                Self::EdgeId: IntegerIdType
            {
                (**self).edge_id_map()
            }

            fn is_directed(&self) -> bool {
                (**self).is_directed()
            }
        }
    }
}

deref_edges_base!(&);
deref_edges_base!(&mut);

macro_rules! deref_edges {
    ($($ref_kind:tt)*) => {
        impl<E, Ty: EdgeType, G> Edges<E, Ty> for $($ref_kind)* G
        where
            G: Edges<E, Ty>,
        {
            type EdgeRef<'a> = G::EdgeRef<'a>
            where
                Self:'a,
                E: 'a;

            type EdgesIter<'a> = G::EdgesIter<'a>
            where
                Self: 'a,
                E: 'a;

            fn edge(&self, id: &Self::EdgeId) -> Option<&E> {
                (**self).edge(id)
            }

            fn edges(&self) -> Self::EdgesIter<'_> {
                (**self).edges()
            }
        }
    }
}

deref_edges!(&);
deref_edges!(&mut);

impl<E, Ty: EdgeType, G> EdgesMut<E, Ty> for &mut G
where
    G: EdgesMut<E, Ty>,
{
    fn edge_mut(&mut self, id: &Self::EdgeId) -> Option<&mut E> {
        (**self).edge_mut(id)
    }

    fn try_add_edge(
        &mut self,
        src: &Self::VertexId,
        dst: &Self::VertexId,
        edge: E,
    ) -> Result<Self::EdgeId, AddEdgeError<E>> {
        (**self).try_add_edge(src, dst, edge)
    }

    fn remove_edge(&mut self, id: &Self::EdgeId) -> Option<E> {
        (**self).remove_edge(id)
    }

    fn replace_edge(&mut self, id: &Self::EdgeId, edge: E) -> E {
        (**self).replace_edge(id, edge)
    }

    fn clear_edges(&mut self) {
        (**self).clear_edges()
    }
}

macro_rules! deref_edges_base_weak {
    ($($ref_kind:tt)*) => {
        impl<Ty: EdgeType, G> EdgesBaseWeak<Ty> for $($ref_kind)* G
        where
            G: EdgesBaseWeak<Ty>,
        {
            fn edge_count_hint(&self) -> Option<usize> {
                (**self).edge_count_hint()
            }

            fn edge_bound_hint(&self) -> Option<usize> {
                (**self).edge_bound_hint()
            }

            fn endpoints_weak(
                &self,
                id: &Self::EdgeId,
            ) -> Option<(Self::VertexId, Self::VertexId)> {
                (**self).endpoints_weak(id)
            }

            fn edge_id_weak(
                &self,
                src: &Self::VertexId,
                dst: &Self::VertexId,
            ) -> Option<Self::EdgeId> {
                (**self).edge_id_weak(src, dst)
            }

            fn is_directed_weak(&self) -> bool {
                (**self).is_directed_weak()
            }
        }
    }
}

deref_edges_base_weak!(&);
deref_edges_base_weak!(&mut);

macro_rules! deref_edges_weak {
    ($($ref_kind:tt)*) => {
        impl<E, Ty: EdgeType, G> EdgesWeak<E, Ty> for $($ref_kind)* G
        where
            G: EdgesWeak<E, Ty>,
        {
            fn edge_weak(&self, id: &Self::EdgeId) -> Option<WeakRef<'_, E>> {
                (**self).edge_weak(id)
            }
        }
    }
}

deref_edges_weak!(&);
deref_edges_weak!(&mut);
