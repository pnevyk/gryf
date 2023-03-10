use std::{fmt, mem};

use thiserror::Error;

use crate::common::CompactIndexMap;

use super::{
    base::{GraphBase, WeakRef},
    index::{IndexType, Indexing, NumIndexType},
    marker::EdgeType,
};

pub trait EdgeRef<VI: IndexType, EI: IndexType, E> {
    fn index(&self) -> &EI;
    fn data(&self) -> &E;
    fn src(&self) -> &VI;
    fn dst(&self) -> &VI;
}

pub trait EdgesBase<Ty: EdgeType>: GraphBase {
    type EdgeIndicesIter<'a>: Iterator<Item = Self::EdgeIndex>
    where
        Self: 'a;

    fn edge_count(&self) -> usize;
    fn edge_bound(&self) -> usize;
    fn endpoints(&self, index: &Self::EdgeIndex) -> Option<(Self::VertexIndex, Self::VertexIndex)>;
    fn edge_index(
        &self,
        src: &Self::VertexIndex,
        dst: &Self::VertexIndex,
    ) -> Option<Self::EdgeIndex>;
    fn edge_indices(&self) -> Self::EdgeIndicesIter<'_>;

    fn contains_edge(&self, index: &Self::EdgeIndex) -> bool {
        self.endpoints(index).is_some()
    }

    fn edge_index_map(&self) -> CompactIndexMap<Self::EdgeIndex>
    where
        Self::EdgeIndex: NumIndexType,
    {
        // Should be overridden to use `isomorphic` whenever possible.
        CompactIndexMap::new(self.edge_indices())
    }

    fn is_directed(&self) -> bool {
        Ty::is_directed()
    }
}

pub trait Edges<E, Ty: EdgeType>: EdgesBase<Ty> {
    type EdgeRef<'a>: EdgeRef<Self::VertexIndex, Self::EdgeIndex, E>
    where
        Self: 'a,
        E: 'a;

    type EdgesIter<'a>: Iterator<Item = Self::EdgeRef<'a>>
    where
        Self: 'a,
        E: 'a;

    fn edge(&self, index: &Self::EdgeIndex) -> Option<&E>;
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

#[derive(Debug, Error)]
#[error("edge does not exist")]
pub struct ReplaceEdgeError<E>(pub E);

pub trait EdgesMut<E, Ty: EdgeType>: Edges<E, Ty> {
    fn edge_mut(&mut self, index: &Self::EdgeIndex) -> Option<&mut E>;
    fn try_add_edge(
        &mut self,
        src: &Self::VertexIndex,
        dst: &Self::VertexIndex,
        edge: E,
    ) -> Result<Self::EdgeIndex, AddEdgeError<E>>;
    fn remove_edge(&mut self, index: &Self::EdgeIndex) -> Option<E>;

    fn add_edge(
        &mut self,
        src: &Self::VertexIndex,
        dst: &Self::VertexIndex,
        edge: E,
    ) -> Self::EdgeIndex {
        match self.try_add_edge(src, dst, edge) {
            Ok(index) => index,
            Err(error) => panic!("{error}"),
        }
    }

    fn try_replace_edge(
        &mut self,
        index: &Self::EdgeIndex,
        edge: E,
    ) -> Result<E, ReplaceEdgeError<E>> {
        match self.edge_mut(index) {
            Some(slot) => Ok(mem::replace(slot, edge)),
            None => Err(ReplaceEdgeError(edge)),
        }
    }

    fn replace_edge(&mut self, index: &Self::EdgeIndex, edge: E) -> E {
        match self.try_replace_edge(index, edge) {
            Ok(original) => original,
            Err(error) => panic!("{error}"),
        }
    }

    fn clear_edges(&mut self) {
        // Should be overridden by an efficient implementation whenever
        // possible.
        let mut edges = self.edge_indices().collect::<Vec<_>>();
        edges.reverse();

        for e in edges {
            self.remove_edge(&e);
        }
    }
}

pub trait EdgesBaseWeak<Ty: EdgeType>: GraphBase {
    fn endpoints_weak(
        &self,
        index: &Self::EdgeIndex,
    ) -> Option<(Self::VertexIndex, Self::VertexIndex)>;
    fn edge_index_weak(
        &self,
        src: &Self::VertexIndex,
        dst: &Self::VertexIndex,
    ) -> Option<Self::EdgeIndex>;

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
    fn edge_weak(&self, index: &Self::EdgeIndex) -> Option<WeakRef<'_, E>>;
}

pub trait MultiEdges<E, Ty: EdgeType>: Edges<E, Ty> {
    type MultiEdgeIndicesIter<'a>: Iterator<Item = Self::EdgeIndex>
    where
        Self: 'a;

    fn multi_edge_index(
        &self,
        src: &Self::VertexIndex,
        dst: &Self::VertexIndex,
    ) -> Self::MultiEdgeIndicesIter<'_>;
}

pub trait IntoEdge<Ix: Indexing, E, Ty: EdgeType> {
    fn unpack(self) -> (Ix::VertexIndex, Ix::VertexIndex, E);
}

impl<'a, VI: IndexType, EI: IndexType, E> EdgeRef<VI, EI, E> for (EI, &'a E, VI, VI) {
    fn index(&self) -> &EI {
        &self.0
    }

    fn data(&self) -> &E {
        self.1
    }

    fn src(&self) -> &VI {
        &self.2
    }

    fn dst(&self) -> &VI {
        &self.3
    }
}

impl<Ix: Indexing, E, Ty: EdgeType, I: Into<Ix::VertexIndex>> IntoEdge<Ix, E, Ty> for (I, I, E) {
    fn unpack(self) -> (Ix::VertexIndex, Ix::VertexIndex, E) {
        (self.0.into(), self.1.into(), self.2)
    }
}

impl<Ix: Indexing, E: Clone, Ty: EdgeType, I: Into<Ix::VertexIndex> + Clone> IntoEdge<Ix, E, Ty>
    for &(I, I, E)
{
    fn unpack(self) -> (Ix::VertexIndex, Ix::VertexIndex, E) {
        (self.0.clone().into(), self.1.clone().into(), self.2.clone())
    }
}

impl<Ix: Indexing, E: Default, Ty: EdgeType, I: Into<Ix::VertexIndex>> IntoEdge<Ix, E, Ty>
    for (I, I)
{
    fn unpack(self) -> (Ix::VertexIndex, Ix::VertexIndex, E) {
        (self.0.into(), self.1.into(), E::default())
    }
}

impl<Ix: Indexing, E: Default, Ty: EdgeType, I: Into<Ix::VertexIndex> + Clone> IntoEdge<Ix, E, Ty>
    for &(I, I)
{
    fn unpack(self) -> (Ix::VertexIndex, Ix::VertexIndex, E) {
        (self.0.clone().into(), self.1.clone().into(), E::default())
    }
}

macro_rules! deref_edges_base {
    ($($ref_kind:tt)*) => {
        impl<Ty: EdgeType, G> EdgesBase<Ty> for $($ref_kind)* G
        where
            G: EdgesBase<Ty>,
        {
            type EdgeIndicesIter<'a> = G::EdgeIndicesIter<'a>
            where
                Self: 'a;

            fn edge_count(&self) -> usize {
                (**self).edge_count()
            }

            fn edge_bound(&self) -> usize {
                (**self).edge_bound()
            }

            fn endpoints(&self, index: &Self::EdgeIndex) -> Option<(Self::VertexIndex, Self::VertexIndex)> {
                (**self).endpoints(index)
            }

            fn edge_index(&self, src: &Self::VertexIndex, dst: &Self::VertexIndex) -> Option<Self::EdgeIndex> {
                (**self).edge_index(src, dst)
            }

            fn edge_indices(&self) -> Self::EdgeIndicesIter<'_> {
                (**self).edge_indices()
            }

            fn contains_edge(&self, index: &Self::EdgeIndex) -> bool {
                (**self).contains_edge(index)
            }

            fn edge_index_map(&self) -> CompactIndexMap<Self::EdgeIndex>
            where
                Self::EdgeIndex: NumIndexType
            {
                (**self).edge_index_map()
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

            fn edge(&self, index: &Self::EdgeIndex) -> Option<&E> {
                (**self).edge(index)
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
    fn edge_mut(&mut self, index: &Self::EdgeIndex) -> Option<&mut E> {
        (**self).edge_mut(index)
    }

    fn try_add_edge(
        &mut self,
        src: &Self::VertexIndex,
        dst: &Self::VertexIndex,
        edge: E,
    ) -> Result<Self::EdgeIndex, AddEdgeError<E>> {
        (**self).try_add_edge(src, dst, edge)
    }

    fn remove_edge(&mut self, index: &Self::EdgeIndex) -> Option<E> {
        (**self).remove_edge(index)
    }

    fn replace_edge(&mut self, index: &Self::EdgeIndex, edge: E) -> E {
        (**self).replace_edge(index, edge)
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
                index: &Self::EdgeIndex,
            ) -> Option<(Self::VertexIndex, Self::VertexIndex)> {
                (**self).endpoints_weak(index)
            }

            fn edge_index_weak(
                &self,
                src: &Self::VertexIndex,
                dst: &Self::VertexIndex,
            ) -> Option<Self::EdgeIndex> {
                (**self).edge_index_weak(src, dst)
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
            fn edge_weak(&self, index: &Self::EdgeIndex) -> Option<WeakRef<'_, E>> {
                (**self).edge_weak(index)
            }
        }
    }
}

deref_edges_weak!(&);
deref_edges_weak!(&mut);
