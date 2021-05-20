use std::ops::Deref;

use crate::index::{EdgeIndex, VertexIndex};
use crate::infra::CompactIndexMap;
use crate::marker::{Direction, EdgeType};
use crate::traits::*;

#[derive(Debug)]
pub struct Frozen<S> {
    inner: S,
}

impl<S> Frozen<S> {
    fn new(inner: S) -> Self {
        Self { inner }
    }

    pub fn into_inner(self) -> S {
        self.inner
    }
}

impl<S> From<S> for Frozen<S> {
    fn from(inner: S) -> Self {
        Self::new(inner)
    }
}

impl<V, S> Vertices<V> for Frozen<S>
where
    S: Vertices<V>,
{
    type VertexIndicesIter<'a, T: 'a> = S::VertexIndicesIter<'a, T>;
    type VertexRef<'a, T: 'a> = S::VertexRef<'a, T>;
    type VerticesIter<'a, T: 'a> = S::VerticesIter<'a, T>;

    fn vertex_count(&self) -> usize {
        self.inner.vertex_count()
    }

    fn vertex_bound(&self) -> usize {
        self.inner.vertex_bound()
    }

    fn vertex(&self, index: VertexIndex) -> Option<&V> {
        self.inner.vertex(index)
    }

    fn vertex_indices(&self) -> Self::VertexIndicesIter<'_, V> {
        self.inner.vertex_indices()
    }

    fn contains_vertex(&self, index: VertexIndex) -> bool {
        self.inner.contains_vertex(index)
    }

    fn vertices(&self) -> Self::VerticesIter<'_, V> {
        self.inner.vertices()
    }

    fn vertex_index_map(&self) -> CompactIndexMap<VertexIndex> {
        self.inner.vertex_index_map()
    }
}

impl<E, Ty: EdgeType, S> Edges<E, Ty> for Frozen<S>
where
    S: Edges<E, Ty>,
{
    type EdgeIndicesIter<'a, T: 'a> = S::EdgeIndicesIter<'a, T>;
    type EdgeRef<'a, T: 'a> = S::EdgeRef<'a, T>;
    type EdgesIter<'a, T: 'a> = S::EdgesIter<'a, T>;

    fn edge_count(&self) -> usize {
        self.inner.edge_count()
    }

    fn edge_bound(&self) -> usize {
        self.inner.edge_bound()
    }

    fn edge(&self, index: EdgeIndex) -> Option<&E> {
        self.inner.edge(index)
    }

    fn endpoints(&self, index: EdgeIndex) -> Option<(VertexIndex, VertexIndex)> {
        self.inner.endpoints(index)
    }

    fn edge_index(&self, src: VertexIndex, dst: VertexIndex) -> Option<EdgeIndex> {
        self.inner.edge_index(src, dst)
    }

    fn edge_indices(&self) -> Self::EdgeIndicesIter<'_, E> {
        self.inner.edge_indices()
    }

    fn contains_edge(&self, index: EdgeIndex) -> bool {
        self.inner.contains_edge(index)
    }

    fn edge_index_map(&self) -> CompactIndexMap<EdgeIndex> {
        self.inner.edge_index_map()
    }

    fn edges(&self) -> Self::EdgesIter<'_, E> {
        self.inner.edges()
    }
}

impl<S> Neighbors for Frozen<S>
where
    S: Neighbors,
{
    type NeighborRef<'a> = S::NeighborRef<'a>;
    type NeighborsIter<'a> = S::NeighborsIter<'a>;

    fn neighbors(&self, src: VertexIndex) -> Self::NeighborsIter<'_> {
        self.inner.neighbors(src)
    }

    fn neighbors_directed(&self, src: VertexIndex, dir: Direction) -> Self::NeighborsIter<'_> {
        self.inner.neighbors_directed(src, dir)
    }
}

impl<S> StableIndices for Frozen<S> {}

impl<S> Deref for Frozen<S> {
    type Target = S;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

pub trait Freeze {
    fn freeze(self) -> Frozen<Self>
    where
        Self: Sized;
}
