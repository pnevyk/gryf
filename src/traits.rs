use std::cmp::max;
use std::mem;
use std::ops::{Add, Deref};

use crate::index::{EdgeIndex, IndexType, VertexIndex};
use crate::infra::CompactIndexMap;
use crate::marker::{Direction, EdgeType};

pub trait VertexRef<V> {
    fn index(&self) -> VertexIndex;
    fn data(&self) -> &V;
}

pub trait EdgeRef<E> {
    fn index(&self) -> EdgeIndex;
    fn data(&self) -> &E;
    fn src(&self) -> VertexIndex;
    fn dst(&self) -> VertexIndex;
}

pub trait HyperEdgeRef<E> {
    fn index(&self) -> EdgeIndex;
    fn data(&self) -> &E;
    fn vertices(&self) -> &[VertexIndex];
}

enum WeakRefData<'a, T> {
    Borrowed(&'a T),
    Owned(T),
}

pub struct WeakRef<'a, T> {
    data: WeakRefData<'a, T>,
}

impl<'a, T> WeakRef<'a, T> {
    pub fn borrowed(borrowed: &'a T) -> Self {
        Self {
            data: WeakRefData::Borrowed(borrowed),
        }
    }

    pub fn owned(owned: T) -> Self {
        Self {
            data: WeakRefData::Owned(owned),
        }
    }
}

impl<T> Deref for WeakRef<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self.data {
            WeakRefData::Borrowed(data) => data,
            WeakRefData::Owned(ref data) => data,
        }
    }
}

impl<T> AsRef<T> for WeakRef<'_, T> {
    fn as_ref(&self) -> &T {
        match self.data {
            WeakRefData::Borrowed(data) => data,
            WeakRefData::Owned(ref data) => data,
        }
    }
}

pub trait VerticesBase {
    type VertexIndicesIter<'a>: Iterator<Item = VertexIndex>
    where
        Self: 'a;

    fn vertex_count(&self) -> usize;
    fn vertex_bound(&self) -> usize;
    fn vertex_indices(&self) -> Self::VertexIndicesIter<'_>;

    fn contains_vertex(&self, index: VertexIndex) -> bool {
        self.vertex_indices().any(|vertex| vertex == index)
    }

    fn vertex_index_map(&self) -> CompactIndexMap<VertexIndex> {
        // Should be overridden to use `isomorphic` whenever possible.
        CompactIndexMap::new(self.vertex_indices())
    }
}

pub trait Vertices<V>: VerticesBase {
    type VertexRef<'a, T: 'a>: VertexRef<T>
    where
        Self: 'a;

    type VerticesIter<'a, T: 'a>: Iterator<Item = Self::VertexRef<'a, T>>
    where
        Self: 'a;

    fn vertex(&self, index: VertexIndex) -> Option<&V>;
    fn vertices(&self) -> Self::VerticesIter<'_, V>;
}

pub trait VerticesMut<V>: Vertices<V> {
    fn vertex_mut(&mut self, index: VertexIndex) -> Option<&mut V>;
    fn add_vertex(&mut self, vertex: V) -> VertexIndex;
    fn remove_vertex(&mut self, index: VertexIndex) -> Option<V>;

    fn replace_vertex(&mut self, index: VertexIndex, vertex: V) -> V {
        let slot = self.vertex_mut(index).expect("vertex does not exist");
        mem::replace(slot, vertex)
    }

    fn clear(&mut self) {
        // Should be overridden by an efficient implementation whenever
        // possible.
        let mut vertices = self.vertex_indices().collect::<Vec<_>>();
        vertices.reverse();

        for vertex in vertices {
            self.remove_vertex(vertex);
        }
    }
}

pub trait VerticesBaseWeak {
    type VertexIndex = VertexIndex;

    fn vertex_count_hint(&self) -> Option<usize>;
    fn vertex_bound_hint(&self) -> Option<usize>;
}

pub trait VerticesWeak<V>: VerticesBaseWeak {
    fn vertex_weak(&self, index: Self::VertexIndex) -> Option<WeakRef<'_, V>>;
}

pub trait EdgesBase<Ty: EdgeType> {
    type EdgeIndicesIter<'a>: Iterator<Item = EdgeIndex>
    where
        Self: 'a;

    fn edge_count(&self) -> usize;
    fn edge_bound(&self) -> usize;
    fn endpoints(&self, index: EdgeIndex) -> Option<(VertexIndex, VertexIndex)>;
    fn edge_index(&self, src: VertexIndex, dst: VertexIndex) -> Option<EdgeIndex>;
    fn edge_indices(&self) -> Self::EdgeIndicesIter<'_>;

    fn contains_edge(&self, index: EdgeIndex) -> bool {
        self.endpoints(index).is_some()
    }

    fn edge_index_map(&self) -> CompactIndexMap<EdgeIndex> {
        // Should be overridden to use `isomorphic` whenever possible.
        CompactIndexMap::new(self.edge_indices())
    }

    fn is_directed(&self) -> bool {
        Ty::is_directed()
    }
}

pub trait Edges<E, Ty: EdgeType>: EdgesBase<Ty> {
    type EdgeRef<'a, T: 'a>: EdgeRef<T>
    where
        Self: 'a;

    type EdgesIter<'a, T: 'a>: Iterator<Item = Self::EdgeRef<'a, T>>
    where
        Self: 'a;

    fn edge(&self, index: EdgeIndex) -> Option<&E>;
    fn edges(&self) -> Self::EdgesIter<'_, E>;
}

pub trait EdgesMut<E, Ty: EdgeType>: Edges<E, Ty> {
    fn edge_mut(&mut self, index: EdgeIndex) -> Option<&mut E>;
    fn add_edge(&mut self, src: VertexIndex, dst: VertexIndex, edge: E) -> EdgeIndex;
    fn remove_edge(&mut self, index: EdgeIndex) -> Option<E>;

    fn replace_edge(&mut self, index: EdgeIndex, edge: E) -> E {
        let slot = self.edge_mut(index).expect("edge does not exist");
        mem::replace(slot, edge)
    }

    fn clear_edges(&mut self) {
        // Should be overridden by an efficient implementation whenever
        // possible.
        let mut edges = self.edge_indices().collect::<Vec<_>>();
        edges.reverse();

        for edge in edges {
            self.remove_edge(edge);
        }
    }
}

pub trait EdgesBaseWeak<Ty: EdgeType> {
    type VertexIndex = VertexIndex;
    type EdgeIndex = EdgeIndex;

    fn edge_count_hint(&self) -> Option<usize>;
    fn edge_bound_hint(&self) -> Option<usize>;
    fn endpoints_weak(
        &self,
        index: Self::EdgeIndex,
    ) -> Option<(Self::VertexIndex, Self::VertexIndex)>;
    fn edge_index_weak(
        &self,
        src: Self::VertexIndex,
        dst: Self::VertexIndex,
    ) -> Option<Self::EdgeIndex>;

    fn is_directed_weak(&self) -> bool {
        Ty::is_directed()
    }
}

pub trait EdgesWeak<E, Ty: EdgeType>: EdgesBaseWeak<Ty> {
    fn edge_weak(&self, index: Self::EdgeIndex) -> Option<WeakRef<'_, E>>;
}

pub trait MultiEdges<E, Ty: EdgeType>: Edges<E, Ty> {
    type MultiEdgeIndicesIter<'a>: Iterator<Item = EdgeIndex>
    where
        Self: 'a;

    fn multi_edge_index(
        &self,
        src: VertexIndex,
        dst: VertexIndex,
    ) -> Self::MultiEdgeIndicesIter<'_>;
}

pub trait HyperEdges<E, Ty: EdgeType> {
    fn edge_count(&self) -> usize;
    fn edge(&self, index: EdgeIndex) -> Option<&E>;
    fn edge_index(&self, vertices: &[VertexIndex]) -> Option<EdgeIndex>;

    fn contains_edge(&self, index: EdgeIndex) -> bool {
        self.edge(index).is_some()
    }
}

pub trait NeighborRef {
    fn index(&self) -> VertexIndex;
    fn edge(&self) -> EdgeIndex;
    fn src(&self) -> VertexIndex;
    fn dir(&self) -> Direction;
}

pub trait Neighbors {
    type NeighborRef<'a>: NeighborRef
    where
        Self: 'a;

    type NeighborsIter<'a>: Iterator<Item = Self::NeighborRef<'a>>
    where
        Self: 'a;

    fn neighbors(&self, src: VertexIndex) -> Self::NeighborsIter<'_>;
    fn neighbors_directed(&self, src: VertexIndex, dir: Direction) -> Self::NeighborsIter<'_>;

    fn degree(&self, index: VertexIndex) -> usize {
        self.neighbors(index).count()
    }

    fn degree_directed(&self, index: VertexIndex, dir: Direction) -> usize {
        self.neighbors_directed(index, dir).count()
    }
}

pub trait IntoEdge<E, Ty: EdgeType> {
    fn unpack(self) -> (VertexIndex, VertexIndex, E);
}

pub trait Create<V, E, Ty: EdgeType>: VerticesMut<V> + EdgesMut<E, Ty> + Default {
    fn with_capacity(vertex_count: usize, edge_count: usize) -> Self;
}

pub trait ExtendWithEdges<T, V, E, Ty: EdgeType>
where
    T: IntoEdge<E, Ty>,
    V: Default,
    Self: Create<V, E, Ty>,
{
    fn extend_with_edges<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>;

    fn from_edges<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        let iter = iter.into_iter();
        let edge_count = iter.size_hint().1.unwrap_or(32);
        let vertex_count = max(edge_count / 4, 2);

        let mut graph = Self::with_capacity(vertex_count, edge_count);
        graph.extend_with_edges(iter);
        graph
    }
}

impl<T, V, E, Ty: EdgeType, G> ExtendWithEdges<T, V, E, Ty> for G
where
    T: IntoEdge<E, Ty>,
    V: Default,
    G: Create<V, E, Ty>,
{
    fn extend_with_edges<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        for edge in iter {
            let (src, dst, edge) = edge.unpack();
            let vertex_bound = max(src, dst).to_usize();

            while self.vertex_count() <= vertex_bound {
                self.add_vertex(V::default());
            }

            self.add_edge(src, dst, edge);
        }
    }
}

pub trait ExtendWithVertices<V, E, Ty: EdgeType>
where
    Self: Create<V, E, Ty>,
{
    fn extend_with_vertices<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = V>;

    fn from_vertices<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = V>,
    {
        let iter = iter.into_iter();
        let vertex_count = iter.size_hint().1.unwrap_or(32);

        let mut graph = Self::with_capacity(vertex_count, 0);
        graph.extend_with_vertices(iter);
        graph
    }
}

impl<V, E, Ty: EdgeType, G> ExtendWithVertices<V, E, Ty> for G
where
    G: Create<V, E, Ty>,
{
    fn extend_with_vertices<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = V>,
    {
        for vertex in iter {
            self.add_vertex(vertex);
        }
    }
}

pub trait Stability: private::Sealed + 'static {
    fn can_replace_removed() -> bool;
}

#[derive(Debug, Clone, Copy)]
pub enum NoReplace {}

#[derive(Debug, Clone, Copy)]
pub enum ReplaceRemoved {}

impl Stability for NoReplace {
    fn can_replace_removed() -> bool {
        false
    }
}

impl Stability for ReplaceRemoved {
    fn can_replace_removed() -> bool {
        true
    }
}

pub trait StableIndices<T: IndexType, S: Stability> {}

pub trait Guarantee {
    fn is_loop_free() -> bool {
        false
    }

    fn has_paths_only() -> bool {
        false
    }

    fn has_trees_only() -> bool {
        // Paths are also trees by definition.
        Self::has_paths_only()
    }

    fn has_bipartite_only() -> bool {
        // Paths and trees are bipartite by definition.
        Self::has_paths_only() || Self::has_trees_only()
    }

    fn is_connected() -> bool {
        false
    }
}

pub trait Constrained<G> {
    type Error;

    fn check(graph: &G) -> Result<(), Self::Error>;
    fn constrain(graph: G) -> Result<Self, Self::Error>
    where
        Self: Sized;
}

pub trait Weight: Ord + Add<Self, Output = Self> + Clone + Sized {
    fn zero() -> Self;
    fn inf() -> Self;
    fn is_unsigned() -> bool;
}

mod imp {
    use super::*;

    impl<'a, V> VertexRef<V> for (VertexIndex, &'a V) {
        fn index(&self) -> VertexIndex {
            self.0
        }

        fn data(&self) -> &V {
            self.1
        }
    }

    impl<'a, E> EdgeRef<E> for (EdgeIndex, &'a E, VertexIndex, VertexIndex) {
        fn index(&self) -> EdgeIndex {
            self.0
        }

        fn data(&self) -> &E {
            self.1
        }

        fn src(&self) -> VertexIndex {
            self.2
        }

        fn dst(&self) -> VertexIndex {
            self.3
        }
    }

    impl NeighborRef for (VertexIndex, EdgeIndex, VertexIndex, Direction) {
        fn index(&self) -> VertexIndex {
            self.0
        }

        fn edge(&self) -> EdgeIndex {
            self.1
        }

        fn src(&self) -> VertexIndex {
            self.2
        }

        fn dir(&self) -> Direction {
            self.3
        }
    }

    impl<E, Ty: EdgeType, I: Into<VertexIndex>> IntoEdge<E, Ty> for (I, I, E) {
        fn unpack(self) -> (VertexIndex, VertexIndex, E) {
            (self.0.into(), self.1.into(), self.2)
        }
    }

    impl<E: Clone, Ty: EdgeType, I: Into<VertexIndex> + Clone> IntoEdge<E, Ty> for &(I, I, E) {
        fn unpack(self) -> (VertexIndex, VertexIndex, E) {
            (self.0.clone().into(), self.1.clone().into(), self.2.clone())
        }
    }

    impl<E: Default, Ty: EdgeType, I: Into<VertexIndex>> IntoEdge<E, Ty> for (I, I) {
        fn unpack(self) -> (VertexIndex, VertexIndex, E) {
            (self.0.into(), self.1.into(), E::default())
        }
    }

    impl<E: Default, Ty: EdgeType, I: Into<VertexIndex> + Clone> IntoEdge<E, Ty> for &(I, I) {
        fn unpack(self) -> (VertexIndex, VertexIndex, E) {
            (self.0.clone().into(), self.1.clone().into(), E::default())
        }
    }

    macro_rules! impl_num_weight {
        ($ty:ty, $is_unsigned:expr) => {
            impl Weight for $ty {
                fn zero() -> Self {
                    0
                }

                fn inf() -> Self {
                    <$ty>::MAX
                }

                fn is_unsigned() -> bool {
                    $is_unsigned
                }
            }
        };
    }

    impl_num_weight!(i8, false);
    impl_num_weight!(i16, false);
    impl_num_weight!(i32, false);
    impl_num_weight!(i64, false);
    impl_num_weight!(u8, true);
    impl_num_weight!(u16, true);
    impl_num_weight!(u32, true);
    impl_num_weight!(u64, true);
    impl_num_weight!(isize, false);
    impl_num_weight!(usize, true);

    macro_rules! deref_vertices_base {
        ($($ref_kind:tt)*) => {
            impl<G> VerticesBase for $($ref_kind)* G
            where
                G: VerticesBase,
            {
                type VertexIndicesIter<'a> = G::VertexIndicesIter<'a>
                where
                    Self: 'a;

                fn vertex_count(&self) -> usize {
                    (**self).vertex_count()
                }

                fn vertex_bound(&self) -> usize {
                    (**self).vertex_bound()
                }

                fn vertex_indices(&self) -> Self::VertexIndicesIter<'_> {
                    (**self).vertex_indices()
                }

                fn contains_vertex(&self, index: VertexIndex) -> bool {
                    (**self).contains_vertex(index)
                }

                fn vertex_index_map(&self) -> CompactIndexMap<VertexIndex> {
                    (**self).vertex_index_map()
                }
            }
        };
    }

    deref_vertices_base!(&);
    deref_vertices_base!(&mut);

    macro_rules! deref_vertices {
        ($($ref_kind:tt)*) => {
            impl<V, G> Vertices<V> for $($ref_kind)* G
            where
                G: Vertices<V>,
            {
                type VertexRef<'a, T: 'a> = G::VertexRef<'a, T>
                where
                    Self: 'a;

                type VerticesIter<'a, T: 'a> = G::VerticesIter<'a, T>
                where
                    Self: 'a;

                fn vertex(&self, index: VertexIndex) -> Option<&V> {
                    (**self).vertex(index)
                }

                fn vertices(&self) -> Self::VerticesIter<'_, V> {
                    (**self).vertices()
                }
            }
        };
    }

    deref_vertices!(&);
    deref_vertices!(&mut);

    impl<V, G> VerticesMut<V> for &mut G
    where
        G: VerticesMut<V>,
    {
        fn vertex_mut(&mut self, index: VertexIndex) -> Option<&mut V> {
            (**self).vertex_mut(index)
        }

        fn add_vertex(&mut self, vertex: V) -> VertexIndex {
            (**self).add_vertex(vertex)
        }

        fn remove_vertex(&mut self, index: VertexIndex) -> Option<V> {
            (**self).remove_vertex(index)
        }

        fn replace_vertex(&mut self, index: VertexIndex, vertex: V) -> V {
            (**self).replace_vertex(index, vertex)
        }

        fn clear(&mut self) {
            (**self).clear()
        }
    }

    macro_rules! deref_vertices_base_weak {
        ($($ref_kind:tt)*) => {
            impl<G> VerticesBaseWeak for $($ref_kind)* G
            where
                G: VerticesBaseWeak,
            {
                type VertexIndex = G::VertexIndex;

                fn vertex_count_hint(&self) -> Option<usize> {
                    (**self).vertex_count_hint()
                }

                fn vertex_bound_hint(&self) -> Option<usize> {
                    (**self).vertex_bound_hint()
                }
            }
        }
    }

    deref_vertices_base_weak!(&);
    deref_vertices_base_weak!(&mut);

    macro_rules! deref_vertices_weak {
        ($($ref_kind:tt)*) => {
            impl<V, G> VerticesWeak<V> for $($ref_kind)* G
            where
                G: VerticesWeak<V>,
            {
                fn vertex_weak(&self, index: Self::VertexIndex) -> Option<WeakRef<'_, V>> {
                    (**self).vertex_weak(index)
                }
            }
        }
    }

    deref_vertices_weak!(&);
    deref_vertices_weak!(&mut);

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

                fn endpoints(&self, index: EdgeIndex) -> Option<(VertexIndex, VertexIndex)> {
                    (**self).endpoints(index)
                }

                fn edge_index(&self, src: VertexIndex, dst: VertexIndex) -> Option<EdgeIndex> {
                    (**self).edge_index(src, dst)
                }

                fn edge_indices(&self) -> Self::EdgeIndicesIter<'_> {
                    (**self).edge_indices()
                }

                fn contains_edge(&self, index: EdgeIndex) -> bool {
                    (**self).contains_edge(index)
                }

                fn edge_index_map(&self) -> CompactIndexMap<EdgeIndex> {
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
                type EdgeRef<'a, T: 'a> = G::EdgeRef<'a, T>
                where
                    Self:'a;

                type EdgesIter<'a, T: 'a> = G::EdgesIter<'a, T>
                where
                    Self: 'a;

                fn edge(&self, index: EdgeIndex) -> Option<&E> {
                    (**self).edge(index)
                }

                fn edges(&self) -> Self::EdgesIter<'_, E> {
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
        fn edge_mut(&mut self, index: EdgeIndex) -> Option<&mut E> {
            (**self).edge_mut(index)
        }

        fn add_edge(&mut self, src: VertexIndex, dst: VertexIndex, edge: E) -> EdgeIndex {
            (**self).add_edge(src, dst, edge)
        }

        fn remove_edge(&mut self, index: EdgeIndex) -> Option<E> {
            (**self).remove_edge(index)
        }

        fn replace_edge(&mut self, index: EdgeIndex, edge: E) -> E {
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
                type VertexIndex = G::VertexIndex;
                type EdgeIndex = G::EdgeIndex;

                fn edge_count_hint(&self) -> Option<usize> {
                    (**self).edge_count_hint()
                }

                fn edge_bound_hint(&self) -> Option<usize> {
                    (**self).edge_bound_hint()
                }

                fn endpoints_weak(
                    &self,
                    index: Self::EdgeIndex,
                ) -> Option<(Self::VertexIndex, Self::VertexIndex)> {
                    (**self).endpoints_weak(index)
                }

                fn edge_index_weak(
                    &self,
                    src: Self::VertexIndex,
                    dst: Self::VertexIndex,
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
                fn edge_weak(&self, index: Self::EdgeIndex) -> Option<WeakRef<'_, E>> {
                    (**self).edge_weak(index)
                }
            }
        }
    }

    deref_edges_weak!(&);
    deref_edges_weak!(&mut);

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

                fn neighbors(&self, src: VertexIndex) -> Self::NeighborsIter<'_> {
                    (**self).neighbors(src)
                }

                fn neighbors_directed(&self, src: VertexIndex, dir: Direction) -> Self::NeighborsIter<'_> {
                    (**self).neighbors_directed(src, dir)
                }

                fn degree(&self, index: VertexIndex) -> usize {
                    (**self).degree(index)
                }

                fn degree_directed(&self, index: VertexIndex, dir: Direction) -> usize {
                    (**self).degree_directed(index, dir)
                }
            }
        }
    }

    deref_neighbors!(&);
    deref_neighbors!(&mut);
}

mod private {
    use super::*;

    pub trait Sealed {}

    impl Sealed for NoReplace {}
    impl Sealed for ReplaceRemoved {}
}
