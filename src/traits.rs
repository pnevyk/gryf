use std::borrow::Borrow;
use std::cmp::{max, Ordering};
use std::hash::{Hash, Hasher};
use std::mem;
use std::ops::{Add, Deref};

use crate::index::{CustomIndexing, IndexType, Indexing, NumIndexType};
use crate::infra::CompactIndexMap;
use crate::marker::{Direction, EdgeType};

pub trait VertexRef<Ix: Indexing + ?Sized, V> {
    fn index(&self) -> &Ix::VertexIndex;
    fn data(&self) -> &V;
}

pub trait EdgeRef<Ix: Indexing + ?Sized, E> {
    fn index(&self) -> &Ix::EdgeIndex;
    fn data(&self) -> &E;
    fn src(&self) -> &Ix::VertexIndex;
    fn dst(&self) -> &Ix::VertexIndex;
}

#[derive(Debug, Clone)]
pub enum WeakRef<'a, T> {
    Borrowed(&'a T),
    Owned(T),
}

impl<T: Clone> WeakRef<'_, T> {
    pub fn into_owned(self) -> T {
        match self {
            WeakRef::Borrowed(data) => data.clone(),
            WeakRef::Owned(data) => data,
        }
    }
}

impl<T> PartialEq for WeakRef<'_, T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        PartialEq::eq(&**self, &**other)
    }
}

impl<T> Eq for WeakRef<'_, T> where T: Eq {}

impl<T> Hash for WeakRef<'_, T>
where
    T: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }
}

impl<T> PartialOrd for WeakRef<'_, T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        PartialOrd::partial_cmp(&**self, &**other)
    }
}

impl<T> Ord for WeakRef<'_, T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(&**self, &**other)
    }
}

impl<'a, T> From<&'a T> for WeakRef<'a, T> {
    fn from(value: &'a T) -> Self {
        WeakRef::Borrowed(value)
    }
}

impl<T> From<T> for WeakRef<'_, T> {
    fn from(value: T) -> Self {
        WeakRef::Owned(value)
    }
}

impl<T> Deref for WeakRef<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            WeakRef::Borrowed(data) => data,
            WeakRef::Owned(ref data) => data,
        }
    }
}

impl<T> AsRef<T> for WeakRef<'_, T> {
    fn as_ref(&self) -> &T {
        match self {
            WeakRef::Borrowed(data) => data,
            WeakRef::Owned(ref data) => data,
        }
    }
}

impl<T> Borrow<T> for WeakRef<'_, T> {
    fn borrow(&self) -> &T {
        self
    }
}

pub trait GraphBase {
    type VertexIndex: IndexType;
    type EdgeIndex: IndexType;
}

impl<G> GraphBase for &G
where
    G: GraphBase,
{
    type VertexIndex = G::VertexIndex;
    type EdgeIndex = G::EdgeIndex;
}

impl<G> GraphBase for &mut G
where
    G: GraphBase,
{
    type VertexIndex = G::VertexIndex;
    type EdgeIndex = G::EdgeIndex;
}

impl<G> Indexing for G
where
    G: GraphBase,
{
    type VertexIndex = G::VertexIndex;
    type EdgeIndex = G::EdgeIndex;
}

pub trait VerticesBase: GraphBase {
    type VertexIndicesIter<'a>: Iterator<Item = Self::VertexIndex>
    where
        Self: 'a;

    fn vertex_count(&self) -> usize;
    fn vertex_bound(&self) -> usize;
    fn vertex_indices(&self) -> Self::VertexIndicesIter<'_>;

    fn contains_vertex(&self, index: &Self::VertexIndex) -> bool {
        self.vertex_indices().any(|v| &v == index)
    }

    fn vertex_index_map(&self) -> CompactIndexMap<Self::VertexIndex>
    where
        Self::VertexIndex: NumIndexType,
    {
        // Should be overridden to use `isomorphic` whenever possible.
        CompactIndexMap::new(self.vertex_indices())
    }
}

pub trait Vertices<V>: VerticesBase {
    type VertexRef<'a>: VertexRef<CustomIndexing<Self::VertexIndex, Self::EdgeIndex>, V>
    where
        Self: 'a,
        V: 'a;

    type VerticesIter<'a>: Iterator<Item = Self::VertexRef<'a>>
    where
        Self: 'a,
        V: 'a;

    fn vertex(&self, index: &Self::VertexIndex) -> Option<&V>;
    fn vertices(&self) -> Self::VerticesIter<'_>;
}

pub trait VerticesMut<V>: Vertices<V> {
    fn vertex_mut(&mut self, index: &Self::VertexIndex) -> Option<&mut V>;
    fn add_vertex(&mut self, vertex: V) -> Self::VertexIndex;
    fn remove_vertex(&mut self, index: &Self::VertexIndex) -> Option<V>;

    fn replace_vertex(&mut self, index: &Self::VertexIndex, vertex: V) -> V {
        let slot = self.vertex_mut(index).expect("vertex does not exist");
        mem::replace(slot, vertex)
    }

    fn clear(&mut self) {
        // Should be overridden by an efficient implementation whenever
        // possible.
        let mut vertices = self.vertex_indices().collect::<Vec<_>>();
        vertices.reverse();

        for v in vertices {
            self.remove_vertex(&v);
        }
    }
}

pub trait VerticesBaseWeak: GraphBase {
    fn vertex_count_hint(&self) -> Option<usize>;
    fn vertex_bound_hint(&self) -> Option<usize>;
}

pub trait VerticesWeak<V>: VerticesBaseWeak {
    fn vertex_weak(&self, index: &Self::VertexIndex) -> Option<WeakRef<'_, V>>;
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
    type EdgeRef<'a>: EdgeRef<CustomIndexing<Self::VertexIndex, Self::EdgeIndex>, E>
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

pub trait EdgesMut<E, Ty: EdgeType>: Edges<E, Ty> {
    fn edge_mut(&mut self, index: &Self::EdgeIndex) -> Option<&mut E>;
    fn add_edge(
        &mut self,
        src: &Self::VertexIndex,
        dst: &Self::VertexIndex,
        edge: E,
    ) -> Self::EdgeIndex;
    fn remove_edge(&mut self, index: &Self::EdgeIndex) -> Option<E>;

    fn replace_edge(&mut self, index: &Self::EdgeIndex, edge: E) -> E {
        let slot = self.edge_mut(index).expect("edge does not exist");
        mem::replace(slot, edge)
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
    fn edge_count_hint(&self) -> Option<usize>;
    fn edge_bound_hint(&self) -> Option<usize>;
    fn endpoints_weak(
        &self,
        index: &Self::EdgeIndex,
    ) -> Option<(Self::VertexIndex, Self::VertexIndex)>;
    fn edge_index_weak(
        &self,
        src: &Self::VertexIndex,
        dst: &Self::VertexIndex,
    ) -> Option<Self::EdgeIndex>;

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

pub trait NeighborRef<Ix: Indexing + ?Sized> {
    fn index(&self) -> WeakRef<'_, Ix::VertexIndex>;
    fn edge(&self) -> WeakRef<'_, Ix::EdgeIndex>;
    fn src(&self) -> WeakRef<'_, Ix::VertexIndex>;
    fn dir(&self) -> Direction;
}

pub trait Neighbors: GraphBase {
    type NeighborRef<'a>: NeighborRef<CustomIndexing<Self::VertexIndex, Self::EdgeIndex>>
    where
        Self: 'a;

    type NeighborsIter<'a>: Iterator<Item = Self::NeighborRef<'a>>
    where
        Self: 'a;

    fn neighbors(&self, src: &Self::VertexIndex) -> Self::NeighborsIter<'_>;
    fn neighbors_directed(
        &self,
        src: &Self::VertexIndex,
        dir: Direction,
    ) -> Self::NeighborsIter<'_>;

    fn degree(&self, index: &Self::VertexIndex) -> usize {
        self.neighbors(index).count()
    }

    fn degree_directed(&self, index: &Self::VertexIndex, dir: Direction) -> usize {
        self.neighbors_directed(index, dir).count()
    }
}

pub trait IntoEdge<Ix: Indexing, E, Ty: EdgeType> {
    fn unpack(self) -> (Ix::VertexIndex, Ix::VertexIndex, E);
}

pub trait Create<V, E, Ty: EdgeType>: VerticesMut<V> + EdgesMut<E, Ty> + Default {
    fn with_capacity(vertex_count: usize, edge_count: usize) -> Self;
}

pub trait ExtendWithEdges<T, V, E, Ty: EdgeType>
where
    T: IntoEdge<Self, E, Ty>,
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
    T: IntoEdge<Self, E, Ty>,
    V: Default,
    G: Create<V, E, Ty>,
    Self::VertexIndex: NumIndexType,
{
    fn extend_with_edges<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        for edge in iter {
            let (src, dst, edge) = edge.unpack();
            let vertex_bound = max(src, dst).as_usize();

            while self.vertex_count() <= vertex_bound {
                self.add_vertex(V::default());
            }

            self.add_edge(&src, &dst, edge);
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

    impl<'a, Ix: Indexing, V> VertexRef<Ix, V> for (Ix::VertexIndex, &'a V) {
        fn index(&self) -> &Ix::VertexIndex {
            &self.0
        }

        fn data(&self) -> &V {
            self.1
        }
    }

    impl<'a, Ix: Indexing, E> EdgeRef<Ix, E>
        for (Ix::EdgeIndex, &'a E, Ix::VertexIndex, Ix::VertexIndex)
    {
        fn index(&self) -> &Ix::EdgeIndex {
            &self.0
        }

        fn data(&self) -> &E {
            self.1
        }

        fn src(&self) -> &Ix::VertexIndex {
            &self.2
        }

        fn dst(&self) -> &Ix::VertexIndex {
            &self.3
        }
    }

    impl<Ix: Indexing> NeighborRef<Ix>
        for (Ix::VertexIndex, Ix::EdgeIndex, Ix::VertexIndex, Direction)
    {
        fn index(&self) -> WeakRef<'_, Ix::VertexIndex> {
            WeakRef::Borrowed(&self.0)
        }

        fn edge(&self) -> WeakRef<'_, Ix::EdgeIndex> {
            WeakRef::Borrowed(&self.1)
        }

        fn src(&self) -> WeakRef<'_, Ix::VertexIndex> {
            WeakRef::Borrowed(&self.2)
        }

        fn dir(&self) -> Direction {
            self.3
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

    impl<Ix: Indexing, E: Default, Ty: EdgeType, I: Into<Ix::VertexIndex> + Clone>
        IntoEdge<Ix, E, Ty> for &(I, I)
    {
        fn unpack(self) -> (Ix::VertexIndex, Ix::VertexIndex, E) {
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

                fn contains_vertex(&self, index: &Self::VertexIndex) -> bool {
                    (**self).contains_vertex(index)
                }

                fn vertex_index_map(&self) -> CompactIndexMap<Self::VertexIndex>
                where
                    Self::VertexIndex: NumIndexType,
                {
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
                type VertexRef<'a> = G::VertexRef<'a>
                where
                    Self: 'a,
                    V: 'a;

                type VerticesIter<'a> = G::VerticesIter<'a>
                where
                    Self: 'a,
                    V: 'a;

                fn vertex(&self, index: &Self::VertexIndex) -> Option<&V> {
                    (**self).vertex(index)
                }

                fn vertices(&self) -> Self::VerticesIter<'_> {
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
        fn vertex_mut(&mut self, index: &Self::VertexIndex) -> Option<&mut V> {
            (**self).vertex_mut(index)
        }

        fn add_vertex(&mut self, vertex: V) -> Self::VertexIndex {
            (**self).add_vertex(vertex)
        }

        fn remove_vertex(&mut self, index: &Self::VertexIndex) -> Option<V> {
            (**self).remove_vertex(index)
        }

        fn replace_vertex(&mut self, index: &Self::VertexIndex, vertex: V) -> V {
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
                fn vertex_weak(&self, index: &Self::VertexIndex) -> Option<WeakRef<'_, V>> {
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

        fn add_edge(
            &mut self,
            src: &Self::VertexIndex,
            dst: &Self::VertexIndex,
            edge: E,
        ) -> Self::EdgeIndex {
            (**self).add_edge(src, dst, edge)
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

                fn neighbors(&self, src: &Self::VertexIndex) -> Self::NeighborsIter<'_> {
                    (**self).neighbors(src)
                }

                fn neighbors_directed(&self, src: &Self::VertexIndex, dir: Direction) -> Self::NeighborsIter<'_> {
                    (**self).neighbors_directed(src, dir)
                }

                fn degree(&self, index: &Self::VertexIndex) -> usize {
                    (**self).degree(index)
                }

                fn degree_directed(&self, index: &Self::VertexIndex, dir: Direction) -> usize {
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
