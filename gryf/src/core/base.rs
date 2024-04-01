use std::{
    borrow::Borrow,
    cmp::{max, Ordering},
    hash::{Hash, Hasher},
    ops::Deref,
};

use super::{
    edges::{EdgesMut, IntoEdge},
    id::{GraphIdTypes, IdType, IntegerIdType},
    marker::EdgeType,
    vertices::VerticesMut,
};

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
    type VertexId: IdType;
    type EdgeId: IdType;
    type EdgeType: EdgeType;
}

impl<G> GraphBase for &G
where
    G: GraphBase,
{
    type VertexId = G::VertexId;
    type EdgeId = G::EdgeId;
    type EdgeType = G::EdgeType;
}

impl<G> GraphBase for &mut G
where
    G: GraphBase,
{
    type VertexId = G::VertexId;
    type EdgeId = G::EdgeId;
    type EdgeType = G::EdgeType;
}

impl<G> GraphIdTypes for G
where
    G: GraphBase,
{
    type VertexId = G::VertexId;
    type EdgeId = G::EdgeId;
}

pub trait Create<V, E, Ty: EdgeType>: VerticesMut<V> + EdgesMut<E, Ty> + Sized {
    fn with_capacity(vertex_count: usize, edge_count: usize) -> Self;

    fn empty() -> Self {
        Self::with_capacity(0, 0)
    }
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
    Self::VertexId: IntegerIdType,
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
