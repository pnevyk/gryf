use std::{
    iter::{Enumerate, Zip},
    marker::PhantomData,
    ops::Range,
    slice::Iter,
};

use crate::core::{
    base::{EdgeRef, VertexRef},
    id::{IdPair, IdType, IntegerIdType},
    marker::EdgeType,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AdjVertex<Id: IdPair, V> {
    pub attr: V,
    pub edges: [Vec<Id::EdgeId>; 2],
}

impl<Id: IdPair, V> AdjVertex<Id, V> {
    pub fn new(attr: V) -> Self {
        Self {
            attr,
            edges: [Vec::new(), Vec::new()],
        }
    }
}

#[derive(Debug)]
pub struct RangeIds<I: IntegerIdType> {
    range: Range<usize>,
    ty: PhantomData<I>,
}

impl<I: IntegerIdType> Iterator for RangeIds<I> {
    type Item = I;

    fn next(&mut self) -> Option<Self::Item> {
        self.range.next().map(I::from_usize)
    }
}

impl<I: IntegerIdType> From<Range<usize>> for RangeIds<I> {
    fn from(range: Range<usize>) -> Self {
        Self {
            range,
            ty: PhantomData,
        }
    }
}

pub struct VerticesIter<'a, Id, V> {
    inner: Enumerate<Iter<'a, V>>,
    ty: PhantomData<fn() -> Id>,
}

impl<'a, Id, V> VerticesIter<'a, Id, V> {
    pub fn new(inner: Iter<'a, V>) -> Self {
        Self {
            inner: inner.enumerate(),
            ty: PhantomData,
        }
    }
}

impl<'a, Id: IdPair, V> Iterator for VerticesIter<'a, Id, V>
where
    Id::VertexId: IntegerIdType,
{
    type Item = VertexRef<'a, Id::VertexId, V>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(id, vertex)| VertexRef {
            id: IdType::from_usize(id),
            attr: vertex,
        })
    }
}

pub struct AdjVerticesIter<'a, Id: IdPair, V> {
    inner: Enumerate<Iter<'a, AdjVertex<Id, V>>>,
}

impl<'a, Id: IdPair, V> AdjVerticesIter<'a, Id, V> {
    pub fn new(inner: Iter<'a, AdjVertex<Id, V>>) -> Self {
        Self {
            inner: inner.enumerate(),
        }
    }
}

impl<'a, Id: IdPair, V> Iterator for AdjVerticesIter<'a, Id, V>
where
    Id::VertexId: IntegerIdType,
{
    type Item = VertexRef<'a, Id::VertexId, V>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(id, vertex)| VertexRef {
            id: IdType::from_usize(id),
            attr: &vertex.attr,
        })
    }
}

pub struct EdgesIter<'a, Id: IdPair, E> {
    #[allow(clippy::type_complexity)]
    inner: Enumerate<Zip<Iter<'a, E>, Iter<'a, [Id::VertexId; 2]>>>,
}

impl<'a, Id: IdPair, E> EdgesIter<'a, Id, E> {
    pub fn new(edges: Iter<'a, E>, endpoints: Iter<'a, [Id::VertexId; 2]>) -> Self {
        Self {
            inner: edges.zip(endpoints).enumerate(),
        }
    }
}

impl<'a, Id: IdPair, E> Iterator for EdgesIter<'a, Id, E>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    type Item = EdgeRef<'a, Id::VertexId, Id::EdgeId, E>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(index, (attr, endpoints))| EdgeRef {
            id: IdType::from_usize(index),
            attr,
            from: endpoints[0],
            to: endpoints[1],
        })
    }
}

pub fn connect_vertices<Ty: EdgeType>(vertex_count: usize, mut connect: impl FnMut(usize, usize)) {
    for i in 0..vertex_count {
        // Self-loops are considered too.
        let begin = if Ty::is_directed() { 0 } else { i };
        for j in begin..vertex_count {
            connect(i, j);
        }
    }
}
