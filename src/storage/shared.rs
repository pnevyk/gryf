use std::iter::{Enumerate, Zip};
use std::marker::PhantomData;
use std::ops::Range;
use std::slice::Iter;

use crate::index::{EdgeIndex, IndexType, VertexIndex};

#[derive(Debug)]
pub struct AdjVertex<V> {
    pub data: V,
    pub edges: [Vec<EdgeIndex>; 2],
}

impl<V> AdjVertex<V> {
    pub fn new(data: V) -> Self {
        Self {
            data,
            edges: [Vec::new(), Vec::new()],
        }
    }
}

#[derive(Debug)]
pub struct RangeIndices<I: IndexType> {
    range: Range<usize>,
    ty: PhantomData<I>,
}

impl<I: IndexType> Iterator for RangeIndices<I> {
    type Item = I;

    fn next(&mut self) -> Option<Self::Item> {
        self.range.next().map(IndexType::new)
    }
}

impl<I: IndexType> From<Range<usize>> for RangeIndices<I> {
    fn from(range: Range<usize>) -> Self {
        Self {
            range,
            ty: PhantomData,
        }
    }
}

pub struct VerticesIter<'a, V> {
    inner: Enumerate<Iter<'a, V>>,
}

impl<'a, V> VerticesIter<'a, V> {
    pub fn new(inner: Iter<'a, V>) -> Self {
        Self {
            inner: inner.enumerate(),
        }
    }
}

impl<'a, V> Iterator for VerticesIter<'a, V> {
    type Item = (VertexIndex, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|(index, vertex)| (index.into(), vertex))
    }
}

pub struct AdjVerticesIter<'a, V> {
    inner: Enumerate<Iter<'a, AdjVertex<V>>>,
}

impl<'a, V> AdjVerticesIter<'a, V> {
    pub fn new(inner: Iter<'a, AdjVertex<V>>) -> Self {
        Self {
            inner: inner.enumerate(),
        }
    }
}

impl<'a, V> Iterator for AdjVerticesIter<'a, V> {
    type Item = (VertexIndex, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|(index, vertex)| (index.into(), &vertex.data))
    }
}

pub struct EdgesIter<'a, E> {
    inner: Enumerate<Zip<Iter<'a, E>, Iter<'a, [VertexIndex; 2]>>>,
}

impl<'a, E> EdgesIter<'a, E> {
    pub fn new(edges: Iter<'a, E>, endpoints: Iter<'a, [VertexIndex; 2]>) -> Self {
        Self {
            inner: edges.zip(endpoints).enumerate(),
        }
    }
}

impl<'a, E> Iterator for EdgesIter<'a, E> {
    type Item = (EdgeIndex, &'a E, VertexIndex, VertexIndex);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|(index, (edge, endpoints))| (index.into(), edge, endpoints[0], endpoints[1]))
    }
}
