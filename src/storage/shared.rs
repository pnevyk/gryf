use std::fmt;
use std::iter::{Enumerate, Zip};
use std::marker::PhantomData;
use std::ops::Range;
use std::slice::Iter;

use crate::index::{Indexing, NumIndexType};

#[derive(Clone, PartialEq, Eq)]
pub struct AdjVertex<Ix: Indexing, V> {
    pub data: V,
    pub edges: [Vec<Ix::EdgeIndex>; 2],
}

impl<Ix: Indexing, V> AdjVertex<Ix, V> {
    pub fn new(data: V) -> Self {
        Self {
            data,
            edges: [Vec::new(), Vec::new()],
        }
    }
}

impl<Ix, V> fmt::Debug for AdjVertex<Ix, V>
where
    Ix: Indexing,
    Ix::EdgeIndex: fmt::Debug,
    V: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AdjVertex")
            .field("data", &self.data)
            .field("edges", &self.edges)
            .finish()
    }
}

#[derive(Debug)]
pub struct RangeIndices<I: NumIndexType> {
    range: Range<usize>,
    ty: PhantomData<I>,
}

impl<I: NumIndexType> Iterator for RangeIndices<I> {
    type Item = I;

    fn next(&mut self) -> Option<Self::Item> {
        self.range.next().map(I::from_usize)
    }
}

impl<I: NumIndexType> From<Range<usize>> for RangeIndices<I> {
    fn from(range: Range<usize>) -> Self {
        Self {
            range,
            ty: PhantomData,
        }
    }
}

pub struct VerticesIter<'a, Ix, V> {
    inner: Enumerate<Iter<'a, V>>,
    ty: PhantomData<fn() -> Ix>,
}

impl<'a, Ix, V> VerticesIter<'a, Ix, V> {
    pub fn new(inner: Iter<'a, V>) -> Self {
        Self {
            inner: inner.enumerate(),
            ty: PhantomData,
        }
    }
}

impl<'a, Ix: Indexing, V> Iterator for VerticesIter<'a, Ix, V>
where
    Ix::VertexIndex: NumIndexType,
{
    type Item = (Ix::VertexIndex, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|(index, vertex)| (NumIndexType::from_usize(index), vertex))
    }
}

pub struct AdjVerticesIter<'a, Ix: Indexing, V> {
    inner: Enumerate<Iter<'a, AdjVertex<Ix, V>>>,
}

impl<'a, Ix: Indexing, V> AdjVerticesIter<'a, Ix, V> {
    pub fn new(inner: Iter<'a, AdjVertex<Ix, V>>) -> Self {
        Self {
            inner: inner.enumerate(),
        }
    }
}

impl<'a, Ix: Indexing, V> Iterator for AdjVerticesIter<'a, Ix, V>
where
    Ix::VertexIndex: NumIndexType,
{
    type Item = (Ix::VertexIndex, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|(index, vertex)| (NumIndexType::from_usize(index), &vertex.data))
    }
}

pub struct EdgesIter<'a, Ix: Indexing, E> {
    #[allow(clippy::type_complexity)]
    inner: Enumerate<Zip<Iter<'a, E>, Iter<'a, [Ix::VertexIndex; 2]>>>,
}

impl<'a, Ix: Indexing, E> EdgesIter<'a, Ix, E> {
    pub fn new(edges: Iter<'a, E>, endpoints: Iter<'a, [Ix::VertexIndex; 2]>) -> Self {
        Self {
            inner: edges.zip(endpoints).enumerate(),
        }
    }
}

impl<'a, Ix: Indexing, E> Iterator for EdgesIter<'a, Ix, E>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    type Item = (Ix::EdgeIndex, &'a E, Ix::VertexIndex, Ix::VertexIndex);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(index, (edge, endpoints))| {
            (
                NumIndexType::from_usize(index),
                edge,
                endpoints[0],
                endpoints[1],
            )
        })
    }
}
