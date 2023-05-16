use std::{fmt, mem};

use thiserror::Error;

use crate::common::CompactIndexMap;

use super::{
    base::{GraphBase, WeakRef},
    index::{IndexType, NumIndexType},
};

pub trait VertexRef<VI: IndexType, V> {
    fn index(&self) -> &VI;
    fn data(&self) -> &V;
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
    type VertexRef<'a>: VertexRef<Self::VertexIndex, V>
    where
        Self: 'a,
        V: 'a;

    type VerticesIter<'a>: Iterator<Item = Self::VertexRef<'a>>
    where
        Self: 'a,
        V: 'a;

    fn vertex(&self, index: &Self::VertexIndex) -> Option<&V>;
    fn vertices(&self) -> Self::VerticesIter<'_>;

    fn find_vertex(&self, vertex: &V) -> Option<Self::VertexIndex>
    where
        V: Eq,
    {
        self.vertices().find_map(|v| {
            if v.data() == vertex {
                Some(v.index().clone())
            } else {
                None
            }
        })
    }
}

#[derive(Debug, Error, PartialEq)]
#[error("adding vertex failed: {kind}")]
pub struct AddVertexError<V> {
    pub data: V,
    pub kind: AddVertexErrorKind,
}

impl<V> AddVertexError<V> {
    pub fn new(data: V) -> Self {
        Self {
            data,
            kind: AddVertexErrorKind::CapacityOverflow,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddVertexErrorKind {
    CapacityOverflow,
}

impl fmt::Display for AddVertexErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let reason = match self {
            AddVertexErrorKind::CapacityOverflow => "the graph has exhausted its capacity",
        };
        f.write_str(reason)
    }
}

#[derive(Debug, Error)]
#[error("vertex does not exist")]
pub struct ReplaceVertexError<V>(pub V);

pub trait VerticesMut<V>: Vertices<V> {
    fn vertex_mut(&mut self, index: &Self::VertexIndex) -> Option<&mut V>;
    fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexIndex, AddVertexError<V>>;
    fn remove_vertex(&mut self, index: &Self::VertexIndex) -> Option<V>;

    fn add_vertex(&mut self, vertex: V) -> Self::VertexIndex {
        match self.try_add_vertex(vertex) {
            Ok(index) => index,
            Err(error) => panic!("{error}"),
        }
    }

    fn try_replace_vertex(
        &mut self,
        index: &Self::VertexIndex,
        vertex: V,
    ) -> Result<V, ReplaceVertexError<V>> {
        match self.vertex_mut(index) {
            Some(slot) => Ok(mem::replace(slot, vertex)),
            None => Err(ReplaceVertexError(vertex)),
        }
    }

    fn replace_vertex(&mut self, index: &Self::VertexIndex, vertex: V) -> V {
        match self.try_replace_vertex(index, vertex) {
            Ok(original) => original,
            Err(error) => panic!("{error}"),
        }
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

    fn try_get_or_add_vertex(&mut self, vertex: V) -> Result<Self::VertexIndex, AddVertexError<V>>
    where
        V: Eq,
    {
        match self.find_vertex(&vertex) {
            Some(v) => Ok(v),
            None => self.try_add_vertex(vertex),
        }
    }

    fn get_or_add_vertex(&mut self, vertex: V) -> Self::VertexIndex
    where
        V: Eq,
    {
        match self.try_get_or_add_vertex(vertex) {
            Ok(index) => index,
            Err(error) => panic!("{error}"),
        }
    }
}

pub trait VerticesBaseWeak: GraphBase {
    fn vertex_count_hint(&self) -> Option<usize> {
        None
    }

    fn vertex_bound_hint(&self) -> Option<usize> {
        None
    }
}

pub trait VerticesWeak<V>: VerticesBaseWeak {
    fn vertex_weak(&self, index: &Self::VertexIndex) -> Option<WeakRef<'_, V>>;
}

impl<'a, VI: IndexType, V> VertexRef<VI, V> for (VI, &'a V) {
    fn index(&self) -> &VI {
        &self.0
    }

    fn data(&self) -> &V {
        self.1
    }
}

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

    fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexIndex, AddVertexError<V>> {
        (**self).try_add_vertex(vertex)
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
