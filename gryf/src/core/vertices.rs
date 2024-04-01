use std::{fmt, mem};

use thiserror::Error;

use crate::common::CompactIdMap;

use super::{
    base::{GraphBase, VertexRef},
    id::IntegerIdType,
    weak::WeakRef,
};

pub trait VerticesBase: GraphBase {
    type VertexIdsIter<'a>: Iterator<Item = Self::VertexId>
    where
        Self: 'a;

    fn vertex_count(&self) -> usize;
    fn vertex_bound(&self) -> usize;
    fn vertex_ids(&self) -> Self::VertexIdsIter<'_>;

    fn contains_vertex(&self, id: &Self::VertexId) -> bool {
        self.vertex_ids().any(|v| &v == id)
    }

    fn vertex_id_map(&self) -> CompactIdMap<Self::VertexId>
    where
        Self::VertexId: IntegerIdType,
    {
        // Should be overridden to use `isomorphic` whenever possible.
        CompactIdMap::new(self.vertex_ids())
    }
}

pub trait Vertices<V>: VerticesBase {
    type VertexRef<'a>: VertexRef<Self::VertexId, V>
    where
        Self: 'a,
        V: 'a;

    type VerticesIter<'a>: Iterator<Item = Self::VertexRef<'a>>
    where
        Self: 'a,
        V: 'a;

    fn vertex(&self, id: &Self::VertexId) -> Option<&V>;
    fn vertices(&self) -> Self::VerticesIter<'_>;

    fn find_vertex(&self, vertex: &V) -> Option<Self::VertexId>
    where
        V: Eq,
    {
        self.vertices().find_map(|v| {
            if v.data() == vertex {
                Some(v.id().clone())
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
    fn vertex_mut(&mut self, id: &Self::VertexId) -> Option<&mut V>;
    fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>>;
    fn remove_vertex(&mut self, id: &Self::VertexId) -> Option<V>;

    fn add_vertex(&mut self, vertex: V) -> Self::VertexId {
        match self.try_add_vertex(vertex) {
            Ok(id) => id,
            Err(error) => panic!("{error}"),
        }
    }

    fn try_replace_vertex(
        &mut self,
        id: &Self::VertexId,
        vertex: V,
    ) -> Result<V, ReplaceVertexError<V>> {
        match self.vertex_mut(id) {
            Some(slot) => Ok(mem::replace(slot, vertex)),
            None => Err(ReplaceVertexError(vertex)),
        }
    }

    fn replace_vertex(&mut self, id: &Self::VertexId, vertex: V) -> V {
        match self.try_replace_vertex(id, vertex) {
            Ok(original) => original,
            Err(error) => panic!("{error}"),
        }
    }

    fn clear(&mut self) {
        // Should be overridden by an efficient implementation whenever
        // possible.
        let mut vertices = self.vertex_ids().collect::<Vec<_>>();
        vertices.reverse();

        for v in vertices {
            self.remove_vertex(&v);
        }
    }

    fn try_get_or_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>>
    where
        V: Eq,
    {
        match self.find_vertex(&vertex) {
            Some(v) => Ok(v),
            None => self.try_add_vertex(vertex),
        }
    }

    fn get_or_add_vertex(&mut self, vertex: V) -> Self::VertexId
    where
        V: Eq,
    {
        match self.try_get_or_add_vertex(vertex) {
            Ok(id) => id,
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
    fn vertex_weak(&self, id: &Self::VertexId) -> Option<WeakRef<'_, V>>;
}

macro_rules! deref_vertices_base {
    ($($ref_kind:tt)*) => {
        impl<G> VerticesBase for $($ref_kind)* G
        where
            G: VerticesBase,
        {
            type VertexIdsIter<'a> = G::VertexIdsIter<'a>
            where
                Self: 'a;

            fn vertex_count(&self) -> usize {
                (**self).vertex_count()
            }

            fn vertex_bound(&self) -> usize {
                (**self).vertex_bound()
            }

            fn vertex_ids(&self) -> Self::VertexIdsIter<'_> {
                (**self).vertex_ids()
            }

            fn contains_vertex(&self, id: &Self::VertexId) -> bool {
                (**self).contains_vertex(id)
            }

            fn vertex_id_map(&self) -> CompactIdMap<Self::VertexId>
            where
                Self::VertexId: IntegerIdType,
            {
                (**self).vertex_id_map()
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

            fn vertex(&self, id: &Self::VertexId) -> Option<&V> {
                (**self).vertex(id)
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
    fn vertex_mut(&mut self, id: &Self::VertexId) -> Option<&mut V> {
        (**self).vertex_mut(id)
    }

    fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>> {
        (**self).try_add_vertex(vertex)
    }

    fn remove_vertex(&mut self, id: &Self::VertexId) -> Option<V> {
        (**self).remove_vertex(id)
    }

    fn replace_vertex(&mut self, id: &Self::VertexId, vertex: V) -> V {
        (**self).replace_vertex(id, vertex)
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
                fn vertex_weak(&self, id: &Self::VertexId) -> Option<WeakRef<'_, V>> {
                    (**self).vertex_weak(id)
                }
            }
        }
    }

deref_vertices_weak!(&);
deref_vertices_weak!(&mut);
