use std::cmp::max;

use super::{
    base::IntoEdge,
    graph::GraphAdd,
    id::{IdType, IntegerIdType},
};

pub trait Create<V, E>: GraphAdd<V, E> + Sized {
    fn with_capacity(vertex_count: usize, edge_count: usize) -> Self;

    fn empty() -> Self {
        Self::with_capacity(0, 0)
    }
}

pub trait ExtendWithEdges<T, V, E>
where
    T: IntoEdge<Self, E>,
    V: Default,
    Self: Create<V, E>,
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

impl<T, V, E, G> ExtendWithEdges<T, V, E> for G
where
    T: IntoEdge<Self, E>,
    V: Default,
    G: Create<V, E>,
    Self::VertexId: IntegerIdType,
{
    fn extend_with_edges<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        for edge in iter {
            let (src, dst, edge) = edge.unpack();
            let vertex_bound = max(&src, &dst).as_usize();

            while self.vertex_count() <= vertex_bound {
                self.add_vertex(V::default());
            }

            self.add_edge(&src, &dst, edge);
        }
    }
}

pub trait ExtendWithVertices<V, E>
where
    Self: Create<V, E>,
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

impl<V, E, G> ExtendWithVertices<V, E> for G
where
    G: Create<V, E>,
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
