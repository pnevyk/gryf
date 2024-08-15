//! Initialization of graphs.

use super::{base::IntoEdge, graph::GraphAdd};

/// Trait for creating a graph with known or estimated capacity.
pub trait Create<V, E>: GraphAdd<V, E> + Sized {
    #[doc = include_str!("../../docs/include/create.with_capacity.md")]
    fn with_capacity(vertex_capacity: usize, edge_capacity: usize) -> Self;

    #[doc = include_str!("../../docs/include/create.empty.md")]
    fn empty() -> Self {
        Self::with_capacity(0, 0)
    }
}

/// Trait for extending graph with or creating graph from an iterator of vertices.
pub trait ExtendWithVertices<V, E>: Create<V, E> {
    #[doc = include_str!("../../docs/include/extend_with_vertices.extend_with_vertices.md")]
    fn extend_with_vertices<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = V>;

    #[doc = include_str!("../../docs/include/extend_with_vertices.from_vertices.md")]
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

/// Trait for extending graph with an iterator of edges.
pub trait ExtendWithEdges<T, V, E>: Create<V, E>
where
    T: IntoEdge<Self, E>,
{
    #[doc = include_str!("../../docs/include/extend_with_edges.extend_with_edges.md")]
    fn extend_with_edges<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>;
}

impl<T, V, E, G> ExtendWithEdges<T, V, E> for G
where
    T: IntoEdge<Self, E>,
    G: Create<V, E>,
{
    fn extend_with_edges<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        for edge in iter {
            let (from, to, edge) = edge.unpack();
            self.add_edge(&from, &to, edge);
        }
    }
}
