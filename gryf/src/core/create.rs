use super::{base::IntoEdge, graph::GraphAdd};

pub trait Create<V, E>: GraphAdd<V, E> + Sized {
    fn with_capacity(vertex_count: usize, edge_count: usize) -> Self;

    fn empty() -> Self {
        Self::with_capacity(0, 0)
    }
}

pub trait ExtendWithEdges<T, V, E>: Create<V, E>
where
    T: IntoEdge<Self, E>,
{
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
            let (src, dst, edge) = edge.unpack();
            self.add_edge(&src, &dst, edge);
        }
    }
}

pub trait ExtendWithVertices<V, E>: Create<V, E> {
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
