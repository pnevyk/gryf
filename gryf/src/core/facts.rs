//! Collection of simple utilities for various properties and calculations.

use super::marker::EdgeType;

/// Returns the number of edges in a [complete graph] given the vertex count and
/// directionality.
///
/// [complete graph]: https://en.wikipedia.org/wiki/Complete_graph
///
/// # Examples
///
/// ```
/// use gryf::core::{facts::complete_graph_edge_count, marker::Undirected};
///
/// assert_eq!(complete_graph_edge_count::<Undirected>(5), 10);
/// ```
pub fn complete_graph_edge_count<Ty: EdgeType>(vertex_count: usize) -> usize {
    if Ty::is_directed() {
        vertex_count * (vertex_count - 1)
    } else {
        vertex_count * (vertex_count - 1) / 2
    }
}
