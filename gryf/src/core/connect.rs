//! Convenient and efficient insertion of edges between vertices.

use super::graph::GraphAdd;

/// Trait for convenient and efficient insertion of edges between vertices using
/// a predicate.
pub trait ConnectVertices<V, E>: GraphAdd<V, E> {
    #[doc = include_str!("../../docs/include/connect_vertices.connect_vertices.md")]
    ///
    /// # Examples
    ///
    /// See [Graph::connect_vertices](crate::domain::Graph::connect_vertices).
    fn connect_vertices<F>(&mut self, connect: F)
    where
        // NOTE: Ideally the API here would be `F: FnMut(Self::VertexRef<'_>, Self::VertexRef<'_>) -> Option<E>`,
        // but the issue is that on the caller's side that would require `V: 'static`
        // due to a known limitation of the current borrow checker
        // (https://blog.rust-lang.org/2022/10/28/gats-stabilization.html#implied-static-requirement-from-higher-ranked-trait-bounds).
        F: FnMut(&V, &V) -> Option<E>;
}
