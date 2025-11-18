//! Adapters that act as casts to a more constrained type while checking the
//! validity at runtime.

use gryf_derive::{
    EdgeSet, GraphAdd, GraphFull, GraphMut, GraphRef, Guarantee, MultiEdge, Neighbors, VertexSet,
};

use crate::core::{
    GraphBase,
    marker::{Directed, Undirected},
};

/// Cast a graph to a directed graph.
#[derive(
    Debug,
    Neighbors,
    VertexSet,
    EdgeSet,
    GraphRef,
    GraphAdd,
    GraphMut,
    GraphFull,
    MultiEdge,
    Guarantee,
)]
#[gryf_crate]
pub struct CastAsDirected<G> {
    #[graph]
    graph: G,
}

impl<G> GraphBase for CastAsDirected<G>
where
    G: GraphBase,
{
    type VertexId = G::VertexId;
    type EdgeId = G::EdgeId;
    type EdgeType = Directed;
}

impl<G> CastAsDirected<G>
where
    G: GraphBase,
{
    /// Cast a graph to a directed graph.
    ///
    /// # Panics
    ///
    /// Panics if the given graph is not directed.
    pub fn new(graph: G) -> Self {
        assert!(
            graph.is_directed(),
            "undirected graph casted to directed graph"
        );

        Self { graph }
    }
}

/// Cast a graph to an undirected graph.
#[derive(
    Debug,
    Neighbors,
    VertexSet,
    EdgeSet,
    GraphRef,
    GraphAdd,
    GraphMut,
    GraphFull,
    MultiEdge,
    Guarantee,
)]
#[gryf_crate]
pub struct CastAsUndirected<G> {
    #[graph]
    graph: G,
}

impl<G> GraphBase for CastAsUndirected<G>
where
    G: GraphBase,
{
    type VertexId = G::VertexId;
    type EdgeId = G::EdgeId;
    type EdgeType = Undirected;
}

impl<G> CastAsUndirected<G>
where
    G: GraphBase,
{
    /// Cast a graph to an undirected graph.
    ///
    /// # Panics
    ///
    /// Panics if the given graph is not undirected.
    pub fn new(graph: G) -> Self {
        assert!(
            !graph.is_directed(),
            "directed graph casted to undirected graph"
        );

        Self { graph }
    }
}
