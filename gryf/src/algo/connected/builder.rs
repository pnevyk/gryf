use crate::core::{GraphBase, Neighbors, VertexSet};

use super::{Connected, dfs::dfs};

/// Builder for [`Connected`].
pub struct ConnectedBuilder<'a, G>
where
    G: GraphBase,
{
    graph: &'a G,
    between: Option<(&'a G::VertexId, &'a G::VertexId)>,
    as_undirected: bool,
}

impl<G> Connected<G>
where
    G: GraphBase,
{
    #[doc = include_str!("../../../docs/include/algo.on.md")]
    pub fn on(graph: &G) -> ConnectedBuilder<'_, G> {
        ConnectedBuilder {
            graph,
            between: None,
            as_undirected: false,
        }
    }
}

impl<'a, G> ConnectedBuilder<'a, G>
where
    G: GraphBase,
{
    /// Narrows the connectivity check to only these two vertices.
    pub fn between(self, from: &'a G::VertexId, to: &'a G::VertexId) -> Self {
        Self {
            between: Some((from, to)),
            ..self
        }
    }

    /// Instructs the algorithm to ignore the direction of the edges.
    pub fn as_undirected(self) -> Self {
        Self {
            as_undirected: true,
            ..self
        }
    }
}

impl<'a, G> ConnectedBuilder<'a, G>
where
    G: GraphBase,
{
    #[doc = include_str!("../../../docs/include/algo.run.md")]
    pub fn run(self) -> Connected<G>
    where
        G: Neighbors + VertexSet,
    {
        dfs(self.graph, self.between, self.as_undirected)
    }
}
