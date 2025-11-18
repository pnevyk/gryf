use crate::core::{GraphBase, Neighbors, VertexSet, marker::Directed};

use super::{Connected, dfs::dfs};

/// Builder for [`Connected`].
pub struct ConnectedBuilder<'a, G>
where
    G: GraphBase,
{
    graph: &'a G,
    between: Option<(&'a G::VertexId, &'a G::VertexId)>,
    strong: bool,
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
            strong: false,
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
}

impl<'a, G> ConnectedBuilder<'a, G>
where
    G: GraphBase<EdgeType = Directed>,
{
    /// Instructs the algorithm to perform strong connectivity check on the
    /// directed graph.
    pub fn strong(self) -> Self {
        Self {
            strong: true,
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
        dfs(self.graph, self.between, self.strong)
    }
}
