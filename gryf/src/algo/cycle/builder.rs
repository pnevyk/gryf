use crate::core::{GraphBase, Neighbors, VertexSet};

use super::{Cycle, dfs::dfs_find};

/// Builder for [`Cycle`].
pub struct CycleBuilder<'a, G>
where
    G: GraphBase,
{
    graph: &'a G,
    as_undirected: bool,
}

impl<G> Cycle<G>
where
    G: GraphBase,
{
    #[doc = include_str!("../../../docs/include/algo.on.md")]
    pub fn on(graph: &G) -> CycleBuilder<'_, G> {
        CycleBuilder {
            graph,
            as_undirected: false,
        }
    }
}

impl<'a, G> CycleBuilder<'a, G>
where
    G: GraphBase,
{
    /// Instructs the algorithm to ignore the direction of the edges.
    #[allow(clippy::wrong_self_convention)]
    pub fn as_undirected(self) -> Self {
        Self {
            as_undirected: true,
            ..self
        }
    }
}

impl<'a, G> CycleBuilder<'a, G>
where
    G: GraphBase,
{
    #[doc = include_str!("../../../docs/include/algo.run.md")]
    pub fn run(self) -> Option<Cycle<G>>
    where
        G: Neighbors + VertexSet,
    {
        dfs_find(self.graph, self.as_undirected)
    }
}
