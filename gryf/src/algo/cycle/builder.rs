use crate::core::{GraphBase, Neighbors, VertexSet};

use super::{dfs::dfs_find, Cycle};

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
    pub fn run(self) -> Option<Cycle<G>>
    where
        G: Neighbors + VertexSet,
    {
        dfs_find(self.graph, self.as_undirected)
    }
}
