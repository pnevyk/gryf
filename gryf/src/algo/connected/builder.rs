use crate::core::{GraphBase, Neighbors, VertexSet};

use super::{Connected, dfs::dfs};

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
    pub fn between(self, from: &'a G::VertexId, to: &'a G::VertexId) -> Self {
        Self {
            between: Some((from, to)),
            ..self
        }
    }

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
    pub fn run(self) -> Connected<G>
    where
        G: Neighbors + VertexSet,
    {
        dfs(self.graph, self.between, self.as_undirected)
    }
}
