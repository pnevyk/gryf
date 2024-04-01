use crate::core::{marker::EdgeType, EdgesBase, GraphBase, Neighbors, VerticesBase};

use super::{dfs::dfs, Connected};

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
    pub fn between(self, src: &'a G::VertexId, dst: &'a G::VertexId) -> Self {
        Self {
            between: Some((src, dst)),
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
    pub fn run<Ty: EdgeType>(self) -> Connected<G>
    where
        G: Neighbors + VerticesBase + EdgesBase<Ty>,
    {
        dfs(self.graph, self.between, self.as_undirected)
    }
}
