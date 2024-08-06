use crate::{
    adapt::Undirect,
    core::{Neighbors, VertexSet},
    visit::{DfsEvent, DfsEvents, Visitor},
};

use super::Cycle;

pub fn dfs_find<G>(graph: &G, as_undirected: bool) -> Option<Cycle<G>>
where
    G: Neighbors + VertexSet,
{
    let edge = if as_undirected {
        find(&Undirect::new(graph))
    } else {
        find(graph)
    };

    edge.map(|edge| Cycle {
        edge,
        as_undirected,
    })
}

fn find<G>(graph: &G) -> Option<G::EdgeId>
where
    G: Neighbors + VertexSet,
{
    DfsEvents::new(graph)
        .start_all(graph)
        .into_iter(graph)
        .find_map(|event| match event {
            DfsEvent::BackEdge { edge, .. } => Some(edge),
            _ => None,
        })
}
