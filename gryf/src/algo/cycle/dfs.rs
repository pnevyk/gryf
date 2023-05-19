use crate::{
    core::{
        marker::EdgeType, EdgesBase, EdgesBaseWeak, GraphBase, Neighbors, VerticesBase,
        VerticesBaseWeak,
    },
    ops::Undirect,
    visit::Visitor,
    visit::{DfsEvent, DfsEvents},
};

use super::Cycle;

pub fn dfs_find<Ty: EdgeType, G>(graph: &G, as_undirected: bool) -> Option<Cycle<G>>
where
    G: Neighbors + VerticesBase + VerticesBaseWeak + EdgesBase<Ty> + EdgesBaseWeak<Ty>,
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

fn find<EI, Ty: EdgeType, G>(graph: &G) -> Option<EI>
where
    G: Neighbors + VerticesBase + VerticesBaseWeak + EdgesBaseWeak<Ty> + GraphBase<EdgeIndex = EI>,
{
    DfsEvents::new(graph)
        .start_all(graph)
        .into_iter(graph)
        .find_map(|event| match event {
            DfsEvent::BackEdge { edge, .. } => Some(edge),
            _ => None,
        })
}
