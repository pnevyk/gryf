use rustc_hash::FxHashSet;

use crate::{
    core::{GraphBase, Neighbors, VertexSet, marker::Undirected},
    visit::{Dfs, Visitor},
};

pub fn dfs<G>(graph: &G) -> Vec<Vec<G::VertexId>>
where
    G: GraphBase<EdgeType = Undirected> + Neighbors + VertexSet,
{
    let mut traversal = Dfs::new(graph);
    let mut remaining = graph.vertices_by_id().collect::<FxHashSet<_>>();
    let mut components = Vec::new();

    while let Some(vertex) = remaining.iter().next().cloned() {
        if !remaining.contains(&vertex) {
            continue;
        }

        remaining.remove(&vertex);
        let mut component = vec![vertex.clone()];

        let visitor = traversal.start(vertex);

        for vertex in visitor.into_iter(graph) {
            remaining.remove(&vertex);
            component.push(vertex);
        }

        components.push(component);
    }

    components
}
