use crate::{
    adapt::Transpose,
    core::{GraphBase, Neighbors, VertexSet, marker::Directed},
    visit::{Dfs, DfsPostOrder, VisitSet, Visitor},
};

pub fn kosaraju<G>(graph: &G) -> Vec<Vec<G::VertexId>>
where
    G: GraphBase<EdgeType = Directed> + Neighbors + VertexSet,
{
    let mut traversal = DfsPostOrder::new(graph);
    let mut finished = Vec::new();

    for vertex in traversal.start_all(graph).into_iter(graph) {
        finished.push(vertex);
    }

    let transposed = Transpose::new(graph);
    let mut traversal = Dfs::new(&transposed);

    let mut components = Vec::new();

    let mut iter = finished.iter().rev();

    while let Some(vertex) = iter.next().cloned() {
        if traversal.visited().is_visited(&vertex) {
            continue;
        }

        let mut component = vec![vertex.clone()];

        let visitor = traversal.start(vertex);

        for vertex in visitor.into_iter(&transposed) {
            component.push(vertex);
        }

        components.push(component);
    }

    components
}
