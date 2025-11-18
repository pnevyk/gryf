use crate::{
    adapt::{
        Transpose, Undirect,
        cast::{CastAsDirected, CastAsUndirected},
    },
    core::{
        GraphBase, Neighbors, VertexSet,
        id::IdType,
        marker::{Directed, Undirected},
    },
    visit::{Dfs, VisitSet, Visitor},
};

use super::Connected;

pub fn dfs<G>(
    graph: &G,
    between: Option<(&G::VertexId, &G::VertexId)>,
    strong: bool,
) -> Connected<G>
where
    G: Neighbors + VertexSet,
{
    if let Some((start, goal)) = between
        && start == goal
    {
        // A vertex is trivially connected with itself.
        return Connected {
            disconnected_any: None,
            strong: false,
        };
    }

    let disconnected_any = if graph.is_directed() {
        dfs_directed(&CastAsDirected::new(graph), between, strong)
    } else {
        dfs_undirected(&CastAsUndirected::new(graph), between)
    };

    Connected {
        disconnected_any,
        strong,
    }
}

fn dfs_undirected<G, VI>(graph: &G, between: Option<(&VI, &VI)>) -> Option<(VI, VI)>
where
    VI: IdType,
    G: GraphBase<EdgeType = Undirected, VertexId = VI> + Neighbors + VertexSet,
{
    let mut traversal = Dfs::new(&graph);

    match between {
        Some((start, goal)) => {
            for vertex in traversal.start(start.clone()).into_iter(&graph) {
                if &vertex == goal {
                    return None;
                }
            }

            Some((start.clone(), goal.clone()))
        }
        None => {
            let Some(any_vertex) = graph.vertices_by_id().next() else {
                // Empty graph trivially connected.
                return None;
            };

            for _ in traversal.start(any_vertex.clone()).into_iter(&graph) {}

            if traversal.visited().visited_count() == graph.vertex_count() {
                // Visited all vertices.
                return None;
            }

            let other_vertex = graph
                .vertices_by_id()
                .find(|vertex| !traversal.visited().is_visited(vertex))
                .unwrap();

            Some((any_vertex, other_vertex))
        }
    }
}

fn dfs_directed<G, VI>(graph: &G, between: Option<(&VI, &VI)>, strong: bool) -> Option<(VI, VI)>
where
    VI: IdType,
    G: GraphBase<EdgeType = Directed, VertexId = VI> + Neighbors + VertexSet,
{
    if !strong {
        return dfs_undirected(&Undirect::new(graph), between);
    }

    let mut traversal = Dfs::new(graph);

    match between {
        Some((start, goal)) => {
            let mut found = false;

            for vertex in traversal.start(start.clone()).into_iter(graph) {
                if &vertex == goal {
                    found = true;
                    break;
                }
            }

            if !found {
                return Some((start.clone(), goal.clone()));
            }

            // Backwards pass.
            for vertex in traversal.start(goal.clone()).into_iter(graph) {
                if &vertex == start {
                    return None;
                }
            }

            Some((start.clone(), goal.clone()))
        }
        None => {
            let Some(any_vertex) = graph.vertices_by_id().next() else {
                // Empty graph trivially connected.
                return None;
            };

            for _ in traversal.start(any_vertex.clone()).into_iter(graph) {}

            if traversal.visited().visited_count() != graph.vertex_count() {
                let other_vertex = graph
                    .vertices_by_id()
                    .find(|vertex| !traversal.visited().is_visited(vertex))
                    .unwrap();

                return Some((any_vertex, other_vertex));
            }

            // Backwards pass.
            let transpose = Transpose::new(graph);
            let mut backwards = Dfs::new(&transpose);

            for vertex in backwards.start(any_vertex.clone()).into_iter(&transpose) {
                if !traversal.visited_mut().unvisit(&vertex) {
                    return Some((any_vertex.clone(), vertex));
                }
            }

            if traversal.visited().visited_count() == 0 {
                // Visited all vertices.
                None
            } else {
                let other_vertex = graph
                    .vertices_by_id()
                    .find(|vertex| !traversal.visited().is_visited(vertex))
                    .unwrap();

                Some((any_vertex, other_vertex))
            }
        }
    }
}
