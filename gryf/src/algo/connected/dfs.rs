use rustc_hash::FxHashSet;

use crate::{
    core::{Neighbors, VertexSet, base::NeighborReference, marker::Direction},
    visit::VisitSet,
};

use super::Connected;

pub fn dfs<G>(
    graph: &G,
    between: Option<(&G::VertexId, &G::VertexId)>,
    as_undirected: bool,
) -> Connected<G>
where
    G: Neighbors + VertexSet,
{
    let (start, goal) = match between {
        Some((start, goal)) => {
            if start == goal {
                // A vertex is trivially connected with itself.
                return Connected {
                    disconnected_any: None,
                    as_undirected,
                };
            } else {
                (start.clone(), Some(goal))
            }
        }
        None if graph.is_directed() && !as_undirected => match graph
            .vertices_by_id()
            // For directed graph, we can't start in any vertex, because then we
            // might miss the predecessors of such vertex and incorrectly mark
            // the graph as disconnected. Only if there is no vertex with in
            // degree 0, it is safe to start in any vertex.
            .find(|v| graph.degree_directed(v, Direction::Incoming) == 0)
            .or_else(|| graph.vertices_by_id().next())
        {
            Some(v) => (v, None),
            None => {
                // Empty graph is trivially "connected".
                return Connected {
                    disconnected_any: None,
                    as_undirected,
                };
            }
        },
        None => match graph.vertices_by_id().next() {
            Some(v) => (v, None),
            None => {
                // Empty graph is trivially "connected".
                return Connected {
                    disconnected_any: None,
                    as_undirected,
                };
            }
        },
    };

    let mut visited = FxHashSet::default();
    visited.reserve(graph.vertex_count());
    let mut stack = vec![start.clone()];

    match (goal, as_undirected) {
        (None, false) => {
            while let Some(v) = stack.pop() {
                if visited.visit(v.clone()) {
                    for n in graph.neighbors_directed(&v, Direction::Outgoing) {
                        stack.push(n.id().into_owned());
                    }
                }
            }
        }
        (None, true) => {
            while let Some(v) = stack.pop() {
                if visited.visit(v.clone()) {
                    for n in graph.neighbors_undirected(&v) {
                        stack.push(n.id().into_owned());
                    }
                }
            }
        }
        (Some(goal), false) => {
            while let Some(v) = stack.pop() {
                if &v == goal {
                    return Connected {
                        disconnected_any: None,
                        as_undirected,
                    };
                }

                if visited.visit(v.clone()) {
                    for n in graph.neighbors_directed(&v, Direction::Outgoing) {
                        stack.push(n.id().into_owned());
                    }
                }
            }
        }
        (Some(goal), true) => {
            while let Some(v) = stack.pop() {
                if &v == goal {
                    return Connected {
                        disconnected_any: None,
                        as_undirected,
                    };
                }

                if visited.visit(v.clone()) {
                    for n in graph.neighbors_undirected(&v) {
                        stack.push(n.id().into_owned());
                    }
                }
            }
        }
    }

    if visited.visited_count() == graph.vertex_count() {
        Connected {
            disconnected_any: None,
            as_undirected,
        }
    } else {
        if let Some(goal) = goal {
            // If the goal was not visited, we can use it specifically for
            // disconnected pair without looking for an arbitrary unvisited
            // vertex.
            if !visited.is_visited(goal) {
                return Connected {
                    disconnected_any: Some((start, goal.clone())),
                    as_undirected,
                };
            }
        }

        let v = graph
            .vertices_by_id()
            .find(|v| !visited.is_visited(v))
            .expect("unvisited vertex");

        Connected {
            disconnected_any: Some((start, v)),
            as_undirected,
        }
    }
}
