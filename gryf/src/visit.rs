pub mod bfs;
pub mod dfs;

pub(crate) mod raw;
mod visit_set;

pub use bfs::Bfs;
pub use dfs::{Dfs, DfsEvents, DfsNoBacktrack, DfsPostOrder};
pub use visit_set::{TypedBitSet, VisitSet};

use std::{
    collections::{HashSet, VecDeque},
    hash::BuildHasherDefault,
};

use rustc_hash::FxHashSet;

use raw::*;

use crate::core::{id::UseVertexId, GraphBase, Neighbors, VertexSet};

pub trait Visitor<G> {
    type Item;

    fn next(&mut self, graph: &G) -> Option<Self::Item>;

    fn iter<'a>(&'a mut self, graph: &'a G) -> Iter<'a, Self, G>
    where
        Self: Sized,
    {
        Iter {
            visitor: self,
            graph,
        }
    }

    fn into_iter(self, graph: &G) -> IntoIter<'_, Self, G>
    where
        Self: Sized,
    {
        IntoIter {
            visitor: self,
            graph,
        }
    }
}

pub struct Iter<'a, V, G> {
    visitor: &'a mut V,
    graph: &'a G,
}

impl<'a, V, G> Iterator for Iter<'a, V, G>
where
    V: Visitor<G>,
{
    type Item = V::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.visitor.next(self.graph)
    }
}

pub struct IntoIter<'a, V, G> {
    visitor: V,
    graph: &'a G,
}

impl<'a, V, G> Iterator for IntoIter<'a, V, G>
where
    V: Visitor<G>,
{
    type Item = V::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.visitor.next(self.graph)
    }
}

pub struct VisitAll<'a, G>
where
    G: VertexSet,
{
    graph: &'a G,
    ids: G::VertexIdsIter<'a>,
}

impl<'a, G> VisitAll<'a, G>
where
    G: VertexSet,
{
    pub fn new(graph: &'a G) -> Self {
        Self {
            graph,
            ids: graph.vertex_ids(),
        }
    }
}

impl<G> VisitStarts<G::VertexId> for VisitAll<'_, G>
where
    G: VertexSet,
{
    fn get_next(&mut self) -> Option<G::VertexId> {
        self.ids.next()
    }

    // Optimize the early return for cases when the whole graph was traversed
    // yet the `self.ids` iterator (containing all vertices in the graph at the
    // beginning) has not been exhausted.
    fn is_done(&mut self, visited: &impl VisitSet<G::VertexId>) -> bool {
        // Since we are holding a shared reference to the graph, it could not
        // have been mutated during traversal. In particular, the number of
        // vertices didn't change.
        visited.visited_count() == self.graph.vertex_count()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Time(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DfsEvent<G>
where
    G: GraphBase,
{
    Open {
        vertex: G::VertexId,
        time: Time,
    },
    TreeEdge {
        src: G::VertexId,
        dst: G::VertexId,
        edge: G::EdgeId,
    },
    BackEdge {
        src: G::VertexId,
        dst: G::VertexId,
        edge: G::EdgeId,
    },
    CrossForwardEdge {
        src: G::VertexId,
        dst: G::VertexId,
        edge: G::EdgeId,
    },
    Close {
        vertex: G::VertexId,
        time: Time,
    },
}

#[cfg(test)]
mod tests {
    use crate::{
        core::{
            id::DefaultId,
            marker::{Directed, Undirected},
            GraphAdd, GraphFull,
        },
        storage::{AdjList, Stable},
    };

    use super::*;

    macro_rules! dfs_event {
        (open, $v:expr, $t:expr) => {
            DfsEvent::Open {
                vertex: $v,
                time: Time($t),
            }
        };
        (tree, $e:expr, ($u:expr, $v:expr)) => {
            DfsEvent::TreeEdge {
                src: $u,
                dst: $v,
                edge: $e,
            }
        };
        (back, $e:expr, ($u:expr, $v:expr)) => {
            DfsEvent::BackEdge {
                src: $u,
                dst: $v,
                edge: $e,
            }
        };
        (cross_forward, $e:expr, ($u:expr, $v:expr)) => {
            DfsEvent::CrossForwardEdge {
                src: $u,
                dst: $v,
                edge: $e,
            }
        };
        (close, $v:expr, $t:expr) => {
            DfsEvent::Close {
                vertex: $v,
                time: Time($t),
            }
        };
    }

    #[test]
    fn bfs_connected() {
        let mut graph = AdjList::<_, _, Undirected, DefaultId>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        let vertices = Bfs::new(&graph).start(v0).iter(&graph).collect::<Vec<_>>();

        assert_eq!(vertices, vec![v0, v1, v2, v3, v4, v5]);
    }

    #[test]
    fn dfs_connected() {
        let mut graph = AdjList::<_, _, Undirected, DefaultId>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        let vertices = Dfs::new(&graph).start(v0).iter(&graph).collect::<Vec<_>>();

        assert_eq!(vertices, vec![v0, v1, v4, v5, v2, v3]);
    }

    #[test]
    fn bfs_disconnected() {
        let mut graph = Stable::new(AdjList::<_, _, Undirected, DefaultId>::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        graph.remove_vertex(&v1);

        let vertices = Bfs::new(&graph).start(v2).iter(&graph).collect::<Vec<_>>();

        assert_eq!(vertices, vec![v2, v5, v4]);
    }

    #[test]
    fn dfs_disconnected() {
        let mut graph = Stable::new(AdjList::<_, _, Undirected, DefaultId>::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        graph.remove_vertex(&v1);

        let vertices = Dfs::new(&graph).start(v2).iter(&graph).collect::<Vec<_>>();

        assert_eq!(vertices, vec![v2, v5, v4]);
    }

    #[test]
    fn bfs_disconnected_all() {
        let mut graph = Stable::new(AdjList::<_, _, Undirected, DefaultId>::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        graph.remove_vertex(&v1);

        let vertices = Bfs::new(&graph)
            .start_all(&graph)
            .iter(&graph)
            .collect::<Vec<_>>();

        assert_eq!(vertices, vec![v0, v2, v5, v4, v3]);
    }

    #[test]
    fn dfs_disconnected_all() {
        let mut graph = Stable::new(AdjList::<_, _, Undirected, DefaultId>::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        graph.remove_vertex(&v1);

        let vertices = Dfs::new(&graph)
            .start_all(&graph)
            .iter(&graph)
            .collect::<Vec<_>>();

        assert_eq!(vertices, vec![v0, v2, v5, v4, v3]);
    }

    #[test]
    fn bfs_disconnected_multi() {
        let mut graph = Stable::new(AdjList::<_, _, Undirected, DefaultId>::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        graph.remove_vertex(&v1);

        let vertices = Bfs::new(&graph)
            .start_multi([v0, v2].into_iter())
            .iter(&graph)
            .collect::<Vec<_>>();

        assert_eq!(vertices, vec![v0, v2, v5, v4]);
    }

    #[test]
    fn bfs_disconnected_multi_mutation() {
        let mut graph = Stable::new(AdjList::<_, _, Undirected, DefaultId>::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        graph.remove_vertex(&v1);

        let mut visit = Bfs::new(&graph);
        let mut visit = visit.start_multi([v0, v2].into_iter());

        // Remove one of the starts.
        graph.remove_vertex(&v0);

        let vertices = visit.iter(&graph).collect::<Vec<_>>();

        // Removing one of the start causes incomplete visit, but not a crash.
        assert_eq!(vertices, vec![v2, v5, v4]);
    }

    #[test]
    fn dfs_events() {
        // The graph with edge types (assuming implementation details of
        // `AdjList`).
        //
        //                          +----------------- T -----+
        //                          |                         |
        //               +-- B --- (2) -- B --- (3) -- T --- (4)
        //               |                       |
        // (0) -- T --- (1)                      T
        //               |                       |
        //               +-- T --- (5) -- B --- (6) -- T --- (7)
        //                          |                         |
        //                          +---- T ------------------+

        let mut graph = AdjList::<_, _, Undirected, DefaultId>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());
        let v6 = graph.add_vertex(());
        let v7 = graph.add_vertex(());

        let e0 = graph.add_edge(&v0, &v1, ());
        let e1 = graph.add_edge(&v1, &v2, ());
        let e2 = graph.add_edge(&v2, &v3, ());
        let e3 = graph.add_edge(&v3, &v4, ());
        let e4 = graph.add_edge(&v4, &v2, ());
        let e5 = graph.add_edge(&v1, &v5, ());
        let e6 = graph.add_edge(&v5, &v6, ());
        let e7 = graph.add_edge(&v6, &v7, ());
        let e8 = graph.add_edge(&v5, &v7, ());
        let e9 = graph.add_edge(&v3, &v6, ());

        let events = DfsEvents::new(&graph)
            .start(v0)
            .iter(&graph)
            .collect::<Vec<_>>();

        let expected = vec![
            dfs_event!(open, v0, 0),
            dfs_event!(tree, e0, (v0, v1)),
            dfs_event!(open, v1, 1),
            dfs_event!(tree, e5, (v1, v5)),
            dfs_event!(open, v5, 2),
            dfs_event!(tree, e8, (v5, v7)),
            dfs_event!(open, v7, 3),
            dfs_event!(tree, e7, (v7, v6)),
            dfs_event!(open, v6, 4),
            dfs_event!(tree, e9, (v6, v3)),
            dfs_event!(open, v3, 5),
            dfs_event!(tree, e3, (v3, v4)),
            dfs_event!(open, v4, 6),
            dfs_event!(tree, e4, (v4, v2)),
            dfs_event!(open, v2, 7),
            dfs_event!(back, e2, (v2, v3)),
            dfs_event!(back, e1, (v2, v1)),
            dfs_event!(close, v2, 8),
            dfs_event!(close, v4, 9),
            dfs_event!(close, v3, 10),
            dfs_event!(back, e6, (v6, v5)),
            dfs_event!(close, v6, 11),
            dfs_event!(close, v7, 12),
            dfs_event!(close, v5, 13),
            dfs_event!(close, v1, 14),
            dfs_event!(close, v0, 15),
        ];

        assert_eq!(events, expected);
    }

    #[test]
    fn dfs_events_directed() {
        // The graph with edge types (assuming implementation details of
        // `AdjList`).
        //
        //                          +----------------- B -----+
        //                          v                         |
        //               +-- T --> (2) -- T --> (3) -- T --> (4)
        //               |                       |
        // (0) -- T --> (1)                      C
        //               |                       v
        //               +-- T --> (5) -- T --> (6) -- F --> (7)
        //                          |                         ʌ
        //                          +---- T ------------------+

        let mut graph = AdjList::<_, _, Directed, DefaultId>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());
        let v6 = graph.add_vertex(());
        let v7 = graph.add_vertex(());

        let e0 = graph.add_edge(&v0, &v1, ());
        let e1 = graph.add_edge(&v1, &v2, ());
        let e2 = graph.add_edge(&v2, &v3, ());
        let e3 = graph.add_edge(&v3, &v4, ());
        let e4 = graph.add_edge(&v4, &v2, ());
        let e5 = graph.add_edge(&v1, &v5, ());
        let e6 = graph.add_edge(&v5, &v6, ());
        let e7 = graph.add_edge(&v6, &v7, ());
        let e8 = graph.add_edge(&v5, &v7, ());
        let e9 = graph.add_edge(&v3, &v6, ());

        let events = DfsEvents::new(&graph)
            .start(v0)
            .iter(&graph)
            .collect::<Vec<_>>();

        let expected = vec![
            dfs_event!(open, v0, 0),
            dfs_event!(tree, e0, (v0, v1)),
            dfs_event!(open, v1, 1),
            dfs_event!(tree, e5, (v1, v5)),
            dfs_event!(open, v5, 2),
            dfs_event!(tree, e8, (v5, v7)),
            dfs_event!(open, v7, 3),
            dfs_event!(close, v7, 4),
            dfs_event!(tree, e6, (v5, v6)),
            dfs_event!(open, v6, 5),
            dfs_event!(cross_forward, e7, (v6, v7)),
            dfs_event!(close, v6, 6),
            dfs_event!(close, v5, 7),
            dfs_event!(tree, e1, (v1, v2)),
            dfs_event!(open, v2, 8),
            dfs_event!(tree, e2, (v2, v3)),
            dfs_event!(open, v3, 9),
            dfs_event!(cross_forward, e9, (v3, v6)),
            dfs_event!(tree, e3, (v3, v4)),
            dfs_event!(open, v4, 10),
            dfs_event!(back, e4, (v4, v2)),
            dfs_event!(close, v4, 11),
            dfs_event!(close, v3, 12),
            dfs_event!(close, v2, 13),
            dfs_event!(close, v1, 14),
            dfs_event!(close, v0, 15),
        ];

        assert_eq!(events, expected);
    }

    #[test]
    fn dfs_events_isolated() {
        let mut graph = AdjList::<_, (), Undirected, DefaultId>::new();

        let v0 = graph.add_vertex(());

        let events = DfsEvents::new(&graph)
            .start(v0)
            .iter(&graph)
            .collect::<Vec<_>>();

        let expected = vec![dfs_event!(open, v0, 0), dfs_event!(close, v0, 1)];

        assert_eq!(events, expected);
    }

    #[test]
    fn dfs_post_order() {
        let mut graph = AdjList::<_, _, Undirected, DefaultId>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        let vertices = DfsPostOrder::new(&graph)
            .start(v0)
            .iter(&graph)
            .collect::<Vec<_>>();

        assert_eq!(vertices, vec![v2, v5, v4, v3, v1, v0]);
    }

    #[test]
    fn dfs_no_backtrack() {
        let mut graph = AdjList::<_, _, Undirected, DefaultId>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        let vertices = DfsNoBacktrack::new(&graph)
            .start(v0)
            .iter(&graph)
            .collect::<Vec<_>>();

        // v3 is missing because it would be visited only if the algorithm would
        // continue from v1 after traversing the "long" branch.
        assert_eq!(vertices, vec![v0, v1, v2, v5, v4]);
    }
}
