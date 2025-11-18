//! Implementations of graph traversal methods.
//!
//! All traversal implementations in this module are **iterative**, that is,
//! they don't use recursion. This means that
//!
//! * &#128077; visitor is lazy and can be stopped without tricks,
//! * &#128077; visitor state is independent on the graph itself, allowing
//!   mutations during traversal,
//! * &#128077; traversal is not limited by the size of the program stack,
//! * &#128078; there is sometimes extra cost if the [traversal semantics ought
//!   to be
//!   respected](https://11011110.github.io/blog/2013/12/17/stack-based-graph-traversal.html).
//!
//! The order in which the neighbors of a vertex are discovered is not specified
//! and should not be relied upon.

pub mod bfs;
pub mod dfs;

pub(crate) mod raw;
mod visit_set;

#[doc(inline)]
pub use self::{
    bfs::Bfs,
    dfs::{Dfs, DfsEvents, DfsNoBacktrack, DfsPostOrder},
    visit_set::{TypedBitSet, VisitSet},
};

use std::collections::VecDeque;

use rustc_hash::FxHashSet;

use raw::*;

use crate::core::{
    GraphBase, Neighbors, VertexSet,
    id::{IdType, UseVertexId},
};

/// Trait for a specific graph traversal approach.
#[doc(alias = "Walker")]
pub trait Visitor<G> {
    /// The type of the elements being visited.
    type Item;

    /// Advances the visitor and returns the next visited element in given
    /// graph.
    ///
    /// The difference from the [`Iterator::next`] is that the visitor doesn't
    /// hold a reference to the graph and thus allows modifications to the graph
    /// between individual visitor steps or passing the visitor around without
    /// lifetime problems.
    ///
    /// # Examples
    ///
    /// ```
    /// use gryf::{
    ///     algo::is_cyclic,
    ///     visit::{DfsEvent, DfsEvents, Visitor},
    ///     Graph,
    /// };
    ///
    /// let mut graph = Graph::<_, (), _>::new_directed();
    ///
    /// graph.extend_with_vertices(["a", "b", "c", "d", "e", "f"]);
    /// graph.extend_with_edges([(0, 1), (1, 2), (2, 3), (3, 0), (3, 4), (4, 5)]);
    ///
    /// // There is a cycle.
    /// assert!(is_cyclic(&graph));
    /// assert_eq!(graph.edge_count(), 6);
    ///
    /// let root = graph.find_vertex("a").unwrap();
    ///
    /// let mut dfs = DfsEvents::new(&graph);
    /// let mut visitor = dfs.start(root);
    ///
    /// while let Some(event) = visitor.visit_next(&graph) {
    ///     if let DfsEvent::BackEdge { edge, .. } = event {
    ///         // Remove edge that would otherwise create a cycle in the graph.
    ///         graph.remove_edge(edge);
    ///     }
    /// }
    ///
    /// // There is no cycle anymore.
    /// assert!(!is_cyclic(&graph));
    /// assert_eq!(graph.edge_count(), 5);
    /// ```
    fn visit_next(&mut self, graph: &G) -> Option<Self::Item>;

    /// Returns an [iterator](Iterator) that uses the visitor to iterate over
    /// the elements in given graph.
    fn iter<'a>(&'a mut self, graph: &'a G) -> Iter<'a, Self, G>
    where
        Self: Sized,
    {
        Iter {
            visitor: self,
            graph,
        }
    }

    /// Converts the visitor into an [iterator](Iterator) to visit the elements
    /// in given graph.
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

/// Visitor iterator returned from [`Visitor::iter`].
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
        self.visitor.visit_next(self.graph)
    }
}

/// Visitor iterator returned from [`Visitor::into_iter`].
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
        self.visitor.visit_next(self.graph)
    }
}

/// A collection of starting vertices for a graph traversal.
///
/// This trait is implemented for any [`Iterator`].
pub trait VisitRoots<I: IdType> {
    /// Returns next ID to start the traversal from.
    ///
    /// Note that the returned ID might have already been visited. It is the
    /// responsibility of the visitor to ignore such elements.
    fn next_root(&mut self) -> Option<I>;

    /// Returns `true` if the collection can determine that all remaining roots
    /// have already been visited based on the currently visited set.
    ///
    /// This is an optimization for early return without the need of iterating
    /// over all roots in cases where this can be determined to be unnecessary.
    /// An example is [`VisitAll`] which checks for [`VisitSet::visited_count`]
    /// being equal to the number of vertices in the original graph.
    ///
    /// By default, `false` is returned which effectively delegates the
    /// indication of being done for [`VisitRoots::next_root`] by returning
    /// `None`.
    fn is_done(&mut self, _visited: &impl VisitSet<I>) -> bool {
        false
    }
}

impl<I: IdType, T> VisitRoots<I> for T
where
    T: Iterator<Item = I>,
{
    fn next_root(&mut self) -> Option<I> {
        self.next()
    }
}

/// A [`VisitRoots`] collection for visiting all vertices in a graph.
pub struct VisitAll<'a, G>
where
    G: VertexSet,
{
    graph: &'a G,
    ids: G::VerticesByIdIter<'a>,
}

impl<'a, G> VisitAll<'a, G>
where
    G: VertexSet,
{
    /// Creates the collection from the given graph.
    pub fn new(graph: &'a G) -> Self {
        Self {
            graph,
            ids: graph.vertices_by_id(),
        }
    }
}

impl<G> VisitRoots<G::VertexId> for VisitAll<'_, G>
where
    G: VertexSet,
{
    fn next_root(&mut self) -> Option<G::VertexId> {
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

/// Strictly monotonically increasing numbering of graph traversal events.
///
/// This is useful to some algorithms that base their decision on the event time
/// comparison.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Time(pub usize);

impl Time {
    /// The largest possible value of time.
    pub const MAX: Time = Time(usize::MAX);
}

/// Depth-first search visitor event.
///
/// Use [`DfsEvents`] visitor to traverse a graph by reporting DFS events.
///
/// # Examples
///
/// See [`DfsEvents`] for examples.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DfsEvent<G>
where
    G: GraphBase,
{
    /// A new vertex was discovered.
    Open {
        /// Discovered vertex.
        vertex: G::VertexId,

        /// Discovering time.
        time: Time,
    },

    /// An edge of the tree formed by the traversal.
    TreeEdge {
        /// Source endpoint of the edge.
        from: G::VertexId,

        /// Target endpoint of the edge.
        to: G::VertexId,

        /// Edge ID.
        edge: G::EdgeId,
    },

    /// An edge to an already [closed](DfsEvent::Close) vertex.
    ///
    /// Presence of a back edge indicates a cycle in the graph.
    BackEdge {
        /// Source endpoint of the edge.
        from: G::VertexId,

        /// Target (closed) endpoint of the edge.
        to: G::VertexId,

        /// Edge ID.
        edge: G::EdgeId,
    },

    /// An edge to an already [discovered](DfsEvent::Open) but not yet
    /// [closed](DfsEvent::Close) vertex.
    ///
    /// Cross edge is an edge between vertices in different "branches" of the
    /// traversal tree. Forward edge is an edge between vertices in the same
    /// "branch" of the traversal tree. When the [discover
    /// time](DfsEvent::Open::time) of the `from` vertex is higher than `to`
    /// vertex, it's cross edge, otherwise it's forward edge.
    ///
    /// In undirected graphs there is no concept of cross or forward edges.
    CrossForwardEdge {
        /// Source endpoint of the edge.
        from: G::VertexId,

        /// Target (closed) endpoint of the edge.
        to: G::VertexId,

        /// Edge ID.
        edge: G::EdgeId,
    },

    /// All edges from the vertex have been reported.
    Close {
        /// Closed vertex.
        vertex: G::VertexId,

        /// Closing time.
        time: Time,
    },
}

#[cfg(test)]
mod tests {
    use crate::{
        core::{
            GraphAdd, GraphFull,
            id::DefaultId,
            marker::{Directed, Undirected},
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
                from: $u,
                to: $v,
                edge: $e,
            }
        };
        (back, $e:expr, ($u:expr, $v:expr)) => {
            DfsEvent::BackEdge {
                from: $u,
                to: $v,
                edge: $e,
            }
        };
        (cross_forward, $e:expr, ($u:expr, $v:expr)) => {
            DfsEvent::CrossForwardEdge {
                from: $u,
                to: $v,
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
