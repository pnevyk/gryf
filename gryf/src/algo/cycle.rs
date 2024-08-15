//! Find a [cycle] in a graph.
//!
//! See available parameters [here](CycleBuilder#implementations).
//!
//! [cycle]: https://en.wikipedia.org/wiki/Cycle_(graph_theory)
//!
//! # Examples
//!
//! ```
//! use gryf::{algo::is_cyclic, Graph};
//!
//! let mut graph = Graph::new_undirected();
//!
//! let a = graph.add_vertex("a");
//! let b = graph.add_vertex("b");
//! let c = graph.add_vertex("c");
//! let d = graph.add_vertex("d");
//!
//! graph.add_edge(a, b, ());
//! graph.add_edge(b, c, ());
//! graph.add_edge(c, d, ());
//!
//! assert!(!is_cyclic(&graph));
//!
//! graph.add_edge(d, a, ());
//!
//! assert!(is_cyclic(&graph));
//! ```

use std::fmt;

use crate::core::{EdgeSet, GraphBase, Neighbors, VertexSet};

use self::bfs::bfs_collect;

mod bfs;
mod builder;
mod dfs;

pub use builder::CycleBuilder;

/// Cycle in a graph.
///
/// See [module](self) documentation for more details and example.
pub struct Cycle<G: GraphBase> {
    /// An edge that is part of the cycle.
    pub edge: G::EdgeId,
    as_undirected: bool,
}

impl<G> fmt::Debug for Cycle<G>
where
    G: GraphBase,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Cycle")
            .field("edge", &self.edge)
            .field("as_undirected", &self.as_undirected)
            .finish()
    }
}

impl<G> Clone for Cycle<G>
where
    G: GraphBase,
{
    fn clone(&self) -> Self {
        Self {
            edge: self.edge.clone(),
            as_undirected: self.as_undirected,
        }
    }
}

impl<G> PartialEq for Cycle<G>
where
    G: GraphBase,
{
    fn eq(&self, other: &Self) -> bool {
        self.edge == other.edge && self.as_undirected == other.as_undirected
    }
}

impl<G> Eq for Cycle<G> where G: GraphBase {}

impl<G: GraphBase> Cycle<G> {
    pub(crate) fn new(edge: G::EdgeId, as_undirected: bool) -> Self {
        Self {
            edge,
            as_undirected,
        }
    }

    /// Collects the whole cycle in the graph.
    pub fn collect(self, graph: &G) -> Vec<G::EdgeId>
    where
        G: Neighbors + VertexSet + EdgeSet,
    {
        let as_undirected = self.as_undirected;
        bfs_collect(graph, self, as_undirected)
    }
}

/// Returns `true` if the graph is cyclic.
pub fn is_cyclic<G>(graph: &G) -> bool
where
    G: Neighbors + VertexSet + EdgeSet,
{
    Cycle::on(graph).run().is_some()
}

/// Returns `true` if the graph is cyclic, ignoring the direction of the edges.
pub fn is_cyclic_undirected<G>(graph: &G) -> bool
where
    G: Neighbors + VertexSet + EdgeSet,
{
    Cycle::on(graph).as_undirected().run().is_some()
}

#[cfg(test)]
mod tests {
    use std::ops::Add;

    use crate::{
        core::{
            GraphAdd,
            marker::{Directed, Undirected},
        },
        storage::AdjList,
    };

    use super::*;

    fn assert_collected<G>(cycle: Cycle<G>, graph: &G, expected: Vec<G::EdgeId>)
    where
        G: Neighbors + VertexSet + EdgeSet,
    {
        let collected = cycle.collect(graph);
        assert_eq!(
            collected.len(),
            expected.len(),
            "collected cycle contains different number of edges than expected"
        );
        let mut collected = collected.into_iter().cycle();

        let count = expected.len();

        if !collected
            .by_ref()
            .take(count)
            .any(|edge| Some(&edge) == expected.first())
        {
            panic!("collected cycle does not include first edge from expected");
        }

        let forward = collected
            .clone()
            .zip(expected.iter().skip(1).cloned())
            .filter(|(lhs, rhs)| lhs == rhs)
            .count()
            .add(1);
        let reversed = collected
            .clone()
            .zip(expected.iter().skip(1).cloned().rev())
            .filter(|(lhs, rhs)| lhs == rhs)
            .count()
            .add(1);

        if forward != count && reversed != count {
            if forward > reversed {
                for (lhs, rhs) in collected.zip(expected.into_iter().skip(1)) {
                    assert_eq!(lhs, rhs);
                }
            } else {
                for (lhs, rhs) in collected.zip(expected.into_iter().skip(1).rev()) {
                    assert_eq!(lhs, rhs);
                }
            }
        }
    }

    #[test]
    fn cyclic_basic_undirected() {
        let mut graph = AdjList::<_, _, Undirected, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v2, &v0, ());

        let result = Cycle::on(&graph).run();
        assert!(result.is_some());
    }

    #[test]
    fn cyclic_basic_directed() {
        let mut graph = AdjList::<_, _, Directed, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v2, &v0, ());

        let result = Cycle::on(&graph).run();
        assert!(result.is_some());
    }

    #[test]
    fn acyclic_basic_undirected() {
        let mut graph = AdjList::<_, _, Undirected, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());

        let result = Cycle::on(&graph).run();
        assert!(result.is_none());
    }

    #[test]
    fn acyclic_basic_directed() {
        let mut graph = AdjList::<_, _, Directed, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());

        let result = Cycle::on(&graph).run();
        assert!(result.is_none());
    }

    #[test]
    fn acyclic_weakly_basic_directed() {
        let mut graph = AdjList::<_, _, Directed, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v0, &v2, ());

        let result = Cycle::on(&graph).run();
        assert!(result.is_none());
    }

    #[test]
    fn cyclic_directed_as_undirected() {
        let mut graph = AdjList::<_, _, Directed, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v0, &v2, ());

        let result = Cycle::on(&graph).as_undirected().run();
        assert!(result.is_some());
    }

    #[test]
    fn cycle_collected_undirected() {
        let mut graph = AdjList::<_, _, Undirected, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());

        let e0 = graph.add_edge(&v0, &v1, ());
        let e1 = graph.add_edge(&v1, &v2, ());
        let e2 = graph.add_edge(&v2, &v0, ());

        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v2, &v4, ());

        let cycle = Cycle::on(&graph).run().unwrap();
        assert_collected(cycle, &graph, vec![e0, e1, e2]);
    }

    #[test]
    fn cycle_collected_directed() {
        let mut graph = AdjList::<_, _, Directed, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());

        let e0 = graph.add_edge(&v0, &v1, ());
        let e1 = graph.add_edge(&v1, &v2, ());
        let e2 = graph.add_edge(&v2, &v0, ());

        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v2, &v4, ());

        let cycle = Cycle::on(&graph).run().unwrap();
        assert_collected(cycle, &graph, vec![e0, e1, e2]);
    }
}
