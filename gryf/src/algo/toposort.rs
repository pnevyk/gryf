//! Find a [topologically sorted] collection of vertices on a [directed acyclic
//! graph] (DAG).
//!
//! See available parameters [here](TopoSortBuilder#implementations).
//!
//! The exact order in which the vertices are reported is not specified and
//! should not be relied upon.
//!
//! [topologically sorted]: https://en.wikipedia.org/wiki/Topological_sorting
//! [directed acyclic graph]:
//!     https://en.wikipedia.org/wiki/Directed_acyclic_graph
//!
//! # Examples
//!
//! ```
//! use gryf::{algo::TopoSort, Graph};
//!
//! let mut dependency_tree = Graph::<_, (), _>::new_directed();
//!
//! let cargo = dependency_tree.add_vertex("cargo");
//! let cargo_credential = dependency_tree.add_vertex("cargo_credential");
//! let serde = dependency_tree.add_vertex("serde");
//! let serde_json = dependency_tree.add_vertex("serde_json");
//! let time = dependency_tree.add_vertex("time");
//! let cargo_util = dependency_tree.add_vertex("cargo_util");
//! let libc = dependency_tree.add_vertex("libc");
//!
//! // Edge direction in "must be compiled before" relation.
//! dependency_tree.extend_with_edges([
//!     (cargo_credential, cargo),
//!     (serde, cargo_credential),
//!     (serde_json, cargo_credential),
//!     (serde, serde_json),
//!     (time, cargo_credential),
//!     (libc, time),
//!     (serde, time),
//!     (cargo_util, cargo),
//!     (libc, cargo_util),
//! ]);
//!
//! for package in TopoSort::on(&dependency_tree).run().map(Result::unwrap) {
//!     // Compile package
//! }
//! ```

use std::fmt;

use thiserror::Error;

use crate::{
    core::{GraphBase, Neighbors, VertexSet, id::IntegerIdType, marker::Directed},
    visit,
};

use super::Cycle;

mod builder;
mod dfs;
mod kahn;

pub use builder::TopoSortBuilder;
pub use dfs::DfsVisit;
use kahn::KahnIter;

/// Topologically sorted collection of vertices on a directed acyclic graph
/// (DAG).
///
/// See [module](self) documentation for more details and example.
///
/// This type implements the [`Iterator`] trait and, depending on the
/// [algorithm](Algo), it could be **lazy**. If you want the resulting [`Vec`]
/// of the vertices, use [`into_vec`](TopoSort::into_vec).
pub struct TopoSort<'a, G>
where
    G: VertexSet,
{
    inner: TopoSortInner<'a, G>,
}

impl<'a, G> Iterator for TopoSort<'a, G>
where
    G: GraphBase<EdgeType = Directed> + Neighbors + VertexSet,
    G::VertexId: IntegerIdType,
{
    type Item = Result<G::VertexId, Error<G>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

impl<'a, G> TopoSort<'a, G>
where
    G: GraphBase<EdgeType = Directed> + Neighbors + VertexSet,
    G::VertexId: IntegerIdType,
{
    /// Returns the topologically sorted collection of vertices as [`Vec`],
    /// `Err` if a cycle is detected.
    pub fn into_vec(self) -> Result<Vec<G::VertexId>, Error<G>> {
        self.collect()
    }
}

/// Algorithm for [`TopoSort`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum Algo {
    /// [Kahn's
    /// algorithm](https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm).
    ///
    /// Kahn's algorithm is a method that iteratively removes vertices with no
    /// incoming edges (indegree of zero) and adds them to the sorted
    /// collection. This algorithm is generally the most efficient from
    /// available options.
    ///
    /// The implementation is **lazy** and produces an [iterator](Iterator).
    ///
    /// # Use cases
    ///
    /// * Scheduling tasks that have dependencies.
    /// * Determining the order of compilation in build systems.
    Kahn,

    /// A variation on the [depth-first
    /// search](https://en.wikipedia.org/wiki/Depth-first_search) traversal.
    ///
    /// This implementation visits all vertices in reverse direction of all
    /// edges and reports vertices when they are being
    /// [closed](crate::visit::DfsEvent::Close).
    ///
    /// The implementation is **lazy** and produces an [iterator](Iterator).
    /// Moreover, it can also produce a [visitor](crate::visit::Visitor) with
    /// all its advantages. To utilize this possibility, make sure to [choose
    /// the DFS algorithm](TopoSortBuilder::dfs) specifically.
    ///
    /// # Use cases
    ///
    /// * Same as Kahn's algorithm.
    Dfs,
}

mod algo {
    use super::Algo;

    #[derive(Debug)]
    pub struct AnyAlgo;

    #[derive(Debug)]
    pub struct SpecificAlgo(pub Option<Algo>);

    #[derive(Debug)]
    pub struct Dfs;

    #[derive(Debug)]
    pub struct Kahn;
}

/// The error encountered during a [`TopoSort`] run.
#[derive(Error)]
pub enum Error<G>
where
    G: GraphBase,
{
    /// The graph contains a cycle.
    ///
    /// Graphs with cycles don't have a topological order.
    #[error("graph contains cycle")]
    Cycle(Cycle<G>),
}

impl<G> fmt::Debug for Error<G>
where
    G: GraphBase,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Cycle(cycle) => f.debug_tuple("Cycle").field(&cycle.edge).finish(),
        }
    }
}

impl<G> Clone for Error<G>
where
    G: GraphBase,
{
    fn clone(&self) -> Self {
        match self {
            Self::Cycle(cycle) => Self::Cycle(cycle.clone()),
        }
    }
}

impl<G> PartialEq for Error<G>
where
    G: GraphBase,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Cycle(lhs), Self::Cycle(rhs)) => lhs == rhs,
        }
    }
}

impl<G> Eq for Error<G> where G: GraphBase {}

enum TopoSortInner<'a, G>
where
    G: VertexSet,
{
    Dfs(visit::IntoIter<'a, DfsVisit<'a, G>, G>),
    Kahn(KahnIter<'a, G>),
}

impl<'a, G> Iterator for TopoSortInner<'a, G>
where
    G: GraphBase<EdgeType = Directed> + Neighbors + VertexSet,
    G::VertexId: IntegerIdType,
{
    type Item = Result<G::VertexId, Error<G>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            TopoSortInner::Dfs(dfs) => dfs.next(),
            TopoSortInner::Kahn(kahn) => kahn.next(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use assert_matches::assert_matches;
    use proptest::prelude::*;

    use super::*;

    use crate::{
        core::{EdgeSet, GraphAdd, id::DefaultId},
        infra::proptest::graph_directed,
        storage::AdjList,
        visit::{DfsEvent, DfsEvents, Visitor},
    };

    fn assert_valid<'a, G>(toposort: TopoSort<'a, G>, graph: &'a G)
    where
        G: GraphBase<EdgeType = Directed> + Neighbors + VertexSet + EdgeSet,
        G::VertexId: IntegerIdType,
    {
        let sorted = toposort.collect::<Result<Vec<_>, _>>();

        if let Ok(ref sorted) = sorted {
            assert_eq!(
                sorted.len(),
                graph.vertex_count(),
                "sorted sequence length is not equal to vertex count"
            );
        }

        let cycle =
            DfsEvents::new(graph)
                .start_all(graph)
                .iter(graph)
                .find_map(|event| match event {
                    DfsEvent::BackEdge { .. } => Some(()),
                    _ => None,
                });

        match (sorted, cycle) {
            (Ok(sorted), None) => {
                let map = sorted
                    .iter()
                    .copied()
                    .enumerate()
                    .map(|(k, v)| (v, k))
                    .collect::<HashMap<_, _>>();

                for (from, to) in graph.edges_by_id().map(|e| graph.endpoints(&e).unwrap()) {
                    let i = map.get(&from).unwrap();
                    let j = map.get(&to).unwrap();

                    assert!(i < j, "invalid topological order for {from:?} -> {to:?}");
                }
            }
            (Ok(_), Some(_)) => panic!("algorithm did not detect cycle"),
            (Err(error), None) => panic!("algorithm incorrectly returned error: {error:?}"),
            (Err(_), Some(_)) => {}
        }
    }

    fn create_basic_graph() -> AdjList<(), (), Directed, DefaultId> {
        let mut graph = AdjList::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v5, &v2, ());
        graph.add_edge(&v5, &v0, ());
        graph.add_edge(&v4, &v0, ());
        graph.add_edge(&v4, &v1, ());
        graph.add_edge(&v2, &v3, ());
        graph.add_edge(&v3, &v1, ());

        graph
    }

    fn create_cyclic_graph() -> AdjList<(), (), Directed, DefaultId> {
        let mut graph = AdjList::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v5, &v2, ());
        graph.add_edge(&v5, &v0, ());
        graph.add_edge(&v4, &v0, ());
        graph.add_edge(&v4, &v1, ());
        graph.add_edge(&v2, &v3, ());
        graph.add_edge(&v3, &v1, ());

        graph.add_edge(&v1, &v5, ());

        graph
    }

    fn create_disconnected_graph() -> AdjList<(), (), Directed, DefaultId> {
        let mut graph = AdjList::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v5, &v2, ());
        graph.add_edge(&v5, &v0, ());
        graph.add_edge(&v4, &v0, ());
        graph.add_edge(&v4, &v1, ());
        graph.add_edge(&v2, &v3, ());
        graph.add_edge(&v3, &v1, ());

        let v6 = graph.add_vertex(());
        let v7 = graph.add_vertex(());
        let v8 = graph.add_vertex(());
        let v9 = graph.add_vertex(());

        graph.add_edge(&v7, &v6, ());
        graph.add_edge(&v7, &v8, ());
        graph.add_edge(&v6, &v9, ());
        graph.add_edge(&v8, &v9, ());

        graph
    }

    #[test]
    fn dfs_basic() {
        let graph = create_basic_graph();
        let toposort = TopoSort::on(&graph).using(Algo::Dfs).run();

        assert_valid(toposort, &graph);
    }

    #[test]
    fn dfs_cycle() {
        let graph = create_cyclic_graph();
        let toposort = TopoSort::on(&graph).using(Algo::Dfs).run();

        assert_valid(toposort, &graph);
    }

    #[test]
    fn dfs_disconnected() {
        let graph = create_disconnected_graph();
        let toposort = TopoSort::on(&graph).using(Algo::Dfs).run();

        assert_valid(toposort, &graph);
    }

    #[test]
    fn dfs_none_after_cycle() {
        let graph = create_cyclic_graph();
        let mut toposort = TopoSort::on(&graph).using(Algo::Dfs).run();

        for result in toposort.by_ref() {
            match result {
                Ok(_) => {}
                Err(error) => {
                    assert_matches!(error, Error::Cycle(_));
                    break;
                }
            }
        }

        assert_eq!(toposort.next(), None);
    }

    #[test]
    fn kahn_basic() {
        let graph = create_basic_graph();
        let toposort = TopoSort::on(&graph).using(Algo::Kahn).run();

        assert_valid(toposort, &graph);
    }

    #[test]
    fn kahn_cycle() {
        let graph = create_cyclic_graph();
        let toposort = TopoSort::on(&graph).using(Algo::Kahn).run();

        assert_valid(toposort, &graph);
    }

    #[test]
    fn kahn_disconnected() {
        let graph = create_disconnected_graph();
        let toposort = TopoSort::on(&graph).using(Algo::Kahn).run();

        assert_valid(toposort, &graph);
    }

    #[test]
    fn kahn_none_after_cycle() {
        let graph = create_cyclic_graph();
        let mut toposort = TopoSort::on(&graph).using(Algo::Kahn).run();

        for result in toposort.by_ref() {
            match result {
                Ok(_) => {}
                Err(error) => {
                    assert_matches!(error, Error::Cycle(_));
                    break;
                }
            }
        }

        assert_eq!(toposort.next(), None);
    }

    proptest! {
        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_toposort_dfs_acyclic(graph in graph_directed(any::<()>(), any::<()>()).acyclic()) {
            let toposort = TopoSort::on(&graph).using(Algo::Dfs).run();
            assert_valid(toposort, &graph);
        }

        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_toposort_dfs_any(graph in graph_directed(any::<()>(), any::<()>())) {
            let toposort = TopoSort::on(&graph).using(Algo::Dfs).run();
            assert_valid(toposort, &graph);
        }

        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_toposort_kahn_acyclic(graph in graph_directed(any::<()>(), any::<()>()).acyclic()) {
            let toposort = TopoSort::on(&graph).using(Algo::Kahn).run();
            assert_valid(toposort, &graph);
        }

        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_toposort_kahn_any(graph in graph_directed(any::<()>(), any::<()>())) {
            let toposort = TopoSort::on(&graph).using(Algo::Kahn).run();
            assert_valid(toposort, &graph);
        }
    }
}
