//! Find [single source shortest paths] and their distances in a graph.
//!
//! See available parameters [here](ShortestPathsBuilder#implementations).
//!
//! Note that more efficient algorithms can be applied if the edges do not have
//! negative weights. If you have a graph where nonnegative weights can be
//! guaranteed at compile time, make sure to use an [unsigned
//! type](crate::core::weight::Weight::is_unsigned) like `u8`, `u32` or
//! [`uf64`](crate::core::weight::uf64).
//!
//! [single source shortest paths]:
//!     https://en.wikipedia.org/wiki/Shortest_path_problem#Single-source_shortest_paths
//!
//! # Examples
//!
//! ```
//! use gryf::{algo::ShortestPaths, Graph};
//!
//! let mut graph = Graph::new_undirected();
//!
//! let prague = graph.add_vertex("Prague");
//! let bratislava = graph.add_vertex("Bratislava");
//! let vienna = graph.add_vertex("Vienna");
//! let munich = graph.add_vertex("Munich");
//! let nuremberg = graph.add_vertex("Nuremberg");
//! let florence = graph.add_vertex("Florence");
//! let rome = graph.add_vertex("Rome");
//!
//! graph.extend_with_edges([
//!     (prague, bratislava, 328u32),
//!     (prague, nuremberg, 297),
//!     (prague, vienna, 293),
//!     (bratislava, vienna, 79),
//!     (nuremberg, munich, 170),
//!     (vienna, munich, 402),
//!     (vienna, florence, 863),
//!     (munich, florence, 646),
//!     (florence, rome, 278),
//! ]);
//!
//! let shortest_paths = ShortestPaths::on(&graph).goal(prague).run(rome).unwrap();
//! let distance = shortest_paths[prague];
//! let path = shortest_paths
//!     .reconstruct(prague)
//!     .map(|v| graph[v])
//!     .collect::<Vec<_>>()
//!     .join(" - ");
//!
//! println!("{distance} km from Prague through {path}");
//! ```

use std::{borrow::Borrow, ops::Index};

use rustc_hash::FxHashMap;
use thiserror::Error;

use crate::core::GraphBase;

mod bellman_ford;
mod bfs;
mod builder;
mod dijkstra;

pub use builder::ShortestPathsBuilder;

/// Shortest paths and their distances from a single source vertex.
///
/// See [module](self) documentation for more details and example.
#[derive(Debug)]
pub struct ShortestPaths<W, G: GraphBase> {
    source: G::VertexId,
    // Using HashMaps because the algorithm supports early termination when
    // reaching given goal. It is likely that reaching goal means visiting a
    // subgraph which is significantly smaller than the original graph.
    dist: FxHashMap<G::VertexId, W>,
    pred: FxHashMap<G::VertexId, G::VertexId>,
}

impl<W, G> ShortestPaths<W, G>
where
    G: GraphBase,
{
    /// Source vertex where the search was started.
    pub fn source(&self) -> &G::VertexId {
        &self.source
    }

    /// Returns the path distance between the source vertex and the given
    /// vertex, or `None` if it's not known.
    ///
    /// There are two causes why the distance between two vertices is not known:
    /// (1) the vertices are not connected, or (2) the
    /// [goal](ShortestPathsBuilder::goal) was reached before visiting the given
    /// vertex.
    pub fn dist<VI>(&self, to: VI) -> Option<&W>
    where
        VI: Borrow<G::VertexId>,
    {
        self.dist.get(to.borrow())
    }

    /// Returns an iterator over vertices on the path between the given vertex
    /// and the source vertex, in this order, or `None` if it wasn't found.
    ///
    /// There are two causes why the distance between two vertices is not known:
    /// (1) the vertices are not connected, or (2) the
    /// [goal](ShortestPathsBuilder::goal) was reached before visiting the given
    /// vertex.
    pub fn reconstruct(&self, to: G::VertexId) -> PathReconstruction<'_, G> {
        PathReconstruction {
            curr: to,
            pred: &self.pred,
        }
    }
}

impl<W, G, VI> Index<VI> for ShortestPaths<W, G>
where
    G: GraphBase,
    VI: Borrow<G::VertexId>,
{
    type Output = W;

    fn index(&self, index: VI) -> &Self::Output {
        self.dist(index).unwrap()
    }
}

/// Algorithm for [`ShortestPaths`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum Algo {
    /// [Dijkstra's
    /// algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm)
    ///
    /// Dijkstra's algorithm is a popular method on a graph with non-negative
    /// edge weights. It operates by iteratively selecting the vertex with the
    /// smallest known distance from the source and updating the distances of
    /// its neighbors.
    ///
    /// # Use cases
    ///
    /// * Finding the shortest path in road networks.
    /// * Optimizing routing in communication networks.
    /// * Navigation and GPS systems.
    Dijkstra,

    /// [Bellman–Ford
    /// algorithm](https://en.wikipedia.org/wiki/Bellman%E2%80%93Ford_algorithm).
    ///
    /// The Bellman-Ford algorithm can handle graphs with negative edge weights
    /// and can detect negative weight cycles in a graph. However, it is
    /// generally slower than Dijkstra's algorithm.
    ///
    /// # Use cases
    ///
    /// * Finding shortest paths in graphs that may contain negative weight
    ///   edges.
    /// * Detecting negative weight cycles in financial models.
    /// * Network routing protocols like RIP (Routing Information Protocol).
    BellmanFord,
}

mod algo {
    use super::Algo;

    #[derive(Debug)]
    pub struct AnyAlgo;

    #[derive(Debug)]
    pub struct SpecificAlgo(pub Option<Algo>);

    #[derive(Debug)]
    pub struct Dijkstra;

    #[derive(Debug)]
    pub struct BellmanFord;

    #[derive(Debug)]
    pub struct Bfs;
}

/// The error encountered during a [`ShortestPaths`] run.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Error)]
pub enum Error {
    /// An edge with negative weight encountered.
    #[error("edge with negative weight encountered")]
    NegativeWeight,

    /// A negative cycle encountered.
    #[error("negative cycle encountered")]
    NegativeCycle,

    /// The specified goal not reached.
    #[error("specified goal not reached")]
    GoalNotReached,

    /// An edge not available.
    ///
    /// This error should not happen in normal circumstances. If it does, it
    /// indicates a bad implementation of the graph.
    #[error("edge not available")]
    EdgeNotAvailable,
}

/// Iterator over the vertices on the path from a vertex to the source vertex.
///
/// Returned by [`ShortestPaths::reconstruct`].
pub struct PathReconstruction<'a, G: GraphBase> {
    curr: G::VertexId,
    pred: &'a FxHashMap<G::VertexId, G::VertexId>,
}

impl<'a, G: GraphBase> Iterator for PathReconstruction<'a, G> {
    type Item = G::VertexId;

    fn next(&mut self) -> Option<Self::Item> {
        self.curr = self.pred.get(&self.curr).cloned()?;
        Some(self.curr.clone())
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use proptest::prelude::*;

    use crate::{
        core::{
            GraphAdd, GraphMut, VertexSet,
            id::{DefaultId, EdgeId, IdType, VertexId},
            marker::{Directed, Undirected},
        },
        infra::proptest::{graph_directed, graph_undirected},
        storage::AdjList,
    };

    use super::*;

    fn create_basic_graph() -> AdjList<(), i32, Undirected, DefaultId> {
        let mut graph = AdjList::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, 3);
        graph.add_edge(&v0, &v2, 2);
        graph.add_edge(&v1, &v2, 2);
        graph.add_edge(&v1, &v3, 2);
        graph.add_edge(&v1, &v4, 7);
        graph.add_edge(&v2, &v3, 5);
        graph.add_edge(&v3, &v4, 3);
        graph.add_edge(&v4, &v5, 10);

        graph
    }

    fn create_graph_with_isolated_vertex() -> (AdjList<(), i32, Undirected, DefaultId>, VertexId) {
        let mut graph = AdjList::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, 3);
        graph.add_edge(&v0, &v2, 2);
        graph.add_edge(&v1, &v2, 2);

        (graph, v3)
    }

    fn v(index: usize) -> VertexId {
        index.into()
    }

    fn e(index: usize) -> EdgeId {
        index.into()
    }

    #[test]
    fn dijkstra_basic() {
        let graph = create_basic_graph();
        let shortest_paths = ShortestPaths::on(&graph)
            .using(Algo::Dijkstra)
            .run(v(0))
            .unwrap();

        assert_eq!(shortest_paths.dist(v(4)), Some(&8));
        assert_eq!(
            shortest_paths.reconstruct(v(4)).collect::<Vec<_>>(),
            vec![v(3), v(1), v(0)]
        );

        assert_eq!(shortest_paths.dist(v(2)), Some(&2));
    }

    #[test]
    fn dijkstra_early_termination() {
        let graph = create_basic_graph();
        let shortest_paths = ShortestPaths::on(&graph)
            .goal(v(4))
            .using(Algo::Dijkstra)
            .run(v(0))
            .unwrap();

        assert!(shortest_paths.dist(v(5)).is_none());
    }

    #[test]
    fn dijkstra_negative_edge() {
        let mut graph = create_basic_graph();
        graph.replace_edge(&e(2), -1);

        let shortest_paths = ShortestPaths::on(&graph)
            .goal(v(4))
            .using(Algo::Dijkstra)
            .run(v(0));

        assert_matches!(shortest_paths, Err(Error::NegativeWeight));
    }

    #[test]
    fn dijkstra_goal_not_reached() {
        let (graph, u) = create_graph_with_isolated_vertex();

        let shortest_paths = ShortestPaths::on(&graph)
            .goal(u)
            .using(Algo::Dijkstra)
            .run(v(0));

        assert_matches!(shortest_paths, Err(Error::GoalNotReached));
    }

    #[test]
    fn bellman_ford_basic() {
        let graph = create_basic_graph();
        let shortest_paths = ShortestPaths::on(&graph)
            .using(Algo::BellmanFord)
            .run(v(0))
            .unwrap();

        assert_eq!(shortest_paths.dist(v(4)), Some(&8));
        assert_eq!(
            shortest_paths.reconstruct(v(4)).collect::<Vec<_>>(),
            vec![v(3), v(1), v(0)]
        );

        assert_eq!(shortest_paths.dist(v(2)), Some(&2));
    }

    #[test]
    fn bellman_ford_negative_edge() {
        let mut graph = AdjList::<_, _, Directed, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, 3);
        graph.add_edge(&v0, &v2, 2);
        graph.add_edge(&v1, &v2, -1);
        graph.add_edge(&v1, &v3, 2);
        graph.add_edge(&v1, &v4, 7);
        graph.add_edge(&v2, &v3, 5);
        graph.add_edge(&v3, &v4, 3);
        graph.add_edge(&v4, &v5, 10);

        let shortest_paths = ShortestPaths::on(&graph)
            .using(Algo::BellmanFord)
            .run(v(0))
            .unwrap();

        assert_eq!(shortest_paths.dist(v(4)), Some(&8));
        assert_eq!(
            shortest_paths.reconstruct(v(4)).collect::<Vec<_>>(),
            vec![v(3), v(1), v(0)]
        );

        assert_eq!(shortest_paths.dist(v(2)), Some(&2));
    }

    #[test]
    fn bellman_ford_negative_cycle() {
        let mut graph = AdjList::<(), i32, Directed, DefaultId>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, 3);
        graph.add_edge(&v1, &v2, -2);
        graph.add_edge(&v2, &v3, 2);
        graph.add_edge(&v2, &v1, -2);
        graph.add_edge(&v2, &v4, 3);

        let shortest_paths = ShortestPaths::on(&graph).using(Algo::BellmanFord).run(v(0));

        assert_matches!(shortest_paths, Err(Error::NegativeCycle));
    }

    #[test]
    fn bellman_ford_goal_not_reached() {
        let (graph, u) = create_graph_with_isolated_vertex();

        let shortest_paths = ShortestPaths::on(&graph)
            .goal(u)
            .using(Algo::BellmanFord)
            .run(v(0));

        assert_matches!(shortest_paths, Err(Error::GoalNotReached));
    }

    #[test]
    fn bellman_ford_undirected_support() {
        let mut graph = AdjList::<_, _, Undirected, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, 1);

        let shortest_paths = ShortestPaths::on(&graph)
            .using(Algo::BellmanFord)
            .run(v1)
            .unwrap();

        // Assuming that the AdjList reports the edge with endpoints (v0, v1),
        // Bellman-Ford algorithm needs a special support for undirected graphs
        // to report the distance below correctly.
        assert_eq!(shortest_paths.dist(v0), Some(&1));
    }

    #[test]
    fn bfs_basic() {
        let graph = create_basic_graph();
        let shortest_paths = ShortestPaths::on(&graph)
            .unit_weight()
            .bfs()
            .run(v(0))
            .unwrap();

        assert_eq!(shortest_paths.dist(v(4)), Some(&2));
        assert_eq!(
            shortest_paths.reconstruct(v(4)).collect::<Vec<_>>(),
            vec![v(1), v(0)]
        );

        assert_eq!(shortest_paths.dist(v(2)), Some(&1));
    }

    #[test]
    fn bfs_early_termination() {
        let graph = create_basic_graph();
        let shortest_paths = ShortestPaths::on(&graph)
            .goal(v(4))
            .unit_weight()
            .bfs()
            .run(v(0))
            .unwrap();

        assert!(shortest_paths.dist(v(5)).is_none());
    }

    #[test]
    fn bfs_goal_not_reached() {
        let (graph, u) = create_graph_with_isolated_vertex();

        let shortest_paths = ShortestPaths::on(&graph)
            .goal(u)
            .unit_weight()
            .bfs()
            .run(v(0));

        assert_matches!(shortest_paths, Err(Error::GoalNotReached));
    }

    #[test]
    fn prefer_dijkstra_for_undirected() {
        let mut graph = AdjList::<_, _, Undirected, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, -1);

        // Setting the goal vertex to be the same as the starting vertex makes
        // Dijkstra's algorithm finish immediately with success where the
        // Bellman-Ford would report "negative cycle" error because it doesn't
        // consider the goal vertex.
        let shortest_paths = ShortestPaths::on(&graph).goal(v1).run(v1).unwrap();

        assert_eq!(shortest_paths.dist(v1), Some(&0));
    }

    proptest! {
        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_dijkstra_connected_all_reachable(graph in graph_undirected(any::<()>(), any::<u16>().prop_map(|e| e as u32)).connected(), source: u64) {
            let n = graph.vertex_count() as u64;
            prop_assume!(n > 0);

            let source = VertexId::from_bits(source % n);
            let paths = ShortestPaths::on(&graph).using(Algo::Dijkstra).run(source).unwrap();

            for v in graph.vertices_by_id() {
                prop_assert_ne!(paths.dist(v), None);

                let u = paths.reconstruct(v).last();
                if v != source {
                    prop_assert_eq!(u, Some(source));
                } else {
                    prop_assert_eq!(u, None);
                }
            }
        }

        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_bellman_ford_any_directed_negative_weight_no_panic(graph in graph_directed(any::<()>(), any::<i16>().prop_map(|e| e as i32)).max_size(128), source: u64) {
            let n = graph.vertex_count() as u64;
            prop_assume!(n > 0);

            let source = VertexId::from_bits(source % n);
            let _ = ShortestPaths::on(&graph).using(Algo::BellmanFord).run(source);
        }

        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_bellman_ford_all_reachable(graph in graph_undirected(any::<()>(), any::<u16>().prop_map(|e| e as u32)).max_size(128).connected(), source: u64) {
            let n = graph.vertex_count() as u64;
            prop_assume!(n > 0);

            let source = VertexId::from_bits(source % n);
            let paths = ShortestPaths::on(&graph).using(Algo::BellmanFord).run(source).unwrap();

            for v in graph.vertices_by_id() {
                prop_assert_ne!(paths.dist(v), None);

                let u = paths.reconstruct(v).last();
                if v != source {
                    prop_assert_eq!(u, Some(source));
                } else {
                    prop_assert_eq!(u, None);
                }
            }
        }

        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_dijkstra_bellman_ford_agree_any_directed(graph in graph_directed(any::<()>(), any::<u16>().prop_map(|e| e as u32)).max_size(128), source: u64) {
            let n = graph.vertex_count() as u64;
            prop_assume!(n > 0);

            let source = VertexId::from_bits(source % n);
            let paths_d = ShortestPaths::on(&graph).using(Algo::Dijkstra).run(source).unwrap();
            let paths_bf = ShortestPaths::on(&graph).using(Algo::BellmanFord).run(source).unwrap();

            for v in graph.vertices_by_id() {
                prop_assert_eq!(paths_d.dist(v), paths_bf.dist(v));
                // Check only the distances. Paths as found by the two
                // algorithms can be different in general.
            }
        }

        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_dijkstra_bellman_ford_agree_any_undirected(graph in graph_undirected(any::<()>(), any::<u16>().prop_map(|e| e as u32)).max_size(128), source: u64) {
            let n = graph.vertex_count() as u64;
            prop_assume!(n > 0);

            let source = VertexId::from_bits(source % n);
            let paths_d = ShortestPaths::on(&graph).using(Algo::Dijkstra).run(source).unwrap();
            let paths_bf = ShortestPaths::on(&graph).using(Algo::BellmanFord).run(source).unwrap();

            for v in graph.vertices_by_id() {
                prop_assert_eq!(paths_d.dist(v), paths_bf.dist(v));
                // Check only the distances. Paths as found by the two
                // algorithms can be different in general.
            }
        }

        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_dijkstra_bfs_agree_any_directed(graph in graph_directed(any::<()>(), any::<()>()).max_size(128), source: u64) {
            let n = graph.vertex_count() as u64;
            prop_assume!(n > 0);

            let source = VertexId::from_bits(source % n);
            let paths_d = ShortestPaths::on(&graph).unit_weight().dijkstra().run(source).unwrap();
            let paths_bfs = ShortestPaths::on(&graph).unit_weight().bfs().run(source).unwrap();

            for v in graph.vertices_by_id() {
                prop_assert_eq!(paths_d.dist(v), paths_bfs.dist(v));
                // Check only the distances. Paths as found by the two
                // algorithms can be different in general.
            }
        }

        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_dijkstra_bfs_agree_any_undirected(graph in graph_undirected(any::<()>(), any::<()>()).max_size(128), source: u64) {
            let n = graph.vertex_count() as u64;
            prop_assume!(n > 0);

            let source = VertexId::from_bits(source % n);
            let paths_d = ShortestPaths::on(&graph).unit_weight().dijkstra().run(source).unwrap();
            let paths_bfs = ShortestPaths::on(&graph).unit_weight().bfs().run(source).unwrap();

            for v in graph.vertices_by_id() {
                prop_assert_eq!(paths_d.dist(v), paths_bfs.dist(v));
                // Check only the distances. Paths as found by the two
                // algorithms can be different in general.
            }
        }
    }
}
