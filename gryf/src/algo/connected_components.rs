//! Find (strongly) [connected] components in a graph.
//!
//! See available parameters [here](ConnectedComponentsBuilder#implementations)
//! or [here](StronglyConnectedComponentsBuilder#implementations).
//!
//! # Examples
//!
//! ```
//! use gryf::{Graph, algo::{is_connected, is_strongly_connected}};
//!
//! let mut graph = Graph::new_directed();
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
//! // Weak connectivity is checked on directed graphs by default.
//! assert!(is_connected(&graph));
//! assert!(!is_strongly_connected(&graph));
//!
//! graph.add_edge(d, a, ());
//! assert!(is_strongly_connected(&graph));
//! ```
//!
//! [connected]: https://en.wikipedia.org/wiki/Connectivity_(graph_theory)

use crate::core::GraphBase;

mod builder;
mod dfs;
mod kosaraju;

pub use builder::{ConnectedComponentsBuilder, StronglyConnectedComponentsBuilder};

/// Connected components of a graph.
///
/// See [module](self) documentation for more details and example.
#[derive(Debug)]
pub struct ConnectedComponents<G: GraphBase> {
    components: Vec<Vec<G::VertexId>>,
}

/// Strongly connected components of a graph.
///
/// See [module](self) documentation for more details and example.
#[derive(Debug)]
pub struct StronglyConnectedComponents<G: GraphBase> {
    inner: ConnectedComponents<G>,
}

impl<G> ConnectedComponents<G>
where
    G: GraphBase,
{
    /// Returns the number of components.
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.components.len()
    }

    /// Returns an iterator of the components.
    pub fn iter(&self) -> Iter<'_, G> {
        Iter {
            inner: self.components.iter(),
        }
    }
}

impl<G> StronglyConnectedComponents<G>
where
    G: GraphBase,
{
    /// Returns the number of components.
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Returns an iterator of the components.
    pub fn iter(&self) -> Iter<'_, G> {
        self.inner.iter()
    }
}

pub struct Iter<'a, G: GraphBase> {
    inner: std::slice::Iter<'a, Vec<G::VertexId>>,
}

impl<'a, G> Iterator for Iter<'a, G>
where
    G: GraphBase,
{
    type Item = &'a [G::VertexId];

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|component| component.as_slice())
    }
}

/// Algorithm for [`ConnectedComponents`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum AlgoWeak {
    /// Standard DFS algorithm for traversing the graph.
    ///
    /// # Use cases
    ///
    /// * Finding groups where elements relate to each other.
    Dfs,
}

/// Algorithm for [`StronglyConnectedComponents`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum AlgoStrong {
    /// [Kosaraju's
    /// algorithm](https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm).
    ///
    /// Kosaraju's algorithm is an iterative algorithm with two passes, one DFS
    /// pass on the graph collecting the sequence of closed vertices and one DFS
    /// pass on the transposed graph traversing the graph in the reversed order
    /// of vertices collected in the first pass.
    ///
    /// # Use cases
    ///
    /// * Finding groups where elements transitively depend on each other.
    Kosaraju,
}

mod algo {
    use super::{AlgoStrong, AlgoWeak};

    #[derive(Debug)]
    pub struct AnyAlgo;

    #[derive(Debug)]
    pub struct SpecificAlgoWeak(pub Option<AlgoWeak>);

    #[derive(Debug)]
    pub struct SpecificAlgoStrong(pub Option<AlgoStrong>);

    #[derive(Debug)]
    pub struct Dfs;

    #[derive(Debug)]
    pub struct Kosaraju;
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use proptest::prelude::*;

    use crate::{
        adapt::Subgraph,
        algo::{is_connected, is_strongly_connected},
        core::{
            GraphAdd, Neighbors, VertexSet,
            id::DefaultId,
            marker::{Directed, EdgeType, Undirected},
        },
        infra::proptest::{graph_directed, graph_undirected},
        storage::AdjList,
    };

    use super::*;

    fn assert_valid<G>(connected_components: ConnectedComponents<G>, graph: &G)
    where
        G: GraphBase<EdgeType = Undirected> + Neighbors + VertexSet,
    {
        assert_eq!(
            connected_components
                .iter()
                .flat_map(|component| component.iter().cloned())
                .collect::<BTreeSet<_>>(),
            graph.vertices_by_id().collect::<BTreeSet<_>>(),
            "vertices connected components are not corresponding to graph vertices"
        );

        let n_components = connected_components.len();
        if n_components == 1 {
            assert!(
                is_connected(graph),
                "only one connected component but graph not connected"
            );
        } else if n_components > 1 {
            assert!(
                !is_connected(graph),
                "multiple connected components but graph connected"
            );

            for component in connected_components.iter() {
                assert_ne!(component.len(), 0, "component is empty");

                let subgraph = Subgraph::with_state(graph, component)
                    .filter_vertex(|vertex, _, state| state.contains(vertex));

                assert!(
                    is_connected(&subgraph),
                    "component is actually not connected"
                );
            }
        } else {
            assert_eq!(
                graph.vertex_count(),
                0,
                "zero connected components but graph not empty"
            );
        }
    }

    fn assert_valid_strongly<G>(connected_components: ConnectedComponents<G>, graph: &G)
    where
        G: GraphBase<EdgeType = Directed> + Neighbors + VertexSet,
    {
        assert_eq!(
            connected_components
                .iter()
                .flat_map(|component| component.iter().cloned())
                .collect::<BTreeSet<_>>(),
            graph.vertices_by_id().collect::<BTreeSet<_>>(),
            "vertices connected components are not corresponding to graph vertices"
        );

        let n_components = connected_components.len();
        if n_components == 1 {
            assert!(
                is_strongly_connected(graph),
                "only one connected component but graph not connected"
            );
        } else if n_components > 1 {
            assert!(
                !is_strongly_connected(graph),
                "multiple connected components but graph connected"
            );

            for component in connected_components.iter() {
                assert_ne!(component.len(), 0, "component is empty");

                let subgraph = Subgraph::with_state(graph, component)
                    .filter_vertex(|vertex, _, state| state.contains(vertex));

                assert!(
                    is_strongly_connected(&subgraph),
                    "component is actually not connected"
                );
            }
        } else {
            assert_eq!(
                graph.vertex_count(),
                0,
                "zero connected components but graph not empty"
            );
        }
    }

    fn create_empty_graph<Ty: EdgeType>() -> AdjList<(), (), Ty, DefaultId> {
        AdjList::default()
    }

    fn create_connected_graph() -> AdjList<(), (), Undirected, DefaultId> {
        let mut graph = AdjList::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());

        graph
    }

    fn create_disconnected_graph() -> AdjList<(), (), Undirected, DefaultId> {
        let mut graph = AdjList::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v2, &v3, ());

        graph
    }

    fn create_strongly_connected_graph() -> AdjList<(), (), Directed, DefaultId> {
        let mut graph = AdjList::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v2, &v0, ());

        graph
    }

    fn create_strongly_disconnected_graph() -> AdjList<(), (), Directed, DefaultId> {
        let mut graph = AdjList::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v2, &v0, ());
        graph.add_edge(&v2, &v3, ());
        graph.add_edge(&v3, &v4, ());
        graph.add_edge(&v4, &v3, ());

        graph
    }

    #[test]
    fn undirected_dfs_empty() {
        let graph = create_empty_graph::<Undirected>();
        let connected_components = ConnectedComponents::on(&graph).using(AlgoWeak::Dfs).run();

        assert_valid(connected_components, &graph);
    }

    #[test]
    fn undirected_dfs_connected() {
        let graph = create_connected_graph();
        let connected_components = ConnectedComponents::on(&graph).using(AlgoWeak::Dfs).run();

        assert_valid(connected_components, &graph);
    }

    #[test]
    fn undirected_dfs_disconnected() {
        let graph = create_disconnected_graph();
        let connected_components = ConnectedComponents::on(&graph).using(AlgoWeak::Dfs).run();

        assert_valid(connected_components, &graph);
    }

    #[test]
    fn directed_kosaraju_empty() {
        let graph = create_empty_graph::<Directed>();
        let connected_components = StronglyConnectedComponents::on(&graph)
            .using(AlgoStrong::Kosaraju)
            .run();

        assert_valid_strongly(connected_components.inner, &graph);
    }

    #[test]
    fn directed_kosaraju_connected() {
        let graph = create_strongly_connected_graph();
        let connected_components = StronglyConnectedComponents::on(&graph)
            .using(AlgoStrong::Kosaraju)
            .run();

        dbg!(&connected_components);

        assert_valid_strongly(connected_components.inner, &graph);
    }

    #[test]
    fn directed_kosaraju_disconnected() {
        let graph = create_strongly_disconnected_graph();
        let connected_components = StronglyConnectedComponents::on(&graph)
            .using(AlgoStrong::Kosaraju)
            .run();

        dbg!(&connected_components);

        assert_valid_strongly(connected_components.inner, &graph);
    }

    proptest! {
        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_connected_components_undirected_dfs(graph in graph_undirected(any::<()>(), any::<()>())) {
            let connected_components = ConnectedComponents::on(&graph)
                .using(AlgoWeak::Dfs)
                .run();

            assert_valid(connected_components, &graph);
        }

        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_connected_components_directed_kosaraju(graph in graph_directed(any::<()>(), any::<()>())) {
            let connected_components = StronglyConnectedComponents::on(&graph)
                .using(AlgoStrong::Kosaraju)
                .run();

            assert_valid_strongly(connected_components.inner, &graph);
        }
    }
}
