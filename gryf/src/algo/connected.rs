use crate::core::{GraphBase, Neighbors, VertexSet};

mod builder;
mod dfs;

pub use builder::ConnectedBuilder;

pub struct Connected<G>
where
    G: GraphBase,
{
    disconnected_any: Option<(G::VertexId, G::VertexId)>,
    as_undirected: bool,
}

impl<G> Connected<G>
where
    G: GraphBase,
{
    pub fn is(&self) -> bool {
        self.disconnected_any.is_none()
    }

    pub fn disconnected_any(&self) -> Option<(&G::VertexId, &G::VertexId)> {
        self.disconnected_any.as_ref().map(|(ref u, ref v)| (u, v))
    }

    pub fn as_undirected(&self) -> bool {
        self.as_undirected
    }
}

pub fn is_connected<G>(graph: &G) -> bool
where
    G: Neighbors + VertexSet,
{
    Connected::on(graph).run().is()
}

pub fn is_path_between<G>(graph: &G, src: &G::VertexId, dst: &G::VertexId) -> bool
where
    G: Neighbors + VertexSet,
{
    Connected::on(graph).between(src, dst).run().is()
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use proptest::prelude::*;

    use crate::{
        common::VisitSet,
        core::{
            base::NeighborRef,
            id::VertexId,
            marker::{Directed, Direction, Undirected},
            GraphAdd,
        },
        infra::proptest::{graph_directed, graph_undirected},
        storage::AdjList,
    };

    use super::*;

    fn assert_valid<G>(
        connected: Connected<G>,
        graph: &G,
        between: Option<(G::VertexId, G::VertexId)>,
    ) where
        G: Neighbors + VertexSet,
    {
        match connected.disconnected_any() {
            Some((u, v)) => {
                // Check that the vertices are indeed disconnected. Also check
                // if the given vertices are disconnected.
                for (u, v) in std::iter::once((u.clone(), v.clone())).chain(between.clone()) {
                    let mut visited = HashSet::with_capacity(graph.vertex_count());
                    let mut stack = vec![u.clone()];

                    while let Some(w) = stack.pop() {
                        assert!(
                            w != v,
                            "algorithm reported connected vertices as disconnected ({:?}, {:?})",
                            u,
                            v
                        );

                        if visited.visit(w.clone()) {
                            for n in graph.neighbors_directed(&w, Direction::Outgoing) {
                                stack.push(n.id().into_owned());
                            }
                        }
                    }
                }
            }
            None => {
                // Check that the graph is indeed connected. We use undirected
                // algorithm which is simpler and therefore correct. This is
                // fine because connected graph in directed sense is also
                // connected in undirected sense. We are missing case in which
                // the algorithm determined that a directed graph is connected,
                // while it is not in directed sense, but is in undirected
                // sense.
                let mut visited = HashSet::with_capacity(graph.vertex_count());
                let start = match between {
                    Some((ref start, _)) => start.clone(),
                    None => match graph.vertex_ids().next() {
                        Some(start) => start,
                        None => return,
                    },
                };
                let mut stack = vec![start];

                while let Some(u) = stack.pop() {
                    if visited.visit(u.clone()) {
                        for n in graph.neighbors(&u) {
                            stack.push(n.id().into_owned());
                        }
                    }
                }

                if let Some((_, dst)) = between {
                    assert!(
                        visited.is_visited(&dst),
                        "algorithm reported connected vertices while they are not"
                    )
                } else {
                    assert_eq!(
                        visited.visited_count(),
                        graph.vertex_count(),
                        "algorithm reported connected graph while it is not"
                    )
                }
            }
        }
    }

    #[test]
    fn connected_basic_undirected() {
        let mut graph = AdjList::<_, _, Undirected, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());

        let connected = Connected::on(&graph).run();
        assert!(connected.is());
    }

    #[test]
    fn connected_basic_directed() {
        let mut graph = AdjList::<_, _, Directed, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());

        let connected = Connected::on(&graph).run();
        assert!(connected.is());
    }

    #[test]
    fn connected_directed_reversed() {
        let mut graph = AdjList::<_, _, Directed, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v1, &v0, ());
        graph.add_edge(&v2, &v1, ());

        let connected = Connected::on(&graph).run();
        assert!(connected.is());
    }

    #[test]
    fn disconnected_basic_undirected() {
        let mut graph = AdjList::<_, _, Undirected, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());

        let connected = Connected::on(&graph).run();
        assert!(!connected.is());
        assert_eq!(
            connected
                .disconnected_any()
                .map(|(u, v)| [u, v].contains(&&v2)),
            Some(true)
        );
    }

    #[test]
    fn disconnected_basic_directed() {
        let mut graph = AdjList::<_, _, Directed, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());

        let connected = Connected::on(&graph).run();
        assert!(!connected.is());
        assert_eq!(
            connected
                .disconnected_any()
                .map(|(u, v)| [u, v].contains(&&v2)),
            Some(true)
        );
    }

    #[test]
    fn disconnected_weakly_basic_directed() {
        let mut graph = AdjList::<_, _, Directed, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v2, &v1, ());

        let connected = Connected::on(&graph).run();
        assert!(!connected.is());
        assert_eq!(connected.disconnected_any(), Some((&v0, &v2)));
    }

    #[test]
    fn disconnected_directed_circles() {
        let mut graph = AdjList::<_, _, Directed, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v2, &v0, ());

        graph.add_edge(&v3, &v4, ());
        graph.add_edge(&v4, &v5, ());
        graph.add_edge(&v5, &v3, ());

        let connected = Connected::on(&graph).run();
        assert!(!connected.is());
    }

    #[test]
    fn connected_directed_as_undirected() {
        let mut graph = AdjList::<_, _, Directed, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v2, &v1, ());

        let connected = Connected::on(&graph).as_undirected().run();
        assert!(connected.is());
        assert!(connected.as_undirected());
    }

    #[test]
    fn connected_between_disconnected_otherwise_undirected() {
        let mut graph = AdjList::<_, _, Undirected, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());

        let connected = Connected::on(&graph).between(&v0, &v1).run();
        assert!(connected.is());
    }

    #[test]
    fn disconnected_between_undirected() {
        let mut graph = AdjList::<_, _, Undirected, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());

        let connected = Connected::on(&graph).between(&v0, &v2).run();
        assert!(!connected.is());
        assert_eq!(connected.disconnected_any(), Some((&v0, &v2)));
    }

    #[test]
    fn connected_empty() {
        let graph = AdjList::<(), (), Undirected, _>::default();

        let connected = Connected::on(&graph).run();
        assert!(connected.is());
    }

    #[test]
    fn connected_single() {
        let mut graph = AdjList::<_, (), Undirected, _>::default();

        graph.add_vertex(());

        let connected = Connected::on(&graph).run();
        assert!(connected.is());
    }

    #[test]
    fn connected_with_itself() {
        let mut graph = AdjList::<_, (), Undirected, _>::default();

        graph.add_vertex(());
        let v1 = graph.add_vertex(());
        graph.add_vertex(());

        let connected = Connected::on(&graph).between(&v1, &v1).run();
        assert!(connected.is());
    }

    proptest! {
        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_connected_undirected_any(graph in graph_undirected(any::<()>(), any::<()>())) {
            let connected = Connected::on(&graph).run();
            assert_valid(connected, &graph, None);
        }

        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_connected_directed_any(graph in graph_directed(any::<()>(), any::<()>())) {
            let connected = Connected::on(&graph).run();
            assert_valid(connected, &graph, None);
        }

        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_connected_between_undirected_any(graph in graph_undirected(any::<()>(), any::<()>()), src: u64, dst: u64) {
            let n = graph.vertex_count() as u64;
            prop_assume!(n > 0);

            let src = VertexId(src % n);
            let dst = VertexId(dst % n);
            let connected = Connected::on(&graph).between(&src, &dst).run();
            assert_valid(connected, &graph, Some((src, dst)));
        }

        #[test]
        #[ignore = "run property-based tests with `cargo test proptest_ -- --ignored`"]
        fn proptest_connected_between_directed_any(graph in graph_directed(any::<()>(), any::<()>()), src: u64, dst: u64) {
            let n = graph.vertex_count() as u64;
            prop_assume!(n > 0);

            let src = VertexId(src % n);
            let dst = VertexId(dst % n);
            let connected = Connected::on(&graph).between(&src, &dst).run();
            assert_valid(connected, &graph, Some((src, dst)));
        }
    }
}
