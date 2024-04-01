use std::fmt;

use thiserror::Error;

use crate::{
    core::{id::NumIdType, marker::Directed, EdgesBase, GraphBase, Neighbors, VerticesBase},
    visit,
};

use super::Cycle;

mod builder;
mod dfs;
mod kahn;

pub use builder::TopoSortBuilder;
pub use dfs::DfsVisit;
use kahn::KahnIter;

pub struct TopoSort<'a, G>
where
    G: VerticesBase + EdgesBase<Directed>,
{
    inner: TopoSortInner<'a, G>,
}

impl<'a, G> Iterator for TopoSort<'a, G>
where
    G: Neighbors + VerticesBase + EdgesBase<Directed>,
    G::VertexId: NumIdType,
{
    type Item = Result<G::VertexId, Error<G>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

impl<'a, G> TopoSort<'a, G>
where
    G: Neighbors + VerticesBase + EdgesBase<Directed>,
    G::VertexId: NumIdType,
{
    pub fn into_vec(self) -> Result<Vec<G::VertexId>, Error<G>> {
        self.collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum Algo {
    Dfs,
    Kahn,
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

#[derive(Error)]
pub enum Error<G>
where
    G: GraphBase,
{
    #[error("the graph contains cycle")]
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
    G: VerticesBase,
{
    Dfs(visit::IntoIter<'a, DfsVisit<'a, G>, G>),
    Kahn(KahnIter<'a, G>),
}

impl<'a, G> Iterator for TopoSortInner<'a, G>
where
    G: Neighbors + VerticesBase + EdgesBase<Directed>,
    G::VertexId: NumIdType,
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
        core::{id::DefaultId, EdgesBaseWeak, EdgesMut, VerticesBaseWeak, VerticesMut},
        infra::proptest::graph_directed,
        storage::AdjList,
        visit::{DfsEvent, DfsEvents, Visitor},
    };

    fn assert_valid<'a, G>(toposort: TopoSort<'a, G>, graph: &'a G)
    where
        G: Neighbors
            + VerticesBase
            + EdgesBase<Directed>
            + VerticesBaseWeak
            + EdgesBaseWeak<Directed>,
        G::VertexId: NumIdType,
    {
        let sorted = toposort.collect::<Result<Vec<_>, _>>();

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

                for (src, dst) in graph.edge_ids().map(|e| graph.endpoints(&e).unwrap()) {
                    let i = map
                        .get(&src)
                        .unwrap_or_else(|| panic!("algorithm omitted vertex {src:?}"));

                    let j = map
                        .get(&dst)
                        .unwrap_or_else(|| panic!("algorithm omitted vertex {dst:?}"));

                    assert!(i < j, "invalid topological order for {src:?} -> {dst:?}");
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
        let toposort = TopoSort::on(&graph).with(Algo::Dfs).run();

        assert_valid(toposort, &graph);
    }

    #[test]
    fn dfs_cycle() {
        let graph = create_cyclic_graph();
        let toposort = TopoSort::on(&graph).with(Algo::Dfs).run();

        assert_valid(toposort, &graph);
    }

    #[test]
    fn dfs_disconnected() {
        let graph = create_disconnected_graph();
        let toposort = TopoSort::on(&graph).with(Algo::Dfs).run();

        assert_valid(toposort, &graph);
    }

    #[test]
    fn dfs_none_after_cycle() {
        let graph = create_cyclic_graph();
        let mut toposort = TopoSort::on(&graph).with(Algo::Dfs).run();

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
        let toposort = TopoSort::on(&graph).with(Algo::Kahn).run();

        assert_valid(toposort, &graph);
    }

    #[test]
    fn kahn_cycle() {
        let graph = create_cyclic_graph();
        let toposort = TopoSort::on(&graph).with(Algo::Kahn).run();

        assert_valid(toposort, &graph);
    }

    #[test]
    fn kahn_disconnected() {
        let graph = create_disconnected_graph();
        let toposort = TopoSort::on(&graph).with(Algo::Kahn).run();

        assert_valid(toposort, &graph);
    }

    #[test]
    fn kahn_none_after_cycle() {
        let graph = create_cyclic_graph();
        let mut toposort = TopoSort::on(&graph).with(Algo::Kahn).run();

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
        #[cfg_attr(not(proptest), ignore = "compile with --cfg proptest")]
        fn toposort_dfs_acyclic(graph in graph_directed(any::<()>(), any::<()>()).acyclic()) {
            let toposort = TopoSort::on(&graph).with(Algo::Dfs).run();
            assert_valid(toposort, &graph);
        }

        #[test]
        #[cfg_attr(not(proptest), ignore = "compile with --cfg proptest")]
        fn toposort_dfs_any(graph in graph_directed(any::<()>(), any::<()>())) {
            let toposort = TopoSort::on(&graph).with(Algo::Dfs).run();
            assert_valid(toposort, &graph);
        }

        #[test]
        #[cfg_attr(not(proptest), ignore = "compile with --cfg proptest")]
        fn toposort_kahn_acyclic(graph in graph_directed(any::<()>(), any::<()>()).acyclic()) {
            let toposort = TopoSort::on(&graph).with(Algo::Kahn).run();
            assert_valid(toposort, &graph);
        }

        #[test]
        #[cfg_attr(not(proptest), ignore = "compile with --cfg proptest")]
        fn toposort_kahn_any(graph in graph_directed(any::<()>(), any::<()>())) {
            let toposort = TopoSort::on(&graph).with(Algo::Kahn).run();
            assert_valid(toposort, &graph);
        }
    }
}
