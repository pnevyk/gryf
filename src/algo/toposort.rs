use crate::{
    core::{index::NumIndexType, marker::Directed, EdgesBase, Neighbors, VerticesBase},
    visit,
};

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
    G::VertexIndex: NumIndexType,
{
    type Item = Result<G::VertexIndex, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    Cycle,
}

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
    G::VertexIndex: NumIndexType,
{
    type Item = Result<G::VertexIndex, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            TopoSortInner::Dfs(dfs) => dfs.next(),
            TopoSortInner::Kahn(kahn) => kahn.next(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        core::{index::DefaultIndexing, EdgesBaseWeak, EdgesMut, VerticesBaseWeak, VerticesMut},
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
        G::VertexIndex: NumIndexType,
    {
        let sorted = toposort.collect::<Result<Vec<_>, _>>();

        let cycle =
            DfsEvents::new(graph)
                .start_all(graph)
                .iter(graph)
                .find_map(|event| match event {
                    DfsEvent::BackEdge { .. } => Some(Error::Cycle),
                    _ => None,
                });

        match (sorted, cycle) {
            (Ok(sorted), None) => {
                for (src, dst) in graph.edge_indices().map(|e| graph.endpoints(&e).unwrap()) {
                    let i = sorted
                        .iter()
                        .copied()
                        .enumerate()
                        .find_map(|(k, v)| (v == src).then_some(Some(k)))
                        .unwrap_or_else(|| panic!("algorithm omitted vertex {:?}", src));

                    let j = sorted
                        .iter()
                        .copied()
                        .enumerate()
                        .find_map(|(k, v)| (v == dst).then_some(Some(k)))
                        .unwrap_or_else(|| panic!("algorithm omitted vertex {:?}", dst));

                    assert!(
                        i < j,
                        "invalid topological order for {:?} -> {:?}",
                        src,
                        dst
                    );
                }
            }
            (Ok(_), Some(error)) => panic!("algorithm did not detect error: {:?}", error),
            (Err(error), None) => panic!("algorithm incorrectly returned error: {:?}", error),
            (Err(_), Some(_)) => {}
        }
    }

    fn create_basic_graph() -> AdjList<(), (), Directed, DefaultIndexing> {
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

    fn create_cyclic_graph() -> AdjList<(), (), Directed, DefaultIndexing> {
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

    fn create_disconnected_graph() -> AdjList<(), (), Directed, DefaultIndexing> {
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
}
