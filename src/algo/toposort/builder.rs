use crate::{index::NumIndexType, marker::Directed, traits::*, visit::Visitor};

use super::dfs::{dfs_visit, DfsVisit};
use super::kahn::kahn;
use super::{algo, Algo, TopoSort, TopoSortInner};

pub struct TopoSortBuilder<'a, G, A> {
    graph: &'a G,
    algo: A,
}

impl<G> TopoSort<'_, G>
where
    G: VerticesBase + EdgesBase<Directed>,
{
    pub fn on(graph: &G) -> TopoSortBuilder<'_, G, algo::Any> {
        TopoSortBuilder {
            graph,
            algo: algo::Any,
        }
    }
}

impl<'a, G, A> TopoSortBuilder<'a, G, A> {
    pub fn dfs(self) -> TopoSortBuilder<'a, G, algo::Dfs> {
        TopoSortBuilder {
            graph: self.graph,
            algo: algo::Dfs,
        }
    }
}

impl<'a, G, A> TopoSortBuilder<'a, G, A> {
    pub fn kahn(self) -> TopoSortBuilder<'a, G, algo::Kahn>
    where
        G: GraphBase,
        G::VertexIndex: NumIndexType,
    {
        TopoSortBuilder {
            graph: self.graph,
            algo: algo::Kahn,
        }
    }
}

impl<'a, G, A> TopoSortBuilder<'a, G, A>
where
    G: GraphBase,
{
    pub fn with(self, algo: Algo) -> TopoSortBuilder<'a, G, algo::Specific>
    where
        G::VertexIndex: NumIndexType,
    {
        TopoSortBuilder {
            graph: self.graph,
            algo: algo::Specific(Some(algo)),
        }
    }

    pub fn with_opt(self, algo: Option<Algo>) -> TopoSortBuilder<'a, G, algo::Specific>
    where
        G::VertexIndex: NumIndexType,
    {
        TopoSortBuilder {
            graph: self.graph,
            algo: algo::Specific(algo),
        }
    }
}

impl<'a, G> TopoSortBuilder<'a, G, algo::Any>
where
    G: VerticesBase + EdgesBase<Directed>,
{
    pub fn run(self) -> TopoSort<'a, G>
    where
        G: Neighbors,
    {
        TopoSort {
            inner: TopoSortInner::Dfs(dfs_visit(self.graph).into_iter(self.graph)),
        }
    }
}

impl<'a, G> TopoSortBuilder<'a, G, algo::Dfs>
where
    G: VerticesBase + EdgesBase<Directed>,
{
    pub fn run(self) -> DfsVisit<'a, G> {
        dfs_visit(self.graph)
    }
}

impl<'a, G> TopoSortBuilder<'a, G, algo::Kahn>
where
    G: VerticesBase + EdgesBase<Directed>,
{
    pub fn run(self) -> TopoSort<'a, G>
    where
        G: Neighbors,
        G::VertexIndex: NumIndexType,
    {
        TopoSort {
            inner: TopoSortInner::Kahn(kahn(self.graph)),
        }
    }
}

impl<'a, G> TopoSortBuilder<'a, G, algo::Specific>
where
    G: VerticesBase + EdgesBase<Directed>,
{
    pub fn run(self) -> TopoSort<'a, G>
    where
        G: Neighbors,
        G::VertexIndex: NumIndexType,
    {
        let inner = match self.algo.0 {
            Some(Algo::Dfs) | None => {
                TopoSortInner::Dfs(dfs_visit(self.graph).into_iter(self.graph))
            }
            Some(Algo::Kahn) => TopoSortInner::Kahn(kahn(self.graph)),
        };

        TopoSort { inner }
    }
}
