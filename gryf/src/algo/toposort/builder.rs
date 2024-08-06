use crate::{
    core::{id::IntegerIdType, marker::Directed, GraphBase, Neighbors, VertexSet},
    visit::Visitor,
};

use super::{
    algo,
    dfs::{dfs_visit, DfsVisit},
    kahn::kahn,
    Algo, TopoSort, TopoSortInner,
};

pub struct TopoSortBuilder<'a, G, A> {
    graph: &'a G,
    algo: A,
}

impl<G> TopoSort<'_, G>
where
    G: GraphBase<EdgeType = Directed> + VertexSet,
{
    pub fn on(graph: &G) -> TopoSortBuilder<'_, G, algo::AnyAlgo> {
        TopoSortBuilder {
            graph,
            algo: algo::AnyAlgo,
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

    pub fn kahn(self) -> TopoSortBuilder<'a, G, algo::Kahn>
    where
        G: GraphBase,
        G::VertexId: IntegerIdType,
    {
        TopoSortBuilder {
            graph: self.graph,
            algo: algo::Kahn,
        }
    }

    pub fn with(self, algo: Algo) -> TopoSortBuilder<'a, G, algo::SpecificAlgo>
    where
        G: GraphBase,
        G::VertexId: IntegerIdType,
    {
        TopoSortBuilder {
            graph: self.graph,
            algo: algo::SpecificAlgo(Some(algo)),
        }
    }

    pub fn with_opt(self, algo: Option<Algo>) -> TopoSortBuilder<'a, G, algo::SpecificAlgo>
    where
        G: GraphBase,
        G::VertexId: IntegerIdType,
    {
        TopoSortBuilder {
            graph: self.graph,
            algo: algo::SpecificAlgo(algo),
        }
    }
}

impl<'a, G> TopoSortBuilder<'a, G, algo::AnyAlgo>
where
    G: GraphBase<EdgeType = Directed> + VertexSet,
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
    G: GraphBase<EdgeType = Directed> + VertexSet,
{
    pub fn run(self) -> DfsVisit<'a, G> {
        dfs_visit(self.graph)
    }
}

impl<'a, G> TopoSortBuilder<'a, G, algo::Kahn>
where
    G: GraphBase<EdgeType = Directed> + VertexSet,
{
    pub fn run(self) -> TopoSort<'a, G>
    where
        G: Neighbors,
        G::VertexId: IntegerIdType,
    {
        TopoSort {
            inner: TopoSortInner::Kahn(kahn(self.graph)),
        }
    }
}

impl<'a, G> TopoSortBuilder<'a, G, algo::SpecificAlgo>
where
    G: GraphBase<EdgeType = Directed> + VertexSet,
{
    pub fn run(self) -> TopoSort<'a, G>
    where
        G: Neighbors,
        G::VertexId: IntegerIdType,
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
