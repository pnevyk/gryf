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

/// Builder for [`TopoSort`].
pub struct TopoSortBuilder<'a, G, A> {
    graph: &'a G,
    algo: A,
}

impl<G> TopoSort<'_, G>
where
    G: GraphBase<EdgeType = Directed> + VertexSet,
{
    #[doc = include_str!("../../../docs/include/algo.on.md")]
    pub fn on(graph: &G) -> TopoSortBuilder<'_, G, algo::AnyAlgo> {
        TopoSortBuilder {
            graph,
            algo: algo::AnyAlgo,
        }
    }
}

impl<'a, G, A> TopoSortBuilder<'a, G, A> {
    /// Chooses the DFS algorithm.
    ///
    /// See [`Algo::Dfs`] for details.
    pub fn dfs(self) -> TopoSortBuilder<'a, G, algo::Dfs> {
        TopoSortBuilder {
            graph: self.graph,
            algo: algo::Dfs,
        }
    }

    /// Chooses the Kahn's algorithm.
    ///
    /// See [`Algo::Kahn`] for details.
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

    #[doc = include_str!("../../../docs/include/algo.using.md")]
    pub fn using(self, algo: Algo) -> TopoSortBuilder<'a, G, algo::SpecificAlgo>
    where
        G: GraphBase,
        G::VertexId: IntegerIdType,
    {
        TopoSortBuilder {
            graph: self.graph,
            algo: algo::SpecificAlgo(Some(algo)),
        }
    }

    #[doc = include_str!("../../../docs/include/algo.using_opt.md")]
    pub fn using_opt(self, algo: Option<Algo>) -> TopoSortBuilder<'a, G, algo::SpecificAlgo>
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
    #[doc = include_str!("../../../docs/include/algo.run.md")]
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

impl<'a, G> TopoSortBuilder<'a, G, algo::Dfs>
where
    G: GraphBase<EdgeType = Directed> + VertexSet,
{
    #[doc = include_str!("../../../docs/include/algo.run.md")]
    pub fn run(self) -> DfsVisit<'a, G> {
        dfs_visit(self.graph)
    }
}

impl<'a, G> TopoSortBuilder<'a, G, algo::Kahn>
where
    G: GraphBase<EdgeType = Directed> + VertexSet,
{
    #[doc = include_str!("../../../docs/include/algo.run.md")]
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
    #[doc = include_str!("../../../docs/include/algo.run.md")]
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
