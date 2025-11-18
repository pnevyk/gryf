use crate::{
    algo::connected_components::kosaraju::kosaraju,
    core::{
        GraphBase, Neighbors, VertexSet,
        marker::{Directed, Undirected},
    },
};

use super::{
    AlgoStrong, AlgoWeak, ConnectedComponents, StronglyConnectedComponents, algo, dfs::dfs,
};

pub struct ConnectedComponentsBuilder<'a, G, A> {
    graph: &'a G,
    algo: A,
}

pub struct StronglyConnectedComponentsBuilder<'a, G, A> {
    graph: &'a G,
    algo: A,
}

impl<G> ConnectedComponents<G>
where
    G: GraphBase,
{
    #[doc = include_str!("../../../docs/include/algo.on.md")]
    pub fn on(graph: &G) -> ConnectedComponentsBuilder<'_, G, algo::AnyAlgo>
    where
        G: GraphBase<EdgeType = Undirected>,
    {
        ConnectedComponentsBuilder {
            graph,
            algo: algo::AnyAlgo,
        }
    }
}

impl<G> StronglyConnectedComponents<G>
where
    G: GraphBase,
{
    #[doc = include_str!("../../../docs/include/algo.on.md")]
    pub fn on(graph: &G) -> StronglyConnectedComponentsBuilder<'_, G, algo::AnyAlgo>
    where
        G: GraphBase<EdgeType = Directed>,
    {
        StronglyConnectedComponentsBuilder {
            graph,
            algo: algo::AnyAlgo,
        }
    }
}

impl<'a, G, A> ConnectedComponentsBuilder<'a, G, A>
where
    G: GraphBase<EdgeType = Undirected>,
{
    /// Chooses the DFS algorithm.
    ///
    /// See [`AlgoWeak::Dfs`] for details.
    pub fn dfs(self) -> ConnectedComponentsBuilder<'a, G, algo::Dfs> {
        ConnectedComponentsBuilder {
            graph: self.graph,
            algo: algo::Dfs,
        }
    }

    #[doc = include_str!("../../../docs/include/algo.using.md")]
    pub fn using(self, algo: AlgoWeak) -> ConnectedComponentsBuilder<'a, G, algo::SpecificAlgoWeak>
    where
        G: GraphBase,
    {
        ConnectedComponentsBuilder {
            graph: self.graph,
            algo: algo::SpecificAlgoWeak(Some(algo)),
        }
    }

    #[doc = include_str!("../../../docs/include/algo.using_opt.md")]
    pub fn using_opt(
        self,
        algo: Option<AlgoWeak>,
    ) -> ConnectedComponentsBuilder<'a, G, algo::SpecificAlgoWeak>
    where
        G: GraphBase,
    {
        ConnectedComponentsBuilder {
            graph: self.graph,
            algo: algo::SpecificAlgoWeak(algo),
        }
    }
}

impl<'a, G, A> StronglyConnectedComponentsBuilder<'a, G, A>
where
    G: GraphBase<EdgeType = Directed>,
{
    /// Chooses the Kosaraju's algorithm.
    ///
    /// See [`AlgoStrong::Kosaraju`] for details.
    pub fn kosaraju(self) -> StronglyConnectedComponentsBuilder<'a, G, algo::Kosaraju> {
        StronglyConnectedComponentsBuilder {
            graph: self.graph,
            algo: algo::Kosaraju,
        }
    }

    #[doc = include_str!("../../../docs/include/algo.using.md")]
    pub fn using(
        self,
        algo: AlgoStrong,
    ) -> StronglyConnectedComponentsBuilder<'a, G, algo::SpecificAlgoStrong>
    where
        G: GraphBase,
    {
        StronglyConnectedComponentsBuilder {
            graph: self.graph,
            algo: algo::SpecificAlgoStrong(Some(algo)),
        }
    }

    #[doc = include_str!("../../../docs/include/algo.using_opt.md")]
    pub fn using_opt(
        self,
        algo: Option<AlgoStrong>,
    ) -> StronglyConnectedComponentsBuilder<'a, G, algo::SpecificAlgoStrong>
    where
        G: GraphBase,
    {
        StronglyConnectedComponentsBuilder {
            graph: self.graph,
            algo: algo::SpecificAlgoStrong(algo),
        }
    }
}

impl<'a, G> ConnectedComponentsBuilder<'a, G, algo::AnyAlgo>
where
    G: GraphBase<EdgeType = Undirected>,
{
    #[doc = include_str!("../../../docs/include/algo.run.md")]
    pub fn run(self) -> ConnectedComponents<G>
    where
        G: Neighbors + VertexSet,
    {
        ConnectedComponents {
            components: dfs(self.graph),
        }
    }
}

impl<'a, G> ConnectedComponentsBuilder<'a, G, algo::Dfs>
where
    G: GraphBase<EdgeType = Undirected>,
{
    #[doc = include_str!("../../../docs/include/algo.run.md")]
    pub fn run(self) -> ConnectedComponents<G>
    where
        G: Neighbors + VertexSet,
    {
        ConnectedComponents {
            components: dfs(self.graph),
        }
    }
}

impl<'a, G> ConnectedComponentsBuilder<'a, G, algo::SpecificAlgoWeak>
where
    G: GraphBase<EdgeType = Undirected>,
{
    #[doc = include_str!("../../../docs/include/algo.run.md")]
    pub fn run(self) -> ConnectedComponents<G>
    where
        G: Neighbors + VertexSet,
    {
        let components = match self.algo.0 {
            Some(AlgoWeak::Dfs) => dfs(self.graph),
            None => dfs(self.graph),
        };

        ConnectedComponents { components }
    }
}

impl<'a, G> StronglyConnectedComponentsBuilder<'a, G, algo::AnyAlgo>
where
    G: GraphBase<EdgeType = Directed>,
{
    #[doc = include_str!("../../../docs/include/algo.run.md")]
    pub fn run(self) -> StronglyConnectedComponents<G>
    where
        G: Neighbors + VertexSet,
    {
        StronglyConnectedComponents {
            inner: ConnectedComponents {
                components: kosaraju(self.graph),
            },
        }
    }
}

impl<'a, G> StronglyConnectedComponentsBuilder<'a, G, algo::Kosaraju>
where
    G: GraphBase<EdgeType = Directed>,
{
    #[doc = include_str!("../../../docs/include/algo.run.md")]
    pub fn run(self) -> StronglyConnectedComponents<G>
    where
        G: Neighbors + VertexSet,
    {
        StronglyConnectedComponents {
            inner: ConnectedComponents {
                components: kosaraju(self.graph),
            },
        }
    }
}

impl<'a, G> StronglyConnectedComponentsBuilder<'a, G, algo::SpecificAlgoStrong>
where
    G: GraphBase<EdgeType = Directed>,
{
    #[doc = include_str!("../../../docs/include/algo.run.md")]
    pub fn run(self) -> StronglyConnectedComponents<G>
    where
        G: Neighbors + VertexSet,
    {
        let components = match self.algo.0 {
            Some(AlgoStrong::Kosaraju) => kosaraju(self.graph),
            None => kosaraju(self.graph),
        };

        StronglyConnectedComponents {
            inner: ConnectedComponents { components },
        }
    }
}
