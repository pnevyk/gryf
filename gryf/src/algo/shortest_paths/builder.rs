use std::marker::PhantomData;

use crate::core::{
    id::IntegerIdType,
    weight::{self, GetWeight, IsConstWeight, Weight},
    GraphBase, GraphRef, GraphWeak, Neighbors,
};

use super::{
    algo, bellman_ford::bellman_ford, bfs::bfs, dijkstra::dijkstra, Algo, Error, ShortestPaths,
};

enum AlgoExt {
    Algo(Algo),
    Bfs,
}

pub struct ShortestPathsBuilder<'a, W, G, F, A>
where
    G: GraphBase,
{
    graph: &'a G,
    goal: Option<G::VertexId>,
    edge_weight: F,
    algo: A,
    ty: PhantomData<fn() -> W>,
}

impl<W, G> ShortestPaths<W, G>
where
    G: GraphBase,
{
    pub fn on(graph: &G) -> ShortestPathsBuilder<'_, W, G, weight::Identity, algo::AnyAlgo> {
        ShortestPathsBuilder {
            graph,
            goal: None,
            edge_weight: weight::Identity,
            algo: algo::AnyAlgo,
            ty: PhantomData,
        }
    }
}

impl<'a, W, G, F, A> ShortestPathsBuilder<'a, W, G, F, A>
where
    G: GraphBase,
{
    pub fn goal(self, goal: G::VertexId) -> Self {
        Self {
            goal: Some(goal),
            ..self
        }
    }

    pub fn edge_weight<F2, V, E>(self, edge_weight: F2) -> ShortestPathsBuilder<'a, W, G, F2, A>
    where
        G: GraphRef<V, E>,
        F2: GetWeight<E, W>,
        W: Weight,
    {
        ShortestPathsBuilder {
            edge_weight,
            graph: self.graph,
            goal: self.goal,
            algo: self.algo,
            ty: PhantomData,
        }
    }

    // Using closures in `edge_weight` gives "type annotations needed" for the
    // closure argument. This method that uses explicit Fn signature circumvents
    // the problem.
    pub fn edge_weight_fn<F2, V, E>(self, edge_weight: F2) -> ShortestPathsBuilder<'a, W, G, F2, A>
    where
        G: GraphRef<V, E>,
        F2: Fn(&E) -> W,
        W: Weight,
    {
        self.edge_weight(edge_weight)
    }

    pub fn unit_weight(self) -> ShortestPathsBuilder<'a, W, G, weight::Unit, A> {
        ShortestPathsBuilder {
            edge_weight: weight::Unit,
            graph: self.graph,
            goal: self.goal,
            algo: self.algo,
            ty: PhantomData,
        }
    }

    pub fn dijkstra<V, E>(self) -> ShortestPathsBuilder<'a, W, G, F, algo::Dijkstra>
    where
        G: Neighbors + GraphWeak<V, E>,
    {
        ShortestPathsBuilder {
            graph: self.graph,
            goal: self.goal,
            edge_weight: self.edge_weight,
            algo: algo::Dijkstra,
            ty: PhantomData,
        }
    }

    pub fn bellman_ford<V, E>(self) -> ShortestPathsBuilder<'a, W, G, F, algo::BellmanFord>
    where
        G: GraphRef<V, E>,
        G::VertexId: IntegerIdType,
    {
        ShortestPathsBuilder {
            graph: self.graph,
            goal: self.goal,
            edge_weight: self.edge_weight,
            algo: algo::BellmanFord,
            ty: PhantomData,
        }
    }

    pub fn bfs(self) -> ShortestPathsBuilder<'a, W, G, F, algo::Bfs>
    where
        G: Neighbors,
        F: IsConstWeight,
    {
        ShortestPathsBuilder {
            graph: self.graph,
            goal: self.goal,
            edge_weight: self.edge_weight,
            algo: algo::Bfs,
            ty: PhantomData,
        }
    }

    pub fn using<V, E>(self, algo: Algo) -> ShortestPathsBuilder<'a, W, G, F, algo::SpecificAlgo>
    where
        G: Neighbors + GraphRef<V, E>,
        G::VertexId: IntegerIdType,
    {
        ShortestPathsBuilder {
            graph: self.graph,
            goal: self.goal,
            edge_weight: self.edge_weight,
            algo: algo::SpecificAlgo(Some(algo)),
            ty: PhantomData,
        }
    }

    pub fn using_opt<V, E>(
        self,
        algo: Option<Algo>,
    ) -> ShortestPathsBuilder<'a, W, G, F, algo::SpecificAlgo>
    where
        G: Neighbors + GraphRef<V, E>,
        G::VertexId: IntegerIdType,
    {
        ShortestPathsBuilder {
            graph: self.graph,
            goal: self.goal,
            edge_weight: self.edge_weight,
            algo: algo::SpecificAlgo(algo),
            ty: PhantomData,
        }
    }
}

impl<'a, W, G, F> ShortestPathsBuilder<'a, W, G, F, algo::AnyAlgo>
where
    G: GraphBase,
{
    pub fn run<V, E>(self, source: G::VertexId) -> Result<ShortestPaths<W, G>, Error>
    where
        G: Neighbors + GraphRef<V, E>,
        G::VertexId: IntegerIdType,
        F: GetWeight<E, W>,
        W: Weight,
    {
        let algo = self.choose_algo();
        let ShortestPathsBuilder {
            graph,
            goal,
            edge_weight,
            ..
        } = self;

        match algo {
            AlgoExt::Algo(Algo::Dijkstra) => dijkstra(graph, source, goal, edge_weight),
            AlgoExt::Algo(Algo::BellmanFord) => bellman_ford(graph, source, goal, edge_weight),
            AlgoExt::Bfs => bfs(graph, source, goal, edge_weight.get_const().unwrap()),
        }
    }
}

impl<'a, W, G, F> ShortestPathsBuilder<'a, W, G, F, algo::Dijkstra>
where
    G: GraphBase,
{
    pub fn run<V, E>(self, source: G::VertexId) -> Result<ShortestPaths<W, G>, Error>
    where
        G: Neighbors + GraphWeak<V, E>,
        F: GetWeight<E, W>,
        W: Weight,
    {
        let ShortestPathsBuilder {
            graph,
            goal,
            edge_weight,
            ..
        } = self;

        dijkstra(graph, source, goal, edge_weight)
    }
}

impl<'a, W, G, F> ShortestPathsBuilder<'a, W, G, F, algo::BellmanFord>
where
    G: GraphBase,
{
    pub fn run<V, E>(self, source: G::VertexId) -> Result<ShortestPaths<W, G>, Error>
    where
        G: GraphRef<V, E>,
        G::VertexId: IntegerIdType,
        F: GetWeight<E, W>,
        W: Weight,
    {
        let ShortestPathsBuilder {
            graph,
            edge_weight,
            goal,
            ..
        } = self;

        bellman_ford(graph, source, goal, edge_weight)
    }
}

impl<'a, W, G, F> ShortestPathsBuilder<'a, W, G, F, algo::Bfs>
where
    G: GraphBase,
{
    pub fn run(self, source: G::VertexId) -> Result<ShortestPaths<W, G>, Error>
    where
        G: Neighbors,
        F: GetWeight<(), W>,
        W: Weight,
    {
        let ShortestPathsBuilder {
            graph,
            goal,
            edge_weight,
            ..
        } = self;
        bfs(graph, source, goal, edge_weight.get_const().unwrap())
    }
}

impl<'a, W, G, F> ShortestPathsBuilder<'a, W, G, F, algo::SpecificAlgo>
where
    G: GraphBase,
{
    pub fn run<V, E>(self, source: G::VertexId) -> Result<ShortestPaths<W, G>, Error>
    where
        G: Neighbors + GraphRef<V, E>,
        G::VertexId: IntegerIdType,
        F: GetWeight<E, W>,
        W: Weight,
    {
        let algo = self
            .algo
            .0
            .map(AlgoExt::Algo)
            .unwrap_or_else(|| self.choose_algo());
        let ShortestPathsBuilder {
            graph,
            goal,
            edge_weight,
            ..
        } = self;

        match algo {
            AlgoExt::Algo(Algo::Dijkstra) => dijkstra(graph, source, goal, edge_weight),
            AlgoExt::Algo(Algo::BellmanFord) => bellman_ford(graph, source, goal, edge_weight),
            AlgoExt::Bfs => bfs(graph, source, goal, edge_weight.get_const().unwrap()),
        }
    }
}

impl<'a, W, G, F, A> ShortestPathsBuilder<'a, W, G, F, A>
where
    G: GraphBase,
{
    fn choose_algo<V, E>(&self) -> AlgoExt
    where
        G: Neighbors + GraphRef<V, E>,
        G::VertexId: IntegerIdType,
        F: GetWeight<E, W>,
        W: Weight,
    {
        let ShortestPathsBuilder {
            graph, edge_weight, ..
        } = self;

        if W::is_unsigned() && edge_weight.is_const() {
            // The weight is constant, we can use standard BFS algorithm without
            // any overhead.
            AlgoExt::Bfs
        } else if !W::is_unsigned() {
            if graph.is_directed() {
                // There is a possibility that a negative weight is encountered,
                // so, for directed graphs, we conservatively use Bellman-Ford.
                AlgoExt::Algo(Algo::BellmanFord)
            } else {
                // Although Bellman-Ford is implemented such that it somehow
                // handles undirected graphs, any negative edge creates negative
                // cycle, which is an error. Hence we use Dijkstra, which
                // returns negative edge error when one is encountered, but has
                // better runtime in the happy case.
                AlgoExt::Algo(Algo::Dijkstra)
            }
        } else {
            AlgoExt::Algo(Algo::Dijkstra)
        }
    }
}
