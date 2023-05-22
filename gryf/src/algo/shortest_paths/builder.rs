use std::{cmp::max, marker::PhantomData};

use crate::core::{
    index::NumIndexType,
    marker::EdgeType,
    weights::{self, GetWeight, IsConstWeight},
    Edges, EdgesWeak, GraphBase, Neighbors, VerticesBase, VerticesBaseWeak, Weight,
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
    goal: Option<G::VertexIndex>,
    edge_weight: F,
    algo: A,
    ty: PhantomData<fn() -> W>,
}

impl<W, G> ShortestPaths<W, G>
where
    G: GraphBase,
{
    pub fn on(graph: &G) -> ShortestPathsBuilder<'_, W, G, weights::Identity, algo::AnyAlgo> {
        ShortestPathsBuilder {
            graph,
            goal: None,
            edge_weight: weights::Identity,
            algo: algo::AnyAlgo,
            ty: PhantomData,
        }
    }
}

impl<'a, W, G, F, A> ShortestPathsBuilder<'a, W, G, F, A>
where
    G: GraphBase,
{
    pub fn goal(self, goal: G::VertexIndex) -> Self {
        Self {
            goal: Some(goal),
            ..self
        }
    }

    pub fn edge_weight<F2, E, Ty: EdgeType>(
        self,
        edge_weight: F2,
    ) -> ShortestPathsBuilder<'a, W, G, F2, A>
    where
        G: Edges<E, Ty>,
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
    pub fn edge_weight_fn<F2, E, Ty: EdgeType>(
        self,
        edge_weight: F2,
    ) -> ShortestPathsBuilder<'a, W, G, F2, A>
    where
        G: Edges<E, Ty>,
        F2: Fn(&E) -> W,
        W: Weight,
    {
        self.edge_weight(edge_weight)
    }

    pub fn unit_weight(self) -> ShortestPathsBuilder<'a, W, G, weights::Unit, A> {
        ShortestPathsBuilder {
            edge_weight: weights::Unit,
            graph: self.graph,
            goal: self.goal,
            algo: self.algo,
            ty: PhantomData,
        }
    }

    pub fn dijkstra<E, Ty: EdgeType>(self) -> ShortestPathsBuilder<'a, W, G, F, algo::Dijkstra>
    where
        G: VerticesBaseWeak + EdgesWeak<E, Ty> + Neighbors,
    {
        ShortestPathsBuilder {
            graph: self.graph,
            goal: self.goal,
            edge_weight: self.edge_weight,
            algo: algo::Dijkstra,
            ty: PhantomData,
        }
    }

    pub fn bellman_ford<E, Ty: EdgeType>(
        self,
    ) -> ShortestPathsBuilder<'a, W, G, F, algo::BellmanFord>
    where
        G: VerticesBase + Edges<E, Ty>,
        G::VertexIndex: NumIndexType,
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
        G: VerticesBaseWeak + Neighbors,
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

    pub fn with<E, Ty: EdgeType>(
        self,
        algo: Algo,
    ) -> ShortestPathsBuilder<'a, W, G, F, algo::SpecificAlgo>
    where
        G: VerticesBase + Edges<E, Ty> + VerticesBaseWeak + EdgesWeak<E, Ty> + Neighbors,
        G::VertexIndex: NumIndexType,
    {
        ShortestPathsBuilder {
            graph: self.graph,
            goal: self.goal,
            edge_weight: self.edge_weight,
            algo: algo::SpecificAlgo(Some(algo)),
            ty: PhantomData,
        }
    }

    pub fn with_opt<E, Ty: EdgeType>(
        self,
        algo: Option<Algo>,
    ) -> ShortestPathsBuilder<'a, W, G, F, algo::SpecificAlgo>
    where
        G: VerticesBase + Edges<E, Ty> + VerticesBaseWeak + EdgesWeak<E, Ty> + Neighbors,
        G::VertexIndex: NumIndexType,
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
    pub fn run<E, Ty: EdgeType>(self, start: G::VertexIndex) -> Result<ShortestPaths<W, G>, Error>
    where
        G: VerticesBase + Edges<E, Ty> + VerticesBaseWeak + EdgesWeak<E, Ty> + Neighbors,
        G::VertexIndex: NumIndexType,
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
            AlgoExt::Algo(Algo::Dijkstra) => dijkstra(graph, start, goal, edge_weight),
            AlgoExt::Algo(Algo::BellmanFord) => bellman_ford(graph, start, goal, edge_weight),
            AlgoExt::Bfs => bfs(graph, start, goal, edge_weight.get_const().unwrap()),
        }
    }
}

impl<'a, W, G, F> ShortestPathsBuilder<'a, W, G, F, algo::Dijkstra>
where
    G: GraphBase,
{
    pub fn run<E, Ty: EdgeType>(self, start: G::VertexIndex) -> Result<ShortestPaths<W, G>, Error>
    where
        G: VerticesBaseWeak + EdgesWeak<E, Ty> + Neighbors,
        F: GetWeight<E, W>,
        W: Weight,
    {
        let ShortestPathsBuilder {
            graph,
            goal,
            edge_weight,
            ..
        } = self;

        dijkstra(graph, start, goal, edge_weight)
    }
}

impl<'a, W, G, F> ShortestPathsBuilder<'a, W, G, F, algo::BellmanFord>
where
    G: GraphBase,
{
    pub fn run<E, Ty: EdgeType>(self, start: G::VertexIndex) -> Result<ShortestPaths<W, G>, Error>
    where
        G: VerticesBase + Edges<E, Ty>,
        G::VertexIndex: NumIndexType,
        F: GetWeight<E, W>,
        W: Weight,
    {
        let ShortestPathsBuilder {
            graph,
            edge_weight,
            goal,
            ..
        } = self;

        bellman_ford(graph, start, goal, edge_weight)
    }
}

impl<'a, W, G, F> ShortestPathsBuilder<'a, W, G, F, algo::Bfs>
where
    G: GraphBase,
{
    pub fn run(self, start: G::VertexIndex) -> Result<ShortestPaths<W, G>, Error>
    where
        G: VerticesBaseWeak + Neighbors,
        F: GetWeight<(), W>,
        W: Weight,
    {
        let ShortestPathsBuilder {
            graph,
            goal,
            edge_weight,
            ..
        } = self;
        bfs(graph, start, goal, edge_weight.get_const().unwrap())
    }
}

impl<'a, W, G, F> ShortestPathsBuilder<'a, W, G, F, algo::SpecificAlgo>
where
    G: GraphBase,
{
    pub fn run<E, Ty: EdgeType>(self, start: G::VertexIndex) -> Result<ShortestPaths<W, G>, Error>
    where
        G: VerticesBase + Edges<E, Ty> + VerticesBaseWeak + EdgesWeak<E, Ty> + Neighbors,
        G::VertexIndex: NumIndexType,
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
            AlgoExt::Algo(Algo::Dijkstra) => dijkstra(graph, start, goal, edge_weight),
            AlgoExt::Algo(Algo::BellmanFord) => bellman_ford(graph, start, goal, edge_weight),
            AlgoExt::Bfs => bfs(graph, start, goal, edge_weight.get_const().unwrap()),
        }
    }
}

impl<'a, W, G, F, A> ShortestPathsBuilder<'a, W, G, F, A>
where
    G: GraphBase,
{
    fn choose_algo<E, Ty: EdgeType>(&self) -> AlgoExt
    where
        G: VerticesBase + Edges<E, Ty> + VerticesBaseWeak + EdgesWeak<E, Ty> + Neighbors,
        G::VertexIndex: NumIndexType,
        F: GetWeight<E, W>,
        W: Weight,
    {
        let ShortestPathsBuilder {
            graph,
            goal,
            edge_weight,
            ..
        } = self;

        if W::is_unsigned() && edge_weight.get_const().is_some() {
            // The weight is constant, we can use standard BFS algorithm without
            // any overhead.
            AlgoExt::Bfs
        } else if !W::is_unsigned() {
            if Ty::is_directed() {
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
        } else if goal.is_some() {
            // If the goal is specified, Dijkstra's algorithm likely finishes
            // without the need of traversing the entire graph.
            AlgoExt::Algo(Algo::Dijkstra)
        } else {
            let v = graph.vertex_count();
            let e = graph.edge_count();

            // Compare the worst-case bounds. This will result in choosing
            // Dijkstra in vast majority of cases.
            if v * e < max(e, v * (v as f64).log2() as usize) {
                AlgoExt::Algo(Algo::BellmanFord)
            } else {
                AlgoExt::Algo(Algo::Dijkstra)
            }
        }
    }
}
