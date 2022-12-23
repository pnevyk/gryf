use std::{cmp::max, marker::PhantomData};

use crate::core::{
    index::NumIndexType,
    marker::EdgeType,
    weights::{self, GetWeight},
    Edges, EdgesWeak, GraphBase, Neighbors, VerticesBase, VerticesBaseWeak, Weight,
};

use super::{algo, bellman_ford::bellman_ford, dijkstra::dijkstra, Algo, Error, ShortestPaths};

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
            Algo::Dijkstra => dijkstra(graph, start, goal, edge_weight),
            Algo::BellmanFord => bellman_ford(graph, start, edge_weight),
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
            graph, edge_weight, ..
        } = self;

        bellman_ford(graph, start, edge_weight)
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
        let algo = self.algo.0.unwrap_or_else(|| self.choose_algo());
        let ShortestPathsBuilder {
            graph,
            goal,
            edge_weight,
            ..
        } = self;

        match algo {
            Algo::Dijkstra => dijkstra(graph, start, goal, edge_weight),
            Algo::BellmanFord => bellman_ford(graph, start, edge_weight),
        }
    }
}

impl<'a, W, G, F, A> ShortestPathsBuilder<'a, W, G, F, A>
where
    G: GraphBase,
{
    fn choose_algo<E, Ty: EdgeType>(&self) -> Algo
    where
        G: VerticesBase + Edges<E, Ty> + VerticesBaseWeak + EdgesWeak<E, Ty> + Neighbors,
        G::VertexIndex: NumIndexType,
        F: GetWeight<E, W>,
        W: Weight,
    {
        let ShortestPathsBuilder { graph, goal, .. } = self;

        if !W::is_unsigned() {
            // There is a possibility that a negative weight is encountered,
            // so we conservatively use Bellman-Ford.
            Algo::BellmanFord
        } else if goal.is_some() {
            // If the goal is specified, Dijkstra's algorithm likely
            // finishes without the need of traversing the entire graph.
            Algo::Dijkstra
        } else {
            let v = graph.vertex_count();
            let e = graph.edge_count();

            // Compare the worst-case bounds. This will result in choosing
            // Dijkstra in vast majority of cases.
            if v * e < max(e, v * (v as f64).log2() as usize) {
                Algo::BellmanFord
            } else {
                Algo::Dijkstra
            }
        }
    }
}
