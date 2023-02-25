use std::{borrow::Borrow, ops::Index};

use rustc_hash::FxHashMap;
use thiserror::Error;

use crate::core::GraphBase;

mod bellman_ford;
mod builder;
mod dijkstra;

pub use builder::ShortestPathsBuilder;

#[derive(Debug)]
pub struct ShortestPaths<W, G: GraphBase> {
    start: G::VertexIndex,
    // Using HashMaps because the algorithm supports early termination when
    // reaching given goal. It is likely that reaching goal means visiting a
    // subgraph which is significantly smaller than the original graph.
    dist: FxHashMap<G::VertexIndex, W>,
    pred: FxHashMap<G::VertexIndex, G::VertexIndex>,
}

impl<W, G> ShortestPaths<W, G>
where
    G: GraphBase,
{
    pub fn start(&self) -> &G::VertexIndex {
        &self.start
    }

    pub fn dist<VI>(&self, to: VI) -> Option<&W>
    where
        VI: Borrow<G::VertexIndex>,
    {
        self.dist.get(to.borrow())
    }

    pub fn reconstruct(&self, to: G::VertexIndex) -> PathReconstruction<'_, G> {
        PathReconstruction {
            curr: to,
            pred: &self.pred,
        }
    }
}

impl<W, G, VI> Index<VI> for ShortestPaths<W, G>
where
    G: GraphBase,
    VI: Borrow<G::VertexIndex>,
{
    type Output = W;

    fn index(&self, index: VI) -> &Self::Output {
        self.dist(index).unwrap()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum Algo {
    Dijkstra,
    BellmanFord,
}

mod algo {
    use super::Algo;

    #[derive(Debug)]
    pub struct AnyAlgo;

    #[derive(Debug)]
    pub struct SpecificAlgo(pub Option<Algo>);

    #[derive(Debug)]
    pub struct Dijkstra;

    #[derive(Debug)]
    pub struct BellmanFord;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Error)]
pub enum Error {
    #[error("edge with negative weight encountered")]
    NegativeWeight,
    #[error("negative cycle encountered")]
    NegativeCycle,
    #[error("edge not available")]
    EdgeNotAvailable,
}

pub struct PathReconstruction<'a, G: GraphBase> {
    curr: G::VertexIndex,
    pred: &'a FxHashMap<G::VertexIndex, G::VertexIndex>,
}

impl<'a, G: GraphBase> Iterator for PathReconstruction<'a, G> {
    type Item = G::VertexIndex;

    fn next(&mut self) -> Option<Self::Item> {
        self.curr = self.pred.get(&self.curr).cloned()?;
        Some(self.curr.clone())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        core::{
            index::{DefaultIndexing, EdgeIndex, VertexIndex},
            marker::{Directed, Undirected},
            EdgesMut, VerticesMut,
        },
        storage::AdjList,
    };

    use super::*;

    use assert_matches::assert_matches;

    fn create_basic_graph() -> AdjList<(), i32, Undirected, DefaultIndexing> {
        let mut graph = AdjList::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, 3);
        graph.add_edge(&v0, &v2, 2);
        graph.add_edge(&v1, &v2, 2);
        graph.add_edge(&v1, &v3, 2);
        graph.add_edge(&v1, &v4, 7);
        graph.add_edge(&v2, &v3, 5);
        graph.add_edge(&v3, &v4, 3);
        graph.add_edge(&v4, &v5, 10);

        graph
    }

    fn v(index: usize) -> VertexIndex {
        index.into()
    }

    fn e(index: usize) -> EdgeIndex {
        index.into()
    }

    #[test]
    fn dijkstra_basic() {
        let graph = create_basic_graph();
        let shortest_paths = ShortestPaths::on(&graph)
            .with(Algo::Dijkstra)
            .run(v(0))
            .unwrap();

        assert_eq!(shortest_paths.dist(v(4)), Some(&8));
        assert_eq!(
            shortest_paths.reconstruct(v(4)).collect::<Vec<_>>(),
            vec![v(3), v(1), v(0)]
        );

        assert_eq!(shortest_paths.dist(v(2)), Some(&2));
    }

    #[test]
    fn dijkstra_early_termination() {
        let graph = create_basic_graph();
        let shortest_paths = ShortestPaths::on(&graph)
            .goal(v(4))
            .with(Algo::Dijkstra)
            .run(v(0))
            .unwrap();

        assert!(shortest_paths.dist(v(5)).is_none());
    }

    #[test]
    fn dijkstra_negative_edge() {
        let mut graph = create_basic_graph();
        graph.replace_edge(&e(2), -1);

        let shortest_paths = ShortestPaths::on(&graph)
            .goal(v(4))
            .with(Algo::Dijkstra)
            .run(v(0));

        assert_matches!(shortest_paths, Err(Error::NegativeWeight));
    }

    #[test]
    fn bellman_ford_basic() {
        let graph = create_basic_graph();
        let shortest_paths = ShortestPaths::on(&graph)
            .with(Algo::BellmanFord)
            .run(v(0))
            .unwrap();

        assert_eq!(shortest_paths.dist(v(4)), Some(&8));
        assert_eq!(
            shortest_paths.reconstruct(v(4)).collect::<Vec<_>>(),
            vec![v(3), v(1), v(0)]
        );

        assert_eq!(shortest_paths.dist(v(2)), Some(&2));
    }

    #[test]
    fn bellman_ford_negative_edge() {
        let mut graph = create_basic_graph();
        graph.replace_edge(&e(2), -1);

        let shortest_paths = ShortestPaths::on(&graph)
            .with(Algo::BellmanFord)
            .run(v(0))
            .unwrap();

        assert_eq!(shortest_paths.dist(v(4)), Some(&8));
        assert_eq!(
            shortest_paths.reconstruct(v(4)).collect::<Vec<_>>(),
            vec![v(3), v(1), v(0)]
        );

        assert_eq!(shortest_paths.dist(v(2)), Some(&2));
    }

    #[test]
    fn bellman_ford_negative_cycle() {
        let mut graph = AdjList::<(), i32, Directed, DefaultIndexing>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, 3);
        graph.add_edge(&v1, &v2, -2);
        graph.add_edge(&v2, &v3, 2);
        graph.add_edge(&v2, &v1, -2);
        graph.add_edge(&v2, &v4, 3);

        let shortest_paths = ShortestPaths::on(&graph).with(Algo::BellmanFord).run(v(0));

        assert_matches!(shortest_paths, Err(Error::NegativeCycle));
    }
}
