use std::cmp::{max, Reverse};
use std::collections::{hash_map::Entry, BinaryHeap, HashSet};
use std::hash::BuildHasherDefault;

use rustc_hash::{FxHashMap, FxHashSet, FxHasher};

use crate::index::{EdgeIndex, IndexType, VertexIndex, Virtual};
use crate::infra::VisitSet;
use crate::marker::{EdgeType, Outgoing};
use crate::traits::*;
use crate::weight::Weighted;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum Algo {
    Dijkstra,
    BellmanFord,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    NegativeWeight,
    NegativeCycle,
}

#[derive(Debug)]
pub struct ShortestPaths<W> {
    start: VertexIndex,
    // Using HashMaps because the algorithm supports early termination when
    // reaching given goal. It is likely that reaching goal means visiting a
    // subgraph which is significantly smaller than the original graph.
    dist: FxHashMap<VertexIndex, W>,
    pred: FxHashMap<VertexIndex, VertexIndex>,
}

impl<W> ShortestPaths<W>
where
    W: Weight,
{
    pub fn run_algo<V, E, Ty: EdgeType, G, F>(
        graph: &G,
        start: VertexIndex,
        goal: Option<VertexIndex>,
        edge_weight: F,
        algo: Option<Algo>,
    ) -> Result<Self, Error>
    where
        G: Vertices<V>
            + Edges<E, Ty>
            + VerticesWeak<V>
            + EdgesWeak<E, Ty, EdgeIndex = EdgeIndex>
            + Neighbors,
        F: Fn(&E) -> W,
    {
        let algo = algo.unwrap_or_else(|| {
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
        });

        match algo {
            Algo::Dijkstra => dijkstra(graph, start, goal, edge_weight),
            Algo::BellmanFord => bellman_ford(graph, start, edge_weight),
        }
    }

    pub fn run<V, E, Ty: EdgeType, G, F>(
        graph: &G,
        start: VertexIndex,
        goal: Option<VertexIndex>,
        edge_weight: F,
    ) -> Result<Self, Error>
    where
        G: Vertices<V>
            + Edges<E, Ty>
            + VerticesWeak<V>
            + EdgesWeak<E, Ty, EdgeIndex = EdgeIndex>
            + Neighbors,
        F: Fn(&E) -> W,
    {
        Self::run_algo(graph, start, goal, edge_weight, None)
    }

    pub fn run_dijkstra<V, E, Ty: EdgeType, G, F>(
        graph: &G,
        start: VertexIndex,
        goal: Option<VertexIndex>,
        edge_weight: F,
    ) -> Result<Self, Error>
    where
        G: VerticesWeak<V> + EdgesWeak<E, Ty, EdgeIndex = EdgeIndex> + Neighbors,
        F: Fn(&E) -> W,
    {
        dijkstra(graph, start, goal, edge_weight)
    }

    pub fn run_bellman_ford<V, E, Ty: EdgeType, G, F>(
        graph: &G,
        start: VertexIndex,
        edge_weight: F,
    ) -> Result<Self, Error>
    where
        G: Vertices<V> + Edges<E, Ty>,
        F: Fn(&E) -> W,
    {
        bellman_ford(graph, start, edge_weight)
    }

    pub fn start(&self) -> VertexIndex {
        self.start
    }

    pub fn dist(&self, from: VertexIndex) -> Option<&W> {
        self.dist.get(&from)
    }

    pub fn reconstruct(&self, from: VertexIndex) -> PathReconstruction<'_> {
        PathReconstruction {
            curr: from,
            pred: &self.pred,
        }
    }
}

pub struct PathReconstruction<'a> {
    curr: VertexIndex,
    pred: &'a FxHashMap<VertexIndex, VertexIndex>,
}

impl<'a> Iterator for PathReconstruction<'a> {
    type Item = VertexIndex;

    fn next(&mut self) -> Option<Self::Item> {
        self.curr = self.pred.get(&self.curr).copied()?;
        Some(self.curr)
    }
}

pub fn identity<E: Clone>(edge: &E) -> E {
    edge.clone()
}

pub fn unit<E>(_edge: &E) -> usize {
    1
}

fn dijkstra<V, E, Ty: EdgeType, G, W, F>(
    graph: &G,
    start: VertexIndex,
    goal: Option<VertexIndex>,
    edge_weight: F,
) -> Result<ShortestPaths<W>, Error>
where
    G: VerticesWeak<V> + EdgesWeak<E, Ty, EdgeIndex = EdgeIndex> + Neighbors,
    W: Weight,
    F: Fn(&E) -> W,
{
    // Not using FixedBitSet with CompactIndexMap because the algorithm supports
    // early termination when reaching given goal. It is likely that reaching
    // goal means visiting a subgraph which is significantly smaller than the
    // original graph.
    let mut visited: FxHashSet<_> = HashSet::with_capacity_and_hasher(
        graph.vertex_count_hint().unwrap_or(32),
        BuildHasherDefault::<FxHasher>::default(),
    );

    let mut dist = FxHashMap::default();
    let mut pred = FxHashMap::default();
    let mut queue = BinaryHeap::new();

    dist.insert(start, W::zero());
    queue.push(Reverse(Weighted(start, W::zero())));

    while let Some(Reverse(Weighted(vertex, vertex_dist))) = queue.pop() {
        // This can happen due to duplication of vertices when doing relaxation
        // in our implementation.
        if visited.is_visited(vertex) {
            continue;
        }

        if goal.as_ref() == Some(&vertex) {
            break;
        }

        for neighbor in graph.neighbors_directed(vertex, Outgoing) {
            let edge = graph.edge_weak(neighbor.edge()).unwrap();
            let next = neighbor.index();

            if visited.is_visited(next) {
                continue;
            }

            let edge_dist = edge_weight(edge.as_ref());

            // The check for unsignedness should eliminate the negativity weight
            // check, because the implementation of `is_unsigned` method is
            // always a constant boolean in practice.
            if !W::is_unsigned() && edge_dist < W::zero() {
                return Err(Error::NegativeWeight);
            }

            let next_dist = vertex_dist.clone() + edge_dist;

            match dist.entry(next) {
                Entry::Occupied(curr_dist) => {
                    // Relaxation operation. If the distance is better than what
                    // we had so far, update it.
                    if next_dist < *curr_dist.get() {
                        *curr_dist.into_mut() = next_dist.clone();
                        // A textbook version of the algorithm would update the
                        // priority of `next`. Adding it as a new item causes
                        // duplicities which is unfortunate for dense graphs,
                        // but should be fine in practice.
                        queue.push(Reverse(Weighted(next, next_dist)));
                        pred.insert(next, vertex);
                    }
                }
                Entry::Vacant(slot) => {
                    slot.insert(next_dist.clone());
                    queue.push(Reverse(Weighted(next, next_dist)));
                    pred.insert(next, vertex);
                }
            }

            // The vertex is finished.
            visited.visit(vertex);
        }
    }

    Ok(ShortestPaths { start, dist, pred })
}

fn bellman_ford<V, E, Ty: EdgeType, G, W, F>(
    graph: &G,
    start: VertexIndex,
    edge_weight: F,
) -> Result<ShortestPaths<W>, Error>
where
    G: Vertices<V> + Edges<E, Ty>,
    W: Weight,
    F: Fn(&E) -> W,
{
    let vertex_map = graph.vertex_index_map();

    let mut dist = vec![W::inf(); vertex_map.len()];
    let mut pred = vec![Virtual::null(); vertex_map.len()];

    dist[vertex_map.virt(start).to_usize()] = W::zero();

    let mut terminated_early = false;

    // Try to relax edges |V| - 1 times.
    for _ in 1..graph.vertex_count() {
        let mut relaxed = false;

        for edge in graph.edges() {
            let u = vertex_map.virt(edge.src());
            let v = vertex_map.virt(edge.dst());

            let edge_dist = edge_weight(edge.data());
            let next_dist = dist[u.to_usize()].clone() + edge_dist;

            // Relax if better.
            if next_dist < dist[v.to_usize()] {
                dist[v.to_usize()] = next_dist;
                pred[v.to_usize()] = u;
                relaxed = true;
            }
        }

        // If no distance was improved, then subsequent iterations would not
        // improve as well. So we can terminate early.
        if !relaxed {
            terminated_early = true;
            break;
        }
    }

    // Check for negative cycles. If the main loop was terminated early, then
    // the absence of cycle if guaranteed.
    if !terminated_early {
        for edge in graph.edges() {
            let u = vertex_map.virt(edge.src());
            let v = vertex_map.virt(edge.dst());

            let edge_dist = edge_weight(edge.data());

            if dist[u.to_usize()].clone() + edge_dist < dist[v.to_usize()] {
                return Err(Error::NegativeCycle);
            }
        }
    }

    let dist = dist
        .into_iter()
        .enumerate()
        .filter_map(|(i, d)| {
            if d != W::inf() {
                Some((vertex_map.real(Virtual::new(i)), d))
            } else {
                None
            }
        })
        .collect();

    let pred = pred
        .into_iter()
        .enumerate()
        .filter_map(|(i, p)| {
            if !p.is_null() {
                Some((vertex_map.real(Virtual::new(i)), vertex_map.real(p)))
            } else {
                None
            }
        })
        .collect();

    Ok(ShortestPaths { start, dist, pred })
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use super::*;
    use crate::marker::{Directed, Undirected};
    use crate::storage::AdjList;

    fn create_basic_graph() -> AdjList<(), i32, Undirected> {
        let mut graph = AdjList::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(v0, v1, 3);
        graph.add_edge(v0, v2, 2);
        graph.add_edge(v1, v2, 2);
        graph.add_edge(v1, v3, 2);
        graph.add_edge(v1, v4, 7);
        graph.add_edge(v2, v3, 5);
        graph.add_edge(v3, v4, 3);
        graph.add_edge(v4, v5, 10);

        graph
    }

    #[test]
    fn dijkstra_basic() {
        let graph = create_basic_graph();
        let shortest_paths = ShortestPaths::run_dijkstra(&graph, 0.into(), None, identity).unwrap();

        assert_eq!(shortest_paths.dist(4.into()), Some(&8));
        assert_eq!(
            shortest_paths.reconstruct(4.into()).collect::<Vec<_>>(),
            vec![3.into(), 1.into(), 0.into()]
        );

        assert_eq!(shortest_paths.dist(2.into()), Some(&2));
    }

    #[test]
    fn dijkstra_early_termination() {
        let graph = create_basic_graph();
        let shortest_paths =
            ShortestPaths::run_dijkstra(&graph, 0.into(), Some(4.into()), identity).unwrap();

        assert!(shortest_paths.dist(5.into()).is_none());
    }

    #[test]
    fn dijkstra_negative_edge() {
        let mut graph = create_basic_graph();
        graph.replace_edge(2.into(), -1);

        let shortest_paths =
            ShortestPaths::run_dijkstra(&graph, 0.into(), Some(4.into()), identity);

        assert_matches!(shortest_paths, Err(Error::NegativeWeight));
    }

    #[test]
    fn bellman_ford_basic() {
        let graph = create_basic_graph();
        let shortest_paths = ShortestPaths::run_bellman_ford(&graph, 0.into(), identity).unwrap();

        assert_eq!(shortest_paths.dist(4.into()), Some(&8));
        assert_eq!(
            shortest_paths.reconstruct(4.into()).collect::<Vec<_>>(),
            vec![3.into(), 1.into(), 0.into()]
        );

        assert_eq!(shortest_paths.dist(2.into()), Some(&2));
    }

    #[test]
    fn bellman_ford_negative_edge() {
        let mut graph = create_basic_graph();
        graph.replace_edge(2.into(), -1);

        let shortest_paths = ShortestPaths::run_bellman_ford(&graph, 0.into(), identity).unwrap();

        assert_eq!(shortest_paths.dist(4.into()), Some(&8));
        assert_eq!(
            shortest_paths.reconstruct(4.into()).collect::<Vec<_>>(),
            vec![3.into(), 1.into(), 0.into()]
        );

        assert_eq!(shortest_paths.dist(2.into()), Some(&2));
    }

    #[test]
    fn bellman_ford_negative_cycle() {
        let mut graph = AdjList::<(), i32, Directed>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());

        graph.add_edge(v0, v1, 3);
        graph.add_edge(v1, v2, -2);
        graph.add_edge(v2, v3, 2);
        graph.add_edge(v2, v1, -2);
        graph.add_edge(v2, v4, 3);

        let shortest_paths = ShortestPaths::run_bellman_ford(&graph, 0.into(), identity);

        assert_matches!(shortest_paths, Err(Error::NegativeCycle));
    }
}
