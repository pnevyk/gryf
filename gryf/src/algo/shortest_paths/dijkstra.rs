use std::{
    cmp::Reverse,
    collections::{hash_map::Entry, BinaryHeap, HashSet},
    hash::BuildHasherDefault,
};

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    common::VisitSet,
    core::{
        marker::{Direction, EdgeType},
        weights::{GetWeight, Weighted},
        EdgesWeak, NeighborRef, Neighbors, VerticesBaseWeak, Weight,
    },
};

use super::{Error, ShortestPaths};

pub fn dijkstra<E, Ty: EdgeType, G, W, F>(
    graph: &G,
    start: G::VertexIndex,
    goal: Option<G::VertexIndex>,
    edge_weight: F,
) -> Result<ShortestPaths<W, G>, Error>
where
    G: VerticesBaseWeak + EdgesWeak<E, Ty> + Neighbors,
    W: Weight,
    F: GetWeight<E, W>,
{
    // Not using FixedBitSet with CompactIndexMap because the algorithm supports
    // early termination when reaching given goal. It is likely that reaching
    // goal means visiting a subgraph which is significantly smaller than the
    // original graph.
    let mut visited: FxHashSet<_> = HashSet::with_capacity_and_hasher(
        graph.vertex_count_hint().unwrap_or(32),
        BuildHasherDefault::default(),
    );

    let mut dist = FxHashMap::default();
    let mut pred = FxHashMap::default();
    let mut queue = BinaryHeap::new();

    dist.insert(start.clone(), W::zero());
    queue.push(Reverse(Weighted(start.clone(), W::Ord::from(W::zero()))));

    while let Some(Reverse(Weighted(vertex, vertex_dist))) = queue.pop() {
        let vertex_dist = vertex_dist.into();

        // This can happen due to duplication of vertices when doing relaxation
        // in our implementation.
        if visited.is_visited(&vertex) {
            continue;
        }

        if goal.as_ref() == Some(&vertex) {
            break;
        }

        for neighbor in graph.neighbors_directed(&vertex, Direction::Outgoing) {
            let next = neighbor.index();

            if visited.is_visited(&next) {
                continue;
            }

            let next = next.into_owned();

            let edge_dist = edge_weight
                .get_const()
                .or_else(|| {
                    graph
                        .edge_weak(&neighbor.edge())
                        .map(|edge| edge_weight.get(&edge))
                })
                .ok_or(Error::EdgeNotAvailable)?;

            // The check for unsignedness should eliminate the negativity weight
            // check, because the implementation of `is_unsigned` method is
            // always a constant boolean in practice.
            if !W::is_unsigned() && edge_dist < W::zero() {
                return Err(Error::NegativeWeight);
            }

            let next_dist = vertex_dist.clone() + edge_dist;

            match dist.entry(next.clone()) {
                Entry::Occupied(curr_dist) => {
                    // Relaxation operation. If the distance is better than what
                    // we had so far, update it.
                    if next_dist < *curr_dist.get() {
                        *curr_dist.into_mut() = next_dist.clone();
                        // A textbook version of the algorithm would update the
                        // priority of `next`. Adding it as a new item causes
                        // duplicities which is unfortunate for dense graphs,
                        // but should be fine in practice.
                        queue.push(Reverse(Weighted(next.clone(), next_dist.into())));
                        pred.insert(next, vertex.clone());
                    }
                }
                Entry::Vacant(slot) => {
                    slot.insert(next_dist.clone());
                    queue.push(Reverse(Weighted(next.clone(), next_dist.into())));
                    pred.insert(next, vertex.clone());
                }
            }

            // The vertex is finished.
            visited.visit(vertex.clone());
        }
    }

    Ok(ShortestPaths { start, dist, pred })
}
