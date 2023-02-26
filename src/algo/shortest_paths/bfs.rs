use std::{
    collections::{hash_map::Entry, HashSet, VecDeque},
    hash::BuildHasherDefault,
};

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    common::VisitSet,
    core::{marker::Direction, NeighborRef, Neighbors, VerticesBaseWeak, Weight},
};

use super::{Error, ShortestPaths};

pub fn bfs<G, W>(
    graph: &G,
    start: G::VertexIndex,
    goal: Option<G::VertexIndex>,
    edge_dist: W,
) -> Result<ShortestPaths<W, G>, Error>
where
    G: VerticesBaseWeak + Neighbors,
    W: Weight,
{
    // The check for unsignedness should eliminate the negativity weight check,
    // because the implementation of `is_unsigned` method is always a constant
    // boolean in practice.
    if !W::is_unsigned() && edge_dist < W::zero() {
        return Err(Error::NegativeWeight);
    }

    // FIXME: We want to use `visit::Bfs` here instead of reimplementing the
    // algorithm again. For that, we need more information than the current
    // implementation provides (e.g., the other vertex to the currently
    // processed vertex). This should be changed after the capabilities of the
    // visit module are extended.

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
    let mut queue = VecDeque::new();

    dist.insert(start.clone(), W::zero());
    queue.push_back((start.clone(), W::zero()));

    while let Some((vertex, vertex_dist)) = queue.pop_front() {
        if goal.as_ref() == Some(&vertex) {
            break;
        }

        for neighbor in graph.neighbors_directed(&vertex, Direction::Outgoing) {
            let next = neighbor.index();

            if visited.is_visited(&*next) {
                continue;
            }

            let next = next.into_owned();
            let next_dist = vertex_dist.clone() + edge_dist.clone();

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
                        queue.push_back((next.clone(), next_dist));
                        pred.insert(next, vertex.clone());
                    }
                }
                Entry::Vacant(slot) => {
                    slot.insert(next_dist.clone());
                    queue.push_back((next.clone(), next_dist));
                    pred.insert(next, vertex.clone());
                }
            }

            // The vertex is finished.
            visited.visit(vertex.clone());
        }
    }

    Ok(ShortestPaths { start, dist, pred })
}