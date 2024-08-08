use std::{
    collections::{hash_map::Entry, HashSet, VecDeque},
    hash::BuildHasherDefault,
};

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    core::{base::NeighborRef, marker::Direction, weight::Weight, Neighbors},
    visit::VisitSet,
};

use super::{Error, ShortestPaths};

pub fn bfs<G, W>(
    graph: &G,
    start: G::VertexId,
    goal: Option<G::VertexId>,
    edge_dist: W,
) -> Result<ShortestPaths<W, G>, Error>
where
    G: Neighbors,
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

    // Not using FixedBitSet with CompactIdMap because the algorithm supports
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
            // Mark as visited, because below is a test that checks that goal
            // was visited.
            visited.visit(vertex);
            break;
        }

        for neighbor in graph.neighbors_directed(&vertex, Direction::Outgoing) {
            let next = neighbor.id();

            if visited.is_visited(&*next) {
                continue;
            }

            let next = next.into_owned();
            let next_dist = vertex_dist.clone() + edge_dist.clone();

            if let Entry::Vacant(slot) = dist.entry(next.clone()) {
                slot.insert(next_dist.clone());
                pred.insert(next.clone(), vertex.clone());
            }

            queue.push_back((next, next_dist));
        }

        // The vertex is finished.
        visited.visit(vertex.clone());
    }

    if let Some(ref goal) = goal {
        if !visited.is_visited(goal) {
            return Err(Error::GoalNotReached);
        }
    }

    Ok(ShortestPaths { start, dist, pred })
}
