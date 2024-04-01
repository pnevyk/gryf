use crate::{
    adapt::transpose::TransposeRef,
    common::CompactIdMap,
    core::{
        id::{IdType, IntegerIdType, Virtual},
        marker::EdgeType,
        weights::GetWeight,
        EdgeRef, Edges, VerticesBase, Weight,
    },
};

use super::{Error, ShortestPaths};

pub fn bellman_ford<E, Ty: EdgeType, G, W, F>(
    graph: &G,
    start: G::VertexId,
    goal: Option<G::VertexId>,
    edge_weight: F,
) -> Result<ShortestPaths<W, G>, Error>
where
    G: VerticesBase + Edges<E, Ty>,
    G::VertexId: IntegerIdType,
    W: Weight,
    F: GetWeight<E, W>,
{
    let vertex_map = graph.vertex_id_map();

    let mut dist = vec![W::inf(); vertex_map.len()];
    let mut pred = vec![Virtual::sentinel(); vertex_map.len()];

    dist[vertex_map.virt(start).unwrap().as_usize()] = W::zero();

    let mut terminated_early = false;

    // Try to relax edges |V| - 1 times.
    for _ in 1..graph.vertex_count() {
        let mut relaxed = false;

        for edge in graph.edges() {
            if let Some((next_dist, u, v)) = process_edge(&edge, &edge_weight, &vertex_map, &dist) {
                // Relax if better.
                dist[v.as_usize()] = next_dist;
                pred[v.as_usize()] = u;
                relaxed = true;
            }

            if !Ty::is_directed() {
                // For undirected graph, we need to consider also the other
                // "direction" of the edge.
                if let Some((next_dist, u, v)) =
                    process_edge(&TransposeRef::new(edge), &edge_weight, &vertex_map, &dist)
                {
                    dist[v.as_usize()] = next_dist;
                    pred[v.as_usize()] = u;
                    relaxed = true;
                }
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
    // the absence of cycle is guaranteed.
    if !terminated_early {
        for edge in graph.edges() {
            if process_edge(&edge, &edge_weight, &vertex_map, &dist).is_some() {
                // Could be still relaxed, there is a negative cycle.
                return Err(Error::NegativeCycle);
            }

            if !Ty::is_directed()
                && process_edge(&TransposeRef::new(edge), &edge_weight, &vertex_map, &dist)
                    .is_some()
            {
                return Err(Error::NegativeCycle);
            }
        }
    }

    if let Some(goal) = goal {
        if dist[vertex_map.virt(goal).unwrap().as_usize()] == W::inf() {
            return Err(Error::GoalNotReached);
        }
    }

    let dist = dist
        .into_iter()
        .enumerate()
        .filter_map(|(i, d)| {
            if d != W::inf() {
                vertex_map.real(Virtual::from(i)).map(|u| (u, d))
            } else {
                None
            }
        })
        .collect();

    let pred = pred
        .into_iter()
        .enumerate()
        .filter_map(|(i, p)| {
            if !p.is_sentinel() {
                Some((
                    vertex_map.real(Virtual::from(i)).unwrap(),
                    vertex_map.real(p).unwrap(),
                ))
            } else {
                None
            }
        })
        .collect();

    Ok(ShortestPaths { start, dist, pred })
}

#[inline(always)]
fn process_edge<VId, EId, ER, E, W, F>(
    edge: &ER,
    edge_weight: &F,
    vertex_map: &CompactIdMap<VId>,
    dist: &[W],
) -> Option<(W, Virtual<VId>, Virtual<VId>)>
where
    VId: IntegerIdType,
    EId: IdType,
    ER: EdgeRef<VId, EId, E>,
    W: Weight,
    F: GetWeight<E, W>,
{
    let u = vertex_map.virt(*edge.src()).unwrap();

    let short_dist = &dist[u.as_usize()];
    if short_dist == &W::inf() {
        return None;
    }

    let v = vertex_map.virt(*edge.dst()).unwrap();

    let edge_dist = edge_weight.get(edge.attr());
    let next_dist = short_dist.clone() + edge_dist;

    if next_dist < dist[v.as_usize()] {
        Some((next_dist, u, v))
    } else {
        None
    }
}
