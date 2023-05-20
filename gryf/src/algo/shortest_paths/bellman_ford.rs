use crate::core::{
    index::{NumIndexType, Virtual},
    marker::EdgeType,
    weights::GetWeight,
    EdgeRef, Edges, VerticesBase, Weight,
};

use super::{Error, ShortestPaths};

pub fn bellman_ford<E, Ty: EdgeType, G, W, F>(
    graph: &G,
    start: G::VertexIndex,
    goal: Option<G::VertexIndex>,
    edge_weight: F,
) -> Result<ShortestPaths<W, G>, Error>
where
    G: VerticesBase + Edges<E, Ty>,
    G::VertexIndex: NumIndexType,
    W: Weight,
    F: GetWeight<E, W>,
{
    let vertex_map = graph.vertex_index_map();

    let mut dist = vec![W::inf(); vertex_map.len()];
    let mut pred = vec![Virtual::null(); vertex_map.len()];

    dist[vertex_map.virt(start).unwrap().to_usize()] = W::zero();

    let mut terminated_early = false;

    // Try to relax edges |V| - 1 times.
    for _ in 1..graph.vertex_count() {
        let mut relaxed = false;

        for edge in graph.edges() {
            let u = vertex_map.virt(*edge.src()).unwrap();
            let v = vertex_map.virt(*edge.dst()).unwrap();

            let edge_dist = edge_weight.get(edge.data());
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
    // the absence of cycle is guaranteed.
    if !terminated_early {
        for edge in graph.edges() {
            let u = vertex_map.virt(*edge.src()).unwrap();
            let v = vertex_map.virt(*edge.dst()).unwrap();

            let edge_dist = edge_weight.get(edge.data());

            if dist[u.to_usize()].clone() + edge_dist < dist[v.to_usize()] {
                return Err(Error::NegativeCycle);
            }
        }
    }

    if let Some(goal) = goal {
        if dist[vertex_map.virt(goal).unwrap().to_usize()] == W::inf() {
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
            if !p.is_null() {
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
