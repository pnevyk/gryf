use std::collections::VecDeque;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    adapt::Undirect,
    common::VisitSet,
    core::{
        index::IndexType,
        marker::{Direction, EdgeType},
        EdgesBase, EdgesBaseWeak, GraphBase, NeighborRef, Neighbors, VerticesBase,
        VerticesBaseWeak,
    },
};

use super::Cycle;

pub fn bfs_collect<Ty: EdgeType, G>(
    graph: &G,
    cycle: Cycle<G>,
    as_undirected: bool,
) -> Vec<G::EdgeIndex>
where
    G: Neighbors + VerticesBase + VerticesBaseWeak + EdgesBase<Ty> + EdgesBaseWeak<Ty>,
{
    if as_undirected {
        collect(&Undirect::new(graph), cycle.edge)
    } else {
        collect(graph, cycle.edge)
    }
}

fn collect<EI, Ty: EdgeType, G>(graph: &G, edge: EI) -> Vec<EI>
where
    G: Neighbors + VerticesBase + VerticesBaseWeak + EdgesBase<Ty> + GraphBase<EdgeIndex = EI>,
    EI: IndexType,
{
    let (u, v) = match graph.endpoints(&edge) {
        Some(endpoints) => endpoints,
        None => return vec![edge],
    };

    let mut visited = FxHashSet::default();

    let mut pred = FxHashMap::default();
    let mut queue = VecDeque::new();

    queue.push_back(v);

    while let Some(vertex) = queue.pop_front() {
        if vertex == u {
            let mut current = vertex;
            let iter = std::iter::from_fn(|| {
                let (w, e) = pred.get(&current).cloned()?;
                current = w;
                Some(e)
            });

            return std::iter::once(edge).chain(iter).collect();
        }

        for n in graph.neighbors_directed(&vertex, Direction::Outgoing) {
            // Ignore given edge so that another path to the other vertex is
            // found by the algorithm.
            if n.edge().as_ref() == &edge {
                continue;
            }

            let next = n.index();

            if visited.is_visited(&*next) {
                continue;
            }

            let next = next.into_owned();

            pred.insert(next.clone(), (vertex.clone(), n.edge().into_owned()));
            queue.push_back(next);
        }

        visited.visit(vertex.clone());
    }

    // Cycle not found.
    vec![edge]
}
