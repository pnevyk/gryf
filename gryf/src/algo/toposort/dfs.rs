use std::hash::BuildHasherDefault;

use rustc_hash::FxHashSet;

use crate::{
    adapt::Transpose,
    algo::Cycle,
    common::VisitSet,
    core::{id::UseVertexId, marker::Directed, Neighbors, VertexSet},
    visit::{
        raw::{RawDfsExtra, RawDfsExtraEvent, RawEvent, RawVisit, RawVisitMulti},
        VisitAll, Visitor,
    },
};

use super::Error;

pub fn dfs_visit<'a, G>(graph: &'a G) -> DfsVisit<'a, G>
where
    G: VertexSet + 'a,
{
    DfsVisit {
        raw: RawVisit::new(Some(graph.vertex_count())),
        multi: RawVisitMulti::new(VisitAll::new(graph)),
        closed: FxHashSet::with_capacity_and_hasher(
            graph.vertex_count(),
            BuildHasherDefault::default(),
        ),
        cycle: None,
    }
}

pub struct DfsVisit<'a, G>
where
    G: VertexSet + 'a,
{
    raw: RawVisit<G, UseVertexId, RawDfsExtra>,
    multi: RawVisitMulti<G, UseVertexId, RawDfsExtra, VisitAll<'a, G>>,
    closed: FxHashSet<G::VertexId>,
    cycle: Option<G::EdgeId>,
}

impl<'a, G> Visitor<G> for DfsVisit<'a, G>
where
    G: Neighbors<EdgeType = Directed> + VertexSet,
{
    type Item = Result<G::VertexId, Error<G>>;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        if self.cycle.is_some() {
            // We discovered a cycle in the previous iteration, but next vertex
            // was still requested.
            return None;
        }

        // The implementation differs from classic DFS algorithm for topological
        // sorting in order to achieve lazy behavior. The algorithm traverses
        // the graph in reverse DFS order and reports each vertex that is being
        // closed (i.e., when we reached vertex that has no (unreported)
        // predecessors). If a back edge is encountered, the cycle error is
        // reported instead.

        // Reverse the directions of the edges so the traversal actually works
        // in reverse direction.
        let graph = &Transpose::new(graph);

        loop {
            let raw_extra_event = self.multi.next_multi(
                &mut self.raw,
                |raw| {
                    raw.next(graph, |_, raw_event| match raw_event {
                        RawEvent::Skip { vertex, src } if !self.closed.is_visited(&vertex) => {
                            // Vertex not added to the stack, but also
                            // has not been closed. That means that we
                            // encountered a back edge.
                            self.cycle = src.map(|(_, edge)| edge);

                            // Prune to avoid unnecessary work.
                            false
                        }
                        _ => true,
                    })
                },
                |vertex| graph.contains_vertex(vertex),
            )?;

            if let Some(ref cycle) = self.cycle {
                return Some(Err(Error::Cycle(Cycle::new(cycle.clone(), false))));
            }

            if let RawDfsExtraEvent::Close(vertex) = raw_extra_event {
                self.closed.visit(vertex.clone());
                return Some(Ok(vertex));
            }
        }
    }
}
