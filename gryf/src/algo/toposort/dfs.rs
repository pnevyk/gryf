use std::hash::BuildHasherDefault;

use rustc_hash::FxHashSet;

use crate::{
    common::VisitSet,
    core::{index::UseVertexIndex, marker::Directed, EdgesBase, Neighbors, VerticesBase},
    ops::Transpose,
    visit::{
        raw::{RawDfsExtra, RawDfsExtraEvent, RawEvent, RawVisit, RawVisitMulti},
        VisitAll, Visitor,
    },
};

use super::Error;

pub fn dfs_visit<'a, G>(graph: &'a G) -> DfsVisit<'a, G>
where
    G: VerticesBase + 'a,
{
    DfsVisit {
        raw: RawVisit::new(Some(graph.vertex_count())),
        multi: RawVisitMulti::new(VisitAll::new(graph)),
        closed: FxHashSet::with_capacity_and_hasher(
            graph.vertex_count(),
            BuildHasherDefault::default(),
        ),
        cycle: false,
    }
}

pub struct DfsVisit<'a, G>
where
    G: VerticesBase + 'a,
{
    raw: RawVisit<G, UseVertexIndex, RawDfsExtra>,
    multi: RawVisitMulti<G, UseVertexIndex, RawDfsExtra, VisitAll<'a, G>>,
    closed: FxHashSet<G::VertexIndex>,
    cycle: bool,
}

impl<'a, G> Visitor<G> for DfsVisit<'a, G>
where
    G: Neighbors + VerticesBase + EdgesBase<Directed>,
{
    type Item = Result<G::VertexIndex, Error>;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        if self.cycle {
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
                        RawEvent::Skip { vertex, .. } if !self.closed.is_visited(&vertex) => {
                            // Vertex not added to the stack, but also
                            // has not been closed. That means that we
                            // encountered a back edge.
                            self.cycle = true;

                            // Prune to avoid unnecessary work.
                            false
                        }
                        _ => true,
                    })
                },
                |vertex| graph.contains_vertex(vertex),
            )?;

            if self.cycle {
                return Some(Err(Error::Cycle));
            }

            if let RawDfsExtraEvent::Close(vertex) = raw_extra_event {
                self.closed.visit(vertex.clone());
                return Some(Ok(vertex));
            }
        }
    }
}
