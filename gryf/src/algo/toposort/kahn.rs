use crate::{
    algo::Cycle,
    core::{
        GraphBase, Neighbors, VertexSet,
        base::NeighborReference,
        id::{CompactIdMap, IdType, IntegerIdType, Virtual},
        marker::Direction,
    },
};

use super::Error;

pub fn kahn<'a, G>(graph: &'a G) -> KahnIter<'a, G>
where
    G: Neighbors + VertexSet + 'a,
    G::VertexId: IntegerIdType,
{
    let map = graph.vertex_id_map();
    let mut in_deg = Vec::with_capacity(map.len());
    let mut queue = Vec::new();

    for v in graph.vertices_by_id() {
        let deg = graph.degree_directed(&v, Direction::Incoming);
        in_deg.push(deg);

        if deg == 0 {
            queue.push(v);
        }
    }

    KahnIter {
        graph,
        map,
        in_deg,
        queue,
        visited: 0,
        cycle: false,
    }
}

pub struct KahnIter<'a, G>
where
    G: GraphBase,
{
    graph: &'a G,
    map: CompactIdMap<G::VertexId>,
    in_deg: Vec<usize>,
    // Does not need to be FIFO as the order of reported vertices with in degree
    // 0 does not matter.
    queue: Vec<G::VertexId>,
    visited: usize,
    cycle: bool,
}

impl<'a, G> Iterator for KahnIter<'a, G>
where
    G: Neighbors,
    G::VertexId: IntegerIdType,
{
    type Item = Result<G::VertexId, Error<G>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(vertex) = self.queue.pop() {
            self.visited += 1;

            for n in self.graph.neighbors_directed(&vertex, Direction::Outgoing) {
                let i = self.map.to_virt(*n.id()).unwrap().as_usize();
                let deg = &mut self.in_deg[i];
                *deg -= 1;

                if *deg == 0 {
                    self.queue.push(*n.id());
                }
            }

            Some(Ok(vertex))
        } else if self.visited != self.map.len() {
            // `self.map.len()` corresponds to vertex count.

            if self.cycle {
                // We discovered a cycle in the previous iteration, but next
                // vertex was still requested.
                None
            } else {
                self.cycle = true;

                // Find a vertex that has a non-zero in degree. It must be part
                // of a cycle, otherwise its in degree would be reduced to zero.
                let v = self
                    .in_deg
                    .iter()
                    .copied()
                    .enumerate()
                    .find(|(_, deg)| *deg > 0)
                    .map(|(i, _)| self.map.to_real(Virtual::from_usize(i)).unwrap())
                    .unwrap();

                // Find an incoming edge to that vertex from a vertex that has
                // not been visited too.
                let edge = self
                    .graph
                    .neighbors_directed(&v, Direction::Incoming)
                    .find_map(|n| {
                        let i = self.map.to_virt(*n.id()).unwrap().as_usize();
                        if self.in_deg[i] > 0 {
                            Some(n.edge().into_owned())
                        } else {
                            None
                        }
                    })
                    .unwrap();

                Some(Err(Error::Cycle(Cycle::new(edge, false))))
            }
        } else {
            None
        }
    }
}
