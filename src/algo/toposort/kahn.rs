use crate::{index::NumIndexType, infra::CompactIndexMap, marker::Direction, traits::*};

use super::Error;

pub fn kahn<'a, G>(graph: &'a G) -> KahnIter<'a, G>
where
    G: Neighbors + VerticesBase + 'a,
    G::VertexIndex: NumIndexType,
{
    let map = graph.vertex_index_map();
    let mut in_deg = Vec::with_capacity(map.len());
    let mut queue = Vec::new();

    for v in graph.vertex_indices() {
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
    }
}

pub struct KahnIter<'a, G>
where
    G: GraphBase,
{
    graph: &'a G,
    map: CompactIndexMap<G::VertexIndex>,
    in_deg: Vec<usize>,
    // Does not need to be FIFO as the order of reported vertices with in degree
    // 0 does not matter.
    queue: Vec<G::VertexIndex>,
    visited: usize,
}

impl<'a, G> Iterator for KahnIter<'a, G>
where
    G: Neighbors,
    G::VertexIndex: NumIndexType,
{
    type Item = Result<G::VertexIndex, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(vertex) = self.queue.pop() {
            self.visited += 1;

            for n in self.graph.neighbors_directed(&vertex, Direction::Outgoing) {
                let i = self.map.virt(*n.index()).unwrap().to_usize();
                let deg = &mut self.in_deg[i];
                *deg -= 1;

                if *deg == 0 {
                    self.queue.push(*n.index());
                }
            }

            Some(Ok(vertex))
        } else if self.visited != self.map.len() {
            // `self.map.len()` corresponds to vertex count.
            Some(Err(Error::Cycle))
        } else {
            None
        }
    }
}
