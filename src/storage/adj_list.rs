use std::marker::PhantomData;
use std::mem;

use super::shared::AdjVertex as Vertex;
pub use super::shared::{AdjVerticesIter as VerticesIter, EdgesIter, RangeIndices};
use crate::index::{EdgeIndex, IndexType, VertexIndex};
use crate::infra::CompactIndexMap;
use crate::marker::{Direction, EdgeType};
use crate::traits::*;

#[derive(Debug)]
pub struct AdjList<V, E, Ty> {
    vertices: Vec<Vertex<V>>,
    edges: Vec<E>,
    endpoints: Vec<[VertexIndex; 2]>,
    ty: PhantomData<Ty>,
}

impl<V, E, Ty: EdgeType> AdjList<V, E, Ty> {
    pub fn new() -> Self {
        Self {
            vertices: Vec::new(),
            edges: Vec::new(),
            endpoints: Vec::new(),
            ty: PhantomData,
        }
    }

    fn remove_edge_inner(&mut self, index: EdgeIndex, cause: Option<VertexIndex>) -> Option<E> {
        let endpoints = self.endpoints.get(index.to_usize())?;

        for (i, dir) in Self::directions().iter().enumerate() {
            let endpoint = endpoints[i];

            // If this endpoint is not the vertex causing this removal, we need
            // to remove the edge from it. If is the cause, it is not necessary
            // to remove it.
            if Some(endpoint) != cause {
                Self::disconnect(
                    &mut self.vertices[endpoint.to_usize()].edges[dir.index()],
                    index,
                );
            }
        }

        // Remove the edge from the graph.
        let edge = self.edges.swap_remove(index.to_usize());
        self.endpoints.swap_remove(index.to_usize());

        // If `swap_remove` actually moved an existing edge somewhere, we need
        // to fix its index in the entire graph.
        if index.to_usize() < self.edges.len() {
            self.relocate_edge(self.edges.len().into(), index);
        }

        Some(edge)
    }

    fn relocate_vertex(&mut self, old_index: VertexIndex, new_index: VertexIndex) {
        let vertex = &mut self.vertices[new_index.to_usize()];

        // Fix the index of the vertex in all edges it has.
        for dir in Ty::directions() {
            for edge_index in vertex.edges[dir.index()].iter().copied() {
                let endpoints = &mut self.endpoints[edge_index.to_usize()];
                for i in 0..=1 {
                    if endpoints[i] == old_index {
                        endpoints[i] = new_index;
                    }
                }
            }
        }
    }

    fn relocate_edge(&mut self, old_index: EdgeIndex, new_index: EdgeIndex) {
        let endpoints = &mut self.endpoints[new_index.to_usize()];

        // Fix the index of the edge in all vertices it is incident with.
        for i in 0..=1 {
            let vertex = &mut self.vertices[endpoints[i].to_usize()];

            for dir in Ty::directions() {
                for edge_index in &mut vertex.edges[dir.index()] {
                    if *edge_index == old_index {
                        *edge_index = new_index;
                    }
                }
            }

            // If this is a self-loop, then all indices are fixed in the first
            // iteration.
            if endpoints[0] == endpoints[1] {
                break;
            }
        }
    }

    fn disconnect(edges: &mut Vec<EdgeIndex>, index: EdgeIndex) {
        for i in 0..edges.len() {
            if edges[i] == index {
                edges.swap_remove(i);
                break;
            }
        }
    }

    fn directions() -> [Direction; 2] {
        if Ty::is_directed() {
            [Direction::Outgoing, Direction::Incoming]
        } else {
            [Direction::Outgoing, Direction::Outgoing]
        }
    }
}

impl<V, E, Ty: EdgeType> Default for AdjList<V, E, Ty> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V, E, Ty: EdgeType> Vertices<V> for AdjList<V, E, Ty> {
    type VertexRef<'a, T: 'a> = (VertexIndex, &'a T);

    type VertexIndicesIter<'a, T: 'a>
    where
        V: 'a,
        E: 'a,
        Ty: 'a,
    = RangeIndices<VertexIndex>;

    type VerticesIter<'a, T: 'a>
    where
        V: 'a,
        E: 'a,
        Ty: 'a,
    = VerticesIter<'a, T>;

    fn vertex_count(&self) -> usize {
        self.vertices.len()
    }

    fn vertex_bound(&self) -> usize {
        self.vertex_count()
    }

    fn vertex(&self, index: VertexIndex) -> Option<&V> {
        self.vertices
            .get(index.to_usize())
            .map(|vertex| &vertex.data)
    }

    fn vertex_indices(&self) -> Self::VertexIndicesIter<'_, V> {
        (0..self.vertex_bound()).into()
    }

    fn vertices(&self) -> Self::VerticesIter<'_, V> {
        VerticesIter::new(self.vertices.iter())
    }

    fn vertex_index_map(&self) -> CompactIndexMap<VertexIndex> {
        CompactIndexMap::isomorphic(self.vertex_count())
    }
}

impl<V, E, Ty: EdgeType> VerticesMut<V> for AdjList<V, E, Ty> {
    fn vertex_mut(&mut self, index: VertexIndex) -> Option<&mut V> {
        self.vertices
            .get_mut(index.to_usize())
            .map(|vertex| &mut vertex.data)
    }

    fn add_vertex(&mut self, vertex: V) -> VertexIndex {
        let index = self.vertices.len();
        self.vertices.push(Vertex::new(vertex));
        index.into()
    }

    fn remove_vertex(&mut self, index: VertexIndex) -> Option<V> {
        for dir in Ty::directions() {
            // Remove all edges connected to this vertex in this direction.
            loop {
                let vertex = self.vertices.get_mut(index.to_usize())?;
                if vertex.edges[dir.index()].is_empty() {
                    break;
                }

                // Remove the edge from the list of this vertex.
                let edge_index = vertex.edges[dir.index()].swap_remove(0);
                // Remove the edge from the whole graph.
                self.remove_edge_inner(edge_index, Some(index));
            }
        }

        // Remove the vertex from the graph.
        let vertex = self.vertices.swap_remove(index.to_usize());

        // If `swap_remove` actually moved an existing vertex somewhere, we need
        // to fix its index in the entire graph.
        if index.to_usize() < self.vertices.len() {
            self.relocate_vertex(self.vertices.len().into(), index);
        }

        Some(vertex.data)
    }

    fn replace_vertex(&mut self, index: VertexIndex, vertex: V) -> V {
        let slot = self
            .vertices
            .get_mut(index.to_usize())
            .expect("vertex does not exist");

        mem::replace(&mut slot.data, vertex)
    }
}

impl<V, E, Ty: EdgeType> Edges<E, Ty> for AdjList<V, E, Ty> {
    type EdgeRef<'a, T: 'a> = (EdgeIndex, &'a T, VertexIndex, VertexIndex);

    type EdgeIndicesIter<'a, T: 'a>
    where
        V: 'a,
        E: 'a,
        Ty: 'a,
    = RangeIndices<EdgeIndex>;

    type EdgesIter<'a, T: 'a>
    where
        V: 'a,
        E: 'a,
        Ty: 'a,
    = EdgesIter<'a, T>;

    fn edge_count(&self) -> usize {
        self.edges.len()
    }

    fn edge_bound(&self) -> usize {
        self.edge_count()
    }

    fn edge(&self, index: EdgeIndex) -> Option<&E> {
        self.edges.get(index.to_usize())
    }

    fn endpoints(&self, index: EdgeIndex) -> Option<(VertexIndex, VertexIndex)> {
        self.endpoints
            .get(index.to_usize())
            .map(|endpoints| (endpoints[0], endpoints[1]))
    }

    fn edge_index(&self, src: VertexIndex, dst: VertexIndex) -> Option<EdgeIndex> {
        self.vertices
            .get(src.to_usize())
            .and_then(|src| {
                src.edges[Direction::Outgoing.index()]
                    .iter()
                    .find(|edge_index| {
                        let endpoints = &self.endpoints[edge_index.to_usize()];
                        endpoints[1] == dst || (!Ty::is_directed() && endpoints[0] == dst)
                    })
            })
            .copied()
    }

    fn edge_indices(&self) -> Self::EdgeIndicesIter<'_, E> {
        (0..self.edge_bound()).into()
    }

    fn edges(&self) -> Self::EdgesIter<'_, E> {
        EdgesIter::new(self.edges.iter(), self.endpoints.iter())
    }

    fn edge_index_map(&self) -> CompactIndexMap<EdgeIndex> {
        CompactIndexMap::isomorphic(self.edge_count())
    }
}

impl<V, E, Ty: EdgeType> EdgesMut<E, Ty> for AdjList<V, E, Ty> {
    fn edge_mut(&mut self, index: EdgeIndex) -> Option<&mut E> {
        self.edges.get_mut(index.to_usize())
    }

    fn add_edge(&mut self, src: VertexIndex, dst: VertexIndex, edge: E) -> EdgeIndex {
        assert!(
            src.to_usize() < self.vertices.len(),
            "src vertex does not exist"
        );
        assert!(
            dst.to_usize() < self.vertices.len(),
            "dst vertex does not exist"
        );

        let index = self.edges.len().into();
        self.edges.push(edge);
        self.endpoints.push([src, dst]);

        let directions = Self::directions();
        self.vertices[src.to_usize()].edges[directions[0].index()].push(index);
        self.vertices[dst.to_usize()].edges[directions[1].index()].push(index);

        index
    }

    fn remove_edge(&mut self, index: EdgeIndex) -> Option<E> {
        self.remove_edge_inner(index, None)
    }

    fn replace_edge(&mut self, index: EdgeIndex, edge: E) -> E {
        let slot = self
            .edges
            .get_mut(index.to_usize())
            .expect("edge does not exist");

        mem::replace(slot, edge)
    }
}

impl<V, E, Ty: EdgeType> MultiEdges<E, Ty> for AdjList<V, E, Ty> {
    type MultiEdgeIndicesIter<'a>
    where
        V: 'a,
        E: 'a,
        Ty: 'a,
    = MultiEdgeIndicesIter<'a>;

    fn multi_edge_index(
        &self,
        src: VertexIndex,
        dst: VertexIndex,
    ) -> Self::MultiEdgeIndicesIter<'_> {
        let vertex = self
            .vertices
            .get(src.to_usize())
            .expect("vertex does not exist");

        MultiEdgeIndicesIter {
            src,
            dst,
            edges: &vertex.edges[0],
            endpoints: self.endpoints.as_slice(),
        }
    }
}

impl<V, E, Ty: EdgeType> Neighbors for AdjList<V, E, Ty> {
    type NeighborRef<'a> = (VertexIndex, EdgeIndex, VertexIndex, Direction);

    type NeighborsIter<'a>
    where
        V: 'a,
        E: 'a,
        Ty: 'a,
    = NeighborsIter<'a>;

    fn neighbors(&self, src: VertexIndex) -> Self::NeighborsIter<'_> {
        let vertex = self
            .vertices
            .get(src.to_usize())
            .expect("vertex does not exist");

        NeighborsIter {
            src,
            edges: [&vertex.edges[0], &vertex.edges[1]],
            endpoints: self.endpoints.as_slice(),
            dir: 0,
        }
    }

    fn neighbors_directed(&self, src: VertexIndex, dir: Direction) -> Self::NeighborsIter<'_> {
        let vertex = self
            .vertices
            .get(src.to_usize())
            .expect("vertex does not exist");

        let adj_dir = if !Ty::is_directed() {
            // If the graph is undirected, then the direction does not matter.
            // However, we need to index the "outgoing" edge list in the vertex,
            // because the "incoming" list is empty.
            Direction::Outgoing
        } else {
            dir
        };

        let mut edges: [&[EdgeIndex]; 2] = [&[], &[]];
        edges[dir.index()] = &vertex.edges[adj_dir.index()];

        NeighborsIter {
            src,
            edges,
            endpoints: self.endpoints.as_slice(),
            dir: dir.index(),
        }
    }
}

impl<V, E, Ty: EdgeType> Create<V, E, Ty> for AdjList<V, E, Ty> {
    fn with_capacity(vertex_count: usize, edge_count: usize) -> Self {
        Self {
            vertices: Vec::with_capacity(vertex_count),
            edges: Vec::with_capacity(edge_count),
            endpoints: Vec::with_capacity(edge_count),
            ty: PhantomData,
        }
    }
}

pub struct MultiEdgeIndicesIter<'a> {
    src: VertexIndex,
    dst: VertexIndex,
    edges: &'a [EdgeIndex],
    endpoints: &'a [[VertexIndex; 2]],
}

impl<'a> Iterator for MultiEdgeIndicesIter<'a> {
    type Item = EdgeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (edge, tail) = self.edges.split_first()?;
            self.edges = tail;

            let endpoints = self.endpoints[edge.to_usize()];

            if endpoints[0] == self.src && endpoints[1] == self.dst {
                return Some(*edge);
            }
        }
    }
}

pub struct NeighborsIter<'a> {
    src: VertexIndex,
    edges: [&'a [EdgeIndex]; 2],
    endpoints: &'a [[VertexIndex; 2]],
    dir: usize,
}

impl Iterator for NeighborsIter<'_> {
    type Item = (VertexIndex, EdgeIndex, VertexIndex, Direction);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.dir == self.edges.len() {
                return None;
            }

            if self.edges[self.dir].is_empty() {
                self.dir += 1;
            } else {
                break;
            }
        }

        let (head, tail) = self.edges[self.dir].split_at(1);
        self.edges[self.dir] = tail;
        let edge = head[0];

        let endpoints = self.endpoints[edge.to_usize()];

        let neighbor = if endpoints[0] != self.src {
            endpoints[0]
        } else {
            endpoints[1]
        };

        let dir = Direction::from_index(self.dir);

        Some((neighbor, edge, self.src, dir))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::marker::{Directed, Undirected};
    use crate::storage::tests::*;

    #[test]
    fn basic_undirected() {
        test_basic::<Undirected, AdjList<_, _, _>>();
    }

    #[test]
    fn basic_directed() {
        test_basic::<Directed, AdjList<_, _, _>>();
    }

    #[test]
    fn multi_undirected() {
        test_multi::<Undirected, AdjList<_, _, _>>();
    }

    #[test]
    fn multi_directed() {
        test_multi::<Directed, AdjList<_, _, _>>();
    }
}
