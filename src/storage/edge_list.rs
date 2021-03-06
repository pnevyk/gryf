use std::iter::Enumerate;
use std::marker::PhantomData;
use std::slice;

use super::shared::{EdgesIter, RangeIndices, VerticesIter};
use crate::index::{EdgeIndex, IndexType, VertexIndex};
use crate::infra::CompactIndexMap;
use crate::marker::{Direction, EdgeType};
use crate::traits::*;
use crate::{EdgesBaseWeak, EdgesWeak, VerticesBaseWeak, VerticesWeak};

#[derive(Debug, VerticesBaseWeak, VerticesWeak, EdgesBaseWeak, EdgesWeak)]
pub struct EdgeList<V, E, Ty> {
    vertices: Vec<V>,
    edges: Vec<E>,
    endpoints: Vec<[VertexIndex; 2]>,
    ty: PhantomData<Ty>,
}

impl<V, E, Ty: EdgeType> EdgeList<V, E, Ty> {
    pub fn new() -> Self {
        Self {
            vertices: Vec::new(),
            edges: Vec::new(),
            endpoints: Vec::new(),
            ty: PhantomData,
        }
    }

    fn relocate_vertex(&mut self, old_index: VertexIndex, new_index: VertexIndex) {
        self.endpoints.iter_mut().for_each(|endpoints| {
            for endpoint in endpoints.iter_mut() {
                if *endpoint == old_index {
                    *endpoint = new_index;
                }
            }
        })
    }
}

impl<V, E, Ty: EdgeType> Default for EdgeList<V, E, Ty> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V, E, Ty: EdgeType> VerticesBase for EdgeList<V, E, Ty> {
    type VertexIndicesIter<'a> = RangeIndices<VertexIndex>
    where
        Self: 'a;

    fn vertex_count(&self) -> usize {
        self.vertices.len()
    }

    fn vertex_bound(&self) -> usize {
        self.vertex_count()
    }

    fn vertex_indices(&self) -> Self::VertexIndicesIter<'_> {
        (0..self.vertex_bound()).into()
    }

    fn vertex_index_map(&self) -> CompactIndexMap<VertexIndex> {
        CompactIndexMap::isomorphic(self.vertex_count())
    }
}

impl<V, E, Ty: EdgeType> Vertices<V> for EdgeList<V, E, Ty> {
    type VertexRef<'a, T: 'a> = (VertexIndex, &'a T)
    where
        Self: 'a;

    type VerticesIter<'a, T: 'a> = VerticesIter<'a, T>
    where
        Self: 'a;

    fn vertex(&self, index: VertexIndex) -> Option<&V> {
        self.vertices.get(index.to_usize())
    }

    fn vertices(&self) -> Self::VerticesIter<'_, V> {
        VerticesIter::new(self.vertices.iter())
    }
}

impl<V, E, Ty: EdgeType> VerticesMut<V> for EdgeList<V, E, Ty> {
    fn vertex_mut(&mut self, index: VertexIndex) -> Option<&mut V> {
        self.vertices.get_mut(index.to_usize())
    }

    fn add_vertex(&mut self, vertex: V) -> VertexIndex {
        let index = self.vertices.len();
        self.vertices.push(vertex);
        index.into()
    }

    fn remove_vertex(&mut self, index: VertexIndex) -> Option<V> {
        // Remove all edges connected to this vertex in any direction.
        let mut i = 0;
        while i < self.endpoints.len() {
            let endpoints = &self.endpoints[i];
            if endpoints[0] == index || endpoints[1] == index {
                self.edges.swap_remove(i);
                self.endpoints.swap_remove(i);
            } else {
                i += 1
            }
        }

        // Remove the vertex from the graph.
        let vertex = self.vertices.swap_remove(index.to_usize());

        // If `swap_remove` actually moved an existing vertex somewhere, we need
        // to fix its index in the entire graph.
        if index.to_usize() < self.vertices.len() {
            self.relocate_vertex(self.vertices.len().into(), index);
        }

        Some(vertex)
    }

    fn clear(&mut self) {
        self.vertices.clear();
        self.edges.clear();
        self.endpoints.clear();
    }
}

impl<V, E, Ty: EdgeType> EdgesBase<Ty> for EdgeList<V, E, Ty> {
    type EdgeIndicesIter<'a> = RangeIndices<EdgeIndex>
    where
        Self: 'a;

    fn edge_count(&self) -> usize {
        self.edges.len()
    }

    fn edge_bound(&self) -> usize {
        self.edge_count()
    }

    fn endpoints(&self, index: EdgeIndex) -> Option<(VertexIndex, VertexIndex)> {
        self.endpoints
            .get(index.to_usize())
            .map(|endpoints| (endpoints[0], endpoints[1]))
    }

    fn edge_index(&self, src: VertexIndex, dst: VertexIndex) -> Option<EdgeIndex> {
        self.endpoints
            .iter()
            .enumerate()
            .find_map(|(i, endpoints)| {
                #[allow(clippy::if_same_then_else)]
                if endpoints[0] == src && endpoints[1] == dst {
                    Some(i.into())
                } else if !Ty::is_directed() && endpoints[1] == src && endpoints[0] == dst {
                    Some(i.into())
                } else {
                    None
                }
            })
    }

    fn edge_indices(&self) -> Self::EdgeIndicesIter<'_> {
        (0..self.edge_bound()).into()
    }

    fn edge_index_map(&self) -> CompactIndexMap<EdgeIndex> {
        CompactIndexMap::isomorphic(self.edge_count())
    }
}

impl<V, E, Ty: EdgeType> Edges<E, Ty> for EdgeList<V, E, Ty> {
    type EdgeRef<'a, T: 'a> = (EdgeIndex, &'a T, VertexIndex, VertexIndex)
    where
        Self: 'a;

    type EdgesIter<'a, T: 'a> = EdgesIter<'a, T>
    where
        Self: 'a;

    fn edge(&self, index: EdgeIndex) -> Option<&E> {
        self.edges.get(index.to_usize())
    }

    fn edges(&self) -> Self::EdgesIter<'_, E> {
        EdgesIter::new(self.edges.iter(), self.endpoints.iter())
    }
}

impl<V, E, Ty: EdgeType> EdgesMut<E, Ty> for EdgeList<V, E, Ty> {
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

        self.endpoints.push([src, dst]);
        let index = self.edges.len();
        self.edges.push(edge);
        index.into()
    }

    fn remove_edge(&mut self, index: EdgeIndex) -> Option<E> {
        self.edge(index)?;
        self.endpoints.swap_remove(index.to_usize());
        Some(self.edges.swap_remove(index.to_usize()))
    }

    fn clear_edges(&mut self) {
        self.edges.clear();
        self.endpoints.clear();
    }
}

impl<V, E, Ty: EdgeType> MultiEdges<E, Ty> for EdgeList<V, E, Ty> {
    type MultiEdgeIndicesIter<'a> = MultiEdgeIndicesIter<'a, Ty>
    where
        Self: 'a;

    fn multi_edge_index(
        &self,
        src: VertexIndex,
        dst: VertexIndex,
    ) -> Self::MultiEdgeIndicesIter<'_> {
        self.vertex(src).expect("vertex does not exist");

        MultiEdgeIndicesIter {
            src,
            dst,
            endpoints: self.endpoints.iter().enumerate(),
            ty: PhantomData,
        }
    }
}

impl<V, E, Ty: EdgeType> Neighbors for EdgeList<V, E, Ty> {
    type NeighborRef<'a> = (VertexIndex, EdgeIndex, VertexIndex, Direction)
    where
        Self: 'a;

    type NeighborsIter<'a> = NeighborsIter<'a>
    where
        Self: 'a;

    fn neighbors(&self, src: VertexIndex) -> Self::NeighborsIter<'_> {
        self.vertex(src).expect("vertex does not exist");

        NeighborsIter {
            src,
            edges: self.endpoints.as_slice(),
            dir: None,
            index: 0,
            is_directed: Ty::is_directed(),
        }
    }

    fn neighbors_directed(&self, src: VertexIndex, dir: Direction) -> Self::NeighborsIter<'_> {
        self.vertex(src).expect("vertex does not exist");

        NeighborsIter {
            src,
            edges: self.endpoints.as_slice(),
            dir: Some(dir),
            index: 0,
            is_directed: Ty::is_directed(),
        }
    }
}

impl<V, E, Ty: EdgeType> Create<V, E, Ty> for EdgeList<V, E, Ty> {
    fn with_capacity(vertex_count: usize, edge_count: usize) -> Self {
        Self {
            vertices: Vec::with_capacity(vertex_count),
            edges: Vec::with_capacity(edge_count),
            endpoints: Vec::with_capacity(edge_count),
            ty: PhantomData,
        }
    }
}

impl<V, E, Ty: EdgeType> Guarantee for EdgeList<V, E, Ty> {}

pub struct MultiEdgeIndicesIter<'a, Ty: EdgeType> {
    src: VertexIndex,
    dst: VertexIndex,
    endpoints: Enumerate<slice::Iter<'a, [VertexIndex; 2]>>,
    ty: PhantomData<Ty>,
}

impl<'a, Ty: EdgeType> Iterator for MultiEdgeIndicesIter<'a, Ty> {
    type Item = EdgeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        for (index, endpoints) in self.endpoints.by_ref() {
            if endpoints[0] == self.src && endpoints[1] == self.dst {
                return Some(index.into());
            }
        }

        None
    }
}

pub struct NeighborsIter<'a> {
    src: VertexIndex,
    edges: &'a [[VertexIndex; 2]],
    index: usize,
    dir: Option<Direction>,
    is_directed: bool,
}

impl<'a> Iterator for NeighborsIter<'a> {
    type Item = (VertexIndex, EdgeIndex, VertexIndex, Direction);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (endpoints, tail) = self.edges.split_first()?;
            self.edges = tail;

            let index = EdgeIndex::new(self.index);
            self.index += 1;

            let neighbor = match (self.dir, self.is_directed) {
                (Some(Direction::Outgoing), true) => {
                    if endpoints[0] == self.src {
                        Some((endpoints[1], Direction::Outgoing))
                    } else {
                        None
                    }
                }
                (Some(Direction::Incoming), true) => {
                    if endpoints[1] == self.src {
                        Some((endpoints[0], Direction::Incoming))
                    } else {
                        None
                    }
                }
                (Some(dir), false) => {
                    if endpoints[0] == self.src {
                        Some((endpoints[1], dir))
                    } else if endpoints[1] == self.src {
                        Some((endpoints[0], dir))
                    } else {
                        None
                    }
                }
                (None, _) => {
                    if endpoints[0] == self.src {
                        Some((endpoints[1], Direction::Outgoing))
                    } else if endpoints[1] == self.src {
                        Some((endpoints[0], Direction::Incoming))
                    } else {
                        None
                    }
                }
            };

            if let Some((neighbor, dir)) = neighbor {
                return Some((neighbor, index, self.src, dir));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::marker::{Directed, Undirected};
    use crate::storage::tests::*;

    #[test]
    fn basic_undirected() {
        test_basic::<Undirected, EdgeList<_, _, _>>();
    }

    #[test]
    fn basic_directed() {
        test_basic::<Directed, EdgeList<_, _, _>>();
    }

    #[test]
    fn multi_undirected() {
        test_multi::<Undirected, EdgeList<_, _, _>>();
    }

    #[test]
    fn multi_directed() {
        test_multi::<Directed, EdgeList<_, _, _>>();
    }
}
