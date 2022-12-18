use std::iter::Enumerate;
use std::marker::PhantomData;
use std::slice;

use super::shared::{EdgesIter, RangeIndices, VerticesIter};
use crate::index::{Indexing, NumIndexType};
use crate::infra::CompactIndexMap;
use crate::marker::{Direction, EdgeType};
use crate::traits::*;
use crate::{EdgesBaseWeak, EdgesWeak, VerticesBaseWeak, VerticesWeak};

#[derive(Debug, VerticesBaseWeak, VerticesWeak, EdgesBaseWeak, EdgesWeak)]
pub struct EdgeList<V, E, Ty, Ix: Indexing> {
    vertices: Vec<V>,
    edges: Vec<E>,
    endpoints: Vec<[Ix::VertexIndex; 2]>,
    ty: PhantomData<Ty>,
}

impl<V, E, Ty: EdgeType, Ix: Indexing> EdgeList<V, E, Ty, Ix> {
    pub fn new() -> Self {
        Self {
            vertices: Vec::new(),
            edges: Vec::new(),
            endpoints: Vec::new(),
            ty: PhantomData,
        }
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> EdgeList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
{
    fn relocate_vertex(&mut self, old_index: Ix::VertexIndex, new_index: Ix::VertexIndex) {
        self.endpoints.iter_mut().for_each(|endpoints| {
            for endpoint in endpoints.iter_mut() {
                if *endpoint == old_index {
                    *endpoint = new_index;
                }
            }
        })
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> Default for EdgeList<V, E, Ty, Ix> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> GraphBase for EdgeList<V, E, Ty, Ix> {
    type VertexIndex = Ix::VertexIndex;
    type EdgeIndex = Ix::EdgeIndex;
}

impl<V, E, Ty: EdgeType, Ix: Indexing> VerticesBase for EdgeList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
{
    type VertexIndicesIter<'a> = RangeIndices<Ix::VertexIndex>
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

    fn vertex_index_map(&self) -> CompactIndexMap<Ix::VertexIndex>
    where
        Ix::VertexIndex: NumIndexType,
    {
        CompactIndexMap::isomorphic(self.vertex_count())
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> Vertices<V> for EdgeList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
{
    type VertexRef<'a> = (Ix::VertexIndex, &'a V)
    where
        Self: 'a,
        V: 'a;

    type VerticesIter<'a> = VerticesIter<'a, Ix, V>
    where
        Self: 'a,
        V: 'a;

    fn vertex(&self, index: &Ix::VertexIndex) -> Option<&V> {
        self.vertices.get(index.to_usize())
    }

    fn vertices(&self) -> Self::VerticesIter<'_> {
        VerticesIter::new(self.vertices.iter())
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> VerticesMut<V> for EdgeList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
{
    fn vertex_mut(&mut self, index: &Ix::VertexIndex) -> Option<&mut V> {
        self.vertices.get_mut(index.to_usize())
    }

    fn add_vertex(&mut self, vertex: V) -> Ix::VertexIndex {
        let index = self.vertices.len();
        self.vertices.push(vertex);
        Ix::VertexIndex::from_usize(index)
    }

    fn remove_vertex(&mut self, index: &Ix::VertexIndex) -> Option<V> {
        // Remove all edges connected to this vertex in any direction.
        let mut i = 0;
        while i < self.endpoints.len() {
            let endpoints = &self.endpoints[i];
            if &endpoints[0] == index || &endpoints[1] == index {
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
            self.relocate_vertex(Ix::VertexIndex::from_usize(self.vertices.len()), *index);
        }

        Some(vertex)
    }

    fn clear(&mut self) {
        self.vertices.clear();
        self.edges.clear();
        self.endpoints.clear();
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> EdgesBase<Ty> for EdgeList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    type EdgeIndicesIter<'a> = RangeIndices<Ix::EdgeIndex>
    where
        Self: 'a;

    fn edge_count(&self) -> usize {
        self.edges.len()
    }

    fn edge_bound(&self) -> usize {
        self.edge_count()
    }

    fn endpoints(&self, index: &Ix::EdgeIndex) -> Option<(Ix::VertexIndex, Ix::VertexIndex)> {
        self.endpoints
            .get(index.to_usize())
            .map(|endpoints| (endpoints[0], endpoints[1]))
    }

    fn edge_index(&self, src: &Ix::VertexIndex, dst: &Ix::VertexIndex) -> Option<Ix::EdgeIndex> {
        self.endpoints
            .iter()
            .enumerate()
            .find_map(|(i, endpoints)| {
                #[allow(clippy::if_same_then_else)]
                if &endpoints[0] == src && &endpoints[1] == dst {
                    Some(Ix::EdgeIndex::from_usize(i))
                } else if !Ty::is_directed() && &endpoints[1] == src && &endpoints[0] == dst {
                    Some(Ix::EdgeIndex::from_usize(i))
                } else {
                    None
                }
            })
    }

    fn edge_indices(&self) -> Self::EdgeIndicesIter<'_> {
        (0..self.edge_bound()).into()
    }

    fn edge_index_map(&self) -> CompactIndexMap<Ix::EdgeIndex>
    where
        Ix::EdgeIndex: NumIndexType,
    {
        CompactIndexMap::isomorphic(self.edge_count())
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> Edges<E, Ty> for EdgeList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    type EdgeRef<'a> = (Ix::EdgeIndex, &'a E, Ix::VertexIndex, Ix::VertexIndex)
    where
        Self: 'a,
        E: 'a;

    type EdgesIter<'a> = EdgesIter<'a, Ix, E>
    where
        Self: 'a,
        E: 'a;

    fn edge(&self, index: &Ix::EdgeIndex) -> Option<&E> {
        self.edges.get(index.to_usize())
    }

    fn edges(&self) -> Self::EdgesIter<'_> {
        EdgesIter::new(self.edges.iter(), self.endpoints.iter())
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> EdgesMut<E, Ty> for EdgeList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    fn edge_mut(&mut self, index: &Ix::EdgeIndex) -> Option<&mut E> {
        self.edges.get_mut(index.to_usize())
    }

    fn add_edge(&mut self, src: &Ix::VertexIndex, dst: &Ix::VertexIndex, edge: E) -> Ix::EdgeIndex {
        assert!(
            src.to_usize() < self.vertices.len(),
            "src vertex does not exist"
        );
        assert!(
            dst.to_usize() < self.vertices.len(),
            "dst vertex does not exist"
        );

        self.endpoints.push([*src, *dst]);
        let index = self.edges.len();
        self.edges.push(edge);
        Ix::EdgeIndex::from_usize(index)
    }

    fn remove_edge(&mut self, index: &Ix::EdgeIndex) -> Option<E> {
        self.edge(index)?;
        self.endpoints.swap_remove(index.to_usize());
        Some(self.edges.swap_remove(index.to_usize()))
    }

    fn clear_edges(&mut self) {
        self.edges.clear();
        self.endpoints.clear();
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> MultiEdges<E, Ty> for EdgeList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    type MultiEdgeIndicesIter<'a> = MultiEdgeIndicesIter<'a, Ty, Ix>
    where
        Self: 'a;

    fn multi_edge_index(
        &self,
        src: &Ix::VertexIndex,
        dst: &Ix::VertexIndex,
    ) -> Self::MultiEdgeIndicesIter<'_> {
        self.vertex(src).expect("vertex does not exist");

        MultiEdgeIndicesIter {
            src: *src,
            dst: *dst,
            endpoints: self.endpoints.iter().enumerate(),
            ty: PhantomData,
        }
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> Neighbors for EdgeList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    type NeighborRef<'a> = (Ix::VertexIndex, Ix::EdgeIndex, Ix::VertexIndex, Direction)
    where
        Self: 'a;

    type NeighborsIter<'a> = NeighborsIter<'a, Ix>
    where
        Self: 'a;

    fn neighbors(&self, src: &Ix::VertexIndex) -> Self::NeighborsIter<'_> {
        self.vertex(src).expect("vertex does not exist");

        NeighborsIter {
            src: *src,
            edges: self.endpoints.as_slice(),
            dir: None,
            index: 0,
            is_directed: Ty::is_directed(),
        }
    }

    fn neighbors_directed(&self, src: &Ix::VertexIndex, dir: Direction) -> Self::NeighborsIter<'_> {
        self.vertex(src).expect("vertex does not exist");

        NeighborsIter {
            src: *src,
            edges: self.endpoints.as_slice(),
            dir: Some(dir),
            index: 0,
            is_directed: Ty::is_directed(),
        }
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> Create<V, E, Ty> for EdgeList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    fn with_capacity(vertex_count: usize, edge_count: usize) -> Self {
        Self {
            vertices: Vec::with_capacity(vertex_count),
            edges: Vec::with_capacity(edge_count),
            endpoints: Vec::with_capacity(edge_count),
            ty: PhantomData,
        }
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> Guarantee for EdgeList<V, E, Ty, Ix> {}

pub struct MultiEdgeIndicesIter<'a, Ty: EdgeType, Ix: Indexing> {
    src: Ix::VertexIndex,
    dst: Ix::VertexIndex,
    endpoints: Enumerate<slice::Iter<'a, [Ix::VertexIndex; 2]>>,
    ty: PhantomData<Ty>,
}

impl<'a, Ty: EdgeType, Ix: Indexing> Iterator for MultiEdgeIndicesIter<'a, Ty, Ix>
where
    Ix::EdgeIndex: NumIndexType,
{
    type Item = Ix::EdgeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        for (index, endpoints) in self.endpoints.by_ref() {
            if endpoints[0] == self.src && endpoints[1] == self.dst {
                return Some(Ix::EdgeIndex::from_usize(index));
            }
        }

        None
    }
}

pub struct NeighborsIter<'a, Ix: Indexing> {
    src: Ix::VertexIndex,
    edges: &'a [[Ix::VertexIndex; 2]],
    index: usize,
    dir: Option<Direction>,
    is_directed: bool,
}

impl<'a, Ix: Indexing> Iterator for NeighborsIter<'a, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    type Item = (Ix::VertexIndex, Ix::EdgeIndex, Ix::VertexIndex, Direction);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (endpoints, tail) = self.edges.split_first()?;
            self.edges = tail;

            let index = Ix::EdgeIndex::from_usize(self.index);
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
    use crate::index::DefaultIndexing;
    use crate::marker::{Directed, Undirected};
    use crate::storage::tests::*;

    #[test]
    fn basic_undirected() {
        test_basic::<Undirected, EdgeList<_, _, _, DefaultIndexing>>();
    }

    #[test]
    fn basic_directed() {
        test_basic::<Directed, EdgeList<_, _, _, DefaultIndexing>>();
    }

    #[test]
    fn multi_undirected() {
        test_multi::<Undirected, EdgeList<_, _, _, DefaultIndexing>>();
    }

    #[test]
    fn multi_directed() {
        test_multi::<Directed, EdgeList<_, _, _, DefaultIndexing>>();
    }
}
