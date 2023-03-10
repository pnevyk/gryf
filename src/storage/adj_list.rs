use std::marker::PhantomData;

use crate::{
    common::CompactIndexMap,
    core::{
        index::{Indexing, NumIndexType},
        marker::{Direction, EdgeType},
        AddEdgeError, AddEdgeErrorKind, AddVertexError, ConnectVertices, Create, Edges, EdgesBase,
        EdgesMut, GraphBase, Guarantee, MultiEdges, Neighbors, Vertices, VerticesBase, VerticesMut,
    },
};

use crate::derive::{EdgesBaseWeak, EdgesWeak, VerticesBaseWeak, VerticesWeak};

// TODO: Remove these imports once hygiene of procedural macros is fixed.
use crate::core::{EdgesBaseWeak, EdgesWeak, VerticesBaseWeak, VerticesWeak, WeakRef};

use super::shared;
pub use super::shared::{
    AdjVertex as Vertex, AdjVerticesIter as VerticesIter, EdgesIter, RangeIndices as EdgeIndices,
    RangeIndices as VertexIndices,
};

#[derive(Debug, VerticesBaseWeak, VerticesWeak, EdgesBaseWeak, EdgesWeak, Clone, PartialEq, Eq)]
pub struct AdjList<V, E, Ty, Ix: Indexing> {
    vertices: Vec<Vertex<Ix, V>>,
    edges: Vec<E>,
    endpoints: Vec<[Ix::VertexIndex; 2]>,
    ty: PhantomData<fn() -> (Ty, Ix)>,
}

impl<V, E, Ty: EdgeType, Ix: Indexing> AdjList<V, E, Ty, Ix> {
    pub fn new() -> Self {
        Self {
            vertices: Vec::new(),
            edges: Vec::new(),
            endpoints: Vec::new(),
            ty: PhantomData,
        }
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> AdjList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    fn remove_edge_inner(
        &mut self,
        index: Ix::EdgeIndex,
        cause: Option<Ix::VertexIndex>,
    ) -> Option<E> {
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
            self.relocate_edge(NumIndexType::from_usize(self.edges.len()), index);
        }

        Some(edge)
    }

    fn relocate_vertex(&mut self, old_index: Ix::VertexIndex, new_index: Ix::VertexIndex) {
        let vertex = &mut self.vertices[new_index.to_usize()];

        // Fix the index of the vertex in all edges it has.
        for dir in Ty::directions() {
            for edge_index in vertex.edges[dir.index()].iter() {
                let endpoints = &mut self.endpoints[edge_index.to_usize()];
                for endpoint in endpoints.iter_mut() {
                    if *endpoint == old_index {
                        *endpoint = new_index;
                    }
                }
            }
        }
    }

    fn relocate_edge(&mut self, old_index: Ix::EdgeIndex, new_index: Ix::EdgeIndex) {
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

    fn disconnect(edges: &mut Vec<Ix::EdgeIndex>, index: Ix::EdgeIndex) {
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

impl<V, E, Ty: EdgeType, Ix: Indexing> Default for AdjList<V, E, Ty, Ix> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> GraphBase for AdjList<V, E, Ty, Ix> {
    type VertexIndex = Ix::VertexIndex;
    type EdgeIndex = Ix::EdgeIndex;
}

impl<V, E, Ty: EdgeType, Ix: Indexing> VerticesBase for AdjList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
{
    type VertexIndicesIter<'a> = VertexIndices<Self::VertexIndex>
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

    fn vertex_index_map(&self) -> CompactIndexMap<Self::VertexIndex>
    where
        Self::VertexIndex: NumIndexType,
    {
        CompactIndexMap::isomorphic(self.vertex_count())
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> Vertices<V> for AdjList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
{
    type VertexRef<'a> = (Self::VertexIndex, &'a V)
    where
        Self: 'a;

    type VerticesIter<'a> = VerticesIter<'a, Ix, V>
    where
        Self: 'a;

    fn vertex(&self, index: &Self::VertexIndex) -> Option<&V> {
        self.vertices
            .get(index.to_usize())
            .map(|vertex| &vertex.data)
    }

    fn vertices(&self) -> Self::VerticesIter<'_> {
        VerticesIter::new(self.vertices.iter())
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> VerticesMut<V> for AdjList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    fn vertex_mut(&mut self, index: &Self::VertexIndex) -> Option<&mut V> {
        self.vertices
            .get_mut(index.to_usize())
            .map(|vertex| &mut vertex.data)
    }

    fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexIndex, AddVertexError<V>> {
        let index = self.vertices.len();
        self.vertices.push(Vertex::new(vertex));
        Ok(index.into())
    }

    fn remove_vertex(&mut self, index: &Self::VertexIndex) -> Option<V> {
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
                self.remove_edge_inner(edge_index, Some(*index));
            }
        }

        // Remove the vertex from the graph.
        let vertex = self.vertices.swap_remove(index.to_usize());

        // If `swap_remove` actually moved an existing vertex somewhere, we need
        // to fix its index in the entire graph.
        if index.to_usize() < self.vertices.len() {
            self.relocate_vertex(NumIndexType::from_usize(self.vertices.len()), *index);
        }

        Some(vertex.data)
    }

    fn clear(&mut self) {
        self.vertices.clear();
        self.edges.clear();
        self.endpoints.clear();
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> EdgesBase<Ty> for AdjList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    type EdgeIndicesIter<'a> = EdgeIndices<Self::EdgeIndex>
    where
        Self: 'a;
    type EdgeIndexIter<'a> = EdgeIndexIter<'a, Ix>
    where
        Self: 'a;

    fn edge_count(&self) -> usize {
        self.edges.len()
    }

    fn edge_bound(&self) -> usize {
        self.edge_count()
    }

    fn endpoints(&self, index: &Self::EdgeIndex) -> Option<(Self::VertexIndex, Self::VertexIndex)> {
        self.endpoints
            .get(index.to_usize())
            .map(|endpoints| (endpoints[0], endpoints[1]))
    }

    fn edge_index(
        &self,
        src: &Self::VertexIndex,
        dst: &Self::VertexIndex,
    ) -> Self::EdgeIndexIter<'_> {
        match self.vertices.get(src.to_usize()) {
            Some(vertex) => EdgeIndexIter {
                src: *src,
                dst: *dst,
                edges: &vertex.edges[Direction::Outgoing.index()],
                endpoints: self.endpoints.as_slice(),
            },
            None => EdgeIndexIter {
                src: *src,
                dst: *dst,
                edges: &[],
                endpoints: &[],
            },
        }
    }

    fn edge_indices(&self) -> Self::EdgeIndicesIter<'_> {
        (0..self.edge_bound()).into()
    }

    fn edge_index_map(&self) -> CompactIndexMap<Self::EdgeIndex>
    where
        Self::EdgeIndex: NumIndexType,
    {
        CompactIndexMap::isomorphic(self.edge_count())
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> Edges<E, Ty> for AdjList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    type EdgeRef<'a> = (Self::EdgeIndex, &'a E, Self::VertexIndex, Self::VertexIndex)
    where
        Self: 'a;

    type EdgesIter<'a> = EdgesIter<'a, Ix, E>
    where
        Self: 'a;

    fn edge(&self, index: &Self::EdgeIndex) -> Option<&E> {
        self.edges.get(index.to_usize())
    }

    fn edges(&self) -> Self::EdgesIter<'_> {
        EdgesIter::new(self.edges.iter(), self.endpoints.iter())
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> EdgesMut<E, Ty> for AdjList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    fn edge_mut(&mut self, index: &Self::EdgeIndex) -> Option<&mut E> {
        self.edges.get_mut(index.to_usize())
    }

    fn try_add_edge(
        &mut self,
        src: &Self::VertexIndex,
        dst: &Self::VertexIndex,
        edge: E,
    ) -> Result<Self::EdgeIndex, AddEdgeError<E>> {
        if src.to_usize() >= self.vertices.len() {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::SourceAbsent));
        }

        if dst.to_usize() >= self.vertices.len() {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::DestinationAbsent));
        }

        let index = NumIndexType::from_usize(self.edges.len());
        self.edges.push(edge);
        self.endpoints.push([*src, *dst]);

        let directions = Self::directions();
        self.vertices[src.to_usize()].edges[directions[0].index()].push(index);
        self.vertices[dst.to_usize()].edges[directions[1].index()].push(index);

        Ok(index)
    }

    fn remove_edge(&mut self, index: &Self::EdgeIndex) -> Option<E> {
        self.remove_edge_inner(*index, None)
    }

    fn clear_edges(&mut self) {
        self.edges.clear();
        self.endpoints.clear();
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> MultiEdges<Ty> for AdjList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
}

impl<V, E, Ty: EdgeType, Ix: Indexing> Neighbors for AdjList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    type NeighborRef<'a> = (Self::VertexIndex, Self::EdgeIndex, Self::VertexIndex, Direction)
    where
        Self: 'a;

    type NeighborsIter<'a> = NeighborsIter<'a, Ix>
    where
        Self: 'a;

    fn neighbors(&self, src: &Self::VertexIndex) -> Self::NeighborsIter<'_> {
        let vertex = self
            .vertices
            .get(src.to_usize())
            .expect("vertex does not exist");

        NeighborsIter {
            src: *src,
            edges: [&vertex.edges[0], &vertex.edges[1]],
            endpoints: self.endpoints.as_slice(),
            dir: 0,
        }
    }

    fn neighbors_directed(
        &self,
        src: &Self::VertexIndex,
        dir: Direction,
    ) -> Self::NeighborsIter<'_> {
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

        let mut edges: [&[Self::EdgeIndex]; 2] = [&[], &[]];
        edges[dir.index()] = &vertex.edges[adj_dir.index()];

        NeighborsIter {
            src: *src,
            edges,
            endpoints: self.endpoints.as_slice(),
            dir: dir.index(),
        }
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> Create<V, E, Ty> for AdjList<V, E, Ty, Ix>
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

impl<V, E, Ty: EdgeType, Ix: Indexing> ConnectVertices<V, E, Ty> for AdjList<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    fn connect_vertices<F>(&mut self, mut connect: F)
    where
        F: FnMut(&V, &V) -> Option<E>,
    {
        shared::connect_vertices::<Ty>(self.vertices.len(), |i, j| {
            let src = &self.vertices[i].data;
            let dst = &self.vertices[j].data;

            if let Some(edge) = connect(src, dst) {
                let src = Ix::VertexIndex::from_usize(i);
                let dst = Ix::VertexIndex::from_usize(j);

                self.add_edge(&src, &dst, edge);
            }
        })
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> Guarantee for AdjList<V, E, Ty, Ix> {}

pub struct EdgeIndexIter<'a, Ix: Indexing> {
    src: Ix::VertexIndex,
    dst: Ix::VertexIndex,
    edges: &'a [Ix::EdgeIndex],
    endpoints: &'a [[Ix::VertexIndex; 2]],
}

impl<'a, Ix: Indexing> Iterator for EdgeIndexIter<'a, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    type Item = Ix::EdgeIndex;

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

pub struct NeighborsIter<'a, Ix: Indexing> {
    src: Ix::VertexIndex,
    edges: [&'a [Ix::EdgeIndex]; 2],
    endpoints: &'a [[Ix::VertexIndex; 2]],
    dir: usize,
}

impl<Ix: Indexing> Iterator for NeighborsIter<'_, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    type Item = (Ix::VertexIndex, Ix::EdgeIndex, Ix::VertexIndex, Direction);

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
    use crate::{
        core::{
            index::DefaultIndexing,
            marker::{Directed, Undirected},
        },
        storage::tests::*,
    };

    #[test]
    fn basic_undirected() {
        test_basic::<Undirected, AdjList<_, _, _, DefaultIndexing>>();
    }

    #[test]
    fn basic_directed() {
        test_basic::<Directed, AdjList<_, _, _, DefaultIndexing>>();
    }

    #[test]
    fn multi_undirected() {
        test_multi::<Undirected, AdjList<_, _, _, DefaultIndexing>>();
    }

    #[test]
    fn multi_directed() {
        test_multi::<Directed, AdjList<_, _, _, DefaultIndexing>>();
    }

    #[test]
    fn connect_vertices_undirected() {
        test_connect_vertices::<Undirected, AdjList<_, _, _, DefaultIndexing>>();
    }

    #[test]
    fn connect_vertices_directed() {
        test_connect_vertices::<Directed, AdjList<_, _, _, DefaultIndexing>>();
    }
}
