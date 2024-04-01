use std::{iter::Enumerate, marker::PhantomData, slice};

use crate::common::CompactIdMap;
use crate::core::{
    id::{DefaultId, GraphIdTypes, IntegerIdType},
    marker::{Direction, EdgeType},
    AddEdgeError, AddEdgeErrorKind, AddVertexError, ConnectVertices, Create, Edges, EdgesBase,
    EdgesMut, GraphBase, Guarantee, MultiEdges, Neighbors, Vertices, VerticesBase, VerticesMut,
};

use gryf_derive::{EdgesBaseWeak, EdgesWeak, VerticesBaseWeak, VerticesWeak};

// TODO: Remove these imports once hygiene of procedural macros is fixed.
use crate::core::{EdgesBaseWeak, EdgesWeak, VerticesBaseWeak, VerticesWeak, WeakRef};

use super::shared;
pub use super::shared::{EdgesIter, RangeIds as VertexIds, RangeIds as EdgeIds, VerticesIter};

#[derive(Debug, VerticesBaseWeak, VerticesWeak, EdgesBaseWeak, EdgesWeak)]
pub struct EdgeList<V, E, Ty, Id: GraphIdTypes> {
    vertices: Vec<V>,
    edges: Vec<E>,
    endpoints: Vec<[Id::VertexId; 2]>,
    ty: PhantomData<Ty>,
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> EdgeList<V, E, Ty, Id> {
    pub fn new() -> Self {
        Self {
            vertices: Vec::new(),
            edges: Vec::new(),
            endpoints: Vec::new(),
            ty: PhantomData,
        }
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
{
    fn relocate_vertex(&mut self, old_id: Id::VertexId, new_id: Id::VertexId) {
        self.endpoints.iter_mut().for_each(|endpoints| {
            for endpoint in endpoints.iter_mut() {
                if *endpoint == old_id {
                    *endpoint = new_id;
                }
            }
        })
    }
}

impl<V, E, Ty: EdgeType> Default for EdgeList<V, E, Ty, DefaultId> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> GraphBase for EdgeList<V, E, Ty, Id> {
    type VertexId = Id::VertexId;
    type EdgeId = Id::EdgeId;
    type EdgeType = Ty;
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> VerticesBase for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
{
    type VertexIdsIter<'a> = VertexIds<Self::VertexId>
    where
        Self: 'a;

    fn vertex_count(&self) -> usize {
        self.vertices.len()
    }

    fn vertex_bound(&self) -> usize {
        self.vertex_count()
    }

    fn vertex_ids(&self) -> Self::VertexIdsIter<'_> {
        (0..self.vertex_bound()).into()
    }

    fn vertex_id_map(&self) -> CompactIdMap<Self::VertexId>
    where
        Id::VertexId: IntegerIdType,
    {
        CompactIdMap::isomorphic(self.vertex_count())
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> Vertices<V> for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
{
    type VertexRef<'a> = (Self::VertexId, &'a V)
    where
        Self: 'a,
        V: 'a;

    type VerticesIter<'a> = VerticesIter<'a, Id, V>
    where
        Self: 'a,
        V: 'a;

    fn vertex(&self, id: &Self::VertexId) -> Option<&V> {
        self.vertices.get(id.to_usize())
    }

    fn vertices(&self) -> Self::VerticesIter<'_> {
        VerticesIter::new(self.vertices.iter())
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> VerticesMut<V> for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
{
    fn vertex_mut(&mut self, id: &Self::VertexId) -> Option<&mut V> {
        self.vertices.get_mut(id.to_usize())
    }

    fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>> {
        let index = self.vertices.len();
        self.vertices.push(vertex);
        Ok(index.into())
    }

    fn remove_vertex(&mut self, id: &Self::VertexId) -> Option<V> {
        self.vertex(id)?;

        // Remove all edges connected to this vertex in any direction.
        let mut i = 0;
        while i < self.endpoints.len() {
            let endpoints = &self.endpoints[i];
            if &endpoints[0] == id || &endpoints[1] == id {
                self.edges.swap_remove(i);
                self.endpoints.swap_remove(i);
            } else {
                i += 1
            }
        }

        // Remove the vertex from the graph.
        let vertex = self.vertices.swap_remove(id.to_usize());

        // If `swap_remove` actually moved an existing vertex somewhere, we need
        // to fix its id in the entire graph.
        if id.to_usize() < self.vertices.len() {
            self.relocate_vertex(Id::VertexId::from_usize(self.vertices.len()), *id);
        }

        Some(vertex)
    }

    fn clear(&mut self) {
        self.vertices.clear();
        self.edges.clear();
        self.endpoints.clear();
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> EdgesBase<Ty> for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    type EdgeIdsIter<'a> = EdgeIds<Self::EdgeId>
    where
        Self: 'a;
    type EdgeIdIter<'a> = EdgeIdIter<'a, Ty, Id>
    where
        Self: 'a;

    fn edge_count(&self) -> usize {
        self.edges.len()
    }

    fn edge_bound(&self) -> usize {
        self.edge_count()
    }

    fn endpoints(&self, id: &Id::EdgeId) -> Option<(Self::VertexId, Self::VertexId)> {
        self.endpoints
            .get(id.to_usize())
            .map(|endpoints| (endpoints[0], endpoints[1]))
    }

    fn edge_id(&self, src: &Self::VertexId, dst: &Self::VertexId) -> Self::EdgeIdIter<'_> {
        EdgeIdIter {
            src: *src,
            dst: *dst,
            endpoints: self.endpoints.iter().enumerate(),
            ty: PhantomData,
        }
    }

    fn edge_ids(&self) -> Self::EdgeIdsIter<'_> {
        (0..self.edge_bound()).into()
    }

    fn edge_id_map(&self) -> CompactIdMap<Self::EdgeId>
    where
        Id::EdgeId: IntegerIdType,
    {
        CompactIdMap::isomorphic(self.edge_count())
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> Edges<E, Ty> for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    type EdgeRef<'a> = (Self::EdgeId, &'a E, Self::VertexId, Self::VertexId)
    where
        Self: 'a,
        E: 'a;

    type EdgesIter<'a> = EdgesIter<'a, Id, E>
    where
        Self: 'a,
        E: 'a;

    fn edge(&self, id: &Self::EdgeId) -> Option<&E> {
        self.edges.get(id.to_usize())
    }

    fn edges(&self) -> Self::EdgesIter<'_> {
        EdgesIter::new(self.edges.iter(), self.endpoints.iter())
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> EdgesMut<E, Ty> for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    fn edge_mut(&mut self, id: &Self::EdgeId) -> Option<&mut E> {
        self.edges.get_mut(id.to_usize())
    }

    fn try_add_edge(
        &mut self,
        src: &Self::VertexId,
        dst: &Self::VertexId,
        edge: E,
    ) -> Result<Self::EdgeId, AddEdgeError<E>> {
        if src.to_usize() >= self.vertices.len() {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::SourceAbsent));
        }

        if dst.to_usize() >= self.vertices.len() {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::DestinationAbsent));
        }

        self.endpoints.push([*src, *dst]);
        let index = self.edges.len();
        self.edges.push(edge);
        Ok(index.into())
    }

    fn remove_edge(&mut self, id: &Self::EdgeId) -> Option<E> {
        self.edge(id)?;
        self.endpoints.swap_remove(id.to_usize());
        Some(self.edges.swap_remove(id.to_usize()))
    }

    fn clear_edges(&mut self) {
        self.edges.clear();
        self.endpoints.clear();
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> MultiEdges<Ty> for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> Neighbors for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    type NeighborRef<'a> = (Id::VertexId, Id::EdgeId, Id::VertexId, Direction)
    where
        Self: 'a;

    type NeighborsIter<'a> = NeighborsIter<'a, Ty, Id>
    where
        Self: 'a;

    fn neighbors(&self, src: &Id::VertexId) -> Self::NeighborsIter<'_> {
        self.vertex(src).expect("vertex does not exist");

        NeighborsIter {
            src: *src,
            edges: self.endpoints.as_slice(),
            dir: None,
            index: 0,
            self_loop: None,
            ty: PhantomData,
        }
    }

    fn neighbors_directed(&self, src: &Id::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
        self.vertex(src).expect("vertex does not exist");

        NeighborsIter {
            src: *src,
            edges: self.endpoints.as_slice(),
            dir: Some(dir),
            index: 0,
            self_loop: None,
            ty: PhantomData,
        }
    }

    fn degree(&self, id: &Self::VertexId) -> usize {
        self.vertex(id).expect("vertex does not exist");

        self.endpoints
            .iter()
            .filter(|[u, v]| u == id || v == id)
            .map(|[u, v]| 1 + (u == v) as usize)
            .sum()
    }

    fn degree_directed(&self, id: &Self::VertexId, dir: Direction) -> usize {
        if Ty::is_directed() {
            match dir {
                Direction::Outgoing => self
                    .endpoints
                    .iter()
                    .filter(|[u, _]| u == id)
                    .map(|[u, v]| 1 + (!Ty::is_directed() && u == v) as usize)
                    .sum(),
                Direction::Incoming => self
                    .endpoints
                    .iter()
                    .filter(|[_, v]| v == id)
                    .map(|[u, v]| 1 + (!Ty::is_directed() && u == v) as usize)
                    .sum(),
            }
        } else {
            self.degree(id)
        }
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> Create<V, E, Ty> for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
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

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> ConnectVertices<V, E, Ty> for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    fn connect_vertices<F>(&mut self, mut connect: F)
    where
        F: FnMut(&V, &V) -> Option<E>,
    {
        shared::connect_vertices::<Ty>(self.vertices.len(), |i, j| {
            let src = &self.vertices[i];
            let dst = &self.vertices[j];

            if let Some(edge) = connect(src, dst) {
                let src = Id::VertexId::from_usize(i);
                let dst = Id::VertexId::from_usize(j);

                self.add_edge(&src, &dst, edge);
            }
        })
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> Guarantee for EdgeList<V, E, Ty, Id> {}

pub struct EdgeIdIter<'a, Ty: EdgeType, Id: GraphIdTypes> {
    src: Id::VertexId,
    dst: Id::VertexId,
    endpoints: Enumerate<slice::Iter<'a, [Id::VertexId; 2]>>,
    ty: PhantomData<Ty>,
}

impl<'a, Ty: EdgeType, Id: GraphIdTypes> Iterator for EdgeIdIter<'a, Ty, Id>
where
    Id::EdgeId: IntegerIdType,
{
    type Item = Id::EdgeId;

    fn next(&mut self) -> Option<Self::Item> {
        for (index, endpoints) in self.endpoints.by_ref() {
            let src_dst = endpoints[0] == self.src && endpoints[1] == self.dst;
            let dst_src =
                !Ty::is_directed() && endpoints[0] == self.dst && endpoints[1] == self.src;
            if src_dst || dst_src {
                return Some(Id::EdgeId::from_usize(index));
            }
        }

        None
    }
}

pub struct NeighborsIter<'a, Ty, Id: GraphIdTypes> {
    src: Id::VertexId,
    edges: &'a [[Id::VertexId; 2]],
    index: usize,
    dir: Option<Direction>,
    self_loop: Option<(Id::EdgeId, Direction)>,
    ty: PhantomData<Ty>,
}

impl<'a, Ty: EdgeType, Id: GraphIdTypes> Iterator for NeighborsIter<'a, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    type Item = (Id::VertexId, Id::EdgeId, Id::VertexId, Direction);

    fn next(&mut self) -> Option<Self::Item> {
        if Ty::is_directed() {
            if let Some((id, dir)) = self.self_loop.take() {
                return Some((self.src, id, self.src, dir));
            }
        }

        loop {
            let (endpoints, tail) = self.edges.split_first()?;
            self.edges = tail;

            let id = Id::EdgeId::from_usize(self.index);
            self.index += 1;

            let neighbor = match (self.dir, Ty::is_directed()) {
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
                if Ty::is_directed() && neighbor == self.src && self.dir.is_none() {
                    // There is only one edge-item in edge list for a self-loop
                    // in directed graph. But we need to report the edge twice,
                    // for each direction.
                    self.self_loop = Some((id, dir.opposite()));
                }

                return Some((neighbor, id, self.src, dir));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        core::marker::{Directed, Undirected},
        infra::arbitrary::{ArbitraryId, Index},
        storage::tests::*,
    };

    #[test]
    fn basic_undirected() {
        test_basic::<Undirected, EdgeList<_, _, _, DefaultId>>();
    }

    #[test]
    fn basic_directed() {
        test_basic::<Directed, EdgeList<_, _, _, DefaultId>>();
    }

    #[test]
    fn multi_undirected() {
        test_multi::<Undirected, EdgeList<_, _, _, DefaultId>>();
    }

    #[test]
    fn multi_directed() {
        test_multi::<Directed, EdgeList<_, _, _, DefaultId>>();
    }

    #[test]
    fn connect_vertices_undirected() {
        test_connect_vertices::<Undirected, EdgeList<_, _, _, DefaultId>>();
    }

    #[test]
    fn connect_vertices_directed() {
        test_connect_vertices::<Directed, EdgeList<_, _, _, DefaultId>>();
    }

    #[test]
    fn neighbors_edge_cases_undirected() {
        test_neighbors_edge_cases::<Undirected, EdgeList<_, _, _, DefaultId>>();
    }

    #[test]
    fn neighbors_edge_cases_directed() {
        test_neighbors_edge_cases::<Directed, EdgeList<_, _, _, DefaultId>>();
    }

    #[test]
    fn fuzz_trophy1() {
        let mut graph = EdgeList::<_, (), Undirected, ArbitraryId>::new();

        graph.add_vertex(0);
        graph.remove_vertex(&Index(0));
        graph.remove_vertex(&Index(0));
    }
}
