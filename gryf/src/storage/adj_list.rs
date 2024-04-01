use std::marker::PhantomData;

use crate::{
    common::CompactIdMap,
    core::{
        error::{AddEdgeError, AddEdgeErrorKind, AddVertexError},
        id::{DefaultId, GraphIdTypes, IdType, IntegerIdType},
        marker::{Direction, EdgeType},
        ConnectVertices, Create, Edges, EdgesBase, EdgesMut, GraphBase, Guarantee, MultiEdges,
        Neighbors, Vertices, VerticesBase, VerticesMut,
    },
};

use gryf_derive::{EdgesBaseWeak, EdgesWeak, VerticesBaseWeak, VerticesWeak};

// TODO: Remove these imports once hygiene of procedural macros is fixed.
use crate::core::{EdgesBaseWeak, EdgesWeak, VerticesBaseWeak, VerticesWeak, WeakRef};

use super::shared;
pub use super::shared::{
    AdjVertex as Vertex, AdjVerticesIter as VerticesIter, EdgesIter, RangeIds as EdgeIds,
    RangeIds as VertexIds,
};

#[derive(Debug, VerticesBaseWeak, VerticesWeak, EdgesBaseWeak, EdgesWeak, Clone, PartialEq, Eq)]
pub struct AdjList<V, E, Ty, Id: GraphIdTypes> {
    vertices: Vec<Vertex<Id, V>>,
    edges: Vec<E>,
    endpoints: Vec<[Id::VertexId; 2]>,
    ty: PhantomData<fn() -> (Ty, Id)>,
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> AdjList<V, E, Ty, Id> {
    pub fn new() -> Self {
        Self {
            vertices: Vec::new(),
            edges: Vec::new(),
            endpoints: Vec::new(),
            ty: PhantomData,
        }
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> AdjList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    fn remove_edge_inner(&mut self, id: Id::EdgeId, cause: Option<Id::VertexId>) -> Option<E> {
        let endpoints = self.endpoints.get(id.as_usize())?;

        for (i, dir) in Self::directions().iter().enumerate() {
            let endpoint = endpoints[i];

            // If this endpoint is not the vertex causing this removal, we need
            // to remove the edge from it. If is the cause, it is not necessary
            // to remove it.
            if Some(endpoint) != cause {
                Self::disconnect(
                    &mut self.vertices[endpoint.as_usize()].edges[dir.index()],
                    id,
                );
            }
        }

        // Remove the edge from the graph.
        let edge = self.edges.swap_remove(id.as_usize());
        self.endpoints.swap_remove(id.as_usize());

        // If `swap_remove` actually moved an existing edge somewhere, we need
        // to fix its id in the entire graph.
        if id.as_usize() < self.edges.len() {
            self.relocate_edge(IdType::from_usize(self.edges.len()), id);
        }

        Some(edge)
    }

    fn relocate_vertex(&mut self, old_id: Id::VertexId, new_id: Id::VertexId) {
        let vertex = &mut self.vertices[new_id.as_usize()];

        // Fix the id of the vertex in all edges it has.
        for dir in Ty::directions() {
            for edge_id in vertex.edges[dir.index()].iter() {
                let endpoints = &mut self.endpoints[edge_id.as_usize()];
                for endpoint in endpoints.iter_mut() {
                    if *endpoint == old_id {
                        *endpoint = new_id;
                    }
                }
            }
        }
    }

    fn relocate_edge(&mut self, old_id: Id::EdgeId, new_id: Id::EdgeId) {
        let endpoints = &mut self.endpoints[new_id.as_usize()];

        // Fix the id of the edge in all vertices it is incident with.
        for i in 0..=1 {
            let vertex = &mut self.vertices[endpoints[i].as_usize()];

            for dir in Ty::directions() {
                for edge_id in &mut vertex.edges[dir.index()] {
                    if *edge_id == old_id {
                        *edge_id = new_id;
                    }
                }
            }

            // If this is a self-loop, then all ids are fixed in the first
            // iteration.
            if endpoints[0] == endpoints[1] {
                break;
            }
        }
    }

    fn disconnect(edges: &mut Vec<Id::EdgeId>, id: Id::EdgeId) {
        for i in 0..edges.len() {
            if edges[i] == id {
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

impl<V, E, Ty: EdgeType> Default for AdjList<V, E, Ty, DefaultId> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> GraphBase for AdjList<V, E, Ty, Id> {
    type VertexId = Id::VertexId;
    type EdgeId = Id::EdgeId;
    type EdgeType = Ty;
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> VerticesBase for AdjList<V, E, Ty, Id>
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
        Self::VertexId: IntegerIdType,
    {
        CompactIdMap::isomorphic(self.vertex_count())
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> Vertices<V> for AdjList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
{
    type VertexRef<'a> = (Self::VertexId, &'a V)
    where
        Self: 'a;

    type VerticesIter<'a> = VerticesIter<'a, Id, V>
    where
        Self: 'a;

    fn vertex(&self, id: &Self::VertexId) -> Option<&V> {
        self.vertices.get(id.as_usize()).map(|vertex| &vertex.data)
    }

    fn vertices(&self) -> Self::VerticesIter<'_> {
        VerticesIter::new(self.vertices.iter())
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> VerticesMut<V> for AdjList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    fn vertex_mut(&mut self, id: &Self::VertexId) -> Option<&mut V> {
        self.vertices
            .get_mut(id.as_usize())
            .map(|vertex| &mut vertex.data)
    }

    fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>> {
        let index = self.vertices.len();
        self.vertices.push(Vertex::new(vertex));
        Ok(index.into())
    }

    fn remove_vertex(&mut self, id: &Self::VertexId) -> Option<V> {
        for dir in Ty::directions() {
            // Remove all edges connected to this vertex in this direction.
            loop {
                let vertex = self.vertices.get_mut(id.as_usize())?;
                if vertex.edges[dir.index()].is_empty() {
                    break;
                }

                // Remove the edge from the list of this vertex.
                let edge_id = vertex.edges[dir.index()].swap_remove(0);
                // Remove the edge from the whole graph.
                self.remove_edge_inner(edge_id, Some(*id));
            }
        }

        // Remove the vertex from the graph.
        let vertex = self.vertices.swap_remove(id.as_usize());

        // If `swap_remove` actually moved an existing vertex somewhere, we need
        // to fix its id in the entire graph.
        if id.as_usize() < self.vertices.len() {
            self.relocate_vertex(IdType::from_usize(self.vertices.len()), *id);
        }

        Some(vertex.data)
    }

    fn clear(&mut self) {
        self.vertices.clear();
        self.edges.clear();
        self.endpoints.clear();
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> EdgesBase<Ty> for AdjList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    type EdgeIdsIter<'a> = EdgeIds<Self::EdgeId>
    where
        Self: 'a;
    type EdgeIdIter<'a> = EdgeIdIter<'a, Id>
    where
        Self: 'a;

    fn edge_count(&self) -> usize {
        self.edges.len()
    }

    fn edge_bound(&self) -> usize {
        self.edge_count()
    }

    fn endpoints(&self, id: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)> {
        self.endpoints
            .get(id.as_usize())
            .map(|endpoints| (endpoints[0], endpoints[1]))
    }

    fn edge_id(&self, src: &Self::VertexId, dst: &Self::VertexId) -> Self::EdgeIdIter<'_> {
        match self.vertices.get(src.as_usize()) {
            Some(vertex) => EdgeIdIter {
                src: *src,
                dst: *dst,
                edges: &vertex.edges[Direction::Outgoing.index()],
                endpoints: self.endpoints.as_slice(),
            },
            None => EdgeIdIter {
                src: *src,
                dst: *dst,
                edges: &[],
                endpoints: &[],
            },
        }
    }

    fn edge_ids(&self) -> Self::EdgeIdsIter<'_> {
        (0..self.edge_bound()).into()
    }

    fn edge_id_map(&self) -> CompactIdMap<Self::EdgeId>
    where
        Self::EdgeId: IntegerIdType,
    {
        CompactIdMap::isomorphic(self.edge_count())
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> Edges<E, Ty> for AdjList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    type EdgeRef<'a> = (Self::EdgeId, &'a E, Self::VertexId, Self::VertexId)
    where
        Self: 'a;

    type EdgesIter<'a> = EdgesIter<'a, Id, E>
    where
        Self: 'a;

    fn edge(&self, id: &Self::EdgeId) -> Option<&E> {
        self.edges.get(id.as_usize())
    }

    fn edges(&self) -> Self::EdgesIter<'_> {
        EdgesIter::new(self.edges.iter(), self.endpoints.iter())
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> EdgesMut<E, Ty> for AdjList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    fn edge_mut(&mut self, id: &Self::EdgeId) -> Option<&mut E> {
        self.edges.get_mut(id.as_usize())
    }

    fn try_add_edge(
        &mut self,
        src: &Self::VertexId,
        dst: &Self::VertexId,
        edge: E,
    ) -> Result<Self::EdgeId, AddEdgeError<E>> {
        if src.as_usize() >= self.vertices.len() {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::SourceAbsent));
        }

        if dst.as_usize() >= self.vertices.len() {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::DestinationAbsent));
        }

        let id = IdType::from_usize(self.edges.len());
        self.edges.push(edge);
        self.endpoints.push([*src, *dst]);

        let directions = Self::directions();
        self.vertices[src.as_usize()].edges[directions[0].index()].push(id);
        self.vertices[dst.as_usize()].edges[directions[1].index()].push(id);

        Ok(id)
    }

    fn remove_edge(&mut self, id: &Self::EdgeId) -> Option<E> {
        self.remove_edge_inner(*id, None)
    }

    fn clear_edges(&mut self) {
        self.edges.clear();
        self.endpoints.clear();

        for vertex in self.vertices.iter_mut() {
            vertex.edges[0].clear();
            vertex.edges[1].clear();
        }
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> MultiEdges<Ty> for AdjList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> Neighbors for AdjList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    type NeighborRef<'a> = (Self::VertexId, Self::EdgeId, Self::VertexId, Direction)
    where
        Self: 'a;

    type NeighborsIter<'a> = NeighborsIter<'a, Ty, Id>
    where
        Self: 'a;

    fn neighbors(&self, src: &Self::VertexId) -> Self::NeighborsIter<'_> {
        let vertex = self
            .vertices
            .get(src.as_usize())
            .expect("vertex does not exist");

        NeighborsIter {
            src: *src,
            edges: [&vertex.edges[0], &vertex.edges[1]],
            endpoints: self.endpoints.as_slice(),
            dir: 0,
            ty: PhantomData,
        }
    }

    fn neighbors_directed(&self, src: &Self::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
        let vertex = self
            .vertices
            .get(src.as_usize())
            .expect("vertex does not exist");

        let adj_dir = if !Ty::is_directed() {
            // If the graph is undirected, then the direction does not matter.
            // However, we need to index the "outgoing" edge list in the vertex,
            // because the "incoming" list is empty.
            Direction::Outgoing
        } else {
            dir
        };

        let mut edges: [&[Self::EdgeId]; 2] = [&[], &[]];
        edges[dir.index()] = &vertex.edges[adj_dir.index()];

        NeighborsIter {
            src: *src,
            edges,
            endpoints: self.endpoints.as_slice(),
            dir: dir.index(),
            ty: PhantomData,
        }
    }

    fn degree(&self, id: &Self::VertexId) -> usize {
        Ty::directions()
            .iter()
            .map(|dir| self.degree_directed(id, *dir))
            .sum()
    }

    fn degree_directed(&self, id: &Self::VertexId, dir: Direction) -> usize {
        let vertex = self
            .vertices
            .get(id.as_usize())
            .expect("vertex does not exist");

        let adj_dir = if !Ty::is_directed() {
            // If the graph is undirected, then the direction does not matter.
            // However, we need to index the "outgoing" edge list in the vertex,
            // because the "incoming" list is empty.
            Direction::Outgoing
        } else {
            dir
        };

        vertex.edges[adj_dir.index()].len()
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> Create<V, E, Ty> for AdjList<V, E, Ty, Id>
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

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> ConnectVertices<V, E, Ty> for AdjList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    fn connect_vertices<F>(&mut self, mut connect: F)
    where
        F: FnMut(&V, &V) -> Option<E>,
    {
        shared::connect_vertices::<Ty>(self.vertices.len(), |i, j| {
            let src = &self.vertices[i].data;
            let dst = &self.vertices[j].data;

            if let Some(edge) = connect(src, dst) {
                let src = Id::VertexId::from_usize(i);
                let dst = Id::VertexId::from_usize(j);

                self.add_edge(&src, &dst, edge);
            }
        })
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> Guarantee for AdjList<V, E, Ty, Id> {}

pub struct EdgeIdIter<'a, Id: GraphIdTypes> {
    src: Id::VertexId,
    dst: Id::VertexId,
    edges: &'a [Id::EdgeId],
    endpoints: &'a [[Id::VertexId; 2]],
}

impl<'a, Id: GraphIdTypes> Iterator for EdgeIdIter<'a, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    type Item = Id::EdgeId;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (edge, tail) = self.edges.split_first()?;
            self.edges = tail;

            let endpoints = self.endpoints[edge.as_usize()];

            if endpoints[0] == self.src && endpoints[1] == self.dst {
                return Some(*edge);
            }
        }
    }
}

pub struct NeighborsIter<'a, Ty, Id: GraphIdTypes> {
    src: Id::VertexId,
    edges: [&'a [Id::EdgeId]; 2],
    endpoints: &'a [[Id::VertexId; 2]],
    dir: usize,
    ty: PhantomData<Ty>,
}

impl<Ty: EdgeType, Id: GraphIdTypes> Iterator for NeighborsIter<'_, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    type Item = (Id::VertexId, Id::EdgeId, Id::VertexId, Direction);

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

        let endpoints = self.endpoints[edge.as_usize()];

        let neighbor = if endpoints[0] != self.src {
            endpoints[0]
        } else {
            endpoints[1]
        };

        if !Ty::is_directed() && neighbor == self.src {
            // Skip self-loop edge duplication.
            let (head, tail) = self.edges[self.dir].split_at(1);
            self.edges[self.dir] = tail;

            debug_assert_eq!(head[0], edge);
        }

        let dir = Direction::from_index(self.dir);

        Some((neighbor, edge, self.src, dir))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        core::marker::{Directed, Undirected},
        infra::{
            arbitrary::{ArbitraryId, Index},
            testing::check_consistency,
        },
        storage::tests::*,
    };

    #[test]
    fn basic_undirected() {
        test_basic::<Undirected, AdjList<_, _, _, DefaultId>>();
    }

    #[test]
    fn basic_directed() {
        test_basic::<Directed, AdjList<_, _, _, DefaultId>>();
    }

    #[test]
    fn multi_undirected() {
        test_multi::<Undirected, AdjList<_, _, _, DefaultId>>();
    }

    #[test]
    fn multi_directed() {
        test_multi::<Directed, AdjList<_, _, _, DefaultId>>();
    }

    #[test]
    fn connect_vertices_undirected() {
        test_connect_vertices::<Undirected, AdjList<_, _, _, DefaultId>>();
    }

    #[test]
    fn connect_vertices_directed() {
        test_connect_vertices::<Directed, AdjList<_, _, _, DefaultId>>();
    }

    #[test]
    fn neighbors_edge_cases_undirected() {
        test_neighbors_edge_cases::<Undirected, AdjList<_, _, _, DefaultId>>();
    }

    #[test]
    fn neighbors_edge_cases_directed() {
        test_neighbors_edge_cases::<Directed, AdjList<_, _, _, DefaultId>>();
    }

    #[test]
    fn fuzz_trophy1() {
        let mut graph = AdjList::<_, _, Undirected, ArbitraryId>::new();

        graph.add_vertex(0);
        graph.add_edge(&Index(0), &Index(0), -68);
        graph.clear_edges();
        graph.remove_edge_between(&Index(0), &Index(0));
    }

    #[test]
    fn fuzz_trophy2() {
        let mut graph = AdjList::<_, _, Directed, ArbitraryId>::new();

        graph.add_vertex(5);
        graph.add_edge(&Index(0), &Index(0), -1);
        graph.clear_edges();

        check_consistency(&graph).unwrap();
    }
}
