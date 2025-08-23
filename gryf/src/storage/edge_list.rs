//! [Edge list] graph representation.
//!
//! See [module](crate::storage) documentation for comparison with other
//! storages.
//!
//! [Edge list]: https://en.wikipedia.org/wiki/Edge_list

use std::{iter::Enumerate, marker::PhantomData, slice};

use crate::core::{
    EdgeSet, GraphAdd, GraphBase, GraphFull, GraphMut, GraphRef, Neighbors, VertexSet,
    base::{EdgeRef, NeighborRef, VertexRef},
    connect::ConnectVertices,
    create::Create,
    error::{AddEdgeError, AddEdgeErrorKind, AddVertexError},
    id::{CompactIdMap, DefaultId, IdPair, IdType, IntegerIdType},
    marker::{Direction, EdgeType},
    props::{Guarantee, MultiEdge},
};

use super::shared;
pub use super::shared::{EdgesIter, RangeIds as VertexIds, RangeIds as EdgeIds, VerticesIter};

/// [Edge list] graph representation.
///
/// [Edge list]: https://en.wikipedia.org/wiki/Edge_list
#[derive(Debug)]
pub struct EdgeList<V, E, Ty, Id: IdPair> {
    vertices: Vec<V>,
    edges: Vec<E>,
    endpoints: Vec<[Id::VertexId; 2]>,
    ty: PhantomData<Ty>,
}

impl<V, E, Ty: EdgeType, Id: IdPair> EdgeList<V, E, Ty, Id> {
    /// Creates a new empty storage.
    pub fn new() -> Self {
        Self {
            vertices: Vec::new(),
            edges: Vec::new(),
            endpoints: Vec::new(),
            ty: PhantomData,
        }
    }
}

impl<V, E, Ty: EdgeType> EdgeList<V, E, Ty, DefaultId> {
    /// Creates a new empty storage with given ID pair.
    pub fn with_id<Id: IdPair>() -> EdgeList<V, E, Ty, Id> {
        EdgeList::new()
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> EdgeList<V, E, Ty, Id>
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

impl<V, E, Ty: EdgeType, Id: IdPair> GraphBase for EdgeList<V, E, Ty, Id> {
    type VertexId = Id::VertexId;
    type EdgeId = Id::EdgeId;
    type EdgeType = Ty;
}

impl<V, E, Ty: EdgeType, Id: IdPair> Neighbors for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    type NeighborRef<'a>
        = NeighborRef<Self::VertexId, Self::EdgeId>
    where
        Self: 'a;

    type NeighborsIter<'a>
        = NeighborsIter<'a, Ty, Id>
    where
        Self: 'a;

    fn neighbors_undirected(&self, from: &Id::VertexId) -> Self::NeighborsIter<'_> {
        self.vertex(from).expect("vertex does not exist");

        NeighborsIter {
            from: *from,
            edges: self.endpoints.as_slice(),
            dir: None,
            index: 0,
            self_loop: None,
            ty: PhantomData,
        }
    }

    fn neighbors_directed(&self, from: &Id::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
        self.vertex(from).expect("vertex does not exist");

        NeighborsIter {
            from: *from,
            edges: self.endpoints.as_slice(),
            dir: Some(dir),
            index: 0,
            self_loop: None,
            ty: PhantomData,
        }
    }

    fn degree_undirected(&self, id: &Self::VertexId) -> usize {
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
            self.degree_undirected(id)
        }
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> VertexSet for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
{
    type VerticesByIdIter<'a>
        = VertexIds<Self::VertexId>
    where
        Self: 'a;

    fn vertices_by_id(&self) -> Self::VerticesByIdIter<'_> {
        (0..self.vertices.len()).into()
    }

    fn vertex_count(&self) -> usize {
        self.vertices.len()
    }

    fn vertex_bound(&self) -> usize
    where
        Self::VertexId: IntegerIdType,
    {
        self.vertex_count()
    }

    fn contains_vertex(&self, id: &Self::VertexId) -> bool {
        self.vertices.get(id.as_usize()).is_some()
    }

    fn vertex_id_map(&self) -> CompactIdMap<Self::VertexId> {
        CompactIdMap::isomorphic(self.vertices.len())
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> EdgeSet for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    type EdgesByIdIter<'a>
        = EdgeIds<Self::EdgeId>
    where
        Self: 'a;

    type EdgeIdIter<'a>
        = EdgeIdIter<'a, Ty, Id>
    where
        Self: 'a;

    fn edges_by_id(&self) -> Self::EdgesByIdIter<'_> {
        (0..self.edges.len()).into()
    }

    fn edge_id(&self, from: &Self::VertexId, to: &Self::VertexId) -> Self::EdgeIdIter<'_> {
        EdgeIdIter {
            from: *from,
            to: *to,
            endpoints: self.endpoints.iter().enumerate(),
            ty: PhantomData,
        }
    }

    fn endpoints(&self, id: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)> {
        self.endpoints
            .get(id.as_usize())
            .map(|endpoints| (endpoints[0], endpoints[1]))
    }

    fn edge_count(&self) -> usize {
        self.edges.len()
    }

    fn edge_bound(&self) -> usize
    where
        Self::EdgeId: IntegerIdType,
    {
        self.edge_count()
    }

    fn contains_edge(&self, id: &Self::EdgeId) -> bool {
        self.edges.get(id.as_usize()).is_some()
    }

    fn edge_id_map(&self) -> CompactIdMap<Self::EdgeId> {
        CompactIdMap::isomorphic(self.edges.len())
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> GraphRef<V, E> for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    type VertexRef<'a>
        = VertexRef<'a, Self::VertexId, V>
    where
        Self: 'a,
        V: 'a;

    type VerticesIter<'a>
        = VerticesIter<'a, Id, V>
    where
        Self: 'a,
        V: 'a;

    type EdgeRef<'a>
        = EdgeRef<'a, Self::VertexId, Self::EdgeId, E>
    where
        Self: 'a,
        E: 'a;

    type EdgesIter<'a>
        = EdgesIter<'a, Id, E>
    where
        Self: 'a,
        E: 'a;

    fn vertices(&self) -> Self::VerticesIter<'_> {
        VerticesIter::new(self.vertices.iter())
    }

    fn edges(&self) -> Self::EdgesIter<'_> {
        EdgesIter::new(self.edges.iter(), self.endpoints.iter())
    }

    fn vertex(&self, id: &Self::VertexId) -> Option<&V> {
        self.vertices.get(id.as_usize())
    }

    fn edge(&self, id: &Self::EdgeId) -> Option<&E> {
        self.edges.get(id.as_usize())
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> GraphMut<V, E> for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    fn vertex_mut(&mut self, id: &Self::VertexId) -> Option<&mut V> {
        self.vertices.get_mut(id.as_usize())
    }

    fn edge_mut(&mut self, id: &Self::EdgeId) -> Option<&mut E> {
        self.edges.get_mut(id.as_usize())
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> GraphAdd<V, E> for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>> {
        let index = self.vertices.len();
        self.vertices.push(vertex);
        Ok(index.into())
    }

    fn try_add_edge(
        &mut self,
        from: &Self::VertexId,
        to: &Self::VertexId,
        edge: E,
    ) -> Result<Self::EdgeId, AddEdgeError<E>> {
        if from.as_usize() >= self.vertices.len() {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::TailAbsent));
        }

        if to.as_usize() >= self.vertices.len() {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::HeadAbsent));
        }

        self.endpoints.push([*from, *to]);
        let index = self.edges.len();
        self.edges.push(edge);
        Ok(index.into())
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> GraphFull<V, E> for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
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
        let vertex = self.vertices.swap_remove(id.as_usize());

        // If `swap_remove` actually moved an existing vertex somewhere, we need
        // to fix its id in the entire graph.
        if id.as_usize() < self.vertices.len() {
            self.relocate_vertex(Id::VertexId::from_usize(self.vertices.len()), *id);
        }

        Some(vertex)
    }

    fn remove_edge(&mut self, id: &Self::EdgeId) -> Option<E> {
        self.edge(id)?;
        self.endpoints.swap_remove(id.as_usize());
        Some(self.edges.swap_remove(id.as_usize()))
    }

    fn clear(&mut self) {
        self.vertices.clear();
        self.edges.clear();
        self.endpoints.clear();
    }

    fn clear_edges(&mut self) {
        self.edges.clear();
        self.endpoints.clear();
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> MultiEdge for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
}

impl<V, E, Ty: EdgeType, Id: IdPair> Create<V, E> for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    fn with_capacity(vertex_capacity: usize, edge_capacity: usize) -> Self {
        Self {
            vertices: Vec::with_capacity(vertex_capacity),
            edges: Vec::with_capacity(edge_capacity),
            endpoints: Vec::with_capacity(edge_capacity),
            ty: PhantomData,
        }
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> ConnectVertices<V, E> for EdgeList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    fn connect_vertices<F>(&mut self, mut connect: F)
    where
        F: FnMut(&V, &V) -> Option<E>,
    {
        shared::connect_vertices::<Ty>(self.vertices.len(), |i, j| {
            let from = &self.vertices[i];
            let to = &self.vertices[j];

            if let Some(edge) = connect(from, to) {
                let from = Id::VertexId::from_usize(i);
                let to = Id::VertexId::from_usize(j);

                self.add_edge(&from, &to, edge);
            }
        })
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> Guarantee for EdgeList<V, E, Ty, Id> {}

pub struct EdgeIdIter<'a, Ty: EdgeType, Id: IdPair> {
    from: Id::VertexId,
    to: Id::VertexId,
    endpoints: Enumerate<slice::Iter<'a, [Id::VertexId; 2]>>,
    ty: PhantomData<Ty>,
}

impl<'a, Ty: EdgeType, Id: IdPair> Iterator for EdgeIdIter<'a, Ty, Id>
where
    Id::EdgeId: IntegerIdType,
{
    type Item = Id::EdgeId;

    fn next(&mut self) -> Option<Self::Item> {
        for (index, endpoints) in self.endpoints.by_ref() {
            let from_to = endpoints[0] == self.from && endpoints[1] == self.to;
            let to_from =
                !Ty::is_directed() && endpoints[0] == self.to && endpoints[1] == self.from;
            if from_to || to_from {
                return Some(Id::EdgeId::from_usize(index));
            }
        }

        None
    }
}

pub struct NeighborsIter<'a, Ty, Id: IdPair> {
    from: Id::VertexId,
    edges: &'a [[Id::VertexId; 2]],
    index: usize,
    dir: Option<Direction>,
    self_loop: Option<(Id::EdgeId, Direction)>,
    ty: PhantomData<Ty>,
}

impl<'a, Ty: EdgeType, Id: IdPair> Iterator for NeighborsIter<'a, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    type Item = NeighborRef<Id::VertexId, Id::EdgeId>;

    fn next(&mut self) -> Option<Self::Item> {
        if Ty::is_directed()
            && let Some((id, dir)) = self.self_loop.take()
        {
            return Some(NeighborRef {
                id: self.from,
                edge: id,
                pred: self.from,
                dir,
            });
        }

        loop {
            let (endpoints, tail) = self.edges.split_first()?;
            self.edges = tail;

            let id = Id::EdgeId::from_usize(self.index);
            self.index += 1;

            let neighbor = match (self.dir, Ty::is_directed()) {
                (Some(Direction::Outgoing), true) => {
                    if endpoints[0] == self.from {
                        Some((endpoints[1], Direction::Outgoing))
                    } else {
                        None
                    }
                }
                (Some(Direction::Incoming), true) => {
                    if endpoints[1] == self.from {
                        Some((endpoints[0], Direction::Incoming))
                    } else {
                        None
                    }
                }
                (Some(dir), false) => {
                    if endpoints[0] == self.from {
                        Some((endpoints[1], dir))
                    } else if endpoints[1] == self.from {
                        Some((endpoints[0], dir))
                    } else {
                        None
                    }
                }
                (None, _) => {
                    if endpoints[0] == self.from {
                        Some((endpoints[1], Direction::Outgoing))
                    } else if endpoints[1] == self.from {
                        Some((endpoints[0], Direction::Incoming))
                    } else {
                        None
                    }
                }
            };

            if let Some((neighbor, dir)) = neighbor {
                if Ty::is_directed() && neighbor == self.from && self.dir.is_none() {
                    // There is only one edge-item in edge list for a self-loop
                    // in directed graph. But we need to report the edge twice,
                    // for each direction.
                    self.self_loop = Some((id, dir.opposite()));
                }

                return Some(NeighborRef {
                    id: neighbor,
                    edge: id,
                    pred: self.from,
                    dir,
                });
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
        test_basic::<EdgeList<_, _, Undirected, DefaultId>>();
    }

    #[test]
    fn basic_directed() {
        test_basic::<EdgeList<_, _, Directed, DefaultId>>();
    }

    #[test]
    fn multi_undirected() {
        test_multi::<EdgeList<_, _, Undirected, DefaultId>>();
    }

    #[test]
    fn multi_directed() {
        test_multi::<EdgeList<_, _, Directed, DefaultId>>();
    }

    #[test]
    fn connect_vertices_undirected() {
        test_connect_vertices::<EdgeList<_, _, Undirected, DefaultId>>();
    }

    #[test]
    fn connect_vertices_directed() {
        test_connect_vertices::<EdgeList<_, _, Directed, DefaultId>>();
    }

    #[test]
    fn neighbors_edge_cases_undirected() {
        test_neighbors_edge_cases::<EdgeList<_, _, Undirected, DefaultId>>();
    }

    #[test]
    fn neighbors_edge_cases_directed() {
        test_neighbors_edge_cases::<EdgeList<_, _, Directed, DefaultId>>();
    }

    #[test]
    fn fuzz_trophy1() {
        let mut graph = EdgeList::<_, (), Undirected, ArbitraryId>::new();

        graph.add_vertex(0);
        graph.remove_vertex(&Index(0));
        graph.remove_vertex(&Index(0));
    }
}
