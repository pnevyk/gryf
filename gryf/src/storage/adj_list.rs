//! [Adjacency list] graph representation.
//!
//! See [module](crate::storage) documentation for comparison with other
//! storages.
//!
//! [Adjacency list]: https://en.wikipedia.org/wiki/Adjacency_list

use std::marker::PhantomData;

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
pub use super::shared::{
    AdjVertex as Vertex, AdjVerticesIter as VerticesIter, EdgesIter, RangeIds as EdgeIds,
    RangeIds as VertexIds,
};

/// [Adjacency list] graph representation.
///
/// [Adjacency list]: https://en.wikipedia.org/wiki/Adjacency_list
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AdjList<V, E, Ty, Id: IdPair> {
    vertices: Vec<Vertex<Id, V>>,
    edges: Vec<E>,
    endpoints: Vec<[Id::VertexId; 2]>,
    ty: PhantomData<fn() -> (Ty, Id)>,
}

impl<V, E, Ty: EdgeType, Id: IdPair> AdjList<V, E, Ty, Id> {
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

impl<V, E, Ty: EdgeType> AdjList<V, E, Ty, DefaultId> {
    /// Creates a new empty storage with given ID pair.
    pub fn with_id<Id: IdPair>() -> AdjList<V, E, Ty, Id> {
        AdjList::new()
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> AdjList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    fn remove_edge_inner(&mut self, id: Id::EdgeId) -> Option<E> {
        let endpoints = self.endpoints.get(id.as_usize())?;

        for (i, dir) in Self::directions().iter().enumerate() {
            let endpoint = endpoints[i];

            Self::disconnect(
                &mut self.vertices[endpoint.as_usize()].edges[dir.index()],
                id,
            );
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
        if let Some(i) = edges
            .iter()
            .enumerate()
            .find_map(|(i, edge_id)| (edge_id == &id).then_some(i))
        {
            edges.swap_remove(i);
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

impl<V, E, Ty: EdgeType, Id: IdPair> GraphBase for AdjList<V, E, Ty, Id> {
    type VertexId = Id::VertexId;
    type EdgeId = Id::EdgeId;
    type EdgeType = Ty;
}

impl<V, E, Ty: EdgeType, Id: IdPair> Neighbors for AdjList<V, E, Ty, Id>
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

    fn neighbors_undirected(&self, from: &Self::VertexId) -> Self::NeighborsIter<'_> {
        let vertex = self
            .vertices
            .get(from.as_usize())
            .expect("vertex does not exist");

        NeighborsIter {
            from: *from,
            edges: [&vertex.edges[0], &vertex.edges[1]],
            endpoints: self.endpoints.as_slice(),
            dir: 0,
            ty: PhantomData,
        }
    }

    fn neighbors_directed(&self, from: &Self::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
        let vertex = self
            .vertices
            .get(from.as_usize())
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
            from: *from,
            edges,
            endpoints: self.endpoints.as_slice(),
            dir: dir.index(),
            ty: PhantomData,
        }
    }

    fn degree_undirected(&self, id: &Self::VertexId) -> usize {
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

impl<V, E, Ty: EdgeType, Id: IdPair> VertexSet for AdjList<V, E, Ty, Id>
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

impl<V, E, Ty: EdgeType, Id: IdPair> EdgeSet for AdjList<V, E, Ty, Id>
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
        match self.vertices.get(from.as_usize()) {
            Some(vertex) => EdgeIdIter {
                from: *from,
                to: *to,
                edges: &vertex.edges[Direction::Outgoing.index()],
                endpoints: self.endpoints.as_slice(),
                ty: PhantomData,
            },
            None => EdgeIdIter {
                from: *from,
                to: *to,
                edges: &[],
                endpoints: &[],
                ty: PhantomData,
            },
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

impl<V, E, Ty: EdgeType, Id: IdPair> GraphRef<V, E> for AdjList<V, E, Ty, Id>
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
        self.vertices.get(id.as_usize()).map(|vertex| &vertex.attr)
    }

    fn edge(&self, id: &Self::EdgeId) -> Option<&E> {
        self.edges.get(id.as_usize())
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> GraphMut<V, E> for AdjList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    fn vertex_mut(&mut self, id: &Self::VertexId) -> Option<&mut V> {
        self.vertices
            .get_mut(id.as_usize())
            .map(|vertex| &mut vertex.attr)
    }

    fn edge_mut(&mut self, id: &Self::EdgeId) -> Option<&mut E> {
        self.edges.get_mut(id.as_usize())
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> GraphAdd<V, E> for AdjList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>> {
        let index = self.vertices.len();
        self.vertices.push(Vertex::new(vertex));
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

        let id = IdType::from_usize(self.edges.len());
        self.edges.push(edge);
        self.endpoints.push([*from, *to]);

        let directions = Self::directions();
        self.vertices[from.as_usize()].edges[directions[0].index()].push(id);
        self.vertices[to.as_usize()].edges[directions[1].index()].push(id);

        Ok(id)
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> GraphFull<V, E> for AdjList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
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
                self.remove_edge_inner(edge_id);
            }
        }

        // Remove the vertex from the graph.
        let vertex = self.vertices.swap_remove(id.as_usize());

        // If `swap_remove` actually moved an existing vertex somewhere, we need
        // to fix its id in the entire graph.
        if id.as_usize() < self.vertices.len() {
            self.relocate_vertex(IdType::from_usize(self.vertices.len()), *id);
        }

        Some(vertex.attr)
    }

    fn remove_edge(&mut self, id: &Self::EdgeId) -> Option<E> {
        self.remove_edge_inner(*id)
    }

    fn clear(&mut self) {
        self.vertices.clear();
        self.edges.clear();
        self.endpoints.clear();
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

impl<V, E, Ty: EdgeType, Id: IdPair> MultiEdge for AdjList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
}

impl<V, E, Ty: EdgeType, Id: IdPair> Create<V, E> for AdjList<V, E, Ty, Id>
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

impl<V, E, Ty: EdgeType, Id: IdPair> ConnectVertices<V, E> for AdjList<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    fn connect_vertices<F>(&mut self, mut connect: F)
    where
        F: FnMut(&V, &V) -> Option<E>,
    {
        shared::connect_vertices::<Ty>(self.vertices.len(), |i, j| {
            let from = &self.vertices[i].attr;
            let to = &self.vertices[j].attr;

            if let Some(edge) = connect(from, to) {
                let from = Id::VertexId::from_usize(i);
                let to = Id::VertexId::from_usize(j);

                self.add_edge(&from, &to, edge);
            }
        })
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> Guarantee for AdjList<V, E, Ty, Id> {}

pub struct EdgeIdIter<'a, Ty: EdgeType, Id: IdPair> {
    from: Id::VertexId,
    to: Id::VertexId,
    edges: &'a [Id::EdgeId],
    endpoints: &'a [[Id::VertexId; 2]],
    ty: PhantomData<Ty>,
}

impl<'a, Ty: EdgeType, Id: IdPair> Iterator for EdgeIdIter<'a, Ty, Id>
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

            let from_to = endpoints[0] == self.from && endpoints[1] == self.to;
            let to_from =
                !Ty::is_directed() && endpoints[0] == self.to && endpoints[1] == self.from;

            if from_to || to_from {
                return Some(*edge);
            }
        }
    }
}

pub struct NeighborsIter<'a, Ty, Id: IdPair> {
    from: Id::VertexId,
    edges: [&'a [Id::EdgeId]; 2],
    endpoints: &'a [[Id::VertexId; 2]],
    dir: usize,
    ty: PhantomData<Ty>,
}

impl<Ty: EdgeType, Id: IdPair> Iterator for NeighborsIter<'_, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    type Item = NeighborRef<Id::VertexId, Id::EdgeId>;

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

        let neighbor = if endpoints[0] != self.from {
            endpoints[0]
        } else {
            endpoints[1]
        };

        if !Ty::is_directed() && neighbor == self.from {
            // Skip self-loop edge duplication.
            let (head, tail) = self.edges[self.dir].split_at(1);
            self.edges[self.dir] = tail;

            debug_assert_eq!(head[0], edge);
        }

        let dir = Direction::from_index(self.dir);

        Some(NeighborRef {
            id: neighbor,
            edge,
            pred: self.from,
            dir,
        })
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
        test_basic::<AdjList<_, _, Undirected, DefaultId>>();
    }

    #[test]
    fn basic_directed() {
        test_basic::<AdjList<_, _, Directed, DefaultId>>();
    }

    #[test]
    fn multi_undirected() {
        test_multi::<AdjList<_, _, Undirected, DefaultId>>();
    }

    #[test]
    fn multi_directed() {
        test_multi::<AdjList<_, _, Directed, DefaultId>>();
    }

    #[test]
    fn connect_vertices_undirected() {
        test_connect_vertices::<AdjList<_, _, Undirected, DefaultId>>();
    }

    #[test]
    fn connect_vertices_directed() {
        test_connect_vertices::<AdjList<_, _, Directed, DefaultId>>();
    }

    #[test]
    fn neighbors_edge_cases_undirected() {
        test_neighbors_edge_cases::<AdjList<_, _, Undirected, DefaultId>>();
    }

    #[test]
    fn neighbors_edge_cases_directed() {
        test_neighbors_edge_cases::<AdjList<_, _, Directed, DefaultId>>();
    }

    #[test]
    fn fuzz_trophy1() {
        let mut graph = AdjList::<_, _, Undirected, ArbitraryId>::new();

        graph.add_vertex(0);
        graph.add_edge(&Index(0), &Index(0), -68);
        graph.clear_edges();
        graph.remove_edge_any_between(&Index(0), &Index(0));
    }

    #[test]
    fn fuzz_trophy2() {
        let mut graph = AdjList::<_, _, Directed, ArbitraryId>::new();

        graph.add_vertex(5);
        graph.add_edge(&Index(0), &Index(0), -1);
        graph.clear_edges();

        check_consistency(&graph).unwrap();
    }

    #[test]
    fn fuzz_trophy3() {
        let mut graph = AdjList::<_, _, Directed, ArbitraryId>::new();

        graph.add_vertex(126);
        graph.add_edge(&Index(0), &Index(0), 0);
        graph.add_vertex(55);
        graph.add_edge(&Index(1), &Index(1), 0);
        graph.remove_vertex(&Index(0));

        assert_eq!(graph.edge_count(), 1);
    }

    #[test]
    fn fuzz_trophy4() {
        let mut graph = AdjList::<_, _, Undirected, ArbitraryId>::new();

        graph.add_vertex(0);
        graph.add_vertex(-49);
        graph.add_vertex(1);
        graph.add_edge(&Index(0), &Index(2), 0);

        let edge = graph.edge_id_any(&Index(2), &Index(0));
        println!("{edge:?}");

        graph.remove_edge_any_between(&Index(2), &Index(0));

        assert_eq!(graph.edge_count(), 0);
    }
}
