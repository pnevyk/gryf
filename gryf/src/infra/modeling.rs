use std::marker::PhantomData;

use arbitrary::Arbitrary;

use crate::core::{
    error::{AddEdgeError, AddEdgeErrorKind, AddVertexError},
    id::{EdgeId, IdType, IntegerIdType, VertexId},
    marker::{Direction, EdgeType},
    EdgeSet, GraphAdd, GraphBase, GraphFull, GraphMut, GraphRef, Neighbors, VertexSet,
};

use super::arbitrary::MutOp;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Arbitrary)]
pub enum RemovalBehavior {
    SwapRemove,
    TombStone,
}

#[derive(Debug, Clone, PartialEq, Eq, Arbitrary)]
pub struct ModelParams {
    allow_multi_edges: bool,
    allow_loops: bool,
    max_vertex_count: Option<usize>,
    max_edge_count: Option<usize>,
    max_remove_vertices: Option<usize>,
    max_remove_edges: Option<usize>,
    removal_behavior: RemovalBehavior,
}

impl Default for ModelParams {
    fn default() -> Self {
        Self {
            allow_multi_edges: false,
            allow_loops: false,
            max_vertex_count: None,
            max_edge_count: None,
            max_remove_vertices: None,
            max_remove_edges: None,
            removal_behavior: RemovalBehavior::SwapRemove,
        }
    }
}

impl ModelParams {
    pub fn allow_multi_edges(self) -> Self {
        Self {
            allow_multi_edges: true,
            ..self
        }
    }

    pub fn allow_loops(self) -> Self {
        Self {
            allow_loops: true,
            ..self
        }
    }

    pub fn max_vertex_count(self, value: usize) -> Self {
        Self {
            max_vertex_count: Some(value),
            ..self
        }
    }

    pub fn max_edge_count(self, value: usize) -> Self {
        Self {
            max_edge_count: Some(value),
            ..self
        }
    }

    pub fn max_remove_vertices(self, value: usize) -> Self {
        Self {
            max_remove_vertices: Some(value),
            ..self
        }
    }

    pub fn no_remove_vertices(self) -> Self {
        Self {
            max_remove_vertices: Some(0),
            ..self
        }
    }

    pub fn max_remove_edges(self, value: usize) -> Self {
        Self {
            max_remove_edges: Some(value),
            ..self
        }
    }

    pub fn no_remove_edges(self) -> Self {
        Self {
            max_remove_edges: Some(0),
            ..self
        }
    }

    pub fn no_remove(self) -> Self {
        self.no_remove_vertices().no_remove_edges()
    }

    pub fn swap_remove(self) -> Self {
        self.removal_behavior(RemovalBehavior::SwapRemove)
    }

    pub fn tomb_stone(self) -> Self {
        self.removal_behavior(RemovalBehavior::TombStone)
    }

    pub fn removal_behavior(self, value: RemovalBehavior) -> Self {
        Self {
            removal_behavior: value,
            ..self
        }
    }
}

#[derive(Debug, Clone)]
pub struct Model<V, E, Ty> {
    vertices: Vec<Option<V>>,
    edges: Vec<Option<E>>,
    neighbors: Vec<Option<(usize, usize)>>,
    removed_vertices: usize,
    removed_edges: usize,
    ty: PhantomData<Ty>,
    params: ModelParams,
}

impl<V, E, Ty: EdgeType> Model<V, E, Ty> {
    pub fn new(params: ModelParams) -> Self {
        Self {
            vertices: Vec::new(),
            edges: Vec::new(),
            neighbors: Vec::new(),
            removed_vertices: 0,
            removed_edges: 0,
            ty: PhantomData,
            params,
        }
    }

    pub fn check(&self, op: &MutOp<V, E>) -> bool {
        match op {
            MutOp::RemoveVertex(_)
                if self.params.max_remove_vertices == Some(self.removed_vertices) =>
            {
                return false
            }
            MutOp::RemoveEdge(_, _) if self.params.max_remove_edges == Some(self.removed_edges) => {
                return false
            }
            _ => {}
        }

        match op {
            MutOp::AddEdge(from, to, _) if !self.params.allow_loops && from == to => return false,
            _ => {}
        }

        if !self.params.allow_multi_edges {
            if let MutOp::AddEdge(from, to, _) = op {
                let n = self.vertex_bound();
                if let (Some(from), Some(to)) = (from.get(n), to.get(n)) {
                    if self.edge_exists(from, to) {
                        return false;
                    }
                }
            }
        }

        true
    }

    fn edge_exists(&self, from: usize, to: usize) -> bool {
        self.neighbors
            .iter()
            .filter_map(Option::as_ref)
            .copied()
            .any(|(u, v)| u == from && v == to || (!Ty::is_directed() && u == to && v == from))
    }
}

impl<V, E, Ty: EdgeType> GraphBase for Model<V, E, Ty> {
    type VertexId = VertexId;
    type EdgeId = EdgeId;
    type EdgeType = Ty;
}

impl<V, E, Ty: EdgeType> VertexSet for Model<V, E, Ty> {
    type VerticesByIdIter<'a> = IdIter<'a, VertexId, V>
    where
        Self: 'a;

    fn vertices_by_id(&self) -> Self::VerticesByIdIter<'_> {
        IdIter::new(&self.vertices)
    }

    fn vertex_count(&self) -> usize {
        let holes = match self.params.removal_behavior {
            RemovalBehavior::SwapRemove => 0,
            RemovalBehavior::TombStone => self.removed_vertices,
        };
        self.vertices.len() - holes
    }

    fn vertex_bound(&self) -> usize {
        self.vertices.len()
    }
}

impl<V, E, Ty: EdgeType> EdgeSet for Model<V, E, Ty> {
    type EdgesByIdIter<'a> = IdIter<'a, EdgeId, E>
    where
        Self: 'a;

    type EdgeIdIter<'a> = EdgeIdIter<'a, Ty>
    where
        Self: 'a;

    fn edges_by_id(&self) -> Self::EdgesByIdIter<'_> {
        IdIter::new(&self.edges)
    }

    fn edge_id(&self, from: &Self::VertexId, to: &Self::VertexId) -> Self::EdgeIdIter<'_> {
        let (from, to) = (from.as_usize(), to.as_usize());
        EdgeIdIter::new([from, to], &self.neighbors)
    }

    fn endpoints(&self, id: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)> {
        let &(from, to) = self.neighbors.get(id.as_usize())?.as_ref()?;
        Some((VertexId::from_usize(from), VertexId::from_usize(to)))
    }

    fn edge_count(&self) -> usize {
        let holes = match self.params.removal_behavior {
            RemovalBehavior::SwapRemove => 0,
            RemovalBehavior::TombStone => self.removed_edges,
        };
        self.edges.len() - holes
    }

    fn edge_bound(&self) -> usize {
        self.edges.len()
    }
}

impl<V, E, Ty: EdgeType> GraphRef<V, E> for Model<V, E, Ty> {
    type VertexRef<'a> = (VertexId, &'a V)
    where
        Self: 'a,
        V: 'a;

    type VerticesIter<'a> = VerticesIter<'a, V>
    where
        Self: 'a,
        V: 'a;

    type EdgeRef<'a> = (EdgeId, &'a E, VertexId, VertexId)
        where
            Self: 'a,
            E: 'a;

    type EdgesIter<'a> = EdgesIter<'a, E>
        where
            Self: 'a,
            E: 'a;

    fn vertices(&self) -> Self::VerticesIter<'_> {
        VerticesIter::new(&self.vertices)
    }

    fn edges(&self) -> Self::EdgesIter<'_> {
        EdgesIter::new(&self.edges, &self.neighbors)
    }

    fn vertex(&self, id: &Self::VertexId) -> Option<&V> {
        self.vertices.get(id.as_usize()).and_then(Option::as_ref)
    }

    fn edge(&self, id: &Self::EdgeId) -> Option<&E> {
        self.edges.get(id.as_usize()).and_then(Option::as_ref)
    }
}

impl<V, E, Ty: EdgeType> GraphMut<V, E> for Model<V, E, Ty> {
    fn vertex_mut(&mut self, id: &Self::VertexId) -> Option<&mut V> {
        self.vertices
            .get_mut(id.as_usize())
            .and_then(Option::as_mut)
    }

    fn edge_mut(&mut self, id: &Self::EdgeId) -> Option<&mut E> {
        self.edges.get_mut(id.as_usize()).and_then(Option::as_mut)
    }
}

impl<V, E, Ty: EdgeType> GraphAdd<V, E> for Model<V, E, Ty> {
    fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>> {
        if Some(self.vertex_count()) == self.params.max_vertex_count {
            return Err(AddVertexError::new(vertex));
        }

        let id = VertexId::from_usize(self.vertices.len());
        self.vertices.push(Some(vertex));

        Ok(id)
    }

    fn try_add_edge(
        &mut self,
        from: &Self::VertexId,
        to: &Self::VertexId,
        edge: E,
    ) -> Result<Self::EdgeId, AddEdgeError<E>> {
        if self.vertex(from).is_none() {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::TailAbsent));
        }

        if self.vertex(to).is_none() {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::HeadAbsent));
        }

        if Some(self.edge_count()) == self.params.max_edge_count {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::CapacityOverflow));
        }

        let (from, to) = (from.as_usize(), to.as_usize());

        if !self.params.allow_multi_edges && self.edge_exists(from, to) {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::MultiEdge));
        }

        let id = EdgeId::from_usize(self.edges.len());
        self.edges.push(Some(edge));
        self.neighbors.push(Some((from, to)));

        Ok(id)
    }
}

impl<V, E, Ty: EdgeType> GraphFull<V, E> for Model<V, E, Ty> {
    fn remove_vertex(&mut self, id: &Self::VertexId) -> Option<V> {
        self.vertex(id)?;

        let index = id.as_usize();
        self.removed_vertices += 1;

        match self.params.removal_behavior {
            RemovalBehavior::SwapRemove => {
                let mut i = 0;
                while i < self.edges.len() {
                    if let Some(&(from, to)) = self.neighbors[i].as_ref() {
                        if from == index || to == index {
                            self.edges.swap_remove(i);
                            self.neighbors.swap_remove(i);
                            continue;
                        }
                    }

                    i += 1;
                }

                self.vertices.swap_remove(index)
            }
            RemovalBehavior::TombStone => {
                self.neighbors
                    .iter_mut()
                    .zip(self.edges.iter_mut())
                    .filter(|(n, _)| {
                        if let Some(&(from, to)) = n.as_ref() {
                            from == index || to == index
                        } else {
                            false
                        }
                    })
                    .for_each(|(n, e)| {
                        n.take();
                        e.take();
                        self.removed_edges += 1;
                    });

                self.vertices[index].take()
            }
        }
    }

    fn remove_edge(&mut self, id: &Self::EdgeId) -> Option<E> {
        self.edge(id)?;

        let index = id.as_usize();
        self.removed_edges += 1;

        match self.params.removal_behavior {
            RemovalBehavior::SwapRemove => {
                self.neighbors.swap_remove(index);
                self.edges.swap_remove(index)
            }
            RemovalBehavior::TombStone => {
                self.neighbors[index].take();
                self.edges[index].take()
            }
        }
    }
}

impl<V, E, Ty: EdgeType> Neighbors for Model<V, E, Ty> {
    type NeighborRef<'a> = (VertexId, EdgeId, VertexId, Direction)
    where
        Self: 'a;

    type NeighborsIter<'a> = NeighborsIter<'a, Ty>
    where
        Self: 'a;

    fn neighbors_undirected(&self, from: &Self::VertexId) -> Self::NeighborsIter<'_> {
        NeighborsIter::new(from.as_usize(), &self.neighbors, None)
    }

    fn neighbors_directed(&self, from: &Self::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
        NeighborsIter::new(from.as_usize(), &self.neighbors, Some(dir))
    }
}

#[derive(Debug)]
pub struct IdIter<'a, Id, T> {
    data: &'a [Option<T>],
    index: usize,
    ty: PhantomData<Id>,
}

impl<'a, Id, T> IdIter<'a, Id, T> {
    fn new(data: &'a [Option<T>]) -> Self {
        Self {
            data,
            index: 0,
            ty: PhantomData,
        }
    }
}

impl<Id: IntegerIdType, T> Iterator for IdIter<'_, Id, T> {
    type Item = Id;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (head, tail) = self.data.split_first()?;
            self.data = tail;

            if head.is_some() {
                return Some(Id::from_usize(self.index));
            }

            self.index += 1;
        }
    }
}

#[derive(Debug)]
pub struct VerticesIter<'a, T> {
    data: &'a [Option<T>],
    index: usize,
}

impl<'a, T> VerticesIter<'a, T> {
    pub fn new(data: &'a [Option<T>]) -> Self {
        Self { data, index: 0 }
    }
}

impl<'a, T> Iterator for VerticesIter<'a, T> {
    type Item = (VertexId, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (head, tail) = self.data.split_first()?;
            self.data = tail;

            let id = VertexId::from_usize(self.index);
            self.index += 1;

            if let Some(value) = head {
                return Some((id, value));
            }
        }
    }
}

#[derive(Debug)]
pub struct EdgesIter<'a, T> {
    data: &'a [Option<T>],
    neighbors: &'a [Option<(usize, usize)>],
    index: usize,
}

impl<'a, T> EdgesIter<'a, T> {
    pub fn new(data: &'a [Option<T>], neighbors: &'a [Option<(usize, usize)>]) -> Self {
        Self {
            data,
            neighbors,
            index: 0,
        }
    }
}

impl<'a, T> Iterator for EdgesIter<'a, T> {
    type Item = (EdgeId, &'a T, VertexId, VertexId);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (head, tail) = self.data.split_first()?;
            self.data = tail;

            let (pair, tail) = self.neighbors.split_first()?;
            self.neighbors = tail;

            let id = EdgeId::from_usize(self.index);
            self.index += 1;

            if let Some(value) = head {
                let (from, to) = pair.unwrap();
                return Some((
                    id,
                    value,
                    VertexId::from_usize(from),
                    VertexId::from_usize(to),
                ));
            }
        }
    }
}

#[derive(Debug)]
pub struct EdgeIdIter<'a, Ty> {
    between: [usize; 2],
    neighbors: &'a [Option<(usize, usize)>],
    index: usize,
    ty: PhantomData<Ty>,
}

impl<'a, Ty> EdgeIdIter<'a, Ty> {
    pub fn new(between: [usize; 2], neighbors: &'a [Option<(usize, usize)>]) -> Self {
        Self {
            between,
            neighbors,
            index: 0,
            ty: PhantomData,
        }
    }
}

impl<'a, Ty: EdgeType> Iterator for EdgeIdIter<'a, Ty> {
    type Item = EdgeId;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (pair, tail) = self.neighbors.split_first()?;
            self.neighbors = tail;

            let index = EdgeId::from_usize(self.index);
            self.index += 1;

            if let Some(&(from, to)) = pair.as_ref() {
                let from_to = from == self.between[0] && to == self.between[1];
                let to_from =
                    !Ty::is_directed() && to == self.between[0] && from == self.between[1];

                if from_to || to_from {
                    return Some(index);
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct NeighborsIter<'a, Ty> {
    from: usize,
    neighbors: &'a [Option<(usize, usize)>],
    dir: Option<Direction>,
    index: usize,
    ty: PhantomData<Ty>,
}

impl<'a, Ty> NeighborsIter<'a, Ty> {
    pub fn new(
        from: usize,
        neighbors: &'a [Option<(usize, usize)>],
        dir: Option<Direction>,
    ) -> Self {
        Self {
            from,
            neighbors,
            dir,
            index: 0,
            ty: PhantomData,
        }
    }
}

impl<'a, Ty: EdgeType> Iterator for NeighborsIter<'a, Ty> {
    type Item = (VertexId, EdgeId, VertexId, Direction);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (pair, tail) = self.neighbors.split_first()?;
            self.neighbors = tail;

            let id = EdgeId::from_usize(self.index);
            self.index += 1;

            match (pair.as_ref(), self.dir, Ty::is_directed()) {
                // If vertex matches source and the graph is undirected.
                (Some(&(from, to)), _, false)
                // If the vertex matches source and the graph is directed but
                // the direction does not matter.
                | (Some(&(from, to)), None, true)
                // If the vertex matches source and the graph is directed and
                // the direction is outgoing.
                | (Some(&(from, to)), Some(Direction::Outgoing), true)
                    if from == self.from =>
                {
                    return Some((
                        VertexId::from_usize(to),
                        id,
                        VertexId::from_usize(from),
                        Direction::Outgoing,
                    ));
                }
                // If the vertex matches destination and the graph is
                // undirected, we still consider the edge outgoing.
                (Some(&(from, to)), _, false) if to == self.from => {
                    return Some((
                        VertexId::from_usize(from),
                        id,
                        VertexId::from_usize(to),
                        Direction::Outgoing,
                    ));
                }
                // If the vertex matches destination and the graph is directed
                // but the direction does not matter.
                (Some(&(from, to)), None, true)
                // If the vertex matches destination and the graph is directed
                // and the direction is incoming.
                | (Some(&(from, to)), Some(Direction::Incoming), true)
                    if to == self.from =>
                {
                    return Some((
                        VertexId::from_usize(from),
                        id,
                        VertexId::from_usize(to),
                        Direction::Incoming,
                    ));
                }
                _ => {}
            }
        }
    }
}
