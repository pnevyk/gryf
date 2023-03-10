use std::marker::PhantomData;

use arbitrary::Arbitrary;

use crate::core::{
    index::{EdgeIndex, NumIndexType, VertexIndex},
    marker::{Direction, EdgeType},
    AddEdgeError, AddEdgeErrorKind, AddVertexError, Edges, EdgesBase, EdgesMut, GraphBase,
    Neighbors, Vertices, VerticesBase, VerticesMut,
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
            MutOp::AddEdge(src, dst, _) if !self.params.allow_loops && src == dst => return false,
            _ => {}
        }

        if !self.params.allow_multi_edges {
            if let MutOp::AddEdge(src, dst, _) = op {
                let n = self.vertex_bound();
                if let (Some(src), Some(dst)) = (src.get(n), dst.get(n)) {
                    if self.edge_exists(src, dst) {
                        return false;
                    }
                }
            }
        }

        true
    }

    fn edge_exists(&self, src: usize, dst: usize) -> bool {
        self.neighbors
            .iter()
            .filter_map(Option::as_ref)
            .copied()
            .any(|(u, v)| u == src && v == dst || (!Ty::is_directed() && u == dst && v == src))
    }
}

impl<V, E, Ty> GraphBase for Model<V, E, Ty> {
    type VertexIndex = VertexIndex;
    type EdgeIndex = EdgeIndex;
}

impl<V, E, Ty> VerticesBase for Model<V, E, Ty> {
    type VertexIndicesIter<'a> = IndexIter<'a, VertexIndex, V>
    where
        Self: 'a;

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

    fn vertex_indices(&self) -> Self::VertexIndicesIter<'_> {
        IndexIter::new(&self.vertices)
    }
}

impl<V, E, Ty> Vertices<V> for Model<V, E, Ty> {
    type VertexRef<'a> = (VertexIndex, &'a V)
    where
        Self: 'a,
        V: 'a;

    type VerticesIter<'a> = VerticesIter<'a, V>
    where
        Self: 'a,
        V: 'a;

    fn vertex(&self, index: &Self::VertexIndex) -> Option<&V> {
        self.vertices.get(index.to_usize()).and_then(Option::as_ref)
    }

    fn vertices(&self) -> Self::VerticesIter<'_> {
        VerticesIter::new(&self.vertices)
    }
}

impl<V, E, Ty> VerticesMut<V> for Model<V, E, Ty> {
    fn vertex_mut(&mut self, index: &Self::VertexIndex) -> Option<&mut V> {
        self.vertices
            .get_mut(index.to_usize())
            .and_then(Option::as_mut)
    }

    fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexIndex, AddVertexError<V>> {
        if Some(self.vertex_count()) == self.params.max_vertex_count {
            return Err(AddVertexError::new(vertex));
        }

        let index = VertexIndex::from_usize(self.vertices.len());
        self.vertices.push(Some(vertex));

        Ok(index)
    }

    fn remove_vertex(&mut self, index: &Self::VertexIndex) -> Option<V> {
        self.vertex(index)?;

        let index = index.to_usize();
        self.removed_vertices += 1;

        match self.params.removal_behavior {
            RemovalBehavior::SwapRemove => {
                let mut i = 0;
                while i < self.edges.len() {
                    if let Some(&(src, dst)) = self.neighbors[i].as_ref() {
                        if src == index || dst == index {
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
                        if let Some(&(src, dst)) = n.as_ref() {
                            src == index || dst == index
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
}

impl<V, E, Ty: EdgeType> EdgesBase<Ty> for Model<V, E, Ty> {
    type EdgeIndicesIter<'a> = IndexIter<'a, EdgeIndex, E>
    where
        Self: 'a;
    type EdgeIndexIter<'a> = EdgeIndexIter<'a, Ty>
    where
        Self: 'a;

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

    fn endpoints(&self, index: &Self::EdgeIndex) -> Option<(Self::VertexIndex, Self::VertexIndex)> {
        let &(src, dst) = self.neighbors.get(index.to_usize())?.as_ref()?;
        Some((VertexIndex::from_usize(src), VertexIndex::from_usize(dst)))
    }

    fn edge_index(
        &self,
        src: &Self::VertexIndex,
        dst: &Self::VertexIndex,
    ) -> Self::EdgeIndexIter<'_> {
        let (src, dst) = (src.to_usize(), dst.to_usize());
        EdgeIndexIter::new([src, dst], &self.neighbors)
    }

    fn edge_indices(&self) -> Self::EdgeIndicesIter<'_> {
        IndexIter::new(&self.edges)
    }
}

impl<V, E, Ty: EdgeType> Edges<E, Ty> for Model<V, E, Ty> {
    type EdgeRef<'a> = (EdgeIndex, &'a E, VertexIndex, VertexIndex)
    where
        Self: 'a,
        E: 'a;

    type EdgesIter<'a> = EdgesIter<'a, E>
    where
        Self: 'a,
        E: 'a;

    fn edge(&self, index: &Self::EdgeIndex) -> Option<&E> {
        self.edges.get(index.to_usize()).and_then(Option::as_ref)
    }

    fn edges(&self) -> Self::EdgesIter<'_> {
        EdgesIter::new(&self.edges, &self.neighbors)
    }
}

impl<V, E, Ty: EdgeType> EdgesMut<E, Ty> for Model<V, E, Ty> {
    fn edge_mut(&mut self, index: &Self::EdgeIndex) -> Option<&mut E> {
        self.edges
            .get_mut(index.to_usize())
            .and_then(Option::as_mut)
    }

    fn try_add_edge(
        &mut self,
        src: &Self::VertexIndex,
        dst: &Self::VertexIndex,
        edge: E,
    ) -> Result<Self::EdgeIndex, AddEdgeError<E>> {
        if self.vertex(src).is_none() {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::SourceAbsent));
        }

        if self.vertex(dst).is_none() {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::DestinationAbsent));
        }

        if Some(self.edge_count()) == self.params.max_edge_count {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::CapacityOverflow));
        }

        let (src, dst) = (src.to_usize(), dst.to_usize());

        if !self.params.allow_multi_edges && self.edge_exists(src, dst) {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::MultiEdge));
        }

        let index = EdgeIndex::from_usize(self.edges.len());
        self.edges.push(Some(edge));
        self.neighbors.push(Some((src, dst)));

        Ok(index)
    }

    fn remove_edge(&mut self, index: &Self::EdgeIndex) -> Option<E> {
        self.edge(index)?;

        let index = index.to_usize();
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
    type NeighborRef<'a> = (VertexIndex, EdgeIndex, VertexIndex, Direction)
    where
        Self: 'a;

    type NeighborsIter<'a> = NeighborsIter<'a, Ty>
    where
        Self: 'a;

    fn neighbors(&self, src: &Self::VertexIndex) -> Self::NeighborsIter<'_> {
        NeighborsIter::new(src.to_usize(), &self.neighbors, None)
    }

    fn neighbors_directed(
        &self,
        src: &Self::VertexIndex,
        dir: Direction,
    ) -> Self::NeighborsIter<'_> {
        NeighborsIter::new(src.to_usize(), &self.neighbors, Some(dir))
    }
}

#[derive(Debug)]
pub struct IndexIter<'a, Idx, T> {
    data: &'a [Option<T>],
    index: usize,
    ty: PhantomData<Idx>,
}

impl<'a, Idx, T> IndexIter<'a, Idx, T> {
    fn new(data: &'a [Option<T>]) -> Self {
        Self {
            data,
            index: 0,
            ty: PhantomData,
        }
    }
}

impl<Idx: NumIndexType, T> Iterator for IndexIter<'_, Idx, T> {
    type Item = Idx;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (head, tail) = self.data.split_first()?;
            self.data = tail;

            if head.is_some() {
                return Some(Idx::from_usize(self.index));
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
    type Item = (VertexIndex, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (head, tail) = self.data.split_first()?;
            self.data = tail;

            let index = VertexIndex::from_usize(self.index);
            self.index += 1;

            if let Some(value) = head {
                return Some((index, value));
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
    type Item = (EdgeIndex, &'a T, VertexIndex, VertexIndex);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (head, tail) = self.data.split_first()?;
            self.data = tail;

            let (pair, tail) = self.neighbors.split_first()?;
            self.neighbors = tail;

            let index = EdgeIndex::from_usize(self.index);
            self.index += 1;

            if let Some(value) = head {
                let (src, dst) = pair.unwrap();
                return Some((
                    index,
                    value,
                    VertexIndex::from_usize(src),
                    VertexIndex::from_usize(dst),
                ));
            }
        }
    }
}

#[derive(Debug)]
pub struct EdgeIndexIter<'a, Ty> {
    between: [usize; 2],
    neighbors: &'a [Option<(usize, usize)>],
    index: usize,
    ty: PhantomData<Ty>,
}

impl<'a, Ty> EdgeIndexIter<'a, Ty> {
    pub fn new(between: [usize; 2], neighbors: &'a [Option<(usize, usize)>]) -> Self {
        Self {
            between,
            neighbors,
            index: 0,
            ty: PhantomData,
        }
    }
}

impl<'a, Ty: EdgeType> Iterator for EdgeIndexIter<'a, Ty> {
    type Item = EdgeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (pair, tail) = self.neighbors.split_first()?;
            self.neighbors = tail;

            let index = EdgeIndex::from_usize(self.index);
            self.index += 1;

            if let Some(&(src, dst)) = pair.as_ref() {
                let src_dst = src == self.between[0] && dst == self.between[1];
                let dst_src =
                    !Ty::is_directed() && dst == self.between[0] && src == self.between[1];

                if src_dst || dst_src {
                    return Some(index);
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct NeighborsIter<'a, Ty> {
    src: usize,
    neighbors: &'a [Option<(usize, usize)>],
    dir: Option<Direction>,
    index: usize,
    ty: PhantomData<Ty>,
}

impl<'a, Ty> NeighborsIter<'a, Ty> {
    pub fn new(
        src: usize,
        neighbors: &'a [Option<(usize, usize)>],
        dir: Option<Direction>,
    ) -> Self {
        Self {
            src,
            neighbors,
            dir,
            index: 0,
            ty: PhantomData,
        }
    }
}

impl<'a, Ty: EdgeType> Iterator for NeighborsIter<'a, Ty> {
    type Item = (VertexIndex, EdgeIndex, VertexIndex, Direction);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (pair, tail) = self.neighbors.split_first()?;
            self.neighbors = tail;

            let index = EdgeIndex::from_usize(self.index);
            self.index += 1;

            match (pair.as_ref(), self.dir, Ty::is_directed()) {
                // If vertex matches source and the graph is undirected.
                (Some(&(src, dst)), _, false)
                // If the vertex matches source and the graph is directed but
                // the direction does not matter.
                | (Some(&(src, dst)), None, true)
                // If the vertex matches source and the graph is directed and
                // the direction is outgoing.
                | (Some(&(src, dst)), Some(Direction::Outgoing), true)
                    if src == self.src =>
                {
                    return Some((
                        VertexIndex::from_usize(dst),
                        index,
                        VertexIndex::from_usize(src),
                        Direction::Outgoing,
                    ));
                }
                // If the vertex matches destination and the graph is
                // undirected, we still consider the edge outgoing.
                (Some(&(src, dst)), _, false) if dst == self.src => {
                    return Some((
                        VertexIndex::from_usize(src),
                        index,
                        VertexIndex::from_usize(dst),
                        Direction::Outgoing,
                    ));
                }
                // If the vertex matches destination and the graph is directed
                // but the direction does not matter.
                (Some(&(src, dst)), None, true)
                // If the vertex matches destination and the graph is directed
                // and the direction is incoming.
                | (Some(&(src, dst)), Some(Direction::Incoming), true)
                    if dst == self.src =>
                {
                    return Some((
                        VertexIndex::from_usize(src),
                        index,
                        VertexIndex::from_usize(dst),
                        Direction::Incoming,
                    ));
                }
                _ => {}
            }
        }
    }
}
