use std::marker::PhantomData;

use crate::core::{
    EdgeSet, GraphAdd, GraphBase, GraphFull, GraphMut, GraphRef, Neighbors, VertexSet,
    base::{EdgeRef, NeighborRef, VertexRef},
    connect::ConnectVertices,
    create::Create,
    error::{AddEdgeError, AddEdgeErrorKind, AddVertexError},
    id::{CompactIdMap, DefaultId, IdPair, IdType, IntegerIdType},
    marker::{Direction, EdgeType},
    props::Guarantee,
};

use super::shared;
pub use super::shared::{RangeIds as VertexIds, VerticesIter};

#[derive(Debug)]
pub struct AdjMatrix<V, E, Ty, Id> {
    matrix: raw::Matrix<E, Ty, Id>,
    vertices: Vec<V>,
    n_edges: usize,
}

impl<V, E, Ty: EdgeType, Id: IdPair> AdjMatrix<V, E, Ty, Id> {
    pub fn new() -> Self {
        Self {
            matrix: raw::Matrix::with_capacity(8),
            vertices: Vec::new(),
            n_edges: 0,
        }
    }
}

impl<V, E, Ty: EdgeType> AdjMatrix<V, E, Ty, DefaultId> {
    pub fn with_id<Id: IdPair>() -> AdjMatrix<V, E, Ty, Id> {
        AdjMatrix::new()
    }
}

impl<V, E, Ty: EdgeType> Default for AdjMatrix<V, E, Ty, DefaultId> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> GraphBase for AdjMatrix<V, E, Ty, Id> {
    type VertexId = Id::VertexId;
    type EdgeId = Id::EdgeId;
    type EdgeType = Ty;
}

impl<V, E, Ty: EdgeType, Id: IdPair> Neighbors for AdjMatrix<V, E, Ty, Id>
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
        self.vertex(from).expect("vertex does not exist");

        let filter = if Ty::is_directed() {
            None
        } else {
            // Use only the outgoing direction for undirected graphs.
            Some(Direction::Outgoing)
        };

        NeighborsIter {
            matrix: self.matrix.detach(),
            from: *from,
            other: 0,
            vertex_count: self.vertex_count(),
            filter,
            dir: Direction::Outgoing,
        }
    }

    fn neighbors_directed(&self, from: &Self::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
        self.vertex(from).expect("vertex does not exist");

        NeighborsIter {
            matrix: self.matrix.detach(),
            from: *from,
            other: 0,
            vertex_count: self.vertex_count(),
            filter: Some(dir),
            dir,
        }
    }

    fn degree_undirected(&self, id: &Self::VertexId) -> usize {
        Ty::directions()
            .iter()
            .map(|dir| self.degree_directed(id, *dir))
            .sum()
    }

    fn degree_directed(&self, id: &Self::VertexId, dir: Direction) -> usize {
        self.vertex(id).expect("vertex does not exist");

        self.matrix.degree_directed(*id, dir, self.vertices.len())
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> VertexSet for AdjMatrix<V, E, Ty, Id>
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

    fn vertex_id_map(&self) -> CompactIdMap<Self::VertexId>
    where
        Self::VertexId: IntegerIdType,
    {
        CompactIdMap::isomorphic(self.vertices.len())
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> EdgeSet for AdjMatrix<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    type EdgesByIdIter<'a>
        = EdgesByIdIter<'a, Ty, Id>
    where
        Self: 'a;

    type EdgeIdIter<'a>
        = std::option::IntoIter<Self::EdgeId>
    where
        Self: 'a;

    fn edges_by_id(&self) -> Self::EdgesByIdIter<'_> {
        EdgesByIdIter {
            matrix: self.matrix.detach(),
            index: 0,
            edge_bound: self.edge_bound(),
            ty: PhantomData,
        }
    }

    fn edge_id(&self, from: &Self::VertexId, to: &Self::VertexId) -> Self::EdgeIdIter<'_> {
        let id = self.matrix.index(from.as_usize(), to.as_usize());
        self.matrix.get(id).map(|_| id).into_iter()
    }

    fn endpoints(&self, id: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)> {
        let (row, col) = self.matrix.coords(*id);
        if row < self.vertex_count() {
            Some((row.into(), col.into()))
        } else {
            None
        }
    }

    fn edge_count(&self) -> usize {
        self.n_edges
    }

    fn edge_bound(&self) -> usize
    where
        Self::EdgeId: IntegerIdType,
    {
        self.matrix.edge_bound(self.vertex_count())
    }

    fn contains_edge(&self, id: &Self::EdgeId) -> bool {
        self.matrix.contains(*id)
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> GraphRef<V, E> for AdjMatrix<V, E, Ty, Id>
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
        = EdgesIter<'a, E, Ty, Id>
    where
        Self: 'a,
        E: 'a;

    fn vertices(&self) -> Self::VerticesIter<'_> {
        VerticesIter::new(self.vertices.iter())
    }

    fn edges(&self) -> Self::EdgesIter<'_> {
        EdgesIter {
            matrix: &self.matrix,
            index: 0,
            edge_bound: self.edge_bound(),
            ty: PhantomData,
        }
    }

    fn vertex(&self, id: &Self::VertexId) -> Option<&V> {
        self.vertices.get(id.as_usize())
    }

    fn edge(&self, id: &Self::EdgeId) -> Option<&E> {
        self.matrix.get(*id)
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> GraphMut<V, E> for AdjMatrix<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    fn vertex_mut(&mut self, id: &Self::VertexId) -> Option<&mut V> {
        self.vertices.get_mut(id.as_usize())
    }

    fn edge_mut(&mut self, id: &Self::EdgeId) -> Option<&mut E> {
        self.matrix.get_mut(*id)
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> GraphAdd<V, E> for AdjMatrix<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>> {
        self.matrix.ensure_capacity(self.vertex_count() + 1);

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

        let id = self.matrix.index(from.as_usize(), to.as_usize());

        if self.matrix.contains(id) {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::MultiEdge));
        }

        self.matrix.insert(id, edge);
        self.n_edges += 1;

        Ok(id)
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> GraphFull<V, E> for AdjMatrix<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    fn remove_vertex(&mut self, id: &Self::VertexId) -> Option<V> {
        self.vertex(id)?;

        let index = id.as_usize();

        // Remove incident edges.
        for i in 0..self.vertices.len() {
            let edge_id = self.matrix.index(index, i);
            if self.matrix.remove(edge_id).is_some() {
                self.n_edges -= 1;
            }

            if Ty::is_directed() {
                let edge_id = self.matrix.index(i, index);
                if self.matrix.remove(edge_id).is_some() {
                    self.n_edges -= 1;
                }
            }
        }

        let vertex = self.vertices.swap_remove(index);

        // Relocate the edges of the last vertex, if it is going to replace the
        // removed vertex.
        if index < self.vertices.len() {
            let last_index = self.vertices.len();

            for i in 0..self.vertices.len() {
                let edge_id = self.matrix.index(last_index, i);
                if let Some(edge) = self.matrix.remove(edge_id) {
                    let edge_id = self.matrix.index(index, i);
                    self.matrix.insert(edge_id, edge);
                }

                if Ty::is_directed() {
                    let edge_id = self.matrix.index(i, last_index);
                    if let Some(edge) = self.matrix.remove(edge_id) {
                        let edge_id = self.matrix.index(i, index);
                        self.matrix.insert(edge_id, edge);
                    }
                }
            }

            // Handle self-loops.
            let edge_id = self.matrix.index(last_index, last_index);
            if let Some(edge) = self.matrix.remove(edge_id) {
                let edge_id = self.matrix.index(index, index);
                self.matrix.insert(edge_id, edge);
            }
        }

        Some(vertex)
    }

    fn remove_edge(&mut self, id: &Self::EdgeId) -> Option<E> {
        match self.matrix.remove(*id) {
            Some(edge) => {
                self.n_edges -= 1;
                Some(edge)
            }
            None => None,
        }
    }

    fn clear(&mut self) {
        self.matrix.clear();
        self.vertices.clear();
        self.n_edges = 0;
    }

    fn clear_edges(&mut self) {
        self.matrix.clear();
        self.n_edges = 0;
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> Create<V, E> for AdjMatrix<V, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    fn with_capacity(vertex_capacity: usize, _edge_capacity: usize) -> Self {
        Self {
            matrix: raw::Matrix::with_capacity(vertex_capacity),
            vertices: Vec::with_capacity(vertex_capacity),
            n_edges: 0,
        }
    }
}

impl<V, E, Ty: EdgeType, Id: IdPair> ConnectVertices<V, E> for AdjMatrix<V, E, Ty, Id>
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

impl<V, E, Ty: EdgeType, Id: IdPair> Guarantee for AdjMatrix<V, E, Ty, Id> {}

pub struct EdgesByIdIter<'a, Ty, Id> {
    matrix: raw::DetachedMatrix<'a, Ty, Id>,
    index: usize,
    edge_bound: usize,
    ty: PhantomData<fn() -> Id>,
}

impl<'a, Ty: EdgeType, Id: IdPair> Iterator for EdgesByIdIter<'a, Ty, Id>
where
    Id::EdgeId: IntegerIdType,
{
    type Item = Id::EdgeId;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.index == self.edge_bound {
                return None;
            }

            let id = Id::EdgeId::from_usize(self.index);
            self.index += 1;

            if self.matrix.contains(id) {
                return Some(id);
            }
        }
    }
}

pub struct EdgesIter<'a, E, Ty, Id> {
    matrix: &'a raw::Matrix<E, Ty, Id>,
    index: usize,
    edge_bound: usize,
    ty: PhantomData<fn() -> Id>,
}

impl<'a, E, Ty: EdgeType, Id: IdPair> Iterator for EdgesIter<'a, E, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    type Item = EdgeRef<'a, Id::VertexId, Id::EdgeId, E>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.index == self.edge_bound {
                return None;
            }

            let id = Id::EdgeId::from_usize(self.index);
            self.index += 1;

            if let Some(attr) = self.matrix.get(id) {
                let (row, col) = self.matrix.coords(id);
                return Some(EdgeRef {
                    id,
                    attr,
                    from: row.into(),
                    to: col.into(),
                });
            }
        }
    }
}

pub struct NeighborsIter<'a, Ty, Id: IdPair> {
    matrix: raw::DetachedMatrix<'a, Ty, Id>,
    from: Id::VertexId,
    other: usize,
    vertex_count: usize,
    filter: Option<Direction>,
    dir: Direction,
}

impl<'a, Ty: EdgeType, Id: IdPair> Iterator for NeighborsIter<'a, Ty, Id>
where
    Id::VertexId: IntegerIdType,
    Id::EdgeId: IntegerIdType,
{
    type Item = NeighborRef<Id::VertexId, Id::EdgeId>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.other == self.vertex_count {
                match (self.filter, self.dir) {
                    // Exhausted the direction.
                    (Some(_), _) => return None,
                    // Exhausted both directions.
                    (None, Direction::Incoming) => return None,
                    // Switching to incoming.
                    (None, Direction::Outgoing) => {
                        self.other = 0;
                        self.dir = Direction::Incoming;
                    }
                }
            } else {
                let to = self.other;
                self.other += 1;

                let id = match self.dir {
                    Direction::Outgoing => self.matrix.index(self.from.as_usize(), to),
                    Direction::Incoming => self.matrix.index(to, self.from.as_usize()),
                };

                if self.matrix.contains(id) {
                    return Some(NeighborRef {
                        id: Id::VertexId::from_usize(to),
                        edge: id,
                        pred: self.from,
                        dir: self.dir,
                    });
                }
            }
        }
    }
}

mod raw {
    use std::{
        marker::PhantomData,
        mem::{self, MaybeUninit},
    };

    use bitvec::prelude::*;

    use crate::core::{
        id::{IdPair, IdType, IntegerIdType},
        marker::{Direction, EdgeType},
        matrix::*,
    };

    #[derive(Debug)]
    struct FlaggedVec<T> {
        flags: BitVec,
        data: Vec<MaybeUninit<T>>,
    }

    impl<T> FlaggedVec<T> {
        pub fn with_capacity(capacity: usize) -> Self {
            Self {
                flags: BitVec::with_capacity(capacity),
                data: Vec::with_capacity(capacity),
            }
        }

        pub fn resize(&mut self, new_len: usize) {
            self.flags.resize(new_len, false);
            self.data.resize_with(new_len, || MaybeUninit::uninit());
        }

        pub fn len(&self) -> usize {
            self.data.len()
        }

        pub fn contains(&self, index: usize) -> bool {
            self.flags[index]
        }

        pub fn get(&self, index: usize) -> Option<&T> {
            if self.flags[index] {
                // SAFETY: self.flags and self.data are consistent with each
                // other. If self.flags confirms that there is an item at given
                // index, then the corresponding data are guaranteed to be
                // initialized.
                Some(unsafe { self.data[index].assume_init_ref() })
            } else {
                None
            }
        }

        pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
            if self.flags[index] {
                // SAFETY: See FlaggedVec::get.
                Some(unsafe { self.data[index].assume_init_mut() })
            } else {
                None
            }
        }

        pub fn push(&mut self, value: Option<T>) {
            match value {
                Some(value) => {
                    self.flags.push(true);
                    self.data.push(MaybeUninit::new(value));
                }
                None => {
                    self.flags.push(false);
                    self.data.push(MaybeUninit::uninit());
                }
            }
        }

        pub fn insert(&mut self, index: usize, value: T) -> Option<T> {
            let prev = if self.fetch_set(index, true) {
                // There was already an item, we return it.

                let slot = &mut self.data[index];
                let prev = mem::replace(slot, MaybeUninit::uninit());
                // SAFETY: See FlaggedVec::get.
                Some(unsafe { prev.assume_init() })
            } else {
                None
            };

            // SAFETY: Initializing value of MaybeUninit.
            unsafe { self.data[index].as_mut_ptr().write(value) };

            prev
        }

        pub fn remove(&mut self, index: usize) -> Option<T> {
            if self.fetch_set(index, false) {
                let slot = &mut self.data[index];
                let prev = mem::replace(slot, MaybeUninit::uninit());
                // SAFETY: See FlaggedVec::get.
                Some(unsafe { prev.assume_init() })
            } else {
                None
            }
        }

        pub fn clear(&mut self) {
            // Just clear all items, but do not shrink the vectors.

            for i in self.flags.iter_ones() {
                // SAFETY: See FlaggedVec::get.
                unsafe { self.data[i].assume_init_drop() };
            }

            self.flags.clear();
            self.flags.resize(self.data.len(), false);
        }

        pub fn detach(&self) -> &BitVec {
            &self.flags
        }

        fn fetch_set(&mut self, index: usize, value: bool) -> bool {
            let mut flag = self.flags.get_mut(index).unwrap();
            flag.replace(value)
        }
    }

    impl<T> Drop for FlaggedVec<T> {
        fn drop(&mut self) {
            for (flag, value) in self.flags.iter().by_vals().zip(self.data.iter_mut()) {
                if flag {
                    // SAFETY: See FlaggedVec::get.
                    unsafe { MaybeUninit::assume_init_drop(value) };
                }
            }
        }
    }

    impl<T> Default for FlaggedVec<T> {
        fn default() -> Self {
            Self {
                flags: Default::default(),
                data: Default::default(),
            }
        }
    }

    struct FlaggedIter<T> {
        inner: std::iter::Zip<
            <BitVec as IntoIterator>::IntoIter,
            <Vec<MaybeUninit<T>> as IntoIterator>::IntoIter,
        >,
    }

    impl<T> Iterator for FlaggedIter<T> {
        type Item = Option<T>;

        fn next(&mut self) -> Option<Self::Item> {
            self.inner.next().map(|(flag, value)| {
                if flag {
                    // SAFETY: See FlaggedVec::get.
                    Some(unsafe { value.assume_init() })
                } else {
                    None
                }
            })
        }
    }

    impl<E> MatrixLinearStorage<E> for FlaggedVec<E> {
        fn with_capacity(capacity: usize) -> Self {
            Self::with_capacity(capacity)
        }

        fn resize_with_none(&mut self, new_len: usize) {
            self.resize(new_len)
        }

        fn push(&mut self, value: Option<E>) {
            self.push(value)
        }

        fn len(&self) -> usize {
            self.len()
        }

        fn into_entries(mut self) -> impl Iterator<Item = Option<E>> {
            let flags = mem::take(&mut self.flags);
            let data = mem::take(&mut self.data);

            FlaggedIter {
                inner: flags.into_iter().zip(data),
            }
        }
    }

    // The matrix could be implemented safely as Vec<Option<E>>. However, that
    // ties generic type E to any usage of the matrix, which causes lifetime
    // troubles in some of the traits (namely Neighbors). Having the possibility
    // to detach the information of the edges presence from the data solves this
    // problem. Unfortunately, it brings unsafe code and a bit more logic
    // complexity. We hide this unsafe code behind FlaggedVec abstraction.
    #[derive(Debug)]
    pub struct Matrix<E, Ty, Id> {
        data: FlaggedVec<E>,
        capacity: usize,
        ty: PhantomData<(Ty, Id)>,
    }

    impl<E, Ty: EdgeType, Id: IdPair> Matrix<E, Ty, Id> {
        pub fn with_capacity(capacity: usize) -> Self {
            if capacity == 0 {
                return Self {
                    data: Default::default(),
                    capacity,
                    ty: PhantomData,
                };
            }

            let capacity = capacity.next_power_of_two();
            let len = linear_len::<Ty>(capacity);
            let mut data = FlaggedVec::with_capacity(len);
            data.resize(len);

            Self {
                data,
                capacity,
                ty: PhantomData,
            }
        }

        pub fn ensure_capacity(&mut self, capacity: usize) {
            if self.capacity < capacity {
                self.capacity = (self.capacity * 2).max(capacity.next_power_of_two());
                resize::<E, Ty, _>(&mut self.data, self.capacity);
            }
        }

        pub fn contains(&self, id: Id::EdgeId) -> bool {
            self.data.contains(id.as_usize())
        }

        pub fn get(&self, id: Id::EdgeId) -> Option<&E> {
            self.data.get(id.as_usize())
        }

        pub fn get_mut(&mut self, id: Id::EdgeId) -> Option<&mut E> {
            self.data.get_mut(id.as_usize())
        }

        pub fn insert(&mut self, id: Id::EdgeId, edge: E) {
            self.data.insert(id.as_usize(), edge);
        }

        pub fn remove(&mut self, id: Id::EdgeId) -> Option<E> {
            self.data.remove(id.as_usize())
        }

        pub fn index(&self, row: usize, col: usize) -> Id::EdgeId {
            Id::EdgeId::from_usize(index::<Ty>(row, col, self.capacity))
        }

        pub fn coords(&self, id: Id::EdgeId) -> (usize, usize) {
            coords::<Ty>(id.as_usize(), self.capacity)
        }

        pub fn edge_bound(&self, vertex_count: usize) -> usize {
            index::<Ty>(vertex_count, 0, self.capacity)
        }

        pub fn degree_directed(&self, v: Id::VertexId, dir: Direction, n_vertices: usize) -> usize {
            let v = v.as_usize();
            let mut degree = 0;

            if Ty::is_directed() {
                let (mut i, stride) = match dir {
                    Direction::Outgoing => (index::<Ty>(v, 0, self.capacity), 1),
                    Direction::Incoming => (index::<Ty>(0, v, self.capacity), self.capacity),
                };

                for _ in 0..n_vertices {
                    if self.data.contains(i) {
                        degree += 1;
                    }

                    i += stride;
                }
            } else {
                for col in 0..n_vertices {
                    let i = index::<Ty>(v, col, self.capacity);

                    if self.data.contains(i) {
                        degree += 1;

                        // Self-loop counts twice.
                        degree += (v == col) as usize;
                    }
                }
            }

            degree
        }

        pub fn detach(&self) -> DetachedMatrix<'_, Ty, Id> {
            DetachedMatrix {
                data: self.data.detach(),
                capacity: self.capacity,
                ty: PhantomData,
            }
        }
    }

    impl<E, Ty: EdgeType, Id: IdPair> Matrix<E, Ty, Id> {
        pub fn clear(&mut self) {
            self.data.clear();
        }
    }

    pub struct DetachedMatrix<'a, Ty, Id> {
        data: &'a BitVec,
        capacity: usize,
        ty: PhantomData<(Ty, Id)>,
    }

    impl<Ty: EdgeType, Id: IdPair> DetachedMatrix<'_, Ty, Id>
    where
        Id::EdgeId: IntegerIdType,
    {
        pub fn contains(&self, id: Id::EdgeId) -> bool {
            self.data[id.as_usize()]
        }

        pub fn index(&self, row: usize, col: usize) -> Id::EdgeId {
            Id::EdgeId::from_usize(index::<Ty>(row, col, self.capacity))
        }

        #[allow(unused)]
        pub fn coords(&self, id: Id::EdgeId) -> (usize, usize) {
            coords::<Ty>(id.as_usize(), self.capacity)
        }
    }

    impl<Ty: EdgeType, Id> std::fmt::Debug for DetachedMatrix<'_, Ty, Id> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            if f.alternate() {
                writeln!(f, "DetachedMatrix {{")?;
                for row in 0..self.capacity {
                    write!(f, "    ")?;
                    for col in 0..self.capacity {
                        if col > 0 {
                            write!(f, " ")?;
                        }

                        let index = index::<Ty>(row, col, self.capacity);
                        write!(f, "{}", self.data[index] as usize)?;
                    }
                    writeln!(f)?;
                }
                writeln!(f, "}}")
            } else {
                f.debug_struct("DetachedMatrix")
                    .field("data", &self.data)
                    .field("capacity", &self.capacity)
                    .finish()
            }
        }
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
        test_basic::<AdjMatrix<_, _, Undirected, DefaultId>>();
    }

    #[test]
    fn basic_directed() {
        test_basic::<AdjMatrix<_, _, Directed, DefaultId>>();
    }

    #[test]
    fn connect_vertices_undirected() {
        test_connect_vertices::<AdjMatrix<_, _, Undirected, DefaultId>>();
    }

    #[test]
    fn connect_vertices_directed() {
        test_connect_vertices::<AdjMatrix<_, _, Directed, DefaultId>>();
    }

    #[test]
    fn neighbors_edge_cases_undirected() {
        test_neighbors_edge_cases::<AdjMatrix<_, _, Undirected, DefaultId>>();
    }

    #[test]
    fn neighbors_edge_cases_directed() {
        test_neighbors_edge_cases::<AdjMatrix<_, _, Directed, DefaultId>>();
    }

    #[test]
    fn fuzz_trophy1() {
        let mut graph = AdjMatrix::<_, _, Undirected, ArbitraryId>::new();

        graph.add_vertex(10);
        graph.add_edge(&Index(0), &Index(0), 0);

        check_consistency(&graph).unwrap();
    }

    #[test]
    fn fuzz_trophy2() {
        let mut graph = AdjMatrix::<_, _, Undirected, ArbitraryId>::new();

        graph.add_vertex(1);
        graph.add_vertex(100);
        graph.add_edge(&Index(1), &Index(1), 2);
        graph.remove_vertex(&Index(0));

        check_consistency(&graph).unwrap();
    }

    #[test]
    fn fuzz_trophy3() {
        let mut graph = AdjMatrix::<_, _, Directed, ArbitraryId>::new();

        graph.add_vertex(1);
        graph.add_vertex(42);
        graph.add_edge(&Index(1), &Index(1), -34);
        graph.add_vertex(-34);
        graph.add_vertex(-34);
        graph.add_vertex(-34);
        graph.add_vertex(-34);
        graph.add_vertex(-34);
        graph.add_vertex(-34);
        graph.add_vertex(0);

        check_consistency(&graph).unwrap();
    }

    #[test]
    fn fuzz_trophy4() {
        let mut graph = AdjMatrix::<_, _, Directed, ArbitraryId>::new();

        graph.add_vertex(-55);
        graph.add_vertex(-127);
        graph.add_vertex(-1);
        graph.add_edge(&Index(1), &Index(2), -33);
        graph.add_vertex(-13);
        graph.add_vertex(21);
        graph.add_vertex(-13);
        graph.add_vertex(74);
        graph.add_vertex(0);
        graph.add_vertex(35);

        check_consistency(&graph).unwrap();
    }
}
