use std::marker::PhantomData;

use crate::{
    common::CompactIdMap,
    core::{
        id::{DefaultId, GraphIdTypes, NumIdType},
        marker::{Direction, EdgeType},
        AddEdgeError, AddEdgeErrorKind, AddVertexError, ConnectVertices, Create, Edges, EdgesBase,
        EdgesMut, GraphBase, Guarantee, Neighbors, Vertices, VerticesBase, VerticesMut,
    },
};

use gryf_derive::{EdgesBaseWeak, EdgesWeak, VerticesBaseWeak, VerticesWeak};

// TODO: Remove these imports once hygiene of procedural macros is fixed.
use crate::core::{EdgesBaseWeak, EdgesWeak, VerticesBaseWeak, VerticesWeak, WeakRef};

use super::shared;
pub use super::shared::{RangeIds as VertexIds, VerticesIter};

#[derive(Debug, VerticesBaseWeak, VerticesWeak, EdgesBaseWeak, EdgesWeak)]
pub struct AdjMatrix<V, E, Ty, Id> {
    matrix: raw::Matrix<E, Ty, Id>,
    vertices: Vec<V>,
    n_edges: usize,
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> AdjMatrix<V, E, Ty, Id>
where
    Id::VertexId: NumIdType,
    Id::EdgeId: NumIdType,
{
    pub fn new() -> Self {
        Self {
            matrix: raw::Matrix::with_capacity(8),
            vertices: Vec::new(),
            n_edges: 0,
        }
    }
}

impl<V, E, Ty: EdgeType> Default for AdjMatrix<V, E, Ty, DefaultId> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> GraphBase for AdjMatrix<V, E, Ty, Id> {
    type VertexId = Id::VertexId;
    type EdgeId = Id::EdgeId;
    type EdgeType = Ty;
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> VerticesBase for AdjMatrix<V, E, Ty, Id>
where
    Id::VertexId: NumIdType,
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
        Id::VertexId: NumIdType,
    {
        CompactIdMap::isomorphic(self.vertex_count())
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> Vertices<V> for AdjMatrix<V, E, Ty, Id>
where
    Id::VertexId: NumIdType,
{
    type VertexRef<'a> = (Self::VertexId, &'a V)
    where
        Self: 'a,
        V: 'a;

    type VerticesIter<'a> = VerticesIter<'a, Id, V>
    where
        Self: 'a,
        V: 'a;

    fn vertex(&self, index: &Self::VertexId) -> Option<&V> {
        self.vertices.get(index.to_usize())
    }

    fn vertices(&self) -> Self::VerticesIter<'_> {
        VerticesIter::new(self.vertices.iter())
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> VerticesMut<V> for AdjMatrix<V, E, Ty, Id>
where
    Id::VertexId: NumIdType,
    Id::EdgeId: NumIdType,
{
    fn vertex_mut(&mut self, index: &Self::VertexId) -> Option<&mut V> {
        self.vertices.get_mut(index.to_usize())
    }

    fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>> {
        self.matrix.ensure_capacity(self.vertex_count() + 1);

        let index = self.vertices.len();
        self.vertices.push(vertex);
        Ok(index.into())
    }

    fn remove_vertex(&mut self, index: &Self::VertexId) -> Option<V> {
        self.vertex(index)?;

        let index = index.to_usize();

        // Remove incident edges.
        for i in 0..self.vertices.len() {
            let edge_index = self.matrix.index(index, i);
            if self.matrix.remove(edge_index).is_some() {
                self.n_edges -= 1;
            }

            if Ty::is_directed() {
                let edge_index = self.matrix.index(i, index);
                if self.matrix.remove(edge_index).is_some() {
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
                let edge_index = self.matrix.index(last_index, i);
                if let Some(edge) = self.matrix.remove(edge_index) {
                    let edge_index = self.matrix.index(index, i);
                    self.matrix.insert(edge_index, edge);
                }

                if Ty::is_directed() {
                    let edge_index = self.matrix.index(i, last_index);
                    if let Some(edge) = self.matrix.remove(edge_index) {
                        let edge_index = self.matrix.index(i, index);
                        self.matrix.insert(edge_index, edge);
                    }
                }
            }

            // Handle self-loops.
            let edge_index = self.matrix.index(last_index, last_index);
            if let Some(edge) = self.matrix.remove(edge_index) {
                let edge_index = self.matrix.index(index, index);
                self.matrix.insert(edge_index, edge);
            }
        }

        Some(vertex)
    }

    fn clear(&mut self) {
        self.matrix.clear();
        self.vertices.clear();
        self.n_edges = 0;
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> EdgesBase<Ty> for AdjMatrix<V, E, Ty, Id>
where
    Id::VertexId: NumIdType,
    Id::EdgeId: NumIdType,
{
    type EdgeIdsIter<'a> = EdgeIdsIter<'a, Ty, Id>
    where
        Self: 'a;
    type EdgeIdIter<'a> = std::option::IntoIter<Self::EdgeId>
    where
        Self: 'a;

    fn edge_count(&self) -> usize {
        self.n_edges
    }

    fn edge_bound(&self) -> usize {
        self.matrix.index(self.vertex_count(), 0).to_usize()
    }

    fn endpoints(&self, index: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)> {
        let (row, col) = self.matrix.coords(*index);
        if row < self.vertex_count() {
            Some((row.into(), col.into()))
        } else {
            None
        }
    }

    fn edge_id(&self, src: &Self::VertexId, dst: &Self::VertexId) -> Self::EdgeIdIter<'_> {
        let index = self.matrix.index(src.to_usize(), dst.to_usize());
        self.matrix.get(index).map(|_| index).into_iter()
    }

    fn edge_ids(&self) -> Self::EdgeIdsIter<'_> {
        EdgeIdsIter {
            matrix: self.matrix.detach(),
            index: 0,
            edge_bound: self.edge_bound(),
            ty: PhantomData,
        }
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> Edges<E, Ty> for AdjMatrix<V, E, Ty, Id>
where
    Id::VertexId: NumIdType,
    Id::EdgeId: NumIdType,
{
    type EdgeRef<'a> = (Self::EdgeId, &'a E, Self::VertexId, Self::VertexId)
    where
        Self: 'a,
        E: 'a;

    type EdgesIter<'a> = EdgesIter<'a, E, Ty, Id>
    where
        Self: 'a,
        E:'a;

    fn edge(&self, index: &Self::EdgeId) -> Option<&E> {
        self.matrix.get(*index)
    }

    fn edges(&self) -> Self::EdgesIter<'_> {
        EdgesIter {
            matrix: &self.matrix,
            index: 0,
            edge_bound: self.edge_bound(),
            ty: PhantomData,
        }
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> EdgesMut<E, Ty> for AdjMatrix<V, E, Ty, Id>
where
    Id::VertexId: NumIdType,
    Id::EdgeId: NumIdType,
{
    fn edge_mut(&mut self, index: &Id::EdgeId) -> Option<&mut E> {
        self.matrix.get_mut(*index)
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

        let index = self.matrix.index(src.to_usize(), dst.to_usize());

        if self.matrix.contains(index) {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::MultiEdge));
        }

        self.matrix.insert(index, edge);
        self.n_edges += 1;

        Ok(index)
    }

    fn remove_edge(&mut self, index: &Self::EdgeId) -> Option<E> {
        match self.matrix.remove(*index) {
            Some(edge) => {
                self.n_edges -= 1;
                Some(edge)
            }
            None => None,
        }
    }

    fn clear_edges(&mut self) {
        self.matrix.clear();
        self.n_edges = 0;
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> Neighbors for AdjMatrix<V, E, Ty, Id>
where
    Id::VertexId: NumIdType,
    Id::EdgeId: NumIdType,
{
    type NeighborRef<'a> = (Self::VertexId, Self::EdgeId, Self::VertexId, Direction)
    where
        Self: 'a;

    type NeighborsIter<'a> = NeighborsIter<'a, Ty, Id>
    where
        Self: 'a;

    fn neighbors(&self, src: &Self::VertexId) -> Self::NeighborsIter<'_> {
        self.vertex(src).expect("vertex does not exist");

        let filter = if Ty::is_directed() {
            None
        } else {
            // Use only the outgoing direction for undirected graphs.
            Some(Direction::Outgoing)
        };

        NeighborsIter {
            matrix: self.matrix.detach(),
            src: *src,
            other: 0,
            vertex_count: self.vertex_count(),
            filter,
            dir: Direction::Outgoing,
        }
    }

    fn neighbors_directed(&self, src: &Self::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
        self.vertex(src).expect("vertex does not exist");

        NeighborsIter {
            matrix: self.matrix.detach(),
            src: *src,
            other: 0,
            vertex_count: self.vertex_count(),
            filter: Some(dir),
            dir,
        }
    }

    fn degree(&self, index: &Self::VertexId) -> usize {
        Ty::directions()
            .iter()
            .map(|dir| self.degree_directed(index, *dir))
            .sum()
    }

    fn degree_directed(&self, index: &Self::VertexId, dir: Direction) -> usize {
        self.vertex(index).expect("vertex does not exist");

        self.matrix
            .degree_directed(*index, dir, self.vertices.len())
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> Create<V, E, Ty> for AdjMatrix<V, E, Ty, Id>
where
    Id::VertexId: NumIdType,
    Id::EdgeId: NumIdType,
{
    fn with_capacity(vertex_count: usize, _edge_count: usize) -> Self {
        Self {
            matrix: raw::Matrix::with_capacity(vertex_count),
            vertices: Vec::with_capacity(vertex_count),
            n_edges: 0,
        }
    }
}

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> ConnectVertices<V, E, Ty> for AdjMatrix<V, E, Ty, Id>
where
    Id::VertexId: NumIdType,
    Id::EdgeId: NumIdType,
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

impl<V, E, Ty: EdgeType, Id: GraphIdTypes> Guarantee for AdjMatrix<V, E, Ty, Id> {}

pub struct EdgeIdsIter<'a, Ty, Id> {
    matrix: raw::DetachedMatrix<'a, Ty, Id>,
    index: usize,
    edge_bound: usize,
    ty: PhantomData<fn() -> Id>,
}

impl<'a, Ty: EdgeType, Id: GraphIdTypes> Iterator for EdgeIdsIter<'a, Ty, Id>
where
    Id::EdgeId: NumIdType,
{
    type Item = Id::EdgeId;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.index == self.edge_bound {
                return None;
            }

            let index = Id::EdgeId::from_usize(self.index);
            self.index += 1;

            if self.matrix.contains(index) {
                return Some(index);
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

impl<'a, E, Ty: EdgeType, Id: GraphIdTypes> Iterator for EdgesIter<'a, E, Ty, Id>
where
    Id::VertexId: NumIdType,
    Id::EdgeId: NumIdType,
{
    type Item = (Id::EdgeId, &'a E, Id::VertexId, Id::VertexId);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.index == self.edge_bound {
                return None;
            }

            let index = Id::EdgeId::from_usize(self.index);
            self.index += 1;

            if let Some(edge) = self.matrix.get(index) {
                let (row, col) = self.matrix.coords(index);
                return Some((index, edge, row.into(), col.into()));
            }
        }
    }
}

pub struct NeighborsIter<'a, Ty, Id: GraphIdTypes> {
    matrix: raw::DetachedMatrix<'a, Ty, Id>,
    src: Id::VertexId,
    other: usize,
    vertex_count: usize,
    filter: Option<Direction>,
    dir: Direction,
}

impl<'a, Ty: EdgeType, Id: GraphIdTypes> Iterator for NeighborsIter<'a, Ty, Id>
where
    Id::VertexId: NumIdType,
    Id::EdgeId: NumIdType,
{
    type Item = (Id::VertexId, Id::EdgeId, Id::VertexId, Direction);

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
                let dst = self.other;
                self.other += 1;

                let index = match self.dir {
                    Direction::Outgoing => self.matrix.index(self.src.to_usize(), dst),
                    Direction::Incoming => self.matrix.index(dst, self.src.to_usize()),
                };

                if self.matrix.contains(index) {
                    return Some((Id::VertexId::from_usize(dst), index, self.src, self.dir));
                }
            }
        }
    }
}

mod raw {
    use std::marker::PhantomData;
    use std::mem::{self, MaybeUninit};

    use bitvec::prelude::*;

    use crate::common::matrix::*;
    use crate::core::id::{GraphIdTypes, NumIdType};
    use crate::core::marker::{Direction, EdgeType};

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

    impl<T> IntoIterator for FlaggedVec<T> {
        type Item = Option<T>;
        type IntoIter = FlaggedIter<T>;

        fn into_iter(mut self) -> Self::IntoIter {
            let flags = mem::take(&mut self.flags);
            let data = mem::take(&mut self.data);

            FlaggedIter {
                inner: flags.into_iter().zip(data),
            }
        }
    }

    impl<E> MatrixResize<E> for FlaggedVec<E> {
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

    impl<E, Ty: EdgeType, Id: GraphIdTypes> Matrix<E, Ty, Id>
    where
        Id::VertexId: NumIdType,
        Id::EdgeId: NumIdType,
    {
        pub fn with_capacity(capacity: usize) -> Self {
            if capacity == 0 {
                return Self {
                    data: Default::default(),
                    capacity,
                    ty: PhantomData,
                };
            }

            let capacity = capacity.next_power_of_two();
            let len = size_of::<Ty>(capacity);
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

        pub fn contains(&self, index: Id::EdgeId) -> bool {
            self.data.contains(index.to_usize())
        }

        pub fn get(&self, index: Id::EdgeId) -> Option<&E> {
            self.data.get(index.to_usize())
        }

        pub fn get_mut(&mut self, index: Id::EdgeId) -> Option<&mut E> {
            self.data.get_mut(index.to_usize())
        }

        pub fn insert(&mut self, index: Id::EdgeId, edge: E) {
            self.data.insert(index.to_usize(), edge);
        }

        pub fn remove(&mut self, index: Id::EdgeId) -> Option<E> {
            self.data.remove(index.to_usize())
        }

        pub fn index(&self, row: usize, col: usize) -> Id::EdgeId {
            Id::EdgeId::from_usize(index::<Ty>(row, col, self.capacity))
        }

        pub fn coords(&self, index: Id::EdgeId) -> (usize, usize) {
            coords::<Ty>(index.to_usize(), self.capacity)
        }

        pub fn degree_directed(&self, v: Id::VertexId, dir: Direction, n_vertices: usize) -> usize {
            let v = v.to_usize();
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

    impl<E, Ty: EdgeType, Id: GraphIdTypes> Matrix<E, Ty, Id> {
        pub fn clear(&mut self) {
            self.data.clear();
        }
    }

    pub struct DetachedMatrix<'a, Ty, Id> {
        data: &'a BitVec,
        capacity: usize,
        ty: PhantomData<(Ty, Id)>,
    }

    impl<Ty: EdgeType, Id: GraphIdTypes> DetachedMatrix<'_, Ty, Id>
    where
        Id::EdgeId: NumIdType,
    {
        pub fn contains(&self, index: Id::EdgeId) -> bool {
            self.data[index.to_usize()]
        }

        pub fn index(&self, row: usize, col: usize) -> Id::EdgeId {
            Id::EdgeId::from_usize(index::<Ty>(row, col, self.capacity))
        }

        #[allow(unused)]
        pub fn coords(&self, index: Id::EdgeId) -> (usize, usize) {
            coords::<Ty>(index.to_usize(), self.capacity)
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
        test_basic::<Undirected, AdjMatrix<_, _, _, DefaultId>>();
    }

    #[test]
    fn basic_directed() {
        test_basic::<Directed, AdjMatrix<_, _, _, DefaultId>>();
    }

    #[test]
    fn connect_vertices_undirected() {
        test_connect_vertices::<Undirected, AdjMatrix<_, _, _, DefaultId>>();
    }

    #[test]
    fn connect_vertices_directed() {
        test_connect_vertices::<Directed, AdjMatrix<_, _, _, DefaultId>>();
    }

    #[test]
    fn neighbors_edge_cases_undirected() {
        test_neighbors_edge_cases::<Undirected, AdjMatrix<_, _, _, DefaultId>>();
    }

    #[test]
    fn neighbors_edge_cases_directed() {
        test_neighbors_edge_cases::<Directed, AdjMatrix<_, _, _, DefaultId>>();
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
