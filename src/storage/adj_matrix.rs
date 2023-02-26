use std::marker::PhantomData;

use crate::{
    common::CompactIndexMap,
    core::{
        index::{Indexing, NumIndexType},
        marker::{Direction, EdgeType},
        AddEdgeError, AddEdgeErrorKind, AddVertexError, ConnectVertices, Create, Edges, EdgesBase,
        EdgesMut, GraphBase, Guarantee, Neighbors, Vertices, VerticesBase, VerticesMut,
    },
};

use crate::derive::{EdgesBaseWeak, EdgesWeak, VerticesBaseWeak, VerticesWeak};

// TODO: Remove these imports once hygiene of procedural macros is fixed.
use crate::core::{EdgesBaseWeak, EdgesWeak, VerticesBaseWeak, VerticesWeak, WeakRef};

use self::matrix::{DetachedMatrix, Matrix};
use super::shared;
pub use super::shared::{RangeIndices as VertexIndices, VerticesIter};

#[derive(Debug, VerticesBaseWeak, VerticesWeak, EdgesBaseWeak, EdgesWeak)]
pub struct AdjMatrix<V, E, Ty, Ix> {
    matrix: Matrix<E, Ty, Ix>,
    vertices: Vec<V>,
    n_edges: usize,
}

impl<V, E, Ty: EdgeType, Ix: Indexing> AdjMatrix<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    pub fn new() -> Self {
        Self {
            matrix: Matrix::with_capacity(8),
            vertices: Vec::new(),
            n_edges: 0,
        }
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> Default for AdjMatrix<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> GraphBase for AdjMatrix<V, E, Ty, Ix> {
    type VertexIndex = Ix::VertexIndex;
    type EdgeIndex = Ix::EdgeIndex;
}

impl<V, E, Ty: EdgeType, Ix: Indexing> VerticesBase for AdjMatrix<V, E, Ty, Ix>
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
        Ix::VertexIndex: NumIndexType,
    {
        CompactIndexMap::isomorphic(self.vertex_count())
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> Vertices<V> for AdjMatrix<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
{
    type VertexRef<'a> = (Self::VertexIndex, &'a V)
    where
        Self: 'a,
        V: 'a;

    type VerticesIter<'a> = VerticesIter<'a, Ix, V>
    where
        Self: 'a,
        V: 'a;

    fn vertex(&self, index: &Self::VertexIndex) -> Option<&V> {
        self.vertices.get(index.to_usize())
    }

    fn vertices(&self) -> Self::VerticesIter<'_> {
        VerticesIter::new(self.vertices.iter())
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> VerticesMut<V> for AdjMatrix<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    fn vertex_mut(&mut self, index: &Self::VertexIndex) -> Option<&mut V> {
        self.vertices.get_mut(index.to_usize())
    }

    fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexIndex, AddVertexError<V>> {
        self.matrix.ensure_capacity(self.vertex_count() + 1);

        let index = self.vertices.len();
        self.vertices.push(vertex);
        Ok(index.into())
    }

    fn remove_vertex(&mut self, index: &Self::VertexIndex) -> Option<V> {
        self.vertex(index)?;

        // Remove incident edges.
        for i in 0..self.vertices.len() {
            let edge_index = self.matrix.index(index.to_usize(), i);
            if self.matrix.remove(edge_index).is_some() {
                self.n_edges -= 1;
            }

            if Ty::is_directed() {
                let edge_index = self.matrix.index(i, index.to_usize());
                if self.matrix.remove(edge_index).is_some() {
                    self.n_edges -= 1;
                }
            }
        }

        let vertex = self.vertices.swap_remove(index.to_usize());

        // Relocate the edges of the last vertex, if it is going to replace the
        // removed vertex.
        if index.to_usize() < self.vertices.len() {
            let last_index = Ix::VertexIndex::from_usize(self.vertices.len());

            // We already removed the vertex from the vector, so its size is one
            // less than before. But we need to iterate over entire matrix, so
            // the range is inclusive.
            for i in 0..=self.vertices.len() {
                let edge_index = self.matrix.index(last_index.to_usize(), i);
                if let Some(edge) = self.matrix.remove(edge_index) {
                    let edge_index = self.matrix.index(index.to_usize(), i);
                    self.matrix.insert(edge_index, edge);
                }

                if Ty::is_directed() {
                    let edge_index = self.matrix.index(i, last_index.to_usize());
                    if let Some(edge) = self.matrix.remove(edge_index) {
                        let edge_index = self.matrix.index(i, index.to_usize());
                        self.matrix.insert(edge_index, edge);
                    }
                }
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

impl<V, E, Ty: EdgeType, Ix: Indexing> EdgesBase<Ty> for AdjMatrix<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    type EdgeIndicesIter<'a> = EdgeIndicesIter<'a, Ty, Ix>
    where
        Self: 'a;

    fn edge_count(&self) -> usize {
        self.n_edges
    }

    fn edge_bound(&self) -> usize {
        self.matrix.index(self.vertex_count(), 0).to_usize()
    }

    fn endpoints(&self, index: &Self::EdgeIndex) -> Option<(Self::VertexIndex, Self::VertexIndex)> {
        let (row, col) = self.matrix.coords(*index);
        if row < self.vertex_count() {
            Some((row.into(), col.into()))
        } else {
            None
        }
    }

    fn edge_index(
        &self,
        src: &Self::VertexIndex,
        dst: &Self::VertexIndex,
    ) -> Option<Self::EdgeIndex> {
        let index = self.matrix.index(src.to_usize(), dst.to_usize());
        self.matrix.get(index).map(|_| index)
    }

    fn edge_indices(&self) -> Self::EdgeIndicesIter<'_> {
        EdgeIndicesIter {
            matrix: self.matrix.detach(),
            index: 0,
            edge_bound: self.edge_bound(),
            ty: PhantomData,
        }
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> Edges<E, Ty> for AdjMatrix<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    type EdgeRef<'a> = (Self::EdgeIndex, &'a E, Self::VertexIndex, Self::VertexIndex)
    where
        Self: 'a,
        E: 'a;

    type EdgesIter<'a> = EdgesIter<'a, E, Ty, Ix>
    where
        Self: 'a,
        E:'a;

    fn edge(&self, index: &Self::EdgeIndex) -> Option<&E> {
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

impl<V, E, Ty: EdgeType, Ix: Indexing> EdgesMut<E, Ty> for AdjMatrix<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    fn edge_mut(&mut self, index: &Ix::EdgeIndex) -> Option<&mut E> {
        self.matrix.get_mut(*index)
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

        let index = self.matrix.index(src.to_usize(), dst.to_usize());

        if self.matrix.contains(index) {
            return Err(AddEdgeError::new(edge, AddEdgeErrorKind::MultiEdge));
        }

        self.matrix.insert(index, edge);
        self.n_edges += 1;

        Ok(index)
    }

    fn remove_edge(&mut self, index: &Self::EdgeIndex) -> Option<E> {
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

impl<V, E, Ty: EdgeType, Ix: Indexing> Neighbors for AdjMatrix<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    type NeighborRef<'a> = (Self::VertexIndex, Self::EdgeIndex, Self::VertexIndex, Direction)
    where
        Self: 'a;

    type NeighborsIter<'a> = NeighborsIter<'a, Ty, Ix>
    where
        Self: 'a;

    fn neighbors(&self, src: &Self::VertexIndex) -> Self::NeighborsIter<'_> {
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

    fn neighbors_directed(
        &self,
        src: &Self::VertexIndex,
        dir: Direction,
    ) -> Self::NeighborsIter<'_> {
        NeighborsIter {
            matrix: self.matrix.detach(),
            src: *src,
            other: 0,
            vertex_count: self.vertex_count(),
            filter: Some(dir),
            dir,
        }
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> Create<V, E, Ty> for AdjMatrix<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    fn with_capacity(vertex_count: usize, _edge_count: usize) -> Self {
        Self {
            matrix: Matrix::with_capacity(vertex_count),
            vertices: Vec::with_capacity(vertex_count),
            n_edges: 0,
        }
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> ConnectVertices<V, E, Ty> for AdjMatrix<V, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    fn connect_vertices<F>(&mut self, mut connect: F)
    where
        F: FnMut(&V, &V) -> Option<E>,
    {
        shared::connect_vertices::<Ty>(self.vertices.len(), |i, j| {
            let src = &self.vertices[i];
            let dst = &self.vertices[j];

            if let Some(edge) = connect(src, dst) {
                let src = Ix::VertexIndex::from_usize(i);
                let dst = Ix::VertexIndex::from_usize(j);

                self.add_edge(&src, &dst, edge);
            }
        })
    }
}

impl<V, E, Ty: EdgeType, Ix: Indexing> Guarantee for AdjMatrix<V, E, Ty, Ix> {}

pub struct EdgeIndicesIter<'a, Ty, Ix> {
    matrix: DetachedMatrix<'a, Ty, Ix>,
    index: usize,
    edge_bound: usize,
    ty: PhantomData<fn() -> Ix>,
}

impl<'a, Ty: EdgeType, Ix: Indexing> Iterator for EdgeIndicesIter<'a, Ty, Ix>
where
    Ix::EdgeIndex: NumIndexType,
{
    type Item = Ix::EdgeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.index == self.edge_bound {
                return None;
            }

            let index = Ix::EdgeIndex::from_usize(self.index);
            self.index += 1;

            if self.matrix.contains(index) {
                return Some(index);
            }
        }
    }
}

pub struct EdgesIter<'a, E, Ty, Ix> {
    matrix: &'a Matrix<E, Ty, Ix>,
    index: usize,
    edge_bound: usize,
    ty: PhantomData<fn() -> Ix>,
}

impl<'a, E, Ty: EdgeType, Ix: Indexing> Iterator for EdgesIter<'a, E, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    type Item = (Ix::EdgeIndex, &'a E, Ix::VertexIndex, Ix::VertexIndex);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.index == self.edge_bound {
                return None;
            }

            let index = Ix::EdgeIndex::from_usize(self.index);
            self.index += 1;

            if let Some(edge) = self.matrix.get(index) {
                let (row, col) = self.matrix.coords(index);
                return Some((index, edge, row.into(), col.into()));
            }
        }
    }
}

pub struct NeighborsIter<'a, Ty, Ix: Indexing> {
    matrix: DetachedMatrix<'a, Ty, Ix>,
    src: Ix::VertexIndex,
    other: usize,
    vertex_count: usize,
    filter: Option<Direction>,
    dir: Direction,
}

impl<'a, Ty: EdgeType, Ix: Indexing> Iterator for NeighborsIter<'a, Ty, Ix>
where
    Ix::VertexIndex: NumIndexType,
    Ix::EdgeIndex: NumIndexType,
{
    type Item = (Ix::VertexIndex, Ix::EdgeIndex, Ix::VertexIndex, Direction);

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
                    return Some((Ix::VertexIndex::from_usize(dst), index, self.src, self.dir));
                }
            }
        }
    }
}

mod matrix {
    use std::marker::PhantomData;
    use std::mem::{self, MaybeUninit};

    use bitvec::prelude::*;

    use crate::core::index::{Indexing, NumIndexType};
    use crate::core::marker::EdgeType;

    fn size_of<Ty: EdgeType>(capacity: usize) -> usize {
        if Ty::is_directed() {
            capacity * capacity
        } else {
            capacity * (capacity + 1) / 2
        }
    }

    fn resize<E, Ty: EdgeType>(prev: &mut FlaggedVec<E>) {
        let prev_len = prev.len();
        let len = size_of::<Ty>(2 * prev_len);

        if Ty::is_directed() {
            let mut next = FlaggedVec::with_capacity(len);

            // Add the top-right corner.
            for (i, value) in mem::take(prev).into_iter().enumerate() {
                next.push(value);

                // Are we on the right edge of the original square?
                if (i + 1) % prev_len == 0 {
                    // New elements into top-right corner.
                    next.resize(i + prev_len);
                }
            }

            // Add the bottom rectangle.
            next.resize(len);
            *prev = next;
        } else {
            // Just continue the lower triangle.
            prev.resize(len);
        }
    }

    pub fn index<Ty: EdgeType>(row: usize, col: usize, capacity: usize) -> usize {
        if Ty::is_directed() {
            row * capacity + col
        } else {
            // Make sure that the coordinates are in the lower triangle.
            let (row, col) = if row >= col { (row, col) } else { (col, row) };
            // The rows are 1 + 2 + 3 + ... + n = n (n + 1) / 2.
            row * (row + 1) / 2 + col
        }
    }

    pub fn coords<Ty: EdgeType>(index: usize, capacity: usize) -> (usize, usize) {
        if Ty::is_directed() {
            let col = index % capacity;
            let row = index / capacity;
            (row, col)
        } else {
            // index = row * (row + 1) / 2 + col => 2 * (index - col) = row^2 + row
            //
            // Quadratic equation for row. We don't know col so we use just
            // index => discriminant is generally not an integer, we need to
            // round down. The difference between index and start of the row is
            // the column.
            let d = (1. + 8. * index as f64).sqrt().floor() as usize;
            let row = (d - 1) / 2;
            let col = index - row * (row + 1) / 2;
            (row, col)
        }
    }

    #[derive(Debug)]
    pub struct FlaggedVec<T> {
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

        pub fn into_iter(mut self) -> impl Iterator<Item = Option<T>> {
            let flags = mem::take(&mut self.flags);
            let data = mem::take(&mut self.data);

            flags
                .into_iter()
                .zip(data.into_iter())
                .map(|(flag, value)| {
                    if flag {
                        // SAFETY: See FlaggedVec::get.
                        Some(unsafe { value.assume_init() })
                    } else {
                        None
                    }
                })
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

    // The matrix could be implemented safely as Vec<Option<E>>. However, that
    // ties generic type E to any usage of the matrix, which causes lifetime
    // troubles in some of the traits (namely Neighbors). Having the possibility
    // to detach the information of the edges presence from the data solves this
    // problem. Unfortunately, it brings unsafe code and a bit more logic
    // complexity. We hide this unsafe code behind FlaggedVec abstraction.
    #[derive(Debug)]
    pub struct Matrix<E, Ty, Ix> {
        data: FlaggedVec<E>,
        capacity: usize,
        ty: PhantomData<(Ty, Ix)>,
    }

    impl<E, Ty: EdgeType, Ix: Indexing> Matrix<E, Ty, Ix>
    where
        Ix::EdgeIndex: NumIndexType,
    {
        pub fn with_capacity(capacity: usize) -> Self {
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
                resize::<E, Ty>(&mut self.data);
            }
        }

        pub fn contains(&self, index: Ix::EdgeIndex) -> bool {
            self.data.contains(index.to_usize())
        }

        pub fn get(&self, index: Ix::EdgeIndex) -> Option<&E> {
            self.data.get(index.to_usize())
        }

        pub fn get_mut(&mut self, index: Ix::EdgeIndex) -> Option<&mut E> {
            self.data.get_mut(index.to_usize())
        }

        pub fn insert(&mut self, index: Ix::EdgeIndex, edge: E) {
            self.data.insert(index.to_usize(), edge);
        }

        pub fn remove(&mut self, index: Ix::EdgeIndex) -> Option<E> {
            self.data.remove(index.to_usize())
        }

        pub fn index(&self, row: usize, col: usize) -> Ix::EdgeIndex {
            Ix::EdgeIndex::from_usize(index::<Ty>(row, col, self.capacity))
        }

        pub fn coords(&self, index: Ix::EdgeIndex) -> (usize, usize) {
            coords::<Ty>(index.to_usize(), self.capacity)
        }

        pub fn detach(&self) -> DetachedMatrix<'_, Ty, Ix> {
            DetachedMatrix {
                data: self.data.detach(),
                capacity: self.capacity,
                ty: PhantomData,
            }
        }
    }

    impl<E, Ty: EdgeType, Ix: Indexing> Matrix<E, Ty, Ix> {
        pub fn clear(&mut self) {
            self.data.clear();
        }
    }

    pub struct DetachedMatrix<'a, Ty, Ix> {
        data: &'a BitVec,
        capacity: usize,
        ty: PhantomData<(Ty, Ix)>,
    }

    impl<Ty: EdgeType, Ix: Indexing> DetachedMatrix<'_, Ty, Ix>
    where
        Ix::EdgeIndex: NumIndexType,
    {
        pub fn contains(&self, index: Ix::EdgeIndex) -> bool {
            self.data[index.to_usize()]
        }

        pub fn index(&self, row: usize, col: usize) -> Ix::EdgeIndex {
            Ix::EdgeIndex::from_usize(index::<Ty>(row, col, self.capacity))
        }

        #[allow(unused)]
        pub fn coords(&self, index: Ix::EdgeIndex) -> (usize, usize) {
            coords::<Ty>(index.to_usize(), self.capacity)
        }
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
        test_basic::<Undirected, AdjMatrix<_, _, _, DefaultIndexing>>();
    }

    #[test]
    fn basic_directed() {
        test_basic::<Directed, AdjMatrix<_, _, _, DefaultIndexing>>();
    }

    #[test]
    fn connect_vertices_undirected() {
        test_connect_vertices::<Undirected, AdjMatrix<_, _, _, DefaultIndexing>>();
    }

    #[test]
    fn connect_vertices_directed() {
        test_connect_vertices::<Directed, AdjMatrix<_, _, _, DefaultIndexing>>();
    }
}
