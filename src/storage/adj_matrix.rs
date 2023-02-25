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

use self::matrix::{BitMatrix, Matrix};
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
            matrix: &self.matrix,
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
        self.matrix.clear_edges();
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
    matrix: &'a BitMatrix<Ty, Ix>,
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
    matrix: &'a BitMatrix<Ty, Ix>,
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
    use std::ops::Deref;

    use bitvec::prelude::*;

    use crate::core::index::{Indexing, NumIndexType};
    use crate::core::marker::EdgeType;

    #[derive(Debug)]
    pub struct BitMatrix<Ty, Ix> {
        bits: BitVec,
        capacity: usize,
        ty: PhantomData<(Ty, Ix)>,
    }

    // The matrix could be implemented safely as Vec<Option<E>>. However, that
    // ties generic type E to any usage of the matrix, which causes lifetime
    // troubles in some of the traits (namely Neighbors). Having the possibility
    // to detach the information of the edges presence from the data solves this
    // problem. Unfortunately, it brings unsafe code and a bit more logic
    // complexity.
    #[derive(Debug)]
    pub struct Matrix<E, Ty, Ix> {
        inner: BitMatrix<Ty, Ix>,
        data: Vec<MaybeUninit<E>>,
    }

    fn size_of<Ty: EdgeType>(capacity: usize) -> usize {
        if Ty::is_directed() {
            capacity * capacity
        } else {
            capacity * (capacity + 1) / 2
        }
    }

    fn resize<E, Ty: EdgeType>(
        bits: &mut BitVec,
        data: &mut Vec<MaybeUninit<E>>,
        old_capacity: usize,
    ) -> usize {
        let new_capacity = 2 * old_capacity;
        let size = size_of::<Ty>(new_capacity);

        if Ty::is_directed() {
            let mut new_bits = BitVec::with_capacity(size);
            let mut new_data = Vec::with_capacity(size);

            // Add the top-right corner.
            for (i, bit) in bits.iter().enumerate() {
                new_bits.push(*bit);

                if *bit {
                    // Take the edge from the original matrix.
                    let edge = mem::replace(&mut data[i], MaybeUninit::uninit());
                    new_data.push(edge);
                } else {
                    new_data.push(MaybeUninit::uninit());
                }

                // Are we on the right edge of the original square?
                if (i + 1) % old_capacity == 0 {
                    // New elements into top-right corner.
                    let extended_size = new_bits.len() + old_capacity;
                    new_bits.resize(extended_size, false);
                    new_data.resize_with(extended_size, || MaybeUninit::uninit());
                }
            }

            // Add the bottom rectangle.
            new_bits.resize(size, false);
            new_data.resize_with(size, || MaybeUninit::uninit());

            *bits = new_bits;
            *data = new_data;
        } else {
            // Just continue the lower triangle.
            bits.resize(size, false);
            data.resize_with(size, || MaybeUninit::uninit());
        }

        new_capacity
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

    impl<Ty: EdgeType, Ix: Indexing> BitMatrix<Ty, Ix>
    where
        Ix::EdgeIndex: NumIndexType,
    {
        fn with_capacity(capacity: usize) -> Self {
            Self {
                bits: bitvec![0; size_of::<Ty>(capacity)],
                capacity,
                ty: PhantomData,
            }
        }

        pub fn index(&self, row: usize, col: usize) -> Ix::EdgeIndex {
            Ix::EdgeIndex::from_usize(index::<Ty>(row, col, self.capacity))
        }

        pub fn coords(&self, index: Ix::EdgeIndex) -> (usize, usize) {
            coords::<Ty>(index.to_usize(), self.capacity)
        }

        pub fn set(&mut self, index: Ix::EdgeIndex, value: bool) -> bool {
            let mut bit = self.bits.get_mut(index.to_usize()).unwrap();
            bit.replace(value)
        }

        pub fn contains(&self, index: Ix::EdgeIndex) -> bool {
            self.bits[index.to_usize()]
        }
    }

    impl<E, Ty: EdgeType, Ix: Indexing> Matrix<E, Ty, Ix>
    where
        Ix::EdgeIndex: NumIndexType,
    {
        pub fn with_capacity(capacity: usize) -> Self {
            let capacity = capacity.next_power_of_two();
            let inner = BitMatrix::with_capacity(capacity);

            let size = size_of::<Ty>(capacity);
            let mut data = Vec::with_capacity(size);
            data.resize_with(size, || MaybeUninit::uninit());

            Self { inner, data }
        }

        pub fn ensure_capacity(&mut self, capacity: usize) {
            if self.inner.capacity < capacity {
                let capacity = self.capacity;
                let capacity = resize::<E, Ty>(&mut self.inner.bits, &mut self.data, capacity);
                self.inner.capacity = capacity;
            }
        }

        pub fn get(&self, index: Ix::EdgeIndex) -> Option<&E> {
            if self.inner.contains(index) {
                // SAFETY: self.inner and self.data are consistent with each
                // other. If self.inner confirms that there is an edge at given
                // index, then the corresponding data are guaranteed to be
                // initialized.
                Some(unsafe { self.data[index.to_usize()].assume_init_ref() })
            } else {
                None
            }
        }

        pub fn get_mut(&mut self, index: Ix::EdgeIndex) -> Option<&mut E> {
            if self.inner.contains(index) {
                // SAFETY: See Matrix::get.
                Some(unsafe { self.data[index.to_usize()].assume_init_mut() })
            } else {
                None
            }
        }

        pub fn insert(&mut self, index: Ix::EdgeIndex, edge: E) {
            if self.inner.set(index, true) {
                // There was already an edge, that we must drop so it does not
                // leak.

                // SAFETY: See Matrix::get.
                unsafe { self.data[index.to_usize()].assume_init_drop() };
            }

            // SAFETY: Initializing value of MaybeUninit.
            unsafe { self.data[index.to_usize()].as_mut_ptr().write(edge) };
        }

        pub fn remove(&mut self, index: Ix::EdgeIndex) -> Option<E> {
            if self.inner.contains(index) {
                self.inner.set(index, false);
                let slot = &mut self.data[index.to_usize()];
                let old = mem::replace(slot, MaybeUninit::uninit());
                // SAFETY: See Matrix::get.
                Some(unsafe { old.assume_init() })
            } else {
                None
            }
        }

        pub fn detach(&self) -> &BitMatrix<Ty, Ix> {
            &self.inner
        }
    }

    impl<E, Ty, Ix> Matrix<E, Ty, Ix> {
        pub fn clear(&mut self) {
            self.clear_edges();
            self.inner.bits.clear();
            self.data.clear();
        }

        pub fn clear_edges(&mut self) {
            for (mut bit, edge) in self.inner.bits.iter_mut().zip(self.data.iter_mut()) {
                if *bit {
                    *bit = false;

                    // SAFETY: See Matrix::get.
                    unsafe { edge.assume_init_drop() };
                }
            }
        }
    }

    impl<E, Ty, Ix> Drop for Matrix<E, Ty, Ix> {
        fn drop(&mut self) {
            self.clear_edges()
        }
    }

    impl<E, Ty: EdgeType, Ix> Deref for Matrix<E, Ty, Ix> {
        type Target = BitMatrix<Ty, Ix>;

        fn deref(&self) -> &Self::Target {
            &self.inner
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
