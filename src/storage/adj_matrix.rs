use self::matrix::{BitMatrix, Matrix};
pub use super::shared::{RangeIndices, VerticesIter};
use crate::index::{EdgeIndex, IndexType, VertexIndex};
use crate::infra::CompactIndexMap;
use crate::marker::{Direction, EdgeType};
use crate::traits::*;
use crate::{EdgesBaseWeak, EdgesWeak, VerticesBaseWeak, VerticesWeak};

#[derive(Debug, VerticesBaseWeak, VerticesWeak, EdgesBaseWeak, EdgesWeak)]
pub struct AdjMatrix<V, E, Ty> {
    matrix: Matrix<E, Ty>,
    vertices: Vec<V>,
    n_edges: usize,
}

impl<V, E, Ty: EdgeType> AdjMatrix<V, E, Ty> {
    pub fn new() -> Self {
        Self {
            matrix: Matrix::with_capacity(8),
            vertices: Vec::new(),
            n_edges: 0,
        }
    }
}

impl<V, E, Ty: EdgeType> Default for AdjMatrix<V, E, Ty> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V, E, Ty: EdgeType> VerticesBase for AdjMatrix<V, E, Ty> {
    type VertexIndicesIter<'a> = RangeIndices<VertexIndex>
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

    fn vertex_index_map(&self) -> CompactIndexMap<VertexIndex> {
        CompactIndexMap::isomorphic(self.vertex_count())
    }
}

impl<V, E, Ty: EdgeType> Vertices<V> for AdjMatrix<V, E, Ty> {
    type VertexRef<'a, T: 'a> = (VertexIndex, &'a T)
    where
        Self: 'a;

    type VerticesIter<'a, T: 'a> = VerticesIter<'a, T>
    where
        Self: 'a;

    fn vertex(&self, index: VertexIndex) -> Option<&V> {
        self.vertices.get(index.to_usize())
    }

    fn vertices(&self) -> Self::VerticesIter<'_, V> {
        VerticesIter::new(self.vertices.iter())
    }
}

impl<V, E, Ty: EdgeType> VerticesMut<V> for AdjMatrix<V, E, Ty> {
    fn vertex_mut(&mut self, index: VertexIndex) -> Option<&mut V> {
        self.vertices.get_mut(index.to_usize())
    }

    fn add_vertex(&mut self, vertex: V) -> VertexIndex {
        self.matrix.ensure_capacity(self.vertex_count() + 1);

        let index = self.vertices.len();
        self.vertices.push(vertex);
        index.into()
    }

    fn remove_vertex(&mut self, index: VertexIndex) -> Option<V> {
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
            let last_index = VertexIndex::new(self.vertices.len());

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

impl<V, E, Ty: EdgeType> EdgesBase<Ty> for AdjMatrix<V, E, Ty> {
    type EdgeIndicesIter<'a> = EdgeIndicesIter<'a, Ty>
    where
        Self: 'a;

    fn edge_count(&self) -> usize {
        self.n_edges
    }

    fn edge_bound(&self) -> usize {
        self.matrix.index(self.vertex_count(), 0).to_usize()
    }

    fn endpoints(&self, index: EdgeIndex) -> Option<(VertexIndex, VertexIndex)> {
        let (row, col) = self.matrix.coords(index);
        if row < self.vertex_count() {
            Some((row.into(), col.into()))
        } else {
            None
        }
    }

    fn edge_index(&self, src: VertexIndex, dst: VertexIndex) -> Option<EdgeIndex> {
        let index = self.matrix.index(src.to_usize(), dst.to_usize());
        self.matrix.get(index).map(|_| index)
    }

    fn edge_indices(&self) -> Self::EdgeIndicesIter<'_> {
        EdgeIndicesIter {
            matrix: &self.matrix,
            index: 0,
            edge_bound: self.edge_bound(),
        }
    }
}

impl<V, E, Ty: EdgeType> Edges<E, Ty> for AdjMatrix<V, E, Ty> {
    type EdgeRef<'a, T: 'a> = (EdgeIndex, &'a T, VertexIndex, VertexIndex)
    where
        Self: 'a;

    type EdgesIter<'a, T: 'a> = EdgesIter<'a, T, Ty>
    where
        Self: 'a;

    fn edge(&self, index: EdgeIndex) -> Option<&E> {
        self.matrix.get(index)
    }

    fn edges(&self) -> Self::EdgesIter<'_, E> {
        EdgesIter {
            matrix: &self.matrix,
            index: 0,
            edge_bound: self.edge_bound(),
        }
    }
}

impl<V, E, Ty: EdgeType + 'static> EdgesMut<E, Ty> for AdjMatrix<V, E, Ty> {
    fn edge_mut(&mut self, index: EdgeIndex) -> Option<&mut E> {
        self.matrix.get_mut(index)
    }

    fn add_edge(&mut self, src: VertexIndex, dst: VertexIndex, edge: E) -> EdgeIndex {
        let index = self.matrix.index(src.to_usize(), dst.to_usize());
        self.matrix.insert(index, edge);
        self.n_edges += 1;
        index
    }

    fn remove_edge(&mut self, index: EdgeIndex) -> Option<E> {
        match self.matrix.remove(index) {
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

impl<V, E, Ty: EdgeType> Neighbors for AdjMatrix<V, E, Ty> {
    type NeighborRef<'a> = (VertexIndex, EdgeIndex, VertexIndex, Direction)
    where
        Self: 'a;

    type NeighborsIter<'a> = NeighborsIter<'a, Ty>
    where
        Self: 'a;

    fn neighbors(&self, src: VertexIndex) -> Self::NeighborsIter<'_> {
        let filter = if Ty::is_directed() {
            None
        } else {
            // Use only the outgoing direction for undirected graphs.
            Some(Direction::Outgoing)
        };

        NeighborsIter {
            matrix: self.matrix.detach(),
            src,
            other: 0,
            vertex_count: self.vertex_count(),
            filter,
            dir: Direction::Outgoing,
        }
    }

    fn neighbors_directed(&self, src: VertexIndex, dir: Direction) -> Self::NeighborsIter<'_> {
        NeighborsIter {
            matrix: self.matrix.detach(),
            src,
            other: 0,
            vertex_count: self.vertex_count(),
            filter: Some(dir),
            dir,
        }
    }
}

impl<V, E, Ty: EdgeType + 'static> Create<V, E, Ty> for AdjMatrix<V, E, Ty> {
    fn with_capacity(vertex_count: usize, _edge_count: usize) -> Self {
        Self {
            matrix: Matrix::with_capacity(vertex_count),
            vertices: Vec::with_capacity(vertex_count),
            n_edges: 0,
        }
    }
}

impl<V, E, Ty: EdgeType> Guarantee for AdjMatrix<V, E, Ty> {}

pub struct EdgeIndicesIter<'a, Ty> {
    matrix: &'a BitMatrix<Ty>,
    index: usize,
    edge_bound: usize,
}

impl<'a, Ty: EdgeType> Iterator for EdgeIndicesIter<'a, Ty> {
    type Item = EdgeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.index == self.edge_bound {
                return None;
            }

            let index = self.index.into();
            self.index += 1;

            if self.matrix.contains(index) {
                return Some(index);
            }
        }
    }
}

pub struct EdgesIter<'a, E, Ty> {
    matrix: &'a Matrix<E, Ty>,
    index: usize,
    edge_bound: usize,
}

impl<'a, E, Ty: EdgeType> Iterator for EdgesIter<'a, E, Ty> {
    type Item = (EdgeIndex, &'a E, VertexIndex, VertexIndex);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.index == self.edge_bound {
                return None;
            }

            let index = self.index.into();
            self.index += 1;

            if let Some(edge) = self.matrix.get(index) {
                let (row, col) = self.matrix.coords(index);
                return Some((index, edge, row.into(), col.into()));
            }
        }
    }
}

pub struct NeighborsIter<'a, Ty> {
    matrix: &'a BitMatrix<Ty>,
    src: VertexIndex,
    other: usize,
    vertex_count: usize,
    filter: Option<Direction>,
    dir: Direction,
}

impl<'a, Ty: EdgeType> Iterator for NeighborsIter<'a, Ty> {
    type Item = (VertexIndex, EdgeIndex, VertexIndex, Direction);

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
                    return Some((dst.into(), index, self.src, self.dir));
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

    use crate::index::{EdgeIndex, IndexType};
    use crate::marker::EdgeType;

    #[derive(Debug)]
    pub struct BitMatrix<Ty> {
        bits: BitVec,
        capacity: usize,
        ty: PhantomData<Ty>,
    }

    // The matrix could be implemented safely as Vec<Option<E>>. However, that
    // ties generic type E to any usage of the matrix, which causes lifetime
    // troubles in some of the traits (namely Neighbors). Having the possibility
    // to detach the information of the edges presence from the data solves this
    // problem. Unfortunately, it brings unsafe code and a bit more logic
    // complexity.
    #[derive(Debug)]
    pub struct Matrix<E, Ty> {
        inner: BitMatrix<Ty>,
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

    impl<Ty: EdgeType> BitMatrix<Ty> {
        fn with_capacity(capacity: usize) -> Self {
            Self {
                bits: bitvec![0; size_of::<Ty>(capacity)],
                capacity,
                ty: PhantomData,
            }
        }

        pub fn index(&self, row: usize, col: usize) -> EdgeIndex {
            index::<Ty>(row, col, self.capacity).into()
        }

        pub fn coords(&self, index: EdgeIndex) -> (usize, usize) {
            coords::<Ty>(index.to_usize(), self.capacity)
        }

        pub fn set(&mut self, index: EdgeIndex, value: bool) -> bool {
            let mut bit = self.bits.get_mut(index.to_usize()).unwrap();
            bit.replace(value)
        }

        pub fn contains(&self, index: EdgeIndex) -> bool {
            self.bits[index.to_usize()]
        }
    }

    impl<E, Ty: EdgeType> Matrix<E, Ty> {
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

        pub fn get(&self, index: EdgeIndex) -> Option<&E> {
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

        pub fn get_mut(&mut self, index: EdgeIndex) -> Option<&mut E> {
            if self.inner.contains(index) {
                // SAFETY: See Matrix::get.
                Some(unsafe { self.data[index.to_usize()].assume_init_mut() })
            } else {
                None
            }
        }

        pub fn insert(&mut self, index: EdgeIndex, edge: E) {
            if self.inner.set(index, true) {
                // There was already an edge, that we must drop so it does not
                // leak.

                // SAFETY: See Matrix::get.
                unsafe { self.data[index.to_usize()].assume_init_drop() };
            }

            // SAFETY: Initializing value of MaybeUninit.
            unsafe { self.data[index.to_usize()].as_mut_ptr().write(edge) };
        }

        pub fn remove(&mut self, index: EdgeIndex) -> Option<E> {
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

        pub fn detach(&self) -> &BitMatrix<Ty> {
            &self.inner
        }
    }

    impl<E, Ty> Matrix<E, Ty> {
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

    impl<E, Ty> Drop for Matrix<E, Ty> {
        fn drop(&mut self) {
            self.clear_edges()
        }
    }

    impl<E, Ty: EdgeType> Deref for Matrix<E, Ty> {
        type Target = BitMatrix<Ty>;

        fn deref(&self) -> &Self::Target {
            &self.inner
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::marker::{Directed, Undirected};
    use crate::storage::tests::*;

    #[test]
    fn basic_undirected() {
        test_basic::<Undirected, AdjMatrix<_, _, _>>();
    }

    #[test]
    fn basic_directed() {
        test_basic::<Directed, AdjMatrix<_, _, _>>();
    }
}
