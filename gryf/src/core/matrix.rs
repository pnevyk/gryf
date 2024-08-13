use crate::core::marker::EdgeType;

#[allow(clippy::len_without_is_empty)]
pub trait MatrixLinearStorage<E>: Default {
    fn with_capacity(capacity: usize) -> Self;
    fn resize_with_none(&mut self, new_len: usize);
    fn push(&mut self, value: Option<E>);
    fn len(&self) -> usize;
    fn into_entries(self) -> impl Iterator<Item = Option<E>>;
}

pub fn linear_len<Ty: EdgeType>(vertex_capacity: usize) -> usize {
    if Ty::is_directed() {
        vertex_capacity * vertex_capacity
    } else {
        vertex_capacity * (vertex_capacity + 1) / 2
    }
}

pub fn resize<E, Ty: EdgeType, M: MatrixLinearStorage<E>>(prev: &mut M, vertex_capacity: usize) {
    let prev_len = prev.len();
    let len = linear_len::<Ty>(vertex_capacity);

    if len <= prev_len {
        // This routine is only for growing.
        return;
    }

    if Ty::is_directed() {
        let mut next = M::with_capacity(len);
        let prev_capacity = (prev_len as f32).sqrt() as usize;

        // Add the top-right corner.
        for (i, value) in core::mem::take(prev).into_entries().enumerate() {
            next.push(value);

            // Are we on the right edge of the original square?
            if (i + 1) % prev_capacity == 0 {
                // New elements into top-right corner.
                let additional = next.len() + vertex_capacity - prev_capacity;
                next.resize_with_none(additional);
            }
        }

        // Add the bottom rectangle.
        next.resize_with_none(len);
        *prev = next;
    } else {
        // Just continue the lower triangle.
        prev.resize_with_none(len);
    }
}

pub fn index<Ty: EdgeType>(row: usize, col: usize, vertex_capacity: usize) -> usize {
    if Ty::is_directed() {
        row * vertex_capacity + col
    } else {
        // Make sure that the coordinates are in the lower triangle.
        let (row, col) = if row >= col { (row, col) } else { (col, row) };
        // The rows are 1 + 2 + 3 + ... + n = n (n + 1) / 2.
        row * (row + 1) / 2 + col
    }
}

pub fn coords<Ty: EdgeType>(index: usize, vertex_capacity: usize) -> (usize, usize) {
    if Ty::is_directed() {
        let col = index % vertex_capacity;
        let row = index / vertex_capacity;
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

mod imp {
    use bitvec::{order::BitOrder, store::BitStore, vec::BitVec};

    use super::MatrixLinearStorage;

    impl<E> MatrixLinearStorage<E> for Vec<Option<E>> {
        fn with_capacity(capacity: usize) -> Self {
            Self::with_capacity(capacity)
        }

        fn resize_with_none(&mut self, new_len: usize) {
            self.resize_with(new_len, || None);
        }

        fn push(&mut self, value: Option<E>) {
            self.push(value);
        }

        fn len(&self) -> usize {
            self.len()
        }

        fn into_entries(self) -> impl Iterator<Item = Option<E>> {
            self.into_iter()
        }
    }

    impl<T, O> MatrixLinearStorage<()> for BitVec<T, O>
    where
        T: BitStore,
        O: BitOrder,
    {
        fn with_capacity(capacity: usize) -> Self {
            Self::with_capacity(capacity)
        }

        fn resize_with_none(&mut self, new_len: usize) {
            self.resize_with(new_len, |_| false);
        }

        fn push(&mut self, value: Option<()>) {
            self.push(value.is_some())
        }

        fn len(&self) -> usize {
            self.len()
        }

        fn into_entries(self) -> impl Iterator<Item = Option<()>> {
            self.into_iter().map(|bit| bit.then_some(()))
        }
    }
}
