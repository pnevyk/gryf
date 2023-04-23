use crate::core::marker::EdgeType;

#[allow(clippy::len_without_is_empty)]
pub trait MatrixResize<E>: Default + IntoIterator<Item = Option<E>> {
    fn with_capacity(capacity: usize) -> Self;
    fn resize_with_none(&mut self, new_len: usize);
    fn push(&mut self, value: Option<E>);
    fn len(&self) -> usize;
}

pub fn size_of<Ty: EdgeType>(capacity: usize) -> usize {
    if Ty::is_directed() {
        capacity * capacity
    } else {
        capacity * (capacity + 1) / 2
    }
}

pub fn resize<E, Ty: EdgeType, M: MatrixResize<E>>(prev: &mut M, capacity: usize) {
    let prev_len = prev.len();
    let len = size_of::<Ty>(capacity);

    if len <= prev_len {
        // This routine is only for growing.
        return;
    }

    if Ty::is_directed() {
        let mut next = M::with_capacity(len);
        let prev_capacity = (prev_len as f32).sqrt() as usize;

        // Add the top-right corner.
        for (i, value) in core::mem::take(prev).into_iter().enumerate() {
            next.push(value);

            // Are we on the right edge of the original square?
            if (i + 1) % prev_capacity == 0 {
                // New elements into top-right corner.
                let additional = next.len() + capacity - prev_capacity;
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

mod imp {
    use super::MatrixResize;

    impl<E> MatrixResize<E> for Vec<Option<E>> {
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
    }
}
