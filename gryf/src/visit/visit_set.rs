use std::{
    collections::{BTreeSet, HashSet},
    hash::BuildHasher,
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use fixedbitset::FixedBitSet;

use crate::core::id::{IdType, IntegerIdType};

/// A set of visited vertices or edges.
pub trait VisitSet<I: IdType> {
    /// Marks the element as visited.
    ///
    /// Returns `true` when this is the first time the element is visited.
    fn visit(&mut self, id: I) -> bool;

    /// Returns `true` if the element is marked as visited.
    fn is_visited(&self, id: &I) -> bool;

    /// Returns the number of visited elements.
    fn visited_count(&self) -> usize;

    /// Resets the set of visited elements to be empty.
    fn reset_visited(&mut self);
}

impl<I: IdType> VisitSet<I> for BTreeSet<I> {
    fn visit(&mut self, id: I) -> bool {
        self.insert(id)
    }

    fn is_visited(&self, id: &I) -> bool {
        self.contains(id)
    }

    fn visited_count(&self) -> usize {
        self.len()
    }

    fn reset_visited(&mut self) {
        self.clear();
    }
}

impl<I: IdType, S: BuildHasher> VisitSet<I> for HashSet<I, S> {
    fn visit(&mut self, id: I) -> bool {
        self.insert(id)
    }

    fn is_visited(&self, id: &I) -> bool {
        self.contains(id)
    }

    fn visited_count(&self) -> usize {
        self.len()
    }

    fn reset_visited(&mut self) {
        self.clear()
    }
}

impl<I: IntegerIdType> VisitSet<I> for FixedBitSet {
    fn visit(&mut self, id: I) -> bool {
        if self.len() < id.as_usize() {
            self.grow(id.as_usize() - self.len());
        }
        !self.put(id.as_usize())
    }

    fn is_visited(&self, id: &I) -> bool {
        self.contains(id.as_usize())
    }

    fn visited_count(&self) -> usize {
        self.count_ones(0..self.len())
    }

    fn reset_visited(&mut self) {
        self.clear()
    }
}

impl<I: IntegerIdType> VisitSet<I> for TypedBitSet<I> {
    fn visit(&mut self, id: I) -> bool {
        (**self).visit(id)
    }

    fn is_visited(&self, id: &I) -> bool {
        (**self).is_visited(id)
    }

    fn visited_count(&self) -> usize {
        VisitSet::<I>::visited_count(&**self)
    }

    fn reset_visited(&mut self) {
        VisitSet::<I>::reset_visited(&mut **self)
    }
}

/// Tiny [`FixedBitSet`] wrapper adding a generic type of the elements the set
/// holds.
pub struct TypedBitSet<T> {
    inner: FixedBitSet,
    ty: PhantomData<T>,
}

impl<T> TypedBitSet<T> {
    /// Creates a new empty bit set.
    pub fn new() -> Self {
        Self {
            inner: FixedBitSet::new(),
            ty: PhantomData,
        }
    }

    /// Creates a new empty bit set with given capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            inner: FixedBitSet::with_capacity(capacity),
            ty: PhantomData,
        }
    }
}

impl<T> Default for TypedBitSet<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Deref for TypedBitSet<T> {
    type Target = FixedBitSet;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for TypedBitSet<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
