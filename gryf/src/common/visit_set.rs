use std::{
    collections::{BTreeSet, HashSet},
    hash::BuildHasher,
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use fixedbitset::FixedBitSet;

use crate::core::id::{IdType, NumIdType};

pub trait VisitSet<I: IdType> {
    fn visit(&mut self, index: I) -> bool;
    fn is_visited(&self, index: &I) -> bool;
    fn visited_count(&self) -> usize;
    fn reset_visited(&mut self);
}

impl<I: IdType> VisitSet<I> for BTreeSet<I> {
    fn visit(&mut self, index: I) -> bool {
        self.insert(index)
    }

    fn is_visited(&self, index: &I) -> bool {
        self.contains(index)
    }

    fn visited_count(&self) -> usize {
        self.len()
    }

    fn reset_visited(&mut self) {
        self.clear();
    }
}

impl<I: IdType, S: BuildHasher> VisitSet<I> for HashSet<I, S> {
    fn visit(&mut self, index: I) -> bool {
        self.insert(index)
    }

    fn is_visited(&self, index: &I) -> bool {
        self.contains(index)
    }

    fn visited_count(&self) -> usize {
        self.len()
    }

    fn reset_visited(&mut self) {
        self.clear()
    }
}

impl<I: NumIdType> VisitSet<I> for FixedBitSet {
    fn visit(&mut self, index: I) -> bool {
        if self.len() < index.to_usize() {
            self.grow(index.to_usize() - self.len());
        }
        !self.put(index.to_usize())
    }

    fn is_visited(&self, index: &I) -> bool {
        self.contains(index.to_usize())
    }

    fn visited_count(&self) -> usize {
        self.count_ones(0..self.len())
    }

    fn reset_visited(&mut self) {
        self.clear()
    }
}

impl<I: NumIdType> VisitSet<I> for TypedBitSet<I> {
    fn visit(&mut self, index: I) -> bool {
        (**self).visit(index)
    }

    fn is_visited(&self, index: &I) -> bool {
        (**self).is_visited(index)
    }

    fn visited_count(&self) -> usize {
        VisitSet::<I>::visited_count(&**self)
    }

    fn reset_visited(&mut self) {
        VisitSet::<I>::reset_visited(&mut **self)
    }
}

pub struct TypedBitSet<T> {
    inner: FixedBitSet,
    ty: PhantomData<T>,
}

impl<T> TypedBitSet<T> {
    pub fn new() -> Self {
        Self {
            inner: FixedBitSet::new(),
            ty: PhantomData,
        }
    }

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
