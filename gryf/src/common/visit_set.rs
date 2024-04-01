use std::{
    collections::{BTreeSet, HashSet},
    hash::BuildHasher,
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use fixedbitset::FixedBitSet;

use crate::core::id::{IdType, NumIdType};

pub trait VisitSet<I: IdType> {
    fn visit(&mut self, id: I) -> bool;
    fn is_visited(&self, id: &I) -> bool;
    fn visited_count(&self) -> usize;
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

impl<I: NumIdType> VisitSet<I> for FixedBitSet {
    fn visit(&mut self, id: I) -> bool {
        if self.len() < id.to_usize() {
            self.grow(id.to_usize() - self.len());
        }
        !self.put(id.to_usize())
    }

    fn is_visited(&self, id: &I) -> bool {
        self.contains(id.to_usize())
    }

    fn visited_count(&self) -> usize {
        self.count_ones(0..self.len())
    }

    fn reset_visited(&mut self) {
        self.clear()
    }
}

impl<I: NumIdType> VisitSet<I> for TypedBitSet<I> {
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
