use std::{
    collections::{BTreeSet, HashSet},
    hash::BuildHasher,
};

use fixedbitset::FixedBitSet;

use crate::{index::IndexType, infra::TypedBitSet};

pub trait VisitSet<I: IndexType> {
    fn visit(&mut self, index: I) -> bool;
    fn is_visited(&self, index: I) -> bool;
    fn visited_count(&self) -> usize;
    fn reset_visited(&mut self);
}

impl<I: IndexType> VisitSet<I> for BTreeSet<I> {
    fn visit(&mut self, index: I) -> bool {
        self.insert(index)
    }

    fn is_visited(&self, index: I) -> bool {
        self.contains(&index)
    }

    fn visited_count(&self) -> usize {
        self.len()
    }

    fn reset_visited(&mut self) {
        self.clear();
    }
}

impl<I: IndexType, S: BuildHasher> VisitSet<I> for HashSet<I, S> {
    fn visit(&mut self, index: I) -> bool {
        self.insert(index)
    }

    fn is_visited(&self, index: I) -> bool {
        self.contains(&index)
    }

    fn visited_count(&self) -> usize {
        self.len()
    }

    fn reset_visited(&mut self) {
        self.clear()
    }
}

impl<I: IndexType> VisitSet<I> for FixedBitSet {
    fn visit(&mut self, index: I) -> bool {
        if self.len() < index.to_usize() {
            self.grow(index.to_usize() - self.len());
        }
        !self.put(index.to_usize())
    }

    fn is_visited(&self, index: I) -> bool {
        self.contains(index.to_usize())
    }

    fn visited_count(&self) -> usize {
        self.count_ones(0..self.len())
    }

    fn reset_visited(&mut self) {
        self.clear()
    }
}

impl<I: IndexType> VisitSet<I> for TypedBitSet<I> {
    fn visit(&mut self, index: I) -> bool {
        (**self).visit(index)
    }

    fn is_visited(&self, index: I) -> bool {
        (**self).is_visited(index)
    }

    fn visited_count(&self) -> usize {
        VisitSet::<I>::visited_count(&**self)
    }

    fn reset_visited(&mut self) {
        VisitSet::<I>::reset_visited(&mut **self)
    }
}
