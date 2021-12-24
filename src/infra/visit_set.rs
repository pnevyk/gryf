use std::{
    collections::{BTreeSet, HashSet},
    hash::BuildHasher,
};

use fixedbitset::FixedBitSet;

use crate::IndexType;

pub trait VisitSet<I: IndexType> {
    fn visit(&mut self, index: I) -> bool;
    fn is_visited(&self, index: I) -> bool;
}

impl<I: IndexType> VisitSet<I> for BTreeSet<I> {
    fn visit(&mut self, index: I) -> bool {
        self.insert(index)
    }

    fn is_visited(&self, index: I) -> bool {
        self.contains(&index)
    }
}

impl<I: IndexType, S: BuildHasher> VisitSet<I> for HashSet<I, S> {
    fn visit(&mut self, index: I) -> bool {
        self.insert(index)
    }

    fn is_visited(&self, index: I) -> bool {
        self.contains(&index)
    }
}

impl<I: IndexType> VisitSet<I> for FixedBitSet {
    fn visit(&mut self, index: I) -> bool {
        !self.put(index.to_usize())
    }

    fn is_visited(&self, index: I) -> bool {
        self.contains(index.to_usize())
    }
}
