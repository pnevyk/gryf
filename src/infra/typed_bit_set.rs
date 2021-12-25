use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use fixedbitset::FixedBitSet;

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
