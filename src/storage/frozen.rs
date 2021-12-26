use std::ops::Deref;

use crate::index::{EdgeIndex, VertexIndex};
use crate::infra::CompactIndexMap;
use crate::marker::{Direction, EdgeType};
use crate::traits::*;
use crate::{Edges, EdgesWeak, Guarantee, Neighbors, Vertices, VerticesWeak};

#[derive(Debug, Vertices, Edges, Neighbors, VerticesWeak, EdgesWeak, Guarantee)]
pub struct Frozen<S> {
    #[graph]
    inner: S,
}

impl<S> Frozen<S> {
    fn new(inner: S) -> Self {
        Self { inner }
    }

    pub fn into_inner(self) -> S {
        self.inner
    }
}

impl<S> From<S> for Frozen<S> {
    fn from(inner: S) -> Self {
        Self::new(inner)
    }
}

impl<S> StableIndices for Frozen<S> {}

impl<S> Deref for Frozen<S> {
    type Target = S;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

pub trait Freeze {
    fn freeze(self) -> Frozen<Self>
    where
        Self: Sized;
}
