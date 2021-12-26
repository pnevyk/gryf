use std::ops::Deref;

use crate::index::{EdgeIndex, VertexIndex};
use crate::infra::CompactIndexMap;
use crate::marker::{Direction, EdgeType};
use crate::traits::*;
use crate::{Vertices, Edges, Neighbors};

#[derive(Debug, Vertices, Edges, Neighbors)]
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

impl<S: Guarantee> Guarantee for Frozen<S> {
    fn is_loop_free() -> bool {
        S::is_loop_free()
    }

    fn has_paths_only() -> bool {
        S::has_paths_only()
    }

    fn has_trees_only() -> bool {
        S::has_trees_only()
    }

    fn has_bipartite_only() -> bool {
        S::has_bipartite_only()
    }

    fn is_connected<Ty: EdgeType>() -> bool {
        S::is_connected::<Ty>()
    }
}

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
