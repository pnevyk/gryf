use std::ops::Deref;

use crate::core::{
    index::{EdgeIndex, VertexIndex},
    Stability, StableIndices,
};

use gryf_derive::{
    Edges, EdgesBase, EdgesBaseWeak, EdgesWeak, GraphBase, Guarantee, Neighbors, Vertices,
    VerticesBase, VerticesBaseWeak, VerticesWeak,
};

// TODO: Remove these imports once hygiene of procedural macros is fixed.
use crate::common::CompactIndexMap;
use crate::core::{
    index::NumIndexType,
    marker::{Direction, EdgeType},
    Edges, EdgesBase, EdgesBaseWeak, EdgesWeak, GraphBase, Guarantee, Neighbors, Vertices,
    VerticesBase, VerticesBaseWeak, VerticesWeak, WeakRef,
};

#[derive(
    Debug,
    GraphBase,
    VerticesBase,
    Vertices,
    EdgesBase,
    Edges,
    Neighbors,
    VerticesBaseWeak,
    VerticesWeak,
    EdgesBaseWeak,
    EdgesWeak,
    Guarantee,
)]
pub struct Frozen<G> {
    #[graph]
    inner: G,
}

impl<G> Frozen<G> {
    pub fn new(inner: G) -> Self {
        Self { inner }
    }

    pub fn into_inner(self) -> G {
        self.inner
    }
}

impl<G> From<G> for Frozen<G> {
    fn from(inner: G) -> Self {
        Self::new(inner)
    }
}

impl<G, S: Stability> StableIndices<VertexIndex, S> for Frozen<G> {}
impl<G, S: Stability> StableIndices<EdgeIndex, S> for Frozen<G> {}

impl<G> Deref for Frozen<G> {
    type Target = G;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

pub trait Freeze {
    fn freeze(self) -> Frozen<Self>
    where
        Self: Sized;
}
