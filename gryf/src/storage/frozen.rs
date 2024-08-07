use std::ops::Deref;

use crate::core::{
    id::{EdgeId, VertexId},
    properties::{Stability, StableId},
};

use gryf_derive::{EdgeSet, GraphBase, GraphRef, Guarantee, Neighbors, VertexSet};

#[derive(Debug, GraphBase, Neighbors, VertexSet, EdgeSet, GraphRef, Guarantee)]
#[gryf_crate]
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

impl<G, S: Stability> StableId<VertexId, S> for Frozen<G> {}
impl<G, S: Stability> StableId<EdgeId, S> for Frozen<G> {}

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
