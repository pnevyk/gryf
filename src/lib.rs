#![allow(incomplete_features)]
#![feature(generic_associated_types)]
#![feature(maybe_uninit_ref)]
#![feature(maybe_uninit_extra)]
#![feature(assert_matches)]

pub mod algo;
pub mod export;
pub mod graph;
mod index;
pub mod infra;
pub mod marker;
pub mod storage;
pub mod traits;
mod util;
pub mod visit;
pub mod weight;

#[cfg(feature = "arbitrary")]
pub mod arbitrary;

#[cfg(feature = "proptest")]
pub mod proptest;

pub mod testing;

pub use index::{EdgeIndex, IndexType, VertexIndex};

pub mod prelude {
    pub use crate::marker::{Directed, EdgeType, Incoming, Outgoing, Undirected};
    pub use crate::storage::AdjList;
    pub use crate::traits::*;
    pub use crate::{EdgeIndex, VertexIndex};
}
