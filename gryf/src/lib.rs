pub mod adapt;
pub mod algo;
pub mod common;
pub mod core;
pub mod graph;
pub mod infra;
pub mod storage;
pub mod visit;

pub use self::graph::Graph;

pub mod prelude {
    pub use crate::core::id::{IdType, NumIdType};
    pub use crate::core::{EdgeRef, NeighborRef, VertexRef};
    pub use crate::graph::Graph;
    pub use crate::visit::Visitor;
}

#[cfg(feature = "derive")]
pub mod derive {
    pub use gryf_derive::*;
}
