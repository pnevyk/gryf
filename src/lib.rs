pub mod algo;
pub mod common;
pub mod core;
pub mod graph;
pub mod infra;
pub mod ops;
pub mod storage;
pub mod visit;

pub use self::graph::Graph;

pub mod prelude {
    pub use crate::core::index::{IndexType, NumIndexType};
    pub use crate::core::{EdgeRef, NeighborRef, VertexRef};
    pub use crate::graph::Graph;
    pub use crate::visit::Visitor;
}

pub mod derive {
    pub use macros::*;
}
