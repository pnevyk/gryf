pub mod adapt;
pub mod algo;
pub mod common;
pub mod core;
pub mod graph;
pub mod infra;
pub mod storage;
pub mod visit;

pub mod prelude {
    #[doc(hidden)]
    pub use crate::{
        core::base::{EdgeRef, NeighborRef, VertexRef},
        visit::Visitor,
    };
}

#[cfg(feature = "derive")]
pub mod derive {
    pub use gryf_derive::*;
}

#[cfg(test)]
mod tests {
    use gryf_derive::*;

    use crate::{
        core::{id::DefaultId, marker::Directed},
        storage::AdjList,
    };

    // Test hygiene of the custom derive macros.
    #[derive(
        GraphBase,
        Neighbors,
        VertexSet,
        EdgeSet,
        GraphRef,
        GraphMut,
        GraphAdd,
        GraphFull,
        MultiEdge,
        Guarantee,
    )]
    #[gryf_crate]
    struct TestWrapper1 {
        #[graph]
        graph: AdjList<(), (), Directed, DefaultId>,
    }
}
