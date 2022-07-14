#![feature(generic_associated_types)]
#![feature(assert_matches)]
#![feature(int_log)]
#![feature(try_blocks)]
#![feature(auto_traits)]
#![feature(negative_impls)]

pub mod algo;
pub mod export;
pub mod facts;
pub mod graph;
mod index;
pub mod infra;
pub mod marker;
pub mod operator;
pub mod storage;
pub mod traits;
mod util;
pub mod visit;
pub mod weight;

pub use macros::{
    Edges, EdgesBase, EdgesBaseWeak, EdgesMut, EdgesWeak, GraphBase, Guarantee, MultiEdges,
    Neighbors, Vertices, VerticesBase, VerticesBaseWeak, VerticesMut, VerticesWeak,
};

#[cfg(feature = "arbitrary")]
pub mod arbitrary;

#[cfg(feature = "proptest")]
pub mod proptest;

pub mod testing;

pub use index::{
    DefaultIndexing, EdgeIndex, IndexType, Indexing, NumIndexType, VertexIndex, Virtual,
};

pub mod prelude {
    pub use crate::graph::Graph;
    pub use crate::marker::{Directed, EdgeType, Incoming, Outgoing, Undirected};
    pub use crate::storage::AdjList;
    pub use crate::traits::*;
    pub use crate::visit::Visitor;
    pub use crate::{EdgeIndex, VertexIndex};
}
