mod generic;
mod path;

pub use generic::Graph;
pub use path::{Path, PathError};

use thiserror::Error;

use crate::core::{AddEdgeError, AddVertexError};

#[derive(Debug, Error, PartialEq)]
pub enum AddEdgeConnectingError<V, E> {
    #[error("{0}")]
    AddVertex(#[from] AddVertexError<V>),
    #[error("{0}")]
    AddEdge(#[from] AddEdgeError<E>),
}
