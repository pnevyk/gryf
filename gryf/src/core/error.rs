use std::fmt;

use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
#[error("adding vertex failed: {kind}")]
pub struct AddVertexError<V> {
    pub data: V,
    pub kind: AddVertexErrorKind,
}

impl<V> AddVertexError<V> {
    pub fn new(data: V) -> Self {
        Self {
            data,
            kind: AddVertexErrorKind::CapacityOverflow,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddVertexErrorKind {
    CapacityOverflow,
}

impl fmt::Display for AddVertexErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let reason = match self {
            AddVertexErrorKind::CapacityOverflow => "the graph has exhausted its capacity",
        };
        f.write_str(reason)
    }
}

#[derive(Debug, Error)]
#[error("vertex does not exist")]
pub struct ReplaceVertexError<V>(pub V);

#[derive(Debug, Error, PartialEq)]
#[error("adding edge failed: {kind}")]
pub struct AddEdgeError<E> {
    pub data: E,
    pub kind: AddEdgeErrorKind,
}

impl<E> AddEdgeError<E> {
    pub fn new(data: E, kind: AddEdgeErrorKind) -> Self {
        Self { data, kind }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddEdgeErrorKind {
    SourceAbsent,
    DestinationAbsent,
    MultiEdge,
    CapacityOverflow,
}

impl fmt::Display for AddEdgeErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let reason = match self {
            AddEdgeErrorKind::SourceAbsent => "source does not exist",
            AddEdgeErrorKind::DestinationAbsent => "destination does not exist",
            AddEdgeErrorKind::MultiEdge => {
                "an edge already exists and the graph does not allow multi edges"
            }
            AddEdgeErrorKind::CapacityOverflow => "the graph has exhausted its capacity",
        };
        f.write_str(reason)
    }
}

#[derive(Debug, Error, PartialEq)]
#[error("edge does not exist")]
pub struct ReplaceEdgeError<E>(pub E);

#[derive(Debug, Error, PartialEq)]
pub enum AddEdgeConnectingError<V, E> {
    #[error("{0}")]
    AddVertex(#[from] AddVertexError<V>),
    #[error("{0}")]
    AddEdge(#[from] AddEdgeError<E>),
}
