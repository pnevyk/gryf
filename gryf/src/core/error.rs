//! Collection of graph operations errors.

use std::fmt;

use thiserror::Error;

/// The error type for vertex addition operations.
#[derive(Debug, Error, PartialEq)]
#[error("adding vertex failed: {kind}")]
pub struct AddVertexError<V> {
    /// Vertex attribute that could not be added.
    pub attr: V,
    /// Specific error kind.
    pub kind: AddVertexErrorKind,
}

impl<V> AddVertexError<V> {
    /// Creates the failed addition error with the vertex attribute and error
    /// kind.
    pub fn new(attr: V, kind: AddVertexErrorKind) -> Self {
        Self { attr, kind }
    }
}

/// Vertex addition error kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddVertexErrorKind {
    /// The graph has exhausted its capacity.
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

/// The error type for vertex replacement operations.
#[derive(Debug, Error)]
#[error("replacing vertex failed: {kind}")]
pub struct ReplaceVertexError<V> {
    /// Vertex attribute that could not be replaced with.
    pub attr: V,
    /// Specific error kind.
    pub kind: ReplaceVertexErrorKind,
}

impl<V> ReplaceVertexError<V> {
    /// Creates the failed replacement error with the vertex attribute and error
    /// kind.
    pub fn new(attr: V, kind: ReplaceVertexErrorKind) -> Self {
        Self { attr, kind }
    }
}

/// Vertex replacement error kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReplaceVertexErrorKind {
    /// The vertex does not exist.
    VertexAbsent,
}

impl fmt::Display for ReplaceVertexErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let reason = match self {
            ReplaceVertexErrorKind::VertexAbsent => "the vertex does not exist",
        };
        f.write_str(reason)
    }
}

/// The error type for edge addition operations.
#[derive(Debug, Error, PartialEq)]
#[error("adding edge failed: {kind}")]
pub struct AddEdgeError<E> {
    /// Edge attribute that could not be added.
    pub attr: E,
    /// Specific error kind.
    pub kind: AddEdgeErrorKind,
}

impl<E> AddEdgeError<E> {
    /// Creates the failed addition error with the edge attribute and error kind.
    pub fn new(attr: E, kind: AddEdgeErrorKind) -> Self {
        Self { attr, kind }
    }
}

/// Edge addition error kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddEdgeErrorKind {
    /// The tail vertex of the edge does not exist.
    TailAbsent,
    /// The head vertex of the edge does not exist.
    HeadAbsent,
    /// An edge already exists, and the graph does not allow multiple edges.
    MultiEdge,
    /// The graph has exhausted its capacity.
    CapacityOverflow,
}

impl fmt::Display for AddEdgeErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let reason = match self {
            AddEdgeErrorKind::TailAbsent => "the tail vertex of the edge does not exist",
            AddEdgeErrorKind::HeadAbsent => "the head vertex of the edge does not exist",
            AddEdgeErrorKind::MultiEdge => {
                "an edge already exists, and the graph does not allow multi edges"
            }
            AddEdgeErrorKind::CapacityOverflow => "the graph has exhausted its capacity",
        };
        f.write_str(reason)
    }
}

/// The error type for edge replacement operations.
#[derive(Debug, Error)]
#[error("replacing edge failed: {kind}")]
pub struct ReplaceEdgeError<E> {
    /// Edge attribute that could not be replaced with.
    pub attr: E,
    /// Specific error kind.
    pub kind: ReplaceEdgeErrorKind,
}

impl<E> ReplaceEdgeError<E> {
    /// Creates the failed replacement error with the edge attribute and error
    /// kind.
    pub fn new(attr: E, kind: ReplaceEdgeErrorKind) -> Self {
        Self { attr, kind }
    }
}

/// Edge replacement error kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReplaceEdgeErrorKind {
    /// The edge does not exist.
    EdgeAbsent,
}

impl fmt::Display for ReplaceEdgeErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let reason = match self {
            ReplaceEdgeErrorKind::EdgeAbsent => "the edge does not exist",
        };
        f.write_str(reason)
    }
}

/// The error type for operations that involve adding vertices and connecting
/// the with an edge.
#[derive(Debug, Error, PartialEq)]
pub enum AddEdgeConnectingError<V, E> {
    /// Error that occurred during vertex addition.
    #[error("{0}")]
    AddVertex(#[from] AddVertexError<V>),
    /// Error that occurred during edge addition.
    #[error("{0}")]
    AddEdge(#[from] AddEdgeError<E>),
}
