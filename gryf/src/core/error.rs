use std::fmt;

use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
#[error("adding vertex failed: {kind}")]
pub struct AddVertexError<V> {
    pub attr: V,
    pub kind: AddVertexErrorKind,
}

impl<V> AddVertexError<V> {
    pub fn new(attr: V, kind: AddVertexErrorKind) -> Self {
        Self { attr, kind }
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
#[error("replacing vertex failed: {kind}")]
pub struct ReplaceVertexError<V> {
    pub attr: V,
    pub kind: ReplaceVertexErrorKind,
}

impl<V> ReplaceVertexError<V> {
    pub fn new(attr: V, kind: ReplaceVertexErrorKind) -> Self {
        Self { attr, kind }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReplaceVertexErrorKind {
    VertexAbsent,
}

impl fmt::Display for ReplaceVertexErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let reason = match self {
            ReplaceVertexErrorKind::VertexAbsent => "vertex does not exist",
        };
        f.write_str(reason)
    }
}

#[derive(Debug, Error, PartialEq)]
#[error("adding edge failed: {kind}")]
pub struct AddEdgeError<E> {
    pub attr: E,
    pub kind: AddEdgeErrorKind,
}

impl<E> AddEdgeError<E> {
    pub fn new(attr: E, kind: AddEdgeErrorKind) -> Self {
        Self { attr, kind }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddEdgeErrorKind {
    TailAbsent,
    HeadAbsent,
    MultiEdge,
    CapacityOverflow,
}

impl fmt::Display for AddEdgeErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let reason = match self {
            AddEdgeErrorKind::TailAbsent => "edge tail vertex does not exist",
            AddEdgeErrorKind::HeadAbsent => "edge head vertex does not exist",
            AddEdgeErrorKind::MultiEdge => {
                "an edge already exists and the graph does not allow multi edges"
            }
            AddEdgeErrorKind::CapacityOverflow => "the graph has exhausted its capacity",
        };
        f.write_str(reason)
    }
}

#[derive(Debug, Error)]
#[error("replacing edge failed: {kind}")]
pub struct ReplaceEdgeError<V> {
    pub attr: V,
    pub kind: ReplaceEdgeErrorKind,
}

impl<V> ReplaceEdgeError<V> {
    pub fn new(attr: V, kind: ReplaceEdgeErrorKind) -> Self {
        Self { attr, kind }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReplaceEdgeErrorKind {
    EdgeAbsent,
}

impl fmt::Display for ReplaceEdgeErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let reason = match self {
            ReplaceEdgeErrorKind::EdgeAbsent => "edge does not exist",
        };
        f.write_str(reason)
    }
}

#[derive(Debug, Error, PartialEq)]
pub enum AddEdgeConnectingError<V, E> {
    #[error("{0}")]
    AddVertex(#[from] AddVertexError<V>),
    #[error("{0}")]
    AddEdge(#[from] AddEdgeError<E>),
}
