//! Traits and types representing basic properties of graphs and related types.

/// Edge direction with respect to a vertex.
///
/// The variants are exported to the scope so that they can be used directly
/// like [`gryf::core::marker::Outgoing`](crate::core::marker::Outgoing).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    /// The edge is directed away from a vertex.
    Outgoing,

    /// The edge is directed towards a vertex.
    Incoming,
}

pub use Direction::*;

impl Direction {
    /// Returns the index value representing the direction.
    ///
    /// This is useful for storing data specific to a direction into a
    /// two-element array.
    ///
    /// * `Outgoing` → 0
    /// * `Incoming` → 1
    #[inline]
    pub fn index(&self) -> usize {
        match self {
            Direction::Outgoing => 0,
            Direction::Incoming => 1,
        }
    }

    /// Parses the direction from the index value produced by
    /// [`Direction::index`].
    #[inline]
    pub fn from_index(index: usize) -> Self {
        if index.is_multiple_of(2) {
            Direction::Outgoing
        } else {
            Direction::Incoming
        }
    }

    /// Returns the opposite direction.
    ///
    /// * `Outgoing` → `Incoming`
    /// * `Incoming` → `Outgoing`
    #[inline]
    #[must_use]
    pub fn opposite(&self) -> Self {
        match self {
            Outgoing => Incoming,
            Incoming => Outgoing,
        }
    }
}

/// Represents undirected graphs.
///
/// Check [`EdgeType`] for more details.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Undirected {}

/// Represents directed graphs.
///
/// Check [`EdgeType`] for more details.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Directed {}

/// Directionality of the graph. Can be either [undirected](Undirected) or
/// [directed](Directed).
pub trait EdgeType: private::Sealed + 'static {
    /// Returns `true` if the graph is directed.
    fn is_directed() -> bool;

    /// Returns the array of relevant [directions](Direction) for the directionality.
    ///
    /// * `Undirected` → `[Outgoing]`
    /// * `Directed` → `[Outgoing, Incoming]`
    fn directions() -> &'static [Direction];
}

impl EdgeType for Undirected {
    fn is_directed() -> bool {
        false
    }

    fn directions() -> &'static [Direction] {
        &[Outgoing]
    }
}

impl EdgeType for Directed {
    fn is_directed() -> bool {
        true
    }

    fn directions() -> &'static [Direction] {
        &[Outgoing, Incoming]
    }
}

mod private {
    use super::*;

    pub trait Sealed {}

    impl Sealed for Undirected {}
    impl Sealed for Directed {}
}
