#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Outgoing,
    Incoming,
}

pub use Direction::*;

impl Direction {
    #[inline]
    pub fn index(&self) -> usize {
        match self {
            Direction::Outgoing => 0,
            Direction::Incoming => 1,
        }
    }

    #[inline]
    pub fn from_index(index: usize) -> Self {
        if index.is_multiple_of(2) {
            Direction::Outgoing
        } else {
            Direction::Incoming
        }
    }

    #[inline]
    #[must_use]
    pub fn opposite(&self) -> Self {
        match self {
            Outgoing => Incoming,
            Incoming => Outgoing,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Undirected {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Directed {}

pub trait EdgeType: private::Sealed + 'static {
    fn is_directed() -> bool;
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
