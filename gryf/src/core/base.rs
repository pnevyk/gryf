//! Helper traits for giving semantics to existing types.

use super::{
    borrow::OwnableRef,
    id::{IdPair, IdType},
    marker::Direction,
};

/// Default [reference](VertexReference) to a vertex with an attribute in a
/// graph.
#[doc(alias = "Node")]
pub struct VertexRef<'a, VI: IdType, V> {
    /// Vertex ID.
    pub id: VI,

    /// Vertex attribute.
    pub attr: &'a V,
}

/// Generic reference to a vertex with an attribute in a graph.
pub trait VertexReference<VI: IdType, V> {
    /// Vertex ID.
    fn id(&self) -> &VI;

    /// Vertex attribute.
    fn attr(&self) -> &V;
}

/// Default [reference](EdgeReference) to an edge with an attribute in a graph.
pub struct EdgeRef<'a, VI: IdType, EI: IdType, E> {
    /// Edge ID.
    pub id: EI,

    /// Edge attribute.
    pub attr: &'a E,

    /// First endpoint of the edge. In directed graphs, this is the tail
    /// (source, origin).
    pub from: VI,

    /// Second endpoint of the edge. In directed graphs, this is the head
    /// (target, destination).
    pub to: VI,
}

/// Generic reference to an edge with an attribute in a graph.
pub trait EdgeReference<VI: IdType, EI: IdType, E> {
    /// Edge ID.
    fn id(&self) -> &EI;

    /// Edge attribute.
    fn attr(&self) -> &E;

    /// First endpoint of the edge. In directed graphs, this is the tail
    /// (source, origin).
    fn from(&self) -> &VI;

    /// Second endpoint of the edge. In directed graphs, this is the head
    /// (target, destination).
    fn to(&self) -> &VI;
}

/// Default [reference](NeighborReference) to a neighbor in a graph.
pub struct NeighborRef<VI: IdType, EI: IdType> {
    /// Vertex ID.
    pub id: VI,

    /// Edge ID that led to the vertex.
    pub edge: EI,

    /// The other endpoint of the edge led to the vertex.
    pub pred: VI,

    /// Direction of the edge that led to the vertex.
    pub dir: Direction,
}

/// Generic reference to a neighbor in a graph.
pub trait NeighborReference<VI: IdType, EI: IdType> {
    /// Vertex ID.
    fn id(&self) -> OwnableRef<'_, VI>;

    /// Edge ID that led to the vertex.
    fn edge(&self) -> OwnableRef<'_, EI>;

    /// The other endpoint of the edge led to the vertex.
    fn pred(&self) -> OwnableRef<'_, VI>;

    /// Direction of the edge that led to the vertex.
    fn dir(&self) -> Direction;
}

/// Helper trait for representing an edge for convenience for edges with default
/// attributes or without attributes.
///
/// # Examples
///
/// ```
/// use gryf::core::{
///     base::IntoEdge,
///     id::{DefaultId, IdType, VertexId},
/// };
///
/// fn take_edge<E>(e: impl IntoEdge<DefaultId, E>) -> E {
///     let (_from, _to, attr) = e.unpack();
///     attr
/// }
///
/// let from = VertexId::from_usize(0);
/// let to = VertexId::from_usize(1);
///
/// let attr: i32 = take_edge((from, to, 42));
/// assert_eq!(attr, 42);
///
/// let attr: i32 = take_edge((from, to));
/// assert_eq!(attr, 0);
///
/// let attr: () = take_edge((from, to));
/// ```
pub trait IntoEdge<Id: IdPair, E> {
    /// Takes the edge representation and returns its endpoints and attribute.
    fn unpack(self) -> (Id::VertexId, Id::VertexId, E);
}

mod imp {
    use super::*;

    impl<'a, VI: IdType, V> VertexReference<VI, V> for VertexRef<'a, VI, V> {
        fn id(&self) -> &VI {
            &self.id
        }

        fn attr(&self) -> &V {
            self.attr
        }
    }

    impl<'a, VI: IdType, EI: IdType, E> EdgeReference<VI, EI, E> for EdgeRef<'a, VI, EI, E> {
        fn id(&self) -> &EI {
            &self.id
        }

        fn attr(&self) -> &E {
            self.attr
        }

        fn from(&self) -> &VI {
            &self.from
        }

        fn to(&self) -> &VI {
            &self.to
        }
    }

    impl<VI: IdType, EI: IdType> NeighborReference<VI, EI> for NeighborRef<VI, EI> {
        fn id(&self) -> OwnableRef<'_, VI> {
            OwnableRef::Borrowed(&self.id)
        }

        fn edge(&self) -> OwnableRef<'_, EI> {
            OwnableRef::Borrowed(&self.edge)
        }

        fn pred(&self) -> OwnableRef<'_, VI> {
            OwnableRef::Borrowed(&self.pred)
        }

        fn dir(&self) -> Direction {
            self.dir
        }
    }

    impl<Id: IdPair, E, I: Into<Id::VertexId>> IntoEdge<Id, E> for (I, I, E) {
        fn unpack(self) -> (Id::VertexId, Id::VertexId, E) {
            (self.0.into(), self.1.into(), self.2)
        }
    }

    impl<'e, Id: IdPair, E, I: Into<Id::VertexId> + Clone> IntoEdge<Id, &'e E> for &'e (I, I, E) {
        fn unpack(self) -> (Id::VertexId, Id::VertexId, &'e E) {
            (self.0.clone().into(), self.1.clone().into(), &self.2)
        }
    }

    impl<Id: IdPair, E: Default, I: Into<Id::VertexId>> IntoEdge<Id, E> for (I, I) {
        fn unpack(self) -> (Id::VertexId, Id::VertexId, E) {
            (self.0.into(), self.1.into(), E::default())
        }
    }

    impl<Id: IdPair, E: Default, I: Into<Id::VertexId> + Clone> IntoEdge<Id, E> for &(I, I) {
        fn unpack(self) -> (Id::VertexId, Id::VertexId, E) {
            (self.0.clone().into(), self.1.clone().into(), E::default())
        }
    }
}
