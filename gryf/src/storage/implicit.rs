//! Graph represented by a function returning neighbors of a vertex.
//!
//! [Implicit graph](https://en.wikipedia.org/wiki/Implicit_graph) is a
//! representation that does not store the structure and data explicitly, but
//! produces them algorithmically. [`Implicit`] type is a wrapper over a
//! function returning neighbors of a vertex along with distances that
//! implements gryf traits that enable to use it with algorithms that do not
//! require graph finiteness.
//!
//! # Examples
//!
//! This example is [adapted from `pathfinding` crate
//! documentation](https://docs.rs/pathfinding/4.14.0/pathfinding/directed/dijkstra/fn.dijkstra_all.html#example):
//!
//! ```
//! use gryf::{
//!     algo::ShortestPaths,
//!     storage::{Implicit, implicit::ImplicitId}
//! };
//!
//! fn successors(&n: &u32) -> Vec<(u32, usize)> {
//!     if n <= 4 {
//!         vec![(n * 2, 10), (n * 2 + 1, 10)]
//!     } else {
//!         vec![]
//!     }
//! }
//!
//! let paths = ShortestPaths::on(&Implicit::new(successors))
//!     .dijkstra()
//!     // Converting u32 into the ImplicitId
//!     .run(1.into())
//!     .unwrap();
//!
//! assert_eq!(paths[ImplicitId::from(9)], 30);
//! ```

use std::{fmt::Debug, hash::Hash, marker::PhantomData};

use crate::core::{
    GraphBase, GraphWeak, Neighbors,
    base::NeighborRef,
    borrow::OwnableRef,
    id::IdType,
    marker::{Directed, Direction, EdgeType, Undirected},
};

/// Wrapper over `Fn(&V) -> IntoIterator<Item = (V, E)>`.
///
/// Types `V` and `E` must implement the supertraits of [`IdType`] so that
/// [`ImplicitId`] can be used as IDs in this implicit graph.
pub struct Implicit<V, E, N, F, Ty: EdgeType> {
    neighbors: F,
    #[allow(clippy::type_complexity)]
    ty: PhantomData<fn() -> (V, E, N, Ty)>,
}

impl<V, E, N, F> Implicit<V, E, N, F, Directed>
where
    N: IntoIterator<Item = (V, E)>,
    F: Fn(&V) -> N,
{
    /// Wraps a function returning neighbors of a vertex along with distances.
    ///
    /// The graph is considered directed by default. Use
    /// [`undirected`](Self::undirected) to mark it as undirected.
    pub fn new(neighbors: F) -> Self {
        Self {
            neighbors,
            ty: PhantomData,
        }
    }

    /// Marks the implicit graph as undirected for algorithms that require it.
    pub fn undirected(self) -> Implicit<V, E, N, F, Undirected> {
        Implicit {
            neighbors: self.neighbors,
            ty: PhantomData,
        }
    }
}

impl<V, E, N, F, Ty: EdgeType> GraphBase for Implicit<V, E, N, F, Ty>
where
    N: IntoIterator<Item = (V, E)>,
    F: Fn(&V) -> N,
    ImplicitId<V>: IdType,
    ImplicitId<E>: IdType,
{
    type VertexId = ImplicitId<V>;
    type EdgeId = ImplicitId<E>;
    type EdgeType = Ty;
}

impl<V, E, N, F, Ty: EdgeType> Neighbors for Implicit<V, E, N, F, Ty>
where
    N: IntoIterator<Item = (V, E)>,
    F: Fn(&V) -> N,
    ImplicitId<V>: IdType,
    ImplicitId<E>: IdType,
{
    type NeighborRef<'a>
        = NeighborRef<ImplicitId<V>, ImplicitId<E>>
    where
        Self: 'a;

    type NeighborsIter<'a>
        = NeighborsIter<V, E, N::IntoIter>
    where
        Self: 'a;

    fn neighbors_undirected(&self, from: &Self::VertexId) -> Self::NeighborsIter<'_> {
        let from = from.0.as_ref().expect("non-sentinel id");

        let neighbors = (self.neighbors)(from).into_iter();

        NeighborsIter {
            neighbors,
            ty: PhantomData,
        }
    }

    fn neighbors_directed(&self, from: &Self::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
        assert_eq!(
            dir,
            Direction::Outgoing,
            "incoming direction is not supported in implicit graph"
        );

        self.neighbors_undirected(from)
    }
}

impl<V, E, N, F, Ty: EdgeType> GraphWeak<V, E> for Implicit<V, E, N, F, Ty>
where
    N: IntoIterator<Item = (V, E)>,
    F: Fn(&V) -> N,
    ImplicitId<V>: IdType,
    ImplicitId<E>: IdType,
{
    fn vertex_weak<'a>(&'a self, id: &'a Self::VertexId) -> Option<OwnableRef<'a, V>> {
        id.0.as_ref().map(OwnableRef::Borrowed)
    }

    fn edge_weak<'a>(&'a self, id: &'a Self::EdgeId) -> Option<OwnableRef<'a, E>> {
        id.0.as_ref().map(OwnableRef::Borrowed)
    }
}

/// An [`IdType`] used by [`Implicit`] graph representation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplicitId<T>(Option<T>);

impl<T> From<T> for ImplicitId<T> {
    fn from(value: T) -> Self {
        Self(Some(value))
    }
}

impl<T> AsRef<T> for ImplicitId<T> {
    fn as_ref(&self) -> &T {
        self.0.as_ref().expect("non-sentinel id")
    }
}

impl<T> ImplicitId<T> {
    pub fn into_inner(self) -> T {
        self.0.expect("non-sentinel id")
    }
}

impl<T> IdType for ImplicitId<T>
where
    T: Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash,
{
    fn sentinel() -> Self {
        Self(None)
    }

    fn is_integer() -> bool {
        false
    }

    fn as_bits(&self) -> u64 {
        panic!("unsupported");
    }

    fn from_bits(_: u64) -> Self {
        panic!("unsupported");
    }
}

pub struct NeighborsIter<V, E, N> {
    neighbors: N,
    ty: PhantomData<(V, E, N)>,
}

impl<V, E, N> Iterator for NeighborsIter<V, E, N>
where
    N: Iterator<Item = (V, E)>,
    ImplicitId<V>: IdType,
    ImplicitId<E>: IdType,
{
    type Item = NeighborRef<ImplicitId<V>, ImplicitId<E>>;

    fn next(&mut self) -> Option<Self::Item> {
        let (vertex, edge) = self.neighbors.next()?;

        Some(NeighborRef {
            id: ImplicitId(Some(vertex)),
            edge: ImplicitId(Some(edge)),
            // Using sentinel to avoid cloning.
            pred: ImplicitId::sentinel(),
            dir: Direction::Outgoing,
        })
    }
}
