//! Traits representing semantic properties of graphs.

use super::id::IdType;

/// Type-level information that the implementing graph supports multiple edges
/// between two vertices.
pub trait MultiEdge {}

/// Represents the level of ID stability.
///
/// _Stable IDs_ means that the IDs of existing vertices or edges don't change
/// even when other vertices or edges are removed from the graph. This is a
/// property of the graph (storage). It is important for cases which depend on
/// stability of IDs kept around.
pub trait Stability: private::Sealed + 'static {
    /// Returns true if the stability level allows to replace IDs of removed
    /// elements with _new_ elements.
    fn can_replace_removed() -> bool;
}

/// [Stability] level disallowing any assignment of IDs of removed elements.
#[derive(Debug, Clone, Copy)]
pub enum NoReplace {}

/// [Stability] level allowing assigning IDs of removed elements to new elements.
#[derive(Debug, Clone, Copy)]
pub enum ReplaceRemoved {}

impl Stability for NoReplace {
    fn can_replace_removed() -> bool {
        false
    }
}

impl Stability for ReplaceRemoved {
    fn can_replace_removed() -> bool {
        true
    }
}

/// Type-level information that the implementing graph has stable IDs for given
/// ID type (vertices or edges) with given [stability](Stability) level.
pub trait StableId<T: IdType, S: Stability> {}

/// Trait for various graph properties known at compile-time.
///
/// Compile-time properties can be utilized for performance-oriented choices
/// that can exploit the properties. For example, there can be a more efficient
/// algorithm that works only for a specific class of graphs.
///
/// The default implementation returns `false` for all properties which should
/// always be a safe option.
pub trait Guarantee {
    /// Returns `true` if the graph does not contain loops (edges from a vertex
    /// to itself).
    ///
    /// Note that loops are not [cycles].
    ///
    /// [cycles]: https://en.wikipedia.org/wiki/Cycle_(graph_theory)
    fn is_loop_free() -> bool {
        false
    }

    /// Returns `true` if the graph contains only [path] subgraphs (one or
    /// more).
    ///
    /// Note that the graph doesn't need to be
    /// [connected](Guarantee::is_connected).
    ///
    /// [path]: https://en.wikipedia.org/wiki/Path_(graph_theory)
    fn has_paths_only() -> bool {
        false
    }

    /// Returns `true` if the graph contains only [tree] subgraphs (one or
    /// more).
    ///
    /// Note that the graph doesn't need to be
    /// [connected](Guarantee::is_connected).
    ///
    /// [tree]: https://en.wikipedia.org/wiki/Tree_(graph_theory)
    fn has_trees_only() -> bool {
        // Paths are also trees by definition.
        Self::has_paths_only()
    }

    /// Returns `true` if the graph contains only [bipartite] subgraphs (one or
    /// more).
    ///
    /// Note that the graph doesn't need to be
    /// [connected](Guarantee::is_connected).
    ///
    /// [bipartite]: https://en.wikipedia.org/wiki/Bipartite_graph
    fn has_bipartite_only() -> bool {
        // Paths and trees are bipartite by definition.
        Self::has_paths_only() || Self::has_trees_only()
    }

    /// Returns `true` if the graph is connected, that is, there exists a path
    /// between any two vertices.
    fn is_connected() -> bool {
        false
    }
}

/// Trait for controlled construction of constrained classes of graphs.
pub trait Constrained<G> {
    /// Error representing the violated semantic property of the graph class.
    type Error;

    /// Returns `Ok` if the passed graph satisfies all semantic properties of
    /// the graph class. Otherwise, returns an error corresponding to the
    /// violated property.
    fn check(graph: &G) -> Result<(), Self::Error>;

    /// Returns `Ok` with the graph class wrapper if the passed graph satisfies
    /// all semantic properties of the graph class. Otherwise, returns an error
    /// corresponding to the violated property.
    fn constrain(graph: G) -> Result<Self, Self::Error>
    where
        Self: Sized;
}

mod private {
    use super::*;

    pub trait Sealed {}

    impl Sealed for NoReplace {}
    impl Sealed for ReplaceRemoved {}
}

mod imp {
    use super::{Guarantee, MultiEdge};

    impl<G> MultiEdge for &G where G: MultiEdge {}
    impl<G> MultiEdge for &mut G where G: MultiEdge {}

    macro_rules! deref_guarantee {
        ($($ref_kind:tt)*) => {
            impl<G> Guarantee for $($ref_kind)* G
            where
                G: Guarantee,
            {
                fn is_loop_free() -> bool {
                    G::is_loop_free()
                }

                fn has_paths_only() -> bool {
                    G::has_paths_only()
                }

                fn has_trees_only() -> bool {
                    G::has_trees_only()
                }

                fn has_bipartite_only() -> bool {
                    G::has_bipartite_only()
                }

                fn is_connected() -> bool {
                    G::is_connected()
                }
            }
        }
    }

    deref_guarantee!(&);
    deref_guarantee!(&mut );
}
