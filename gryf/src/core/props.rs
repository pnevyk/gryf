use super::id::IdType;

pub trait MultiEdge {}

pub trait Stability: private::Sealed + 'static {
    fn can_replace_removed() -> bool;
}

#[derive(Debug, Clone, Copy)]
pub enum NoReplace {}

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

pub trait StableId<T: IdType, S: Stability> {}

pub trait Guarantee {
    fn is_loop_free() -> bool {
        false
    }

    fn has_paths_only() -> bool {
        false
    }

    fn has_trees_only() -> bool {
        // Paths are also trees by definition.
        Self::has_paths_only()
    }

    fn has_bipartite_only() -> bool {
        // Paths and trees are bipartite by definition.
        Self::has_paths_only() || Self::has_trees_only()
    }

    fn is_connected() -> bool {
        false
    }
}

pub trait Constrained<G> {
    type Error;

    fn check(graph: &G) -> Result<(), Self::Error>;
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
