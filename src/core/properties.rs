use super::index::IndexType;

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

pub trait StableIndices<T: IndexType, S: Stability> {}

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
