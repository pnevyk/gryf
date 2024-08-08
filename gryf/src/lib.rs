pub mod adapt;
pub mod algo;
pub mod common;
pub mod core;
pub mod graph;
pub mod infra;
pub mod storage;
pub mod visit;

pub mod prelude {
    #[doc(hidden)]
    pub use crate::{
        core::base::{EdgeRef, NeighborRef, VertexRef},
        visit::Visitor,
    };
}

#[cfg(feature = "derive")]
pub mod derive {
    pub use gryf_derive::*;
}

#[cfg(test)]
mod tests {
    use gryf_derive::*;

    use crate::{
        core::{
            id::DefaultId,
            marker::Directed,
            props::{Guarantee, MultiEdge},
            EdgeSet, GraphAdd, GraphBase, GraphFull, GraphMut, GraphRef, GraphWeak, Neighbors,
            VertexSet,
        },
        storage::AdjList,
    };

    // Test hygiene of the custom derive macros.
    #[derive(
        Clone,
        GraphBase,
        Neighbors,
        VertexSet,
        EdgeSet,
        GraphRef,
        GraphMut,
        GraphAdd,
        GraphFull,
        MultiEdge,
        Guarantee,
    )]
    #[gryf_crate]
    struct TestWrapper {
        #[graph]
        graph: AdjList<(), (), Directed, DefaultId>,
    }

    fn require_graph_base(_: impl GraphBase) {}
    fn require_neighbors(_: impl Neighbors) {}
    fn require_vertex_set(_: impl VertexSet) {}
    fn require_edge_set(_: impl EdgeSet) {}
    fn require_graph_ref(_: impl GraphRef<(), ()>) {}
    fn require_graph_weak(_: impl GraphWeak<(), ()>) {}
    fn require_graph_mut(_: impl GraphMut<(), ()>) {}
    fn require_graph_add(_: impl GraphAdd<(), ()>) {}
    fn require_graph_full(_: impl GraphFull<(), ()>) {}
    fn require_multi_edge(_: impl MultiEdge) {}
    fn require_guarantee(_: impl Guarantee) {}

    #[test]
    fn trait_impl() {
        let mut g = TestWrapper {
            graph: AdjList::default(),
        };

        require_graph_base(g.clone());
        require_graph_base(&g);
        require_graph_base(&mut g);

        require_neighbors(g.clone());
        require_neighbors(&g);
        require_neighbors(&mut g);

        require_vertex_set(g.clone());
        require_vertex_set(&g);
        require_vertex_set(&mut g);

        require_edge_set(g.clone());
        require_edge_set(&g);
        require_edge_set(&mut g);

        require_graph_ref(g.clone());
        require_graph_ref(&g);
        require_graph_ref(&mut g);

        require_graph_weak(g.clone());
        require_graph_weak(&g);
        require_graph_weak(&mut g);

        require_graph_mut(g.clone());
        require_graph_mut(&mut g);

        require_graph_add(g.clone());
        require_graph_add(&mut g);

        require_graph_full(g.clone());
        require_graph_full(&mut g);

        require_multi_edge(g.clone());
        require_multi_edge(&g);
        require_multi_edge(&mut g);

        require_guarantee(g.clone());
        require_guarantee(&g);
        require_guarantee(&mut g);
    }
}
