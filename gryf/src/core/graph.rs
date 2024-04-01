use super::{id::IdType, marker::EdgeType};

pub trait GraphBase {
    type VertexId: IdType;
    type EdgeId: IdType;
    type EdgeType: EdgeType;
}

mod imp {
    use crate::core::id::GraphIdTypes;

    use super::*;

    impl<G> GraphBase for &G
    where
        G: GraphBase,
    {
        type VertexId = G::VertexId;
        type EdgeId = G::EdgeId;
        type EdgeType = G::EdgeType;
    }

    impl<G> GraphBase for &mut G
    where
        G: GraphBase,
    {
        type VertexId = G::VertexId;
        type EdgeId = G::EdgeId;
        type EdgeType = G::EdgeType;
    }

    impl<G> GraphIdTypes for G
    where
        G: GraphBase,
    {
        type VertexId = G::VertexId;
        type EdgeId = G::EdgeId;
    }
}
