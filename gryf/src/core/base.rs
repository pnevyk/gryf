use super::{
    id::{GraphIdTypes, IdType},
    marker::EdgeType,
};

pub trait GraphBase {
    type VertexId: IdType;
    type EdgeId: IdType;
    type EdgeType: EdgeType;
}

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
