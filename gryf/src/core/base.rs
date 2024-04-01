use super::{
    id::{GraphIdTypes, IdType},
    marker::{Direction, EdgeType},
    weak::WeakRef,
};

pub trait VertexRef<VId: IdType, V> {
    fn id(&self) -> &VId;
    fn attr(&self) -> &V;
}

pub trait EdgeRef<VId: IdType, EId: IdType, E> {
    fn id(&self) -> &EId;
    fn attr(&self) -> &E;
    fn src(&self) -> &VId;
    fn dst(&self) -> &VId;
}

pub trait NeighborRef<VId: IdType, EId: IdType> {
    fn id(&self) -> WeakRef<'_, VId>;
    fn edge(&self) -> WeakRef<'_, EId>;
    fn src(&self) -> WeakRef<'_, VId>;
    fn dir(&self) -> Direction;
}

pub trait IntoEdge<Id: GraphIdTypes, E, Ty: EdgeType> {
    fn unpack(self) -> (Id::VertexId, Id::VertexId, E);
}

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

mod imp {
    use super::*;

    impl<'a, VId: IdType, V> VertexRef<VId, V> for (VId, &'a V) {
        fn id(&self) -> &VId {
            &self.0
        }

        fn attr(&self) -> &V {
            self.1
        }
    }

    impl<'a, VId: IdType, EId: IdType, E> EdgeRef<VId, EId, E> for (EId, &'a E, VId, VId) {
        fn id(&self) -> &EId {
            &self.0
        }

        fn attr(&self) -> &E {
            self.1
        }

        fn src(&self) -> &VId {
            &self.2
        }

        fn dst(&self) -> &VId {
            &self.3
        }
    }

    impl<VId: IdType, EId: IdType> NeighborRef<VId, EId> for (VId, EId, VId, Direction) {
        fn id(&self) -> WeakRef<'_, VId> {
            WeakRef::Borrowed(&self.0)
        }

        fn edge(&self) -> WeakRef<'_, EId> {
            WeakRef::Borrowed(&self.1)
        }

        fn src(&self) -> WeakRef<'_, VId> {
            WeakRef::Borrowed(&self.2)
        }

        fn dir(&self) -> Direction {
            self.3
        }
    }

    impl<Id: GraphIdTypes, E, Ty: EdgeType, I: Into<Id::VertexId>> IntoEdge<Id, E, Ty> for (I, I, E) {
        fn unpack(self) -> (Id::VertexId, Id::VertexId, E) {
            (self.0.into(), self.1.into(), self.2)
        }
    }

    impl<Id: GraphIdTypes, E: Clone, Ty: EdgeType, I: Into<Id::VertexId> + Clone>
        IntoEdge<Id, E, Ty> for &(I, I, E)
    {
        fn unpack(self) -> (Id::VertexId, Id::VertexId, E) {
            (self.0.clone().into(), self.1.clone().into(), self.2.clone())
        }
    }

    impl<Id: GraphIdTypes, E: Default, Ty: EdgeType, I: Into<Id::VertexId>> IntoEdge<Id, E, Ty>
        for (I, I)
    {
        fn unpack(self) -> (Id::VertexId, Id::VertexId, E) {
            (self.0.into(), self.1.into(), E::default())
        }
    }

    impl<Id: GraphIdTypes, E: Default, Ty: EdgeType, I: Into<Id::VertexId> + Clone>
        IntoEdge<Id, E, Ty> for &(I, I)
    {
        fn unpack(self) -> (Id::VertexId, Id::VertexId, E) {
            (self.0.clone().into(), self.1.clone().into(), E::default())
        }
    }
}
