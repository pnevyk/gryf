use super::{
    id::{IdPair, IdType},
    marker::Direction,
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

pub trait IntoEdge<Id: IdPair, E> {
    fn unpack(self) -> (Id::VertexId, Id::VertexId, E);
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

    impl<Id: IdPair, E, I: Into<Id::VertexId>> IntoEdge<Id, E> for (I, I, E) {
        fn unpack(self) -> (Id::VertexId, Id::VertexId, E) {
            (self.0.into(), self.1.into(), self.2)
        }
    }

    impl<Id: IdPair, E: Clone, I: Into<Id::VertexId> + Clone> IntoEdge<Id, E> for &(I, I, E) {
        fn unpack(self) -> (Id::VertexId, Id::VertexId, E) {
            (self.0.clone().into(), self.1.clone().into(), self.2.clone())
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
