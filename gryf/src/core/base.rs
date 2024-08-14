use super::{
    borrow::OwnableRef,
    id::{IdPair, IdType},
    marker::Direction,
};

pub trait VertexRef<VI: IdType, V> {
    fn id(&self) -> &VI;
    fn attr(&self) -> &V;
}

pub trait EdgeRef<VI: IdType, EI: IdType, E> {
    fn id(&self) -> &EI;
    fn attr(&self) -> &E;
    fn from(&self) -> &VI;
    fn to(&self) -> &VI;
}

pub trait NeighborRef<VI: IdType, EI: IdType> {
    fn id(&self) -> OwnableRef<'_, VI>;
    fn edge(&self) -> OwnableRef<'_, EI>;
    fn pred(&self) -> OwnableRef<'_, VI>;
    fn dir(&self) -> Direction;
}

pub trait IntoEdge<Id: IdPair, E> {
    fn unpack(self) -> (Id::VertexId, Id::VertexId, E);
}

mod imp {
    use super::*;

    impl<'a, VI: IdType, V> VertexRef<VI, V> for (VI, &'a V) {
        fn id(&self) -> &VI {
            &self.0
        }

        fn attr(&self) -> &V {
            self.1
        }
    }

    impl<'a, VI: IdType, EI: IdType, E> EdgeRef<VI, EI, E> for (EI, &'a E, VI, VI) {
        fn id(&self) -> &EI {
            &self.0
        }

        fn attr(&self) -> &E {
            self.1
        }

        fn from(&self) -> &VI {
            &self.2
        }

        fn to(&self) -> &VI {
            &self.3
        }
    }

    impl<VI: IdType, EI: IdType> NeighborRef<VI, EI> for (VI, EI, VI, Direction) {
        fn id(&self) -> OwnableRef<'_, VI> {
            OwnableRef::Borrowed(&self.0)
        }

        fn edge(&self) -> OwnableRef<'_, EI> {
            OwnableRef::Borrowed(&self.1)
        }

        fn pred(&self) -> OwnableRef<'_, VI> {
            OwnableRef::Borrowed(&self.2)
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
