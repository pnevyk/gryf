use super::{
    borrow::OwnableRef,
    id::{IdPair, IdType},
    marker::Direction,
};

pub struct VertexRef<'a, VI: IdType, V> {
    pub id: VI,
    pub attr: &'a V,
}

pub trait VertexReference<VI: IdType, V> {
    fn id(&self) -> &VI;
    fn attr(&self) -> &V;
}

pub struct EdgeRef<'a, VI: IdType, EI: IdType, E> {
    pub id: EI,
    pub attr: &'a E,
    pub from: VI,
    pub to: VI,
}

pub trait EdgeReference<VI: IdType, EI: IdType, E> {
    fn id(&self) -> &EI;
    fn attr(&self) -> &E;
    fn from(&self) -> &VI;
    fn to(&self) -> &VI;
}

pub struct NeighborRef<VI: IdType, EI: IdType> {
    pub id: VI,
    pub edge: EI,
    pub pred: VI,
    pub dir: Direction,
}

pub trait NeighborReference<VI: IdType, EI: IdType> {
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

    impl<'a, VI: IdType, V> VertexReference<VI, V> for VertexRef<'a, VI, V> {
        fn id(&self) -> &VI {
            &self.id
        }

        fn attr(&self) -> &V {
            self.attr
        }
    }

    impl<'a, VI: IdType, EI: IdType, E> EdgeReference<VI, EI, E> for EdgeRef<'a, VI, EI, E> {
        fn id(&self) -> &EI {
            &self.id
        }

        fn attr(&self) -> &E {
            self.attr
        }

        fn from(&self) -> &VI {
            &self.from
        }

        fn to(&self) -> &VI {
            &self.to
        }
    }

    impl<VI: IdType, EI: IdType> NeighborReference<VI, EI> for NeighborRef<VI, EI> {
        fn id(&self) -> OwnableRef<'_, VI> {
            OwnableRef::Borrowed(&self.id)
        }

        fn edge(&self) -> OwnableRef<'_, EI> {
            OwnableRef::Borrowed(&self.edge)
        }

        fn pred(&self) -> OwnableRef<'_, VI> {
            OwnableRef::Borrowed(&self.pred)
        }

        fn dir(&self) -> Direction {
            self.dir
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
