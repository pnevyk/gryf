use std::{fmt, marker::PhantomData};

use arbitrary::{Arbitrary, Unstructured};

use crate::index::{EdgeIndex, VertexIndex};
use crate::infra::CompactIndexMap;
use crate::marker::{Direction, EdgeType};
use crate::testing::{Applier, ApplyMutOps, MutOp};
use crate::traits::*;
use crate::{
    Edges, EdgesBase, EdgesBaseWeak, EdgesMut, EdgesWeak, Guarantee, Neighbors, Vertices,
    VerticesBase, VerticesBaseWeak, VerticesMut, VerticesWeak,
};

#[derive(
    VerticesBase,
    Vertices,
    VerticesMut,
    EdgesBase,
    Edges,
    EdgesMut,
    Neighbors,
    VerticesBaseWeak,
    VerticesWeak,
    EdgesBaseWeak,
    EdgesWeak,
    Guarantee,
)]
pub struct ArbitraryGraph<V, E, Ty: EdgeType, G> {
    #[graph]
    graph: G,
    ty: PhantomData<(V, E, Ty)>,
}

impl<V, E, Ty: EdgeType, G> fmt::Debug for ArbitraryGraph<V, E, Ty, G>
where
    G: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.graph)
    }
}

impl<'a, V, E, Ty: EdgeType, G> Arbitrary<'a> for ArbitraryGraph<V, E, Ty, G>
where
    V: Arbitrary<'a>,
    E: Arbitrary<'a>,
    G: Default + VerticesMut<V> + EdgesMut<E, Ty>,
{
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        let mut graph = G::default();
        let mut applier = Applier::new(&mut graph);

        let _: Result<(), arbitrary::Error> = try {
            loop {
                // Choose a kind of operation with different weights.
                let kind = u.choose(&[
                    OpKind::Vertex,
                    OpKind::Vertex,
                    OpKind::Edge,
                    OpKind::Edge,
                    OpKind::Edge,
                    OpKind::Edge,
                    OpKind::Edge,
                ])?;

                let op = match kind {
                    OpKind::Vertex => MutOp::AddVertex(V::arbitrary(u)?),
                    OpKind::Edge => MutOp::AddEdge(
                        usize::arbitrary(u)?,
                        usize::arbitrary(u)?,
                        E::arbitrary(u)?,
                        PhantomData,
                    ),
                };

                applier.apply_one(op);
            }
        };

        Ok(Self {
            graph,
            ty: PhantomData,
        })
    }
}

#[derive(Arbitrary)]
enum OpKind {
    Vertex,
    Edge,
}

impl<V, E, Ty: EdgeType, G> ArbitraryGraph<V, E, Ty, G> {
    pub fn into_inner(self) -> G {
        self.graph
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::marker::Undirected;
    use crate::storage::AdjList;

    #[test]
    fn is_arbitrary() {
        fn assert_arbitrary<'a, T: Arbitrary<'a>>() {}

        assert_arbitrary::<ArbitraryGraph<(), (), Undirected, AdjList<_, _, _>>>();
    }
}
