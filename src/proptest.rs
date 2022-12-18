use std::{fmt::Debug, marker::PhantomData};

use proptest::{
    arbitrary::{any, Arbitrary},
    strategy::Strategy,
};

use crate::{
    index::NumIndexType,
    marker::EdgeType,
    testing::{Applier, ApplyMutOps, ApplyOptions, MutOp},
    traits::Create,
};

// For testing correctness of storages.
pub fn graph_ops_strategy<V, E, Ty: EdgeType, G>() -> impl Strategy<Value = G>
where
    V: Arbitrary + Debug,
    E: Arbitrary + Debug,
    Ty: Debug,
    G: Create<V, E, Ty> + Debug,
    G::VertexIndex: NumIndexType,
{
    graph_ops_strategy_with(ApplyOptions::default())
}

pub fn graph_ops_strategy_with<V, E, Ty: EdgeType, G>(
    options: ApplyOptions,
) -> impl Strategy<Value = G>
where
    V: Arbitrary + Debug,
    E: Arbitrary + Debug,
    Ty: Debug,
    G: Create<V, E, Ty> + Debug,
    G::VertexIndex: NumIndexType,
{
    any::<Vec<MutOp<V, E, Ty>>>().prop_map(move |ops| {
        let mut graph = G::with_capacity(ops.len(), ops.len());
        let mut applier = Applier::with_options(&mut graph, options);
        applier.apply_many(ops.into_iter());
        graph
    })
}

pub fn graph_strategy<V, E, Ty: EdgeType, G>() -> impl Strategy<Value = G>
where
    V: Arbitrary + Debug,
    E: Arbitrary + Debug,
    Ty: Debug,
    G: Create<V, E, Ty> + Debug,
    G::VertexIndex: NumIndexType,
{
    graph_strategy_with(ApplyOptions::default())
}

pub fn graph_strategy_with<V, E, Ty: EdgeType, G>(options: ApplyOptions) -> impl Strategy<Value = G>
where
    V: Arbitrary + Debug,
    E: Arbitrary + Debug,
    Ty: Debug,
    G: Create<V, E, Ty> + Debug,
    G::VertexIndex: NumIndexType,
{
    (any::<Vec<V>>(), any::<Vec<(usize, usize, E)>>()).prop_map(move |(vertices, edges)| {
        let vertex_count = vertices.len();
        let edge_count = edges.len();

        let vertices = vertices.into_iter().map(|v| MutOp::AddVertex(v));
        let edges = edges
            .into_iter()
            .map(|(src, dst, e)| MutOp::AddEdge(src, dst, e, PhantomData));
        let ops = vertices.chain(edges);

        let mut graph = G::with_capacity(vertex_count, edge_count);
        let mut applier = Applier::with_options(&mut graph, options);
        applier.apply_many(ops);
        graph
    })
}
