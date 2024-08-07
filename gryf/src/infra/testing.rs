use std::{fmt, marker::PhantomData};

use thiserror::Error;

use crate::core::{
    create::Create,
    facts,
    id::IntegerIdType,
    marker::{Direction, EdgeType},
    GraphRef, Neighbors,
};

use gryf_derive::{
    EdgeSet, GraphAdd, GraphBase, GraphFull, GraphMut, GraphRef, MultiEdge, Neighbors, VertexSet,
};

use super::export::Dot;

pub fn create_complete<V, E, G>(vertex_count: usize) -> G
where
    V: Default,
    E: Default,
    G: Create<V, E>,
{
    let mut graph = G::with_capacity(
        vertex_count,
        facts::complete_graph_edge_count::<G::EdgeType>(vertex_count),
    );

    let vertices = (0..vertex_count)
        .map(|_| graph.add_vertex(V::default()))
        .collect::<Vec<_>>();

    for u in vertices.clone() {
        for v in vertices.clone() {
            if u == v {
                continue;
            }

            if !G::EdgeType::is_directed() && v > u {
                break;
            }

            graph.add_edge(&u, &v, E::default());
        }
    }

    graph
}

pub fn create_bipartite<V, E, G, F>(
    vertex_count_lhs: usize,
    vertex_count_rhs: usize,
    connect: F,
) -> G
where
    V: Default,
    G: Create<V, E>,
    F: Fn(&G::VertexId, &G::VertexId, Direction) -> Option<E>,
{
    let vertex_count = vertex_count_lhs + vertex_count_rhs;

    let mut graph = G::with_capacity(vertex_count, vertex_count);

    let vertices_lhs = (0..vertex_count_lhs)
        .map(|_| graph.add_vertex(V::default()))
        .collect::<Vec<_>>();

    let vertices_rhs = (0..vertex_count_rhs)
        .map(|_| graph.add_vertex(V::default()))
        .collect::<Vec<_>>();

    for dir in G::EdgeType::directions() {
        for i in vertices_lhs.clone() {
            for j in vertices_rhs.clone() {
                if let Some(edge) = connect(&i, &j, *dir) {
                    match dir {
                        Direction::Outgoing => graph.add_edge(&i, &j, edge),
                        Direction::Incoming => graph.add_edge(&j, &i, edge),
                    };
                }
            }
        }
    }

    graph
}

pub fn create_path<V, E, G>(vertex_count: usize) -> G
where
    V: Default,
    E: Default,
    G: Create<V, E>,
{
    if vertex_count == 0 {
        return G::empty();
    }

    let mut graph = G::with_capacity(vertex_count, vertex_count - 1);
    let mut src = graph.add_vertex(V::default());

    for _ in 1..vertex_count {
        let dst = graph.add_vertex(V::default());
        graph.add_edge(&src, &dst, E::default());
        src = dst;
    }

    graph
}

fn degree_dir(dir: Direction) -> &'static str {
    match dir {
        Direction::Outgoing => "out",
        Direction::Incoming => "in",
    }
}

#[derive(Debug, Clone, PartialEq, Error)]
pub enum ConsistencyCheckError {
    #[error("vertex ids iterator count ({0}) is not equal to vertex count ({1})")]
    VertexIdsVertexCountMismatch(usize, usize),
    #[error("vertices iterator count ({0}) is not equal to vertex count ({1})")]
    VerticesVertexCountMismatch(usize, usize),
    #[error("vertex bound ({0}) is less than vertex count ({1})")]
    VertexBoundInvalid(usize, usize),
    #[error("edge ids iterator count ({0}) is not equal to edge count ({1})")]
    EdgeIdsEdgeCountMismatch(usize, usize),
    #[error("edges iterator count ({0}) is not equal to edge count ({1})")]
    EdgesEdgeCountMismatch(usize, usize),
    #[error("edge bound ({0}) is less than edge count ({1})")]
    EdgeBoundInvalid(usize, usize),
    #[error("edge id {0} (zero-based) is invalid, either edge does not exist or is different")]
    EdgeIdsInvalid(usize),
    #[error("sum of directed degrees ({0}) is not equal to sum of undirected degrees ({1})")]
    DirectedUndirectedDegreeMismatch(usize, usize),
    #[error("sum of degrees ({0}) is not equal to doubled edge count ({1})")]
    HandshakingLemma(usize, usize),
    #[error("sum of {} degrees ({0}) is not equal to edge count ({1})", degree_dir(*.2))]
    HandshakingLemmaDirected(usize, usize, Direction),
}

pub fn check_consistency<V, E, G>(graph: &G) -> Result<(), ConsistencyCheckError>
where
    G: GraphRef<V, E> + Neighbors,
    G::VertexId: IntegerIdType,
    G::EdgeId: IntegerIdType,
{
    enum Ordering {
        Equal,
        GreaterOrEqual,
    }

    impl Ordering {
        fn cmp<T, U>(self, lhs: &T, rhs: &U) -> bool
        where
            T: PartialOrd<U>,
        {
            match self {
                Equal => lhs == rhs,
                GreaterOrEqual => lhs >= rhs,
            }
        }
    }

    fn cmp<F, E>(actual: usize, expected: usize, ord: Ordering, error: F) -> Result<(), E>
    where
        F: FnOnce(usize, usize) -> E,
    {
        if ord.cmp(&actual, &expected) {
            Ok(())
        } else {
            Err(error(actual, expected))
        }
    }

    use Ordering::*;

    let vertex_count = graph.vertex_count();

    cmp(
        graph.vertex_ids().count(),
        vertex_count,
        Equal,
        ConsistencyCheckError::VertexIdsVertexCountMismatch,
    )?;
    cmp(
        graph.vertices().count(),
        vertex_count,
        Equal,
        ConsistencyCheckError::VerticesVertexCountMismatch,
    )?;
    cmp(
        graph.vertex_bound(),
        vertex_count,
        GreaterOrEqual,
        ConsistencyCheckError::VertexBoundInvalid,
    )?;

    let edge_count = graph.edge_count();

    cmp(
        graph.edge_ids().count(),
        edge_count,
        Equal,
        ConsistencyCheckError::EdgeIdsEdgeCountMismatch,
    )?;
    cmp(
        graph.edges().count(),
        edge_count,
        Equal,
        ConsistencyCheckError::EdgesEdgeCountMismatch,
    )?;
    cmp(
        graph.edge_bound(),
        edge_count,
        GreaterOrEqual,
        ConsistencyCheckError::EdgeBoundInvalid,
    )?;

    let invalid_edge_id = graph.edge_ids().enumerate().find_map(|(i, id)| {
        graph
            .endpoints(&id)
            // Ideally we would check `== Some(id)` but for that we would
            // need to use multi edge iterator, which is not implemented for all
            // storages.
            .filter(|(src, dst)| graph.edge_id(src, dst).any(|e| e == id))
            .is_none()
            .then_some(i)
    });

    if let Some(id) = invalid_edge_id {
        return Err(ConsistencyCheckError::EdgeIdsInvalid(id));
    }

    let deg_sum = graph
        .vertex_ids()
        .map(|id| graph.degree(&id))
        .sum::<usize>();

    let out_deg_sum = graph
        .vertex_ids()
        .map(|id| graph.degree_directed(&id, Direction::Outgoing))
        .sum::<usize>();

    let in_deg_sum = graph
        .vertex_ids()
        .map(|id| graph.degree_directed(&id, Direction::Incoming))
        .sum::<usize>();

    if G::EdgeType::is_directed() {
        cmp(
            out_deg_sum + in_deg_sum,
            deg_sum,
            Equal,
            ConsistencyCheckError::DirectedUndirectedDegreeMismatch,
        )?;

        fn handshaking_lemma_directed(
            dir: Direction,
        ) -> impl FnOnce(usize, usize) -> ConsistencyCheckError {
            move |actual, expected| {
                ConsistencyCheckError::HandshakingLemmaDirected(actual, expected, dir)
            }
        }

        // https://en.wikipedia.org/wiki/Handshaking_lemma
        cmp(
            in_deg_sum,
            edge_count,
            Equal,
            handshaking_lemma_directed(Direction::Incoming),
        )?;

        cmp(
            out_deg_sum,
            edge_count,
            Equal,
            handshaking_lemma_directed(Direction::Outgoing),
        )?;
    } else {
        cmp(
            out_deg_sum + in_deg_sum,
            2 * deg_sum,
            Equal,
            ConsistencyCheckError::DirectedUndirectedDegreeMismatch,
        )?;

        cmp(
            deg_sum,
            2 * edge_count,
            Equal,
            ConsistencyCheckError::HandshakingLemma,
        )?;
    }

    Ok(())
}

// A fast check for graphs similarity. This is not a full isomorphism check!
pub fn check_potential_isomorphism<V, E, G1, G2>(lhs: &G1, rhs: &G2) -> bool
where
    G1: GraphRef<V, E> + Neighbors,
    G2: GraphRef<V, E> + Neighbors,
{
    if lhs.vertex_count() != rhs.vertex_count() {
        return false;
    }

    if lhs.edge_count() != rhs.edge_count() {
        return false;
    }

    let mut deg_seq_lhs = lhs
        .vertex_ids()
        .map(|id| lhs.degree_directed(&id, Direction::Outgoing))
        .collect::<Vec<_>>();

    let mut deg_seq_rhs = rhs
        .vertex_ids()
        .map(|id| rhs.degree_directed(&id, Direction::Outgoing))
        .collect::<Vec<_>>();

    deg_seq_lhs.sort_unstable();
    deg_seq_rhs.sort_unstable();

    deg_seq_lhs == deg_seq_rhs
}

#[derive(
    GraphBase, Neighbors, VertexSet, EdgeSet, GraphRef, GraphMut, GraphAdd, GraphFull, MultiEdge,
)]
#[gryf_crate]
pub struct AsDot<V, E, G> {
    #[graph]
    graph: G,
    ty: PhantomData<(V, E)>,
}

impl<V, E, G> AsDot<V, E, G> {
    pub fn new(graph: G) -> Self {
        Self {
            graph,
            ty: PhantomData,
        }
    }
}

impl<V, E, G> fmt::Debug for AsDot<V, E, G>
where
    G: GraphRef<V, E>,
    V: fmt::Debug,
    E: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let dot = Dot::new(None, |v| format!("{v:?}"), |e| format!("{e:?}")).to_string(&self.graph);
        f.write_str(&dot)
    }
}

impl<V, E, G> PartialEq for AsDot<V, E, G>
where
    G: GraphRef<V, E>,
    V: fmt::Debug,
    E: fmt::Debug,
{
    fn eq(&self, other: &Self) -> bool {
        format!("{:?}", self) == format!("{:?}", other)
    }
}

impl<V, E, G> From<G> for AsDot<V, E, G> {
    fn from(graph: G) -> Self {
        Self::new(graph)
    }
}
