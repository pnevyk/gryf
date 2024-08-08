use std::fmt;

use arbitrary::{Arbitrary, Unstructured};

use crate::core::{
    error::{AddEdgeError, AddVertexError},
    id::{IdPair, IdType, IntegerIdType},
    GraphFull,
};

#[derive(Debug, Arbitrary, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Index(pub usize);

impl Index {
    pub fn get(&self, m: usize) -> Option<usize> {
        if m > 0 {
            Some(self.0 % m)
        } else {
            None
        }
    }

    pub fn map(self, m: usize) -> Option<Index> {
        self.get(m).map(Index)
    }
}

#[derive(Debug, Arbitrary, Clone)]
pub enum MutOp<V, E> {
    AddVertex(V),
    RemoveVertex(Index),
    Clear,
    AddEdge(Index, Index, E),
    RemoveEdge(Index, Index),
    ClearEdges,
}

#[derive(Debug)]
pub enum MutOpResult<V, E, VId, EId> {
    AddVertex(Result<VId, AddVertexError<V>>),
    RemoveVertex(Option<V>),
    Clear,
    AddEdge(Result<EId, AddEdgeError<E>>),
    RemoveEdge(Option<E>),
    ClearEdges,
}

impl<V, E, VId1, EId1, VId2, EId2> PartialEq<MutOpResult<V, E, VId2, EId2>>
    for MutOpResult<V, E, VId1, EId1>
where
    V: PartialEq,
    E: PartialEq,
    VId1: PartialEq<VId2>,
    EId1: PartialEq<EId2>,
{
    fn eq(&self, other: &MutOpResult<V, E, VId2, EId2>) -> bool {
        // `PartialEq` is implemented for `Result<T, E>` only for `T:
        // PartialEq<T>`.
        fn compare_results<T, U, Error>(lhs: &Result<T, Error>, rhs: &Result<U, Error>) -> bool
        where
            T: PartialEq<U>,
            Error: PartialEq,
        {
            match (lhs, rhs) {
                (Ok(lhs), Ok(rhs)) => lhs == rhs,
                (Ok(_), Err(_)) | (Err(_), Ok(_)) => false,
                (Err(lhs), Err(rhs)) => lhs == rhs,
            }
        }

        match (self, other) {
            (MutOpResult::AddVertex(lhs), MutOpResult::AddVertex(rhs)) => compare_results(lhs, rhs),
            (MutOpResult::RemoveVertex(lhs), MutOpResult::RemoveVertex(rhs)) => lhs == rhs,
            (MutOpResult::Clear, MutOpResult::Clear) => true,
            (MutOpResult::AddEdge(lhs), MutOpResult::AddEdge(rhs)) => compare_results(lhs, rhs),
            (MutOpResult::RemoveEdge(lhs), MutOpResult::RemoveEdge(rhs)) => lhs == rhs,
            (MutOpResult::ClearEdges, MutOpResult::ClearEdges) => true,
            _ => false,
        }
    }
}

impl<V, E> MutOp<V, E> {
    pub fn apply<G>(self, graph: &mut G) -> MutOpResult<V, E, G::VertexId, G::EdgeId>
    where
        G: GraphFull<V, E>,
        G::VertexId: IntegerIdType,
        G::EdgeId: IntegerIdType,
    {
        let n = graph.vertex_bound();

        match self {
            MutOp::AddVertex(vertex) => MutOpResult::AddVertex(graph.try_add_vertex(vertex)),
            MutOp::RemoveVertex(index) => {
                let id = G::VertexId::from_usize(index.get(n).unwrap_or_default());
                MutOpResult::RemoveVertex(graph.remove_vertex(&id))
            }
            MutOp::Clear => {
                graph.clear();
                MutOpResult::Clear
            }
            MutOp::AddEdge(src, dst, edge) => {
                let src = G::VertexId::from_usize(src.get(n).unwrap_or_default());
                let dst = G::VertexId::from_usize(dst.get(n).unwrap_or_default());
                MutOpResult::AddEdge(graph.try_add_edge(&src, &dst, edge))
            }
            MutOp::RemoveEdge(src, dst) => {
                let src = G::VertexId::from_usize(src.get(n).unwrap_or_default());
                let dst = G::VertexId::from_usize(dst.get(n).unwrap_or_default());
                let id = match graph.edge_id_any(&src, &dst) {
                    Some(id) => id,
                    None => return MutOpResult::RemoveEdge(None),
                };
                MutOpResult::RemoveEdge(graph.remove_edge(&id))
            }
            MutOp::ClearEdges => {
                graph.clear_edges();
                MutOpResult::ClearEdges
            }
        }
    }
}

pub struct MutOpsSeq<V, E>(pub Vec<MutOp<V, E>>);

impl<V, E> IntoIterator for MutOpsSeq<V, E> {
    type Item = MutOp<V, E>;
    type IntoIter = std::vec::IntoIter<MutOp<V, E>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<V, E> MutOpsSeq<V, E> {
    pub fn replay<G>(self, graph: &mut G)
    where
        V: fmt::Debug,
        E: fmt::Debug,
        G: GraphFull<V, E>,
        G::VertexId: IntegerIdType,
        G::EdgeId: IntegerIdType,
    {
        println!("let mut graph; // graph storage with ArbitraryIndexing");
        println!();

        for op in self {
            let n = graph.vertex_bound();

            let op = match op {
                MutOp::RemoveVertex(index) => MutOp::RemoveVertex(index.map(n).unwrap_or_default()),
                MutOp::AddEdge(src, dst, edge) => MutOp::AddEdge(
                    src.map(n).unwrap_or_default(),
                    dst.map(n).unwrap_or_default(),
                    edge,
                ),
                MutOp::RemoveEdge(src, dst) => MutOp::RemoveEdge(
                    src.map(n).unwrap_or_default(),
                    dst.map(n).unwrap_or_default(),
                ),
                op => op,
            };

            match &op {
                MutOp::AddVertex(vertex) => println!("graph.add_vertex({vertex:?});"),
                MutOp::RemoveVertex(index) => println!("graph.remove_vertex(&{index:?});"),
                MutOp::Clear => println!("graph.clear();"),
                MutOp::AddEdge(src, dst, edge) => {
                    println!("graph.add_edge(&{src:?}, &{dst:?}, {edge:?});")
                }
                MutOp::RemoveEdge(src, dst) => {
                    println!("graph.remove_edge_between(&{src:?}, &{dst:?});")
                }
                MutOp::ClearEdges => println!("graph.clear_edges();"),
            }

            op.apply(graph);
        }

        println!();
        println!("check_consistency(&graph).unwrap();");
    }
}

impl<V: fmt::Debug, E: fmt::Debug> fmt::Debug for MutOpsSeq<V, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "MutOpsSeq(vec![")?;

        for op in self.0.iter() {
            writeln!(f, "    MutOp::{:?},", op)?;
        }

        writeln!(f, "])")?;
        writeln!(f, ".replay(&mut graph);")?;
        writeln!(f)?;
        writeln!(f, "// use `cargo test fuzz_replay_mut_ops_seq`")
    }
}

impl IdType for Index {
    fn sentinel() -> Self {
        Self(usize::MAX)
    }

    fn is_integer() -> bool {
        true
    }

    fn as_bits(&self) -> u64 {
        self.0 as u64
    }

    fn from_bits(bits: u64) -> Self {
        Self(bits as usize)
    }
}

impl From<usize> for Index {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl From<Index> for usize {
    fn from(value: Index) -> Self {
        value.0
    }
}

impl IntegerIdType for Index {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ArbitraryId {}

impl IdPair for ArbitraryId {
    type VertexId = Index;
    type EdgeId = Index;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum OpKind {
    AddVertex,
    AddEdge,
    RemoveVertex,
    RemoveEdge,
    Clear,
    ClearEdges,
}

impl<'a, V, E> Arbitrary<'a> for MutOpsSeq<V, E>
where
    V: Arbitrary<'a>,
    E: Arbitrary<'a>,
{
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        let n_target = u.int_in_range(10..=1000)?;
        let r = u.nice_f64()?;

        let m_target = ((n_target * (n_target - 1) / 2) as f64 * r).round() as usize;

        let total = u.len();

        let mut n = 0usize;
        let mut m = 0usize;
        let mut seq = Vec::with_capacity(n_target);

        while !u.is_empty() {
            let rv = (n as f64 / n_target as f64).min(1.0);
            let re = (m as f64 / m_target as f64).min(1.0);
            let r = (total - u.len()) as f64 / total as f64;

            let op = match arbitrary_op(u, rv, re, r) {
                Ok(op) => op,
                Err(_) => break,
            };

            match op {
                MutOp::AddVertex(_) => n += 1,
                MutOp::RemoveVertex(_) => n = n.saturating_sub(1),
                MutOp::Clear => {
                    n = 0;
                    m = 0
                }
                MutOp::AddEdge(_, _, _) => m += 1,
                MutOp::RemoveEdge(_, _) => m = m.saturating_sub(1),
                MutOp::ClearEdges => m = 0,
            }

            seq.push(op);
        }

        Ok(MutOpsSeq(seq))
    }
}

fn arbitrary_op<'a, V, E>(
    u: &mut Unstructured<'a>,
    rv: f64,
    re: f64,
    r: f64,
) -> arbitrary::Result<MutOp<V, E>>
where
    V: Arbitrary<'a>,
    E: Arbitrary<'a>,
{
    // The more vertices/edges are in the graph, the less is needed to add them.
    // The ratio of added vertices/edges is artificially biased to be larger by
    // averaging with progress ratio. The weights of the average are chosen such
    // that the progress ratio less influences the edges so that we keep adding
    // edges during the process.
    let wv = non_linear_decrease(0.4 * rv + 0.6 * r);
    let we = non_linear_decrease(0.6 * re + 0.4 * r);

    u.choose_weighted(
        &[
            OpKind::AddVertex,
            OpKind::AddEdge,
            OpKind::RemoveVertex,
            OpKind::RemoveEdge,
            OpKind::Clear,
            OpKind::ClearEdges,
        ],
        // Removal weights are opposite to the adding weights, scaled down a
        // little bit (to prefer adding compared to removing). Clearing should
        // be rare operation.
        &[wv, we, (1.0 - wv) * 0.25, (1.0 - we) * 0.5, 0.01, 0.01],
    )
    .and_then(|kind| match kind {
        OpKind::AddVertex => Ok(MutOp::AddVertex(u.arbitrary()?)),
        OpKind::AddEdge => Ok(MutOp::AddEdge(
            u.arbitrary()?,
            u.arbitrary()?,
            u.arbitrary()?,
        )),
        OpKind::RemoveVertex => Ok(MutOp::RemoveVertex(u.arbitrary()?)),
        OpKind::RemoveEdge => Ok(MutOp::RemoveEdge(u.arbitrary()?, u.arbitrary()?)),
        OpKind::Clear => Ok(MutOp::Clear),
        OpKind::ClearEdges => Ok(MutOp::ClearEdges),
    })
}

trait UnstructuredExt {
    fn nice_f64(&mut self) -> arbitrary::Result<f64>;
    fn choose_weighted<'b, T>(
        &mut self,
        choices: &'b [T],
        weights: &'b [f64],
    ) -> arbitrary::Result<&'b T>;
}

impl<'a> UnstructuredExt for Unstructured<'a> {
    fn nice_f64(&mut self) -> arbitrary::Result<f64> {
        const RESOLUTION: u8 = 100;
        let int = self.int_in_range(0..=RESOLUTION)?;
        Ok(int as f64 / RESOLUTION as f64)
    }

    fn choose_weighted<'b, T>(
        &mut self,
        choices: &'b [T],
        weights: &'b [f64],
    ) -> arbitrary::Result<&'b T> {
        if choices.is_empty() || choices.len() != weights.len() {
            return Err(arbitrary::Error::EmptyChoose);
        }

        let weight_sum = weights.iter().copied().sum::<f64>();

        let random = self.nice_f64()?;
        let bound = random * weight_sum;

        let mut acc = 0.0;
        for (choice, weight) in choices.iter().zip(weights.iter().copied()) {
            acc += weight;

            if acc >= bound {
                return Ok(choice);
            }
        }

        unreachable!();
    }
}

// f(0) = 1, f(1) ~= 0.152
fn non_linear_decrease(x: f64) -> f64 {
    1.0 / (x + 1.0).powf(std::f64::consts::E)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use fastrand::Rng;

    use super::*;

    #[test]
    fn mut_ops_seq_arbitrary_sanity() {
        let mut frequency = HashMap::<_, usize>::new();
        let mut position = HashMap::<_, Vec<f64>>::new();

        let mut total = 0;

        // Try to collect operations frequency and position at which they occur
        // in the sequence from a few runs.
        for size in [500, 1000, 5000, 10000] {
            let mut raw = vec![0; size];

            for seed in [0, 3, 7, 13, 23, 42, 69, 123, 666, 1024] {
                let rng = Rng::with_seed(seed);
                rng.fill(&mut raw);

                let mut u = Unstructured::new(&raw);
                let seq: MutOpsSeq<i8, i8> = u.arbitrary().unwrap();

                let count = seq.0.len();
                total += count;

                for (i, op) in seq.into_iter().enumerate() {
                    let kind = match op {
                        MutOp::AddVertex(_) => OpKind::AddVertex,
                        MutOp::RemoveVertex(_) => OpKind::RemoveVertex,
                        MutOp::Clear => OpKind::Clear,
                        MutOp::AddEdge(_, _, _) => OpKind::AddEdge,
                        MutOp::RemoveEdge(_, _) => OpKind::RemoveEdge,
                        MutOp::ClearEdges => OpKind::ClearEdges,
                    };

                    *frequency.entry(kind).or_default() += 1;
                    position
                        .entry(kind)
                        .or_default()
                        .push(i as f64 / count as f64);
                }
            }
        }

        let frequency = frequency
            .into_iter()
            .map(|(key, value)| (key, value as f64 / total as f64))
            .collect::<HashMap<_, _>>();
        let position = position
            .into_iter()
            .map(|(key, values)| {
                let count = values.len();
                let average = values.into_iter().sum::<f64>() / count as f64;
                (key, average)
            })
            .collect::<HashMap<_, _>>();

        macro_rules! assert_range {
            ($range:expr, $map:expr, $kind:expr) => {
                let value = $map.get(&$kind).copied().unwrap_or_default();
                assert!(
                    ($range).contains(&value),
                    "{} of {:?} ({} not in {:?})",
                    stringify!($map),
                    $kind,
                    value,
                    $range,
                );
            };
        }

        assert_range!(0.25..=0.5, frequency, OpKind::AddVertex);
        assert_range!(0.4..=0.75, frequency, OpKind::AddEdge);
        assert_range!(0.05..=0.2, frequency, OpKind::RemoveVertex);
        assert_range!(0.05..=0.2, frequency, OpKind::RemoveEdge);
        assert_range!(0.005..=0.01, frequency, OpKind::Clear);
        assert_range!(0.005..=0.01, frequency, OpKind::ClearEdges);

        // Addition operations should lean towards the beginning of the process.
        // Removal operation should lean towards the end.
        assert_range!(0.2..=0.5, position, OpKind::AddVertex);
        assert_range!(0.2..=0.6, position, OpKind::AddEdge);
        assert_range!(0.5..=0.8, position, OpKind::RemoveVertex);
        assert_range!(0.4..=0.8, position, OpKind::RemoveEdge);
    }

    #[test]
    #[ignore = "placeholder for fuzz findings"]
    fn fuzz_replay_mut_ops_seq() {
        #[allow(unused_imports)]
        use crate::{
            core::marker::{Directed, Undirected},
            storage::*,
        };

        // Replace wit graph type under test.
        let mut graph = AdjList::<_, _, Undirected, ArbitraryId>::new();

        MutOpsSeq(vec![
            MutOp::AddVertex(0),
            MutOp::AddEdge(Index(0), Index(0), 0),
            MutOp::ClearEdges,
            MutOp::RemoveEdge(Index(0), Index(0)),
        ])
        .replay(&mut graph);

        panic!("check_consistency is required for reproduction");
    }
}
