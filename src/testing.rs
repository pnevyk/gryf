use crate::marker::{Direction, EdgeType};
use crate::traits::*;

pub fn create_complete<V, E, Ty: EdgeType, G>(vertex_count: usize) -> G
where
    V: Default,
    E: Default,
    G: Create<V, E, Ty>,
{
    let edge_count = if Ty::is_directed() {
        vertex_count * (vertex_count - 1)
    } else {
        vertex_count * (vertex_count - 1) / 2
    };

    let mut graph = G::with_capacity(vertex_count, edge_count);

    for _ in 0..vertex_count {
        graph.add_vertex(V::default());
    }

    for u in 0..vertex_count {
        for v in 0..vertex_count {
            if u == v {
                continue;
            }

            if !Ty::is_directed() && v > u {
                break;
            }

            graph.add_edge(u.into(), v.into(), E::default());
        }
    }

    graph
}

pub fn create_bipartite<V, E, Ty: EdgeType, G, F>(
    vertex_count_lhs: usize,
    vertex_count_rhs: usize,
    connect: F,
) -> G
where
    V: Default,
    G: Create<V, E, Ty>,
    F: Fn(usize, usize, Direction) -> Option<E>,
{
    let vertex_count = vertex_count_lhs + vertex_count_rhs;

    let mut graph = G::with_capacity(vertex_count, vertex_count);

    for _ in 0..vertex_count {
        graph.add_vertex(V::default());
    }

    for dir in Ty::directions() {
        for i in 0..vertex_count_lhs {
            for j in 0..vertex_count_rhs {
                if let Some(edge) = connect(i, j, *dir) {
                    match dir {
                        Direction::Outgoing => graph.add_edge(i.into(), j.into(), edge),
                        Direction::Incoming => graph.add_edge(j.into(), i.into(), edge),
                    };
                }
            }
        }
    }

    graph
}

pub fn create_path<V, E, Ty: EdgeType, G>(vertex_count: usize) -> G
where
    V: Default,
    E: Default,
    G: Create<V, E, Ty>,
{
    if vertex_count == 0 {
        return G::default();
    }

    let mut graph = G::with_capacity(vertex_count, vertex_count - 1);
    graph.add_vertex(V::default());

    for i in 1..vertex_count {
        let dst = graph.add_vertex(V::default());
        graph.add_edge((i - 1).into(), dst, E::default());
    }

    graph
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MutOp<V, E, Ty: EdgeType> {
    AddVertex(V),
    RemoveVertex(usize),
    AddEdge(usize, usize, E, Ty),
    RemoveEdge(usize, usize),
}

pub trait ApplyMutOps<V, E, Ty: EdgeType> {
    fn apply_one(&mut self, op: MutOp<V, E, Ty>);
    fn apply_many(&mut self, ops: impl Iterator<Item = MutOp<V, E, Ty>>) {
        for op in ops {
            self.apply_one(op);
        }
    }
}

impl<V, E, Ty: EdgeType, G> ApplyMutOps<V, E, Ty> for G
where
    G: VerticesMut<V> + EdgesMut<E, Ty>,
{
    fn apply_one(&mut self, op: MutOp<V, E, Ty>) {
        match op {
            MutOp::AddVertex(vertex) => {
                self.add_vertex(vertex);
            }
            MutOp::RemoveVertex(index) => {
                self.remove_vertex(index.into());
            }
            MutOp::AddEdge(src, dst, edge, _) => {
                self.add_edge(src.into(), dst.into(), edge);
            }
            MutOp::RemoveEdge(src, dst) => {
                let index = self.edge_index(src.into(), dst.into()).unwrap();
                self.remove_edge(index);
            }
        }
    }
}
