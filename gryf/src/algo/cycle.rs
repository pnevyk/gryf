use crate::core::{
    marker::EdgeType, EdgesBase, EdgesBaseWeak, GraphBase, Neighbors, VerticesBase,
    VerticesBaseWeak,
};

use self::bfs::bfs_collect;

mod bfs;
mod builder;
mod dfs;

pub struct Cycle<G: GraphBase> {
    pub edge: G::EdgeIndex,
    as_undirected: bool,
}

impl<G: GraphBase> Cycle<G> {
    pub fn collect<Ty: EdgeType>(self, graph: &G) -> Vec<G::EdgeIndex>
    where
        G: Neighbors + VerticesBase + VerticesBaseWeak + EdgesBase<Ty> + EdgesBaseWeak<Ty>,
    {
        let as_undirected = self.as_undirected;
        bfs_collect(graph, self, as_undirected)
    }
}

pub fn is_cyclic<Ty: EdgeType, G>(graph: &G) -> bool
where
    G: Neighbors + VerticesBase + VerticesBaseWeak + EdgesBase<Ty> + EdgesBaseWeak<Ty>,
{
    Cycle::on(graph).run().is_some()
}

pub fn is_cyclic_undirected<Ty: EdgeType, G>(graph: &G) -> bool
where
    G: Neighbors + VerticesBase + VerticesBaseWeak + EdgesBase<Ty> + EdgesBaseWeak<Ty>,
{
    Cycle::on(graph).as_undirected().run().is_some()
}

#[cfg(test)]
mod tests {
    use std::ops::Add;

    use crate::{
        core::{
            marker::{Directed, Undirected},
            EdgesMut, VerticesMut,
        },
        storage::AdjList,
    };

    use super::*;

    fn assert_collected<Ty: EdgeType, G>(cycle: Cycle<G>, graph: &G, expected: Vec<G::EdgeIndex>)
    where
        G: Neighbors + VerticesBase + VerticesBaseWeak + EdgesBase<Ty> + EdgesBaseWeak<Ty>,
    {
        let collected = cycle.collect(graph);
        assert_eq!(
            collected.len(),
            expected.len(),
            "collected cycle contains different number of edges than expected"
        );
        let mut collected = collected.into_iter().cycle();

        let count = expected.len();

        if !collected
            .by_ref()
            .take(count)
            .any(|edge| Some(&edge) == expected.first())
        {
            panic!("collected cycle does not include first edge from expected");
        }

        let forward = collected
            .clone()
            .zip(expected.iter().skip(1).cloned())
            .filter(|(lhs, rhs)| lhs == rhs)
            .count()
            .add(1);
        let reversed = collected
            .clone()
            .zip(expected.iter().skip(1).cloned().rev())
            .filter(|(lhs, rhs)| lhs == rhs)
            .count()
            .add(1);

        if forward != count && reversed != count {
            if forward > reversed {
                for (lhs, rhs) in collected.zip(expected.into_iter().skip(1)) {
                    assert_eq!(lhs, rhs);
                }
            } else {
                for (lhs, rhs) in collected.zip(expected.into_iter().skip(1).rev()) {
                    assert_eq!(lhs, rhs);
                }
            }
        }
    }

    #[test]
    fn cyclic_basic_undirected() {
        let mut graph = AdjList::<_, _, Undirected, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v2, &v0, ());

        let result = Cycle::on(&graph).run();
        assert!(result.is_some());
    }

    #[test]
    fn cyclic_basic_directed() {
        let mut graph = AdjList::<_, _, Directed, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v2, &v0, ());

        let result = Cycle::on(&graph).run();
        assert!(result.is_some());
    }

    #[test]
    fn acyclic_basic_undirected() {
        let mut graph = AdjList::<_, _, Undirected, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());

        let result = Cycle::on(&graph).run();
        assert!(result.is_none());
    }

    #[test]
    fn acyclic_basic_directed() {
        let mut graph = AdjList::<_, _, Directed, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());

        let result = Cycle::on(&graph).run();
        assert!(result.is_none());
    }

    #[test]
    fn acyclic_weakly_basic_directed() {
        let mut graph = AdjList::<_, _, Directed, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v0, &v2, ());

        let result = Cycle::on(&graph).run();
        assert!(result.is_none());
    }

    #[test]
    fn cyclic_directed_as_undirected() {
        let mut graph = AdjList::<_, _, Directed, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v0, &v2, ());

        let result = Cycle::on(&graph).as_undirected().run();
        assert!(result.is_some());
    }

    #[test]
    fn cycle_collected_undirected() {
        let mut graph = AdjList::<_, _, Undirected, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());

        let e0 = graph.add_edge(&v0, &v1, ());
        let e1 = graph.add_edge(&v1, &v2, ());
        let e2 = graph.add_edge(&v2, &v0, ());

        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v2, &v4, ());

        let cycle = Cycle::on(&graph).run().unwrap();
        assert_collected(cycle, &graph, vec![e0, e1, e2]);
    }

    #[test]
    fn cycle_collected_directed() {
        let mut graph = AdjList::<_, _, Directed, _>::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());

        let e0 = graph.add_edge(&v0, &v1, ());
        let e1 = graph.add_edge(&v1, &v2, ());
        let e2 = graph.add_edge(&v2, &v0, ());

        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v2, &v4, ());

        let cycle = Cycle::on(&graph).run().unwrap();
        assert_collected(cycle, &graph, vec![e0, e1, e2]);
    }
}
