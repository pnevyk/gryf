use std::marker::PhantomData;

use crate::{
    index::VertexIndex,
    infra::{CompactIndexMap, TypedBitSet, VisitSet},
    marker::{Directed, Direction},
    operator::Transpose,
    traits::*,
    visit::{self, raw::*, VisitAll, Visitor},
    IndexType,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum Algo {
    Dfs,
    Kahn,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    Cycle,
}

pub struct TopoSort<'a, V, E, G>
where
    G: Vertices<V> + Edges<E, Directed>,
{
    inner: TopoSortInner<'a, V, E, G>,
}

impl<'a, V, E, G> TopoSort<'a, V, E, G>
where
    G: Neighbors + Vertices<V> + Edges<E, Directed>,
{
    pub fn run_algo(graph: &'a G, algo: Option<Algo>) -> Self {
        match algo {
            Some(Algo::Dfs) | None => Self {
                inner: TopoSortInner::Dfs(Self::run_dfs(graph).into_iter(graph)),
            },
            Some(Algo::Kahn) => Self {
                inner: TopoSortInner::Kahn(Self::run_kahn(graph)),
            },
        }
    }

    pub fn run(graph: &'a G) -> Self {
        Self::run_algo(graph, None)
    }

    pub fn run_dfs(graph: &'a G) -> DfsVisit<'a, V, E, G> {
        DfsVisit::new(graph)
    }

    pub fn run_kahn(graph: &'a G) -> KahnIter<'a, V, E, G> {
        KahnIter::new(graph)
    }
}

impl<'a, V, E, G> Iterator for TopoSort<'a, V, E, G>
where
    G: Neighbors + Vertices<V> + Edges<E, Directed>,
{
    type Item = Result<VertexIndex, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

enum TopoSortInner<'a, V, E, G>
where
    G: Vertices<V>,
{
    Dfs(visit::IntoIter<'a, DfsVisit<'a, V, E, G>, G>),
    Kahn(KahnIter<'a, V, E, G>),
}

impl<'a, V, E, G> Iterator for TopoSortInner<'a, V, E, G>
where
    G: Neighbors + Vertices<V> + Edges<E, Directed>,
{
    type Item = Result<VertexIndex, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            TopoSortInner::Dfs(dfs) => dfs.next(),
            TopoSortInner::Kahn(kahn) => kahn.next(),
        }
    }
}

pub struct DfsVisit<'a, V, E, G>
where
    G: Vertices<V> + 'a,
{
    raw: RawVisit<RawDfsExtra>,
    multi: RawVisitMulti<RawDfsExtra, VisitAll<'a, V, G>>,
    closed: TypedBitSet<VertexIndex>,
    ty: PhantomData<(V, E)>,
}

impl<'a, V, E, G> DfsVisit<'a, V, E, G>
where
    G: Vertices<V> + 'a,
{
    fn new(graph: &'a G) -> Self {
        Self {
            raw: RawVisit::new(Some(graph.vertex_count())),
            multi: RawVisitMulti::new(VisitAll::new(graph)),
            closed: TypedBitSet::with_capacity(graph.vertex_count()),
            ty: PhantomData,
        }
    }
}

impl<'a, V, E, G> Visitor<G> for DfsVisit<'a, V, E, G>
where
    G: Neighbors + Vertices<V> + Edges<E, Directed>,
{
    type Item = Result<VertexIndex, Error>;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        // The implementation differs from classic DFS algorithm for topological
        // sorting in order to achieve lazy behavior. The algorithm traverses
        // the graph in reverse DFS order and reports each vertex that is being
        // closed (i.e., when we reached vertex that has no (unreported)
        // predecessors). If a back edge is encountered, the cycle error is
        // reported instead.

        // Reverse the directions of the edges so the traversal actually works
        // in reverse direction.
        let graph = &Transpose::new(graph);

        loop {
            let mut cycle = false;

            let raw_extra_event = self.multi.next_multi(
                &mut self.raw,
                |raw| {
                    raw.next(graph, |_, raw_event| match raw_event {
                        RawEvent::Skip { vertex, .. } if !self.closed.is_visited(vertex) => {
                            // Vertex not added to the stack, but also
                            // has not been closed. That means that we
                            // encountered a back edge.
                            cycle = true;

                            // Prune to avoid unnecessary work.
                            false
                        }
                        _ => true,
                    })
                },
                |vertex| graph.contains_vertex(*vertex),
            )?;

            if cycle {
                return Some(Err(Error::Cycle));
            }

            if let RawDfsExtraEvent::Close(vertex) = raw_extra_event {
                self.closed.visit(vertex);
                return Some(Ok(vertex));
            }
        }
    }
}

pub struct KahnIter<'a, V, E, G> {
    graph: &'a G,
    map: CompactIndexMap<VertexIndex>,
    in_deg: Vec<usize>,
    // Does not need to be LIFO as the order of reported vertices with in degree
    // 0 does not matter.
    queue: Vec<VertexIndex>,
    visited: usize,
    ty: PhantomData<(V, E)>,
}

impl<'a, V, E, G> KahnIter<'a, V, E, G>
where
    G: Neighbors + Vertices<V> + 'a,
{
    fn new(graph: &'a G) -> Self {
        let map = graph.vertex_index_map();
        let mut in_deg = Vec::with_capacity(map.len());
        let mut queue = Vec::new();

        for v in graph.vertex_indices() {
            let deg = graph.degree_directed(v, Direction::Incoming);
            in_deg.push(deg);

            if deg == 0 {
                queue.push(v);
            }
        }

        Self {
            graph,
            map,
            in_deg,
            queue,
            visited: 0,
            ty: PhantomData,
        }
    }
}

impl<'a, V, E, G> Iterator for KahnIter<'a, V, E, G>
where
    G: Neighbors,
{
    type Item = Result<VertexIndex, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(vertex) = self.queue.pop() {
            self.visited += 1;

            for n in self.graph.neighbors_directed(vertex, Direction::Outgoing) {
                let i = self.map.virt(n.index()).to_usize();
                let deg = &mut self.in_deg[i];
                *deg -= 1;

                if *deg == 0 {
                    self.queue.push(n.index());
                }
            }

            Some(Ok(vertex))
        } else if self.visited != self.map.len() {
            // `self.map.len()` corresponds to vertex count.
            Some(Err(Error::Cycle))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        storage::AdjList,
        visit::{DfsEvent, DfsEvents},
    };

    fn assert_valid<'a, V, E, G>(toposort: TopoSort<'a, V, E, G>, graph: &'a G)
    where
        G: Neighbors + Vertices<V> + Edges<E, Directed> + VerticesWeak<V> + EdgesWeak<E, Directed>,
    {
        let sorted = toposort.collect::<Result<Vec<_>, _>>();

        let cycle =
            DfsEvents::new(graph)
                .start_all(graph)
                .iter(graph)
                .find_map(|event| match event {
                    DfsEvent::BackEdge { .. } => Some(Error::Cycle),
                    _ => None,
                });

        match (sorted, cycle) {
            (Ok(sorted), None) => {
                for edge in graph.edges() {
                    let i = sorted
                        .iter()
                        .copied()
                        .enumerate()
                        .find_map(|(k, v)| (v == edge.src()).then(|| Some(k)))
                        .unwrap_or_else(|| panic!("algorithm omitted vertex {:?}", edge.src()));

                    let j = sorted
                        .iter()
                        .copied()
                        .enumerate()
                        .find_map(|(k, v)| (v == edge.dst()).then(|| Some(k)))
                        .unwrap_or_else(|| panic!("algorithm omitted vertex {:?}", edge.dst()));

                    assert!(
                        i < j,
                        "invalid topological order for {:?} -> {:?}",
                        edge.src(),
                        edge.dst()
                    );
                }
            }
            (Ok(_), Some(error)) => panic!("algorithm did not detect error: {:?}", error),
            (Err(error), None) => panic!("algorithm incorrectly returned error: {:?}", error),
            (Err(_), Some(_)) => {}
        }
    }

    fn create_basic_graph() -> AdjList<(), (), Directed> {
        let mut graph = AdjList::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(v5, v2, ());
        graph.add_edge(v5, v0, ());
        graph.add_edge(v4, v0, ());
        graph.add_edge(v4, v1, ());
        graph.add_edge(v2, v3, ());
        graph.add_edge(v3, v1, ());

        graph
    }

    fn create_cyclic_graph() -> AdjList<(), (), Directed> {
        let mut graph = AdjList::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(v5, v2, ());
        graph.add_edge(v5, v0, ());
        graph.add_edge(v4, v0, ());
        graph.add_edge(v4, v1, ());
        graph.add_edge(v2, v3, ());
        graph.add_edge(v3, v1, ());

        graph.add_edge(v1, v5, ());

        graph
    }

    fn create_disconnected_graph() -> AdjList<(), (), Directed> {
        let mut graph = AdjList::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(v5, v2, ());
        graph.add_edge(v5, v0, ());
        graph.add_edge(v4, v0, ());
        graph.add_edge(v4, v1, ());
        graph.add_edge(v2, v3, ());
        graph.add_edge(v3, v1, ());

        let v6 = graph.add_vertex(());
        let v7 = graph.add_vertex(());
        let v8 = graph.add_vertex(());
        let v9 = graph.add_vertex(());

        graph.add_edge(v7, v6, ());
        graph.add_edge(v7, v8, ());
        graph.add_edge(v6, v9, ());
        graph.add_edge(v8, v9, ());

        graph
    }

    #[test]
    fn dfs_basic() {
        let graph = create_basic_graph();
        let toposort = TopoSort::run_algo(&graph, Some(Algo::Dfs));

        assert_valid(toposort, &graph);
    }

    #[test]
    fn dfs_cycle() {
        let graph = create_cyclic_graph();
        let toposort = TopoSort::run_algo(&graph, Some(Algo::Dfs));

        assert_valid(toposort, &graph);
    }

    #[test]
    fn dfs_disconnected() {
        let graph = create_disconnected_graph();
        let toposort = TopoSort::run_algo(&graph, Some(Algo::Dfs));

        assert_valid(toposort, &graph);
    }

    #[test]
    fn kahn_basic() {
        let graph = create_basic_graph();
        let toposort = TopoSort::run_algo(&graph, Some(Algo::Kahn));

        assert_valid(toposort, &graph);
    }

    #[test]
    fn kahn_cycle() {
        let graph = create_cyclic_graph();
        let toposort = TopoSort::run_algo(&graph, Some(Algo::Kahn));

        assert_valid(toposort, &graph);
    }

    #[test]
    fn kahn_disconnected() {
        let graph = create_disconnected_graph();
        let toposort = TopoSort::run_algo(&graph, Some(Algo::Kahn));

        assert_valid(toposort, &graph);
    }
}
