use std::marker::PhantomData;

use rustc_hash::FxHashSet;

use super::{OpMut, OpOwned};
use crate::facts;
use crate::index::{EdgeIndex, IndexType, VertexIndex};
use crate::infra::CompactIndexMap;
use crate::marker::{Direction, Outgoing, Undirected};
use crate::traits::*;
use crate::{Vertices, VerticesMut};

#[derive(Debug, Vertices, VerticesMut)]
pub struct Complement<V, E, G> {
    #[graph]
    graph: G,
    edge: E,
    ty: PhantomData<V>,
}

impl<V, E, G> Complement<V, E, G>
where
    G: Vertices<V> + Edges<E, Undirected>,
{
    pub fn new(graph: G, edge: E) -> Self {
        Self {
            graph,
            edge,
            ty: PhantomData,
        }
    }

    pub fn into_inner(self) -> G {
        self.graph
    }

    pub fn edge_count(&self) -> usize {
        facts::complete_graph_edge_count::<Undirected>(self.vertex_count())
            - self.graph.edge_count()
    }
}

impl<V, E, G1, G2> OpMut<G2> for Complement<V, E, G1>
where
    G1: Vertices<V> + Edges<E, Undirected>,
    G2: VerticesMut<V> + EdgesMut<E, Undirected>,
    V: Clone,
    E: Clone,
{
    fn apply_mut(self, result: &mut G2) {
        // TODO: Clear result to make sure that the indices of added vertices go
        // from zero.

        let vertex_map = self.graph.vertex_index_map();

        let mut cur = 0;

        for v in self.graph.vertices() {
            let idx = result.add_vertex(v.data().clone());

            // Assumption: adding vertices to the result graph generates index
            // sequence going from zero with step 1.
            debug_assert!(idx.to_usize() == cur, "unexpected behavior of `add_vertex`");
            cur += 1;
        }

        for u in self.graph.vertex_indices() {
            for v in self.graph.vertex_indices() {
                if u.to_usize() < v.to_usize() && self.graph.edge_index(u, v).is_none() {
                    let u = vertex_map.virt(u).to_usize().into();
                    let v = vertex_map.virt(v).to_usize().into();
                    result.add_edge(u, v, self.edge.clone());
                }
            }
        }
    }
}

impl<V, E, G1, G2> OpOwned<G2> for Complement<V, E, G1>
where
    G1: Vertices<V> + Edges<E, Undirected>,
    G2: VerticesMut<V> + EdgesMut<E, Undirected> + Create<V, E, Undirected>,
    V: Clone,
    E: Clone,
{
    fn apply(self) -> G2 {
        // XXX: Is it possible to do it in place in a way that would be more
        // efficient that the out-of-place approach? It would also have a nice
        // side effect of not changing the vertex indices when they are holes.

        let mut result = G2::with_capacity(self.graph.vertex_count(), self.edge_count());
        self.apply_mut(&mut result);
        result
    }
}

impl<V, E, G> EdgesWeak<E, Undirected> for Complement<V, E, G>
where
    G: Vertices<V> + Edges<E, Undirected>,
{
    fn edge_count_hint(&self) -> Option<usize> {
        Some(self.edge_count())
    }

    fn edge_bound_hint(&self) -> Option<usize> {
        self.edge_count_hint()
    }

    fn edge_weak(&self, index: EdgeIndex) -> Option<WeakRef<'_, E>> {
        if self.graph.contains_edge(index) {
            None
        } else {
            Some(WeakRef::borrowed(&self.edge))
        }
    }

    fn endpoints_weak(&self, _index: EdgeIndex) -> Option<(Self::VertexIndex, Self::VertexIndex)> {
        None
    }

    fn edge_index_weak(&self, src: Self::VertexIndex, dst: Self::VertexIndex) -> Option<EdgeIndex> {
        if self.graph.edge_index(src, dst).is_some() {
            None
        } else {
            Some(EdgeIndex::null())
        }
    }
}

impl<V, E, G> Neighbors for Complement<V, E, G>
where
    G: Neighbors + Vertices<V>,
{
    type NeighborRef<'a> = (VertexIndex, EdgeIndex, VertexIndex, Direction);

    type NeighborsIter<'a>
    where
        Self: 'a,
    = NeighborsIter<'a, V, G>;

    fn neighbors(&self, src: VertexIndex) -> Self::NeighborsIter<'_> {
        NeighborsIter {
            src,
            dir: Outgoing,
            neighbors: self.graph.neighbors(src).map(|n| n.index()).collect(),
            vertices: self.graph.vertex_indices(),
        }
    }

    fn neighbors_directed(
        &self,
        src: VertexIndex,
        dir: crate::marker::Direction,
    ) -> Self::NeighborsIter<'_> {
        NeighborsIter {
            src,
            dir,
            neighbors: self
                .graph
                .neighbors_directed(src, dir)
                .map(|n| n.index())
                .collect(),
            vertices: self.graph.vertex_indices(),
        }
    }
}

pub struct NeighborsIter<'a, V, G>
where
    G: Vertices<V> + 'a,
{
    src: VertexIndex,
    dir: Direction,
    neighbors: FxHashSet<VertexIndex>,
    vertices: G::VertexIndicesIter<'a>,
}

impl<'a, V, G> Iterator for NeighborsIter<'a, V, G>
where
    G: Vertices<V> + 'a,
{
    type Item = (VertexIndex, EdgeIndex, VertexIndex, Direction);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let v = self.vertices.next()?;

            if v != self.src && !self.neighbors.contains(&v) {
                return Some((v, EdgeIndex::null(), self.src, self.dir));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::storage::{AdjList, Stable};

    use super::*;

    #[test]
    fn edge_count() {
        let mut graph = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());
        graph.add_edge(v2, v3, ());
        graph.add_edge(v3, v1, ());

        let complement = Complement::new(graph, ());
        assert_eq!(complement.edge_count(), 2);
    }

    #[test]
    fn apply() {
        let mut graph = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());
        graph.add_edge(v2, v3, ());
        graph.add_edge(v3, v1, ());

        let complement: AdjList<_, _, _> = Complement::new(graph, ()).apply();

        assert!(complement.edge_index(v0, v1).is_none());
        assert!(complement.edge_index(v1, v2).is_none());
        assert!(complement.edge_index(v2, v3).is_none());
        assert!(complement.edge_index(v3, v1).is_none());

        assert!(complement.edge_index(v0, v2).is_some());
        assert!(complement.edge_index(v0, v3).is_some());
    }

    #[test]
    fn neighbors() {
        let mut graph = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());
        graph.add_edge(v2, v3, ());
        graph.add_edge(v3, v1, ());

        let complement = Complement::new(graph, ());

        assert_eq!(
            complement
                .neighbors(v0)
                .map(|n| n.index())
                .collect::<HashSet<_>>(),
            vec![v2, v3].into_iter().collect()
        );
        assert_eq!(complement.neighbors(v1).count(), 0);
        assert_eq!(
            complement
                .neighbors(v2)
                .map(|n| n.index())
                .collect::<HashSet<_>>(),
            vec![v0].into_iter().collect()
        );
        assert_eq!(
            complement
                .neighbors(v3)
                .map(|n| n.index())
                .collect::<HashSet<_>>(),
            vec![v0].into_iter().collect()
        );
    }

    #[test]
    fn apply_holes() {
        let mut graph = Stable::new(AdjList::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());
        graph.add_edge(v2, v3, ());
        graph.add_edge(v3, v4, ());
        graph.add_edge(v4, v1, ());

        graph.remove_vertex(v3);

        let complement: AdjList<_, _, _> = Complement::new(graph, ()).apply();

        // XXX: Complement does not preserve the vertex indices when there are
        // holes. This would change if we implement an in-place algorithm.
        let v0 = VertexIndex::new(0);
        let v1 = VertexIndex::new(1);
        let v2 = VertexIndex::new(2);
        let v4 = VertexIndex::new(3);

        assert!(complement.edge_index(v0, v1).is_none());
        assert!(complement.edge_index(v1, v2).is_none());
        assert!(complement.edge_index(v4, v1).is_none());

        assert!(complement.edge_index(v0, v2).is_some());
        assert!(complement.edge_index(v0, v4).is_some());
        assert!(complement.edge_index(v2, v4).is_some());
    }
}
