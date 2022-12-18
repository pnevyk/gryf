use rustc_hash::FxHashSet;

use super::{OpMut, OpOwned};
use crate::facts;
use crate::index::NumIndexType;
use crate::infra::CompactIndexMap;
use crate::marker::{Direction, Outgoing, Undirected};
use crate::traits::*;
use crate::{GraphBase, Vertices, VerticesBase, VerticesMut};

#[derive(Debug, GraphBase, VerticesBase, Vertices, VerticesMut)]
pub struct Complement<E, G> {
    #[graph]
    graph: G,
    edge: E,
}

impl<E, G> Complement<E, G>
where
    G: VerticesBase + EdgesBase<Undirected>,
{
    pub fn new(graph: G, edge: E) -> Self {
        Self { graph, edge }
    }

    pub fn into_unmodified(self) -> G {
        self.graph
    }

    pub fn edge_count(&self) -> usize {
        facts::complete_graph_edge_count::<Undirected>(self.vertex_count())
            - self.graph.edge_count()
    }
}

impl<V, E, G1, G2> OpMut<G2, V> for Complement<E, G1>
where
    G1: Vertices<V> + Edges<E, Undirected>,
    G2: VerticesMut<V> + EdgesMut<E, Undirected>,
    G1::VertexIndex: NumIndexType,
    G2::VertexIndex: NumIndexType,
    V: Clone,
    E: Clone,
{
    fn apply_mut(self, result: &mut G2) {
        // Make sure that the result graph is initially empty.
        result.clear();

        let vertex_map = self.graph.vertex_index_map();

        for (cur, v) in self.graph.vertices().enumerate() {
            let idx = result.add_vertex(v.data().clone());

            // Assumption: adding vertices to the result graph generates index
            // sequence going from zero with step 1.
            debug_assert!(idx.to_usize() == cur, "unexpected behavior of `add_vertex`");
        }

        for u in self.graph.vertex_indices() {
            for v in self.graph.vertex_indices() {
                if u < v && self.graph.edge_index(&u, &v).is_none() {
                    let u = NumIndexType::from_usize(vertex_map.virt(u).unwrap().to_usize());
                    let v = NumIndexType::from_usize(vertex_map.virt(v).unwrap().to_usize());
                    result.add_edge(&u, &v, self.edge.clone());
                }
            }
        }
    }
}

impl<V, E, G> OpOwned<G, V> for Complement<E, G>
where
    G: VerticesMut<V> + EdgesMut<E, Undirected> + Create<V, E, Undirected>,
    G::VertexIndex: NumIndexType,
    V: Clone,
    E: Clone,
{
    fn apply(self) -> G {
        // XXX: Is it possible to do it in place in a way that would be more
        // efficient that the out-of-place approach? It would also have a nice
        // side effect of not changing the vertex indices when they are holes.

        let mut result = G::with_capacity(self.graph.vertex_count(), self.edge_count());
        self.apply_mut(&mut result);
        result
    }
}

impl<E, G> Neighbors for Complement<E, G>
where
    G: Neighbors + VerticesBase,
    G::EdgeIndex: NumIndexType,
{
    type NeighborRef<'a> = (G::VertexIndex, G::EdgeIndex, G::VertexIndex, Direction)
    where
        Self: 'a;

    type NeighborsIter<'a> = NeighborsIter<'a, G>
    where
        Self: 'a;

    fn neighbors(&self, src: &G::VertexIndex) -> Self::NeighborsIter<'_> {
        NeighborsIter {
            src: src.clone(),
            dir: Outgoing,
            neighbors: self
                .graph
                .neighbors(src)
                .map(|n| n.index().into_owned().into())
                .collect(),
            vertices: self.graph.vertex_indices(),
        }
    }

    fn neighbors_directed(&self, src: &G::VertexIndex, dir: Direction) -> Self::NeighborsIter<'_> {
        NeighborsIter {
            src: src.clone(),
            dir,
            neighbors: self
                .graph
                .neighbors_directed(src, dir)
                .map(|n| n.index().into_owned().into())
                .collect(),
            vertices: self.graph.vertex_indices(),
        }
    }
}

pub struct NeighborsIter<'a, G>
where
    G: VerticesBase + 'a,
{
    src: G::VertexIndex,
    dir: Direction,
    neighbors: FxHashSet<WeakRef<'a, G::VertexIndex>>,
    vertices: G::VertexIndicesIter<'a>,
}

impl<'a, G> Iterator for NeighborsIter<'a, G>
where
    G: VerticesBase + 'a,
    G::EdgeIndex: NumIndexType,
{
    type Item = (G::VertexIndex, G::EdgeIndex, G::VertexIndex, Direction);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let v = self.vertices.next()?;

            if v != self.src && !self.neighbors.contains(&v) {
                return Some((v, G::EdgeIndex::null(), self.src.clone(), self.dir));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::{
        index::{DefaultIndexing, VertexIndex},
        storage::{AdjList, Stable},
    };

    use super::*;

    #[test]
    fn edge_count() {
        let mut graph: AdjList<_, _, _, DefaultIndexing> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v2, &v3, ());
        graph.add_edge(&v3, &v1, ());

        let complement = Complement::new(graph, ());
        assert_eq!(complement.edge_count(), 2);
    }

    #[test]
    fn apply() {
        let mut graph: AdjList<_, _, _, DefaultIndexing> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v2, &v3, ());
        graph.add_edge(&v3, &v1, ());

        let complement: AdjList<_, _, _, _> = Complement::new(graph, ()).apply();

        assert!(complement.edge_index(&v0, &v1).is_none());
        assert!(complement.edge_index(&v1, &v2).is_none());
        assert!(complement.edge_index(&v2, &v3).is_none());
        assert!(complement.edge_index(&v3, &v1).is_none());

        assert!(complement.edge_index(&v0, &v3).is_some());
        assert!(complement.edge_index(&v0, &v2).is_some());
    }

    #[test]
    fn neighbors() {
        let mut graph: AdjList<_, _, _, DefaultIndexing> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v2, &v3, ());
        graph.add_edge(&v3, &v1, ());

        let complement = Complement::new(graph, ());

        assert_eq!(
            complement
                .neighbors(&v0)
                .map(|n| NeighborRef::<DefaultIndexing>::index(&n).into_owned())
                .collect::<HashSet<VertexIndex>>(),
            vec![v2, v3].into_iter().collect()
        );
        assert_eq!(complement.neighbors(&v1).count(), 0);
        assert_eq!(
            complement
                .neighbors(&v2)
                .map(|n| NeighborRef::<DefaultIndexing>::index(&n).into_owned())
                .collect::<HashSet<_>>(),
            vec![v0].into_iter().collect()
        );
        assert_eq!(
            complement
                .neighbors(&v3)
                .map(|n| NeighborRef::<DefaultIndexing>::index(&n).into_owned())
                .collect::<HashSet<_>>(),
            vec![v0].into_iter().collect()
        );
    }

    #[test]
    fn apply_holes() {
        let mut graph: Stable<AdjList<_, _, _, DefaultIndexing>> = Stable::new(AdjList::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v2, &v3, ());
        graph.add_edge(&v3, &v4, ());
        graph.add_edge(&v4, &v1, ());

        graph.remove_vertex(&v3);

        let complement: Stable<AdjList<_, _, _, _>> = Complement::new(graph, ()).apply();

        // XXX: Complement does not preserve the vertex indices when there are
        // holes. This would change if we implement an in-place algorithm.
        let v0 = VertexIndex::from(0);
        let v1 = VertexIndex::from(1);
        let v2 = VertexIndex::from(2);
        let v4 = VertexIndex::from(3);

        assert!(complement.edge_index(&v0, &v1).is_none());
        assert!(complement.edge_index(&v1, &v2).is_none());
        assert!(complement.edge_index(&v4, &v1).is_none());

        assert!(complement.edge_index(&v0, &v2).is_some());
        assert!(complement.edge_index(&v0, &v4).is_some());
        assert!(complement.edge_index(&v2, &v4).is_some());
    }
}
