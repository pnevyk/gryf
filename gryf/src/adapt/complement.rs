use rustc_hash::FxHashSet;

use crate::core::{
    facts,
    id::{IdType, IntegerIdType},
    marker::{Direction, Undirected},
    EdgeSet, GraphFull, Neighbors, WeakRef,
};

use gryf_derive::{GraphBase, VertexSet};

// TODO: Remove these imports once hygiene of procedural macros is fixed.
use crate::common::CompactIdMap;
use crate::core::{GraphBase, NeighborRef, VertexSet};

#[derive(Debug, GraphBase, VertexSet)]
pub struct Complement<E, G> {
    #[graph]
    graph: G,
    edge: E,
}

impl<E, G> Complement<E, G>
where
    G: GraphBase<EdgeType = Undirected> + VertexSet + EdgeSet,
{
    pub fn new(graph: G, edge: E) -> Self {
        Self { graph, edge }
    }

    pub fn into_inner(self) -> G {
        self.graph
    }

    pub fn edge_count(&self) -> usize {
        facts::complete_graph_edge_count::<Undirected>(self.graph.vertex_count())
            - self.graph.edge_count()
    }

    pub fn apply<V>(self) -> G
    where
        G: GraphFull<V, E>,
        E: Clone,
    {
        let mut graph = self.graph;
        let original_edges = graph
            .edge_ids()
            .map(|e| graph.endpoints(&e).unwrap())
            .collect::<FxHashSet<_>>();
        let vertices = graph.vertex_ids().collect::<Vec<_>>();

        graph.clear_edges();

        for (i, u) in vertices.iter().enumerate() {
            for v in vertices.iter().skip(i) {
                if !original_edges.contains(&(u.clone(), v.clone()))
                    && !original_edges.contains(&(v.clone(), u.clone()))
                {
                    graph.add_edge(u, v, self.edge.clone());
                }
            }
        }

        graph
    }
}

impl<E, G> Neighbors for Complement<E, G>
where
    G: Neighbors + VertexSet,
    G::EdgeId: IntegerIdType,
{
    type NeighborRef<'a> = (G::VertexId, G::EdgeId, G::VertexId, Direction)
    where
        Self: 'a;

    type NeighborsIter<'a> = NeighborsIter<'a, G>
    where
        Self: 'a;

    fn neighbors(&self, src: &G::VertexId) -> Self::NeighborsIter<'_> {
        NeighborsIter {
            src: src.clone(),
            dir: Direction::Outgoing,
            neighbors: self
                .graph
                .neighbors(src)
                .map(|n| n.id().into_owned().into())
                .collect(),
            vertices: self.graph.vertex_ids(),
        }
    }

    fn neighbors_directed(&self, src: &G::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
        NeighborsIter {
            src: src.clone(),
            dir,
            neighbors: self
                .graph
                .neighbors_directed(src, dir)
                .map(|n| n.id().into_owned().into())
                .collect(),
            vertices: self.graph.vertex_ids(),
        }
    }
}

pub struct NeighborsIter<'a, G>
where
    G: VertexSet + 'a,
{
    src: G::VertexId,
    dir: Direction,
    neighbors: FxHashSet<WeakRef<'a, G::VertexId>>,
    vertices: G::VertexIdsIter<'a>,
}

impl<'a, G> Iterator for NeighborsIter<'a, G>
where
    G: VertexSet + 'a,
    G::EdgeId: IntegerIdType,
{
    type Item = (G::VertexId, G::EdgeId, G::VertexId, Direction);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let v = self.vertices.next()?;

            if v != self.src && !self.neighbors.contains(&v) {
                return Some((v, IdType::sentinel(), self.src.clone(), self.dir));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        core::{
            id::{DefaultId, VertexId},
            GraphAdd,
        },
        storage::{AdjList, Stable},
    };

    use super::*;

    use std::collections::HashSet;

    #[test]
    fn edge_count() {
        let mut graph: AdjList<_, _, _, DefaultId> = AdjList::new();

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
        let mut graph: AdjList<_, _, _, DefaultId> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v2, &v3, ());
        graph.add_edge(&v3, &v1, ());

        let complement: AdjList<_, _, _, _> = Complement::new(graph, ()).apply();

        assert!(complement.edge_id_any(&v0, &v1).is_none());
        assert!(complement.edge_id_any(&v1, &v2).is_none());
        assert!(complement.edge_id_any(&v2, &v3).is_none());
        assert!(complement.edge_id_any(&v3, &v1).is_none());

        assert!(complement.edge_id_any(&v0, &v3).is_some());
        assert!(complement.edge_id_any(&v0, &v2).is_some());
    }

    #[test]
    fn neighbors() {
        let mut graph: AdjList<_, _, _, DefaultId> = AdjList::new();

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
                .map(|n| n.id().into_owned())
                .collect::<HashSet<VertexId>>(),
            vec![v2, v3].into_iter().collect()
        );
        assert_eq!(complement.neighbors(&v1).count(), 0);
        assert_eq!(
            complement
                .neighbors(&v2)
                .map(|n| n.id().into_owned())
                .collect::<HashSet<_>>(),
            vec![v0].into_iter().collect()
        );
        assert_eq!(
            complement
                .neighbors(&v3)
                .map(|n| n.id().into_owned())
                .collect::<HashSet<_>>(),
            vec![v0].into_iter().collect()
        );
    }

    #[test]
    fn apply_holes() {
        let mut graph: Stable<AdjList<_, _, _, DefaultId>> = Stable::new(AdjList::new());

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

        assert!(complement.edge_id_any(&v0, &v1).is_none());
        assert!(complement.edge_id_any(&v1, &v2).is_none());
        assert!(complement.edge_id_any(&v4, &v1).is_none());

        assert!(complement.edge_id_any(&v0, &v2).is_some());
        assert!(complement.edge_id_any(&v0, &v4).is_some());
        assert!(complement.edge_id_any(&v2, &v4).is_some());
    }
}
