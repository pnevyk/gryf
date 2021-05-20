pub mod adj_list;
pub mod adj_matrix;
pub mod edge_list;
pub mod frozen;
mod shared;
pub mod stable;

pub use adj_list::AdjList;
pub use adj_matrix::AdjMatrix;
pub use edge_list::EdgeList;
pub use frozen::Frozen;
pub use stable::Stable;

#[cfg(test)]
mod tests {
    use crate::marker::{Direction, EdgeType};
    use crate::traits::*;

    pub fn test_basic<V, E, Ty: EdgeType, G>()
    where
        V: Default,
        E: Default,
        G: Create<V, E, Ty> + Neighbors,
    {
        let mut graph = G::default();

        let v0 = graph.add_vertex(V::default());
        let v1 = graph.add_vertex(V::default());
        let v2 = graph.add_vertex(V::default());
        let v3 = graph.add_vertex(V::default());

        graph.add_edge(v0, v1, E::default());
        graph.add_edge(v0, v2, E::default());
        let e = graph.add_edge(v0, v3, E::default());
        graph.add_edge(v2, v1, E::default());
        graph.add_edge(v2, v3, E::default());

        graph.remove_edge(e);
        graph.remove_vertex(v1);

        assert_eq!(graph.vertex_count(), 3);
        assert_eq!(graph.vertex_indices().count(), graph.vertex_count());
        assert_eq!(graph.vertices().count(), graph.vertex_count());

        assert_eq!(graph.edge_count(), 2);
        assert_eq!(graph.edge_indices().count(), graph.edge_count());
        assert_eq!(graph.edges().count(), graph.edge_count());

        let valid_edge_indices = graph.edge_indices().all(|edge_index| {
            let (src, dst) = graph.endpoints(edge_index).unwrap();
            graph.edge_index(src, dst) == Some(edge_index)
        });
        assert!(valid_edge_indices);

        let mut deg = graph
            .vertex_indices()
            .map(|index| graph.degree(index))
            .collect::<Vec<_>>();

        let mut out_deg = graph
            .vertex_indices()
            .map(|index| graph.degree_directed(index, Direction::Outgoing))
            .collect::<Vec<_>>();

        let mut in_deg = graph
            .vertex_indices()
            .map(|index| graph.degree_directed(index, Direction::Incoming))
            .collect::<Vec<_>>();

        deg.sort();
        out_deg.sort();
        in_deg.sort();

        if Ty::is_directed() {
            assert_eq!(deg, vec![1, 1, 2]);
            assert_eq!(out_deg, vec![0, 1, 1]);
            assert_eq!(in_deg, vec![0, 1, 1]);
        } else {
            assert_eq!(deg, vec![1, 1, 2]);
            assert_eq!(out_deg, vec![1, 1, 2]);
            assert_eq!(in_deg, vec![1, 1, 2]);
        }
    }
}
