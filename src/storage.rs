pub mod adj_list;
pub mod adj_matrix;
pub mod edge_list;
pub mod frozen;
pub mod reversed;
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

    pub fn test_basic<Ty: EdgeType, G>()
    where
        G: Create<(), (), Ty> + Neighbors,
    {
        let mut graph = G::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v0, v2, ());
        let e = graph.add_edge(v0, v3, ());
        graph.add_edge(v2, v1, ());
        graph.add_edge(v2, v3, ());

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

        graph.clear_edges();
        assert_eq!(graph.edge_count(), 0);
        assert_eq!(graph.vertex_count(), 3);

        graph.add_edge(v0, v1, ());
        assert_eq!(graph.edge_count(), 1);

        graph.clear();
        assert_eq!(graph.vertex_count(), 0);
        assert_eq!(graph.edge_count(), 0);
    }

    pub fn test_multi<Ty: EdgeType, G>()
    where
        G: Create<(), i32, Ty> + MultiEdges<i32, Ty>,
    {
        let mut graph = G::default();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(v0, v1, 0);
        graph.add_edge(v0, v2, 1);
        graph.add_edge(v0, v1, 2);

        let mut e01 = graph
            .multi_edge_index(v0, v1)
            .map(|e| graph.edge(e))
            .collect::<Vec<_>>();

        e01.sort();

        let e02 = graph
            .multi_edge_index(v0, v2)
            .map(|e| graph.edge(e))
            .collect::<Vec<_>>();

        assert_eq!(e01, vec![Some(&0), Some(&2)]);
        assert_eq!(e02, vec![Some(&1)]);
    }
}
