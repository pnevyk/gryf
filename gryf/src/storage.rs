pub mod adj_list;
pub mod adj_matrix;
pub mod edge_list;
mod shared;
pub mod stable;

#[doc(hidden)]
pub mod frozen;

pub use adj_list::AdjList;
pub use adj_matrix::AdjMatrix;
pub use edge_list::EdgeList;
pub use stable::Stable;

#[doc(hidden)]
pub use frozen::Frozen;

#[cfg(test)]
mod tests {
    use crate::core::{
        connect::ConnectVertices, create::Create, facts, marker::Direction, props::MultiEdge,
        GraphFull, Neighbors,
    };

    pub fn test_basic<G>()
    where
        G: Create<(), ()> + GraphFull<(), ()> + Neighbors,
    {
        let mut graph = G::empty();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v0, &v2, ());
        let e = graph.add_edge(&v0, &v3, ());
        graph.add_edge(&v2, &v1, ());
        graph.add_edge(&v2, &v3, ());

        graph.remove_edge(&e);
        graph.remove_vertex(&v1);

        assert_eq!(graph.vertex_count(), 3);
        assert_eq!(graph.vertices_by_id().count(), graph.vertex_count());
        assert_eq!(graph.vertices().count(), graph.vertex_count());

        assert_eq!(graph.edge_count(), 2);
        assert_eq!(graph.edges_by_id().count(), graph.edge_count());
        assert_eq!(graph.edges().count(), graph.edge_count());

        let valid_edges_by_id = graph.edges_by_id().all(|edge_id| {
            let (from, to) = graph.endpoints(&edge_id).unwrap();
            graph.edge_id_any(&from, &to) == Some(edge_id)
        });
        assert!(valid_edges_by_id);

        let mut deg = graph
            .vertices_by_id()
            .map(|id| graph.degree_undirected(&id))
            .collect::<Vec<_>>();

        let mut out_deg = graph
            .vertices_by_id()
            .map(|id| graph.degree_directed(&id, Direction::Outgoing))
            .collect::<Vec<_>>();

        let mut in_deg = graph
            .vertices_by_id()
            .map(|id| graph.degree_directed(&id, Direction::Incoming))
            .collect::<Vec<_>>();

        deg.sort_unstable();
        out_deg.sort_unstable();
        in_deg.sort_unstable();

        if graph.is_directed() {
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

        graph.add_edge(&v0, &v2, ());
        assert_eq!(graph.edge_count(), 1);

        graph.clear();
        assert_eq!(graph.vertex_count(), 0);
        assert_eq!(graph.edge_count(), 0);

        let v0 = graph.add_vertex(());
        graph.add_edge(&v0, &v0, ());

        assert_eq!(graph.degree_undirected(&v0), 2);

        if graph.is_directed() {
            assert_eq!(graph.degree_directed(&v0, Direction::Outgoing), 1);
            assert_eq!(graph.degree_directed(&v0, Direction::Incoming), 1);
        } else {
            // In undirected graphs, degree_directed == degree_undirected
            // regardless of the direction.
            assert_eq!(graph.degree_directed(&v0, Direction::Outgoing), 2);
            assert_eq!(graph.degree_directed(&v0, Direction::Incoming), 2);
        }
    }

    pub fn test_multi<G>()
    where
        G: Create<(), i32> + MultiEdge,
    {
        let mut graph = G::empty();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, 0);
        graph.add_edge(&v0, &v2, 1);
        graph.add_edge(&v0, &v1, 2);

        let mut e01 = graph
            .edge_id(&v0, &v1)
            .map(|e| graph.edge(&e))
            .collect::<Vec<_>>();

        e01.sort();

        let e02 = graph
            .edge_id(&v0, &v2)
            .map(|e| graph.edge(&e))
            .collect::<Vec<_>>();

        assert_eq!(e01, vec![Some(&0), Some(&2)]);
        assert_eq!(e02, vec![Some(&1)]);
    }

    pub fn test_connect_vertices<G>()
    where
        G: Create<i32, ()> + ConnectVertices<i32, ()> + GraphFull<i32, ()>,
    {
        let mut graph = G::empty();

        graph.add_vertex(1);
        graph.add_vertex(2);
        graph.add_vertex(3);

        graph.connect_vertices(|_, _| Some(()));

        let n = graph.vertex_count();

        // Number of edges should be complete graph + all self-loops.
        assert_eq!(
            graph.edge_count(),
            facts::complete_graph_edge_count::<G::EdgeType>(n) + n
        );

        graph.clear_edges();

        graph.connect_vertices(|a, b| (a + b == 4 && b >= a).then_some(()));

        // 1 -> 3, 2 -> 2
        assert_eq!(graph.edge_count(), 2);
    }

    pub fn test_neighbors_edge_cases<G>()
    where
        G: Create<(), ()> + Neighbors,
    {
        let mut graph = G::empty();

        let v0 = graph.add_vertex(());

        graph.add_edge(&v0, &v0, ());

        // For undirected graphs, we want to iterate over the self-loop edge
        // only once. But the degree should still be 2. This is the required
        // behavior for neighbors for any storage.
        let n_neighbors = if graph.is_directed() { 2 } else { 1 };
        assert_eq!(graph.neighbors_undirected(&v0).count(), n_neighbors);
        assert_eq!(graph.degree_undirected(&v0), 2);

        if graph.is_directed() {
            assert_eq!(graph.degree_directed(&v0, Direction::Outgoing), 1);
            assert_eq!(graph.degree_directed(&v0, Direction::Incoming), 1);
        }
    }
}
