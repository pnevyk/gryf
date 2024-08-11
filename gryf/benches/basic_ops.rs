mod common;

use std::collections::BTreeSet;

use common::{RandomEdges, RANDOM_SEED};
use fastrand::Rng;
use petgraph::prelude::*;

fn main() {
    divan::main();
}

#[divan::bench(consts = [100, 1000], types = [gryf::core::marker::Directed, gryf::core::marker::Undirected], args = [0.05, 0.5, 0.95])]
fn gryf_adj_list_add_remove<const N: usize, Ty: gryf::core::marker::EdgeType>(density: f32) {
    let mut rng = Rng::with_seed(RANDOM_SEED);

    let mut graph = gryf::graph::Graph::<_, _, Ty>::new();

    for _ in 0..N {
        graph.add_vertex(rng.u32(0..100));
    }

    let mut edges = RandomEdges::new(N, density);

    while let Some((u, v)) = edges.next_edge(&mut rng) {
        graph.add_edge(u, v, rng.f32());
    }

    for _ in 0..(N / 4) {
        let u = rng.usize(..graph.vertex_count());

        graph.remove_vertex(u);
    }

    let mut edges = RandomEdges::new(N / 4, density);

    while let Some((u, v)) = edges.next_edge(&mut rng) {
        if let Some(e) = graph.edge_id_any(u, v) {
            graph.remove_edge(e);
        }
    }

    let mut edges = RandomEdges::new(N - (N / 4), density);

    while let Some((u, v)) = edges.next_edge(&mut rng) {
        graph.add_edge(u, v, rng.f32());
    }
}

#[divan::bench(consts = [100, 1000], types = [gryf::core::marker::Directed, gryf::core::marker::Undirected], args = [0.05, 0.5, 0.95])]
fn gryf_adj_matrix_add_remove<const N: usize, Ty: gryf::core::marker::EdgeType>(density: f32) {
    let mut rng = Rng::with_seed(RANDOM_SEED);

    let mut graph =
        gryf::graph::Graph::<_, _, Ty, _>::new_in(
            gryf::storage::AdjMatrix::<_, _, Ty, _>::default(),
        );

    for _ in 0..N {
        graph.add_vertex(rng.u32(0..100));
    }

    let mut edges = RandomEdges::new(N, density);

    while let Some((u, v)) = edges.next_edge(&mut rng) {
        if !graph.contains_edge_between(u, v) {
            graph.add_edge(u, v, rng.f32());
        }
    }

    for _ in 0..(N / 4) {
        let u = rng.usize(..graph.vertex_count());

        graph.remove_vertex(u);
    }

    let mut edges = RandomEdges::new(N / 4, density);

    while let Some((u, v)) = edges.next_edge(&mut rng) {
        if let Some(e) = graph.edge_id_any(u, v) {
            graph.remove_edge(e);
        }
    }

    let mut edges = RandomEdges::new(N - (N / 4), density);

    while let Some((u, v)) = edges.next_edge(&mut rng) {
        if !graph.contains_edge_between(u, v) {
            graph.add_edge(u, v, rng.f32());
        }
    }
}

#[divan::bench(consts = [100, 1000], types = [petgraph::Directed, petgraph::Undirected], args = [0.05, 0.5, 0.95])]
fn petgraph_graph_add_remove<const N: usize, Ty: petgraph::EdgeType>(density: f32) {
    let mut rng = Rng::with_seed(RANDOM_SEED);

    let mut graph = petgraph::Graph::<_, _, Ty>::with_capacity(0, 0);

    for _ in 0..N {
        graph.add_node(rng.u32(0..100));
    }

    let mut edges = RandomEdges::new(N, density);

    while let Some((u, v)) = edges.next_edge(&mut rng) {
        graph.add_edge(NodeIndex::new(u), NodeIndex::new(v), rng.f32());
    }

    for _ in 0..(N / 4) {
        let u = rng.usize(..graph.node_count());

        graph.remove_node(NodeIndex::new(u));
    }

    let mut edges = RandomEdges::new(N / 4, density);

    while let Some((u, v)) = edges.next_edge(&mut rng) {
        if let Some(e) = graph
            .edges_connecting(NodeIndex::new(u), NodeIndex::new(v))
            .next()
        {
            graph.remove_edge(e.id());
        }
    }

    let mut edges = RandomEdges::new(N - (N / 4), density);

    while let Some((u, v)) = edges.next_edge(&mut rng) {
        graph.add_edge(NodeIndex::new(u), NodeIndex::new(v), rng.f32());
    }
}

#[divan::bench(consts = [100, 1000], types = [petgraph::Directed, petgraph::Undirected], args = [0.05, 0.5, 0.95])]
fn petgraph_matrix_graph_add_remove<const N: usize, Ty: petgraph::EdgeType>(density: f32) {
    let mut rng = Rng::with_seed(RANDOM_SEED);

    let mut graph = petgraph::matrix_graph::MatrixGraph::<_, _, Ty>::with_capacity(0);

    for _ in 0..N {
        graph.add_node(rng.u32(0..100));
    }

    let mut edges = RandomEdges::new(N, density);

    while let Some((u, v)) = edges.next_edge(&mut rng) {
        if !graph.has_edge(NodeIndex::new(u), NodeIndex::new(v)) {
            graph.add_edge(NodeIndex::new(u), NodeIndex::new(v), rng.f32());
        }
    }

    // petgraph::MatrixGraph doesn't swap-remove matrix row and columns for
    // removed nodes and panics if one tries to remove a node that does not
    // exist anymore. Thus we need to keep collection of removed nodes and avoid
    // removing non-existing nodes. Note that this difference in behavior makes
    // this benchmark not directly comparable with others.
    let mut removed_nodes = BTreeSet::new();

    for _ in 0..(N / 4) {
        let u = rng.usize(..graph.node_count());

        if !removed_nodes.contains(&u) {
            graph.remove_node(NodeIndex::new(u));
            removed_nodes.insert(u);
        }
    }

    let mut edges = RandomEdges::new(N / 4, density);

    while let Some((u, v)) = edges.next_edge(&mut rng) {
        if graph.has_edge(NodeIndex::new(u), NodeIndex::new(v)) {
            graph.remove_edge(NodeIndex::new(u), NodeIndex::new(v));
        }
    }

    let mut edges = RandomEdges::new(N - (N / 4), density);

    while let Some((u, v)) = edges.next_edge(&mut rng) {
        if !graph.has_edge(NodeIndex::new(u), NodeIndex::new(v)) {
            graph.add_edge(NodeIndex::new(u), NodeIndex::new(v), rng.f32());
        }
    }
}
