use gryf::prelude::*;
use gryf::storage::AdjMatrix;

fn main() {
    let mut graph = Graph::new_undirected_in(AdjMatrix::default());

    let u = graph.add_vertex("u");
    let v = graph.add_vertex("v");
    let w = graph.add_vertex("w");

    graph.add_edge(u, v, 1);
    graph.add_edge(v, w, 2);
    graph.add_edge(w, u, 3);
}
