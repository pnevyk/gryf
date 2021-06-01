use gryf::algo::{shortest_paths::identity, ShortestPaths};
use gryf::prelude::*;

fn main() {
    let mut graph: AdjList<&str, u32, Undirected> = AdjList::new();

    let prague = graph.add_vertex("Prague");
    let bratislava = graph.add_vertex("Bratislava");
    let vienna = graph.add_vertex("Vienna");
    let munich = graph.add_vertex("Munich");
    let nuremberg = graph.add_vertex("Nuremberg");
    let florence = graph.add_vertex("Florence");
    let rome = graph.add_vertex("Rome");

    graph.add_edge(prague, bratislava, 328);
    graph.add_edge(prague, nuremberg, 293);
    graph.add_edge(bratislava, vienna, 79);
    graph.add_edge(nuremberg, munich, 170);
    graph.add_edge(vienna, munich, 402);
    graph.add_edge(munich, florence, 646);
    graph.add_edge(florence, rome, 278);

    // As the edge weights are unsigned and there is a specific goal, Dijktra's
    // algorithm is applied. For signed edges, Bellman-Ford would be used.
    let shortest_paths = ShortestPaths::run(&graph, rome, Some(prague), identity).unwrap();
    let distance = shortest_paths.dist(prague).unwrap();
    let path = shortest_paths
        .reconstruct(prague)
        .map(|v| *graph.vertex(v).unwrap())
        .collect::<Vec<_>>()
        .join(" - ");

    println!("{} km from Prague through {}", distance, path);
    // 1387 km from Prague through Nuremberg - Munich - Florence - Rome
}
