use gryf::algo::ShortestPaths;
use gryf::prelude::*;

fn main() {
    // Default storage is adjacency list, but that can be simply changed by
    // using `Graph::with_storage`.
    let mut graph = Graph::new_undirected();

    let prague = graph.add_vertex("Prague");
    let bratislava = graph.add_vertex("Bratislava");
    let vienna = graph.add_vertex("Vienna");
    let munich = graph.add_vertex("Munich");
    let nuremberg = graph.add_vertex("Nuremberg");
    let florence = graph.add_vertex("Florence");
    let rome = graph.add_vertex("Rome");

    graph.add_edge(prague, bratislava, 328u32);
    graph.add_edge(prague, nuremberg, 293);
    graph.add_edge(bratislava, vienna, 79);
    graph.add_edge(nuremberg, munich, 170);
    graph.add_edge(vienna, munich, 402);
    graph.add_edge(munich, florence, 646);
    graph.add_edge(florence, rome, 278);

    // As the edge weights are unsigned and there is a specific goal, Dijktra's
    // algorithm is applied. For signed edges, Bellman-Ford would be used.
    let shortest_paths = ShortestPaths::on(&graph).goal(prague).run(rome).unwrap();
    let distance = shortest_paths[prague];
    let path = shortest_paths
        .reconstruct(prague)
        .map(|v| *graph.vertex(v).unwrap())
        .collect::<Vec<_>>()
        .join(" - ");

    println!("{distance} km from Prague through {path}");
    // 1387 km from Prague through Nuremberg - Munich - Florence - Rome
}
