use gryf::{algo::ShortestPaths, domain::Graph};

fn main() {
    // Default storage is adjacency list, but that can be simply changed by
    // using `Graph::new_undirected_in`.
    let mut graph = Graph::new_undirected();

    let prague = graph.add_vertex("Prague");
    let bratislava = graph.add_vertex("Bratislava");
    let vienna = graph.add_vertex("Vienna");
    let munich = graph.add_vertex("Munich");
    let nuremberg = graph.add_vertex("Nuremberg");
    let florence = graph.add_vertex("Florence");
    let rome = graph.add_vertex("Rome");

    graph.extend_with_edges([
        (prague, bratislava, 328u32),
        (prague, nuremberg, 297),
        (prague, vienna, 293),
        (bratislava, vienna, 79),
        (nuremberg, munich, 170),
        (vienna, munich, 402),
        (vienna, florence, 863),
        (munich, florence, 646),
        (florence, rome, 278),
    ]);

    // As the edge weights are unsigned and there is a specific goal, Dijktra's
    // algorithm is applied. For signed edges, Bellman-Ford would be used.
    let shortest_paths = ShortestPaths::on(&graph).goal(prague).run(rome).unwrap();
    let distance = shortest_paths[prague];
    let path = shortest_paths
        .reconstruct(prague)
        .map(|v| graph[v])
        .collect::<Vec<_>>()
        .join(" - ");

    println!("{distance} km from Prague through {path}");
    // 1391 km from Prague through Nuremberg - Munich - Florence - Rome
}
