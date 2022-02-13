# gryf

An *experimental* graph library written in Rust. Requires unstable nightly-only
features and is not released to crates.io yet. **It is incomplete, lacks
documentation and contains bugs. Breaking changes are inevitable.** Design ideas
contributions are very welcome!

## Usage

```toml
gryf = { git = "https://github.com/pnevyk/gryf.git" }
```

## Example

```rust
use gryf::algo::{shortest_paths::identity, ShortestPaths};
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
```

## Key points

*Not everything from the list is implemented*

* Separation of graph data storage (adjacency list, adjacency matrix, ...) from graph semantics (generic graph, simple graph, path, ...)
* Organized into problems, not algorithms (e.g., `ShortestPaths` instead of `dijkstra` and `bellman_ford`)
  * Most suitable algorithm is automatically selected based on graph instance properties
  * Result structures provide high-level interface (e.g., path reconstruction)
* Focus on flexible and expressive, yet not overly complex trait system
  * The core is `VerticesBase`, `Vertices`, `VerticesMut`, `EdgesBase`, `Edges`, `EdgesMut`, `Neighbors`
  * Not important for the user (that much), only for algorithms implementors
* Support for graph generalizations (e.g., hypergraph)
* Accessible codebase without sacrificing performance (too much)

## Alternatives (and inspiration)

* [petgraph](https://github.com/petgraph/petgraph)
* [prepona](https://github.com/maminrayej/prepona)
* [graphlib](https://crates.io/crates/graphlib)
* [graphific](https://crates.io/crates/graphific)
* [pathfinding](https://crates.io/crates/pathfinding)

## Name

*Gryf* /ɡrɨf/ is the czech word for
[griffin](https://en.wikipedia.org/wiki/Griffin), a legendary creature from
medieval times. And edit-distance-wise, it is not far from "graf" which is the
czech for "graph".

## License

Dual-licensed under [MIT](LICENSE) and [UNLICENSE](UNLICENSE). Feel free to use
it, contribute or spread the word.
