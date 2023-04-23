<div align="center">

# gryf

> Graph data structure library aspiring to be convenient, versatile, correct and performant.

</div>

**Status:** The library is not released on crates.io yet. It is incomplete,
lacks documentation and contains bugs. Breaking changes are expected. Design
contributions are very welcome!

```toml
gryf = { git = "https://github.com/pnevyk/gryf.git" }
```

## Example

```rust
use gryf::algo::ShortestPaths;
use gryf::prelude::*;

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
```

## Overview

The main goal of gryf is to be

* **convenient** -- "make the common case straightforward and natural<sup>1</sup>",
* **versatile** -- "offer simplicity as well as flexibility and strive for a
  good balance if in conflict",
* **correct** -- "use extensive fuzzing and property-based testing to increase
  confidence about correctness", and
* **performant** -- "write the code with performance and memory efficiency in
  mind".

The algorithms are [organized into the
problems](#problems-instead-of-algorithms) they solve. For specifying the
options of an algorithm the [builder pattern](#builder-pattern-for-algorithms)
is utilized. Graphs can use different storage [under the
hood](#separation-of-graph-storage-and-semantics).

<sup>1</sup>Failing in this should be considered a bug and reported.

## Details

_For more details, see the [design doc](./DESIGN.md)._

### Problems instead of algorithms

For users without much experience or knowledge in graph theory and algorithms,
it may not be obvious which algorithm should (or even can) be used to solve the
given problem at hand. Instead, gryf organizes the algorithms into the problem
they solve (e.g., `ShortestPaths`) instead of exposing the algorithms directly
(`dijkstra`, `bellman_ford`).

This brings a number of benefits, among which the most important are:

* It is convenient for the user, especially if they are a beginner. It allows
  them not to care about details if they don't want to care.
* Having a specific type instead of a generic one such as `Vec` or `HashMap`
  gives the opportunity to provide additional functionality (like path
  reconstruction for shortest paths or "is perfect?" query on matching).
* Not specifying the algorithm enables the use of **automatic algorithm
  selection**, which makes the decision based on the properties of the input
  graph.

```rust
let shortest_paths = ShortestPaths::on(&graph).run(rome).unwrap();
```

### Builder pattern for algorithms

Specifying arguments for algorithms is done using the builder pattern. This
avoids the need of passing dummy values (like `None`) to parameters that are not
useful for the use case. On the other hand, it allows tweaking the algorithm
with many optional arguments. Moreover, new optional parameters can be added in
a backward-compatible way. A lot of care is taken to make the error feedback
from the compiler helpful and obvious.

```rust
let shortest_paths = ShortestPaths::on(&graph)
    .edge_weight_fn(|e| e.distance)
    .goal(prague)
    .run(rome)
    .unwrap();
```

### Separation of graph storage and semantics

In gryf, high-level semantics provided by user-facing types are strictly
separated from the underlying storage/representation. The graph data can be
stored in a common representation (e.g., adjacency list or adjacency matrix),
but it can as well be stored in or represented by a custom, problem-tailored
implementation, as long as it implements provided interfaces.

On top of a storage, there is an encapsulation with clear semantics. The most
general is a generic graph, but restricted forms include simple graph (without
parallel edges), path, bipartite graph and so on. Among the advantages of
restrictive encapsulations are:

* The type of graph clearly communicates the intention and structure.
* The API is limited such that it is impossible to violate the rules of the
  user-desired class of graph.
* The guaranteed properties of a restricted graph can be utilized in choosing a
  more efficient algorithm.

```rust
use gryf::storage::AdjMatrix;

let mut graph = Graph::new_undirected_in(AdjMatrix::default());
```

## Alternatives (and inspiration)

* [petgraph](https://crates.io/crates/petgraph)
* [prepona](https://crates.io/crates/prepona)
* [pathfinding](https://crates.io/crates/pathfinding)
* [graph](https://crates.io/crates/graph)
* [graphlib](https://crates.io/crates/graphlib)
* [graphific](https://crates.io/crates/graphific)

See the differences between them and gryf in [this comparison repository](https://github.com/pnevyk/rusty-graphs).

## License

Dual-licensed under [MIT](LICENSE) and [UNLICENSE](UNLICENSE). Feel free to use
it, contribute or spread the word.
