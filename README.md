<div align="center">

<img src="https://raw.githubusercontent.com/pnevyk/gryf/main/assets/logo-gryf-text.png" width="400px" />

> Graph data structure library aspiring to be convenient, versatile, correct and performant.

[![Cargo](https://img.shields.io/crates/v/gryf.svg)](https://crates.io/crates/gryf)
[![License](https://img.shields.io/badge/license-MIT_OR_Unlicense-blue.svg)](https://github.com/pnevyk/gryf)
[![Lint and test](https://github.com/pnevyk/gryf/actions/workflows/main.yml/badge.svg)](https://github.com/pnevyk/gryf/actions/workflows/main.yml)
[![Documentation](https://docs.rs/gryf/badge.svg)](https://docs.rs/gryf)

</div>

**Status:** This library is experimental. It is incomplete and contains bugs.
Breaking changes are expected. Design contributions are very welcome!

## Example

```rust
use gryf::{algo::ShortestPaths, Graph};

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
```

##  Goals

The main goals of gryf are to be

* _convenient_, that is, "making the common case straightforward and natural",
* _versatile_, that is, "offering simplicity as well as flexibility and striving
  for a good balance if in conflict",
* _correct_, that is, "using extensive fuzzing and property-based testing to
  increase confidence about correctness", and
* _performant_, that is, "writing the code with performance and memory
  efficiency in mind".

Failing in any of these should be considered an issue to be reported.

## Design

_For more details, see the [design doc](./DESIGN.md)._

### Problems instead of algorithms

It may not be obvious which algorithm should (or even can) be used to solve the
given problem at hand, especially for users without much experience or knowledge
in graph theory and algorithms. Instead, gryf organizes the algorithms into the
problem they solve (e.g., `ShortestPaths`) instead of requiring to call the
algorithms directly (`dijkstra`, `bellman_ford`).

Organizing algorithms into problems brings a number of benefits, among which the
most important are:

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
avoids the need to pass dummy values (like `None`) to parameters that are not
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

High-level semantics provided by user-facing types are strictly separated from
the underlying storage/representation. The graph data can be stored in a common
representation (e.g., adjacency list or adjacency matrix), but it can also be
stored in or represented by a custom, problem-tailored implementation, as long
as it implements provided interfaces.

On top of storage, there is an encapsulation with clear semantics. The most
general is a generic graph, but restricted forms include simple graphs (without
parallel edges), paths, bipartite graphs and so on. Among the advantages of
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

Check the [rusty graphs](https://github.com/pnevyk/rusty-graphs) repository for
a detailed comparison of gryf and other graph libraries available for Rust with
examples and commentary.

## License

Dual-licensed under [MIT](LICENSE) and [UNLICENSE](UNLICENSE). Feel free to use
it, contribute or spread the word.
