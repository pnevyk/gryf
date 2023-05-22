# Contributing guide

Thank you for your interest in contributing! :tada:

* Are you eager to jump straight in to coding? Have a look at [good first
  issues](https://github.com/pnevyk/gryf/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22)
  or [code map](#code-map) to find something that catches you.
* Did you find a bug or think about an improvement suggestion? Do not hesitate
  to [file an issue](https://github.com/pnevyk/gryf/issues/new).
* Did you fix a bug or improve documentation? Just
  [fork](https://github.com/pnevyk/gryf/fork) the repository and submit a pull
  request.
* Do you intend to add a new feature or change an existing one? Better to
  discuss it in [issues](https://github.com/pnevyk/gryf/issues/new) first.
* Do you have a question? Ask in
  [issues](https://github.com/pnevyk/gryf/issues/new).

:warning: _In the current stage, the main focus is on designing user and
developer APIs. In particular, new algorithms should be added only if they
contribute to or validate API designs._

## Code map

This section describes high-level structure of the library. It is very brief on
purpose and should serve as a quick overview of the most important parts and
their relationships. For more detailed explanation, see [DESIGN](./DESIGN.md)
document.

* `gryf/src/core` -- Mostly core traits and marker types + their implementations. Most important:
    * `gryf/src/core/index.rs` -- Index type (vertex index and edge index) traits and implementations.
    * `gryf/src/core/marker.rs` -- Marker types for directionality of graphs (directed and undirected).
    * `gryf/src/core/{base,vertices,edges,neighbors}.rs` -- Graph access and manipulation traits.
* `gryf/src/storage` -- Implementations of various graph [representations](https://en.wikipedia.org/wiki/Graph_(abstract_data_type)#Common_data_structures_for_graph_representation).
* `gryf/src/visit.rs` -- Graph traversal (BFS, DFS) implementations. Their internals are based on `gryf/src/visit/raw.rs`.
* `gryf/src/algo` -- Implementations of graph algorithms. The structure of an algorithm follows this convention:
    * root -- The main and accompanying types for an algorithmic problem. The main type also represents a successful result of the corresponding algorithms.
    * `builder.rs` -- The algorithm builder that allows specifying parameters as well as fine-tuning the run.
    * `<algorithm>.rs` -- The implementation of an algorithm for given problem. This is private code used by the public API.
* `gryf/src/graph` -- Wrappers for storages that represent different semantics (such as generic graph or [path](https://en.wikipedia.org/wiki/Path_(graph_theory))).
* `gryf/src/adapt` -- Implementations of various adapters and operations on graphs.
* `gryf-derive` -- Derive macros for the fundamental traits.
