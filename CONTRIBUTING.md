# Contributing guide

As the codebase in this repository is not documented, this short text tries to
summarize main design concepts and should help to understand the code and start
contributing.

This repository is supposed to be used for wild experimentation and as an
incubator for fresh ideas. Breaking things on the way and trying blind paths is
expected, even desirable. Nothing currently implemented is set in stone.

*The main focus is on designing API and coming up with mechanisms to achieve the
following goals, not on implementing algorithms.*

## Balance between expressiveness/flexibility and complexity of the interfaces

There should be a number of core traits that are supposed to be implemented by
graphs if possible. The number and variety of traits in
[petgraph](https://github.com/petgraph/petgraph) is a little bit overwhelming,
but offer great flexibility. The graph implementors can implement exactly the
set of capabilities that their particular graph representation/semantics allow,
while algorithm developers can precisely express the constraints.

Ideally, I would like to have a reasonable number of core traits tha provide
just enough flexibility. There is a trade-off between having a large number of
tiny, highly specialized traits (e.g., `NodeCount` or `NodeIndexable` in
petgraph) or a few coarse-grained traits for which it often happens that one
cannot implement a trait due to not being able to provide an implementation for
one functionality out of many.

Currently, the core traits are `Vertices<V>`, `VerticesMut<V>`, `Edges<E, Ty>`,
`EdgesMut<E, Ty>` and `Neighbors`. There are also "weak" variants `VerticesWeak`
and `EdgesWeak` which relax on some requirements of their non-weak counterparts.
All these traits (among others) reside in [`traits.rs`](src/traits.rs) (this
file should be split into multiple submodules).

An idea is to require the smallest set of functionality that makes sense for the
trait and add default implementations for functions that can be expressed in the
means of other functions. For example, there is a required function
`vertex(index) -> Option<V>` which can be used for default implementation of
`contains_vertex(index) -> bool`. Nevertheless, the implementor can provide a
better (more efficient) implementation if that makes sense for the graph type.

## Graph storage / semantics separation

Inspired by [prepona](https://github.com/maminrayej/prepona), there is
distinction between graph storage (or "representation") and various graph
semantics. The advantage is that we can have implementations of most common
graph representations such as adjacency matrix and adjacency list, and then
provide types that impose additional semantics such as being loop-free, path,
bipartite and so on, while using standard storage under the hood.

The graph storages are in [storage](src/storage) module. There are also storage
adapters such as `Stable` (guaranteeing stable vertex and edge indices even
after remove operations) and `Freeze` (making the storage immutable by *not*
implementing the mutating traits).

The graphs with various semantics are in [graph](src/graph) module. This module
is currently in its beginnings and only `Path` has an existing and decent
implementation. The mutating interfaces for these types will usually be specific
to given semantics, but it would be nice to design them in as unified shape as
possible.

## Problems instead of algorithms

For a user not experiences in graph theory and algorithms it may feel
intimidating and overwhelming to see a list of all algorithms implemented by a
library. It may not be obvious which algorithm can and should be used to solve
the user's problem, and in order to find that they need to consult the
documentation or do their own research. For instance, what are the options for
finding shortest path? And which of them is best suited for the particular use
cases of the user? Are all of them even applicable?

Instead, the algorithms are organized into the problem they solve represented as
a type. These types then provide a single entry point to run an algorithm for
given input. The most suitable algorithm is chosen automatically based on input
properties. For example, for determining the shortest path to a target
Dijkstra's algorithm is preferred, but Bellman-Ford algorithm is used when the
weight can be negative.

The output types of the algorithms are the problem types themselves, not generic
types such as `Vec` or `HashMap`. The main advantages are that changing
internals does not affect the API and the types can offer additional
functionality (e.g., path reconstruction iterator).

It is also possible to use specific algorithms instead of relying on the
automatic selection. This is useful when the user knows exactly what they want
or if an algorithm has looser constraints on input or richer output than the
common entrypoint.

## Statically guaranteed semantics

The goal is to communicate various guarantees on a graph semantics (such as
being loop-free or path). These can be then used in algorithms (e.g., choosing a
more efficient algorithm for bipartite graphs). In order to minimize runtime
performance penalty, these guarantees must be known "statically". The properties
must be either maintained by a graph type by restricting the mutation or
determined dynamically just once when creating the wrapper type.

The current mechanics for providing the guarantees about properties statically
is having traits with associated functions (not taking `self` as parameter) that
return boolean value. A nice thing is that Rust compiler can often optimize out
the tests for these properties (e.g., in automatic algorithm selection) thanks
to monomorphization and dead code elimination.

See `Guarantee` trait or `Weight::is_unsigned` for examples.

## Prefer iteration over recursion

Iterative traversals are preferred over recursive traversals. The main advantage
is that iterative traversals are lazy, thus allowing to stop the traversal
without "hacking" the control flow semantics into callback return type as in
petgraph `depth_first_search`; and can be "detached" from the graph so that the
graph can be manipulated during traversal. Iterative traversal is also not
limited by the size of the program stack. However, in some cases this choice
imposes a efficiency penalty. See implementation of `RawDfsExtra` for the most
notable example.

The current implementation is split into ["raw" base](src/visit/raw.rs) and
high-level [wrappers](src/visit.rs). The raw base provides implementations of
fundamental types of traversal (BFS, a few variations on DFS), whereas the
wrappers use them to offer variety of traversals with different capabilities and
semantics (BFS, DFS, DFS events, DFS without backtracking, post order DFS). Raw
base can also be used to implement specialized traversals in algorithms.

## Graph generalizations

The ultimate goal is to support great variety of graphs and their
generalizations. Examples include hypergraph, mixed (directed & undirected)
graph or implicit/infinite graphs.
