//! Gryf is a [graph](domain) data structure library aspiring to be convenient,
//! versatile, correct and performant.
//!
//! A graph is made up of _vertices_ (also called nodes) which are connected by
//! _edges_. Both vertices and edges might have _attributes_. Graphs can be used
//! to model pairwise relations between objects and have many [applications] in
//! various areas like _computer science_ (packet routing in the internet),
//! _transportation_ (navigation in a city), _linguistics_ (lexical semantics
//! relationships), _physics and chemistry_ (processing of molecular structures)
//! or _social sciences_ (social network analysis), among others.
//!
//! Gryf implements various [storages](storage) to hold the graph data and
//! structure and [encapsulations](domain) that guarantee specific semantics.
//! Then it provides common [graph traversal](visit) methods and a collection of
//! [algorithms](algo) on graphs. The algorithms are [organized into the
//! problems](#problems-instead-of-algorithms) they solve. For specifying the
//! parameters of an algorithm the [builder
//! pattern](#builder-pattern-for-algorithms) is utilized.
//!
//! <style>svg { width: 100%; height: 100%; }</style>
#![doc = include_str!("../docs/assets/main_example.excalidraw.svg")]
//!
//! ```
//! use gryf::{algo::ShortestPaths, Graph};
//!
//! // Default storage is adjacency list, but that can be simply changed by
//! // using `Graph::new_undirected_in`.
//! let mut graph = Graph::new_undirected();
//!
//! let prague = graph.add_vertex("Prague");
//! let bratislava = graph.add_vertex("Bratislava");
//! let vienna = graph.add_vertex("Vienna");
//! let munich = graph.add_vertex("Munich");
//! let nuremberg = graph.add_vertex("Nuremberg");
//! let florence = graph.add_vertex("Florence");
//! let rome = graph.add_vertex("Rome");
//!
//! graph.extend_with_edges([
//!     (prague, bratislava, 328u32),
//!     (prague, nuremberg, 297),
//!     (prague, vienna, 293),
//!     (bratislava, vienna, 79),
//!     (nuremberg, munich, 170),
//!     (vienna, munich, 402),
//!     (vienna, florence, 863),
//!     (munich, florence, 646),
//!     (florence, rome, 278),
//! ]);
//!
//! // As the edge weights are unsigned and there is a specific goal, Dijktra's
//! // algorithm is applied. For signed edges, Bellman-Ford would be used.
//! let shortest_paths = ShortestPaths::on(&graph).goal(prague).run(rome).unwrap();
//! let distance = shortest_paths[prague];
//! let path = shortest_paths
//!     .reconstruct(prague)
//!     .map(|v| graph[v])
//!     .collect::<Vec<_>>()
//!     .join(" - ");
//!
//! println!("{distance} km from Prague through {path}");
//! // 1391 km from Prague through Nuremberg - Munich - Florence - Rome
//! ```
//!
//! [graph]: https://en.wikipedia.org/wiki/Graph_theory
//! [applications]: https://en.wikipedia.org/wiki/Graph_theory#Applications
//!
//! # Common operations
//!
//! See the [core] module documentation.
//!
//! # Goals
//!
//! The main goals of gryf are to be
//!
//! * _convenient_, that is, "making the common case straightforward and
//!   natural",
//! * _versatile_, that is, "offering simplicity as well as flexibility and
//!   striving for a good balance if in conflict",
//! * _correct_, that is, "using extensive fuzzing and property-based testing to
//!   increase confidence about correctness", and
//! * _performant_, that is, "writing the code with performance and memory
//!   efficiency in mind".
//!
//! Failing in any of these should be considered an issue to be reported.
//!
//! # Design
//!
//! _For more details, see the [design document]_.
//!
//! ## Problems instead of algorithms
//!
//! It may not be obvious which algorithm should (or even can) be used to solve
//! the given problem at hand, especially for users without much experience or
//! knowledge in graph theory and algorithms. Instead, gryf organizes the
//! algorithms into the problem they solve (e.g.,
//! [`ShortestPaths`](algo::ShortestPaths)) instead of requiring to call the
//! algorithms directly (`dijkstra`, `bellman_ford`).
//!
//! Organizing algorithms into problems brings a number of benefits, among which
//! the most important are:
//!
//! * It is convenient for the user, especially if they are a beginner. It
//!   allows them not to care about details if they don't want to care.
//! * Having a specific type instead of a generic one such as `Vec` or `HashMap`
//!   gives the opportunity to provide additional functionality (like path
//!   reconstruction for shortest paths or "is perfect?" query on matching).
//! * Not specifying the algorithm enables the use of automatic algorithm
//!   selection, which makes the decision based on the properties of the input
//!   graph.
//!
//! ```
//! # use gryf::{algo::ShortestPaths, Graph};
//! # let mut graph = Graph::new_undirected();
//! # let prague = graph.add_vertex("Prague");
//! # let rome = graph.add_vertex("Rome");
//! # graph.add_edge(prague, rome, 0);
//! let shortest_paths = ShortestPaths::on(&graph).run(rome).unwrap();
//! ```
//!
//! ## Builder pattern for algorithms
//!
//! Specifying arguments for algorithms is done using the builder pattern. This
//! avoids the need to pass dummy values (like `None`) to parameters that are
//! not useful for the use case. On the other hand, it allows tweaking the
//! algorithm with many optional arguments. Moreover, new optional parameters
//! can be added in a backward-compatible way. A lot of care is taken to make
//! the error feedback from the compiler helpful and obvious.
//!
//! ```
//! # use gryf::{algo::ShortestPaths, Graph};
//! # struct Edge { distance: u32 }
//! # let mut graph = Graph::new_undirected();
//! # let prague = graph.add_vertex("Prague");
//! # let rome = graph.add_vertex("Rome");
//! # graph.add_edge(prague, rome, Edge { distance: 0 });
//! let shortest_paths = ShortestPaths::on(&graph)
//!     .edge_weight_fn(|e| e.distance)
//!     .goal(prague)
//!     .run(rome)
//!     .unwrap();
//! ```
//!
//! ## Separation of graph storage and semantics
//!
//! High-level semantics provided by user-facing types are strictly separated
//! from the underlying storage/representation. The graph data can be stored in
//! a common representation (e.g., [adjacency list](storage::adj_list) or
//! [adjacency matrix](storage::adj_matrix)), but it can also be stored in or
//! represented by a custom, problem-tailored implementation, as long as it
//! implements provided interfaces.
//!
//! On top of storage, there is an encapsulation with clear semantics. The most
//! general is a generic graph, but restricted forms include simple graphs
//! (without parallel edges), paths, bipartite graphs and so on. Among the
//! advantages of restrictive encapsulations are:
//!
//! * The type of graph clearly communicates the intention and structure.
//! * The API is limited such that it is impossible to violate the rules of the
//!   user-desired class of graph.
//! * The guaranteed properties of a restricted graph can be utilized in
//!   choosing a more efficient algorithm.
//!
//! ```
//! # use gryf::Graph;
//! use gryf::storage::AdjMatrix;
//!
//! let mut graph = Graph::new_undirected_in(AdjMatrix::default());
//! # let a = graph.add_vertex("a");
//! # let b = graph.add_vertex("b");
//! # graph.add_edge(a, b, ());
//! ```
//!
//! ## Graph interfaces and generic algorithms
//!
//! There is a set of [core traits](crate::core) that represent different levels
//! of functionality that is supported by a graph representation. The levels
//! range from basic properties like directionality of the graph over structural
//! properties like vertex/edge count and vertex neighbors to different kinds of
//! manipulation capability (readonly, append-only, removable). The traits
//! require only a bare minimum of methods and provide default, overridable
//! implementations (even if inefficient) for the remaining API to reduce the
//! implementation burden.
//!
//! Algorithms then constrain the input graph only with traits that are
//! necessary for its function and don't make any assumption on the specific
//! graph representation used.
//!
//! ## Iteration over recursion
//!
//! Iterative graph traversals are preferred over recursion. The main benefits
//! of this choice are:
//!
//! * Traversal is lazy and can be stopped without tricks.
//! * Traversal state is independent on the graph itself, allowing mutations
//!   during traversal.
//! * Traversal is not limited by the size of the program stack.
//!
//! ```
//! use gryf::visit::{DfsEvent, DfsEvents, Visitor};
//! # use gryf::Graph;
//! # let mut graph = Graph::new_directed();
//! # let root = graph.add_vertex("root");
//! # let v = graph.add_vertex("v");
//! # graph.add_edge(root, v, 0);
//!
//! let is_cyclic = DfsEvents::new(&graph)
//!     .start(root)
//!     .into_iter(&graph)
//!     .any(|event| matches!(event, DfsEvent::BackEdge { .. }));
//! ```
//!
//! [design document]: https://github.com/pnevyk/gryf/blob/main/DESIGN.md
//!
//! # Comparison with alternatives
//!
//! Check the [rusty graphs](https://github.com/pnevyk/rusty-graphs) repository
//! for a detailed comparison of gryf and other graph libraries available for
//! Rust with examples and commentary.

pub mod adapt;
pub mod algo;
pub mod core;
pub mod domain;
pub mod infra;
pub mod storage;
pub mod visit;

pub use domain::Graph;

#[cfg(feature = "derive")]
pub mod derive {
    pub use gryf_derive::*;
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use gryf_derive::*;

    use crate::{
        core::{
            EdgeSet, GraphAdd, GraphBase, GraphFull, GraphMut, GraphRef, GraphWeak, Neighbors,
            VertexSet,
            id::DefaultId,
            marker::Directed,
            props::{Guarantee, MultiEdge},
        },
        storage::AdjList,
    };

    // Test hygiene of the custom derive macros.
    #[derive(
        Clone,
        GraphBase,
        Neighbors,
        VertexSet,
        EdgeSet,
        GraphRef,
        GraphMut,
        GraphAdd,
        GraphFull,
        MultiEdge,
        Guarantee,
    )]
    #[gryf_crate]
    struct TestWrapper {
        #[graph]
        graph: AdjList<(), (), Directed, DefaultId>,
    }

    fn require_graph_base(_: impl GraphBase) {}
    fn require_neighbors(_: impl Neighbors) {}
    fn require_vertex_set(_: impl VertexSet) {}
    fn require_edge_set(_: impl EdgeSet) {}
    fn require_graph_ref(_: impl GraphRef<(), ()>) {}
    fn require_graph_weak(_: impl GraphWeak<(), ()>) {}
    fn require_graph_mut(_: impl GraphMut<(), ()>) {}
    fn require_graph_add(_: impl GraphAdd<(), ()>) {}
    fn require_graph_full(_: impl GraphFull<(), ()>) {}
    fn require_multi_edge(_: impl MultiEdge) {}
    fn require_guarantee(_: impl Guarantee) {}

    #[test]
    fn trait_impl() {
        let mut g = TestWrapper {
            graph: AdjList::default(),
        };

        require_graph_base(g.clone());
        require_graph_base(&g);
        require_graph_base(&mut g);

        require_neighbors(g.clone());
        require_neighbors(&g);
        require_neighbors(&mut g);

        require_vertex_set(g.clone());
        require_vertex_set(&g);
        require_vertex_set(&mut g);

        require_edge_set(g.clone());
        require_edge_set(&g);
        require_edge_set(&mut g);

        require_graph_ref(g.clone());
        require_graph_ref(&g);
        require_graph_ref(&mut g);

        require_graph_weak(g.clone());
        require_graph_weak(&g);
        require_graph_weak(&mut g);

        require_graph_mut(g.clone());
        require_graph_mut(&mut g);

        require_graph_add(g.clone());
        require_graph_add(&mut g);

        require_graph_full(g.clone());
        require_graph_full(&mut g);

        require_multi_edge(g.clone());
        require_multi_edge(&g);
        require_multi_edge(&mut g);

        require_guarantee(g.clone());
        require_guarantee(&g);
        require_guarantee(&mut g);
    }

    // Check that the "auto" implementations of core traits implement all (even
    // default) methods by delegating them to the dereferenced type (in case of
    // standard implementation) or derived-for type (in case of macro). Method
    // delegation of default methods is important to keep performance
    // characteristics of dereferenced/derived-for types on these methods.
    #[test]
    fn trait_impl_correspondence() {
        let graph_traits_source = include_str!("core/graph.rs");
        let macros_source = include_str!("../../gryf-derive/src/lib.rs");
        let props_traits_source = include_str!("core/props.rs");

        test_method_list_equality(
            graph_traits_source,
            graph_traits_source,
            "GraphBase",
            ImplKind::Impl,
            Some(vec!["is_directed"]),
        );

        test_method_list_equality(
            graph_traits_source,
            macros_source,
            "GraphBase",
            ImplKind::Macro,
            Some(vec!["is_directed"]),
        );

        test_method_list_equality(
            graph_traits_source,
            graph_traits_source,
            "Neighbors",
            ImplKind::Impl,
            None,
        );
        test_method_list_equality(
            graph_traits_source,
            macros_source,
            "Neighbors",
            ImplKind::Macro,
            None,
        );

        test_method_list_equality(
            graph_traits_source,
            graph_traits_source,
            "VertexSet",
            ImplKind::Impl,
            None,
        );
        test_method_list_equality(
            graph_traits_source,
            macros_source,
            "VertexSet",
            ImplKind::Macro,
            None,
        );

        test_method_list_equality(
            graph_traits_source,
            graph_traits_source,
            "EdgeSet",
            ImplKind::Impl,
            None,
        );
        test_method_list_equality(
            graph_traits_source,
            macros_source,
            "EdgeSet",
            ImplKind::Macro,
            None,
        );

        test_method_list_equality(
            graph_traits_source,
            graph_traits_source,
            "GraphRef",
            ImplKind::Impl,
            None,
        );
        test_method_list_equality(
            graph_traits_source,
            macros_source,
            "GraphRef",
            ImplKind::Macro,
            None,
        );

        test_method_list_equality(
            graph_traits_source,
            graph_traits_source,
            "GraphWeak",
            ImplKind::Impl,
            None,
        );

        test_method_list_equality(
            graph_traits_source,
            graph_traits_source,
            "GraphMut",
            ImplKind::Impl,
            None,
        );
        test_method_list_equality(
            graph_traits_source,
            macros_source,
            "GraphMut",
            ImplKind::Macro,
            None,
        );

        test_method_list_equality(
            graph_traits_source,
            graph_traits_source,
            "GraphAdd",
            ImplKind::Impl,
            None,
        );
        test_method_list_equality(
            graph_traits_source,
            macros_source,
            "GraphAdd",
            ImplKind::Macro,
            None,
        );

        test_method_list_equality(
            graph_traits_source,
            graph_traits_source,
            "GraphFull",
            ImplKind::Impl,
            None,
        );
        test_method_list_equality(
            graph_traits_source,
            macros_source,
            "GraphFull",
            ImplKind::Macro,
            None,
        );

        test_method_list_equality(
            props_traits_source,
            props_traits_source,
            "Guarantee",
            ImplKind::Impl,
            None,
        );
        test_method_list_equality(
            props_traits_source,
            macros_source,
            "Guarantee",
            ImplKind::Macro,
            None,
        );
    }

    // Check that the graph encapsulations implement all methods that they are
    // supposed to.
    #[test]
    fn graph_impl_consistency() {
        let graph_traits_source = include_str!("core/graph.rs");
        let generic_source = include_str!("domain/generic.rs");
        let path_source = include_str!("domain/path.rs");

        let readonly_traits = vec!["GraphBase", "Neighbors", "VertexSet", "EdgeSet", "GraphRef"];
        let mutable_traits = readonly_traits
            .clone()
            .into_iter()
            .chain(std::iter::once("GraphMut"))
            .collect::<Vec<_>>();
        let all_traits = mutable_traits
            .clone()
            .into_iter()
            .chain(["GraphAdd", "GraphFull"])
            .collect::<Vec<_>>();

        let ignored = vec!["vertex_id_map", "edge_id_map"];

        for trait_name in all_traits.iter() {
            test_method_list_subset(
                graph_traits_source,
                generic_source,
                trait_name,
                "Graph",
                Some(ignored.clone()),
            );
        }

        for trait_name in mutable_traits.iter() {
            test_method_list_subset(
                graph_traits_source,
                path_source,
                trait_name,
                "Path",
                Some(ignored.clone()),
            );
        }
    }

    fn test_method_list_equality(
        def_source: &str,
        impl_source: &str,
        name: &'static str,
        impl_kind: ImplKind,
        ignore: Option<Vec<&'static str>>,
    ) {
        let mut definition = parse_trait_method_list(def_source, SourceKind::Trait(name));

        if let Some(ignore) = ignore {
            definition.retain(|def| !ignore.contains(&def.as_str()));
        }

        let mut implementation =
            parse_trait_method_list(impl_source, impl_kind.to_source_kind(name));

        definition.sort_unstable();
        implementation.sort_unstable();

        assert_eq!(definition, implementation, "{name} trait ({impl_kind:?})");
    }

    fn test_method_list_subset(
        trait_source: &str,
        graph_source: &str,
        trait_name: &'static str,
        graph_name: &'static str,
        ignore: Option<Vec<&'static str>>,
    ) {
        let mut definition = parse_trait_method_list(trait_source, SourceKind::Trait(trait_name));

        if let Some(ignore) = ignore {
            definition.retain(|def| !ignore.contains(&def.as_str()));
        }

        let implementation = parse_graph_method_list(graph_source, graph_name);

        let mut missing = BTreeSet::from_iter(definition);

        for method in implementation {
            missing.remove(&method);
        }

        assert!(
            missing.is_empty(),
            "{graph_name} implementation does not implement {missing:?}"
        );
    }

    #[derive(Debug, Clone, Copy)]
    enum ImplKind {
        Impl,
        Macro,
    }

    impl ImplKind {
        fn to_source_kind(self, name: &'static str) -> SourceKind {
            match self {
                ImplKind::Impl => SourceKind::Impl(name),
                ImplKind::Macro => SourceKind::Macro(name),
            }
        }
    }

    #[derive(Debug)]
    enum SourceKind {
        Trait(&'static str),
        Impl(&'static str),
        Macro(&'static str),
    }

    // Extract the list of methods from a trait definition or implementation.
    // The extraction implementation is text-based, quite primitive and not
    // robust. Nevertheless, it does its job.
    fn parse_trait_method_list(source: &str, kind: SourceKind) -> Vec<String> {
        let mut lines = source.split('\n');
        let lines = lines.by_ref();

        let anchor = match kind {
            SourceKind::Trait(name) => vec![format!("pub trait {name}")],
            SourceKind::Impl(name) | SourceKind::Macro(name) => {
                vec!["impl".to_string(), name.to_string(), "for".to_string()]
            }
        };

        let item_start = lines
            .find(|line| anchor.iter().all(|pat| line.contains(pat)))
            .unwrap_or_else(|| panic!("item start not found ({kind:?})"));

        assert!(!item_start.contains("{}"), "empty item ({kind:?})");

        let indent = item_start.chars().take_while(|&ch| ch == ' ').count();
        let item_end = " ".repeat(indent) + "}";

        let body_lines = lines.take_while(|&line| line != item_end);

        let method_list = body_lines
            .filter_map(|line| {
                let trimmed = line.trim_start();
                if trimmed.starts_with("fn") {
                    let name = trimmed
                        .strip_prefix("fn ")
                        .unwrap()
                        .chars()
                        .take_while(|&ch| ch != '(')
                        .collect::<String>();

                    Some(name)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        assert!(!method_list.is_empty(), "no methods found ({kind:?})");

        assert!(
            !lines.any(|line| anchor.iter().all(|pat| line.contains(pat))),
            "multiple items found ({kind:?})"
        );

        method_list
    }

    // Extract the list of methods from a graph implementation. The extraction
    // implementation is text-based, quite primitive and not robust.
    // Nevertheless, it does its job.
    fn parse_graph_method_list(source: &str, name: &'static str) -> Vec<String> {
        let mut lines = source.split('\n');
        let lines = lines.by_ref();

        let mut method_list = Vec::new();

        while let Some(item_start) = lines.find(|line| {
            ["impl", name].iter().all(|pat| line.contains(pat))
                // Only inherent implementations, not trait implementations.
                && !["for"].iter().any(|pat| line.contains(pat))
        }) {
            let indent = item_start.chars().take_while(|&ch| ch == ' ').count();
            let item_end = " ".repeat(indent) + "}";

            let body_lines = lines.take_while(|&line| line != item_end);

            let methods = body_lines.filter_map(|line| {
                let trimmed = line.trim_start();
                if trimmed.starts_with("pub fn") {
                    let name = trimmed
                        .strip_prefix("pub fn ")
                        .unwrap()
                        .chars()
                        .take_while(|&ch| ch != '(' && ch != '<')
                        .collect::<String>();

                    Some(name)
                } else {
                    None
                }
            });

            method_list.extend(methods);
        }

        assert!(!method_list.is_empty(), "no methods found");

        method_list
    }
}
