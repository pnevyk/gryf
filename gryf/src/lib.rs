pub mod adapt;
pub mod algo;
pub mod core;
pub mod graph;
pub mod infra;
pub mod storage;
pub mod visit;

pub mod prelude {
    #[doc(hidden)]
    pub use crate::{
        core::base::{EdgeRef, NeighborRef, VertexRef},
        visit::Visitor,
    };
}

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
            id::DefaultId,
            marker::Directed,
            props::{Guarantee, MultiEdge},
            EdgeSet, GraphAdd, GraphBase, GraphFull, GraphMut, GraphRef, GraphWeak, Neighbors,
            VertexSet,
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
        let generic_source = include_str!("graph/generic.rs");
        let path_source = include_str!("graph/path.rs");

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
