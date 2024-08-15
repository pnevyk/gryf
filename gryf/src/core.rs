//! Core traits and types used in gryf.
//!
//! The module mostly contains interfaces that storages and graph
//! representations implement. The main traits that represent different levels
//! of graph functionality are
//!
//! * [GraphBase]
//! * [Neighbors]
//! * [VertexSet]
//! * [EdgeSet]
//! * [GraphRef]
//! * [GraphMut]
//! * [GraphAdd]
//! * [GraphFull]
//!
//! The hierarchy and relations between these are depicted on the diagram below
//! (expand the _Trait hierarchy_ box).
//!
//! <details>
//! <summary>Trait hierarchy</summary>
//!
//! <style>svg { width: 100%; height: 100%; }</style>
#![doc = include_str!("../docs/assets/trait_hierarchy.excalidraw.svg")]
//!
//! </details>
//!
//! # Common operations
//!
//! ## Create an empty graph
//!
//! ```
//! use gryf::{core::marker::Directed, Graph};
//!
//! // Directed graph with `&str` vertex attributes and `u32` edge attributes.
//! let graph = Graph::<&str, u32, Directed>::new();
//!
//! // Prefer this if the attribute types (`&str`, `u32`) can be inferred
//! // from the vertices and edges addition.
//! let graph = Graph::<&str, u32, _>::new_directed();
//! ```
//!
//! ## Create an empty graph in a specific storage
//!
//! ```
//! use gryf::{core::marker::Undirected, storage::AdjMatrix, Graph};
//!
//! // The last generic type corresponds to the types of vertex/edge IDs.
//! // Using `::default()` chooses the default ID pair.
//! let graph = Graph::<&str, u32, _, _>::new_in(AdjMatrix::<&str, u32, Undirected, _>::default());
//! ```
//!
//! ## Add vertices and edges
//!
//! ```
//! use gryf::Graph;
//!
//! let mut graph = Graph::new_directed();
//!
//! let foo = graph.add_vertex("foo");
//! let bar = graph.add_vertex("bar");
//! let baz = graph.add_vertex("baz");
//!
//! let foobar = graph.add_edge(foo, bar, 3);
//! graph.extend_with_edges([
//!     (foo, baz, 5),
//!     (bar, baz, 8),
//! ]);
//! ```
//!
//! ```
//! use gryf::{core::marker::Directed, Graph};
//!
//! let vertices = ["foo", "bar", "baz"];
//! let edges = [(0, 1, 3), (0, 2, 5), (1, 2, 8)];
//!
//! let mut graph = Graph::<&str, u32, Directed>::with_capacity(vertices.len(), edges.len());
//! graph.extend_with_vertices(vertices);
//! graph.extend_with_edges(edges);
//! ```
//!
//! ## Basic properties
//!
//! ```
//! use gryf::{
//!     core::marker::{Incoming, Outgoing},
//!     Graph,
//! };
//!
//! let mut graph = Graph::new_directed();
//!
//! let foo = graph.add_vertex("foo");
//! let bar = graph.add_vertex("bar");
//! let baz = graph.add_vertex("baz");
//! graph.add_edge(foo, bar, 3);
//! graph.add_edge(bar, baz, 5);
//!
//! assert!(graph.is_directed());
//! assert_eq!(graph.vertex_count(), 3);
//! assert_eq!(graph.edge_count(), 2);
//! assert_eq!(graph.degree_directed(bar, Outgoing), 1);
//! assert_eq!(graph.degree_directed(bar, Incoming), 1);
//! assert_eq!(graph.degree_undirected(bar), 2);
//! assert!(graph.contains_vertex(baz));
//! assert!(graph.contains_edge_between(foo, bar));
//! assert!(graph.edge_id(foo, bar).next().is_some());
//! ```
//!
//! ## Elements iteration
//!
//! ```
//! use gryf::{core::id::VertexId, Graph};
//!
//! let mut graph = Graph::new_directed();
//!
//! let foo = graph.add_vertex("foo");
//! let bar = graph.add_vertex("bar");
//! graph.add_edge(foo, bar, 3);
//!
//! let by_id: Vec<VertexId> = graph.vertices_by_id().collect();
//! let attrs: Vec<&str> = graph.vertices().map(|v| *v.attr).collect();
//! ```
//!
//! ## Graph traversal
//!
//! ```
//! use gryf::{core::marker::Outgoing, Graph};
//!
//! let mut graph = Graph::new_directed();
//!
//! let foo = graph.add_vertex("foo");
//! let bar = graph.add_vertex("bar");
//! graph.add_edge(foo, bar, 3);
//!
//! assert_eq!(
//!     graph.neighbors_directed(foo, Outgoing).map(|n| n.id).next(),
//!     Some(bar)
//! );
//! ```
//!
//! ## Attributes
//!
//! ```
//! use gryf::Graph;
//!
//! let mut graph = Graph::new_directed();
//!
//! let foo = graph.add_vertex("foo");
//! let bar = graph.add_vertex("bar");
//! let foobar = graph.add_edge(foo, bar, 3);
//!
//! assert_eq!(graph.vertex(foo), Some(&"foo"));
//! assert_eq!(graph.edge(foobar), Some(&3));
//! assert_eq!(graph.find_vertex("bar"), Some(bar));
//! *graph.edge_mut(foobar).unwrap() = 5;
//! ```

pub mod base;
pub mod borrow;
pub mod connect;
pub mod create;
pub mod error;
pub mod facts;
pub mod id;
pub mod marker;
pub mod matrix;
pub mod props;
pub mod weight;

mod graph;

pub use graph::*;
