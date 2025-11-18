//! Collection of graph algorithms.
//!
//! Algorithms are organized into problems they solve and these problems are
//! represented by a single [type](#structs). The actual algorithm to solve the
//! problem is **automatically selected** by gryf, unless explicitly specified
//! otherwise.
//!
//! Parametrization of an algorithm is done by using a [builder pattern]. The
//! general approach is to call `Problem::on(graph)` method, followed by setters
//! of optional algorithm parameters and completed by a `run` method with
//! required algorithm parameters. The returned value is the type that
//! represents the problem solution which provides a convenient API for the
//! properties of the solution.
//!
//! For simple cases, there are aliases in form of a [function](#functions).
//!
//! [builder pattern]:
//!     https://rust-unofficial.github.io/patterns/patterns/creational/builder.html

pub mod connected;
pub mod cycle;
pub mod shortest_paths;
pub mod toposort;

#[doc(inline)]
pub use self::{
    connected::{Connected, is_connected, is_path_between, is_strongly_connected},
    cycle::{Cycle, is_cyclic, is_cyclic_undirected},
    shortest_paths::ShortestPaths,
    toposort::TopoSort,
};
